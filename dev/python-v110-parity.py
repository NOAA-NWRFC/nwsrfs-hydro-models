"""Generate synthetic autocalibration directories and Python 1.1.0 reference outputs.

Used by `dev/test-cross-package.R` to verify live cross-package parity between
`nwsrfspy` 1.1.0 and `nwsrfsr` 1.1.0 on:
  1. Lookup tables (`uh`, `peadj_table`, `adj_table`) on NRKW1 and SFLN2
  2. Ordinate UH (`uh_0, uh_1, ...`) + PCHIP timestep resampling (`3` and `12` hr)
  3. Shared basin UH (`shared_uh_logic = True`)
  4. `area_elev*.csv` + `RSnwElev` precipitation typing (`ptps_logic = False`),
     including `update_pars()` recalculation of `ptps`
  5. Multi-timestep (`12`-hour) simulation across FA, SAC-Snow, UH, Lag-K,
     Chanloss, and Consuse
"""

from __future__ import annotations

import os
import shutil
import sys

import pandas as pd

from nwsrfs_py import simulation, utils


def _copy_subset_autocalb(lid: str, run_subdir: str, dst: str, n_steps: int, step_stride: int = 1):
    os.makedirs(os.path.join(dst, run_subdir), exist_ok=True)
    with utils._get_example_dir(lid) as src_dir:
        pars = pd.read_csv(os.path.join(src_dir, run_subdir, "pars_optimal.csv"))
        pars.to_csv(os.path.join(dst, run_subdir, "pars_optimal.csv"), index=False)

        for fname in sorted(os.listdir(src_dir)):
            fpath = os.path.join(src_dir, fname)
            if not os.path.isfile(fpath) or not fname.endswith(".csv"):
                continue
            if fname.startswith("forcing_por_"):
                df = pd.read_csv(fpath).iloc[:n_steps:step_stride].copy()
                if step_stride > 1 and "map_mm" in df.columns:
                    df["map_mm"] = df["map_mm"] * step_stride
                df.to_csv(os.path.join(dst, fname), index=False)
            elif fname.startswith("upflow_"):
                df = pd.read_csv(fpath).iloc[:n_steps:step_stride].copy()
                df.to_csv(os.path.join(dst, fname), index=False)
            elif fname.startswith("flow_daily_"):
                n_days = max(10, (n_steps // 4) + 5)
                df = pd.read_csv(fpath).iloc[:n_days].copy()
                df.to_csv(os.path.join(dst, fname), index=False)


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python dev/python-v110-parity.py OUTDIR", file=sys.stderr)
        return 2

    outdir = sys.argv[1]
    if os.path.exists(outdir):
        shutil.rmtree(outdir)
    os.makedirs(outdir, exist_ok=True)

    # 1. Lookup tables on bundled NRKW1 and SFLN2
    r_nrkw1 = simulation.NwsrfsRun.load_example("NRKW1")
    r_sfln2 = simulation.NwsrfsRun.load_example("SFLN2")
    r_nrkw1.uh.reset_index(names="hours").to_csv(os.path.join(outdir, "uh_NRKW1.csv"), index=False)
    r_sfln2.uh.reset_index(names="hours").to_csv(os.path.join(outdir, "uh_SFLN2.csv"), index=False)
    r_nrkw1.peadj_table.reset_index().to_csv(os.path.join(outdir, "peadj_NRKW1.csv"), index=False)
    r_sfln2.peadj_table.reset_index().to_csv(os.path.join(outdir, "peadj_SFLN2.csv"), index=False)
    r_sfln2.adj_table.reset_index().to_csv(os.path.join(outdir, "chanloss_SFLN2.csv"), index=False)

    n_steps = 2924  # 2 full years of 6-hour steps

    # 2. SFLN2 with uneven-length ordinate UH + PCHIP resampling (3 hr & 12 hr)
    ord_dir = os.path.join(outdir, "case_ordinate_uh")
    _copy_subset_autocalb("SFLN2", "results_por_01", ord_dir, n_steps=n_steps)
    pars_ord = pd.read_csv(os.path.join(ord_dir, "results_por_01", "pars_optimal.csv"))
    uh_ref = r_sfln2.uh.copy()
    keep_mask = ~((pars_ord["type"] == "uh") & (pars_ord["name"] != "zone_area"))
    pars_base = pars_ord.loc[keep_mask].copy()
    ord_rows = []
    for idx_z, z in enumerate(uh_ref.columns):
        vals = uh_ref[z].dropna().to_numpy()
        # Trim last zero on zone 2 if > 3 ordinates so lengths differ across zones
        if idx_z == 1 and len(vals) > 3 and vals[-1] == 0:
            vals = vals[:-1]
        for k, val in enumerate(vals):
            ord_rows.append(
                {
                    "p_name": f"uh_{k}_{z}",
                    "name": f"uh_{k}",
                    "type": "uh",
                    "zone": z,
                    "value": float(val),
                }
            )
    pd.concat([pars_base, pd.DataFrame(ord_rows)], ignore_index=True).to_csv(
        os.path.join(ord_dir, "results_por_01", "pars_optimal.csv"), index=False
    )
    py_ord = simulation.NwsrfsRun(ord_dir, "results_por_01")
    py_ord.sim.rename("sim").to_csv(os.path.join(outdir, "sim_ordinate_uh.csv"), index=False)
    py_ord._uh.return_uh(3).set_axis(py_ord.uh_zone_names, axis=1).reset_index(
        names="hours"
    ).to_csv(os.path.join(outdir, "uh_ordinate_3hr.csv"), index=False)
    py_ord._uh.return_uh(12).set_axis(py_ord.uh_zone_names, axis=1).reset_index(
        names="hours"
    ).to_csv(os.path.join(outdir, "uh_ordinate_12hr.csv"), index=False)

    # 3. NRKW1 with shared gamma UH ("NRKW1-UH") + Lag-K
    sh_dir = os.path.join(outdir, "case_shared_uh")
    _copy_subset_autocalb("NRKW1", "results_por_02", sh_dir, n_steps=n_steps)
    pars_sh = pd.read_csv(os.path.join(sh_dir, "results_por_02", "pars_optimal.csv"))
    pars_sh = pars_sh.loc[~((pars_sh["type"] == "uh") & (pars_sh["name"] != "zone_area"))].copy()
    shared_rows = pd.DataFrame(
        [
            {"p_name": "unit_shape_NRKW1-UH", "name": "unit_shape", "type": "uh", "zone": "NRKW1-UH", "value": 2.3},
            {"p_name": "unit_toc_NRKW1-UH", "name": "unit_toc", "type": "uh", "zone": "NRKW1-UH", "value": 24.0},
            {"p_name": "unit_toc_adj_NRKW1-UH", "name": "unit_toc_adj", "type": "uh", "zone": "NRKW1-UH", "value": 1.0},
        ]
    )
    pd.concat([pars_sh, shared_rows], ignore_index=True).to_csv(
        os.path.join(sh_dir, "results_por_02", "pars_optimal.csv"), index=False
    )
    py_sh = simulation.NwsrfsRun(sh_dir, "results_por_02")
    py_sh.sim.rename("sim").to_csv(os.path.join(outdir, "sim_shared_uh.csv"), index=False)
    py_sh.uh.reset_index(names="hours").to_csv(os.path.join(outdir, "uh_shared.csv"), index=False)

    # 4. SFLN2 with area_elev + RSnwElev (no ptps in forcing CSVs, no ptps_lower/upper limits) + update_pars
    rs_dir = os.path.join(outdir, "case_rsnwelev")
    _copy_subset_autocalb("SFLN2", "results_por_01", rs_dir, n_steps=n_steps)
    for fname in sorted(os.listdir(rs_dir)):
        if fname.startswith("forcing_por_"):
            fpath = os.path.join(rs_dir, fname)
            df = pd.read_csv(fpath).drop(columns=["ptps"])
            df.to_csv(fpath, index=False)

    quantiles = [round(i * 0.05, 2) for i in range(21)]
    ae_df = pd.DataFrame(
        {
            "quantile": quantiles,
            "SFLN2-1": [1585 + i * 28 for i in range(21)],
            "SFLN2-2": [1830 + i * 32 for i in range(21)],
            "SFLN2-CU": [1585 + i * 28 for i in range(21)],
        }
    )
    ae_df.to_csv(os.path.join(rs_dir, "area_elev_SFLN2.csv"), index=False)

    pars_rs = pd.read_csv(os.path.join(rs_dir, "results_por_01", "pars_optimal.csv"))
    pars_rs = pars_rs.loc[~pars_rs["name"].str.match(r"^ptps_(lower|upper)_")].copy()
    rs_rows = []
    for z in ("SFLN2-1", "SFLN2-2", "SFLN2-CU"):
        rs_rows.append({"p_name": f"pxtemp_{z}", "name": "pxtemp", "type": "rsnwelev", "zone": z, "value": 0.5})
        rs_rows.append({"p_name": f"talr_{z}", "name": "talr", "type": "rsnwelev", "zone": z, "value": 0.55})
    pd.concat([pars_rs, pd.DataFrame(rs_rows)], ignore_index=True).to_csv(
        os.path.join(rs_dir, "results_por_01", "pars_optimal.csv"), index=False
    )

    py_rs = simulation.NwsrfsRun(rs_dir, "results_por_01")
    py_rs.sim.rename("sim").to_csv(os.path.join(outdir, "sim_rsnwelev_init.csv"), index=False)
    py_rs.update_pars(
        pd.DataFrame({"p_name": ["pxtemp_SFLN2-1", "talr_SFLN2-2"], "value": [2.5, 0.75]})
    )
    py_rs.sim.rename("sim").to_csv(os.path.join(outdir, "sim_rsnwelev_updated.csv"), index=False)

    # 5. Multi-timestep (12-hour) NRKW1 (with Lag-K) and SFLN2 (with Chanloss + Consuse)
    nrkw1_12_dir = os.path.join(outdir, "case_nrkw1_12hr")
    _copy_subset_autocalb("NRKW1", "results_por_02", nrkw1_12_dir, n_steps=n_steps, step_stride=2)
    py_nrkw1_12 = simulation.NwsrfsRun(nrkw1_12_dir, "results_por_02")
    py_nrkw1_12.sim.rename("sim").to_csv(os.path.join(outdir, "sim_nrkw1_12hr.csv"), index=False)

    sfln2_12_dir = os.path.join(outdir, "case_sfln2_12hr")
    _copy_subset_autocalb("SFLN2", "results_por_01", sfln2_12_dir, n_steps=n_steps, step_stride=2)
    py_sfln2_12 = simulation.NwsrfsRun(sfln2_12_dir, "results_por_01")
    py_sfln2_12.sim.rename("sim").to_csv(os.path.join(outdir, "sim_sfln2_12hr.csv"), index=False)

    print(f"Wrote v1.1.0 parity cases to {outdir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
