"""Write the Python simulation for every case in the cross-package argument matrix.

`dev/test-cross-package.R` compares the R simulation against these outputs. The
case list lives here rather than in the R script so the two cannot drift: the R
side reads `cases.csv` and calls `load_example()` with whatever arguments it
finds there.

The matrix exists because the strict baseline comparisons only ever ran
`load_example()` with default arguments, which is how `forcing_adj` being
ignored by the R package went unnoticed. Each non-default argument is varied on
its own, plus one case with all of them at once.

Usage:

    python dev/python-sim-matrix.py OUTDIR

Writes:

    OUTDIR/cases.csv         case_id, lid, forcing_adj, shift_sf, return_inst
    OUTDIR/sim_<case_id>.csv single "sim" column of 6 hourly flow in cfs
    OUTDIR/pars_<LID>.csv    the parameter table each run was built from

Output is meant for a temporary directory. These are full 43 year simulations,
roughly 1 MB per case, and are not committed.
"""

from __future__ import annotations

import csv
import os
import sys

from nwsrfs_py import simulation

# forcing_adj is True, False, or a list of forcing types; shift_sf and
# return_inst are booleans.
CASES = []
for _lid, _subset in (("NRKW1", ["mat", "pet"]), ("SFLN2", ["map", "ptps"])):
    CASES += [
        (f"{_lid}_default", _lid, True, True, True),
        (f"{_lid}_no_fa", _lid, False, True, True),
        (f"{_lid}_fa_subset", _lid, _subset, True, True),
        (f"{_lid}_no_shift", _lid, True, False, True),
        (f"{_lid}_peravg", _lid, True, True, False),
        (f"{_lid}_all_nondefault", _lid, False, False, False),
    ]


def serialize_forcing_adj(forcing_adj) -> str:
    """Render forcing_adj so the R side can round-trip it."""
    if isinstance(forcing_adj, bool):
        return "TRUE" if forcing_adj else "FALSE"
    return "|".join(forcing_adj)


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    outdir = sys.argv[1]
    os.makedirs(outdir, exist_ok=True)

    with open(os.path.join(outdir, "cases.csv"), "w", newline="") as fh:
        writer = csv.writer(fh)
        writer.writerow(["case_id", "lid", "forcing_adj", "shift_sf", "return_inst"])

        for case_id, lid, forcing_adj, shift_sf, return_inst in CASES:
            print(
                f"Running {case_id}: forcing_adj={forcing_adj}, "
                f"shift_sf={shift_sf}, return_inst={return_inst}"
            )
            run = simulation.NwsrfsRun.load_example(
                lid,
                forcing_adj=forcing_adj,
                shift_sf=shift_sf,
                return_inst=return_inst,
            )
            run.sim.rename("sim").to_csv(
                os.path.join(outdir, f"sim_{case_id}.csv"), index=False, header=True
            )
            pars_path = os.path.join(outdir, f"pars_{lid}.csv")
            if not os.path.exists(pars_path):
                run.pars.to_csv(pars_path, index=False)
            writer.writerow(
                [
                    case_id,
                    lid,
                    serialize_forcing_adj(forcing_adj),
                    "TRUE" if shift_sf else "FALSE",
                    "TRUE" if return_inst else "FALSE",
                ]
            )

    print(f"Wrote {len(CASES)} cases to {outdir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
