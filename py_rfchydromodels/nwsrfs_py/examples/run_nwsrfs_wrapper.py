import numpy as np
import pandas as pd
from nwsrfs_py import simulation
from nwsrfs_py import nwsrfs

def main():
    print("Initializing NWSRFS Wrapper Example...")
    print(" ")
    print("~~Running SNOW17 & SAC-SMA Wrappers~~")
    print(" ")

    lid = 'SFLN2'
    # 1. Access a the example data
    nwsrfs_sim = simulation.NwsrfsRun.load_example(lid)

    # 2. Get SNOW-17 and SAC-SMA parameters

    #Get a nested dictionary for SAC-SMA, Snow17 parameter values
    pars_dict = {}
    for par_type in ['sac','snow']:
        pars_dict[par_type]= {}
        for par in nwsrfs_sim.pars.loc[nwsrfs_sim.pars.type==par_type].name.unique():
            pars_dict[par_type][par] = nwsrfs_sim.pars.loc[(nwsrfs_sim.pars.type==par_type)&
                (nwsrfs_sim.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()

    #Format required dataclass inputs sac_pars and snow_pars
    sac_pars = np.concatenate([[pars_dict['sac']['uztwm']], [pars_dict['sac']['uzfwm']], [pars_dict['sac']['lztwm']],
                                [pars_dict['sac']['lzfpm']],[pars_dict['sac']['lzfsm']], [pars_dict['sac']['adimp']],
                                [pars_dict['sac']['uzk']],[pars_dict['sac']['lzpk']], [pars_dict['sac']['lzsk']],
                                [pars_dict['sac']['zperc']],[pars_dict['sac']['rexp']], [pars_dict['sac']['pctim']],
                                [pars_dict['sac']['pfree']],[pars_dict['sac']['riva']], [pars_dict['sac']['side']],
                                [pars_dict['sac']['rserv']],[pars_dict['sac']['efc']]
                                ],axis=0)
    
    snow_pars = np.concatenate([[pars_dict['snow']['scf']], [pars_dict['snow']['mfmax']], [pars_dict['snow']['mfmin']],
                        [pars_dict['snow']['uadj']],[pars_dict['snow']['si']], [pars_dict['snow']['nmf']],
                        [pars_dict['snow']['tipm']],[pars_dict['snow']['mbase']], [pars_dict['snow']['plwhc']],
                        [pars_dict['snow']['daygm']],[pars_dict['snow']['adc_a']], [pars_dict['snow']['adc_b']],
                        [pars_dict['snow']['adc_c']]
                        ],axis=0)

    # 3. Create SNOW-17/SAC-SMA Dataclass

    #Initate SacSnowPars data class
    sacsnow_dc = nwsrfs.SacSnowPars(year = nwsrfs_sim.year,month =nwsrfs_sim.month,day = nwsrfs_sim.day,hour = nwsrfs_sim.hour,
                            alat = pars_dict['snow']['alat'], elev = pars_dict['snow']['elev'], 
                            sac_pars =  sac_pars, snow_pars = snow_pars,
                            init_swe = pars_dict['snow']['init_swe'], 
                            pxadj =  pars_dict['sac']['pxadj'], peadj = pars_dict['sac']['peadj'],
                            forcings_map = nwsrfs_sim.forcings['map'].to_numpy(),
                            forcings_mat = nwsrfs_sim.forcings['mat'].to_numpy(),
                            forcings_ptps = nwsrfs_sim.forcings['ptps'].to_numpy(),
                                forcings_etd = nwsrfs_sim.forcings['etd'].to_numpy())

    # 4.  Create SacSnowPars wrapper class
    sacsnow_wapper = nwsrfs.SacSnow(pars_dataclass = sacsnow_dc)

    # 5.  Return tci
    print("~~Return tci~~")
    print(sacsnow_wapper.sacsnow_tci.head())

if __name__ == "__main__":
    main() 