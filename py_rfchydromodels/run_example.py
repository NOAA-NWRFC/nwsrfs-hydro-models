#!/usr/bin/env python

#############NOTE####################
#Prior to running this code the FORTRAN code must be compiled using
#using the Make file located in utilities.
#The make file requires a python environment with Numpy installed

import os
from utilities.model import *
from utilities.model_prep import *
from utilities.adjustq import *
#import pdb; pdb.set_trace()

#############INPUT####################
folder = os.getcwd()
lid='NRKW1'
run_dir=os.path.join(folder,'ex_opt_run',lid)

######Prep data and initalize model class#########

#Prep Data
forcing, pars, upflow, flow =nwsrfs_prep(run_dir,'results_por_02')

#Initialize model class
sim = Model(forcing,pars,upflow,flow)

#############Example Calls####################

#run complete suite of NWSRFS models associated with this calibration
nwsrfs_run = sim.run_all()

#get SAC-SMA and Snow17 states
nwsrfs_states = sim.sacsnow_states_run()

#get zone 6hr unit hydrograph for each zone
nwsrfs_uh = sim.uh.unit_hydrograph

#get climatological forcing adjustment table
fa_table = sim.forcings.fa_fac(sim.dt_seconds,sim.dates)

#other useful calls
#for models with routing reaches:  sim.lagk_states_run()
#for models with CONS-USE:  sim.consuse_run()

#############Save Sim####################

#Create example csv
nwsrfs_run.to_csv(os.path.join(folder,'EXAMPLE-NRKW1_Sim.csv'))


#############Create AdjustQ Timeseries####################

#Used to format upstream routing reach flow by merging 
# average daily flow observation, instantaneous flow observation,
# and nwsrfs simulation
#emulates CHPS transformation, https://publicwiki.deltares.nl/display/FEWSDOC/AdjustQ

daily_flow_path = os.path.join(run_dir,'flow_daily_'+lid+'.csv')
inst_flow_path = os.path.join(run_dir,'flow_instantaneous_'+lid+'.csv')
sim_path = os.path.join(run_dir,'results_por_02')

adjustq_class = adjustq(daily_flow_path,inst_flow_path,sim_path)
adjustq_example = adjustq_class.adjustq