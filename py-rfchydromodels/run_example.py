#############NOTE####################
#Prior to running this code the FORTRAN code must be compiled using
#using the Make file located in utilities.
#The make file requires a python environment with Numpy installed

import os

from utilities.model import *
from utilities.model_prep import *

#############INPUT####################
folder = os.getcwd()
lid='NRKW1'
run_dir=os.path.join(folder,'ex_opt_run',lid)
#########################################

#Prep and run the fortran NWRFS code
forcing, pars, upflow, flow =nwsrfs_prep(run_dir,'results_por_02')
sim = Model(forcing,pars,upflow,flow)

#Look through the model.py, many more call than this.  However this is the one which will return the total flow
sim.run_all()