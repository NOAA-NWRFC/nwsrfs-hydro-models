import pandas as pd
import numpy as np
from utilities.model import Model
import os
import spotpy
from platypus import Problem, Real

class SacSnowUH(Problem):
    def __init__(self, basin, basin_dir, objectives=None):

        # basin name and where to find the data
        self.basin = basin
        self.basin_dir = basin_dir

        # default objective if none is specefied
        if objectives is None:
            self.objectives = ['rmse']
        else:
            self.objectives = objectives

        # default parameter file define all the model parameters and number of zones
        self.default_pars = pd.read_csv(os.path.join(basin_dir,'pars_default.csv'))
        self.zones = self.default_pars['zone'].unique()
        self.n_zones = len(self.zones)

        # forcings for each zone, list of pd.DataFrame
        self.forcings = []
        for zone in self.zones:
            f = pd.read_csv(os.path.join(basin_dir,'forcing_' + zone + '.csv'))
            f.index = pd.to_datetime(f[['year', 'month', 'day', 'hour']])
            self.forcings.append(f)

        # daily observed flow
        self.obs = pd.read_csv(os.path.join(basin_dir,'flow_' + basin + '.csv'))
        self.obs.index = pd.to_datetime(self.obs[['year', 'month', 'day']])
        self.obs['datetime'] = self.obs.index

        # parameter limits for the optimizer
        # any parameter in this file will be optimized
        self.limits = pd.read_csv(os.path.join(basin_dir, 'pars_limits.csv'))

        #import pdb
        #pdb.set_trace()

        self.npar = self.limits['name'].__len__()

        # invoke parent class to set the problem dimension and number of objectives
        super(SacSnowUH, self).__init__(self.npar, self.get_nobj())

        types = []
        self.lower = np.full([self.npar],np.nan)
        self.upper = np.full([self.npar],np.nan)
        for pname,i in zip(self.limits['name'],range(self.npar)):
            lb = self.limits[self.limits['name'] == pname]['lower'].to_numpy()[0]
            ub = self.limits[self.limits['name'] == pname]['upper'].to_numpy()[0]
            self.lower[i] = lb
            self.upper[i] = ub
            types.append(Real(lb,ub))
        self.types[:] = types

        # set up the sac model but dont run it yet
        self.model = Model(self.forcings, self.default_pars, self.obs)


    def update_pars(self,p):
        # make a copy of the parameter df so as not to update the default values
        pars = self.default_pars.copy()
        # update the parameters in the same order they were set
        # TODO find a better way to do this operation
        for pname,i in zip(self.limits['name'],range(len(p))):
            par, zone = pname.rsplit('_', 1)
            pars.loc[(pars['name']==par) & (pars['zone']==zone),'value'] = p[i]
        return pars

    def get_sim(self, p):
        # update the new parameter set from the optimizer and run the model
        pars = self.update_pars(p)
        self.model.update_pars(pars)
        self.model.run()
        # return daily average flows
        return self.model.sim_flow_cfs.resample('D').mean()

    def get_obs(self):
        return self.obs['flow_cfs']

    def get_nobj(self):
        return self.objectives.__len__()

    def get_name(self):
        return "Multiobjective SAC-SMA, SNOW17, UH Model Calibration"

    def evaluate(self,solution):
        x = solution.variables[:]
        solution.objectives[:] = self.get_objectives(x)

    def get_objectives(self,p):

        sim = self.get_sim(p)
        obs = self.get_obs().filter(sim.index)

        obj = []
        for objective in self.objectives:
            if objective == 'rmse':
                obj.append(spotpy.objectivefunctions.rmse(sim,obs))
            if objective == 'neg_rmse':
                obj.append(-spotpy.objectivefunctions.rmse(sim, obs))
            if objective == 'mae':
                obj.append(spotpy.objectivefunctions.mae(sim,obs))
            if objective == 'log':
                obj.append(self.log_obj(sim,obs))
        return obj

    def log_obj(self,sim,obs):
        return np.sum((np.log(sim + 1) - np.log(obs + 1)) ** 2)
        #float(1 - sum((np.log(s) - np.log(e)) ** 2) / sum((np.log(e) - np.mean(np.log(e))) ** 2))