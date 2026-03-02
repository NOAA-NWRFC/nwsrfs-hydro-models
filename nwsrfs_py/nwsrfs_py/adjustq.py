'''
These class functions replicate the CHPS FEWS AdjustQ tool.

Used as a preprocessing step to create upstream streamflow timeseries for routing reaches where LAG-K is being optimized

AdjustQ adjusts observed mean daily discharges using observed instantaneous and simulated discharges. If both observed 
mean daily and instantaneous data are missing, simulated discharge is used (if available).   
The process combines two CHPS FEWS transformations:

    1. **AdjustQUsingInstantaneousDischarge**: Corrects simulated discharges based on instantaneous observations.
    2. **AdjustQUsingMeanDailyDischarge**: Applies additional corrections if mean daily discharges exceed the error tolerance.

Any resulting negative discharge values are set to zero.
'''

import os, sys, argparse, warnings
import pandas as pd, numpy as np
from .simulation import NwsrfsRun as nwsrfs_sim
from . import utils
#import pdb; pdb.set_trace()

class _AdjustQPrep:

    '''
    Internal-only class to create Pandas Series for observed instantaneous data, observed daily data, 
    and CHPS NWSRFS simulation results. Used within the adjustq class.

    Args:
        daily_flow_path (str): Path to daily average observed streamflow csv provided in a NWRFC autocalb directory [ex: flow_daily_[lid].csv].
        inst_flow_path (str): Path to instantaneous observed streamflow csv provided in a NWRFC autocalb directory [ex: flow_instantaneous_[lid].csv].
        ac_run_path (str | None): Optional path to NWRFC autocalibration results directory [ex: 2zone/[lid]/results_por_02].
    Attributes:
        obs_daily (pd.Series): The loaded daily observation timeseries. (units: cfs)
        obs_inst (pd.Series): The loaded instantaneous observation timeseries. (units: cfs)
        sim (pd.Series): The simulation timeseries (if provided). (units: cfs)
    '''

    def __init__(self,
                daily_flow_path: str,
                inst_flow_path: str,
                ac_run_path: str | None = None):
        
        #import observations csvs
        ob_daily_flow = pd.read_csv(daily_flow_path)
        ob_daily_flow['date'] = pd.to_datetime(ob_daily_flow[['year','month','day']])
        ob_daily_flow.set_index('date',inplace=True)
        self.obs_daily =  ob_daily_flow.flow_cfs
        

        ob_inst_flow = pd.read_csv(inst_flow_path)
        ob_inst_flow['datetime'] = pd.to_datetime(ob_inst_flow[['year','month','day','hour']])
        ob_inst_flow.set_index('datetime',inplace=True)
        self.obs_inst =  ob_inst_flow.flow_cfs

        if ac_run_path is not None:
            ac_path, ac_run = os.path.split(ac_run_path)
            ac_sim = nwsrfs_sim(ac_path,ac_run)
            ac_sim_run = ac_sim.sim
        else:
            ac_sim_run = None
        self.sim = ac_sim_run
        
class AdjustQ(_AdjustQPrep):

    '''
    Class to perform an equivalent CHPS FEWS AdjustQ calculation, inheriting from :class:`_AdjustQPrep`. 
    
    Intended to be used within the NWRFC autocalibration folder/data structure. Uses two different 
    procedures to determine if observed instantaneous or simulated data is used to shape daily average observed streamflow: 

        * One to correct for small gaps in observed instantaneous data.
        * Another method for correcting large gaps in observed instantaneous data. 

    The ``blend`` input variable defines which procedure to use.  If both observed mean daily and instantaneous data is missing, 
    simulated discharge is used (if available). 

    Args:
        daily_flow_path (str): Path to daily average observed streamflow csv provided in a NWRFC autocalb directory.
        inst_flow_path (str): Path to instantaneous observed streamflow csv provided in a NWRFC autocalb directory.
        ac_run_path (str | None): Optional path to NWRFC autocalibration results directory.
        blend (int): Threshold for determining how many time steps of missing observed instantaneous data constitutes a "large gap". Default: 10.
        interp_type (str): Correction procedure used to correct simulated discharges for missing gaps smaller than the blend threshold. 
            Accepts ``'ratio'`` or ``'difference'``. Default: ``'ratio'``.
            
            * **ratio**: Correction factor is based on the ratio between observed and simulated discharges.
            * **difference**: Correction value is based on the difference between observed and simulated discharges.
        
        error_tol (float): Percent tolerance that the instantaneous daily average must match the observed daily flow average. Default: 0.01.
        max_iterations (int): Maximum number of iterations to adjust the instantaneous daily average to match the observed daily flow. Default: 15
    Attributes:
        obs_daily (pd.Series): Daily observation streamflow timeseries. (units: cfs)
        obs_inst (pd.Series): Instantaneous observation streamflow timeseries. (units: cfs)
        sim (pd.Series): Simulation streamflow timeseries (if provided). (units: cfs)
    '''

    def __init__(self,
                daily_flow_path: str,
                inst_flow_path: str,
                ac_run_path: str | None = None,
                blend: int=10,
                interp_type:  str='ratio',
                error_tol: float=0.01,
                max_iterations: int=15):
        
        #Get input timeseries 
        super().__init__(daily_flow_path, inst_flow_path, ac_run_path)

        #Set options
        if interp_type == 'ratio' or interp_type == 'difference':
            self.interp_type = interp_type
        else:
            msg = "interp_type input must be either 'ratio' or 'difference'.  Default is 'ratio'"
            raise ValueError(msg)
        self.blend = int(blend)
        self.error_tol = error_tol
        self.max_iterations = int(max_iterations)

        #Set raw_output to None until run function is executed
        self.__raw_output = None

    @property
    def adjustq(self) -> pd.Series:

        """
        This function detects if a simulation timeseries is provided and uses the appropriate 
        AdjustQ calculation
        
            a. :meth:`.AdjustQ.adjustq_calc` is used if NWRFC autocalibration simulation (``sim``) is provided.
            b. Otherwise :meth:`.AdjustQ.inst_mean_q_merge` is used.

        Returns a pd.Series.
        """

        #Get adjustq 
        if (self.sim is not None) & (self.__raw_output is None):
            self.adjustq_calc()
        elif self.__raw_output is None:
            self.inst_mean_q_merge()

        return self.__raw_output


    def _adjustq_inst_smallgaps(self,obs_sim_working,gap_list):

        """
        Steps to fill in missing data where the gap is < blend parameter.

        This procedure uses observed instantaneous discharges to correct simulated discharges. 
        If observed data exists, it replaces the simulated value. If not, it calculates a corrected value.

        **Correction Methods:**
        
        * **Ratio Option**: Correction factor based on the ratio between observed/simulated at start/end of gap.
        * **Difference Option**: Correction based on the difference between observed/simulated.

        **Special Case:**
        The program overrides the configured method and uses **difference** interpolation if:
        1. The ratio between start and end ratios exceeds 2.
        2. Either ratio is greater than 5.

        Args:
            obs_sim_working (pd.DataFrame): Internal working DataFrame used to build the adjustq timeseries.
            gap_list (pd.DataFrame): Internal DataFrame used to identify periods with small gaps.
            
        Returns:
            pd.DataFrame: The updated working DataFrame with small gaps filled.
        """

        for index, row in gap_list.iterrows():
                #Grab the period with the missing values
                end=index
                periods=row.Period_Gap
                start=end-pd.Timedelta(periods*6, unit='h')
                interp_period=obs_sim_working.loc[start:end].copy()

                #Check if the ratio tolerance limits are violated
                start_ratio=interp_period.iloc[0].Inst_Ratio
                end_ratio=interp_period.iloc[-1].Inst_Ratio

                ratio_check1=abs(start_ratio-end_ratio)>2
                ratio_check2=max(start_ratio,end_ratio)>5

                #If interpolation type is difference or if ratio check are violated use the difference method
                if (self.interp_type[0].lower()=='d') | (ratio_check1) | (ratio_check2):
                    #print('Difference Procedure Initiated')
                    interp_period['Inst_Difference']=interp_period.Inst_Difference.interpolate()
                    interp_period['AdjustQ_Inst']=interp_period.simulated+interp_period.Inst_Difference
                #If not using difference interpolation method, use the ratio method
                elif self.interp_type[0].lower()=='r':
                    #print('Ratio Procedure Initiated')
                    interp_period['Inst_Ratio']=interp_period.Inst_Ratio.interpolate()
                    interp_period['AdjustQ_Inst']=interp_period.simulated*interp_period.Inst_Ratio
                #If you get here, an erroneous interpolation type was selected 
                else:
                    msg = "Interp method must be 'ratio' or 'difference', got '{self.interp_type}'."
                    raise ValueError(msg)

                #Add the interpolated data to the obs_sim_working dataframe
                obs_sim_working.loc[start:end,['AdjustQ_Inst']]=interp_period.loc[start:end,['AdjustQ_Inst']]

        return obs_sim_working


    #Steps to fill in missing data where the gap is < blend parameter
    def _adjustq_inst_largegaps(self,obs_sim_working:pd.DataFrame,gap_list:pd.DataFrame):

        '''
        Steps to fill in missing data where the gap is > blend parameter 
        
        When the gap in observed data is too large to fill with interpolation, this procedure 
        uses a "blend" method to ensure a smooth transition. It blends the difference 
        between the simulated and observed discharge at the start and end of the gap over 
        the specified ``blend`` steps.

        Args:
            obs_sim_working (pd.DataFrame): Internal working DataFrame which is used to build the adjustq timeseries
            gap_list (pd.DataFrame): Internal DataFrame used to identify periods with large gaps 
        Returns:
            pd.DataFrame: The updated working DataFrame with large gaps filled.
        '''
        
        #Steps to fill in missing data where the gap is > blend parameter
        for index, row in gap_list.iterrows():

                #Pull back end of the blend period
                end=index
                end_blend=end-pd.Timedelta(self.blend*6, unit='h')
                end_blend_period=obs_sim_working.loc[end_blend:end].copy()

                if not end_blend_period.empty:
                    end_blend_period.reset_index(inplace=True)
                    #Get the difference to blend
                    end_diff=end_blend_period.iloc[-1].Inst_Difference
                    if not pd.isna(end_diff):
                        #Blend the difference over the specified blend period
                        end_blend_period['AdjustQ_Inst']=end_blend_period.simulated+(end_blend_period.index/self.blend)*end_diff
                        end_blend_period.set_index('datetime_local_tz',inplace=True)

                        #Add the interpolated data to the obs_sim_working dataframe
                        obs_sim_working.loc[end_blend:end,['AdjustQ_Inst']]=end_blend_period.loc[:,['AdjustQ_Inst']]

                periods=row.Period_Gap

                #Pull front end of the blend period
                start=end-pd.Timedelta(periods*6, unit='h')
                start_blend=start+pd.Timedelta(self.blend*6, unit='h')
                start_blend_period=obs_sim_working.loc[start:start_blend].copy()

                if not start_blend_period.empty:
                    start_blend_period.reset_index(inplace=True)
                    #Get the difference to blend
                    start_diff=start_blend_period.iloc[0].Inst_Difference
                    if not pd.isna(start_diff):
                        #Blend the difference over the specified blend period
                        start_blend_period['AdjustQ_Inst']=start_blend_period.simulated+(1-start_blend_period.index/self.blend)*start_diff
                        start_blend_period.set_index('datetime_local_tz',inplace=True)

                        #Add the interpolated data to the obs_sim_working dataframe
                        obs_sim_working.loc[start:start_blend,['AdjustQ_Inst']]=start_blend_period.loc[:,['AdjustQ_Inst']]    
        
        return obs_sim_working

    def _adjustq_daily(self,obs_sim_working:pd.DataFrame):

        '''
        Corrects simulated discharge using mean daily discharge values.

        This iterative procedure scales the instantaneous discharge so that its daily average 
        matches the observed daily average. It repeats until the error is within the 
        specified tolerance (``error_tol``) or ``max_iterations`` is reached.

        Args:
            obs_sim_working (pd.DataFrame): Internal working DataFrame which is used to build the adjustq timeseries.
        Returns:
            pd.DataFrame: The updated working DataFrame with daily volume corrections applied.

        '''

        i = 1
        max_error=self.error_tol+1
        while (i < self.max_iterations+1) & (self.error_tol<max_error):
            #print(i)

            #Get Daily Average simulated flow considering both 00:00 time values on either end (given half weight).
            daily_avg=pd.DataFrame((obs_sim_working.AdjustQ_Inst.rolling(5,center=True).sum()+obs_sim_working.AdjustQ_Inst.rolling(3,center=True).sum())/8)
            #12z time has the correct daily weighted average flow
            daily_avg=daily_avg.loc[daily_avg.index.hour==12]
            daily_avg.rename(columns={'AdjustQ_Inst':'Daily_Sim'},inplace=True)
            #Remove any Na values
            daily_avg=daily_avg.loc[~daily_avg.Daily_Sim.isna()]
            #change timestep to daily
            daily_avg=daily_avg.resample('1D').sum()
            #Pull observed daily data
            daily_avg['Daily_Obs']=self.obs_daily.loc[self.obs_daily.index.isin(daily_avg.index)]
            #Get the daily ratio to adjust the instantaneous flow
            daily_avg['Daily_Ratio']=daily_avg.Daily_Obs.divide(daily_avg.Daily_Sim)
            #Get the pbias to track the tolerance
            daily_avg['Pbias']=abs((daily_avg.Daily_Sim-daily_avg.Daily_Obs)/daily_avg.Daily_Sim)
            
            #Convert the index to a include a hour timestep.  
            daily_avg.index=daily_avg.index+pd.Timedelta(12, unit='h')

            #Add the daily ratio to the obs_sim_working dataframe set at non 00:00 time values
            #For the 00:00 time use average of the two days
            obs_sim_working['Daily_Ratio']=daily_avg.Daily_Ratio
            obs_sim_working['Daily_Ratio']=obs_sim_working.Daily_Ratio.interpolate(method='nearest',limit=1,limit_direction='both')
            obs_sim_working['Daily_Ratio']=obs_sim_working.Daily_Ratio.interpolate(limit_direction='both',limit=2)

            #update the instant flow values using the ratio
            daily_index = obs_sim_working.loc[obs_sim_working.Daily_Ratio.notna()].index
            obs_sim_working.loc[daily_index,'AdjustQ_Inst']=obs_sim_working.loc[daily_index,'AdjustQ_Inst']*obs_sim_working.loc[daily_index,'Daily_Ratio']

            i += 1
            max_error=daily_avg.Pbias.max()
            #print(max_error)
            
        return obs_sim_working

    def adjustq_calc(self):

        '''
        Primary AdjustQ calculation function when NWRFC autocalibration simulation (``sim``) is provided.

        This method orchestrates the full adjustment process:

            1. Identifies gaps in observed data.
            2. Calls :meth:`.AdjustQ._adjustq_inst_smallgaps` for short missing periods.
            3. Calls :meth:`.AdjustQ._adjustq_inst_largegaps` for long missing periods.
            4. Calls :meth:`.AdjustQ._adjustq_daily` to match daily volumes.
            5. Clips any negative results to zero.

        Returns:
            pd.Series:  A Series with a adjustq timeseries (units: cfs)
        '''
        ###############PREP Data################################
        
        if self.sim is None:
            msg = "No model simulation timeseries defined.  Use `inst_mean_q_merge` instead."
            raise ValueError(msg)

        #Format the daily observed flow
        # daily_q = self.obs_daily.copy(deep=True)
        # daily_q.index.rename('date_local_tz',inplace=True)
        # daily_q.rename('Daily_Avg_Streamflow_cfs',inplace=True)
        
        #Format the instantaneous observed flow
        inst_q = self.obs_inst.copy(deep=True)
        inst_q.index.rename('datetime_local_tz',inplace=True)
        inst_q.rename('observed',inplace=True)
          
        #Grab the nearest instantaneous value, within 15min, to each 6hr timestep
        obs_6h_begin=inst_q.index[0].floor(freq='D')
        obs_6h_end=inst_q.index[-1].ceil(freq='D')
        obs_6h=pd.DataFrame(index=pd.date_range(start=obs_6h_begin,end=obs_6h_end,freq='6h'))
        obs_6h.index.rename('datetime_local_tz',inplace=True)
        
        obs_6h=pd.merge_asof(obs_6h,inst_q,left_index=True,right_index=True,tolerance=pd.Timedelta('15m'),direction='nearest')
        #Remove any values below zero
        obs_6h=obs_6h[obs_6h.observed>0]

        #Format Simulated 6hr Data
        sim = self.sim.copy(deep=True)
        sim.rename('simulated',inplace=True)
        sim.index.rename('datetime_local_tz',inplace=True)
        
        #Extend the simulated data to span complete days
        sim_begin=sim.index[0].floor(freq='D')
        sim_end=sim.index[-1].ceil(freq='D')
        sim_extend=pd.Index(data=pd.date_range(start=sim_begin,end=sim_end,freq='6h'),name='datetime_local_tz')
        sim = sim.reindex(sim_extend)
        sim = sim.interpolate(limit_direction='both')

        #Working DataFrame
        working=pd.concat([obs_6h,sim],axis=1,sort=True)

        working['Inst_Ratio']=working['observed']/working['simulated']
        working['Inst_Difference']=working['observed']-working['simulated']
        working['AdjustQ_Inst']=working['observed']

        ####################AdjustQ Instantaneous Discharge#################
           
        #Find gaps in the observed period
        obs_gaps=obs_6h.loc[~obs_6h.observed.isna()].copy()
        obs_gaps['Period_Gap']=(obs_gaps.index.to_series().diff()/pd.to_timedelta('1 hour'))/6
        obs_gaps=obs_gaps.loc[obs_gaps.Period_Gap>1,['Period_Gap']].sort_values(by='Period_Gap')
        
        obs_gaps_interp=obs_gaps.loc[obs_gaps.Period_Gap<self.blend]
        obs_gaps_blend=obs_gaps.loc[obs_gaps.Period_Gap>=self.blend]
        
        #Adjust gaps in the observed data less than the blend length
        working=self._adjustq_inst_smallgaps(working,obs_gaps_interp)
        
        #Adjust gaps in the observed data more than the blend length
        working=self._adjustq_inst_largegaps(working,obs_gaps_blend)
        
        #Condense working dataframe to only include the period where there is simulation data
        working=working.loc[~working.simulated.isna()]
        #For any rows that haven't been filled (didn't meet critera to use inst data), use simulation data directly
        working.loc[working.AdjustQ_Inst.isna(),['AdjustQ_Inst']]=working.loc[working.AdjustQ_Inst.isna(),['simulated']].values
        
        ####################AdjustQ Mean Daily#################
        working=self._adjustq_daily(working)
        
        #Replace any negative flow value with zero
        working['AdjustQ_Inst']=working.AdjustQ_Inst.clip(lower=0)

        self.__raw_output = working.AdjustQ_Inst

        return self.__raw_output

    def inst_mean_q_merge(self):

        '''
        Merges instantaneous observed streamflow data with daily mean streamflow data when NWRFC autocalibration simulation (``sim``) is ``'None'``.

        For locations like reservoir outflows. 

        Where available this method uses instantaneous data to shape daily mean flow using :meth:`.AdjustQ._adjustq_daily`. 

        Returns:
            pd.Series: A Series containing the merged and adjusted discharge timeseries (units: cfs).
        '''
        
        ###############PREP Data################################
        
        #Format the daily observed flow
        #Shifting forward 1 timestep to have 00:00 be part of previous day average
        daily_q_6h = self.obs_daily.copy(deep=True)
        daily_q_6h = daily_q_6h.resample('6h').ffill()
        daily_q_6h.index = daily_q_6h.index+pd.Timedelta(6, unit='h')
        daily_q_6h.index.rename('datetime_local_tz',inplace=True)
        daily_q_6h.rename('Inst_Streamflow_cfs',inplace=True)
          
        #Format the instantaneous observed flow
        inst_q = self.obs_inst.copy(deep=True)
        inst_q.index.rename('datetime_local_tz',inplace=True)
        inst_q.rename('Inst_Streamflow_cfs',inplace=True)
          
        #Grab the nearest instantaneous value, within 2hours, to each 6hr timestep
        inst_q_6h_begin=daily_q_6h.index[0].floor(freq='D')
        inst_q_6h_end=daily_q_6h.index[-1].ceil(freq='D')
        inst_q_6h=pd.DataFrame(index=pd.date_range(start=inst_q_6h_begin,end=inst_q_6h_end,freq='6h'))
        inst_q_6h.index.rename('datetime_local_tz',inplace=True)
        
        #Grab the nearest available the instantaneous data within 15min of the 6hr timesteps
        inst_q_6h=pd.merge_asof(inst_q_6h,inst_q,left_index=True,right_index=True,tolerance=pd.Timedelta('15m'),direction='nearest')
        #Remove any values below zero
        inst_q_6h=inst_q_6h[inst_q_6h.Inst_Streamflow_cfs>0]
        
        #For timestep where instantaneousdata was not available, supplement with daily data
        inst_q_6h_merge=pd.DataFrame({'AdjustQ_Inst':inst_q_6h.Inst_Streamflow_cfs.combine_first(daily_q_6h)})
        
        ####################AdjustQ Mean Daily#################
        inst_q_6h_merge=self._adjustq_daily(inst_q_6h_merge)
        
        #Replace any negative flow value with zero
        inst_q_6h_merge['AdjustQ_Inst']=inst_q_6h_merge.AdjustQ_Inst.clip(lower=0)
        
        self.__raw_output = inst_q_6h_merge.AdjustQ_Inst

        return self.__raw_output

    @classmethod
    def load_example(cls, sim:bool=True, **kwargs):
        '''
        Generates a :class:`AdjustQ` for the package provided example data.

        Example data are NWRFC auto calibration results for location:

        * **NRKW1**: Nooksack River at North Cedarville, WA (USGS-12210700).  The simulated timeseries is utilizing the following models: :class:`~nwsrfs_py.nwsrfs.SacSnow`, :class:`~nwsrfs_py.nwsrfs.GammaUh`, :class:`~nwsrfs_py.nwsrfs.Lagk`.

        Any optional arguments associated with :class:`AdjustQ`, can be passed.

        Args:
            sim (bool):  Incorporate the model simulation results in the AdjustQ calculation.  Default: ``True``.
            **kwargs: Optional keyword arguments passed to :class:`AdjustQ`. 

                Common options include:

                * **blend** (int): Threshold for determining how many time steps of missing observed instantaneous data constitutes a "large gap". Default: 10.
                * **interp_type** (str): Correction procedure used to correct simulated discharges for missing gaps smaller than the blend threshold. Accepts ``'ratio'`` or ``'difference'``. Default: ``'ratio'``.        
                * **error_tol** (float): Percent tolerance that the instantaneous daily average must match the observed daily flow average. Default: 0.01.
                * **max_iterations** (int): Maximum number of iterations to adjust the instantaneous daily average to match the observed daily flow. Default: 15.
        '''

        # Define the mapping within the method to keep it clean
        config = {
            'daily_flow_csv': 'flow_daily_NRKW1.csv',
            'inst_flow_csv': 'flow_instantaneous_NRKW1.csv',
            'sim_flow_dir': 'results_por_02',
        }

        #If the sim option is set to False then set the sim_flow_dir key to None
        if not sim:
            config['sim_flow_dir'] = None


        #Return :class:`nwsrfs_py.adjustq.AdjustQ` for NRKW1.
        with utils._get_example_dir('NRKW1') as example_dir:
            # Note: Ensure __init__ loads all data BEFORE the context manager closes
            
            #Set paths
            daily_flow_path = os.path.join(example_dir,config['daily_flow_csv'])
            inst_flow_path = os.path.join(example_dir,config['inst_flow_csv'])
            #If the sim option is set to False then set the sim_path to None
            if sim:
                sim_path = os.path.join(example_dir,config['sim_flow_dir'])
            else:
                sim_path = None

            return cls(daily_flow_path, inst_flow_path, sim_path, **kwargs)
