#!/usr/bin/env python

#These functions replicate the FEWS/NWRFS Adjust Q tool
#Created by:  Geoffrey Walters PE, 5/24/21, modified 1/27/24

# AdjustQ adjusts simulated discharges using observed instantaneous and mean daily discharges. 
# The process combines two CHPS FEWS transformations:
# 1. AdjustQUsingInstantaneousDischarge: Corrects simulated discharges based on instantaneous observations.
# 2. AdjustQUsingMeanDailyDischarge: Applies additional corrections if mean daily discharges exceed the error tolerance.
# Any resulting negative discharge values are set to zero.


import os, sys, argparse, warnings
import pandas as pd, numpy as np
from .model import *
from .model_prep import *

#import pdb; pdb.set_trace()

# Class to create Pandas Series for observed instantaneous data, observed daily data, 
# and CHPS NWSRFS simulation results.

class adjustq_prep:

    def __init__(self,
                daily_flow_path: str,
                inst_flow_path: str,
                ac_run_path: str=""):
        
        #import observations csvs
        ob_daily_flow = pd.read_csv(daily_flow_path)
        ob_daily_flow['date'] = pd.to_datetime(ob_daily_flow[['year','month','day']])
        ob_daily_flow.set_index('date',inplace=True)
        self.obs_daily =  ob_daily_flow.flow_cfs
        

        ob_inst_flow = pd.read_csv(inst_flow_path)
        ob_inst_flow['datetime'] = pd.to_datetime(ob_inst_flow[['year','month','day','hour']])
        ob_inst_flow.set_index('datetime',inplace=True)
        self.obs_inst =  ob_inst_flow.flow_cfs

        if ac_run_path:
            ac_path, ac_run = os.path.split(ac_run_path)
            forcing, pars, upflow, flow =nwsrfs_prep(ac_path,ac_run)
            ac_sim = Model(forcing,pars,upflow,flow)
            ac_sim_run = ac_sim.run_all()
        else:
            ac_sim_run = pd.Series(dtype=float)
        self.sim = ac_sim_run
        

# Class to perform the AdjustQ calculation, utilizing AdjustQPrep as a subclass.

class adjustq(adjustq_prep):

    def __init__(self,
                daily_flow_path: str,
                inst_flow_path: str,
                ac_run_path: str="",
                interp_type:  str='ratio',
                blend: int=10,
                error_tol: float=0.01,
                max_iterations: int=15):
        
        #Set options
        self.interp_type = interp_type
        self.blend = blend
        self.error_tol = error_tol
        self.max_iterations = max_iterations

        #Get inputs and define them
        adjustq_inputs = adjustq_prep(daily_flow_path,
            inst_flow_path,
            ac_run_path)

        self.obs_daily = adjustq_inputs.obs_daily 
        self.obs_inst = adjustq_inputs.obs_inst
        self.sim = adjustq_inputs.sim

        #Get adjustq 
        if not self.sim.empty:
            self.adjustq = self.adjustq_calc(self.obs_inst,self.obs_daily,self.sim,
                self.interp_type, self.blend,self.error_tol,self.max_iterations)
        else:
           self.adjustq = self.inst_mean_q_merge(self.obs_inst,self.obs_daily,
            self.error_tol,self.max_iterations)

    # Returns the calculated AdjustQ as a Pandas Series.
    def get_adjustq(self):
        return pd.DataFrame(self.adjustq)


    # This procedure uses observed instantaneous discharges to correct simulated discharges. 
    # - If observed data exists for a time step, it replaces the simulated value. 
    # - If no observed data is available, the procedure calculates a corrected value or uses the simulated value as-is.

    # Correction Methods:
    # 1. **Ratio Option**:
    #    - Simulated values are corrected by multiplying them with a correction factor.
    #    - The correction factor is based on the ratio between observed and simulated discharges at the start and end of a gap.
    #    - The factor is linearly interpolated across the gap.
    # 2. **Difference Option**:
    #    - Simulated values are corrected by adding a correction value.
    #    - The correction value is based on the difference between observed and simulated discharges at the start and end of the gap.

    # Special Case:
    # - The program may override the configured interpolation method in certain scenarios:
    #    - If the ratio between start and end ratios exceeds 2, or
    #    - If either ratio is greater than 5.
    # - In such cases, the program switches to interpolating by difference, even if ratio-based interpolation was configured.

    #Steps to fill in missing data where the gap is < blend parameter
    def adjustq_inst_smallgaps(self,obs_sim_working,gap_list,interp_type):

        for index, row in gap_list.iterrows():
                #Grab the period with the missing values
                end=index
                periods=row.Period_Gap
                start=end-pd.Timedelta(periods*6, unit='H')
                interp_period=obs_sim_working.loc[start:end].copy()

                #Check if the ratio tolerence limits are violated
                start_ratio=interp_period.iloc[0].Inst_Ratio
                end_ratio=interp_period.iloc[-1].Inst_Ratio

                ratio_check1=abs(start_ratio-end_ratio)>2
                ratio_check2=max(start_ratio,end_ratio)>5

                #If interpolation type is difference or if ratio check are violated use the difference method
                if (interp_type[0].lower()=='d') | (ratio_check1) | (ratio_check2):
                    #print('Difference Procedure Initiated')
                    interp_period['Inst_Difference']=interp_period.Inst_Difference.interpolate()
                    interp_period['AdjustQ_Inst']=interp_period.simulated+interp_period.Inst_Difference
                #If not using difference interpolation method, use the ratio method
                elif interp_type[0].lower()=='r':
                    #print('Ratio Procedure Initiated')
                    interp_period['Inst_Ratio']=interp_period.Inst_Ratio.interpolate()
                    interp_period['AdjustQ_Inst']=interp_period.simulated*interp_period.Inst_Ratio
                #If you get here, an erronous interpolation type was selected 
                else:
                    #print('Interp method must be specified as ratio or difference \n'+
                    #     '***Stopping Routine***')
                    sys.exit()


                #Add the interpolated data to the obs_sim_working dataframe
                obs_sim_working.loc[start:end,['AdjustQ_Inst']]=interp_period.loc[start:end,['AdjustQ_Inst']]

        return obs_sim_working

    #When the gap in the observed data is to large to fill with a interpolation procedure then the gap in the observed data will be filled with the 
    #blend procedure. This procedure is also used to provide a smooth transition between the observed data and the simulated at T0. The blend procedure 
    #will provide a smooth transition between the observed data and the simulated data. The difference between the simulated discharge and the observed 
    #discharge at the beginning of a gap, end of a gap or at the latest observed value will be used to correct the simulated value.

    # If the gap in observed data is too large to fill using interpolation, the blend procedure is used. 
    # This procedure ensures a smooth transition between observed and simulated data . 
    # The blend procedure corrects simulated values by adjusting for the difference between simulated 
    # and observed discharges at the start of the gap or the end of the gap using the specified blend steps

    #Steps to fill in missing data where the gap is < blend parameter
    def adjustq_inst_largegaps(self,obs_sim_working,gap_list,blend):
        
        #Steps to fill in missing data where the gap is > blend parameter 
        for index, row in gap_list.iterrows():

                #Pull back end of the blend period
                end=index
                end_blend=end-pd.Timedelta(blend*6, unit='H')
                end_blend_period=obs_sim_working.loc[end_blend:end].copy()
                end_blend_period.reset_index(inplace=True)
                #Get the difference to blend
                end_diff=end_blend_period.iloc[-1].Inst_Difference
                #Blend the difference over the specified blend period
                end_blend_period['AdjustQ_Inst']=end_blend_period.simulated+(end_blend_period.index/blend)*end_diff
                end_blend_period.set_index('datetime_local_tz',inplace=True)

                #Add the interpolated data to the obs_sim_working dataframe
                obs_sim_working.loc[end_blend:end,['AdjustQ_Inst']]=end_blend_period.loc[:,['AdjustQ_Inst']]

                periods=row.Period_Gap

                #Pull front end of the blend period
                start=end-pd.Timedelta(periods*6, unit='H')
                start_blend=start+pd.Timedelta(blend*6, unit='H')
                start_blend_period=obs_sim_working.loc[start:start_blend].copy()
                start_blend_period.reset_index(inplace=True)
                #Get the difference to blend
                start_diff=start_blend_period.iloc[0].Inst_Difference
                #Blend the difference over the specified blend period
                start_blend_period['AdjustQ_Inst']=start_blend_period.simulated+(1-start_blend_period.index/blend)*start_diff
                start_blend_period.set_index('datetime_local_tz',inplace=True)

                #Add the interpolated data to the obs_sim_working dataframe
                obs_sim_working.loc[start:start_blend,['AdjustQ_Inst']]=start_blend_period.loc[:,['AdjustQ_Inst']]    
        
        return obs_sim_working

    # This procedure corrects the simulated discharge using mean daily discharge values until the error is within the specified tolerance.

    #Use observed daily average data to shape instantaneous data
    def adjustq_daily(self,obs_sim_working,daily_q,max_iterations,error_tol):
        i = 1
        max_error=error_tol+1
        while (i < max_iterations+1) & (error_tol<max_error):
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
            daily_avg['Daily_Obs']=daily_q.loc[daily_q.index.isin(daily_avg.index)]
            #Get the daily ratio to adjust the instanteous flow
            daily_avg['Daily_Ratio']=daily_avg.Daily_Obs.divide(daily_avg.Daily_Sim)
            #Get the pbias to track the tolerence
            daily_avg['Pbias']=abs((daily_avg.Daily_Sim-daily_avg.Daily_Obs)/daily_avg.Daily_Sim)
            
            #Convet the index to a include a hour timestep.  
            daily_avg.index=daily_avg.index+pd.Timedelta(12, unit='H')

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

    ########################################################################################################
    #########################Primary AdjustQ Function (calls other functions above)#########################
    ########################################################################################################

    def adjustq_calc(self,inst_q,daily_q,sim,interp_type='ratio',blend=10,error_tol=.01,max_iterations=15):
        
        ###############PREP Data################################
        
        #Format the daily observed flow
        daily_q.index.rename('date_local_tz',inplace=True)
        daily_q.rename('Daily_Avg_Streamflow_cfs',inplace=True)
        
        #Format the instantaneous observed flow
        inst_q.index.rename('datetime_local_tz',inplace=True)
        inst_q.rename('observed',inplace=True)
          
        #Grab the nearest instanteous value, within 15min, to each 6hr timestep
        obs_6h_begin=inst_q.index[0].floor(freq='D')
        obs_6h_end=inst_q.index[-1].ceil(freq='D')
        obs_6h=pd.DataFrame(index=pd.date_range(start=obs_6h_begin,end=obs_6h_end,freq='6h'))
        obs_6h.index.rename('datetime_local_tz',inplace=True)
        
        obs_6h=pd.merge_asof(obs_6h,inst_q,left_index=True,right_index=True,tolerance=pd.Timedelta('15m'),direction='nearest')
        #Remove any values below zero
        obs_6h=obs_6h[obs_6h.observed>0]

        #Format Simulated 6hr Data
        sim.rename('simulated',inplace=True)
        sim.index.rename('datetime_local_tz',inplace=True)
        
        #Extend the simulated data to span complete days
        sim_begin=sim.index[0].floor(freq='D')
        sim_end=sim.index[-1].ceil(freq='D')
        sim_extend=pd.DataFrame(index=pd.date_range(start=sim_begin,end=sim_end,freq='6h'))
        sim_extend.index.rename('datetime_local_tz',inplace=True)
        sim=pd.concat([sim_extend,sim],axis=1)
        sim.simulated=sim.simulated.interpolate(limit_direction='both')

        #Working DataFrame
        working=pd.concat([obs_6h,sim],axis=1)

        working['Inst_Ratio']=working['observed']/working['simulated']
        working['Inst_Difference']=working['observed']-working['simulated']
        working['AdjustQ_Inst']=working['observed']

        ####################AdjustQ Instantaneous Discharge#################
           
        #Find gaps in the observed period
        obs_gaps=obs_6h.loc[~obs_6h.observed.isna()].copy()
        obs_gaps['Period_Gap']=(obs_gaps.index.to_series().diff()/pd.to_timedelta('1 hour'))/6
        obs_gaps=obs_gaps.loc[obs_gaps.Period_Gap>1,['Period_Gap']].sort_values(by='Period_Gap')
        
        obs_gaps_interp=obs_gaps.loc[obs_gaps.Period_Gap<blend]
        obs_gaps_blend=obs_gaps.loc[obs_gaps.Period_Gap>=blend]
        
        #Adjust gaps in the observed data less than the blend length
        working=self.adjustq_inst_smallgaps(working,obs_gaps_interp,interp_type)
        
        #Adjust gaps in the observed data more than the blend length
        working=self.adjustq_inst_largegaps(working,obs_gaps_blend,blend)
        
        #Condense working dataframe to only include the period where there is simulation data
        working=working.loc[~working.simulated.isna()]
        #For any rows that haven't been filled (didn't meet critera to use inst data), use simulation data directly
        working.loc[working.AdjustQ_Inst.isna(),['AdjustQ_Inst']]=working.loc[working.AdjustQ_Inst.isna(),['simulated']].values
        
        ####################AdjustQ Mean Daily#################
        working=self.adjustq_daily(working,daily_q,max_iterations,error_tol)
        
        #Replace any negative flow value with zero
        working['AdjustQ_Inst']=working.AdjustQ_Inst.clip(lower=0)
        
        return working.AdjustQ_Inst


    # For locations without an available simulation (e.g., project outflows), this tool fills missing 6-hour data 
    # from the instantaneous dataset using daily mean flow. The resulting 6-hour merged data is smoothed using 
    # the AdjustQDaily function to match daily averages.
    def inst_mean_q_merge(self,inst_q,daily_q,error_tol=.01,max_iterations=15):
        
        ###############PREP Data################################
        
        #Format the daily observed flow
        #Shifting forward 1 timestep to have 00:00 be part of previous day average
        daily_q_6h=daily_q.resample('6h').ffill()
        daily_q_6h.index=daily_q_6h.index+pd.Timedelta(6, unit='H')
        daily_q_6h.index.rename('datetime_local_tz',inplace=True)
        daily_q_6h.rename('Inst_Streamflow_cfs',inplace=True)
          
        #Format the instantaneous observed flow
        inst_q.index.rename('datetime_local_tz',inplace=True)
        inst_q.rename('Inst_Streamflow_cfs',inplace=True)
          
        #Grab the nearest instanteous value, within 2hours, to each 6hr timestep
        inst_q_6h_begin=daily_q_6h.index[0].floor(freq='D')
        inst_q_6h_end=daily_q_6h.index[-1].ceil(freq='D')
        inst_q_6h=pd.DataFrame(index=pd.date_range(start=inst_q_6h_begin,end=inst_q_6h_end,freq='6h'))
        inst_q_6h.index.rename('datetime_local_tz',inplace=True)
        
        #Grab the nearest available the instananeous data within 15min of the 6hr timesteps
        inst_q_6h=pd.merge_asof(inst_q_6h,inst_q,left_index=True,right_index=True,tolerance=pd.Timedelta('15m'),direction='nearest')
        #Remove any values below zero
        inst_q_6h=inst_q_6h[inst_q_6h.Inst_Streamflow_cfs>0]
        
        #For timestep where instaneous data was not available, supplement with daily data
        inst_q_6h_merge=pd.DataFrame({'AdjustQ_Inst':inst_q_6h.Inst_Streamflow_cfs.combine_first(daily_q_6h)})
        
        ####################AdjustQ Mean Daily#################
        inst_q_6h_merge=self.adjustq_daily(inst_q_6h_merge,daily_q,max_iterations,error_tol)
        
        #Replace any negative flow value with zero
        inst_q_6h_merge['AdjustQ_Inst']=inst_q_6h_merge.AdjustQ_Inst.clip(lower=0)
        
        return inst_q_6h_merge.AdjustQ_Inst


#################Argument Declaration#################

########################
# currently not working
########################

# if __name__ == "__main__":

#     desc = "Emulates CHPS FEWS AdjustQ tranformation"
#     parser = argparse.ArgumentParser(description=desc,formatter_class=argparse.ArgumentDefaultsHelpFormatter)

#     # Add an argument with a default value
#     parser.add_argument('-o','--output_file', dest="output_file", type=str,  help='path to output csv file')
#     parser.add_argument('-d','--flow_daily', dest="flow_daily", type=str, help='path to daily flow csv')
#     parser.add_argument('-i','--flow_inst', dest="flow_inst", type=str, help='path to instantaneous flow csv')
#     parser.add_argument('-t','--interp_type', dest="interp_type", type=str, default="ratio" ,help='interp method must be specified as ratio or difference')
#     parser.add_argument('-b','--blend', dest="blend", type=int, default=10 ,help='number of consecutive missing inst observation to distinguish between using a interpolation or fill proceedure')
#     parser.add_argument('-e','--error_tol', dest="error_tol", type=float, default=0.01 ,help='error tollerance to correct subdaily data to match daily data')
#     parser.add_argument('-m','--max_iter', dest="max_iter", type=int, default=15 ,help='max number of iterations to correct subdaily data to match daily data')

#     # Parse the arguments
#     args = parser.parse_args()

#     #Output file
#     output_file = args.output_file
    
#     #Input files
#     flow_daily = args.flow_daily
#     flow_inst = args.flow_inst

#     #Options
#     interp_type = args.interp_type
#     blend = args.blend
#     error_tol = args.error_tol
#     max_iter = args.max_iter

#     adjustq_class = adjustq(flow_daily,flow_inst,interp_type, blend, error_tol, max_iter)

#     #write to csv
#     adjustq_class.get_adjustq().to_csv(output_file)