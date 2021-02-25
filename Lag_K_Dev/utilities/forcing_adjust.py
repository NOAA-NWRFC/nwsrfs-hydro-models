import os, time, sys, gc
import numpy as np, pandas as pd, scipy.stats as stats

# The objective of this tool is to adjust the climatology of the forcings.
# The px_adj, p_redist,std, and shift parameters should
# allow for the optimizer to explore the climatological
# space set by the supplemental data.

# The adjusted monthly climatological forcings can be used
# with the original climatological forcings to derive the adjustment factors
# to utilize when updating the 6hr forcings

# aorc_climo   Monthly climatology, with a column for each zone.
#              A dummy WY of 2099 is used for the date.
# px_adj       Multiplication factor to apply direction to the climo
#              range = []
# p_redist     the percentage of the annual forcing to redistribute
#              range = []
# std          Controls the weighting factor on how the p_redist is partioned
#              out to each month.  Larger number means it is evenly distributed to all months.
#              Smaller number means the distribution is favored to months with larger monthly totals
#              range = []
# shift        shift the climatological curve by x numbers of days in the positive
#              or negative direction.
# upper_limit  upper limit of allowable monthly climatological values,
#              developed using supplemental data. There is a column for each zone.
#              A dummy WY of 2099 is used for the date.
# lower_limit  upper limit of allowable monthly climatological values,
#              developed using supplemental data. There is a column for each zone.
#              A dummy WY of 2099 is used for the date.


##Tool for MAP and PET adjustments
def adj_climo_map_pet(aorc_climo, px_adj, p_redist, std, shift, upper_limit, lower_limit):
    
    # use 1/2 of a normal distribution to control the weighting
    x = np.linspace(1, 12, 12)
    weight=stats.norm.pdf(x, 1, std)
    weight=np.flip(weight/np.sum(weight))

    # create a blank DataFrame to add a column for each zone with the adjusted monthly climatology
    adj_line=pd.DataFrame(index=aorc_climo.index)

    # # single zone or multi-zone
    # if type(aorc_climo) == pd.core.series.Series:
    #     sites = aorc_climo.name
    # else:
    sites = aorc_climo.columns.array

    # Loop through each zone
    for site in sites:
        
        # Redistribute the precip##
        
        # Create a copy of the orginal climo and multiply by px_adj
        working_line=aorc_climo.loc[:,[site]].copy()*px_adj
        # Sort the px_adj climo by monthly value
        working_line.sort_values(by=site, axis=0, inplace=True)
        # Add a column of the weighting distribution
        working_line[site+'_weighting']=weight
        # Create a column of the month climo that will NOT be redistributed
        working_line[site+'_remaining_orig']=working_line[site]*(1-p_redist)
        # Create a column of the updated climo which is: the NONredistribed_mthly_climo PLUS
        # total_climo_to_be_redistributed*weighting_distribution
        working_line[site+'_redistributed']=\
            working_line[site+'_remaining_orig']+\
            working_line.loc[:,site].multiply(p_redist).sum()*working_line[site+'_weighting']
        # print(working_line[site + '_redistributed'])
        # Re-sort the dataframe by date
        working_line.sort_index(inplace=True)
        
        # #Apply shift###
        # print('Before shift')
        # print(working_line[site + '_redistributed'])
        # Add a copy (for interpolating purposes) of Oct and Sep climo to either end of the dataframe
        working_line=pd.concat([working_line.iloc[-1:].shift(-365, freq='d'),
                                working_line,working_line.iloc[:1].shift(365, freq='d')], axis=0)
        # Shift the dataframe by the specified number of days.
        # Also add interpolated entries to the 15th of each month using the shifted dates.
        # Remove the extra Sep/Oct values added in the previous line
        working_line_shift=pd.concat([
            pd.DataFrame(index=working_line.index),working_line.shift(shift, freq='d')],
            axis=0).sort_index().interpolate(method='time').loc['2098-10-15':'2099-9-15']
        # Only keep the interpolated dates on the 15th, remove any potential duplicate rows
        # from the shift
        working_line_shift=working_line_shift.loc[
            working_line_shift.index.day==15].drop_duplicates().dropna()

        # import pdb; pdb.set_trace()
        # print('After shift')
        # print(working_line_shift[site + '_redistributed'])
        
        # Enforce supplemental monthly data limits##
        
        # Check to see if any of the updated climo values violate the set thresholds.
        # If they do, then set to threshold
        upper_limit_exceeded=working_line_shift[site+'_redistributed'] > upper_limit[site]
        working_line_shift.loc[upper_limit_exceeded,[site+'_redistributed']]=\
            upper_limit.loc[upper_limit_exceeded,[site]].values
        lower_limit_exceeded=working_line_shift[site+'_redistributed'] < lower_limit[site]
        working_line_shift.loc[lower_limit_exceeded,[site+'_redistributed']]=\
            lower_limit.loc[lower_limit_exceeded,[site]].values
        
        adj_line=pd.concat([adj_line,working_line_shift],axis=1)

        # print('After limits')
        # print(adj_line[site + '_redistributed'])

    return adj_line
    
    
    
# Tool for MAT and PTPS adjustments
# note that this approach is a slight variation to the MAP/PET approach.
# Adjustment are divided into two.  Adjustments are made
# independently partitioned to the largest 6 months and independently
# partioned to the 6 smallest months
def adj_climo_mat_ptps(aorc_climo, px_adj,p_redist,std,shift,upper_limit,lower_limit):
    
    # use 1/2 of a normal distribution to control the weighting.
    # Use two curve to account for the separation of the 6 smallest/largest
    x = np.linspace(1, 6, 6)
    weight=stats.norm.pdf(x, 1, std)
    weight=np.append(weight,np.flip(weight))
    weight=weight/np.sum(weight)*2
    
    # create a blank DataFrame to add a column for each zone with the adjusted monthly climatology
    adj_line=pd.DataFrame(index=aorc_climo.index)
    
    # Loop through each zone
    for site in aorc_climo.columns.array:  

        # Create a copy of the orginal climo and multiply by px_adj
        working_line=aorc_climo.loc[:,[site]].copy()*px_adj
        # Sort the px_adj climo by monthly value
        working_line.sort_values(by=site, axis=0, inplace=True)
        # Find the deviation from median
        working_line[site+'_Deviation']=working_line.loc[:,[site]]-working_line.loc[:,[site]].median()
        # Add a column of the weighting distribution
        working_line[site+'_weighting']=weight
        # Create a column of the month climo that is to UNTOUCHED
        working_line[site+'_remaining_orig']=working_line[site+'_Deviation']*(1-p_redist)

        # print('deviation')
        # print(working_line[site + '_Deviation'])
        # get the total temperature to above the median and below median
        below_median_sum = working_line.iloc[0:6,
                           working_line.columns.get_loc(site+'_Deviation')].multiply(p_redist).sum()
        above_median_sum = working_line.iloc[6:12,
                           working_line.columns.get_loc(site+'_Deviation')].multiply(p_redist).sum()
        # print([below_median_sum, above_median_sum])
        # combine the total above/below tempearture to redistribute to a single column
        working_line[site+'_distribute'] = np.repeat([below_median_sum,above_median_sum],6)
        # print('before distributing')
        # print(working_line[site + '_distribute'])
        # Create a column of the updated climo which is: the untouched_mthly_climo +PLUS+
        # total_climo_to_be_redistributed*weighting_distribution
        working_line[site+'_redistributed'] = working_line.loc[:,[site]].median().values+\
                                              working_line[site+'_remaining_orig']+\
                                              working_line[site+'_distribute']*working_line[site+'_weighting']

        # print('After distributing')
        # print(working_line[site + '_redistributed'])

        # Resort the dataframe by date
        working_line.sort_index(inplace=True)
        
        #Add a copy (for interpolating purposes) of Oct and Sep climo to either end of the dataframe  
        working_line = pd.concat([
            working_line.iloc[-1:].shift(-365, freq='d'),
            working_line,working_line.iloc[:1].shift(365, freq='d')], axis=0)
        # Shift the dataframe by the specified number of days.  Also add interpolated entries
        # to the 15th of each month using the shifted dates. Remove the extra Sep/Oct values added
        # in the previous line
        working_line_shift = pd.concat([
            pd.DataFrame(index=working_line.index),
            working_line.shift(shift, freq='d')],
            axis=0).sort_index().interpolate(method='time').loc['2098-10-15':'2099-9-15']
        # Only keep the interpolated dates on the 15th, remove any potential duplicate rows from the shift
        working_line_shift = working_line_shift.loc[
            working_line_shift.index.day==15].drop_duplicates().dropna()

        #import pdb;
        #pdb.set_trace()

        # print('After shift')
        # print(working_line_shift[site + '_redistributed'])

        # Check to see if any of the updated climo values violate the set thresholds.
        # If they do, then set to threshold
        upper_limit_exceeded = working_line_shift[site+'_redistributed'] > upper_limit[site]
        working_line_shift.loc[upper_limit_exceeded,[site+'_redistributed']]=\
            upper_limit.loc[upper_limit_exceeded,[site]].values
        lower_limit_exceeded = working_line_shift[site+'_redistributed'] < lower_limit[site]
        working_line_shift.loc[lower_limit_exceeded,[site+'_redistributed']]=\
            lower_limit.loc[lower_limit_exceeded,[site]].values
        
        adj_line=pd.concat([adj_line,working_line_shift],axis=1)             

    return adj_line