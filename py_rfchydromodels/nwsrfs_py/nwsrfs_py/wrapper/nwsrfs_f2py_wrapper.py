import numbers, copy
from dataclasses import dataclass, fields
from typing import NewType
import pandas as pd
import numpy as np
from . import nwsrfs_src as nwsrfs_source
from .. import utils

@dataclass
class sacsnow_pars:

    '''
    Container for all inputs required to run the NWSRFS SAC-SMA and Snow17 models via F2PY bindings. 

    This class supports vectorized execution across multiple zones and timesteps simultaneously. 
    Input arrays should adhere to the following shape conventions:
    
    **Dimensions:**

    * **T**: Number of timesteps.
    * **Z**: Number of zones.
    * **N_pars**: Number of parameters (varies by model).

    **Array Shapes:**

    * **Time Arrays** (e.g., ``year``): Shape (T,)
    * **Forcings** (e.g., ``forcings_map``): Shape (T, Z)
    * **Scalar Parameters** (e.g., ``alat``): Shape (Z,)
    * **Vector Parameters** (e.g., ``sac_pars``, ``snow_pars``): Shape (N_pars, Z) - Axis 0 corresponds to the ordered parameter list.
        * For SAC-SMA: N_pars = 17
        * For Snow17: N_pars = 13

    Attributes:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        alat (np.ndarray): array of each zone's centroid latitude (units: decimal degrees).
        elev (np.ndarray): array of each zone's centroid elevation (units: m).
        sac_pars (np.ndarray): SAC model parameters (ordered array).
            0. **uztwm**: Upper zone tension water capacity (units: mm).
            1. **uzfwm**: Upper zone free water capacity (units: mm).
            2. **lztwm**: Lower zone tension water capacity (units: mm).
            3. **lzfpm**: Lower zone primary free water capacity (units: mm).
            4. **lzfsm**: Lower zone supplemental free water capacity (units: mm).
            5. **adimp**: Fraction of additional impervious area (units: fraction 0-1).
            6. **uzk**: Upper zone free water storage depletion coefficient (units: NA).
            7. **lzpk**: Lower zone primary free water storage depletion coefficient (units: NA).
            8. **lzsk**: Lower zone supplementalfree water storage depletion coefficient (units: NA).
            9. **zperc**: Maximum percolation rate multiplier (units: NA).
            10. **rexp**: Exponent for the percolation equation (units: NA).
            11. **pctim**: Minimum impervious area (units:  fraction 0-1).
            12. **pfree**: Fraction of percolated water which always goes directly to lower zone free water storages (units: fraction 0-1).
            13. **riva**: Fraction of riparian vegetation area (units:  fraction 0-1).
            14. **side**: Fraction of non-channel baseflow (deep groundwater recharge) to channel baseflow (units: fraction 0-1).
            15. **rserv**: Fraction of lower zone free water which cannot be transferred to lztw (units: fraction 0-1).
            16. **efc**:  fraction of effective forest cover (units: fraction 0-1).
        snow_pars (np.ndarray): Snow model parameters (ordered array).
            0. **scf**: Snowfall correction factor (units: NA).
            1. **mfmax**: Maximum non-rain melt factor per time step (units: mm/degc).
            2. **mfmin**: Minimum non-rain melt factor per time step (units: mm/degc).
            3. **uadj**: Average wind function per time step (units: mm/mb).
            4. **si**: SWE threshold above which there is always 100% snow cover (units: mm).
            5. **nmf**: Maximum negative melt factor per time step (units:  mm/degc).
            6. **tipm**: Antecedent snow temperature index parameter (units:  fraction 0-1).
            7. **mbase**: Base temperature for non-rain melt factor (units: degc).
            8. **plwhc**: Maximum amount of liquid waterheld against gravity drainage (units:  fraction 0-1).
            9. **daygm**: Daily melt at the snow-soil interface (units: mm).
            10. **adc_a**: Parameter used to calculate the areal depletion curve parameter [swe/ai = a * AESC**b+(1-a)*AESC**c] (units: NA).
            11. **adc_b**: Parameter used to calculate the areal depletion curve parameter  [swe/ai = a * AESC**b+(1-a)*AESC**c] (units: NA).
            12. **adc_c**: Parameter used to calculate the areal depletion curve parameter  [swe/ai = a * AESC**b+(1-a)*AESC**c] (units: NA).
        init_swe (np.ndarray): Initial snow water equivalent values (units: NA).
        pxadj (np.ndarray): Precipitation adjustment factor (units: NA).
        peadj (np.ndarray): Evapotranspiration adjustment factor (units: NA).
        forcings_map (np.ndarray):  Precipitation array for each timestep (units: mm).
        forcings_mat(np.ndarray): Air temperature array for each timestep (units: degc).
        forcings_ptps(np.ndarray):  Fraction of precipitation as snow array for each timestep (units: fraction 0-1).
        forcings_etd (np.ndarray): Evaporation demand array for each timestep (units: mm).
    '''
    
    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    hour: np.ndarray
    alat: np.ndarray
    elev: np.ndarray
    sac_pars: np.ndarray
    snow_pars: np.ndarray
    init_swe: np.ndarray
    pxadj: np.ndarray
    peadj: np.ndarray
    forcings_map: np.ndarray
    forcings_mat: np.ndarray
    forcings_ptps: np.ndarray
    forcings_etd: np.ndarray

    def __post_init__(self):

        #Convert dt_seconds, year, month, day, hour to integer dtype
        self.dt_seconds = int(utils._define_timestep_sec(self.year, self.month, self.day, self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  int, time_list)

        #convert SAC-SMA and Snow17 pars to double dtype
        pars_list = ['sac_pars','snow_pars','init_swe','peadj','pxadj'] 
        utils._dtype_conversion_batch(self,  np.float64, pars_list)

        #convert forcings to double dtype
        forcing_list = ['forcings_map','forcings_mat','forcings_ptps','forcings_etd']
        utils._dtype_conversion_batch(self,  np.float64, forcing_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)  

    def validate(self):

        #check that time step is postitive
        if self.dt_seconds <= 0:
            raise ValueError("dt_seconds must be a positive integer.")

        #check that year, month, day, hour are 1d arrays
        if not utils._validate_1d_array(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have a 1d shape")

        #check that year, month, day, hour have the same length
        if not utils._validate_array_length(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have the same length")

        #check that the time set equals the dt_seconds 
        if not utils._validate_timestep(self.dt_seconds,self.year,self.month,self.day,self.hour):
            raise ValueError("year, month, day, hour timestep must be consistent and equal to dt_sec")

        #check that the sac_pars and snow_pars are the correct length
        if self.sac_pars.shape[0] != 17:
            raise ValueError("sac_pars must have exactly 17 parameters")
        if self.snow_pars.shape[0] != 13:
            raise ValueError("snow_pars must have exactly 13 parameters")
       
        #check that sac-sma and snow17 parameters are 1d arrays.  
        #NOTE: "*" in front of self.sac_pars and self.snow_pars to unpack nested list
        if not utils._validate_1d_array(self.alat,self.elev,self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars):
            raise ValueError("SAC-SMA and Snow17 parameter arrays must have a 1d shape")

        #check that sac-sma and snow17 parameters have the same length
        if not utils._validate_array_length(self.alat,self.elev,self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars):
            raise ValueError("SAC-SMA and Snow17 parameter arrays must have the same length")

        #check that sac-sma and snow17 parameters are equal or greater than zero
        if np.vstack([self.alat,self.elev,self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars]).min() < 0:
            raise ValueError("SAC-SMA and Snow17 parameters must be equal or greater than zero")    

        #check that forcing inputs have same lengths as year, month, day, hour arrays
        if not utils._validate_array_length(self.year,self.forcings_map,self.forcings_mat,self.forcings_ptps,self.forcings_etd):
            raise ValueError("Forcings must have the same length as time arrays (year, month, day, hour)")

        #check that forcing inputs have the same number of zones as the parameters values
        #Note:  Checking first index only is adequate as nested numpy arrays cannont have a "jangled" shape
        if not utils._validate_array_length(self.alat, self.forcings_map[0],self.forcings_mat[0],self.forcings_ptps[0],self.forcings_etd[0]):
            raise ValueError("Each nested forcing list length must correspond to number of zones which is specified by the length of parameter arrays")

        #check that map forcing values are valid:
        if not utils._validate_positive_values(self.forcings_map):
            raise ValueError("MAP forcings values must be >= 0")

        #check that the ptps forcing values are valid:
        if (self.forcings_ptps.min() < 0) |  (1 < self.forcings_ptps.max()):
            raise ValueError("PTPS forcings values must be between 0 and 1")

#create a new type for sacsnow_tci output
SACSnowTCI = NewType('SACSnowTCI',pd.DataFrame)

class sacsnow():

    '''
    Class to run the NWSRFS SAC-SMA and Snow17 models via F2PY bindings.  Multiple SAC-SMA and Snow17 parameter sets can be ran simultaneously. 

    Args:
        sacsnow_pars (dataclass): Dataclass which contains all inputs to run SAC-SMA and Snow17
        validate (bool): Validate sacsnow_pars dataclass inputs are correct format/type. Default: True
    '''

    def __init__(self,
        pars_dataclass: sacsnow_pars,
        validate:bool = True):

        #Assign parameters
        self.sacsnow_pars = pars_dataclass

        #Validate sacsnow_pars
        if validate:
            self.sacsnow_pars.validate()

        self.__datetime = utils._datetime_conversion(self.sacsnow_pars.year, self.sacsnow_pars.month,
                                                self.sacsnow_pars.day, self.sacsnow_pars.hour).rename('datetime')

        #Set raw_output to None until run function is executed
        self.__raw_output = self.__raw_states_output =None

    def __run_wrapper(self):
        '''
        Runs sacsnow wrapper
        '''
        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.sacsnow_pars)

        self.__raw_output = nwsrfs_source.sacsnow(
            pars.dt_seconds, pars.year, pars.month, pars.day, pars.hour, 
            # general pars
            pars.alat, pars.elev,
            # sac pars
            pars.sac_pars,
            #pet and precp adjustments
            pars.peadj, pars.pxadj,
            # snow pars
            pars.snow_pars,
            # initial swe
            pars.init_swe,
            # forcings
            pars.forcings_map, pars.forcings_ptps, pars.forcings_mat,pars.forcings_etd,
            #Pass states option
            int(0))

    def __run_wrapper_states(self):
        '''
        Runs sacsnow wrapper and return states
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.sacsnow_pars)

        self.__raw_states_output = nwsrfs_source.sacsnow(
            pars.dt_seconds, pars.year, pars.month, pars.day, pars.hour, 
            # general pars
            pars.alat, pars.elev,
            # sac pars
            pars.sac_pars,
            #pet and precp adjustments
            pars.peadj, pars.pxadj,
            # snow pars
            pars.snow_pars,
            # initial swe
            pars.init_swe,
            # forcings
            pars.forcings_map, pars.forcings_ptps, pars.forcings_mat,pars.forcings_etd,
            #Pass states option
            int(1))

    @property
    def sacsnow_tci(self) -> pd.DataFrame:
        '''
        Returns total channel inflow (tci) as a DataFrame with a column for each zone (units: mm).
        '''

        if self.__raw_output is None:
            self.__run_wrapper()

        tci = pd.DataFrame(self.__raw_output[2],index=self.__datetime).add_prefix('tci_')

        return SACSnowTCI(tci)

    @property
    def sacsnow_states(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of DataFrames containing all model states with a column for each zone.
            The dictionary keys are:

            * **tci**: Total channel inflow (units: mm).
            * **map_pxadj**: Precipitation after pxadj applied (units: mm).
            * **etd_peadj**: Evaporation demand after peadj, efc, and aesc adjustments applied (units: mm).
            * **aet**: Actual evapotranspiration (units: mm).
            * **uztwc**: Upper zone tension water contents (units: mm).
            * **uzfwc**: Upper zone free water contents (units: mm).
            * **lztwc**: Lower zone tension water contents (units: mm).
            * **lzfsc**: Lower zone free supplemental water contents (units: mm).
            * **lzfpc**: Lower zone free primary water contents (units: mm).
            * **adimc**: Additional impervious area water contents (units: mm).
            * **roimp**: Impervious runoff prior to riparian vegetation adjustment (units:  mm).
            * **sdro**: Direct runoff prior to riparian vegetation adjustment (units:  mm).
            * **ssur**: Surface runoff prior to riparian vegetation adjustment (units:  mm).
            * **sif**: Interflow prior to riparian vegetation adjustment (units:  mm).
            * **bfs**: Baseflow supplemental runoff prior to riparian vegetation adjustment (units:  mm).
            * **bfp**: Baseflow primary runoff prior to riparian vegetation adjustment (units:  mm).
            * **swe**: Snow water equivalent (units: mm).
            * **aesc**: Areal exent of snow cover (units: fraction 0-1).
            * **neghs**: Snowpack heat deficit (units:  mm).
            * **liqw**: Liquid water held by snow against gravity drainage (units: mm).
            * **raim**: Total rain plus snowmelt (units: mm).
            * **psfall**:  Precipitation falling as snow after scf adjustment has been applied (units: mm).
            * **prain**: Precipitation falling as rain (units: mm).
        '''

        if self.__raw_states_output is None:
            self.__run_wrapper_states()

        state_param = ['map_pxadj','etd_peadj','tci','aet',
            'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc',
            'roimp', 'sdro', 'ssur', 'sif', 'bfs', 'bfp',
            'swe','aesc','neghs','liqw','raim','psfall','prain']
        sacsnow_states = {}
        
        for count, param in  enumerate(state_param):
            sacsnow_states[param] = pd.DataFrame(self.__raw_states_output[count], index=self.__datetime)

        return sacsnow_states

@dataclass
class lagk_pars:

    '''
    Container for all inputs required to run the NWSRFS Lag-K model via F2PY bindings.

    This class supports vectorized execution across multiple upstream reaches and timesteps simultaneously. 
    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.
    * **R**: Number of upstream reaches.

    **Array Shapes:**
    * **Time Arrays** (e.g., ``year``): Shape (T,)
    * **Upstream Reach** (e.g., ``qin``): Shape (T, R)
    * **Scalar Parameters** (e.g., ``tbl_keq_c``): Shape (R,)

    Attributes:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        tbl_lageq_a (np.ndarray): Parameter used to calculate the lag table [lag_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_lageq_b (np.ndarray): Parameter used to calculate the lag table [lag_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_lageq_c (np.ndarray): Parameter used to calculate the lag table [lag_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_lageq_d (np.ndarray): Parameter used to calculate the lag table [lag_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_keq_a (np.ndarray): Parameter used to calculate the k table [k_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_keq_b (np.ndarray): Parameter used to calculate the k table [k_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_keq_c (np.ndarray): Parameter used to calculate the k table [k_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_keq_d (np.ndarray): Parameter used to calculate the k table [k_table=a*(Q-d)**2+b*Q+c] (units: NA).
        tbl_lagmax (np.ndarray):  max lag value for lag table (units: hours).
        tbl_lagmin (np.ndarray):  min lag value for lag table (units: hours).
        tbl_kmax (np.ndarray):  max k value for k table (units: hours).
        tbl_kmin (np.ndarray):  min k value for k table (units: hours).
        tbl_qmax (np.ndarray):  max q value for both lag and k table (units: cfs).
        tbl_qmin (np.ndarray):  min q value for both lag and k table (units: cfs).
        init_co (np.ndarray):  Initial carry over (units: cfs).
        init_qin (np.ndarray):  Initial inflow (units: cfs).
        init_qout (np.ndarray):  Initial outflow (units: cfs).
        init_stor (np.ndarray):  Initial storage (units: cfs).
        qin (np.ndarray):  Input streamflows to route. (units: cfs).

    '''
    
    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    hour: np.ndarray
    tbl_lageq_a: np.ndarray
    tbl_lageq_b: np.ndarray
    tbl_lageq_c: np.ndarray
    tbl_lageq_d: np.ndarray
    tbl_keq_a: np.ndarray
    tbl_keq_b: np.ndarray
    tbl_keq_c: np.ndarray
    tbl_keq_d: np.ndarray
    tbl_lagmax: np.ndarray
    tbl_lagmin: np.ndarray
    tbl_kmax: np.ndarray
    tbl_kmin: np.ndarray
    tbl_qmax: np.ndarray
    tbl_qmin: np.ndarray
    init_co: np.ndarray
    init_qin: np.ndarray
    init_qout: np.ndarray
    init_stor: np.ndarray
    qin: np.ndarray

    def __post_init__(self):

        #Convert time steps to integers
        self.dt_hours = int(utils._define_timestep_sec(self.year, self.month, self.day, self.hour)/3600)

        #Convert all table parameters to double
        tbl_list = ['tbl_lageq_a','tbl_lageq_b','tbl_lageq_c','tbl_lageq_d',
                    'tbl_keq_a','tbl_keq_b','tbl_keq_c','tbl_keq_d',
                    'tbl_lagmax','tbl_lagmin',
                    'tbl_kmax','tbl_kmin',
                    'tbl_qmax','tbl_qmin'] 
        utils._dtype_conversion_batch(self,  np.float64, tbl_list)

        #Convert all init paramaters to double
        init_list = ['init_co', 'init_qin', 'init_qout', 'init_stor']
        utils._dtype_conversion_batch(self,  np.float64, init_list)

        #Convert input streamflow to double
        self.qin = utils._dtype_conversion(self.qin, np.float64)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):

        #check that time step is postitive
        if self.dt_hours <= 0:
            raise ValueError("dt_hours must be a positive integer.")

        #check that year, month, day, hour are 1d arrays
        if not utils._validate_1d_array(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have a 1d shape")

        #check that year, month, day, hour have the same length
        if not utils._validate_array_length(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have the same length")

        #check that the time set equals the dt_seconds 
        if not utils._validate_timestep(self.dt_hours*3600,self.year,self.month,self.day,self.hour):
            raise ValueError("year, month, day, hour timestep must be consistent and equal to dt_hours")

        #check that the table limits are 1d arrays
        if not utils._validate_1d_array(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must have a 1d shape")

        #check that table limits have the same length
        if not utils._validate_array_length(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must have the same length")

        #check that the table limits are positive
        if not utils._validate_positive_values(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must be >= 0")

        #check that the initial states are 1d arrays
        if not utils._validate_1d_array(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must have a 1d shape")

        #check that the initial states have the same length
        if not utils._validate_array_length(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must have the same length")

        #check that the initial states are positive
        if not utils._validate_positive_values(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must be >= 0")

        #check that the lag/k table equation parameter values are 1d arrays
        if not utils._validate_1d_array(self.tbl_lageq_a,self.tbl_lageq_b,self.tbl_lageq_c,self.tbl_lageq_d,self.tbl_keq_a ,self.tbl_keq_b,self.tbl_keq_c,self.tbl_keq_d):
            raise ValueError("Lag/k table equation parameter values must have a 1d shape")

        #check that the lag/k table equation parameter values are the same length
        if not utils._validate_array_length(self.tbl_lageq_a,self.tbl_lageq_b,self.tbl_lageq_c,self.tbl_lageq_d,self.tbl_keq_a ,self.tbl_keq_b,self.tbl_keq_c,self.tbl_keq_d):
            raise ValueError("Lag/k table equation parameter values must have the same length")

        #check that upstream flow inputs have the same number of zones as the parameters values
        #Note:  Checking first index only is adequate as nested numpy arrays cannont have a "jangled" shape
        if not utils._validate_1d_array(self.init_co, self.qin[0]):
            raise ValueError("Each nested upstream flow length must correspond to number of zones which is specified by the length of parameter arrays")

        #check that upstream flow inputs are valid:
        if self.qin.min() < 0:
            raise ValueError("Upstream flow values cannot be less than 0")

class lagk():

    '''
    Class to run the NWSRFS Lag-K models via F2PY bindings.  Multiple upstream routes can be ran simultaneously.

    Args:
        lagk_pars (dataclass): Dataclass which contains all inputs to run LagK.
        validate (bool): Validate lagk dataclass inputs are correct format/type. Default: True
    '''

    def __init__(self,
        pars_dataclass: lagk_pars,
        validate:bool = True):
        #,output_timestep: numbers.Number | None = None):)

        #Assign parameters
        self.lagk_pars = pars_dataclass 

        #Validate lagk_pars
        if validate:
            self.lagk_pars.validate()

        
        self.__datetime = utils._datetime_conversion(self.lagk_pars.year, self.lagk_pars.month, self.lagk_pars.day, self.lagk_pars.hour).rename('datetime')
        self._ita = self._itb = self.lagk_pars.dt_hours
        
        #!!LAGK CAN HAVE DIFFERENT INPUT/OUTPUT TIMESTEP BUT LAGK F90 WRAPPER IS CURRENTLY NOT SET UP TO ACCOMODATE!!
        # if output_timestep is None:
        #     self._ita = self._ita  = pars.dt_hours
        #     self.__datetime  = utils._datetime_conversion(pars.year, pars.month, pars.day, pars.hour)
        # elif isinstance(output_timestep, numbers.Number):
        #     self._ita  = pars.dt_hours
        #     self._itb = int(output_timestep)
        #     #here, need to resample
        #     dt_in = utils._datetime_conversion(pars.year, pars.month, pars.day, pars.hour)
        #     self.__datetime = pd.date_range(start=dt_in.iloc[0],end=dt_in.iloc[-1],freq=f'{str(itb)}H')

        #Set raw_output to None until run function is executed
        self.__raw_output = self.__raw_states_output = None

    def __run_wrapper(self):
        '''
        Runs lagk wrapper
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.lagk_pars)

        self.__raw_output = nwsrfs_source.lagk(
            #input and output timestep 
            self._ita,self._itb,
            #lag table equation fixed values
            pars.tbl_lageq_a,pars.tbl_lageq_b,pars.tbl_lageq_c,pars.tbl_lageq_d,
            #k table equation fixed values
            pars.tbl_keq_a,pars.tbl_keq_b,pars.tbl_keq_c,pars.tbl_keq_d,
            #lag,k,q max
            pars.tbl_lagmax,pars.tbl_kmax,pars.tbl_qmax,
            #lag,k,q min
            pars.tbl_lagmin,pars.tbl_kmin,pars.tbl_qmin,       
            #inital states
            pars.init_co,pars.init_qin,pars.init_qout,pars.init_stor,
            #upstream flow
            pars.qin,
            #Pass states option
            int(0))

    def __run_wrapper_states(self):
        '''
        Runs lagk and return states
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.lagk_pars)

        self.__raw_states_output = nwsrfs_source.lagk(
            #input and output timestep 
            self._ita,self._itb,
            #lag table equation fixed values
            pars.tbl_lageq_a,pars.tbl_lageq_b,pars.tbl_lageq_c,pars.tbl_lageq_d,
            #k table equation fixed values
            pars.tbl_keq_a,pars.tbl_keq_b,pars.tbl_keq_c,pars.tbl_keq_d,
            #lag,k,q max
            pars.tbl_lagmax,pars.tbl_kmax,pars.tbl_qmax,
            #lag,k,q min
            pars.tbl_lagmin,pars.tbl_kmin,pars.tbl_qmin,       
            #inital states
            pars.init_co,pars.init_qin,pars.init_qout,pars.init_stor,
            #upstream flow
            pars.qin,
            #Pass states option
            int(1))

    @property
    def lagk_route(self) -> pd.DataFrame:
        '''
        Returns routed flows as a DataFrame with a column for each upstream reach (units: cfs).
        '''

        if self.__raw_output is None:
            self.__run_wrapper()

        routed = pd.DataFrame(self.__raw_output[0],index=self.__datetime).add_prefix('routed_')

        return routed

    @property
    def lagk_states(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of DataFrames containing all model states with a column for each upstream reach.
            The dictionary keys are:

            * **routed**: Routed flow with lag and k applied (units: cfs).
            * **lag_time**: Lag applied to upstream flow (units: hours).
            * **k_inflow**: upstreamflow with only lag applied  (units: cfs).
            * **k_storage**: Attenuation storage (units: cfs).
        '''

        if self.__raw_states_output is None:
            self.__run_wrapper_states()

        state_param = ['routed','lag_time','k_inflow','k_storage']
        lagk_states = {}

        for count, param in  enumerate(state_param):
            lagk_states[param] = pd.DataFrame(self.__raw_states_output[count], index=self.__datetime)

        return lagk_states

@dataclass
class consuse_pars:

    '''
    Container for all inputs required to run the NWSRFS CONS_USE model via F2PY bindings.  

    Only a single CONS_USE zone can be ran at a time.

    This class supports vectorized execution across all timesteps simultaneously. However only a single CONS_USE zone can be ran at a time.
    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.

    **Array Shapes:**
    * **Time Arrays and Streamflow** (e.g., ``year``, ``qin``): Shape (T,)
    
    Attributes:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        area (numbers.Number): Area of CONS_USE zone (units: km**2).
        irr_eff (numbers.Number): Irrigation efficiency (units:  fraction 0-1).
        min_flow (numbers.Number): Minimum flow (units: cfs).
        rf_accum_rate (numbers.Number): Return flow accumulation rate (units:  fraction 0-1).
        rf_decay_rate (numbers.Number): Return flow decay rate (units: NA).
        pet_adj (numbers.Number):  Potential evaporation (PET) adjustment factor (units: NA).
        pet_adj_table (np.ndarray):  Monthly pe adjust table (units: NA).
        pet (np.ndarray):  Input CONS_USE zone pet (units:  mm).
        qin (np.ndarray):  Input daily average streamflow to adjust for CONS_USE (units: cfs).
    '''

    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    area: numbers.Number
    irr_eff: numbers.Number
    min_flow: numbers.Number
    rf_accum_rate: numbers.Number
    rf_decay_rate: numbers.Number
    pet_adj: numbers.Number
    pet_adj_table :np.ndarray
    pet: np.ndarray
    qin: np.ndarray

    def __post_init__(self):


        #Convert dt_seconds, year, month, day, hour to integer dtype
        time_list = ['year','month','day'] 
        utils._dtype_conversion_batch(self,  int, time_list)

        #Convert all CONS_USE parameters to double
        param_list = ['area','irr_eff','min_flow','rf_accum_rate',
                    'rf_decay_rate','pet_adj','pet_adj_table']
        utils._dtype_conversion_batch(self,  np.float64, param_list)

        #Convert all input timeseries to double
        ts_list = ['pet', 'qin']
        utils._dtype_conversion_batch(self,  np.float64, ts_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):

        #check that year, month, day are 1d arrays
        if not utils._validate_1d_array(self.year, self.month, self.day):
            raise ValueError("Time arrays (year, month, day, hour) must have a 1d shape")

        #check that year, month, day have the same length
        if not utils._validate_array_length(self.year, self.month, self.day):
            raise ValueError("Time arrays (year, month, day) must have the same length")

        #check that pet_adj_table is a 1d array.  
        if not utils._validate_1d_array(self.pet_adj_table):
            raise ValueError("pet_adj_table array must have a 1d shape")

        #check that pet_adj_table has a lengh of 12.  
        if len(self.pet_adj_table) != 12:
            raise ValueError("peadj_table array must have a length of 12")

        #check that CONS_USE  parameters are a single value
        if  0 != sum([x.ndim for x in [self.area,self.irr_eff,self.min_flow, self.rf_accum_rate, self.rf_decay_rate, self.pet_adj]]):
            raise ValueError("All CONS_USE  parameters (area,irr_eff,min_flow,rf_accum_rate,decay_rate,pet_adj) must be a single value")

        #check that CONS_USE  parameters are equal or greater than zero
        if np.vstack([self.area,self.irr_eff,self.min_flow, self.rf_accum_rate, self.rf_decay_rate, self.pet_adj]).min() < 0:
            raise ValueError("All CONS_USE parameters (area,irr_eff,min_flow,rf_accum_rate,decay_rate,pet_adj) must be greater than zero")    

        #check that streamflow and PET inputs are 1d arrays
        if not utils._validate_1d_array(self.pet,self.qin):
            raise ValueError("Streamflow and PET array inputs must have a 1d shape")

        #check that streamflow and PET inputs have same lengths as year, month, day arrays
        if not utils._validate_array_length(self.year,self.pet,self.qin):
            raise ValueError("Streamflow and PET array inputs must have the same length as time arrays (year, month, day)")

        #check that streamflow and PET array input are valid:
        if not utils._validate_positive_values(self.pet) or not utils._validate_positive_values(self.qin):
            raise ValueError("QIN and PET array inputs values must be >= 0")

class consuse():

        '''
        Function to run the NWSRFS CONS_USE model via F2PY bindings.   Only a single consuse zone can be ran at a time.  Timestep is daily average.

        Args:
            consuse_pars (dataclass): Dataclass which contains all inputs to run CONS_USE.
            validate (bool): Validate CONS_USE dataclass inputs are correct format/type. Default: True
        '''
        def __init__(self,
            pars_dataclass: consuse_pars,
            validate:bool = True):

            #Assign parameters
            self.consuse_pars = pars_dataclass

            #Validate consuse_pars
            if validate:
                self.consuse_pars.validate()

            self.__datetime = utils._datetime_conversion(self.consuse_pars.year, self.consuse_pars.month, self.consuse_pars.day).rename('date')

            #Set raw_output to None until run function is executed
            self.__raw_output = None

        def __run_wrapper(self):
            '''
            Runs consuse wrapper
            '''

            #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
            pars = copy.deepcopy(self.consuse_pars)

            self.__raw_output = nwsrfs_source.consuse(pars.year, pars.month, pars.day,
                                 pars.area, pars.irr_eff, pars.min_flow,
                                 pars.rf_accum_rate,pars.rf_decay_rate,
                                 pars.pet_adj_table,pars.pet_adj,
                                 pars.pet,pars.qin)

        @property
        def consuse_qadj(self) -> pd.Series:
            '''
            Returns a Series of daily streamflow with consumptive use adjustments applied (units: cfsd).
            '''

            if self.__raw_output is None:
                self.__run_wrapper()

            qadj_daily = pd.Series(self.__raw_output[0], index=self.__datetime,name='qadj')

            return qadj_daily

        @property
        def consuse_states(self) -> pd.DataFrame:
            '''
            Returns a DataFrame with a column for each CONS_USE model states.
                The column states are:

                * **qadj**: Streamflow with all consumptive use adjustments applied (units: cfsd)
                * **qdiv**: Flow diverted for consumptive use (units: cfsd)
                * **qrf_in**:  Return flow from irrigation area used as input to return flow storage (units:cfsd)
                * **qrf_out**: Return flow to channel from return flow storage (units: cfsd)
                * **qol**:  Diverted flow other loses [eg transport, subsurface] (units:  cfsd)
                * **qcd**: Crop flow demand (units:  cfsd) 
                * **ce**: Crop evapotranspiration demand (units: mm)
                * **rfstor**: Return flow storage contents (units: mm)
            '''

            if self.__raw_output is None:
                self.__run_wrapper()


            state_param=['qadj','qdiv','qrf_in','qrf_out','qol','qcd','ce','rfstor']
            
            state_dic = {key: array for key, array in zip(state_param, self.__raw_output)}

            return pd.DataFrame(state_dic, index=self.__datetime)

@dataclass
class chanloss_pars:

    '''
    Container for all inputs required to run the NWSRFS CHANLOSS model via F2PY bindings.

    This class supports vectorized execution across multiple chanloss periods and timesteps simultaneously. 
    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.
    * **C**: Number of chanloss periods.

    **Array Shapes:**
    * **Time Arrays and Streamflow** (e.g., ``year``, ``qin``): Shape (T,)
    * **Chanloss Factors** (e.g., ``factors``): Shape (C, )
    * **Chanloss Periods** (e.g., ``periods``): Shape (C,2)

    Attributes:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours for each timestep (units: time).
        factors (np.ndarray): The CHANLOSS factors to be applied for each period (if cl_type=1 units: NA, otherwise units: cfs)
        periods (np.ndarray): The beginning and ending months that the CHANLOSS factors are applied (units: month).
        cl_type (numbers.Number):  1=varp (percentage of flow), 2=varc (constant value)
        min_flow (numbers.Number):  Minimum required qin flow for CHANLOSS to be applied (units: cfs).
        qin (np.ndarray):  Input streamflow to adjust for CHANLOSS (units: cfs).
    '''

    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    hour: np.ndarray
    periods: np.ndarray
    factors: np.ndarray
    cl_type: numbers.Number
    min_flow: numbers.Number
    qin: np.ndarray

    def __post_init__(self):

        #Convert time steps to integers
        self.dt_seconds = int(utils._define_timestep_sec(self.year, self.month, self.day, self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  int, time_list)

        #Convert factors, min_flow, and qin to double dtypes
        dbl_list = ['factors','min_flow','qin']
        utils._dtype_conversion_batch(self,  np.float64, dbl_list)

        #Convert periods to integer
        int_list = ['periods','cl_type']
        utils._dtype_conversion_batch(self,  int, int_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):

        #check that year, month, day are 1d arrays
        if not utils._validate_1d_array(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have a 1d shape")

        #check that year, month, day have the same length
        if not utils._validate_array_length(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day) must have the same length")

        #check that periods shape contains the correct shape
        if len(self.periods[0])!=2:
            raise ValueError("periods parameter shape must be n x 2")

        #check that cl_type parameter is 1 or 2
        if not self.cl_type ==1 and not self.cl_type ==2:
            raise ValueError("cl_type must have a integer value of 1 or 2")

        #check that streamflow input and factor parameters are 1d arrays
        if not utils._validate_1d_array(self.qin,self.factors):
            raise ValueError("Streamflow and factors array inputs must have a 1d shape")

        #check that streamflow input has same lengths as year, month, day arrays
        if not utils._validate_array_length(self.year, self.qin):
            raise ValueError("Streamflow input must have the same length as time arrays (year, month, day)")

        #check that qin and PET array input are valid:
        if not utils._validate_positive_values(self.qin):
            raise ValueError("Streamflow inputs values must be >= 0")


class chanloss:

    '''
     Class to run the NWSRFS chanloss model via F2PY bindings.

    Args:
        chanloss_pars (dataclass): Dataclass which contains all inputs to run CHANLOSS.
        validate (bool): Validate chanloss dataclass inputs are correct format/type. Default: True
    '''
    def __init__(self,
            pars_dataclass: chanloss_pars,
            validate:bool = True):

        #Assign parameters
        self.chanloss_pars = copy.deepcopy(pars_dataclass)

        #Validate chanloss_pars
        if validate:
            self.chanloss_pars.validate()

        self.__datetime = utils._datetime_conversion(self.chanloss_pars.year, self.chanloss_pars.month, self.chanloss_pars.day, self.chanloss_pars.hour).rename('datetime')

        #Set raw_output to None until run function is executed
        self.__raw_output = None

    def __run_wrapper(self):
        '''
        Runs chanloss wrapper
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.chanloss_pars)

        self.__raw_output = nwsrfs_source.chanloss(pars.dt_seconds, pars.year, pars.month, pars.day,
                                    pars.factors,pars.periods,pars.cl_type, pars.min_flow,
                                    pars.qin)

    @property
    def chanloss_qadj(self) -> pd.Series:
        '''
        Returns a Series of streamflow with chanloss adjustments applied (units: cfs).
        '''

        if self.__raw_output is None:
            self.__run_wrapper()

        qin_adj = pd.Series(self.__raw_output, index=self.__datetime, name='qin_adj')

        return qin_adj

@dataclass
class fa_pars:

    '''
    Container for all inputs required to apply monthly climatological forcing adjustments to either MAP, MAT, PTPS, or PET.  This dataclass is designed to handle multiple zones.

    This dataclass is designed to be generic to all four forcing types. The units for ``limits``, ``forcings``, 
    and ``climo`` depend on the forcing type being processed:

    * **map** (Precipitation): mm
    * **mat** (Temperature): deg C
    * **ptps** (Precip Typing): fraction (0-1)
    * **pet** (Potential Evap): mm

    pet is calculated using a temperature index approach and does not require the ``forcing`` argument passed. ``None`` should be passed instead.

    This class supports vectorized execution across multiple zones and timesteps simultaneously. 
    Input arrays should adhere to the following shape conventions:
    
    **Dimensions:**

    * **T**: Number of timesteps.
    * **Z**: Number of zones.
    * **N_pars**: Number of fa parameters (4).

    **Array Shapes:**

    * **Time Arrays** (e.g., ``year``): Shape (T,)
    * **Forcings** (e.g., ``forcings``): Shape (T, Z)
    * **Scalar Parameters** (e.g., ``alat``): Shape (Z,)
    * **Vector Parameters** (e.g., ``pars``): Shape (N_pars, Z) - Axis 0 corresponds to the ordered parameter list.
    * **Climo Parameter** (e.g., ``climo``): Shape (12, ) - A value for each month (Jan-Dec)    

    Attributes:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        pars (np.ndarray): Contains the forcing adjustment parameters (ordered array).
            0. **scale**:  Multiplication factor to apply directly to the forcing (units: NA).
            1. **p_redist**: The percentage of the climatological forcing to redistribute (units: fraction 0-1).
            2. **std**:  Controls the weighting factor on how the p_redist is partitioned out to each climatological month based on ranking (units: NA).
            3. **shift**: Shift the climatological values by x numbers of days in the positive or negative (units: days)
        area (np.ndarray): Array of each zone's area (units: km**2).
        alat (np.ndarray): Array of each zone's centroid latitude (units: decimal degrees).
        limits (np.ndarray): Contains the forcing adjustment upper and lower limits for each of the 12 months. Units vary by forcing type (see above).
        forcings (np.ndarray | None):  Input forcings to adjust using pars and limits. ``None`` must be passed for pet.  Units vary by forcing type (see above).
        peadj_m (np.ndarray | None):  Input required for PET forcing adjustment.  Contains potential evaporation adjustment factors [pet -> etd] for each of the 12 months [Not required for other forcing types] (units: NA).
        climo (np.ndarray | None): Optional input to provide the monthly climatology which will be used the limits parameter to establish allowable adjustments.  Otherwise climatology 
                                   will be calculated with ``forcings`` inputs.  Units vary by forcing type (see above).
    '''

    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    hour: np.ndarray
    pars: np.ndarray
    area: np.ndarray
    alat: np.ndarray
    limits: np.ndarray
    forcings: np.ndarray | None = None
    peadj_m: np.ndarray | None = None
    climo:  np.ndarray | None = None

    def __post_init__(self):

        #Convert time steps to integers
        self.dt_seconds = int(utils._define_timestep_sec(self.year, self.month, self.day,self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  int, time_list)

        #Convert pars,area, alat, limits, forcings to double
        fa_inputs = ['pars', 'area', 'alat', 'limits']
        utils._dtype_conversion_batch(self,  np.float64, fa_inputs)

        #if forcings exists, then convert to double
        if self.forcings is not None:
            self.forcings = utils._dtype_conversion(self.forcings, np.float64)

        #if climo exists, then convert to double
        if self.climo is not None:
            self.climo = utils._dtype_conversion(self.climo, np.float64)

        #if peadj_m exists, then convert to double
        if self.peadj_m is not None:
            self.peadj_m = utils._dtype_conversion(self.peadj_m, np.float64)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):

        #check that year, month, day, and hour are 1d arrays
        if not utils._validate_1d_array(self.year, self.month, self.day):
            raise ValueError("Time arrays (year, month, day, hour) must have a 1d shape")

        #check that year, month, day have the same length
        if not utils._validate_array_length(self.year, self.month, self.day, self.hour):
            raise ValueError("Time arrays (year, month, day, hour) must have the same length")

        #check that pars, area, alat are a 1d array
        if not utils._validate_1d_array(self.pars, self.area, self.alat):
            raise ValueError("pars, area, and alat arrays must have a 1d shape")

        #check that the pars attribute has a length of 4
        if len(self.pars) != 4:
           raise ValueError("pars parameter must have a length of 4 (scale,p_redist,std,shift)")

        if not utils._validate_array_length(self.area,self.alat):
            raise ValueError("area and alat must have the same length to represent each zone")

        #check that area array values are valid
        if self.area.min() <= 0:
            raise ValueError("Valid area values must be passed to dataclass (area>0)")

        #check that alat array values are valid
        if np.absolute(self.alat).max() > 90:
            raise ValueError("Valid alat values must be passed to dataclass (-90<alat<90)")

        #check that limits has the correct shape 
        if self.limits.shape != (12,2):
            raise ValueError("limits parameter shape must be 12 x 2")

        #validation check for forcings if exists
        if self.forcings is not None:
            #check that forcings inputs have the same number of zones as the zone and alat arrays
            #Note:  Checking first index only is adequate as nested numpy arrays cannont have a "jangled" shape
            if not utils._validate_array_length(self.area, self.forcings[0]):
                raise ValueError("forcings input must have same number of zones as the area attribute (forcings[0]==area)")
            #check that forcings input has same lengths as year, month, day arrays
            if not utils._validate_array_length(self.year, self.forcings):
                raise ValueError("forcings input must have the same length as time arrays (year, month, day, hour)")

        #validation check for climo if exists
        if self.climo is not None:
            #check that climo is a 1d array
            if not utils._validate_1d_array(self.climo):
                raise ValueError("climo parameter must have a 1d shape")
            #check that climo has a length of 12
            if len(self.climo) != 12:
                raise ValueError("climo parameter must have a length of 12")

        #validation check for peadj_m if exists
        if self.peadj_m is not None:
            #check that peadj_m has the same number of zones as forcings
            if not utils._validate_array_length(self.area, self.peadj_m[0]):
                raise ValueError("peadj_m parameter must have same number of zones as area attribute (peadj_m[0]==area)")
            #check that peadj_m has a length of 12
            if len(self.peadj_m) != 12:
                raise ValueError("peadj_m parameter must have a length of 12")

class fa():
    '''
    Class to apply monthly climatological forcing adjustments to forcings via F2PY bindings.

    Args:
        map_dataclass (fa_pars): Dataclass containing inputs for precipitation adjustments.
        mat_dataclass (fa_pars): Dataclass containing inputs for temperature adjustments.
        ptps_dataclass (fa_pars): Dataclass containing inputs for precipitation typing adjustments.
        pet_dataclass (fa_pars): Dataclass containing inputs for potential evaporation adjustments.
        validate (bool): Validate that map, mat, ptps, and pet dataclass inputs are correct format/type. Default: True
    '''

    def __init__(self,
        map_dataclass: fa_pars,
        mat_dataclass: fa_pars,
        ptps_dataclass: fa_pars,
        pet_dataclass: fa_pars,
        validate:bool = True):

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        self.fa_pars = {'map':map_dataclass,'mat':mat_dataclass,'ptps':ptps_dataclass, 'pet':pet_dataclass}

        #Vaidate inputs
        if validate:
            self.__validate()

        #Create climo fa inputs
        self.__create_climo()

        #Set raw_output to None until run function is executed
        self.__raw_output = None

    def __validate(self):
        '''
        Validate class inputs, utilze fa_pars validation functionality
        '''

        #Run each fa dataclass through its validation function
        for key, fa_dc in self.fa_pars.items():
            try:
                fa_dc.validate()
            except ValueError as e:
                error_message = f"Validation error with {key}_dataclass: {e}"
                raise RuntimeError(error_message) from e

        #Check that ALL forcings dataclass did not pass a climo value or ALL did
        climo_types = {type(fa_dc.climo) for fa_dc in self.fa_pars.values()}
        if len(climo_types) > 1:
            raise ValueError("A mixed use of the climo attribue with the passed fa_pars is not allowed")

        #check that map forcings values are valid:
        if not utils._validate_positive_values(self.fa_pars['map'].forcings):
            raise ValueError("map forcings values must be >= 0")

        #check that the ptps forcing values are valid:
        if (self.fa_pars['ptps'].forcings.min() < 0) |  (1 < self.fa_pars['ptps'].forcings.max()):
            raise ValueError("ptps forcings values must be between 0 and 1")

        #check that peadj_m exists
        if self.fa_pars['pet'].peadj_m is None:
            raise ValueError("peadj_m must be passed by the pet_dataclass")

        #check that pet forcings is not passed
        if self.fa_pars['pet'].forcings is not None:
            raise ValueError("pet forcings cannot be passed")

        time_validate = []
        #Check the time arrays are all the same
        for time_attribute in ['year', 'month', 'day', 'hour']:
            time_all_same = all(np.array_equal(getattr(self.fa_pars['map'],time_attribute),getattr(fa_time,time_attribute)) for fa_time in self.fa_pars.values())
            time_validate.append(time_all_same)
        if not all(time_validate):
            raise ValueError("All time arrays (year, month, day, hour) associated with forcings values must represent equal date ranges")

    def __create_climo(self):
        '''
        Creates ``climo`` attribute, uses dummy values of -9999 if no climo inputs are provided
        '''
        #create a climo array.  Only checking MAP for climo data, assuming validate caught any climo mismatch amongst forcings
        if self.fa_pars['map'].climo is not None:
            self.__climo_array = np.column_stack([self.fa_pars['map'].climo, self.fa_pars['mat'].climo, self.fa_pars['pet'].climo,self.fa_pars['ptps'].climo])
        else:
            #Make dummy climo input
            self.__climo_array = np.full((12, 4), -9999,dtype=np.float64)
        self.__climo_array = np.asfortranarray(self.__climo_array)

    def __run_wrapper(self):
        '''
        runs monthly climatological forcing adjustment wrapper
        '''
        pars = copy.deepcopy(self.fa_pars)

        #arbitrary decision to use map for time, lat, and area varibles.  MAT, PTPS, or PET could have been used 
        self.__raw_output = nwsrfs_source.fa_ts(pars['map'].dt_seconds,pars['map'].year,pars['map'].month,pars['map'].day,pars['map'].hour,
                                pars['map'].alat,pars['map'].area,
                                pars['pet'].peadj_m,
                                pars['map'].pars,pars['mat'].pars,pars['pet'].pars,pars['ptps'].pars,
                                pars['map'].limits,pars['mat'].limits,pars['pet'].limits,pars['ptps'].limits,
                                self.__climo_array,
                                pars['map'].forcings,pars['ptps'].forcings,pars['mat'].forcings)
        
    @property
    def forcings(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of adjusted forcings as DataFrames. 

        The dictionary keys are:
        * **map_fa**: Precipitation (units: mm) 
        * **mat_fa**: Air temperature (units: degc)
        * **ptps_fa**: Fraction of precipitation as snow (units: fraction 0-1)
        * **pet_fa**: Potential evaporation (units: mm)
        * **etd_fa**: Evaporation demand (units: mm)
        '''

        #If the run function has yet to be executed, run it
        if self.__raw_output is None:
            self.__run_wrapper()

        #Calculate datetime
        datetime = utils._datetime_conversion(self.fa_pars['map'].year, self.fa_pars['map'].month, self.fa_pars['map'].day, self.fa_pars['map'].hour).rename('datetime')

        #Create a dictionary of DataFrames for output purposes
        fa_param=['map_fa','mat_fa','ptps_fa','pet_fa','etd_fa']
        fa_dict={}
        for count, param in  enumerate(fa_param):
            fa_dict[param]=pd.DataFrame(self.__raw_output[count+5], index=datetime)
        
        return fa_dict

    @property
    def fa_factors(self) -> pd.DataFrame:
        '''
        Returns a DataFrame (index 1-12) of monthly adjustment factors (index 1-12).
        
        The columns correspond to:
        * **map_fac**: Monthly precipitation adjustment factors (units: NA)
        * **mat_fac**: Monthly temperature adjustment shifts (units: degc)
        * **pet_fac**: Monthly potential evaporation adjustment factors (units: NA)
        * **ptps_fac**: Monthly precip-as-snow adjustment factors (units: NA)
        '''

        #If the run function has yet to be executed, run it
        if self.__raw_output is None:
            self.__run_wrapper()

        #Ignore first output aray as it is climo not a fac adjustment
        fa_run = self.__raw_output[1:5]

        #Create a DataFrame with a column for each factor
        fac_parameters=['map_fac','mat_fac','pet_fac','ptps_fac']
        fac_df=pd.DataFrame(columns=fac_parameters,index=range(1,13))
        for i, col in enumerate(fac_parameters,0):
            fac_df[col]=fa_run[i]

        return fac_df

    @property
    def forcings_climo(self) -> pd.DataFrame:
        '''
        12-month climatology values as DataFrame (index 1-12). 
        
        The columns correspond to:
        * **map_climo**: Monthly precipitation climatology (units: mm/month total)
        * **mat_climo**: Monthly temperature climatology (units: degc monthly avg)
        * **pet_climo**: Monthly potential evaporation climatology (units: mm/month)
        * **ptps_climo**: Monthly precip-as-snow climatology (units: fraction 0-1 monthly avg)
        '''

        #If the run function has yet to be executed, run it
        if self.__raw_output is None:
            self.__run_wrapper()

        return pd.DataFrame(self.__climo_array,columns=['map_climo','mat_climo','pet_climo','ptps_climo'])


@dataclass
class gammauh_pars:

    '''
    Container for all inputs required to create a gamma unit hydrograph models via F2PY bindings. Multiple gamma unit hydrograph parameter sets can be ran simultaneously. 

    This class supports vectorized execution across multiple zones. 
    Input arrays should adhere to the following shape conventions:
    
    **Dimensions:**

    * **Z**: Number of zones.

    **Array Shapes:**

    * **Parameters** (e.g., ``shape``): Shape (Z,)

    Attributes:
        dt_hours (np.ndarray): UH timestep. (units: hours)
        area (np.ndarray): Array of each zone's area. (units:  km**2)
        shape (np.ndarray): Array of each zone's shape parameter. (units:  km**2)
        scale (np.ndarray | None): Optional array of each zone's scale parameter.  If scale is None, toc parameter must be provided. (units: NA)
        toc  (np.ndarray | None): Optional array of each zone's time of concentration (toc) parameter.  If toc is None, the scale parameter must be provided. (units: hours)
    '''
    
    dt_hours: numbers.Number
    area: np.ndarray
    shape: np.ndarray
    scale: np.ndarray | None = None
    toc:  np.ndarray | None = None

    def __post_init__(self):


        #Convert dt_hours to float dtype
        self.dt_hours = np.float64(self.dt_hours)

        #Convert area and shape to double dtype
        pars_list = ['area','shape']
        utils._dtype_conversion_batch(self,  np.float64, pars_list)

        #if scale exists, then convert to double
        if self.scale is not None:
            self.scale = utils._dtype_conversion(self.scale, np.float64)

        #if peadj_m exists, then convert to double
        if self.toc is not None:
            self.toc = utils._dtype_conversion(self.toc, np.float64)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):

        #check that time step is postitive
        if self.dt_hours <= 0:
            raise ValueError("dt_hours must be a positive integer.")

        #check if either the scale or toc parameter is provided
        scale_exist = self.scale is not None
        toc_exists = self.toc is not None
        if scale_exist == toc_exists:
            raise ValueError("Either the scale or toc parameter must be provided, not both")

        #validation check for scale or toc parameter
        if scale_exist:
            self._validate_batch('scale')
        else:
            self._validate_batch('toc')

    def _validate_batch(self,opt_var: str):

        opt_attr = getattr(self, opt_var)
        #check that all uh parameters are 1d array
        if not utils._validate_1d_array(self.area,self.shape,opt_attr):
            raise ValueError(f"Area, shape, and {opt_var} parameters must have a 1d shape")
        #check that all uh parameters has the same length
        if not utils._validate_array_length(self.area,self.shape,opt_attr):
            raise ValueError(f"Area, shape, and {opt_var} parameter arrays must have the same length")
        #check that uh parameters are greater than zero
        if np.vstack([self.area,self.shape,opt_attr]).min() <= 0:
            raise ValueError(f"Area, shape, and {opt_var} parameter parameters must be greater than zero")

class gamma_uh():
    '''
    Class to generate Gamma Unit Hydrographs and route flows via F2PY bindings.
    Args:
        pars_dataclass (dataclass): Dataclass which contains all inputs needed for gamma unit hydrograph calculations. 
        validate (bool): Validate gammauh dataclass inputs are correct format/type. Default: True
    Attributes:
        gamma_uh_pars (dataclass): Dataclass which contains all inputs needed for gamma unit hydrograph calculations. 
    '''
    def __init__(self, 
                pars_dataclass: gammauh_pars, 
                validate:bool=True):

        #Assign parameters
        self.gamma_uh_pars = pars_dataclass

        #Validate gammauh_pars
        if validate:
            self.gamma_uh_pars.validate()

        #Get the number of zones
        self.__n_zones = len(self.gamma_uh_pars.shape)

        #If scale parameter is none, calcuate it
        if self.gamma_uh_pars.scale is None:
            self._get_shape_par()   
        else:
            self.__scale = self.gamma_uh_pars.scale

        #Create attribute to track it uh has been ran
        self.__uh = None

    #calculate the scale parameter for each zone
    def _get_shape_par(self):

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gamma_uh_pars)

        self.__scale = np.zeros(self.__n_zones)
        for i in range(self.__n_zones):
            shape = pars.shape[i]
            toc = pars.toc[i]
            self.__scale[i] = nwsrfs_source.uh2p_get_scale_root(shape,toc,np.float64(1))

    def return_uh(self,
                tstep):
        '''
        Returns a unit hydrograph as a DataFrame at a timestep specified by ``tstep``.
        Args:
            tstep (int): Specifies tstep of unit hydrograph to return (units: hours). 
        Returns:
             pd.DataFrame: A DataFrame containing unit hydrograph (uh) with a column for each zone (units: cfs).
        '''

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gamma_uh_pars)

        #convert tstep to integer
        tstep = int(tstep)

        #Calculate the uh for each zone
        uh_df=pd.DataFrame()
        for i in range(self.__n_zones):

            shape = pars.shape[i]
            scale = self.__scale[i]
            area = pars.area[i]

            #volume calcuation zone_area_km2 to mi2 (0.386102) to ft2 (0.386102*5280)
            #multiplied by 1 inch (in ft) for a volume of ft3
            total_uh_vol = area*0.386102*5280**2*1/12

            #dimensionless uh
            uh_dl = nwsrfs_source.uh2p_call(shape,scale,tstep,int(1000))
            first0 = next(x for x, val in enumerate(uh_dl) if val == 0)
            uh_dl = uh_dl[:first0]
            
            #Distribute volume
            uh_vol = [ordinate * total_uh_vol for ordinate in uh_dl]
            #Divide by timestep in seconds
            uh = [ordinate / (tstep*60**2) for ordinate in uh_vol]
            #Assign to UH DataFrame
            uh_df = pd.concat([uh_df,pd.Series(uh,name=f'uh_{str(i)}')],axis=1)

        #modify and label index
        uh_df.index = uh_df.index.to_series().multiply(tstep)
        uh_df.index.rename('hours',inplace=True)
        
        return uh_df

    @property
    def uh(self) -> pd.DataFrame:
        '''
        Returns a unit hydrograph at as a DataFrame at a timestep specified by the ``dt_hours`` attribute.
        '''

        if self.__uh is None:
            #Create a copy of the parameters
            pars = copy.deepcopy(self.gamma_uh_pars)
            self.__uh = self.return_uh(tstep=pars.dt_hours)

        return self.__uh 

    def return_sf(self,
                tci:SACSnowTCI,
                return_inst:bool = True):

        '''
        Return a timeseries of streamflow for each zone.
        Args:
            tci (SACSnowTCI): Specific tci DataFrame output from sacsnow classes sacsnow_tci property (units:  mm).
            return_inst (bool): The specifies to return instaneous streamflow, rather than period average.  Default: True
        Returns:
            pd.DataFrame: A DataFrame containing streamflow with a column for each zone (units: cfs).
        '''

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gamma_uh_pars)

        #Format tci into a FORTRAN friendly datatype
        tci_array = utils._dtype_conversion(tci.to_numpy(),np.float64)
        tci_array = np.asfortranarray(tci_array)

        m_uh = int(1000)  # max UH length
        n_uh = len(tci_array) + m_uh

        #Calculate the streamflow for each zone
        sf = pd.DataFrame(index=tci.index)
        for i in range(self.__n_zones):

            shape=pars.shape[i]
            scale=self.__scale[i]
            area=pars.area[i]

            col_name = f'sf_{i}'

            flow_routed = nwsrfs_source.duamel(tci_array[:, i], shape, scale,
                pars.dt_hours/24, n_uh, m_uh, int(1), int(0))

            # flow_routed units:  mm, zone_area units:  km2,  1000 is a combined conversion of km2->m2 and mm->m
            # flow routed is depth of runoff over a basin for a time step. that with area converts it to a volume
            # and dt.second (timestep in sec) is used to complete conversion of runoff to flow
            # instantaneous routed flow weighted by zone area
            sim_flow_inst_cfs = flow_routed[0:len(tci_array)] * 1000 * 3.28084 ** 3 / \
                                (pars.dt_hours*60**2) * area

            #return instantaneous or period avg depending on chosen option
            if return_inst:
                sf[col_name] = sim_flow_inst_cfs
            else:
                sf[col_name] = self.inst_to_ave(sim_flow_inst_cfs)
        return sf

    @staticmethod
    def inst_to_ave(ts_inst:np.ndarray):
        '''
        Return a timeseries of period average data.
        Args:
            ts_inst (np.ndarray): A timeseries of instantaneous data.
        Returns:
            np.ndarray: A timeseries of period average data.
        '''

        #Using ffill() to fill in np.na values from shift(-1)
        ts_next = pd.DataFrame(ts_inst).shift(-1).ffill().to_numpy().flatten()

        return (ts_inst + ts_next)/2

