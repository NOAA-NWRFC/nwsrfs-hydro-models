import numbers, copy, warnings
from dataclasses import dataclass, fields
from typing import NewType
import pandas as pd
import numpy as np
from . import nwsrfs_src as nwsrfs_source
from .. import utils
#Debug: import pdb; pdb.set_trace() | breakpoint()

#create a new type for sacsnow_tci output
SACSnowTCI = NewType('SACSnowTCI',pd.DataFrame)
"""
Custom type alias for total channel inflow (tci) output.

This type represents a pandas DataFrame containing tci values with a column 
for each zone (units: mm). It is strictly used to ensure type safety 
when passing tci data between the :class:`SacSnow` and :class:`GammaUh` classes.
"""

@dataclass
class SacSnowPars:

    '''
    Container for all inputs required to run the NWSRFS SAC-SMA and SNOW-17 models via F2PY bindings. 

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
        * For SNOW-17: N_pars = 13

    **ADC Table Equation:**

    swe/ai = a * AESC**b+(1-a)*AESC**c

    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        alat (np.ndarray): array of each zone's centroid latitude (units: decimal degrees).
        elev (np.ndarray): array of each zone's centroid elevation (units: m).
        sac_pars (np.ndarray): SAC-SMA model parameters (ordered array).

            0. **uztwm**: Upper zone tension water capacity (units: mm).
            1. **uzfwm**: Upper zone free water capacity (units: mm).
            2. **lztwm**: Lower zone tension water capacity (units: mm).
            3. **lzfpm**: Lower zone primary free water capacity (units: mm).
            4. **lzfsm**: Lower zone supplemental free water capacity (units: mm).
            5. **adimp**: Fraction of additional impervious area (units: fraction 0-1).
            6. **uzk**: Upper zone free water storage depletion coefficient (units: NA).
            7. **lzpk**: Lower zone primary free water storage depletion coefficient (units: NA).
            8. **lzsk**: Lower zone supplemental free water storage depletion coefficient (units: NA).
            9. **zperc**: Maximum percolation rate multiplier (units: NA).
            10. **rexp**: Exponent for the percolation equation (units: NA).
            11. **pctim**: Minimum impervious area (units:  fraction 0-1).
            12. **pfree**: Fraction of percolated water which always goes directly to lower zone free water storages (units: fraction 0-1).
            13. **riva**: Fraction of riparian vegetation area (units:  fraction 0-1).
            14. **side**: Fraction of non-channel baseflow (deep groundwater recharge) to channel baseflow (units: fraction 0-1).
            15. **rserv**: Fraction of lower zone free water which cannot be transferred to lztw (units: fraction 0-1).
            16. **efc**:  fraction of effective forest cover (units: fraction 0-1).

        snow_pars (np.ndarray): SNOW-17 model parameters (ordered array).

            0. **scf**: Snowfall correction factor (units: NA).
            1. **mfmax**: Maximum non-rain melt factor per time step (units: mm/degc).
            2. **mfmin**: Minimum non-rain melt factor per time step (units: mm/degc).
            3. **uadj**: Average wind function per time step (units: mm/mb).
            4. **si**: SWE threshold above which there is always 100% snow cover (units: mm).
            5. **nmf**: Maximum negative melt factor per time step (units:  mm/degc).
            6. **tipm**: Antecedent snow temperature index parameter (units:  fraction 0-1).
            7. **mbase**: Base temperature for non-rain melt factor (units: degc).
            8. **plwhc**: Maximum amount of liquid water held against gravity drainage (units:  fraction 0-1).
            9. **daygm**: Daily melt at the snow-soil interface (units: mm).
            10. **adc_a**: Parameter used to calculate the areal depletion curve parameter (units: NA).
            11. **adc_b**: Parameter used to calculate the areal depletion curve parameter (units: NA).
            12. **adc_c**: Parameter used to calculate the areal depletion curve parameter (units: NA).

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
        
        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day, self.hour)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day", "hour"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        self.dt_seconds = np.int32(utils._define_timestep_sec(self.year, self.month, self.day, self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self, np.int32, time_list)

        #convert SAC-SMA and Snow17 pars to double dtype
        pars_list = ['alat', 'elev','sac_pars','snow_pars','init_swe','peadj','pxadj']
        utils._dtype_conversion_batch(self,  np.float64, pars_list)

        #convert forcings to double dtype
        forcing_list = ['forcings_map','forcings_mat','forcings_ptps','forcings_etd']
        utils._dtype_conversion_batch(self,  np.float64, forcing_list)

        #If sac_pars or snow_pars are 1D arrays, convert to 2D arrays
        utils._add_dimension_batch(self,['snow_pars','sac_pars'])

        #If forcings are 1D arrays, convert to 2D arrays
        utils._add_dimension_batch(self,forcing_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)  

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            self.dt_seconds <= 0
            or self.dt_seconds > 86400
            or 86400 % self.dt_seconds != 0
        ):
            raise ValueError(
                "Timestep must be positive, no longer than one day, and a factor of 24 hrs "
            )

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(self.dt_seconds,self.year, self.month, self.day, self.hour):
            raise Values("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #When using spin up a minmum one water year is required
        # min_required_steps = int(365 * (86400 / self.dt_seconds))
        # if len(self.year) < min_required_steps:
        #     raiseaise ValueError(f"SacSnow requires at least 1 full year of data ({min_required_steps} timesteps) for model spin-up, got {len(self.year)}.")

        #check that snow_pars and sac_pars are 2d arrays
        if not utils._validate_nd_array(self.sac_pars, self.snow_pars, ndim=2):
            raise ValueError("snow_pars and sac_pars must have a 2d shape")

        #check that the sac_pars and snow_pars are the correct length
        if self.sac_pars.shape[0] != 17:
            raise ValueError("sac_pars must have exactly 17 parameters")
        if self.snow_pars.shape[0] != 13:
            raise ValueError("snow_pars must have exactly 13 parameters")
       
        #check that sac-sma and snow17 parameters are 1d arrays.  
        #NOTE: "*" in front of self.sac_pars and self.snow_pars to unpack nested list
        if not utils._validate_nd_array(self.alat,self.elev,self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars):
            raise ValueError("SAC-SMA and Snow17 parameter arrays must have a 1d shape")

        #check that sac-sma and snow17 parameters have the same length
        if not utils._validate_array_length(self.alat,self.elev,self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars):
            raise ValueError("SAC-SMA and Snow17 parameter arrays must have the same length")

        #check that sac-sma and snow17 parameters are equal or greater than zero
        #if np.vstack([self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars]).min() < 0:
        if not utils._validate_positive_values(self.init_swe,self.peadj,self.pxadj,*self.sac_pars,*self.snow_pars):
            raise ValueError("SAC-SMA and Snow17 parameters (excluding elev and alat) must be equal or greater than zero")    

        #if elev and alat are below zero raise warning
        if not utils._validate_positive_values(self.elev,self.alat):
            warnings.warn("elev and/or alat values are below zero, verify this is correct", category=UserWarning, stacklevel=3)

        #check that forcing inputs are 2d arrays
        if not utils._validate_nd_array(self.forcings_map,self.forcings_mat,self.forcings_ptps,self.forcings_etd, ndim=2):
            raise ValueError("Forcings must have a 2d shape")

        #check that forcing inputs have same lengths as year, month, day, hour arrays
        if not utils._validate_array_length(self.year,self.forcings_map,self.forcings_mat,self.forcings_ptps,self.forcings_etd):
            raise ValueError("Forcings must have the same length as time arrays (year, month, day, hour)")

        #check that forcing inputs have the same number of zones as the parameters values
        #Note:  Checking first index only is adequate as nested numpy arrays cannont have a "ragged" shape
        if not utils._validate_array_length(self.alat, self.forcings_map[0],self.forcings_mat[0],self.forcings_ptps[0],self.forcings_etd[0]):
            raise ValueError("Each nested forcing list length must correspond to number of zones which is specified by the length of parameter arrays")

        #check that map forcing values are valid:
        if not utils._validate_positive_values(self.forcings_map):
            raise ValueError("MAP forcings values must be >= 0")

        #check that etd forcing values are valid:
        if not utils._validate_positive_values(self.forcings_etd):
            raise ValueError("ETD forcings values must be >= 0")

        #check that the ptps forcing values are valid:
        if (self.forcings_ptps.min() < 0) or (1 < self.forcings_ptps.max()):
            raise ValueError("PTPS forcings values must be between 0 and 1")

class SacSnow():

    '''
    Class to run the NWSRFS SAC-SMA and SNOW-17 models via F2PY bindings.  Multiple SAC-SMA and SNOW-17 parameter sets can be run simultaneously. 

    Args:
        pars_dataclass (SacSnowPars): Dataclass which contains all inputs to run SAC-SMA and SNOW-17.
        validate (bool): Validate :class:`SacSnowPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        sacsnow_pars (SacSnowPars): Dataclass which contains all inputs to run SAC-SMA and SNOW-17.
    '''

    def __init__(self,
        pars_dataclass: SacSnowPars,
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
        Runs sacsnow wrapper and returns tci
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
            np.int32(0))

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
            np.int32(1))

    @property
    def sacsnow_tci(self) -> 'SACSnowTCI':
        '''
        Generates total channel inflow (tci) as a DataFrame with a column for each zone (units - mm).
        '''

        #If sacsnow_states has already been ran, use it's output to get tci
        raw = self.__raw_output if self.__raw_output is not None else self.__raw_states_output
        
        if raw is None:
            self.__run_wrapper()
            raw = self.__raw_output

        tci = pd.DataFrame(raw[2],index=self.__datetime).add_prefix('tci_')

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
            * **aesc**: Areal extent of snow cover (units: fraction 0-1).
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
class LagkPars:

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

    **Lag/K Table Equation:**

    Lag/K_table=a*(Q-d)**2+b*Q+c

    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        tbl_lageq_a (np.ndarray): Parameter used to calculate the lag table (units: NA).
        tbl_lageq_b (np.ndarray): Parameter used to calculate the lag table (units: NA).
        tbl_lageq_c (np.ndarray): Parameter used to calculate the lag table (units: NA).
        tbl_lageq_d (np.ndarray): Parameter used to calculate the lag table (units: NA).
        tbl_keq_a (np.ndarray): Parameter used to calculate the k table (units: NA).
        tbl_keq_b (np.ndarray): Parameter used to calculate the k table (units: NA).
        tbl_keq_c (np.ndarray): Parameter used to calculate the k table (units: NA).
        tbl_keq_d (np.ndarray): Parameter used to calculate the k table (units: NA).
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
        
        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day, self.hour)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day", "hour"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        self.dt_hours = np.int32(utils._define_timestep_sec(self.year, self.month, self.day, self.hour)/3600)
        time_list = ['year', 'month', 'day', 'hour']
        utils._dtype_conversion_batch(self, np.int32, time_list)

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

        #If qin is a 1D array, convert to 2D array
        utils._add_dimension_batch(self,['qin'])

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            self.dt_hours <= 0
            or self.dt_hours > 24
            or 24 % self.dt_hours != 0
        ):
            raise ValueError(
                "Timestep must be positive, no longer than one day, and a factor of 24 hrs "
            )

        #check to make there are no gaps in the timeseries
        dt_seconds = self.dt_hours*3600
        if not utils._validate_timestep(dt_seconds,self.year, self.month, self.day, self.hour):
            raise Values("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #check that the table limits are 1d arrays
        if not utils._validate_nd_array(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must have a 1d shape")

        #check that table limits have the same length
        if not utils._validate_array_length(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must have the same length")

        #check that the table limits are positive
        if not utils._validate_positive_values(self.tbl_lagmax, self.tbl_lagmin, self.tbl_kmax, self.tbl_kmin, self.tbl_qmax, self.tbl_qmin):
            raise ValueError("Table min/max limits for lag, k, and q must be >= 0")

        #check that the table max is greater than min 
        if np.any(self.tbl_lagmax < self.tbl_lagmin):
            raise ValueError("tbl_lagmax must be >= tbl_lagmin.")
        if np.any(self.tbl_kmax < self.tbl_kmin):
            raise ValueError("tbl_kmax must be >= tbl_kmin.")
        if np.any(self.tbl_qmax <= self.tbl_qmin):
            raise ValueError("tbl_qmax must be strictly greater than tbl_qmin.")

        #check that the initial states are 1d arrays
        if not utils._validate_nd_array(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must have a 1d shape")

        #check that the initial states have the same length
        if not utils._validate_array_length(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must have the same length")

        #check that the initial states are positive
        if not utils._validate_positive_values(self.init_co, self.init_qin, self.init_qout, self.init_stor):
            raise ValueError("Lagk initial states must be >= 0")

        #check that the lag/k table equation parameter values are 1d arrays
        if not utils._validate_nd_array(self.tbl_lageq_a,self.tbl_lageq_b,self.tbl_lageq_c,self.tbl_lageq_d,self.tbl_keq_a ,self.tbl_keq_b,self.tbl_keq_c,self.tbl_keq_d):
            raise ValueError("Lag/k table equation parameter values must have a 1d shape")

        #check that the lag/k table equation parameter values are the same length
        if not utils._validate_array_length(self.tbl_lageq_a,self.tbl_lageq_b,self.tbl_lageq_c,self.tbl_lageq_d,self.tbl_keq_a ,self.tbl_keq_b,self.tbl_keq_c,self.tbl_keq_d):
            raise ValueError("Lag/k table equation parameter values must have the same length")

        #check that limit, initial states, and equation coefficients have the same length
        if not utils._validate_array_length(self.tbl_lageq_a, self.tbl_lagmax, self.init_co):
            raise ValueError("Table equation parameters, limits, and initial states must all have the same length (number of upstream reaches).")

        #check that qin is a 2d arrays
        if not utils._validate_nd_array(self.qin, ndim=2):
            raise ValueError("qin must have a 2d shape") 

        #check that upstream flow inputs have the same number of upstream reaches as the parameters values
        #Note:  Checking first index only is adequate as nested numpy arrays cannot have a "ragged" shape
        if not utils._validate_array_length(self.init_co, self.qin[0]):
            raise ValueError("Each nested upstream flow length must correspond to number of zones which is specified by the length of parameter arrays")

        #check that upstream flow inputs are valid:
        if self.qin.min() < 0:
            raise ValueError("Upstream flow values cannot be less than 0")

class Lagk():

    '''
    Class to run the NWSRFS Lag-K models via F2PY bindings.  Multiple upstream routes can be run simultaneously.

    Args:
        pars_dataclass (LagkPars): Dataclass which contains all inputs to run Lag-K.
        validate (bool): Validate :class:`LagkPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        lagk_pars (LagkPars): Dataclass which contains all inputs to run Lag-K.
    '''

    def __init__(self,
        pars_dataclass: LagkPars,
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
        #     self._itb = np.int32(output_timestep)
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
            np.int32(0))

    def __run_wrapper_states(self):
        '''
        Runs Lag-K and return states
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
            np.int32(1))

    @property
    def lagk_route(self) -> pd.DataFrame:
        '''
        Generates routed flows as a DataFrame with a column for each upstream reach (units - cfs).
        '''

        #If lagk_states has already been ran, use it's output to get routed flow
        raw = self.__raw_output if self.__raw_output is not None else self.__raw_states_output
        
        if raw is None:
            self.__run_wrapper()
            raw = self.__raw_output

        routed = pd.DataFrame(raw[0],index=self.__datetime).add_prefix('routed_')

        return routed

    @property
    def lagk_states(self) -> dict[str, pd.DataFrame]:
        '''
        Generates a dictionary of DataFrames containing all model states with a column for each upstream reach.

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
class ConsusePars:

    '''
    Container for all inputs required to run the NWSRFS CONS_USE model via F2PY bindings.  

    This class supports vectorized all timesteps simultaneously. Only a single CONS_USE zone can be ran at a time.

    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.

    **Array Shapes:**

    * **Time Arrays, Streamflow, and PET** (e.g., ``year``, ``qin``): Shape (T,)
    * **PE Adjust Table ** (``peadj_m``): Shape (12,)
    
    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        area (numbers.Number): Area of CONS_USE zone (units: km**2).
        irr_eff (numbers.Number): Irrigation efficiency (units:  fraction 0-1).
        min_flow (numbers.Number): Minimum flow (units: cfs).
        rf_accum_rate (numbers.Number): Return flow accumulation rate (units:  fraction 0-1).
        rf_decay_rate (numbers.Number): Fraction of return flow storage released to the channel each day (units: NA).
        pet_adj (numbers.Number):  Potential evaporation (PET) adjustment factor (units: NA).
        peadj_m (np.ndarray):  Monthly pe adjust table (units: NA).
        pet (np.ndarray):  Daily potential evapotranspiration depth for each timestep (units:  mm per day), before applying pet_adj and peadj_m.
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
    peadj_m :np.ndarray
    pet: np.ndarray
    qin: np.ndarray

    def __post_init__(self):

        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        #Convert dt_seconds, year, month, and day to integer dtype
        time_list = ['year','month','day'] 
        utils._dtype_conversion_batch(self,  np.int32, time_list)

        #Convert all CONS_USE parameters to double
        param_list = ['area','irr_eff','min_flow','rf_accum_rate',
                    'rf_decay_rate','pet_adj','peadj_m']
        utils._dtype_conversion_batch(self,  np.float64, param_list)

        #Convert all input timeseries to double
        ts_list = ['pet', 'qin']
        utils._dtype_conversion_batch(self,  np.float64, ts_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""
   
        dt_sec = utils._define_timestep_sec(self.year, self.month, self.day)
        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            dt_sec  <= 0
            or dt_sec != 86400
        ):
            raise ValueError(
                "Timestep must be daily"
            )

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(dt_sec,self.year, self.month, self.day):
            raise Values("Time arrays (year, month, day) cannot have gaps or duplicates")

        #check that peadj_m is a 1d array.  
        if not utils._validate_nd_array(self.peadj_m):
            raise ValueError("peadj_m array must have a 1d shape")

        #check that peadj_m has a lengh of 12.  
        if len(self.peadj_m) != 12:
            raise ValueError("peadj_m array must have a length of 12")

        #check that peadj_m has all positive values.
        if not utils._validate_positive_values(self.peadj_m):
            raise ValueError("peadj_m must contain value >=0")

        #check that CONS_USE  parameters are a single value
        if  0 != sum([x.ndim for x in [self.area,self.irr_eff,self.min_flow, self.rf_accum_rate, self.rf_decay_rate, self.pet_adj]]):
            raise ValueError("All CONS_USE  parameters (area,irr_eff,min_flow,rf_accum_rate,rf_decay_rate,pet_adj) must be a single value")

        #check that CONS_USE parameters are greater than zero
        if not utils._validate_positive_values(np.array((self.area,self.irr_eff, self.rf_accum_rate, self.rf_decay_rate, self.pet_adj)), allow_zero=False):
            raise ValueError("CONS_USE parameters (area,irr_eff,rf_accum_rate,rf_decay_rate,pet_adj) must be greater than zero")    

        #check that min are equal or greater than zero
        if not utils._validate_positive_values(np.array((self.min_flow))):
            raise ValueError("min_flow must be greater or equal to zero")
        
        #check that efficiency, accumulation, and decay parameters are less than or equal to one
        if not np.vstack((self.irr_eff, self.rf_accum_rate, self.rf_decay_rate,)).max() <= 1:
            raise ValueError("irr_eff, rf_accum_rate, and rf_decay_rate must be less than 1")

        #check that streamflow and PET inputs are 1d arrays
        if not utils._validate_nd_array(self.pet,self.qin):
            raise ValueError("Streamflow and PET array inputs must have a 1d shape")

        #check that streamflow and PET inputs have same lengths as year, month, day arrays
        if not utils._validate_array_length(self.year,self.pet,self.qin):
            raise ValueError("Streamflow and PET array inputs must have the same length as time arrays (year, month, day)")

        #check that streamflow and PET array input are valid:
        if not utils._validate_positive_values(self.pet) or not utils._validate_positive_values(self.qin):
            raise ValueError("QIN and PET array inputs values must be >= 0")

class Consuse():

        '''
        Runs the NWSRFS CONS_USE model via F2PY bindings.   Only a single CONS_USE zone can be ran at a time.  Timestep is daily average.

        Args:
            pars_dataclass (ConsusePars): Dataclass which contains all inputs to run CONS_USE.
            validate (bool): Validate :class:`ConsusePars` dataclass inputs are correct format/type. Default: ``True``.
        Attributes:
            consuse_pars (ConsusePars): Dataclass which contains all inputs to run CONS_USE.
            
        '''
        def __init__(self,
            pars_dataclass: ConsusePars,
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
                                 pars.peadj_m,pars.pet_adj,
                                 pars.pet,pars.qin)

        @property
        def consuse_qadj(self) -> pd.Series:
            '''
            Generates a Series of daily mean streamflow with consumptive use adjustments applied (units - cfsd).
            '''

            if self.__raw_output is None:
                self.__run_wrapper()

            qadj_daily = pd.Series(self.__raw_output[0], index=self.__datetime,name='qadj')

            return qadj_daily

        @property
        def consuse_states(self) -> pd.DataFrame:
            '''
            Generates a DataFrame with a column for each CONS_USE model states.

                The column states are:

                * **qadj**: Streamflow with all consumptive use adjustments applied (units: cfsd)
                * **qdiv**: Flow diverted for consumptive use (units: cfsd)
                * **qrf_in**:  Return flow from irrigation area used as input to return flow storage (units:cfsd)
                * **qrf_out**: Return flow to channel from return flow storage (units: cfsd)
                * **qol**:  Diverted flow other losses [eg transport, subsurface] (units:  cfsd)
                * **qcd**: Crop flow demand (units:  cfsd) 
                * **ce**: Crop evapotranspiration demand (units: mm)
                * **rfstor**: Return flow storage depth at the start of each daily timestep, before that day's return flow inflow and outflow (units: mm). 
            '''

            if self.__raw_output is None:
                self.__run_wrapper()


            state_param=['qadj','qdiv','qrf_in','qrf_out','qol','qcd','ce','rfstor']
            
            state_dic = {key: array for key, array in zip(state_param, self.__raw_output)}

            return pd.DataFrame(state_dic, index=self.__datetime)

@dataclass
class ChanlossPars:

    '''
    Container for all inputs required to run the NWSRFS CHANLOSS model via F2PY bindings.

    This class supports vectorized execution across multiple CHANLOSS periods and timesteps simultaneously. 
    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.
    * **C**: Number of CHANLOSS periods.

    **Array Shapes:**

    * **Time Arrays and Streamflow** (e.g., ``year``, ``qin``): Shape (T,)
    * **CHANLOSS Factors** (e.g., ``factors``): Shape (C,)
    * **CHANLOSS Periods** (e.g., ``periods``): Shape (C,2)

    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours for each timestep (units: time).
        factors (np.ndarray): Adjustment for each period. For cl_type=1, a dimensionless multiplier applied to qin (0.8 retains 80% of the flow). For cl_type=2, a flow amount subtracted from qin (units: cfs).
        periods (np.ndarray): Inclusive beginning and ending months for each factor, with integer values from 1 to 12. A beginning month greater than the ending month spans the year boundary (units: month).
        cl_type (numbers.Number):  Adjustment mode: 1 (varp) multiplies streamflow by the interpolated factor; 2 (varc) subtracts the interpolated factor.
        min_flow (numbers.Number):  Minimum required ``qin`` flow for CHANLOSS to be applied (units: cfs).
        qin (np.ndarray):  Lower bound on adjusted streamflow (units: cfs). Output is also bounded below by 0 cfs.
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

        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day, self.hour)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day", "hour"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        self.dt_seconds = np.int32(utils._define_timestep_sec(self.year, self.month, self.day, self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  np.int32, time_list)

        #Convert factors, min_flow, and qin to double dtypes
        dbl_list = ['factors','min_flow','qin']
        utils._dtype_conversion_batch(self,  np.float64, dbl_list)

        #Convert periods to integer
        int_list = ['periods','cl_type']
        utils._dtype_conversion_batch(self,  np.int32, int_list)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            self.dt_seconds <= 0
            or self.dt_seconds > 86400
            or 86400 % self.dt_seconds != 0
        ):
            raise ValueError(
                "Timestep must be positive, no longer than one day, and a factor of 24 hrs "
            )

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(self.dt_seconds,self.year, self.month, self.day, self.hour):
            raise Values("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #check that factors are 1d arrays
        if not utils._validate_nd_array(self.factors):
            raise ValueError("factors must have a 1d shape")

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(self.dt_seconds,self.year, self.month, self.day, self.hour):
            raise Values("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #check that periods is a 2d arrays
        if not utils._validate_nd_array(self.periods, ndim=2):
            raise ValueError("periods must have a 2d shape")

        #check that period values are valid months
        if not np.all((self.periods >= 1) & (self.periods <= 12)):
            raise ValueError("periods must contain integer months from 1 to 12")

        #check that periods shape contains the correct shape
        if len(self.periods[0])!=2:
            raise ValueError("periods parameter shape must be n x 2")

        #check that periods and factors have the same length
        if not utils._validate_array_length(self.periods, self.factors):
            raise ValueError("periods and factors must have the same length")

        #check that cl_type parameter is 1 or 2
        if not self.cl_type ==1 and not self.cl_type ==2:
            raise ValueError("cl_type must have a integer value of 1 or 2")

        #check that streamflow input and factor parameters are 1d arrays
        if not utils._validate_nd_array(self.qin,self.factors):
            raise ValueError("Streamflow and factors array inputs must have a 1d shape")

        #check that streamflow input has same lengths as year, month, day arrays
        if not utils._validate_array_length(self.year, self.qin):
            raise ValueError("Streamflow input must have the same length as time arrays (year, month, day)")

        #check that qin input are valid:
        if not utils._validate_positive_values(self.qin):
            raise ValueError("Streamflow inputs values must be >= 0")


class Chanloss:

    '''
     Class to run the NWSRFS CHANLOSS model via F2PY bindings.

    Args:
        pars_dataclass (ChanlossPars): Dataclass which contains all inputs to run CHANLOSS.
        validate (bool): Validate :class:`ChanlossPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        chanloss_pars (ChanlossPars): Dataclass which contains all inputs to run CHANLOSS.
    '''
    def __init__(self,
            pars_dataclass: ChanlossPars,
            validate:bool = True):

        #Assign parameters
        self.chanloss_pars = pars_dataclass

        #Validate chanloss_pars
        if validate:
            self.chanloss_pars.validate()

        self.__datetime = utils._datetime_conversion(self.chanloss_pars.year, self.chanloss_pars.month, self.chanloss_pars.day, self.chanloss_pars.hour).rename('datetime')

        #Set raw_output to None until run function is executed
        self.__raw_output = None

    def __run_wrapper(self):
        '''
        Runs CHANLOSS wrapper
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.chanloss_pars)

        self.__raw_output = nwsrfs_source.chanloss(pars.dt_seconds, pars.year, pars.month, pars.day,
                                    pars.factors,pars.periods,pars.cl_type, pars.min_flow,
                                    pars.qin)

    @property
    def chanloss_qadj(self) -> pd.Series:
        '''
        Generates a Series of streamflow with CHANLOSS adjustments applied (units - cfs).
        '''

        if self.__raw_output is None:
            self.__run_wrapper()

        qin_adj = pd.Series(self.__raw_output, index=self.__datetime, name='qin_adj')

        return qin_adj

@dataclass
class FAPars:

    '''
    Container for all inputs required to apply monthly climatological forcing adjustments (FA) to either MAP, MAT, PTPS, or PET.  FA parameters and limits are shared across zones.
    This dataclass is designed to be generic to all four forcing types. The units for ``limits``, ``forcings``, 
    and ``climo`` depend on the forcing type being processed:

    * **map** (Precipitation): mm
    * **mat** (Temperature): deg C
    * **ptps** (Precip Typing): fraction (0-1)
    * **pet** (Potential Evap): mm

    Climatologies and adjustment limits use monthly totals in mm for MAP and PET, monthly mean temperature in degrees Celsius for MAT, and monthly mean fractions for PTPS. 
    Climatology is area weighted across zones. All four climatologies must be supplied together or omitted together.


    pet is calculated from adjusted MAT using the Hargreaves–Samani method and zone latitude. Pass forcings=None for PET and supply peadj_m to convert adjusted PET to evaporation demand.

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
    * **Vector Parameters** (e.g., ``pars``): Shape (N_pars, ) - Axis 0 corresponds to the ordered parameter list.
    * **Climo Parameter** (e.g., ``climo``): Shape (12, ) - A value for each month (Jan-Dec)
    * **limits**: Shape (12, 2), with lower bounds in column 0 and upper bounds in column 1.
    * **peadj_m**: Shape (12, Z), with months January–December along axis 0.   

    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        pars (np.ndarray): Contains the forcing adjustment parameters (ordered array).


            0. **scale**:  Multiplication factor to apply directly to the forcing for map, pet, and ptps.  For mat this parameter is an additive shift (units: NA).
            1. **p_redist**: The percentage of the climatological forcing to redistribute (units: fraction 0-1).
            2. **std**:  Controls the weighting factor on how the p_redist is partitioned out to each climatological month based on ranking (units: NA).
            3. **shift**: Shift the climatological values by x numbers of days in the positive or negative (units: days)

        area (np.ndarray): Array of each zone's area (units: km**2).
        alat (np.ndarray): Array of each zone's centroid latitude (units: decimal degrees).
        limits (np.ndarray): Contains the forcing adjustment upper and lower limits for each of the 12 months. Units vary by forcing type (see above).
        forcings (np.ndarray | None):  Input forcings to adjust using pars and limits. ``None`` must be passed for pet.  Units vary by forcing type (see above).
        peadj_m (np.ndarray | None):  Input required for ``PET`` forcing adjustment.  Contains potential evaporation adjustment factors [pet -> etd] for each of the 12 months [Not required for other forcing types] (units: NA).
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

        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day, self.hour)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day", "hour"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        #Convert time steps to integers
        self.dt_seconds = np.int32(utils._define_timestep_sec(self.year, self.month, self.day,self.hour))
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  np.int32, time_list)

        #Convert pars,area, alat, limits, forcings to double
        fa_inputs = ['pars', 'area', 'alat', 'limits']
        utils._dtype_conversion_batch(self,  np.float64, fa_inputs)

        #if forcings exists, then convert to double and convert to a 2d array (if needed)
        if self.forcings is not None:
            self.forcings = utils._dtype_conversion(self.forcings, np.float64)
            utils._add_dimension_batch(self,['forcings'])

        #if climo exists, then convert to double
        if self.climo is not None:
            self.climo = utils._dtype_conversion(self.climo, np.float64)

        #if peadj_m exists, then convert to double
        if self.peadj_m is not None:
            self.peadj_m = utils._dtype_conversion(self.peadj_m, np.float64)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            self.dt_seconds <= 0
            or self.dt_seconds > 86400
            or 86400 % self.dt_seconds != 0
        ):
            raise ValueError(
                "FA timestep must be positive, no longer than one day, and a factor of 24 hrs "
            )

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(self.dt_seconds,self.year, self.month, self.day, self.hour):
            raise ValuesError("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #Check that the run is longer than one year when climo is none
        if (self.climo is None) and (len(np.unique(self.month)) < 12):
            raise ValuesError("When climo is not supplied, Time arrays (year, month, day, hour) must at least be one year long")

        #check that pars, area, alat are a 1d array
        if not utils._validate_nd_array(self.pars, self.area, self.alat):
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

        if np.any(self.limits[:, 0] > self.limits[:, 1]):
            raise ValueError("Each explicit lower limit must be < its upper limit")

        #validation check for forcings if exists
        if self.forcings is not None:
            #check that forcings are 2d arrays
            if not utils._validate_nd_array(self.forcings, ndim=2):
                raise ValueError("forcings must have a 2d shape")
            #check that forcings inputs have the same number of zones as the zone and alat arrays
            #Note:  Checking first index only is adequate as nested numpy arrays cannont have a "jangled" shape
            if not utils._validate_array_length(self.area, self.forcings[0]):
                raise ValueError("forcings input must have same number of zones as the area attribute (forcings[0]==area)")
            #check that forcings input has same lengths as year, month, day arrays
            if not utils._validate_array_length(self.year, self.forcings):
                raise ValueError("forcings input must have the same length as time arrays (year, month, day, hour)")

        #check par values
        scale, p_redist, std, shift = self.pars

        if scale <= 0:
            raise ValueError("FA scale must be > 0")
        if not 0 <= p_redist <= 1:
            raise ValueError("FA p_redist must be between 0 and 1")

        if std <= 0:
            raise ValueError("FA std must be greater than zero")

        if abs(shift) > 30:
            raise ValueError(
                "FA shift must be between -30 and 30 days "
            )

        #validation check for climo if exists
        if self.climo is not None:
            #check that climo is a 1d array
            if not utils._validate_nd_array(self.climo):
                raise ValueError("climo parameter must have a 1d shape")
            #check that climo has a length of 12
            if len(self.climo) != 12:
                raise ValueError("climo parameter must have a length of 12")

        #validation check for peadj_m if exists
        if self.peadj_m is not None:
            #check that peadj_m is a 2d arrays
            if not utils._validate_nd_array(self.peadj_m, ndim=2):
                raise ValueError("peadj_m must have a 2d shape")
            #check that peadj_m has the same number of zones as forcings
            if not utils._validate_array_length(self.area, self.peadj_m[0]):
                raise ValueError("peadj_m parameter must have same number of zones as area attribute (peadj_m[0]==area)")
            #check that peadj_m has a length of 12
            if len(self.peadj_m) != 12:
                raise ValueError("peadj_m parameter must have a length of 12")

class FA():
    '''
    Class to apply monthly climatological forcing adjustments (FA) to forcings via F2PY bindings.

    .. note::

        For subdaily records starting after midnight, initial PET and
        evaporation demand are backfilled from the first midnight's values
        for each zone. This approximates the initial partial day using the
        following day's values. If no midnight occurs in the record, the
        initial zero values are retained.

    Args:
        map_dataclass (FAPars): Dataclass containing inputs for precipitation adjustments.
        mat_dataclass (FAPars): Dataclass containing inputs for temperature adjustments.
        ptps_dataclass (FAPars): Dataclass containing inputs for precipitation typing adjustments.
        pet_dataclass (FAPars): Dataclass containing inputs for potential evaporation adjustments.
        validate (bool): Validate that map, mat, ptps, and pet :class:`FAPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        fa_pars (dict[str, FAPars]): Dictionary of dataclasses representing ``'map'``, ``'mat'``, ``'ptps'``, and ``'pet'`` parameters.
    '''

    def __init__(self,
        map_dataclass: FAPars,
        mat_dataclass: FAPars,
        ptps_dataclass: FAPars,
        pet_dataclass: FAPars,
        validate:bool = True):

        #Create a master fa_pars attibuter
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

        #verify that forcing data in populated
        for key in ("map", "mat", "ptps"):
                    if self.fa_pars[key].forcings is None:
                        raise ValueError(f"{key}_dataclass must supply forcings")

        #Run each fa dataclass through its validation function
        for key, fa_dc in self.fa_pars.items():
            try:
                fa_dc.validate()
            except ValueError as e:
                error_message = f"Validation error with {key}_dataclass: {e}"
                raise RuntimeError(error_message) from e

        #Verify that area and alat parameters are consistent accross all FAPars objects
        reference = self.fa_pars["map"]
        for key, fa_dc in self.fa_pars.items():
            for name in ("area", "alat"):
                if not np.array_equal(
                    getattr(reference, name),
                    getattr(fa_dc, name),
                ):
                    raise ValueError(
                        f"{key}_dataclass.{name} must match "
                        f"map_dataclass.{name} in values and zone order"
                    )

        #Check that ALL forcings dataclass did not pass a climo value or ALL did
        climo_types = {type(fa_dc.climo) for fa_dc in self.fa_pars.values()}
        if len(climo_types) > 1:
            raise ValueError("A mixed use of the climo attribue with the passed fa_pars is not allowed")

        #Check climo values are valid 
        for key in ("map", "pet", "ptps"):
            climo = self.fa_pars[key].climo
            if climo is not None:
                if np.any(climo < 0):
                    raise ValueError(f"{key} climatology must be nonnegative")

                if key == "ptps" and np.any(climo > 1):
                    raise ValueError("ptps climatology must be between 0 and 1")

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
        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
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
        Generates a dictionary of adjusted forcings as DataFrames. 

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
        Generates a DataFrame of monthly adjustment factors (index 1-12).
        
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
        fac_df=pd.DataFrame(columns=fac_parameters,index=pd.Index(range(1,13),name='month'))
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

        return pd.DataFrame(self.__climo_array,index=pd.Index(range(1, 13), name="month"),columns=['map_climo','mat_climo','pet_climo','ptps_climo'])


@dataclass
class GammaUhPars:

    '''
    Container for all inputs required to create a gamma UNIT-HG models via F2PY bindings. Multiple gamma unit hydrograph parameter sets can be ran simultaneously. 

    This class supports vectorized execution across multiple zones. 
    Input arrays should adhere to the following shape conventions:
    
    **Dimensions:**

    * **Z**: Number of zones.

    **Array Shapes:**

    * **Parameters** (e.g., ``shape``): Shape (Z,)

    Args:
        dt_hours (np.ndarray): Finite, positive scalar timestep in hours. (units: hours)
        area (np.ndarray): Array of each zone's area. (units:  km**2)
        shape (np.ndarray): Dimensionless gamma shape parameter for each zone. (units: NA)
        scale (np.ndarray | None): Gamma scale parameter for each zone.  If ``scale`` is None, ``toc`` parameter must be provided. (units: NA)
        toc  (np.ndarray | None): Target truncated hydrograph duration for each zone .  If ``toc`` is None, the ``scale`` parameter must be provided. (units: hours)
    '''
    
    dt_hours: numbers.Number
    area: np.ndarray
    shape: np.ndarray
    scale: np.ndarray | None = None
    toc:  np.ndarray | None = None

    def __post_init__(self):

        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Convert dt_hours to float dtype
        self.dt_hours = np.float64(self.dt_hours)

        #Convert area and shape to double dtype
        pars_list = ['area','shape']
        utils._dtype_conversion_batch(self,  np.float64, pars_list)

        #if scale exists, then convert to double
        if self.scale is not None:
            self.scale = utils._dtype_conversion(self.scale, np.float64)

        #if toc exists, then convert to double
        if self.toc is not None:
            self.toc = utils._dtype_conversion(self.toc, np.float64)

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        if np.ndim(self.dt_hours) != 0:
            raise ValueError("dt_hours must be a scalar")

        if self.dt_hours <= 0:
            raise ValueError("dt_hours must be greater than zero")

        #check if either the scale or toc parameter is provided
        if (self.scale is None) == (self.toc is None):
            raise ValueError("Provide exactly one of scale or toc")

        #validation check for scale or toc parameter
        self._validate_batch("scale" if self.scale is not None else "toc")

    def _validate_batch(self,opt_var: str):

        names = ("area", "shape", opt_var)
        arrays = tuple(getattr(self, name) for name in names)

        #check that all uh parameters are 1d array
        if not utils._validate_nd_array(*arrays):
            raise ValueError(f"area, shape, and {opt_var} must be 1D arrays")
        #check that all uh parameters has the same length
        if not utils._validate_array_length(*arrays):
            raise ValueError(f"area, shape, and {opt_var} must have equal lengths")

        if len(self.area) == 0:
            raise ValueError("At least one zone must be provided")

        #check that uh parameters are greater than zero
        for name, array in zip(names, arrays):
                    if not np.all(array > 0):
                        raise ValueError(
                            f"{name} must contain values greater than zero"
                        )

class GammaUh():
    '''
    Class to generate Gamma UNIT-HG and route flows via F2PY bindings.

    Args:
        pars_dataclass (GammaUhPars): Dataclass which contains all inputs needed for gamma UNIT-HG calculations. 
        validate (bool): Validate :class:`GammaUhPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        gammauh_pars (GammaUhPars): Dataclass which contains all inputs needed for gamma UNIT-HG calculations. 
    
    .. warning::

        Unit hydrographs are limited to 1,000 ordinates. Hydrographs that extend beyond this limit are truncated and normalized over the retained ordinates, which can alter their timing and shape.
    '''
    def __init__(self,  
                pars_dataclass: GammaUhPars, 
                validate:bool=True):

        #Assign parameters
        self.gammauh_pars = pars_dataclass

        #Validate gammauh_pars
        if validate:
            self.gammauh_pars.validate()

        #Get the number of zones
        self.__n_zones = len(self.gammauh_pars.shape)

        #If scale parameter is none, calcuate it
        if self.gammauh_pars.scale is None:
            self._get_shape_par()   
        else:
            self.__scale = self.gammauh_pars.scale.copy()

        #Create attribute to track it uh has been ran
        self.__uh = None

    #calculate the scale parameter for each zone
    def _get_shape_par(self):

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gammauh_pars)

        self.__scale = np.zeros(self.__n_zones)
        for i in range(self.__n_zones):
            shape = pars.shape[i]
            toc = pars.toc[i]
            self.__scale[i] = nwsrfs_source.uh2p_get_scale_root(shape,toc,np.float64(1))

    def return_uh(self,
                tstep):
        '''
        Return unit hydrograph ordinates for each zone (units: cfs/in).

        Args:
            tstep (int): Specifies timestep of unit hydrograph (units: hours). Fractional hours are not supported and will be rounded.
        Returns:
             pd.DataFrame: A DataFrame containing unit hydrograph (uh) with a column for each zone (units: cfs/in). Trailing zero ordinates are omitted; leading zeros are preserved. Shorter zone hydrographs are padded with NaN.
        '''

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gammauh_pars)

        #convert tstep to integer
        tstep = np.int32(tstep)
 
        #Start validation of tstep
        if np.ndim(tstep) != 0:
            raise ValueError("tstep must be a scalar")

        tstep = np.float64(tstep)
        if not np.isfinite(tstep) or tstep <= 0:
            raise ValueError("tstep must be finite and greater than zero")
        #End validation of tstep

        #Calculate the uh for each zone
        uh_dic = {}
        for i in range(self.__n_zones):

            shape = pars.shape[i]
            scale = self.__scale[i]
            area = pars.area[i]

            #dimensionless uh
            uh_dl = np.asarray(nwsrfs_source.uh2p_call(shape,scale,tstep,np.int32(1000)), dtype=np.float64)

            #Start validation of uh_dl
            if not np.all(np.isfinite(uh_dl)):
                raise ValueError(f"Zone {i} produced nonfinite UH ordinates")

            nonzero = np.flatnonzero(uh_dl != 0)
            if nonzero.size == 0:
                raise ValueError(
                    f"Zone {i} produced an all-zero UH; check shape, "
                    "scale, and timestep"
                )
            #End validation of uh_dl

            # Preserve leading zeros and remove trailing zero padding.
            uh_dl = uh_dl[:nonzero[-1] + 1]

            # Volume generated by one inch of runoff over the zone.
            #volume calcuation zone_area_km2 to mi2 (0.386102) to ft2 (0.386102*5280)
            #multiplied by 1 inch (in ft) for a volume of ft3
            total_uh_vol = pars.area[i] * 0.386102 * 5280**2 / 12
            #Distribute volume and divide by timestep in seconds
            uh = uh_dl * total_uh_vol / (tstep * 3600)

            #Assign to UH to dictionary
            uh_dic[f"uh_{i}"] = pd.Series(uh)

        #Create DataFrame and format index
        uh_df = pd.DataFrame(uh_dic)
        uh_df.index = pd.Index(
            uh_df.index.to_numpy(dtype=float) * tstep,
            name="hours",
        )
        
        return uh_df

    @property
    def uh(self) -> pd.DataFrame:
        '''
        Generates a unit hydrograph as a DataFrame at a timestep specified by the ``dt_hours`` attribute (units - cfs/in).
        '''

        if self.__uh is None:
            #Create a copy of the parameters
            pars = copy.deepcopy(self.gammauh_pars)
            self.__uh = self.return_uh(tstep=pars.dt_hours)

        return self.__uh.copy(deep=True)

    def return_sf(self,
                tci:SACSnowTCI,
                return_inst:bool = True):

        '''
        Return a timeseries of streamflow for each zone.

        Args:
            tci (SACSnowTCI):  Custom type alias for total channel inflow from :class:`SacSnow`  (units:  mm).
            return_inst (bool): Specifies to return instantaneous streamflow, rather than period mean flow.  Default: ``True``.
        Returns:
            pd.DataFrame: A DataFrame containing streamflow with a column for each zone (units: cfs).
        '''

        #Create a copy of the parameters
        pars = copy.deepcopy(self.gammauh_pars)

        #tci validation start
        if tci.shape[1] != self.__n_zones:
            raise ValueError(
                f"tci must have exactly {self.__n_zones} zone columns"
            )

        if len(tci) > 1:
            if not utils._validate_timestep(pars.dt_hours * 3600,tci.index.year,tci.index.month,tci.index.day,tci.index.hour):
                raise ValueError(
                    "tci timestep must match GammaUhPars.dt_hours"
                )
        #tci validation end

        #Format tci into a FORTRAN friendly datatype
        tci_array = utils._dtype_conversion(tci.to_numpy(),np.float64)
        tci_array = np.asfortranarray(tci_array)

        m_uh = np.int32(1000)  # max UH length
        n_uh = np.int32(len(tci_array) + m_uh)

        #Calculate the streamflow for each zone
        sf = pd.DataFrame(index=tci.index)
        for i in range(self.__n_zones):

            shape=pars.shape[i]
            scale=self.__scale[i]
            area=pars.area[i]

            col_name = f'sf_{i}'

            flow_routed = nwsrfs_source.duamel(tci_array[:, i], shape, scale,
                pars.dt_hours/24, n_uh, m_uh, np.int32(1), np.int32(0))

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
                sf[col_name] = self._inst_to_ave(sim_flow_inst_cfs)
        return sf

    @staticmethod
    def _inst_to_ave(ts_inst:np.ndarray):
        '''
        Return a timeseries of period average data.

        Args:
            ts_inst (np.ndarray): A timeseries of instantaneous data.
        Returns:
            np.ndarray: A timeseries of period average data.
        '''

        values = np.asarray(ts_inst, dtype=np.float64)

        if values.ndim != 1:
                    raise ValueError("ts_inst must be a 1D array")

        if values.size == 0:
            return values.copy()

        #The final sample is repeated as the next endpoint because no subsequent sample is supplied.
        following = np.concatenate((values[1:], values[-1:]))

        return (values + following)/2

@dataclass
class RSnwElevPars:

    '''
    Container for all inputs required to run the NWSRFS RSNWELEV model via F2PY bindings.

    This class supports vectorized execution across multiple zones and timesteps simultaneously. Both aetbl_index and aeble_value columns must be ascending order.
    Input arrays should adhere to the following shape conventions:

    **Dimensions:**

    * **T**: Number of timesteps.
    * **Z**: Number of zones.
    * **E**: Number of area elevation curve rows.

    **Array Shapes:**

    * **Time Arrays** (e.g., ``year``): Shape (T,)
    * **Temperature Forcings** (e.g., ``forcings_mat``): Shape (T, Z)
    * **Scalar Parameters** (e.g., ``pxtemp``): Shape (Z,)
    * **Area-Elevation Table Index** (e.g., ``aetbl_index``): Shape (E,)
    * **Area-Elevation Table Values** (e.g., ``aetbl_values``): Shape (E,Z)


    Args:
        year (np.ndarray): Array of years for each timestep (units: time).
        month (np.ndarray): Array of months corresponding to each timestep (units: time).
        day (np.ndarray): Array of days for each timestep (units: time).
        hour (np.ndarray): Array of hours corresponding to each timestep (units: time).
        taelev (np.ndarray): Elevation associated with the ``forcings_mat`` time series (units: m).
        talr (np.ndarray): Lapse rate during precipitation periods (units: degc/100m).
        pxtemp (np.ndarray): Rain/snow threshold temperature (units: degc).
        aetbl_index (np.ndarray): Area-elevation table index array (units: fraction 0-1).
        aetbl_values (np.ndarray): Area-elevation table values array(units: m).
        forcings_mat(np.ndarray): Air temperature array for each timestep (units: degc).
    '''
    year: np.ndarray
    month: np.ndarray
    day: np.ndarray
    hour: np.ndarray
    taelev: np.ndarray
    talr: np.ndarray
    pxtemp: np.ndarray
    aetbl_index: np.ndarray
    aetbl_values: np.ndarray
    forcings_mat: np.ndarray

    def __post_init__(self):

        #check that all inputs contain finite values
        utils._validate_finite_fields(self)

        #Start validate time arrays
        time_arrays = (self.year, self.month, self.day, self.hour)

        if not utils._validate_nd_array(*time_arrays):
            raise ValueError("Time arrays must be 1D numpy arrays")

        if not utils._validate_array_length(*time_arrays):
            raise ValueError("Time arrays must have equal lengths")

        if len(self.year) < 2:
            raise ValueError("FAPars requires at least two timestamps to infer the timestep")

        for name in ("year", "month", "day", "hour"):
            values = getattr(self, name)
            if not np.all(values == np.floor(values)):
                raise ValueError(f"{name} must contain integer values")
        #End validate time arrays 

        #Convert time inputs to integer dtypes
        time_list = ['year','month','day','hour'] 
        utils._dtype_conversion_batch(self,  np.int32, time_list)

        #Convert parameter and forcing inputs to double dtypes
        dbl_list = ['taelev','talr','pxtemp','aetbl_index','aetbl_values','forcings_mat']
        utils._dtype_conversion_batch(self,  np.float64, dbl_list)

        #If forcings_mat and aetble_values are 1D arrays, convert to 2D arrays
        utils._add_dimension_batch(self,['forcings_mat','aetbl_values'])

        #Convert all arrays to a fortran friendly format
        utils._arrayasfortran(self)

    def validate(self):
        """Checks that all inputs meet shape, type, and value constraints."""

        dt_sec = utils._define_timestep_sec(self.year, self.month, self.day, self.hour)
        #check to make sure that dates are not reversed, are positive, no longer than a day, and a factor of 24hrs
        if (
            dt_sec  <= 0
            or dt_sec  > 86400
            or 86400 % dt_sec  != 0
        ):
            raise ValueError(
                "Timestep must be positive, no longer than one day, and a factor of 24 hrs "
            )

        #check to make there are no gaps in the timeseries
        if not utils._validate_timestep(dt_sec,self.year, self.month, self.day, self.hour):
            raise Values("Time arrays (year, month, day, hour) cannot have gaps or duplicates")

        #check that taelev, talr, pxtemp, aetbl_index are 1d arrays
        if not utils._validate_nd_array(self.taelev, self.talr, self.pxtemp, self.aetbl_index):
            raise ValueError("taelev, talr, pxtemp, and aetbl_index must have a 1d shape")

        #check that taelev, talr, pxtemp have the same length
        if not utils._validate_array_length(self.taelev, self.talr, self.pxtemp):
            raise ValueError("taelev, talr, and pxtemp must have the same length")

        #check that aetble_values is a 2d array
        if not utils._validate_nd_array(self.aetbl_values, ndim=2):
            raise ValueError("aetble_values must have a 2d shape")

        #check that aetbl_index and aetble_values have the same length
        if not utils._validate_array_length(self.aetbl_index, self.aetbl_values):
            raise ValueError("aetbl_index and aetbl_values must have the same length")

        #check that aetbl_values has the correct number of columns
        if not utils._validate_array_length(self.aetbl_values[0],self.taelev):
            raise ValueError(f"the aetbl_values must have a column for each zone: E x {len(self.taelev)}")

        #check that forcings_mat is a 2d array
        if not utils._validate_nd_array(self.forcings_mat, ndim=2):
            raise ValueError("forcings_mat must have a 2d shape")

        #check that temperature input has same lengths as year, month, day arrays
        if not utils._validate_array_length(self.year, self.forcings_mat):
            raise ValueError("temperature input must have the same length as time arrays (year, month, day, hour)")

        #check that temperature forcing shape contains the correct shape
        if not utils._validate_array_length(self.forcings_mat[0],self.taelev):
            raise ValueError(f"the forcings_mat must have a column for each zone: T x {len(self.taelev)}")

        #check that aetbl_index is a fraction
        if (0 > self.aetbl_index.min()) or (self.aetbl_index.max() > 1):
            raise ValueError("aetbl_index must have fraction units (0-1)")
       
        if len(self.aetbl_index) < 2:
            raise ValueError("aetbl_index must contain at least 2 points")

        #check that aetbl_index is ascending
        if not utils._validate_positive_values(np.diff(self.aetbl_index), allow_zero=False):
            raise ValueError("aetbl_index values must be strictly ascending")

        #check that aetbl_values is ascending
        if not utils._validate_positive_values(np.diff(self.aetbl_values,axis=0), allow_zero=False):
            raise ValueError("aetbl_values values must be strictly ascending")

        #check that talr has only positive values
        if not utils._validate_positive_values(self.talr, allow_zero=False):
            raise ValueError("talr lapse rate must be positive (> 0)")

        #if taelev is below zero, raise warning
        if not utils._validate_positive_values(self.taelev):
            warnings.warn("taelev has below zero values, verify this is correct", category=UserWarning, stacklevel=3)

class RSnwElev:

    '''
     Class to run the NWSRFS RSNWELEV model via F2PY bindings.

    Args:
        pars_dataclass (RSnwElevPars): Dataclass which contains all inputs to run RSNWELEV.
        validate (bool): Validate :class:`RSnwElevPars` dataclass inputs are correct format/type. Default: ``True``.
    Attributes:
        rsnwelev_pars (RSnwElevPars): Dataclass which contains all inputs to run RSNWELEV.
    '''
    def __init__(self,
            pars_dataclass: RSnwElevPars,
            validate:bool = True):

        #Assign parameters
        self.rsnwelev_pars = pars_dataclass

        #Validate rsnwelev_pars
        if validate:
            self.rsnwelev_pars.validate()

        self.__datetime = utils._datetime_conversion(self.rsnwelev_pars.year, self.rsnwelev_pars.month, self.rsnwelev_pars.day, self.rsnwelev_pars.hour).rename('datetime')

        #Set raw_output to None until run function is executed
        self.__raw_output = None

    def __run_wrapper(self):
        '''
        Runs RSNWELEV wrapper
        '''

        #Create a copy to prevent any changes to the par dataclass when running the nwrfs soure code
        pars = copy.deepcopy(self.rsnwelev_pars)

        #Format the area elevation table for the fortran code
        aetabl = np.empty((pars.aetbl_values.shape[0]*2,pars.aetbl_values.shape[1]), dtype=pars.aetbl_values.dtype)
        aetabl[0::2,:] = pars.aetbl_index[:,None]
        aetabl[1::2,:] = pars.aetbl_values
        aetabl = np.asfortranarray(aetabl)

        self.__raw_output = nwsrfs_source.rsnwelev(pars.taelev, pars.talr, pars.pxtemp, aetabl,
                                    pars.forcings_mat)

    @property
    def forcing_ptps(self) -> pd.DataFrame:
        '''
        Generates ptps (precipitation typing) as a DataFrame with a column for each zone (units - fraction 0-1).
        '''

        if self.__raw_output is None:
            self.__run_wrapper()

        ptps= pd.DataFrame(self.__raw_output,index=self.__datetime).add_prefix('ptps_')

        return ptps

