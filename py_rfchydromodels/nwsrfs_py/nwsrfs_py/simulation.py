import os, sys, copy
from typing import NewType
import pandas as pd
import numpy as np
from . import nwsrfs
from . import utils
#import pdb; pdb.set_trace()

#create a new type for sacsnow_tci output
SACSnowTCI = NewType('SACSnowTCI',pd.DataFrame)
"""
Custom type alias for total channel inflow (tci) output.

This type represents a pandas DataFrame containing tci values with a column 
for each zone (units: mm). It is strictly used to ensure type safety 
when passing tci data between the :class:`~nwsrfs_py.nwsrfs.SacSnow` and :class:`~nwsrfs_py.nwsrfs.GammaUh` classes.
"""

class _NwrfcAcPrep:

    '''
    This function reads in forcing, streamflow, and optimzed parameters from the NWRFC autocalibration process.  

    Files and folders in ``autocalb_dir`` must must follow file conventions of the NOAA-NWRFC/nwsrfs-hydro-autocalibration repository 
    optimization tools. The forcing csv files must be in the directory ``'forcing_por_*'``, even if it is a routing only reach 
    (i.e. no local inflow modeled by SAC-SMA, Snow17, UH).

    If no ``run_dir`` is provided, the first ``'results_*'`` directory found within the ``autocalb_dir`` path will be used.

    Arguments:
        autocalb_dir (str): Path to a NWRFC autocalibration directory
        run_dir (str | None): Name of optimization run subdirectory within the ``autocal_dir``. If ``'None'`` is provided, defaults to using first ``'results_*'``' directory found within the ``autocalb_dir``.
    '''

    def __init__(self,
                autocalb_dir: str, 
                run_dir: str | None = None):

            #Check if directory exist and assign
            if run_dir is not None:
                check_path = os.path.join(autocalb_dir,run_dir)
            else:
                check_path = autocalb_dir
            self.autocalb_dir = autocalb_dir
            self._autocalb_dir_basename = os.path.basename(self.autocalb_dir)

            if not os.path.isdir(check_path):
                msg = f'{check_path} is not a directory.'
                raise ValueError(msg)

            #Search autocalb directory for csv files.
            self.autocalb_files = self._create_dir_df(self.autocalb_dir)

            #Assign run_dir if not specified
            if run_dir is not None:
                self.run_dir = run_dir
            else:
                results_dir_query = self.autocalb_files.loc[self.autocalb_files.folder.str.startswith('results'),'folder'].sort_values().unique()

                if len(results_dir_query) == 0:
                    raise ValueError(f"{autocalb_dir} contains no results_* folder.")

                self.run_dir = results_dir_query[0]
                msg = f'Defaulting to using the following results directory: {self.run_dir}'
                print(msg)

            #Filter out files that are not in either autocalb_dir or the run_dir
            self.autocalb_files = self.autocalb_files.loc[self.autocalb_files.folder.isin([self._autocalb_dir_basename,self.run_dir])]

            #Validate autocalb_dir and run_dir contents
            self._validate_contents()

            #Read in files
            self._load_files()

            #Catalog nwsrfs model being utilized
            self._interrogate_pars()

            #Extract time vectors
            self.dates = self.forcings_raw['map'].index.rename('datetime')
            self.year = self.forcings_raw['map'].index.year.to_numpy()
            self.month = self.forcings_raw['map'].index.month.to_numpy()
            self.day = self.forcings_raw ['map'].index.day.to_numpy()
            self.hour = self.forcings_raw['map'].index.hour.to_numpy()

            #Model timestep in hours
            self.dt_hours = int(utils._define_timestep_sec(self.year, self.month, self.day, self.hour)/3600)

            # If a routing only reach only set forcings to None, otherwise assign zones to each DataFrame's column names
            if not self.localflow_logic:
                self.forcings_raw = None
            else:
                self.forcings_raw = {key: df.set_axis(self.zone_names, axis=1) for key, df in self.forcings_raw.items()}

            #If a routing component, assign upflow names to columns
            if self.upflow_logic:
                self.upflow = self.upflow.set_axis(self.upflow_names, axis=1)


    def _validate_contents(self):

        '''
        Validates that necessary files and within ``autocalb_dir``, and checks which auxilary file are present
        '''

        #Check for optimial parameter file
        self.pars_path = os.path.join(self.autocalb_dir,self.run_dir,'pars_optimal.csv')
        if not os.path.isfile(self.pars_path):
            msg = f'{self.pars_path} is missing'
            raise ValueError(msg)

        #Check for flow_daily files
        self._daily_flow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('flow_daily')]
        if len(self._daily_flow_df) == 0:
            msg = 'Daily flow csv file is missing.  File must start with flow_daily_*.'
            raise ValueError(msg)
        elif len(self._daily_flow_df) > 1:
            msg = 'Daily flow csv file is ambiguous. Only one csv file must start with flow_daily_*.'
            raise ValueError(msg)
        self.daily_flow_path = os.path.join(self._daily_flow_df.path.squeeze(),self._daily_flow_df.file_name.squeeze())

        #Check for forcing files
        self.forcings_por_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('forcing_por')].copy()
        self.forcings_por_df.sort_values(by='file_name',inplace=True)
        if len(self.forcings_por_df) == 0:
            msg = 'POR forcing csv files are missing. Files must start with forcing_por_*.'
            raise ValueError(msg)

        #Check for instantaneous flow file 
        self._inst_flow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('flow_instantaneous')]
        if len(self._inst_flow_df) == 0:
            self.inst_flow_logic = False
            self.inst_flow_path = None
        if len(self._inst_flow_df) == 1:
            self.inst_flow_logic = True
            self.inst_flow_path = os.path.join(self._inst_flow_df.path.squeeze(),self._inst_flow_df.file_name.squeeze())
        else:
            msg = 'Instantaneous flow csv file is ambiguous. Only one csv file must start with flow_instantaneous_*.'
            raise ValueError(msg)

        #Check for upflow flow files
        self.upflow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('upflow')].copy()
        self.upflow_df.sort_values(by='file_name',inplace=True)
        if len(self.upflow_df) == 0:
            self.upflow_logic = False
        else:
            self.upflow_logic = True
     
    def _load_files(self):
       
        drop_cols = ['year','month','day','hour']

        #Read pars file
        self.pars=pd.read_csv(self.pars_path)
        self.pars = self.pars.sort_values(['name', 'zone'])

        #Make par file edits for CHANLOSS
        self.pars = self._cl_parfile_edits(self.pars)

        #Read daily flow file
        self.daily_flow = pd.read_csv(self.daily_flow_path)
        self.daily_flow.index = pd.to_datetime(self.daily_flow[['year', 'month', 'day']])
        #Added errors='ignore' to ignore missing hour column
        self.daily_flow.drop(drop_cols,axis=1,inplace=True,errors='ignore')
        self.daily_flow =self.daily_flow.resample('6h').ffill()

        #Read forcing file data as DataFrame and reconstruct as a dictionary with each forcing as a key
        forcings_import = self._read_tsfiles(self.forcings_por_df, drop_cols)
        self.forcings_raw = {'map': pd.DataFrame(index=forcings_import[0].index),
                             'mat': pd.DataFrame(index=forcings_import[0].index),
                             'ptps': pd.DataFrame(index=forcings_import[0].index)}
        
        #Create lists for each type of forcing
        map_list, mat_list, ptps_list = [], [], []
        for i in range(len(forcings_import)):
            map_list.append(forcings_import[i].loc[:,'map_mm'].rename(f'zone_{str(i)}'))
            mat_list.append(forcings_import[i].loc[:,'mat_degc'].rename(f'zone_{str(i)}'))
            ptps_list.append(forcings_import[i].loc[:,'ptps'].rename(f'zone_{str(i)}'))

        #Concatenate forcing list into a dictionary
        self.forcings_raw = {
            'map': pd.concat(map_list, axis=1),
            'mat': pd.concat(mat_list, axis=1),
            'ptps': pd.concat(ptps_list, axis=1)
            }

        #If exists, read in instantaneous flow file
        if self.inst_flow_logic:
            self.inst_flow = pd.read_csv(self.inst_flow_path)
            self.inst_flow.index = pd.to_datetime(self.inst_flow[['year', 'month', 'day', 'hour']])
            self.inst_flow.drop(drop_cols,axis=1,inplace=True)
        else:
            self.inst_flow = None

        #If exists, read in upflow flow files and reconstruct as a single DataFrame
        if self.upflow_logic:
            upflow_import = self._read_tsfiles(self.upflow_df, drop_cols)
            self.upflow = pd.DataFrame(index=upflow_import[0].index)
            for i in range(len(upflow_import)):
                self.upflow  = pd.concat([self.upflow,upflow_import[i].flow_cfs.rename(f'upflow_{i}')],axis=1)
        else:
            self.upflow = None

    def _interrogate_pars(self):

        '''
        Using the parfile, gathers number of zones, zone names, number of upstream points, upstream point names, if CONSUSE is used, and/or if CHANLOSS is used
        '''

        #Get zone names and number , if present
        self.zone_names = tuple(self.pars.loc[(self.pars.zone.str.contains('-'))].zone.sort_values().unique())
        self.n_zones = len(self.zone_names)
        if self.n_zones > 0:
            self.localflow_logic = True
        else:
            self.localflow_logic = False
        
        #Catalog routing, if present
        if self.upflow_logic:
           self.upflow_names = tuple(self.pars.loc[self.pars.type=='lagk'].zone.unique())
           self.n_upflow = len(self.upflow_names)
        else:
           self.upflow_names = None
           self.n_upflow = 0

        #Catalog CONSUSE, if present
        if self.pars.zone.str.contains('-CU').any():
            self.consuse_logic = True
            self.consuse_names = tuple(self.pars.loc[self.pars.type=='consuse'].zone.unique())
            self.n_consuse = len(self.consuse_names)
        else:
            self.consuse_logic = False
            self.consuse_names = None
            self.n_consuse=0

        #Catalog CHANLOSS info if present
        if self.pars.zone.str.contains('_CL').any():
            self.chanloss_logic = True
        else:
            self.chanloss_logic = False

    def _cl_parfile_edits(self, pars:pd.DataFrame):
        '''
        Makes changes to ``pars`` DataFrame in regards to CHANLOSS parameter row's 
        naming convention and parameters provided.
        
        Args:
            pars (pd.DataFrame): A DataFrame containing model parameters

        Returns:
            pars_edit (pd.Dataframe): A Dataframe with edits to naming convention of CHANLOSS parameters   

        '''
        pars_edits = pars.copy() 

        #Rename Zones for each CL Module.  But do not rename cl_type, n_clmods,min_q
        cl_exclude_logic=(pars_edits.type=='chanloss')&(~pars_edits.name.isin(['cl_type','n_clmods','cl_min_q']))
        pars_edits.loc[cl_exclude_logic,['zone']]=pars_edits.loc[cl_exclude_logic].zone + \
                                '_CL'+ pars_edits.loc[cl_exclude_logic].name.str.split('_').str[-1]   
        #n_clmods pars row now unnecessary
        pars_edits.drop(pars_edits.loc[pars_edits.name=='n_clmods'].index,inplace=True)
        #Remove the CL module name from the name columns, except for cl_min_q
        cl_remove_logic = (pars_edits.type=='chanloss')&(pars_edits.name!='cl_min_q')
        pars_edits.loc[cl_remove_logic ,['name']]= \
                                pars_edits.loc[cl_remove_logic].name.str.split('_').str[:-1].str.join('_')
        #Correct the cl_type name
        pars_edits.loc[pars_edits.p_name.str.contains('cl_type'),['name']]='cl_type'

        return pars_edits

    @staticmethod
    def _create_dir_df(path:str):
        '''
        Scans the directory structure to inventory CSV files.

        Args:
            path (str):  Path to directory with a basin nwrfc autocalibration run(s).
        '''
        autocalb_files=pd.DataFrame(columns=['path','file_name']) 
        n=1
        for root, dirs, files in os.walk(path):
            for file in files:
                if '.csv' in file:
                    autocalb_files=pd.concat([autocalb_files,
                                              pd.DataFrame({'path':root,
                                                           'file_name':file},index=[n])],axis=0)
                    n=n+1

        autocalb_files['folder']=autocalb_files.path.apply(os.path.basename)

        return autocalb_files

    @staticmethod
    def _read_tsfiles(path_ts:str, drop_cols: list[str]):
        '''
        Reads in csv timeseries files produced via the nwrfc autocalibration run(s) as a
        pandas DataFrame.

        Args:
            path_ts (str):  Path to csv timeseries file.  File is expected to have ['year', 'month', 'day', 'hour'] columns.
            drop_cols (list[str]):  List of string with columns to drop from dataframe
        '''
        list_df = []
        for index, row in path_ts.iterrows():
            ts_df = pd.read_csv(os.path.join(row.path,row.file_name))
            ts_df.index=pd.to_datetime(ts_df[['year', 'month', 'day', 'hour']])
            ts_df.drop(drop_cols,axis=1,inplace=True)
            list_df.append(ts_df)
        return list_df


class NwsrfsRun(_NwrfcAcPrep,
                nwsrfs.FA,
                nwsrfs.SacSnow,
                nwsrfs.GammaUh,
                nwsrfs.Lagk,
                nwsrfs.Chanloss):
    '''
    Orchestrates the execution of NWSRFS models (SAC-SMA, Snow17, lagk, etc.) using NWRFC autocalibration data.


    Files and folders in ``autocalb_dir`` must follow file conventions of the 
    `NOAA-NWRFC/nwsrfs-hydro-autocalibration repository <https://github.com/NOAA-NWRFC/nwsrfs-hydro-autocalibration>`_ optimization tools.

    If no ``run_dir`` is provided, the first ``'results_*'`` directory found within the ``autocalb_dir`` path will be used.

    Arguments:
        autocalb_dir (str): Path to a NWRFC autocalibration run directory
        run_dir (str | None): Name of optimization run subdirectory within the ``autocal_dir``. If ``'None'`` is provided, defaults to using first ``'results_*'`` directory found within the ``autocalb_dir``.
        forcing_adj (bool | list[str]):  If ``True`` monthly climatological forcing adjustments will be applied to all forcings.  Alternatively, a list with
            with specific forcing to apply climatological forcing adjustments can be supplied: ``'map'``, ``'mat'``, ``'ptps'``, ``'pet'``. Default: ``True``.
        return_inst (bool): Specifies to return instaneous streamflow, rather than period average.  Default: ``True``.
        shift_sf (bool):  Shifts :class:`~nwsrfs_py.nwsrfs.GammaUh` derived streamflow forward on one timestep.  Requirement for NWRFC calibrations. Default: ``True``.
    Attributes:
        pars (pd.DataFrame):  Contains all parameters used with NWSRFS models
        forcings_raw (dict[str, pd.DataFrame]):  A dictionary with forcings prior to applying :class:`~nwsrfs_py.nwsrfs.FA` adjustments
           
           The dictionary keys are:
           
            * **map**: Precipitation (units: mm) 
            * **mat**: Air temperature (units: degc)
            * **ptps**: Fraction of precipitation as snow (units: fraction 0-1)
        
        zone_names (tuple):  Names of zones modeled in basin.
        cu_names (tuple):  Names of consuse zones modeled in basin.
        daily_flow (pd.DataFrame):  Daily mean observed flow.
        inst_flow (pd.DataFrame|None):  Instantaneous observed flow.
        upflow (pd.DataFrame|None): Upstream flow derived from the :class:`adjustq.AdjustQ`.
        fa_pars (dict[str, nwsrfs.FAPars]|None):  If ``localflow_logic`` is ``True``, than dictionary of :class:`~nwsrfs_py.nwsrfs.FAPars` classes representing ``'map'``, ``'mat'``, ``'ptps'``, and ``'pet'``, otherwise ``'None'``.
        sacsnow_pars (nwsrfs.SacSnowPars|None): If ``localflow_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.SacSnowPars` class, otherwise ``'None'``.
        gammauh_pars (nwsrfs.GammmaUhPars|None): If ``localflow_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.GammmaUhPars` class, otherwise ``'None'``.
        lagk_pars (nwsrfs.LagkPars|None): If ``upflow_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.LagkPars` class, otherwise ``'None'``.
        chanloss_pars (nwsrfs.ChanlossPars|None): If ``chanloss_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.ChanlossPars` class, otherwise ``'None'``.
        consuse_pars (dict[str, nwsrfs.ConsusePars]|None): If ``consuse_logic`` is ``True``, than dictionary of :class:`~nwsrfs_py.nwsrfs.ConsusePars` classes representing each ``cu_names``, otherwise ``'None'``.
        localflow_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.FAPars`, :class:`~nwsrfs_py.nwsrfs.SacSnow`, and :class:`~nwsrfs_py.nwsrfs.GammaUh` models exist in configuration.
        upflow_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Lagk` model exist in configuration.
        chanloss_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Chanloss` model exist in configuration.
        consuse_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Consuse` model exist in configuration.
        n_zones (integer):  Number of zones in the configuration.

    '''

    def __init__(self,
                autocalb_dir: str, 
                run_dir: str | None = None,
                forcing_adj: bool | list[str] = True,
                return_inst: bool = True,
                shift_sf: bool = True):

        #Initiate nwsrfs_prep  
        _NwrfcAcPrep.__init__(self, autocalb_dir, run_dir)

        #Set return_inst attribute
        self.return_inst = return_inst
        #Set shift_sf attribute
        self.shift_sf = shift_sf

        #Validate forcing_adj argument
        self._interrogate_fa_arg(forcing_adj)

        #Create instances of all relevant NWSRFS models
        self.nwsrfs_run()

    def _interrogate_fa_arg(self,forcing_adj):
        '''
        Add approprate attributes regarding climatological forcing adjustments as specificed by the forcing_adj argument and if 
        SAC-SMA, Snow17, UH are being utilized.

        Args:
            forcing_adj (bool | list[str]):  If ``True`` monthly climatological forcing adjustments will be applied to all forcings.  Alternatively, a list with
                with specific forcing to apply climatological forcing adjustments can be supplied: 'map','mat', 'ptps','pet' 
        '''

        #Validate forcing_adj argument
        if not self.localflow_logic:
                self.forcing_adj_logic = None
                self.forcing_adj_types = ()
        elif isinstance(forcing_adj,bool):
            if forcing_adj:
                self.forcing_adj_logic = True
                self.forcing_adj_types = ('map','mat', 'ptps','pet')
            else:
                self.forcing_adj_logic = None
                self.forcing_adj_types = ()
        else:
            self.forcing_adj_logic = True
            #Just in case, set string entries to lower case and remove duplicates entries
            forcing_adj = [s.lower() for s in forcing_adj]
            forcing_adj = list(set(forcing_adj))

            #Check if forcing_adj string inputs match xpected forcing types
            forcing_types = {'map','mat','ptps','pet'}
            validate_types = set(forcing_adj).issubset(forcing_types)
            #validate_types is true than assign forcing_adj_types else return error
            if validate_types:
                self.forcing_adj_types = tuple(forcing_types.intersection(forcing_adj))
            else:
                msg = f'One or more string forcing_adj argument string not understood, expecting: {", ".join(forcing_types)}'
                raise ValueError(msg)   

    def nwsrfs_run(self):
        '''
        Prepares and creates instances of all nwsrfs models with parameters provided in the ``pars`` attribute.
        '''

        #Set all dataclass attributes to None
        self.fa_pars = self.sacsnow_pars = self.gammauh_pars = self.lagk_pars = self.chanloss_pars = self.consuse_pars = None

        #If there is a sac-snow model, perform forcing adjustments and activate SAC-SMA, SNOW17, UH Gamma models
        if not self.localflow_logic:
            self.fa_factors = self.forcings_climo = None            
        else:
            self._fa_run()
            self._sacsnow_uh_run()

        #If there is a lagk model, activate
        if self.upflow_logic:
            self._lagk_run()

        #If there is a chanloss model, activate
        if self.chanloss_logic:
            self._chanloss_run()

        #If there is a consuse model, activate
        if self.consuse_logic:
            self._consuse_run()

    def update_pars(self, new_pars: pd.DataFrame):
        '''
        Updates the ``pars`` file with new values.  

        **DataFrame Format**

        ``new_pars`` must have a ``'p_name'`` and ``'value'`` column. All values in the ``'p_name'`` columns must map to an row in the exiting ``pars`` DataFrame attribute.

        Args:
            new_pars (pd.DataFrame): dataframe with parameters to update nwsrfs models
        '''

        new_pars = new_pars.sort_values(['name', 'zone'])
        #Make par file edits for CHANLOSS
        new_pars = self._cl_parfile_edits(new_pars)

        #If the p_name column can't be mapped to p_name values in the existing ``pars`` attribute, then throw a error
        if not set(new_pars.p_name).issubset(self.pars.p_name):
            msg = 'All values in the p_value column of "new_pars" argument must exist in the ``pars`` attribute.'
            raise ValueError(msg)

        #Get old parameter dataframe and rename values column
        old_pars = self.pars.rename({'value':'old_value'},axis=1)

        #Merge old paramter file with new parameter file
        pars = old_pars.merge(new_pars.loc[:,['p_name','value']],on='p_name',how='left')

        #For row with which no new value was provided use the old value
        pars['value'] = pars.value.where(~pars.value.isna(),pars.old_value)
        #Reassign parameter attribute
        self.pars = pars.drop('old_value',axis=1)

        #Rerun all the NWRFS models with new pars file
        self.nwsrfs_run()

    def _fa_run(self,validate:bool = True):
        '''
        Apply monthly climatological forcing adjustments to forcing specified by the ``forcing_adj_types`` attribute
        
        Args:
            validate (bool): Validate that map, mat, ptps, and pet inputs are correct format/type. Default: True
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #If forcing_adj_types is None or then set all fa_pars values to scale=1, p_redist=0, std=10, shift=0
        nofa_pars = np.array([1,0,10,0])
        #fa adjustment parameters:  scale, p_redist, std, shift 
        df_2_dict = dict(zip(self.pars.loc[self.pars.type == 'fa'].name,self.pars.loc[self.pars.type == 'fa'].value))
        fa_pars = {}
        for f in ['map','mat','ptps','pet']:
            #Turn off fa adjustments for all forcings not within forcing_adj_types attribute 
            if f not in self.forcing_adj_types:
                fa_pars[f] = nofa_pars
            #Otherwise use forcing adjustment parameters provided in pars
            else:
                fa_pars[f] = np.array([df_2_dict[f'{f}_scale'], df_2_dict[f'{f}_p_redist'],
                                          df_2_dict[f'{f}_std'], df_2_dict[f'{f}_shift']])
            
        # monthly forcing adjustments
        fa_limits = {key: np.full([12, 2], np.nan)  for key in ['map','mat','ptps','pet']}
        peadj_m = np.full([12, self.n_zones], np.nan)
        peadj = {key:None for key in ['map','mat','ptps']}
        
        for i in range(12):
            m = i + 1
            for f in ['map','mat','ptps','pet']:
                fa_limits[f][i, 0] = self.pars[(self.pars.name == f'{f}_lower_{m:02}')&(self.pars.zone==self.zone_names[0])].value.item()
                fa_limits[f][i, 1] = self.pars[(self.pars.name == f'{f}_upper_{m:02}')&(self.pars.zone==self.zone_names[0])].value.item()
            for j, z in zip(range(self.n_zones), self.zone_names):
                #import pdb; pdb.set_trace()
                peadj_m[i, j] = self.pars.loc[(self.pars.name == f'peadj_{m:02}') & (self.pars.zone == z) &
                        (self.pars.type == 'sac')].value.item()
        peadj['pet'] = peadj_m

        #Make dummy climo input
        climo = None

        # Create forcing arrays
        forcings_raw = {}
        for f in ['map','mat','ptps']:
            forcings_raw[f] = self.forcings_raw[f].to_numpy()
        forcings_raw['pet'] = None

        #Get alat and area parameters
        alat=self.pars.loc[self.pars.name.str.contains('alat')].value.to_numpy()
        area=self.pars.loc[self.pars.name.str.contains('zone_area')].value.to_numpy()

        #Create a dictionary for each forcings dataclass
        fa_dc = {}
        for f in ['map','mat','ptps','pet']:
            fa_dc[f] = nwsrfs.FAPars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                            pars = fa_pars[f],
                            area = area,
                            alat = alat,
                            limits = fa_limits[f],
                            forcings = forcings_raw[f],
                            peadj_m = peadj[f],
                            climo = climo)

        #Initiate fa wrapper class
        nwsrfs.FA.__init__(self,
            map_dataclass = fa_dc['map'],
            mat_dataclass = fa_dc['mat'],
            ptps_dataclass = fa_dc['ptps'],
            pet_dataclass = fa_dc['pet'],
            validate = validate)

    @property
    def forcings(self) -> dict[str, pd.DataFrame]:
        '''
        Generates a dictionary of :class:`~nwsrfs_py.nwsrfs.FA` adjusted forcings as DataFrames. 

        The dictionary keys are:

        * **map**: Precipitation (units: mm) 
        * **mat**: Air temperature (units: degc)
        * **ptps**: Fraction of precipitation as snow (units: fraction 0-1)
        * **pet**: Potential evaporation (units: mm)
        * **etd**: Evaporation demand (units: mm)
        '''
        
        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #Get forcings from fa class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.FA.forcings.fget(self)

        #Remove "_fa" from dictionary keys and rename columns based on zone_names
        fixed_output = {key.split('_')[0]:value.set_axis(self.zone_names, axis=1) for key, value in raw_output.items()}
        
        return fixed_output


    def _sacsnow_uh_run(self, validate:bool=True):
        '''
        Run SAC-SMA, SNOW17, and gamma UH with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that SAC-SMA, Snow17, and gamma UH inputs are correct format/type. Default: True
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #Get a nested dictionary for SAC-SMA, Snow17, and gamma UH with parameter values
        pars_dict = {}
        for par_type in ['sac','snow','uh']:
            pars_dict[par_type]= {}
            for par in self.pars.loc[self.pars.type==par_type].name.unique():
                pars_dict[par_type][par] = self.pars.loc[(self.pars.type==par_type)&
                    (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()

        #Apply toc adjutment to toc parameter
        toc = pars_dict['uh']['unit_toc'] * pars_dict['uh']['unit_toc_adj'] 

        #Initate gammauh_pars data class
        gamma_dc = nwsrfs.GammaUhPars(dt_hours = self.dt_hours, area = pars_dict['uh']['zone_area'], 
                        shape = pars_dict['uh']['unit_shape'], toc = toc)

        #Initiate GammaUh wrapper class
        nwsrfs.GammaUh.__init__(self,pars_dataclass = gamma_dc,validate = validate)        

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

        #Initate SacSnowPars data class
        sacsnow_dc = nwsrfs.SacSnowPars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                                alat = pars_dict['snow']['alat'], elev = pars_dict['snow']['elev'], 
                                sac_pars =  sac_pars, snow_pars = snow_pars,
                                init_swe = pars_dict['snow']['init_swe'], 
                                pxadj =  pars_dict['sac']['pxadj'], peadj = pars_dict['sac']['peadj'],
                                forcings_map = self.forcings['map'].to_numpy(),
                                forcings_mat = self.forcings['mat'].to_numpy(),
                                forcings_ptps = self.forcings['ptps'].to_numpy(),
                                forcings_etd = self.forcings['etd'].to_numpy())

        #Initiate sacsnow wrapper class
        nwsrfs.SacSnow.__init__(self,
            pars_dataclass = sacsnow_dc,
            validate = validate)

    @property
    def sacsnow_tci(self) -> 'SACSnowTCI':
        '''
        Generates a custom type alias for total channel inflow (tci) using :class:`~nwsrfs_py.nwsrfs.SacSnow` with a column for each zone (units - mm).
        '''

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #Get tci from sacsnow class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.SacSnow.sacsnow_tci.fget(self) 

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)    

    @property
    def sacsnow_states(self) -> dict[str, pd.DataFrame]:
        '''
        Returns a dictionary of DataFrames containing all :class:`~nwsrfs_py.nwsrfs.SacSnow` model states with a column for each zone. 
            
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

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #Get states from sacsnow class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.SacSnow.sacsnow_states.fget(self)

        #For each dictionary value, rename column of Dataframe to correpond to zone names
        return {key: df.set_axis(self.zone_names, axis=1) for key, df in raw_output.items()}

    def return_uh(self,tstep):
        '''
        Generates a unit hydrograph as a DataFrame, using :class:`~nwsrfs_py.nwsrfs.GammaUh`, at a timestep specified by ``tstep``.

        Args:
            tstep (int): Specifies timestep of unit hydrograph (units: hours).
        Returns:
             pd.DataFrame: A DataFrame containing unit hydrograph (uh) with a column for each zone (units: cfs/in).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        raw_output = nwsrfs.GammaUh.return_uh(self,tstep)

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    @property
    def uh(self) -> pd.DataFrame:
        '''
        Generates a unit hydrograph as a DataFrame, using :class:`~nwsrfs_py.nwsrfs.GammaUh`, at a timestep specified by the ``dt_hours`` attribute (units - cfs/in).
        '''

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #Get uh from gamma_uh class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.GammaUh.uh.fget(self)

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    def return_sf(self,tci:SACSnowTCI):
        '''
        Generates a timeseries of streamflow as a Series using :class:`~nwsrfs_py.nwsrfs.GammaUh` and input ``tci``.

        Args:
            tci (SACSnowTCI): Custom type alias for total channel inflow from :class:`~nwsrfs_py.nwsrfs.SacSnow`  (units:  mm).
        Returns:
            pd.DataFrame: A DataFrame containing streamflow with a column for each zone (units: cfs).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        raw_output = nwsrfs.GammaUh.return_sf(self,tci,self.return_inst)
        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    @property
    def sacsnow_sf(self)-> pd.DataFrame:
        '''
        Calculates streamflow for each zone using :class:`~nwsrfs_py.nwsrfs.SacSnow` and :class:`~nwsrfs_py.nwsrfs.UhGamma` models (units - cfs). 
        '''

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #Get tci from sacsnow class
        tci_output = self.sacsnow_tci

        #Get streamflow from return_sf gamma_uh class function
        sf_output = self.return_sf(tci=tci_output)

        #For NWRFC Calibrations, UH output needs to be shifted forward one timestep because of how forcings are treated in AutoCalb
        #Repeat the first flow data point and append to the beginning of the ts, so there is no loss in a timestep
        if self.shift_sf:
            shift = sf_output.shift(1)
            sf_output  = shift.combine_first(sf_output)

        return sf_output

    def _lagk_run(self,validate:bool=True):
        '''
        Runs Lagk with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that LagK inputs are correct format/type. Default: True
        '''

        #If lagk parameters don't exist return none
        if not self.upflow_logic:
            return None

        #Get a dictionary of lagk parameter values
        pars_dict = {}
        for par in self.pars.loc[self.pars.type=='lagk'].name.unique():
            pars_dict[par] = self.pars.loc[(self.pars.type=='lagk')&
                (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()


        lagk_dc = nwsrfs.LagkPars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                    tbl_lageq_a = pars_dict['lagtbl_a'], tbl_lageq_b = pars_dict['lagtbl_b'],
                    tbl_lageq_c = pars_dict['lagtbl_c'], tbl_lageq_d = pars_dict['lagtbl_d'],
                    tbl_keq_a = pars_dict['ktbl_a'], tbl_keq_b = pars_dict['ktbl_b'],
                    tbl_keq_c = pars_dict['ktbl_c'], tbl_keq_d = pars_dict['ktbl_d'],
                    tbl_lagmax = pars_dict['lagk_lagmax'], tbl_lagmin = pars_dict['lagk_lagmin'],
                    tbl_kmax = pars_dict['lagk_kmax'], tbl_kmin = pars_dict['lagk_kmin'],
                    tbl_qmax = pars_dict['lagk_qmax'], tbl_qmin = pars_dict['lagk_qmin'],
                    init_co = pars_dict['init_co'], init_stor = pars_dict['init_stor'],
                    init_qin = pars_dict['init_if'], init_qout = pars_dict['init_of'],
                    qin = self.upflow.to_numpy())

        nwsrfs.Lagk.__init__(self,
            pars_dataclass = lagk_dc,
            validate = validate)

    @property
    def lagk_route(self) -> pd.DataFrame:
        '''
        Generates routed flows as a DataFrame using :class:`~nwsrfs_py.nwsrfs.Lagk` with a column for each upstream reach (units - cfs).
        '''

        #If lagk parameters don't exist return none
        if not self.upflow_logic:
            return None

        #Get routings from lagk class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.Lagk.lagk_route.fget(self) 

        #Rename column of Dataframe to correpond to upstream reach name
        return raw_output.set_axis(self.upflow_names, axis=1)    

    @property
    def lagk_states(self) -> dict[str, pd.DataFrame]:
        '''
        Generates a dictionary of DataFrames containing all :class:`~nwsrfs_py.nwsrfs.Lagk` model states with a column for each upstream reach.
            
            The dictionary keys are:

            * **routed**: Routed flow with lag and k applied (units: cfs).
            * **lag_time**: Lag applied to upstream flow (units: hours).
            * **k_inflow**: upstreamflow with only lag applied  (units: cfs).
            * **k_storage**: Attenuation storage (units: cfs).
        '''

        #If lagk parameter don't exist return none
        if not self.upflow_logic:
            return None

        #Get states from lagk class
        #Use .fget (Function Get) to grab the actual function inside because forcings is a property object
        raw_output = nwsrfs.Lagk.lagk_states.fget(self)

        #For each dictionary value, rename column of Dataframe to correpond to zone names
        return {key: df.set_axis(self.upflow_names, axis=1) for key, df in raw_output.items()}

    @property
    def _sacsnow_lagk_sim(self) -> pd.Series:
        '''
          Returns a Series of the sum of any sacsnow run off and upstream routed flow for each timestep (units: cfs)
        '''

        #Create blank simulation series
        qin=pd.Series(0,index=self.dates)

        #If there are sac/snow zone, calculate runoff
        if self.localflow_logic:
            qin += self.sacsnow_sf.sum(axis=1)

        #If there are upstream reaches to route, add them to the total flow
        if self.upflow_logic:
            qin += self.lagk_route.sum(axis=1)

        return qin.rename('sqin')

    def _chanloss_run(self, validate:bool=True):

        '''
        Runs chanloss with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that chanloss inputs are correct format/type. Default: True
        '''

        #If chanloss parameters don't exist return none
        if not self.chanloss_logic:
            return None

        #Get a dictionary of chanloss parameter values
        pars_dict = {}
        for par in self.pars.loc[self.pars.type=='chanloss'].name.unique():
            pars_dict[par] = self.pars.loc[(self.pars.type=='chanloss')&
                (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()

        periods =  np.column_stack((pars_dict['cl_period_start'],pars_dict['cl_period_end']))

        qin = self._sacsnow_lagk_sim

        chanloss_dc = nwsrfs.ChanlossPars(year = self.year,month =self.month,day = self.day,hour = self.hour,
                                periods = periods, factors = pars_dict['cl_factor'],
                                cl_type = pars_dict['cl_type'].item(), min_flow = pars_dict['cl_min_q'].item(),
                                qin = qin)

        nwsrfs.Chanloss.__init__(self,
            pars_dataclass = chanloss_dc,
            validate = validate)

    def chanloss_apply(self, qin:pd.Series, validate:bool=True):
        '''
        Runs :class:`~nwsrfs_py.nwsrfs.Chanloss` with ``qin`` supplied (units: cfs).

        Args:
            qin (pd.Series):  Input streamflow to adjust (units: cfs).
            validate (bool): Validate that chanloss inputs are correct format/type. Default: ``True``
        '''

        #If chanloss parameters don't exist return none
        if not self.chanloss_logic:
            return None

        #create a copy of the existing pars dataclass and replace qin with the supplied qin argument
        dc_pars = copy.deepcopy(self.chanloss_pars)
        dc_pars.qin = qin.to_numpy()

        #Initiate the chanloss class
        cl_class = nwsrfs.Chanloss(pars_dataclass = dc_pars,validate=validate)

        #Return adjustments
        return cl_class.chanloss_qadj

    @property
    def _sacsnow_lagk_chanloss_sim(self) -> pd.Series:
        '''
          Returns a Series of the sum of any sacsnow run off and upstream routed flow for each timestep with chanloss applied (units: cfs)
        '''

        if self.chanloss_logic:
            return self.chanloss_qadj.rename('sqin')
        else:
            return self._sacsnow_lagk_sim

    def _consuse_run(self, validate:bool=True):

        '''
        Runs CONSUSE with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that CONSUSE inputs are correct format/type. Default: True
        '''

        #If consuse parameters don't exist return none
        if not self.consuse_logic:
            return None

        #Create dictionary of ConsusePars dataclass
        self.consuse_pars = dict()

        #Get a dictionary of chanloss parameter values
        pars_dict = dict()
        for par in self.pars.loc[self.pars.type=='consuse'].name.unique():
            pars_dict[par] = self.pars.loc[(self.pars.type=='consuse')&
                (self.pars['name'] == par)].sort_values(by='zone')['value'].to_numpy()

        #format peadj for consuse calculation 
        peadj_cu = np.full([12, self.n_consuse], np.nan)
        for i in range(12):
            m = i + 1
            for j, z in enumerate(self.consuse_names):
                peadj_cu[i, j] = self.pars.loc[(self.pars.name == 'peadj_cu_' + f'{m:02}') & (self.pars.zone == z) &
                        (self.pars.type == 'consuse')].value.item()

        #Get natural flow
        qnat = self._sacsnow_lagk_chanloss_sim

        #Convert natual streamflow to daily average
        qnat_peravg = self._inst_to_ave(qnat.to_numpy())
        qnat_peravg = pd.Series(qnat_peravg,index=self.dates)
        qnat_daily = qnat_peravg.resample('1D').mean()

        #Get Daily PET
        #NOTE:  To match CHPS results have to be shifted back 1 hr so 00:00 timestep
        #       is included in previous day
        pet_shift = self.forcings['pet'].shift(periods=-1, freq='h')
        pet_shift.loc[self.dates[-1],:] = pet_shift.iloc[-1,:].values
        pet_daily = pet_shift.resample('1D').sum()


        #Create a blank state dataframe
        state_param = ['qadj','qdiv','qrf_in','qrf_out','qol','qcd','ce','rfstor']
        self._consuse_states = {key:pd.DataFrame() for key in state_param}

        #Run consuse for each zone individualys
        for n, cu_name in enumerate(self.consuse_names):

            #Get peadj associated with cu zones
            peadj = self.pars.loc[(self.pars.name=='peadj')&(self.pars.zone==cu_name),'value'].squeeze()   

            #Concat the two timeseries and remove any na values
            ts_df = pd.concat([pet_daily[cu_name],qnat_daily],axis=1)
            ts_df = ts_df[~ts_df.isna().any(axis=1)]

            #Get time arrays
            dates = ts_df.index
            year = dates.year.to_numpy()
            month = dates.month.to_numpy()
            day = dates.day.to_numpy()
            
            #Get timeseries arrays
            ts_array = ts_df.to_numpy()

            #Create parameter dataclass
            self.consuse_pars[cu_name] = nwsrfs.ConsusePars(year=year,month=month,day=day,
                area=pars_dict['area_km2'][n], irr_eff=pars_dict['irr_eff'][n], min_flow=pars_dict['min_flow'][n],
                rf_accum_rate=pars_dict['rf_accum_rate'][n],rf_decay_rate=pars_dict['rf_decay_rate'][n],
                pet_adj=peadj, peadj_m=peadj_cu[:,n],
                pet=ts_array[:,0],qin=ts_array[:,1])

            cu_run = nwsrfs.Consuse(self.consuse_pars[cu_name], validate=validate)

            #Concat state value for CU zone to dictionary. IF QADJ,replace value
            for count, param in  enumerate(state_param):
                if param=='qadj':
                    self._consuse_states[param]=cu_run.consuse_qadj
                else:
                    self._consuse_states[param]= pd.concat([self._consuse_states[param],
                                    cu_run.consuse_states[param].rename(cu_name)],axis=1)
                    
    @property
    def consuse_qadj(self) -> pd.Series:
        '''
        Generates a Series of flow adjusted by :class:`~nwsrfs_py.nwsrfs.Consuse` (units - cfsd).
        '''

        #If consuse parameters don't exist return none
        if not self.consuse_logic:
            return None

        return self._consuse_states['qadj']
 
    @property
    def consuse_states(self) -> dict[str, pd.DataFrame]:
        '''
        Generates a dictionary of DataFrames containing all :class:`~nwsrfs_py.nwsrfs.Consuse` states with a column for each zone.
            
            The dictionary keys are:

            * **qadj**: Streamflow with all consumptive use adjustments applied (units: cfsd)
            * **qdiv**: Flow diverted for consumptive use (units: cfsd)
            * **qrf_in**:  Return flow from irrigation area used as input to return flow storage (units:cfsd)
            * **qrf_out**: Return flow to channel from return flow storage (units: cfsd)
            * **qol**:  Diverted flow other loses [eg transport, subsurface] (units:  cfsd)
            * **qcd**: Crop flow demand (units:  cfsd) 
            * **ce**: Crop evapotranspiration demand (units: mm)
            * **rfstor**: Return flow storage contents (units: mm)
        '''

        #If consuse parameters don't exist return none
        if not self.consuse_logic:
            return None

        return self._consuse_states       

    @property
    def sim(self) -> pd.Series:
        '''
        Generates a Series of the sum of any local flow runoff (:class:`~nwsrfs_py.nwsrfs.SacSnow` + :class:`~nwsrfs_py.nwsrfs.GammaUh`)  and upstream routed (:class:`~nwsrfs_py.nwsrfs.Lagk`) flow for each timestep
        with chanloss (:class:`~nwsrfs_py.nwsrfs.Chanloss`) and consuse (:class:`~nwsrfs_py.nwsrfs.Consuse`) applied (units - cfs).
        '''

        #Get flow prior to potential CONSUSE adjustment
        qnat = self._sacsnow_lagk_chanloss_sim

        #If there area CONSUSE areas, adjust the flow
        if self.consuse_logic:

            qnat_cu_adj = self.consuse_states['qdiv'].sum(axis=1)-self.consuse_states['qrf_out'].sum(axis=1)
            #Shift forward a day so that the correct adjustement is applied  to the correct day
            qnat_cu_adj.index = qnat_cu_adj.index+pd.Timedelta(1, unit='D')
            #Backfill to fill all values after 00:00 and fill in nan values at the end of time sereies with last daily 
            #qnat_cu_adj value cut off from reindex
            qnat_cu_adj = qnat_cu_adj.reindex(qnat.index).bfill().replace({np.nan:qnat_cu_adj.values[-1]})
            q_adj = qnat - qnat_cu_adj
            #Replace any negative values with zero
            q_adj.loc[q_adj < 0] = 0
            return q_adj.rename('sqin')
        else:
            return qnat.rename('sqin')

    @classmethod
    def load_example(cls, lid:str='NRKW1', **kwargs):
        '''
        Generates a :class:`NwsrfsRun` for the package provided example data.

        Example data are NWRFC auto calibration results for two locations:

        * **NRKW1**: Nooksack River at North Cedarville, WA (USGS-12210700).  The following models are being utilized: :class:`~nwsrfs_py.nwsrfs.SacSnow`, :class:`~nwsrfs_py.nwsrfs.GammaUh`, :class:`~nwsrfs_py.nwsrfs.Lagk`.
        * **SFLN2**: Salmon Falls Creek NR San Jacinto NV (USGS 13105000).  The following models are being utilized: :class:`~nwsrfs_py.nwsrfs.SacSnow`, :class:`~nwsrfs_py.nwsrfs.GammaUh`, :class:`~nwsrfs_py.nwsrfs.Chanloss`, :class:`~nwsrfs_py.nwsrfs.Consuse`.

        Any optional arguments associated with :class:`NwsrfsRun`, can be passed.

        Args:
            lid (str):  Five letter identifier of example data.  Input can either be ``'NRKW1'`` or ``'SFLN2'``.  Default: ``'NRKW1'``.
            **kwargs: Optional keyword arguments passed to :class:`NwsrfsRun`. 

                Common options include:

                * **forcing_adj** (bool | list[str]): Apply monthly climatological adjustments. Default: ``True``. 
                * **return_inst** (bool): Return instantaneous vs. period-average flow. Default: ``True``. 
                * **shift_sf** (bool): Shift streamflow forward one timestep. Default: ``True``.
        '''

        lid = lid.upper()
        # Define the mapping within the method to keep it clean
        config = {
            'NRKW1': 'results_por_02',
            'SFLN2': 'results_por_01'
        }

        #Throw an error if the lid argument passed is not recognized.
        if lid not in config:
            msg = f"Station ID '{lid}' not recognized. Available: {list(config.keys())}"
            raise ValueError(msg)

        #Return :class:`nwsrfs_py.simulation.Nwsrfs` for the lid specified.
        with utils._get_example_dir(lid) as example_dir:
            # Note: Ensure __init__ loads all data BEFORE the context manager closes
            return cls(example_dir, config[lid], **kwargs)


