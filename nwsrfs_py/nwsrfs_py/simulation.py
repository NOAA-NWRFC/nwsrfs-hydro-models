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
    This function reads in forcing, streamflow, and optimized parameters from the NWRFC autocalibration process.  

    Files and folders in ``autocalb_dir`` must follow file conventions of the NOAA-NWRFC/nwsrfs-hydro-autocalibration repository 
    optimization tools. The forcing csv files must be in the directory ``'forcing_por_*'``, even if it is a routing only reach 
    (i.e. no local inflow modeled by SAC-SMA, Snow17, UH).

    If no ``run_dir`` is provided, the first ``'results_*'`` directory found within the ``autocalb_dir`` path will be used.

    .. rubric:: PTPS input

    The ``ptps`` column supplies the fraction of precipitation falling as snow
    and must be present in every ``forcing_por*`` CSV file to use supplied
    precipitation typing (``ptps_logic = True``). If any forcing file lacks
    ``ptps``, provide a single CSV file whose name starts with ``area_elev``
    in ``autocalb_dir``. The area-elevation CSV must contain one column per
    zone, labeled with the zone name and containing elevations in meters.
    The first CSV column, regardless of its header, is loaded as the index
    and named ``quantile``; zone columns are sorted alphabetically. The index
    represents the nonexceedance fraction: the fraction of the zone's area
    at or below the corresponding elevation. For example,
    a fraction of ``1.0`` corresponds to the maximum elevation in each zone.
    To calculate precipitation typing with ``RSnwElev``, also provide the
    parameters ``pxtemp`` and ``talr`` with ``type == 'rsnwelev'`` in
    ``autocalb_dir/run_dir/pars_optimal.csv`` for each zone. The ``taelev``
    input uses that zone's ``elev`` parameter with ``type == 'snow'``;
    a separate ``taelev`` parameter is not required.

    Multiple ``area_elev*`` CSV files are ambiguous and raise :exc:`ValueError`.
    A :exc:`ValueError` is also raised if ``ptps`` is missing from any forcing
    file and no area-elevation CSV is available.

    .. rubric:: Unit hydrograph selection

    The unit hydrograph method is determined from the ``name`` column of
    ``pars_optimal.csv``, considering only rows where ``type == 'uh'``:

    * ``unit_shape`` identifies a gamma unit hydrograph
      (``uh_type = 'gamma'``).
    * Numbered ordinate parameters named ``uh_0``, ``uh_1``, and so on
      identify an ordinate unit hydrograph (``uh_type = 'ordinate'``).
      Numbering must be consecutive from ``uh_0`` within each zone.
      Values are in cfs/in at the model timestep; shorter zone hydrographs
      are padded with trailing NaN values.

    Providing both ``unit_shape`` and any numbered ``uh_<number>`` entry
    raises :exc:`ValueError` because the method is ambiguous. If neither
    ``unit_shape`` nor ``uh_0`` is present, :exc:`ValueError` is raised because
    the method cannot be determined.

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
        Validates that necessary files and within ``autocalb_dir``, and checks which auxiliary file are present
        '''

        #Check for optimal parameter file
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

        #Check for an optional area-elevation file used for precipitation typing
        area_elev_df = self.autocalb_files.loc[
            self.autocalb_files.file_name.str.startswith('area_elev')]
        if len(area_elev_df) > 1:
            raise ValueError(
                'Area-elevation csv file is ambiguous. Only one csv file must start with area_elev*.')
        self.area_elev_logic = len(area_elev_df) == 1
        self.area_elev_path = (
            os.path.join(area_elev_df.path.squeeze(), area_elev_df.file_name.squeeze())
            if self.area_elev_logic else None)

        #Check for instantaneous flow file 
        self._inst_flow_df = self.autocalb_files.loc[self.autocalb_files.file_name.str.startswith('flow_instantaneous')]
        if len(self._inst_flow_df) == 0:
            self.inst_flow_logic = False
            self.inst_flow_path = None
        elif len(self._inst_flow_df) == 1:
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
        #PTPS is usable only when it is supplied for every forcing file.
        self.ptps_logic = all('ptps' in forcing.columns for forcing in forcings_import)
        self.area_elev = None
        if not self.ptps_logic:
            if not self.area_elev_logic:
                raise ValueError(
                    'Precipitation typing requires a ptps column in every forcing file '
                    'or a single area_elev* csv file.')
            self.area_elev = pd.read_csv(self.area_elev_path, index_col=0)
            self.area_elev.index.name = 'quantile'
            self.area_elev = self.area_elev.sort_index(axis=1)

        #Create lists for each type of forcing
        map_list, mat_list, ptps_list = [], [], []
        for i, forcing in enumerate(forcings_import):
            map_list.append(forcing.loc[:, 'map_mm'].rename(f'zone_{i}'))
            mat_list.append(forcing.loc[:, 'mat_degc'].rename(f'zone_{i}'))
            if self.ptps_logic:
                ptps_list.append(forcing.loc[:, 'ptps'].rename(f'zone_{i}'))

        #PTPS will be calculated separately when area-elevation data is needed.
        self.forcings_raw = {
            'map': pd.concat(map_list, axis=1),
            'mat': pd.concat(mat_list, axis=1)
        }
        if self.ptps_logic:
            self.forcings_raw['ptps'] = pd.concat(ptps_list, axis=1)

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
        
        #Identify the unit hydrograph from parameter names in the UH rows.
        uh_names = self.pars.loc[self.pars.type == 'uh', 'name']
        has_shape = uh_names.eq('unit_shape').any()
        has_ordinates = uh_names.str.fullmatch(r'uh_[0-9]+', na=False).any()
        if has_shape and has_ordinates:
            raise ValueError(
                "UH parameters are ambiguous: unit_shape and uh_<number> entries "
                "cannot both be provided in rows where type == 'uh'.")
        if has_shape:
            self.uh_type = 'gamma'
        elif uh_names.eq('uh_0').any():
            self.uh_type = 'ordinate'
        else:
            raise ValueError(
                "UH parameters (type == 'uh') must include unit_shape for a gamma "
                "unit hydrograph or uh_<number> entries starting with uh_0 "
                "for an ordinate unit hydrograph.")

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
                nwsrfs.RSnwElev,
                nwsrfs.FA,
                nwsrfs.SacSnow,
                nwsrfs.Lagk,
                nwsrfs.Chanloss):
    '''
    Orchestrates the execution of NWSRFS models (SAC-SMA, Snow17, lagk, etc.) using NWRFC autocalibration data.

    Files and folders in ``autocalb_dir`` must follow file conventions of the 
    `NOAA-NWRFC/nwsrfs-hydro-autocalibration repository <https://github.com/NOAA-NWRFC/nwsrfs-hydro-autocalibration>`_ optimization tools.
    Sample calibration data is available on `Zenodo <https://doi.org/10.5281/zenodo.18829935>`_.

    If no ``run_dir`` is provided, the first ``'results_*'`` directory found within the ``autocalb_dir`` path will be used.

    .. rubric:: PTPS input

    The ``ptps`` column supplies the fraction of precipitation falling as snow
    and must be present in every ``forcing_por*`` CSV file to use supplied
    precipitation typing (``ptps_logic = True``). If any forcing file lacks
    ``ptps``, provide a single CSV file whose name starts with ``area_elev``
    in ``autocalb_dir``. The area-elevation CSV must contain one column per
    zone, labeled with the zone name and containing elevations in meters.
    The first CSV column, regardless of its header, is loaded as the index
    and named ``quantile``; zone columns are sorted alphabetically. The index
    represents the nonexceedance fraction: the fraction of the zone's area
    at or below the corresponding elevation. For example,
    a fraction of ``1.0`` corresponds to the maximum elevation in each zone.
    To calculate precipitation typing with ``RSnwElev``, also provide the
    parameters ``pxtemp`` and ``talr`` with ``type == 'rsnwelev'`` in
    ``autocalb_dir/run_dir/pars_optimal.csv`` for each zone. The ``taelev``
    input uses that zone's ``elev`` parameter with ``type == 'snow'``;
    a separate ``taelev`` parameter is not required.

    Multiple ``area_elev*`` CSV files are ambiguous and raise :exc:`ValueError`.
    A :exc:`ValueError` is also raised if ``ptps`` is missing from any forcing
    file and no area-elevation CSV is available.

    .. rubric:: Unit hydrograph selection

    The unit hydrograph method is determined from the ``name`` column of
    ``pars_optimal.csv``, considering only rows where ``type == 'uh'``:

    * ``unit_shape`` identifies a gamma unit hydrograph
      (``uh_type = 'gamma'``).
    * Numbered ordinate parameters named ``uh_0``, ``uh_1``, and so on
      identify an ordinate unit hydrograph (``uh_type = 'ordinate'``).
      Numbering must be consecutive from ``uh_0`` within each zone.
      Values are in cfs/in at the model timestep; shorter zone hydrographs
      are padded with trailing NaN values.

    Providing both ``unit_shape`` and any numbered ``uh_<number>`` entry
    raises :exc:`ValueError` because the method is ambiguous. If neither
    ``unit_shape`` nor ``uh_0`` is present, :exc:`ValueError` is raised because
    the method cannot be determined.

    Arguments:
        autocalb_dir (str): Path to a NWRFC autocalibration run directory
        run_dir (str | None): Name of optimization run subdirectory within the ``autocal_dir``. If ``'None'`` is provided, defaults to using first ``'results_*'`` directory found within the ``autocalb_dir``.
        forcing_adj (bool | list[str]):  If ``True`` monthly climatological forcing adjustments will be applied to all forcings.  Alternatively, a list with
            with specific forcing to apply climatological forcing adjustments can be supplied: ``'map'``, ``'mat'``, ``'ptps'``, ``'pet'``. Default: ``True``.
        return_inst (bool): Specifies to return instantaneous streamflow, rather than period average.  Default: ``True``.
        shift_sf (bool):  Shifts streamflow from the selected UH model forward by one timestep.  Requirement for NWRFC calibrations. Default: ``True``.
    Attributes:
        pars (pd.DataFrame):  Contains all parameters used with NWSRFS models
        forcings_raw (dict[str, pd.DataFrame]):  A dictionary with forcings prior to applying :class:`~nwsrfs_py.nwsrfs.FA` adjustments
           
           The dictionary keys are:
           
            * **map**: Precipitation (units: mm) 
            * **mat**: Air temperature (units: degc)
            * **ptps**: Fraction of precipitation as snow (units: fraction 0-1).
              Included during input preparation only when ``ptps_logic`` is ``True``.
        

        zone_names (tuple):  Names of zones modeled in basin.
        cu_names (tuple):  Names of consuse zones modeled in basin.
        daily_flow (pd.DataFrame):  Daily mean observed flow.
        inst_flow (pd.DataFrame|None):  Instantaneous observed flow.
        upflow (pd.DataFrame|None): Upstream flow derived from the :class:`adjustq.AdjustQ`.
        fa_pars (dict[str, nwsrfs.FAPars]|None):  If ``localflow_logic`` is ``True``, than dictionary of :class:`~nwsrfs_py.nwsrfs.FAPars` classes representing ``'map'``, ``'mat'``, ``'ptps'``, and ``'pet'``, otherwise ``'None'``.
        rsnwelev_pars (nwsrfs.RSnwElevPars|None): If ``localflow_logic`` is ``True`` and ``ptps_logic`` is ``False``, a :class:`~nwsrfs_py.nwsrfs.RSnwElevPars` instance containing precipitation typing inputs; otherwise ``None``.
        sacsnow_pars (nwsrfs.SacSnowPars|None): If ``localflow_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.SacSnowPars` class, otherwise ``'None'``.
        uh_pars (nwsrfs.GammaUhPars | nwsrfs.OrdinatesUhPars | None): Parameters for
            the selected :class:`~nwsrfs_py.nwsrfs.GammaUh` or
            :class:`~nwsrfs_py.nwsrfs.OrdinatesUh` model; ``None`` without local flow.
        lagk_pars (nwsrfs.LagkPars|None): If ``upflow_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.LagkPars` class, otherwise ``'None'``.
        chanloss_pars (nwsrfs.ChanlossPars|None): If ``chanloss_logic`` is ``True``, than :class:`~nwsrfs_py.nwsrfs.ChanlossPars` class, otherwise ``'None'``.
        consuse_pars (dict[str, nwsrfs.ConsusePars]|None): If ``consuse_logic`` is ``True``, than dictionary of :class:`~nwsrfs_py.nwsrfs.ConsusePars` classes representing each ``cu_names``, otherwise ``'None'``.
        localflow_logic (bool): ``True`` when the configuration includes local runoff from SAC/Snow and a unit hydrograph.
        upflow_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Lagk` model exist in configuration.
        chanloss_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Chanloss` model exist in configuration.
        consuse_logic (bool): Logic if :class:`~nwsrfs_py.nwsrfs.Consuse` model exist in configuration.
        n_zones (integer):  Number of zones in the configuration.
        ptps_logic (bool): ``True`` when every forcing CSV contains a ``ptps``
            column; otherwise ``False`` and area-elevation data is required.
        area_elev_logic (bool): ``True`` when a single ``area_elev*`` CSV is
            present in the input file inventory, regardless of whether it is loaded.
        uh_type (str): Unit hydrograph method determined from parameter names
            in rows where ``type == 'uh'``: ``'gamma'`` for ``unit_shape`` or
            ``'ordinate'`` for numbered ``uh_<number>`` entries including ``uh_0``.
            Ambiguous or unrecognized parameter sets raise :exc:`ValueError`.

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
        Add appropriate attributes regarding climatological forcing adjustments as specified by the forcing_adj argument and if 
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

            #Check if forcing_adj string inputs match expected forcing types
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
        self.rsnwelev_pars = self.fa_pars = self.sacsnow_pars = self.uh_pars = self.lagk_pars = self.chanloss_pars = self.consuse_pars = None

        self._uh = None

        #Prepare forcing adjustments, SAC-SMA, SNOW17, and the selected UH.
        if not self.localflow_logic:
            self.fa_factors = self.forcings_climo = None            
        else:
            if not self.ptps_logic:
                self._rsnwelev_run()

            self._fa_run()
            self._sacsnow_run()

            #initate uh, depending on which version using (gamma or ordinates)
            if self.uh_type == 'gamma':
                self._gammauh_run()
            elif self.uh_type == 'ordinate':
                self._ordinatesuh_run()
            else:
                raise ValueError(f"Unknown unit hydrograph type: {self.uh_type!r}")

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

        new_pars = new_pars.sort_values('p_name')
        #Make par file edits for CHANLOSS
        new_pars = self._cl_parfile_edits(new_pars)

        #If the p_name column can't be mapped to p_name values in the existing ``pars`` attribute, then throw a error
        if not set(new_pars.p_name).issubset(self.pars.p_name):
            msg = 'All values in the p_value column of "new_pars" argument must exist in the ``pars`` attribute.'
            raise ValueError(msg)

        #Get old parameter dataframe and rename values column
        old_pars = self.pars.rename({'value':'old_value'},axis=1)

        #Merge old parameter file with new parameter file
        pars = old_pars.merge(new_pars.loc[:,['p_name','value']],on='p_name',how='left')

        #For row with which no new value was provided use the old value
        pars['value'] = pars.value.where(~pars.value.isna(),pars.old_value)
        #Reassign parameter attribute
        self.pars = pars.drop('old_value',axis=1)

        #Rerun all the NWRFS models with new pars file
        self.nwsrfs_run()

    def _parameter_arrays(self, par_type, names, zones, model=None):
        """Read required parameters, checking for missing or duplicate rows.

        Args:
            par_type (str): Value of the parameter table's ``type`` column.
            names (sequence[str]): Required parameter names.
            zones (str | sequence[str]): Required identifiers from the ``zone``
                column, in output order. Accepts a single basin name (for example,
                ``'SSSW1'``), or zone, upstream reach, or channel-loss period names.
            model (str | None): Model label used in error messages.

        Returns:
            dict[str, np.ndarray]: Parameter arrays aligned with ``zones``.
                A single identifier produces one-element arrays.

        Raises:
            ValueError: A required parameter is missing or duplicated. These
                structural checks also run when wrapper validation is disabled.
        """
        model = model or par_type
        zones = (zones,) if isinstance(zones, str) else tuple(zones)
        if not zones:
            raise ValueError(f"{model} requires at least one basin, zone, reach, or period.")
        parameters = self.pars.loc[self.pars.type == par_type]
        result, errors = {}, []
        for name in names:
            rows = parameters.loc[parameters.name == name]
            rows = rows.loc[rows.zone.isin(zones)]
            counts = rows.groupby('zone').size().reindex(zones, fill_value=0)
            invalid = counts[counts != 1]
            if not invalid.empty:
                details = ', '.join(f"{zone} (found {count})" for zone, count in invalid.items())
                errors.append(f"{name}: {details}")
            else:
                result[name] = rows.set_index('zone').value.reindex(zones).to_numpy()
        if errors:
            raise ValueError(
                f"{model} requires exactly one value for each required parameter "
                f"with type == '{par_type}'; " + '; '.join(errors))
        return result

    def _rsnwelev_run(self, validate: bool = True):
        """
        Calculate precipitation typing from unadjusted temperature and elevation.

        For each modeled zone, ``pars`` must contain exactly one ``pxtemp``
        and ``talr`` parameter with ``type == 'rsnwelev'``, and one ``elev``
        parameter with ``type == 'snow'``.  ``area_elev`` must have and an elevation 
        column named for each zone. The index is loaded from the first CSV column 
        regardless of its original header.

        The calculated fractions populate ``forcings_raw['ptps']``. PTPS is
        removed from ``forcing_adj_types`` before :meth:`_fa_run` is called.
        ``ptps_logic`` remains ``False`` so parameter updates recalculate PTPS.
        Supplied PTPS and routing-only configurations are left unchanged.

        Args:
            validate (bool): Validate :class:`~nwsrfs_py.nwsrfs.RSnwElevPars`
                inputs. Default: ``True``.

        Raises:
            ValueError: Required zone parameters or area-elevation columns
                are missing, or zone parameters are duplicated.
        """
        if not self.localflow_logic or self.ptps_logic:
            return None

        pars_dict = self._parameter_arrays(
            'rsnwelev', ['pxtemp', 'talr'], self.zone_names, 'RSnwElev')
        pars_dict['taelev'] = self._parameter_arrays(
            'snow', ['elev'], self.zone_names, 'RSnwElev')['elev']

        #The first CSV column was loaded as the nonexceedance-fraction index.
        required_columns = list(self.zone_names)
        if self.area_elev is None:
            raise ValueError('RSnwElev requires area-elevation data.')
        missing_columns = [name for name in required_columns if name not in self.area_elev.columns]
        if missing_columns:
            raise ValueError(
                f"Area-elevation CSV is missing required columns: {', '.join(missing_columns)}.")

        rsnwelev_dc = nwsrfs.RSnwElevPars(
            year=self.year, month=self.month, day=self.day, hour=self.hour,
            pxtemp=pars_dict['pxtemp'], taelev=pars_dict['taelev'], talr=pars_dict['talr'],
            aetbl_index=self.area_elev.index.to_numpy(),
            aetbl_values=self.area_elev.loc[:, list(self.zone_names)].to_numpy(),
            forcings_mat=self.forcings_raw['mat'].loc[:, list(self.zone_names)].to_numpy())

        #Initialize the parent and evaluate its lazy PTPS output.
        nwsrfs.RSnwElev.__init__(self, pars_dataclass=rsnwelev_dc, validate=validate)
        ptps = nwsrfs.RSnwElev.forcing_ptps.fget(self)
        self.forcings_raw['ptps'] = ptps.set_axis(self.zone_names, axis=1)
        self.forcing_adj_types = tuple(f for f in self.forcing_adj_types if f != 'ptps')

    def _fa_run(self,validate:bool = True):
        '''
        Apply monthly climatological forcing adjustments to forcing specified by the ``forcing_adj_types`` attribute.

        Monthly lower and upper limits are read from ``pars`` only for enabled
        forcing adjustments. Disabled adjustments use the Fortran default-limit
        sentinel ``-999.0`` and do not require limit parameters in the CSV.
        
        Args:
            validate (bool): Validate that map, mat, ptps, and pet inputs are correct format/type. Default: True
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #If forcing_adj_types is None or then set all fa_pars values to scale=1, p_redist=0, std=10, shift=0
        nofa_pars = np.array([1,0,10,0])
        #fa adjustment parameters:  scale, p_redist, std, shift 
        basin_name = self.zone_names[0].split('-', 1)[0]
        fa_pars = {}
        for f in ['map','mat','ptps','pet']:
            #Turn off fa adjustments for all forcings not within forcing_adj_types attribute 
            if f not in self.forcing_adj_types:
                fa_pars[f] = nofa_pars
            #Otherwise use forcing adjustment parameters provided in pars
            else:
                names = [f'{f}_{suffix}' for suffix in ['scale', 'p_redist', 'std', 'shift']]
                values = self._parameter_arrays('fa', names, basin_name, 'FA')
                fa_pars[f] = np.array([values[name].item() for name in names])
            
        #Use Fortran default limits unless forcing adjustments are enabled.
        fa_limits = {key: np.full([12, 2], -999.0) for key in ['map','mat','ptps','pet']}
        for f in self.forcing_adj_types:
            names = [f'{f}_{bound}_{month:02}' for month in range(1, 13)
                     for bound in ['lower', 'upper']]
            #FA limits are shared; preserve the first-zone convention.
            values = self._parameter_arrays('fa_limit', names, self.zone_names[:1], 'FA')
            fa_limits[f] = np.array([values[name].item() for name in names]).reshape(12, 2)
        monthly_names = [f'peadj_{month:02}' for month in range(1, 13)]
        monthly = self._parameter_arrays('sac', monthly_names, self.zone_names, 'FA')
        peadj = {key: None for key in ['map', 'mat', 'ptps']}
        peadj['pet'] = np.stack([monthly[name] for name in monthly_names])

        #Make dummy climo input
        climo = None

        # Create forcing arrays
        forcings_raw = {}
        for f in ['map','mat','ptps']:
            forcings_raw[f] = self.forcings_raw[f].to_numpy()
        forcings_raw['pet'] = None

        #Get alat and area parameters
        alat = self._parameter_arrays('snow', ['alat'], self.zone_names, 'FA')['alat']
        area = self._parameter_arrays('uh', ['zone_area'], self.zone_names, 'FA')['zone_area']

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


    def _sacsnow_run(self, validate:bool=True):
        '''
        Run SAC-SMA and SNOW-17 with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that SAC-SMA and Snow17 inputs are correct format/type. Default: True
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #These lists also define the order expected by the Fortran wrapper.
        sac_names = ['uztwm', 'uzfwm', 'lztwm', 'lzfpm', 'lzfsm', 'adimp',
                     'uzk', 'lzpk', 'lzsk', 'zperc', 'rexp', 'pctim',
                     'pfree', 'riva', 'side', 'rserv', 'efc']
        snow_names = ['scf', 'mfmax', 'mfmin', 'uadj', 'si', 'nmf', 'tipm',
                      'mbase', 'plwhc', 'daygm', 'adc_a', 'adc_b', 'adc_c']
        required = {
            'sac': sac_names + ['pxadj', 'peadj'],
            'snow': snow_names + ['alat', 'elev', 'init_swe']
        }
        pars_dict = {
            par_type: self._parameter_arrays(par_type, names, self.zone_names, 'SacSnow')
            for par_type, names in required.items()
        }

        #Format required dataclass inputs sac_pars and snow_pars
        sac_pars = np.stack([pars_dict['sac'][name] for name in sac_names])
        snow_pars = np.stack([pars_dict['snow'][name] for name in snow_names])

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
            * **aesc**: Areal extent of snow cover (units: fraction 0-1).
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

    def _gammauh_run(self, validate:bool=True):
        '''
        Run gamma UH with the parameters specified in ``pars``.

        Args:
            validate (bool): Validate that UH inputs are correct format/type. Default: True
        '''

        if not self.localflow_logic:
            return None

        names = ['zone_area', 'unit_shape', 'unit_toc', 'unit_toc_adj']

        pars_dict = self._parameter_arrays('uh', names, self.zone_names, 'GammaUh')

        #Apply toc adjustment to toc parameter
        toc = pars_dict['unit_toc'] * pars_dict['unit_toc_adj'] 

        #Initate gammauh_pars data class
        self.uh_pars = nwsrfs.GammaUhPars(dt_hours = self.dt_hours, area = pars_dict['zone_area'], 
                        shape = pars_dict['unit_shape'], toc = toc)

        #Initiate GammaUh wrapper class
        self._uh = nwsrfs.GammaUh(pars_dataclass = self.uh_pars,validate = validate)  

    def _ordinatesuh_run(self, validate: bool = True):
        """Prepare and initialize the supplied-ordinate unit hydrograph.

        Rows with ``type == 'uh'`` must contain ``zone_area`` and numbered
        ``uh_0``, ``uh_1``, ... parameters for each modeled zone. Ordinate
        numbers must be consecutive from zero and values are in cfs/in at
        ``dt_hours`` intervals. Shorter zone hydrographs are padded with NaN.

        Args:
            validate (bool): Validate :class:`~nwsrfs_py.nwsrfs.OrdinatesUhPars`
                inputs, including hydrograph volume. Default: ``True``.

        Raises:
            ValueError: Required parameters are missing, duplicated, or ordinate
                numbering is not consecutive from zero.
        """
        if not self.localflow_logic:
            return None

        area = self._parameter_arrays(
            'uh', ['zone_area'], self.zone_names, 'OrdinatesUh')['zone_area']
        rows = self.pars.loc[(self.pars.type == 'uh') &
                             self.pars.name.str.fullmatch(r'uh_[0-9]+', na=False)]
        zone_ordinates = []
        for zone in self.zone_names:
            names = sorted(rows.loc[rows.zone == zone, 'name'].unique(),
                           key=lambda name: int(name.split('_')[1]))
            if not names or names != [f'uh_{i}' for i in range(len(names))]:
                raise ValueError(
                    f"OrdinatesUh requires consecutive uh_0, uh_1, ... parameters "
                    f"for zone {zone}; found: {', '.join(names) or 'none'}.")
            ordin_dict = self._parameter_arrays('uh', names, zone, 'OrdinatesUh')
            zone_ordinates.append(np.array([ordin_dict[name].item() for name in names]))

        ordinates = np.full((max(map(len, zone_ordinates)), len(self.zone_names)), np.nan)
        for i, values in enumerate(zone_ordinates):
            ordinates[:len(values), i] = values
        self.uh_pars = nwsrfs.OrdinatesUhPars(
            dt_hours=self.dt_hours, area=area, ordinates=ordinates)
        self._uh = nwsrfs.OrdinatesUh(pars_dataclass=self.uh_pars, validate=validate)

    def return_uh(self,tstep):
        '''
        Generates a unit hydrograph as a DataFrame, using the selected UH model, at a timestep specified by ``tstep``.

        Args:
            tstep (int): Specifies timestep of unit hydrograph (units: hours).
        Returns:
             pd.DataFrame: A DataFrame containing unit hydrograph (uh) with a column for each zone (units: cfs/in).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        raw_output = self._uh.return_uh(tstep)

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    @property
    def uh(self) -> pd.DataFrame:
        '''
        Generates a unit hydrograph as a DataFrame, using the selected UH model, at a timestep specified by the ``dt_hours`` attribute (units - cfs/in).
        '''

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #note:  call is the same whether using GammaUH or OrdinatesUH
        raw_output = self._uh.uh

        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    def return_sf(self,tci:SACSnowTCI):
        '''
        Generates a timeseries of streamflow as a DataFrame using the selected UH model and input ``tci``.

        Args:
            tci (SACSnowTCI): Custom type alias for total channel inflow from :class:`~nwsrfs_py.nwsrfs.SacSnow`  (units:  mm).
        Returns:
            pd.DataFrame: A DataFrame containing streamflow with a column for each zone (units: cfs).
        '''

        #If SAC-SMA and SNOW17 parameter don't exist return none
        if not self.localflow_logic:
            return None

        #note:  call is the same whether using GammaUH or OrdinatesUH
        raw_output = self._uh.return_sf(tci, return_inst=self.return_inst)
        #Rename column of Dataframe to correpond to zone names
        return raw_output.set_axis(self.zone_names, axis=1)

    @property
    def sacsnow_sf(self)-> pd.DataFrame:
        '''
        Calculates streamflow for each zone using :class:`~nwsrfs_py.nwsrfs.SacSnow` and the selected UH model (units - cfs).
        '''

        #If SAC-SMA and SNOW17 parameters don't exist return none
        if not self.localflow_logic:
            return None

        #Get tci from sacsnow class
        tci_output = self.sacsnow_tci

        #Route through the selected unit hydrograph.
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

        names = ['lagtbl_a', 'lagtbl_b', 'lagtbl_c', 'lagtbl_d',
                 'ktbl_a', 'ktbl_b', 'ktbl_c', 'ktbl_d',
                 'lagk_lagmax', 'lagk_lagmin', 'lagk_kmax', 'lagk_kmin',
                 'lagk_qmax', 'lagk_qmin', 'init_co', 'init_stor', 'init_if', 'init_of']
        pars_dict = self._parameter_arrays('lagk', names, self.upflow_names, 'LagK')

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

        #Period parameters use synthetic *_CLxx zones; controls are shared.
        period_names = ['cl_period_start', 'cl_period_end', 'cl_factor']
        period_zones = tuple(sorted(self.pars.loc[
            (self.pars.type == 'chanloss') & self.pars.zone.str.contains('_CL'),
            'zone'].unique()))
        pars_dict = self._parameter_arrays('chanloss', period_names, period_zones, 'Chanloss')
        basin_name = period_zones[0].rsplit('_CL', 1)[0]
        pars_dict.update(self._parameter_arrays(
            'chanloss', ['cl_type', 'cl_min_q'], basin_name, 'Chanloss'))

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

        monthly_names = [f'peadj_cu_{month:02}' for month in range(1, 13)]
        names = ['area_km2', 'irr_eff', 'min_flow', 'rf_accum_rate', 'rf_decay_rate']
        pars_dict = self._parameter_arrays(
            'consuse', names + monthly_names, self.consuse_names, 'Consuse')
        peadj_cu = np.stack([pars_dict[name] for name in monthly_names])
        pet_adjustments = self._parameter_arrays(
            'sac', ['peadj'], self.consuse_names, 'Consuse')['peadj']

        #Get natural flow
        qnat = self._sacsnow_lagk_chanloss_sim

        #Convert natural streamflow to daily average
        qnat_peravg = self._uh._inst_to_ave(qnat.to_numpy())
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

        #Run consuse for each zone individually
        for n, cu_name in enumerate(self.consuse_names):

            #Get peadj associated with cu zones
            peadj = pet_adjustments[n]

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
        Generates a Series of the sum of any local flow runoff (:class:`~nwsrfs_py.nwsrfs.SacSnow` and the selected UH model)  and upstream routed (:class:`~nwsrfs_py.nwsrfs.Lagk`) flow for each timestep
        with chanloss (:class:`~nwsrfs_py.nwsrfs.Chanloss`) and consuse (:class:`~nwsrfs_py.nwsrfs.Consuse`) applied (units - cfs).
        '''

        #Get flow prior to potential CONSUSE adjustment
        qnat = self._sacsnow_lagk_chanloss_sim

        #If there area CONSUSE areas, adjust the flow
        if self.consuse_logic:

            qnat_cu_adj = self.consuse_states['qdiv'].sum(axis=1)-self.consuse_states['qrf_out'].sum(axis=1)
            #Shift forward a day so that the correct adjustment is applied  to the correct day
            qnat_cu_adj.index = qnat_cu_adj.index+pd.Timedelta(1, unit='D')
            #Backfill to fill all values after 00:00 and fill in nan values at the end of time series with last daily 
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


