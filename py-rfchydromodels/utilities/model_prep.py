#This function reads in a folder created by the NWRFC autocalibration
#preprocess.R routine and the a subfolder, created during the optimization 
#routine, and builds all of the inputs required to run the model.py "Model" wrapper.
#if no subfolder is provided, the fuction defaults to using "results_01".

#Created by:  Geoffrey Walters PE, 6/10/21

import os, sys
import pandas as pd
import numpy as np

def nwsrfs_prep(autocalb_dir, results_dir_name='results_01'):

    autocalb_files=pd.DataFrame(columns=['path','file_name']) 
    n=1

    for root, dirs, files in os.walk(autocalb_dir):
        for file in files:
            if '.csv' in file:

                autocalb_files=pd.concat([autocalb_files,
                                          pd.DataFrame({'path':root,
                                                       'file_name':file},index=[n])],axis=0)
                n=n+1

    autocalb_files['Folder']=autocalb_files.path.str.split('\\').str[-1]
    autocalb_files=autocalb_files.loc[(~autocalb_files.Folder.str.contains('results'))|(autocalb_files.Folder.str.contains(results_dir_name))]
    
    pars_df=autocalb_files.loc[autocalb_files.file_name=='pars_optimal.csv'].squeeze()
    pars=pd.read_csv(os.path.join(pars_df.path,pars_df.file_name))

    flow_df=autocalb_files.loc[(autocalb_files.file_name.str.startswith('flow_'))].squeeze()
    flow=pd.read_csv(os.path.join(flow_df.path,flow_df.file_name))
    flow.index=pd.to_datetime(flow[['year', 'month', 'day']])
    flow=flow.resample('6h').ffill()

    forcing=[]

    forcing_df=autocalb_files.loc[(autocalb_files.file_name.str.contains('forcing_por'))].copy()
    forcing_df.sort_values(by='file_name',inplace=True)

    for index, row in forcing_df.iterrows():
        forcing_calb=pd.read_csv(os.path.join(row.path,row.file_name))
        forcing_calb.index=pd.to_datetime(forcing_calb[['year', 'month', 'day', 'hour']])
        forcing.append(forcing_calb)

    upflow=[]

    upflow_df=autocalb_files.loc[(autocalb_files.file_name.str.contains('upflow_por'))].copy()
    upflow_df.sort_values(by='file_name',inplace=True)

    for index, row in upflow_df.iterrows():
        upflow_calb=pd.read_csv(os.path.join(row.path,row.file_name))
        upflow_calb.index=pd.to_datetime(upflow_calb[['year', 'month', 'day', 'hour']])
        upflow.append(upflow_calb)
    return forcing, pars, upflow, flow 