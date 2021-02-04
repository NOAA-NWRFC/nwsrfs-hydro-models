#f2py command: f2py -c -m model_lagk fintp7.f fka7.f flag7.f fop7.f
#              fprpc7.f fserc7.f fslag7.f initCommonBlocksPin.f pin7.f 
#              pina7.f umemov.f umemst.f 
#
#loading the library to python:  lagk_f2py.model_lagk as lagk                  
#
#There are three subroutines to execute:  pin7, flag7, fka7
#subroutines should be ran in the order presented
#
#pin7:  Formats inputs (via a P and C array) run LagK operations
#
#   p,c = pin7(ita,itb,jlag,jk,meteng,lagtbl,ktbl,ico,iinfl,ioutfl,istor)
#          p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
#              2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
#              Input output
#          c:   output from pin7 subroutine. Contains initial, inflow, 
#               utflow, storage, and carryover values
#         ita:  Data time interval of the inflow time series (HR)
#         itb:  Data time interval of the outflow time series (HR)
#         jlag: If > 0 - number of pairs of Lag and Q values used to define the
#               variable Lag vs Q curve.  If = 0 - constant Lag will be used
#         jk:   If > 0 - number of pairs of K and Q values used to define the
#               variable K vs Q curve.  If = 0 - constant K will be used
#         meteng:  Code specifying whether units of q, lag, k parameters and initial
#                 values are English or metric:  'ENGL' = enter flow in CFS and 
#                 volume in CFSD,'METR' = enter flow in CMS and volume in CMSD, 
#                 Default is metric.  Note output:  P and C output is ALWAYS converted
#                 to metric
#         lagtbl:  If jlag=0, constant Lag value.  If jlag>0, lag and q pairs, in
#                   that order, in a single column array.  Must be in ascending order
#                   of q (example: [6, 0, 4, 10000, 3.5, 20000]).
#         ktbl:  If jk=0, constant K value.  If jk>0, K and q pairs, in
#                   that order, in a single column array.  Must be in ascending order
#                   of q (example: [1, 100, 1, 40000, 3, 100000]).
#         ico:  Initial carry over 
#         iinfl:  Initial inflow
#         ioutfl:  Initial outflow
#         istor:  Initial storage
#
#  !!Notes: 
#           1) This routine handles all the variables which would be optimized:  
#              lagtbl, ktble, ico, iinfl, ioutfl, istor
#           2) The pin7.f subroutine was edited to start with a empty c/p array
#              far larger than which should be needed [p(500),c(100)].  Below is 
#              python code I used to chop the unused lines after executing the
#              subroutine. This is not necessary for flagk, and fka subroutines to
#              properly run.  I used this document, pg 1-3, as reference:
#              https://www.nws.noaa.gov/ohd/hrl/nwsrfs/users_manual/part8/_pdf/833lagk.pdf
#               k_start=int(p[17])
#               k_len=int(p[k_start-1])SAKW1
#               pina7_len=int(p[k_start+2*k_len])
#               p_end=k_start+2*(k_len+pina7_len)+1
#               p=p[:p_end]
#               c=c[:int(c[0])]
#
#flag7:  Controls the Lag Operation
#    qb = flag7(p,c,qa,[ndt])
#
#            qb: downstream streamflow values (single column array) with only
#                lag applied, time step is assumed to correspond to itb.  
#            p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
#                2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
#                Input output
#            c:  output from pin7 subroutine. Contains initial, inflow, 
#                outflow, storage, and carryover values.  !!NOTE!! use a copy of
#                 the original c array as it get edited during the subroutine
#            qa: Upstream streamflow values (single column array), time step is
#                assumed to correspond to ita
#            ntd:  Optional variable, total number to time steps to process.
#                  if less than full qa array is desired 
#
#fka7:    Perform the attenuation (K) computations 
#    qc = flag7(p,c,qb,[ndt])
#
#            qc: downstream streamflow values (single column array) with both
#                lag and attenuation applied, time step is assumed to correspond
#                to itb.  
#            p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
#                2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
#                Input output
#            c:  output from pin7 subroutine. Contains initial, inflow, 
#                outflow, storage, and carryover values
#            qb: downstream streamflow values (single column array) with only
#                lag applied, time step is assumed to correspond to itb
#            ntd: Optional variable, total number to time steps to process.
#                  if less than full qa array is desired 
#
#--------------------Python Code Example------------------------------------------------
#
import os, sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import lagk_f2py.model_lagk as lagk
#
#This is using an example workbook with information pulled from a CHPS SA with Sauk near Sauk (SAKW1)streamflow routed to Skagit 
#near Concrete (CONW1) information. This is pulling in the upstream (SAKW1) and downstream (SAKW1R) flow.
sakw1_chps=pd.read_excel(os.path.join(folder,'SAKW1R_Lagk_Check.xlsx'),'Flows',index_col='Date_Time',parse_dates=True)
sakw1=sakw1_chps.SAKW1.astype('f4').to_numpy()
#Note:  There is a similar workbook of Skagit near Concrete (CONW1) to SKAGIT--NEAR MT VERNON (MVEW1) routing information
#
#pin7:  ita
ts_in=6
#pin7:  itb
ts_out=6
#pin7:  meteng
units='ENGL'#'METR'
#pin7:  lagtbl (pulled from wb: 'SAKW1R_Lagk_Check.xlsx', ws: 'Parameters'
lagq=np.array([6, 0, 4, 10000, 3.5, 20000, 3, 40000, 3, 100000],dtype='f4')
#pin7:  jlag
len_lagq=len(lagq)/2
#pin7:  ktbl (pulled from wb: 'SAKW1R_Lagk_Check.xlsx', ws: 'Parameters'
kq=np.array([1, 100, 1, 40000, 3, 100000],dtype='f4')
#pin7:  jk
len_kq=len(kq)/2
#pin7:  ico
init_co=float(6025)
#pin7:  iinfl
init_if=float(6018)
#pin7:  ioutfl
init_of=float(6018)
#pin7:  istor
init_stor=float(6307)
#
#Run the pin7 subroutine
p, c = lagk.pin7(ts_in,ts_out,len_lagq,len_kq,units,lagq,kq,init_co,init_if,init_of,init_stor)
#
###Optional removal of unused p and c array rows###
k_start=int(p[17])
k_len=int(p[k_start-1])
pina7_len=int(p[k_start+2*k_len])
p_end=k_start+2*(k_len+pina7_len)+1
p=p[:p_end]
c=c[:int(c[0])]
###################################################
#
#Run flag7 subroutine.  Note a copy of the c array is made, as the flag7 subroutine edits the array.  Also
#streamflow data is converted from cfs to cubic meter per second.
c_copy=c.copy()
qb=lagk.flag7(p,c_copy,sakw1*0.0283168)
#
#Run the fka7 subroutine
qc=lagk.fka7(p,c_copy,qb)
#
#Optional visualization of output:
#
fortran_output=pd.DataFrame({'Lagged':qb,'Attenuated':qc},index=sakw1_chps.index
#
#Controls the range of the x axis
d_start='10//1981'
d_end='9/30/2015'
#
plt.plot(sakw1_chps.loc[d_start:d_end].SAKW1*0.0283168,label='IN')
plt.plot(fortran_output.loc[d_start:d_end].Lagged,label='Lagged')
plt.plot(fortran_output.loc[d_start:d_end].Attenuated,label='Attenuated')
plt.plot(sakw1_chps.loc[d_start:d_end].SAKW1R*0.0283168,label='CHPS_Output')
#
#formatting
plt.xlabel('Timestep') 
plt.ylabel('Streamflow (cms)')
plt.legend() 
plt.rcParams["figure.figsize"] = (10,10