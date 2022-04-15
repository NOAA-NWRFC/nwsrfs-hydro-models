09/04/08                           SUMMARY OF RESJ STATES (CARRY OVER)
02/24/11 increase index size 4 to 6

COMPONENT: (3) reservoir, reach (lagk), and node

METHOD:    (7) adjust, lookup3, calcinflow, spillway, setelevation, setrelease, setwithdraw

==========================================================================================================
RESERVOIR: RESERVOIR[8]  --> 30 (before value) + 64(value) + 80(future) = 172 bytes
==========================================================================================================
RESERVOIR_Id_INITIALRELEASE[30+8]=value   (double value must be follow)
RESERVOIR_Id_INITIALPOOL[38+8]=value      
RESERVOIR_Id_INITIALWITHDRAW[46+8]=value  
RESERVOIR_Id_INITIALINFLOW[54+8]=value   
RESERVOIR_Id_PREVIOUSRELEASE[62+8]=value  
RESERVOIR_Id_PREVIOUSPOOL[70+8]=value     
RESERVOIR_Id_PREVIOUSWITHDRAW[78+8]=value 
RESERVOIR_Id_PREVIOUSINFLOW[86+8]=value   

              Id       Index Values ........................................................
|----12----||----12----|-6-||--8---||--8---||8-----||-8----||-8----||--8---||8-----||-8----||----80--------
RESERVOIR   BALLMTN      17239.99752263.64750.00000028.7303642.85258264.42030.00000031.88484*FUTURE**FUTURE

sprintf(value_str, "%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s%8.8s",
                   rel_str, pool_str, with_str, inflow_str, prevRel_str,
                   prevPool_str, prevWithdraw_str, prevInflow_str);

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%64.64s%80.80s",
                   "RESERVOIR", _id, "-99999", value_str, future_str );

==========================================================================================================
NODE: NODE[6]	         --> 30(before value) + 16(value) + 40(future) = 84 bytes
==========================================================================================================

NODE_Id_DISCHARGE[30+8]=value   (double value must follow)
NODE_Id_PREVIOUSDISCHARGE[38+8]=value (double value must follow)         
NODE_Id_INFLOW[46+8]=value or -888 (for "*FUTURE*")
NODE_Id_PREVIOUSINFLOW[54+8]=value or -888 (for "*FUTURE*")   
NODE_Id_DIVERSION[62+8]=value or -888 (for "*FUTURE*") or -999 (for "MISS" from old CO)
NODE_Id_PREVIOUSDIVERSION[70+8]=value or -888 (for "*FUTURE*") or -999 (for "MISS" from old CO)
 
               Id      Index Values.........................................
|----12----||----12----|-6-||--8---||--8---||8-----||-8----||-8----||--8---||----40---------
NODE        FLD          5120.0000000.000000*FUTURE**FUTURE**FUTURE**FUTURE**FUTURE**FUTURE*
NODE        WLPN3        428824.1630752.0431824.1630752.04310.0000000.000000*FUTURE**FUTURE*

sprintf(value_str, "%8.8s%8.8s", discharge_str, prevDischarge_str);

sprintf(future_str, "%8.8s%8.8s%8.8s%8.8s%8.8s", inflow_str, prevInflow_str, 
		    diversion_str, prevDiversion_str, future);

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%16.16s%40.40s", 
		   "NODE", _id, "99-999", value_str, future_str );

// Check for the new INFLOW and DIVERSION carryover
if( strncasecmp( &value[12+12+6+8+8], "*FUTURE*", 8 ) )
{ 
 // We didn't match.  We have a new carryover.
   NewCO = 1;
}
//Check for the old DIVERSION but missing value	
else 
{
 // We matched, so we have old carryover.
   NewCO = 0;
}

==========================================================================================================
REACH: LAGK[3+numinflow] --> 54(before value) + inflowCO + 24(value) + 28(future)
==========================================================================================================
LAGK3HOUR_id__COINFLOW_#0[54+8]=value
....
LAGK3HOUR_id__COINFLOW_#n[62+8]=value

LagkId_OwnerId_INITIALOUTFLOW[54+NumCOinflow+8]=value (VALUE MUST FOLLOW)
LagkId_OwnerId_INITIALSTORAGE[62+NumCOinflow+8]=value
LagkId_OwnerId_INITIALLAGGEDINFLOW[70+NumCOinflow+8]=value

            OwnerId    Index LagkId     Method type Values .................................
|----12----||----12----|-6-||----12----||----12----||8-----||-8----||-8----||--8---||-8----||----28----
REACH       WEST_RIV    1448LAG3HOUR    LAGK        42.8525839.9975241.425050.00000041.42505*FUTURE**FUT

char tmp_str1[54 + 512], tmp_str2[512], tmp_str3[9] ;
char future_str[8 * 3 + 4 + 1] ;

sprintf ( tmp_str1, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s",
                     "REACH", _owner->_id, "-99999", _id, "LAGK" ) ;

COINFLOW: VALUE
loop index
  strncpy(tmpval, &value[42+NewCO*12+index*8], 8);
end loop
sizeInflowCO = index
    // Prepare Inflow data
    tmp_str2[0] = '\0' ;
    for ( i = 0 ; i < _sizeInflowCO ; i++ )
    {
        sprintf ( tmp_str3, "%f", _co_inflow[i] ) ;
        strncat ( tmp_str2, tmp_str3, 8 ) ;
    }
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the current outflow
    sprintf ( tmp_str2, "%f", _owner->_outflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the current storage
    sprintf ( tmp_str2, "%f", _storageCO ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

    // Now add the lagged inflow
    sprintf ( tmp_str2, "%f", _laggedInflow ) ;
    tmp_str2[8] = '\0' ;
    strcat ( tmp_str1, tmp_str2 ) ;

==========================================================================================================
METHOD: LOOKUP3[5]       --> 54(before value) + 24(value) + 40(future) = 116 bytes
==========================================================================================================
Note: blendts and blendtbl must be an integer number of time steps. Default 1 means no blending.

LOOKUP3_Id_OwnerId_BLENDTS[54+4]=value  
LOOKUP3_Id_OwnerId_BLENDTBL[58+4]=value
LOOKUP3_Id_OwnerId_MAXROW[62+4]=value or -999 (for "SKIP")
LOOKUP3_Id_OwnerId_MAXCOL[66+4]=value or -999 (for "SKIP")
LOOKUP3_Id_OwnerId_INITIALTRANSFER[70+8]=value or -999 (for "MISS") 

            Id         Index            OwnerId     Values .................
|----12----||----12----|-6-||----12----||----12----||--||--||--||--||-8----||-----40-----
METHOD      NORMREPLACE 1196LOOKUP3     BALLMTN        1   1SKIPSKIP27.80968*FUTURE**FUTU
METHOD      HIGHRECOVER 1036LOOKUP3     OOL            1   1SKIPSKIPMISS.000*FUTURE**FUTU


printf(value_str, "%4.4s%4.4s%4.4s%4.4s%8.8s", tsStep_str, tblStep_str,
                  colI_str, rowI_str, lastVal_str);
sprintf( temp_str, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%24.24s%40.40s",
         "METHOD", _id, "-99999", "LOOKUP3", _owner->_id, value_str, future_str );


==========================================================================================================
METHOD: ADJUST[1]        --> 54(before value) + 4(value) + 16(future) = 72 bytes
==========================================================================================================

ADJUST_Id_OwnerId_BLENDTS[54+4]=value (MUST BE INTEGER NUMBER)
	
             Id        Index            OwnerId     Valu
|----12----||----12----|-6-||----12----||----12----||4-||------16------|
METHOD      SET_TO_OBS  1344ADJUST      BALLMTN        1*FUTURE**FUTURE*


char tsStep_str[5], future_str[17], value_str[5];
char temp_str[54+4+16+1];

sprintf(value_str, "%4.4s", tsStep_str );

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%4.4s%16.16s",
                   "METHOD", _id, "-99999", "ADJUST", _owner->_id, value_str, future_str );

==========================================================================================================
METHOD: CALCINFLOW[5]    --> 54(before value) + 48(value) + 80(future) = 180 bytes
==========================================================================================================
Note: double value must follow or -999. for "MISSINGX"

CALCINFLOW_Id_Ownerid_REMAININGVOL[54+16]=value (USR SHOULD ASSIGN THIS VARIABLE A VALUE OF 0 WHEN
	                                         RE(PARAMETERIZING) or string (i.e "MISSINGX")
CALCINFLOW_Id_Ownerid_STARTINFLOW[70+8]=value or -999. for "MISSINGX"
CALCINFLOW_Id_Ownerid_STARTPOOL[78+8]=value or  -999. for "MISSINGX"
CALCINFLOW_Id_Ownerid_STARTRELEASE[86+8]=value or -999. for "MISSINGX"
CALCINFLOW_Id_Ownerid_STARTWITHDRAW[94+8]=value or -999. for "MISSINGX"

              Id       Index            OwnerId    Values .........................................
|----12----||----12----|-6-||----12----||----12----|------16------||--8---||--8---||--8---||--8---||----80--
METHOD      CATCH       1004CALCINFLOW  BALLMTN            10.0000012.23444666.43210.0000000.000000*FUTURE**    

char temp_str[54+48+80+1], future_str[81], value_str[49];

sprintf( value_str, "%16.16s%8.8s%8.8s%8.8s%8.8s", remainingVol_Str,
                startInflow_Str, startPool_Str, startRelease_Str, startWithdrawal_Str );
value_str[48] = '\0';

sprintf( temp_str,
                "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%48.48s%80.80s",
                "METHOD", _id, "-99999", "CALCINFLOW", _owner->_id, value_str, future_str );

==========================================================================================================
METHOD: SETRELEASE/SETELEVATION  --> 54(before value) + 8(value) + 16(future) = 76 bytes
==========================================================================================================

SETRELEASE_Id_Ownerid_BLENDTS[54+4]=value  (MUST BE INTEGER NUMBER OF TIME STEPS)
SETRELEASE_Id_Ownerid_BLENDTBL[58+4]=value ("          "         "           "  )

               Id      Index            OwnerId     Values..
|----12----||----12----|-6-||----12----||----12----||4-||4-||-----16-------|
METHOD      FLD_MINQ1   1816SETRELEASE  TOWNSHD        1   1*FUTURE**FUTURE*
METHOD      TPOOLWEIR   1968SETELEVATIONTOWNSHD        1   1*FUTURE**FUTURE*

setelevation:
char temp_str[54+8+16+1], tsStep_str[5], tblStep_str[5], future_str[17],
value_str[11];

sprintf(value_str, "%4.4s%4.4s", tsStep_str, tblStep_str);
sprintf( temp_str,"%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%8.8s%16.16s",
         "METHOD", _id, "-99999", "SETELEVATION", _owner->_id, value_str, future_str );

setrelease:
char tsStep_str[5], tblStep_str[5], future_str[17], value_str[9];
char temp_str[54+8+16+1];
 
sprintf( tsStep_str, "%d", _ts_step );
sprintf( tblStep_str, "%d", _tbl_step );

sprintf(value_str, "%4.4s%4.4s", tsStep_str, tblStep_str);

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%8.8s%16.16s",
                "METHOD", _id, "-99999", "SETRELEASE", _owner->_id, value_str, future_str );

==========================================================================================================
METHOD: SETWITHDRAW[2]   --> 52(before value) + 16(value) + 8(future) = 76 bytes 
==========================================================================================================

SETWITHDRAW_Id_Ownerid_BLENDTS[54+4]=value or "NONE" (for no value)
SETWITHDRAW_Id_Ownerid_BLENDTBL[58+4]=value or "NONE" (for no value)
SETWITHDRAW_Id_Ownerid_INITIALTRANSFER[62+8]=value (DEFAULT double value is 0.000000)

	        Id     Index            OwnerID     Values .........
|----12----||----12----|-6-||----12----||----12----||4-||4-||---8--||---8--|
METHOD      FLD_FLAG4   2044SETWITHDRAW TOWNSHD        4    0.000000*FUTURE*

char tsStep_str[5], tblStep_str[5], transferVal_str[8],future_str[9],
value_str[17], temp_str[54+16+8+1];

sprintf( tsStep_str, "%d", _ts_step );
sprintf( tblStep_str, "%d", _tbl_step );
sprintf( transferVal_str, "%f", _myValue );

sprintf(value_str, "%4.4s%4.4s%8.8s", tsStep_str, tblStep_str, transferVal_str);

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%16.16s%8.8s",
                   "METHOD", _id, "-99999", "SETWITHDRAW", _owner->_id, value_str, future_str );

==========================================================================================================
METHOD: SPILLWAY[1]     --> 54(before value) + 8(value) + 80(future) = 140 bytes
==========================================================================================================

SPILLWAY_Id_Ownerid_INITIALSPILL[54+8]=value   (default double value 0.000000)

	         Id    Index            OwnerId     Values..
|----12----||----12----|-6-||----12----||----12----||--8---||-------80---|
METHOD      TSPILL      1740SPILLWAY    TOWNSHD     0.000000*FUTURE**FUTU*

char temp_str[54+8+80+1],value_str[9]

sprintf(value_str, "%f", _myValue);
value_str[8] = '\0';

sprintf( temp_str, "%-12.12s%-12.12s%6.6s%-12.12s%-12.12s%8.8s%80.80s",
         "METHOD", _id, "-99999", "SPILLWAY", _owner->_id, value_str, future_str );


***********************************************************
Note: some files have been removed

Do we need to call module cox58???
carryovertransfer58.cxx is called by fcinit_setup/cox58.f

CalcInflow_transferCO.cxx, LagK_transferCO.cxx, Lookup3_transferCO.cxx,
Reservoir_transferCO.cxx, SetWithdraw_transferCO.cxx, and
Spillway_transferCO.cxx contains transferCO function which is called by
carryovertransfer58 function.

9/29/08
Removed resjffcwtco.f, resjfwrite.f, fcwtco.f, ResJ_ccfcwtco.cxx 
and ResJ_ccwrite.cxx because not needed by executing NWSRFS legacy models in
FEWS. Those files related with FCWTCO() module.
