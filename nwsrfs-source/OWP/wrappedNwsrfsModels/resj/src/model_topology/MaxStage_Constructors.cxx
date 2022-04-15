//------------------------------------------------------------------------------
// MaxStage :: MaxStage - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Created initial version.
// 04 Jun 2001	James R. VanShaar, RTi	Added copying of _sumLag
// 14 Aug 2007  Darrin J. Sharp, RTi	Added _max_discharge param for Reservoir
//             				Tools Enhancement
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj_main.h"
#include "MaxStage.h"

MaxStage :: MaxStage( Reservoir* owner ) : ReservoirMethod( owner )
{
	if( owner == NULL ) {
		/*AVCP PrintWarning( 1,*/
	     if( getFewsDebugFlag() > 0 )
	     {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	        "%s (MaxStage::MaxStage) - Cannot set owner to NULL.",
	        warnStr );
	     }
	}
	initialize();

	// Set the _owner
	_owner = owner;
}

MaxStage :: MaxStage( const MaxStage& meth, Reservoir* owner ) : 
	ReservoirMethod( meth, owner )
{
	char	routine[]="MaxStage::MaxStage";

	initialize();

	// Integers
	_inflow_pos = meth._inflow_pos;
	_max_iter = meth._max_iter;
	_n_tstep = meth._n_tstep;

	// Doubles...
	_max_stage = meth._max_stage;
	_max_discharge = meth._max_discharge;
	_maxflow = meth._maxflow;
	_highestFlow = meth._highestFlow;
	_min_release = meth._min_release;
	_criterion = meth._criterion;
	_current_rel = meth._current_rel;
	_sumLag = meth._sumLag;
	_sumK = meth._sumK;

	// Components
	//_dcp or downstream control point should be point to its appropriate component
	//but that component might not available.  So do this to keep its ID
	_dcp = new Node( *(meth._dcp) );

	if(meth._next_ds_comp==NULL)
		_next_ds_comp=NULL;
	else if( !strcasecmp( meth._next_ds_comp->getType(), "RESERVOIR" ) ) {
		_next_ds_comp = new Reservoir( *(Reservoir*)(meth._next_ds_comp) ); 
	}
	else if( !strcasecmp( meth._next_ds_comp->getType(), "REACH" ) ) {
		_next_ds_comp = new Reach( *(Reach*)(meth._next_ds_comp) ); 
	}
	else {
		_next_ds_comp = new Node( *(Node*)(meth._next_ds_comp) );
	}

	// Table
	_stage_flow_tbl = meth._stage_flow_tbl;


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/ofs/src/resj_topology/RCS/MaxStage_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: MaxStage_Constructors.cxx,v 1.6 2007/10/09 13:39:14 hsu Exp $";}
/*  ===================================================  */

}
