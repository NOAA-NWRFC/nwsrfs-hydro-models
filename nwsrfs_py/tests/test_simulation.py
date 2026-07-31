import pytest
import pandas as pd
#import pdb; pdb.set_trace()

def test_nrkw1_logic(nrkw1_model):

    assert nrkw1_model.localflow_logic is True
    assert nrkw1_model.upflow_logic is True
    assert nrkw1_model.chanloss_logic is False
    assert nrkw1_model.consuse_logic is False

def test_sfln2_logic(sfln2_model):

    assert sfln2_model.localflow_logic is True
    assert sfln2_model.upflow_logic is False
    assert sfln2_model.chanloss_logic is True
    assert sfln2_model.consuse_logic is True

def test_fa(sfln2_model,sfln2_fa_baseline):

    sfln2_model_fa = sfln2_model.fa_factors

    pd.testing.assert_frame_equal(sfln2_model_fa, sfln2_fa_baseline, check_dtype=True, atol=1e-4)

def test_nofa(nrkw1_nofa_model):

    nofa_compare = pd.Series({'map_fac':1.0,'mat_fac':0.0,'pet_fac':1.0,'ptps_fac':1.0}).to_frame()

    nofa_return = nrkw1_nofa_model.fa_factors.abs().max().to_frame()
    pd.testing.assert_frame_equal(nofa_compare, nofa_return, check_dtype=True, atol=1e-5)

def test_nomatpet(sfln2_nomatpet_model,sfln2_fa_baseline):

    nofa_compare = pd.Series({'mat_fac':0.0,'pet_fac':1.0}).to_frame()

    fac_return = sfln2_nomatpet_model.fa_factors

    fac_max = fac_return.max().abs().to_frame()
    nofa_return = fac_max.loc[fac_max.index.isin(['mat_fac','pet_fac'])]

    pd.testing.assert_frame_equal(nofa_compare, nofa_return, check_dtype=True, atol=1e-2)

    fa_compare = sfln2_fa_baseline.loc[:,['map_fac','ptps_fac']]

    fa_return = fac_return.loc[:,['map_fac','ptps_fac']]

    pd.testing.assert_frame_equal(fa_compare, fa_return, check_dtype=True, atol=1e-4)

def test_uh6hr(sfln2_model,sfln2_uh6_baseline):

    uh_compare = sfln2_model.uh.reset_index().drop(['hours'],axis=1)

    pd.testing.assert_frame_equal(uh_compare, sfln2_uh6_baseline, check_dtype=True, atol=1e-5)

def test_uh1hr(sfln2_model,sfln2_uh1_baseline):

    uh_compare = sfln2_model.return_uh(1).reset_index().drop(['hours'],axis=1)

    pd.testing.assert_frame_equal(uh_compare, sfln2_uh1_baseline, check_dtype=True, atol=1e-5)

def test_nrkw1_sim(nrkw1_model,nrkw1_sim_baseline):

    #Check for compiler drift
    pd.testing.assert_frame_equal(
        nrkw1_model.sim.to_frame(), 
        nrkw1_sim_baseline.rename('sqin').to_frame(), 
        check_names=False
    )
 
def test_sfln2_sim(sfln2_model,sfln2_sim_baseline):

    #Check for compiler drift
    pd.testing.assert_frame_equal(
        sfln2_model.sim.to_frame(), 
        sfln2_sim_baseline.rename('sqin').to_frame(), 
        check_names=False
    )

def test_peravg(nrkw1_model,nrkw1_peravg_model):

    peravg_compare = nrkw1_model.sacsnow_sf.sum(axis=1).rolling(window=2).mean().shift(-1).loc['2000'].to_frame()
    peravg_return =  nrkw1_peravg_model.sacsnow_sf.sum(axis=1).loc['2000'].to_frame()

    pd.testing.assert_frame_equal(peravg_compare, peravg_return , check_dtype=True, atol=1e-5)

def test_shift(sfln2_model,sfln2_noshift_model):

    shift_compare = sfln2_model.sacsnow_sf.sum(axis=1).to_frame()
    shift_return =  sfln2_noshift_model.sacsnow_sf.sum(axis=1).shift(1).bfill().to_frame()

    pd.testing.assert_frame_equal(shift_compare, shift_return , check_dtype=True, atol=1e-5)