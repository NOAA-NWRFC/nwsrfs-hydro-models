import pytest, os, glob
import pandas as pd
from nwsrfs_py import simulation
from nwsrfs_py import adjustq
from nwsrfs_py  import utils

@pytest.fixture
def nrkw1_model(scope="session"):
    """Provides a pre-loaded NRKW1 model instance to any test."""
    return simulation.NwsrfsRun.load_example('NRKW1')

@pytest.fixture
def nrkw1_nofa_model(scope="session"):
    """Provides a pre-loaded NRKW1 model instance with no FA to test."""
    return simulation.NwsrfsRun.load_example('NRKW1', forcing_adj=False)

@pytest.fixture
def nrkw1_peravg_model(scope="session"):
    """Provides a pre-loaded NRKW1 model instance with return_inst set to False."""
    return simulation.NwsrfsRun.load_example('NRKW1', return_inst=False)

@pytest.fixture
def nrkw1_sim_baseline(scope="session"):
    """Provides a NWSRFS R wrapper return for NRKW1 simulation to test as baseline."""
    with utils._get_example_dir('NRKW1') as example_dir:
        ts = pd.read_csv(os.path.join(example_dir,'results_por_02','optimal_6hr_inst.csv'))
    ts.index = pd.to_datetime(ts[['year','month','day','hour']])
    ts.drop(['year','month','day','hour'],axis=1,inplace=True)

    return ts.sim_flow_cfs.rename('r_sim_cfs')

@pytest.fixture
def sfln2_model(scope="session"):
    """Provides a pre-loaded SFLN2 model instance."""
    return simulation.NwsrfsRun.load_example('SFLN2')

@pytest.fixture
def sfln2_nomatpet_model(scope="session"):
    """Provides a pre-loaded SFLN2 model instance with no map and mat FA  test."""
    return simulation.NwsrfsRun.load_example('SFLN2', forcing_adj=['map','ptps'])

@pytest.fixture
def sfln2_noshift_model(scope="session"):
    """Provides a pre-loaded SFLN2 model instance with shift_sf set to False."""
    return simulation.NwsrfsRun.load_example('SFLN2', shift_sf=False)

@pytest.fixture
def sfln2_sim_baseline(scope="session"):
    """Provides a NWSRFS R wrapper return for SFLN2 simulation to test as baseline."""
    with utils._get_example_dir('SFLN2') as example_dir:
        ts = pd.read_csv(os.path.join(example_dir,'results_por_01','optimal_6hr_inst.csv'))
    ts.index = pd.to_datetime(ts[['year','month','day','hour']])
    ts.drop(['year','month','day','hour'],axis=1,inplace=True)

    return ts.sim_flow_cfs.rename('r_sim_cfs')

@pytest.fixture
def sfln2_uh6_baseline(scope="session"):
    """Provides a NWSRFS R wrapper return for SFLN2 UH 6hr to test as baseline."""
    with utils._get_example_dir('SFLN2') as example_dir:
        uh_files = glob.glob(os.path.join(example_dir,'results_por_01','uh_6hr*.csv'))
        uh_files.sort()
        uh_list = []
        for file in uh_files:
            uh_list.append(pd.read_csv(file))
    col_names = [os.path.basename(file).split('.')[0].split('_')[-1] for file in uh_files]
    uh = pd.concat(uh_list,axis=1)       
    uh.columns = col_names

    return uh

@pytest.fixture
def sfln2_uh1_baseline(scope="session"):
    """Provides a NWSRFS R wrapper return for SFLN2 UH 1hr to test as baseline."""
    with utils._get_example_dir('SFLN2') as example_dir:
        uh_files = glob.glob(os.path.join(example_dir,'results_por_01','uh_1hr*.csv'))
        uh_files.sort()
        uh_list = []
        for file in uh_files:
            uh_list.append(pd.read_csv(file))
    col_names = [os.path.basename(file).split('.')[0].split('_')[-1] for file in uh_files]
    uh = pd.concat(uh_list,axis=1)       
    uh.columns = col_names

    return uh

@pytest.fixture
def sfln2_fa_baseline(scope="session"):
    """Provides a NWSRFS R wrapper return for SFLN2 FA adjustments to test as baseline."""
    with utils._get_example_dir('SFLN2') as example_dir:
        fac = pd.read_csv(os.path.join(example_dir, 'results_por_01','fa_factors.csv'),index_col='month')
    return fac

@pytest.fixture
def adjustq_w_sim(scope="session"):
    """Provides a pre-loaded adjustq with simulation file."""
    return adjustq.AdjustQ.load_example(sim=True)

@pytest.fixture
def adjustq_w_sim_baseline(scope="session"):
    """Provides a pre-loaded adjustq with simulation file."""
    with utils._get_example_dir('Adjustq_check') as example_dir:
        adjq_df = pd.read_csv(os.path.join(example_dir,'NRKW1_w_sim.csv'),index_col='datetime_local_tz',parse_dates=True)
    return adjq_df

@pytest.fixture
def adjustq_wout_sim(scope="session"):
    """Provides a pre-loaded adjustq without simulation file."""
    return adjustq.AdjustQ.load_example(sim=False)

@pytest.fixture
def adjustq_wout_sim_baseline(scope="session"):
    """Provides a pre-loaded adjustq with simulation file."""
    with utils._get_example_dir('Adjustq_check') as example_dir:
        adjq_df= pd.read_csv(os.path.join(example_dir,'NRKW1_wout_sim.csv'),index_col='datetime_local_tz',parse_dates=True)
    return adjq_df
