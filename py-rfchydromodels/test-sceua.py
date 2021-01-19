from utilities.optimization import spot_setup
import spotpy

s = spot_setup('TLMO3','basins/TLMO3-1zone',['rmse','mae','log'])
sampler=spotpy.algorithms.sceua(s, dbname='SCEUA', dbformat='csv',save_sim=False, parallel='mpi')
sampler.sample(5000, ngs=s.parameters().__len__()+1, kstop=3, peps=0.1, pcento=0.1)