import pygmo as pg
isl = pg.island(algo = pg.de(10), prob = pg.ackley(5), size=20, udi=pg.thread_island())
isl.evolve()

#islands = [pg.island(algo = pg.de(gen = 1000, F=effe, CR=cross), prob = pg.rosenbrock(10), size=20, seed=32) for effe in [0.3,0.5,0.7,0.9] for cross in [0.3,0.5,0.7,0.9]]
#_ = [isl.evolve() for isl in islands]
#_ = [isl.wait() for isl in islands]

res = []
for i in range(100):
     islands = [pg.island(algo = pg.de(gen = 1000, F=effe, CR=cross), prob = pg.rosenbrock(10), size=20, seed=32) for effe in [0.3,0.5,0.7,0.9] for cross in [0.3,0.5,0.7,0.9]]
     _ = [isl.evolve() for isl in islands]
     _ = [isl.wait() for isl in islands]
     res.append([isl.get_population().champion_f[0] for isl in islands])
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
sns.boxplot(pd.DataFrame(res))
ylim = plt.ylim([0,20])
plt.title("16 instances of DE on the Rosenbrock 10 problem")
plt.ylabel("obj. fun.")