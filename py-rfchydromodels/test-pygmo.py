import pygmo as pg
from utilities.problems import SacSnowUH_pygmo
from utilities.problem_pygmo import sacsnowuh_udp
import pandas as pd
# import numpy as np
from timeit import default_timer as timer
import matplotlib.pyplot as plt
# from plotnine import *

objectives = ['rmse', 'log']
# number if islands to run, each island is an instance of the algorithm
# islands communicate via migration
nislands = 10
# population size for each island (nsga2 needs a multiple of 4)
# total population will be nislands * pop_size
pop_size = 100
# number of generations to run in each iteration
generations = 10
# number of iterations (ngenerations per iteration)
# results will be plotted after each iteration
iterations = 20
# print out every nprint generations within each iteration
nprint = 9

udp = sacsnowuh_udp(basin='TLMO3',
                    basin_dir='basins/TLMO3-1zone',
                    objectives=objectives)
# user defined problem
prob = pg.problem(udp)
# select algorithm
algo = pg.algorithm(pg.nsga2(gen=generations))
algo.set_verbosity(nprint)

# # create population
# pop = pg.population(prob, size=20)
# # run optimization
# pop = algo.evolve(pop)
# # extract results
# fits, vectors = pop.get_f(), pop.get_x()
# # extract and print non-dominated fronts
# ndf, dl, dc, ndr = pg.fast_non_dominated_sorting(fits)
# print(ndf)

if __name__ == '__main__':
    # island for parallel processing
    # isl = pg.island(algo=algo, prob=prob, size=50)# , udi=pg.thread_island(use_pool=True))
    # print(isl.get_population().get_f())
    # isl.evolve(10)
    # isl.wait()
    # print(isl.get_population().get_f())

    # fully connected topology means all nodes communicate (supposedly)
    # archi.set_topology(t=pg.fully_connected())
    archi = pg.archipelago(n=nislands, t=pg.fully_connected(), algo=algo,
                           prob=prob, pop_size=pop_size)
    # broadcast migration, all nodes talk to eachother
    archi.set_migration_type(mt=pg.migration_type.broadcast)

    # keep migrant solutions in the databse so other islands can use them
    archi.set_migrant_handling(mh=pg.migrant_handling.preserve)

    # loop though ngenerations at a time
    start = timer()
    for i in range(iterations):
        # print(archi)
        # evolve the genetic algorithm for ngenerations and nnodes islands
        archi.evolve()
        # wait for subprocesses to finish, and throw any errors that occured
        archi.wait_check()

        for j, island in enumerate(archi):  # iterate through islands
            print(pd.DataFrame(island.get_population().get_f(), columns=objectives))


    end = timer()
    print('Took: ', (end - start) / 60, ' min')

    # # set up df
    # tot_df = pd.DataFrame(columns=["Gen", "F.evals.", "ADF", "Best fit", "island_#"])
    #
    # here's the 'magic'
    objs = []
    pars = []
    for j, island in enumerate(archi):  # iterate through islands
        objs.append(pd.DataFrame(island.get_population().get_f(), columns=objectives))
        pars.append(island.get_population().get_x())
    obj_df = pd.concat(objs)
    obj_df.plot.scatter('rmse', 'log')
    plt.show()
    #     uda = a.extract(pg.nsga2)  # extract algorithm from algorithm object
    #     log = uda.get_log()  # get the log. Comes as list of tuples
    #
    #     # reshape log
    #     df = pd.DataFrame(np.asarray(log), columns=["Gen", "F.evals.", "ADF", "Best fit"])
    #     df["island_#"] = i  # add island ID
    #     tot_df = pd.concat([tot_df, df], axis='index', ignore_index=True)  # merge with total df
    #
    # tot_df.head(10)

    # udp = dtlz(prob_id=1)
    # pop = population(prob=udp, size=105)
    # algo = algorithm(moead(gen=100))
    # for i in range(10):
    #     pop = algo.evolve(pop)
    #     print(udp.p_distance(pop))

    # pop = pg.population(prob,size=20)
    # uda = pg.nsga2(gen=200)
    # b = pg.bfe()
    # uda.set_bfe(b)
    # algo = pg.algorithm(uda)
    # new_pop = algo.evolve(pop)
