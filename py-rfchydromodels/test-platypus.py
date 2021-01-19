from utilities.problems import SacSnowUH_platypus
from platypus import *
from timeit import default_timer as timer
import matplotlib.pyplot as plt
import pandas as pd
import datetime as dt
import itertools

# total population size, but NSGA-III wont give this many solutions
# see comment above divisions parameter to determine how many solutions
# will pe provided
pop_size = 1000
# number of cores to try and utilize, may not fully utilize all cores if
# the objective function is fast to evaluate
processes = 16
# number of generations per iteration
generations = 1000
# total number of generations is generations*rounds
rounds = 50
# determines the number of reference points (solutions on the pareto front)
#    num_ref_points = choose(n_obj+divisions-1, divisions)
divisions = 12
objectives = ['rmse']#, 'log', 'mae']

n_obj = len(objectives)

# need this construct for the parallelization to work properly
if __name__ == '__main__':
    problem = SacSnowUH_platypus(basin='TLMO3',
                                 basin_dir='basins/TLMO3-1zone',
                                 objectives=objectives)

    # SERIAL
    # algorithm_serial = NSGAIII(problem, population_size=50, divisions_outer=12)
    # start = timer()
    # algorithm_serial.run(10000)
    # end = timer()
    # print((end - start)/60)
    # # algorithm_serial.result.__len__()
    #
    # nondominated_solutions = nondominated(algorithm_serial.result)
    #
    # for solution in algorithm_serial.result:
    #     print(solution.objectives)

    with ProcessPoolEvaluator(processes) as evaluator:
        algorithm_parallel = NSGAIII(problem, evaluator=evaluator,
                                    population_size=pop_size,
                                    divisions_outer=divisions, divisions_inner=0)

        start_overall = timer()
        combos = list(itertools.combinations(objectives, 2))
        fig = plt.figure(figsize=[max(len(combos),1)*3,3])
        hv = []
        # PARALLEL
        for i in range(rounds):
            start = timer()
            algorithm_parallel.run(generations)
            end = timer()
            print('Iteration', i + 1, 'of', rounds, 'took',
                  dt.timedelta(seconds=round(end - start)))
            # algorithm_parallel.result.__len__()

            results = algorithm_parallel.result
            results_nd = nondominated(results)
            print('Nondominated solutions', results_nd.__len__(), 'out of', pop_size)

            hv.append(Hypervolume(minimum=[0 for _ in range(n_obj)],
                                  maximum=[2000 for _ in range(n_obj)]).calculate(results))
            print('Hypervolume', hv[i])

            obj_dict_nd = {}
            for j in range(n_obj):
                obj_dict_nd[objectives[j]] = [s.objectives[j] for s in results_nd]
            obj_nd = pd.DataFrame(obj_dict_nd)

            # display the results
            if n_obj > 1:
                for combo,i in zip(combos,range(len(combos))):
                    ax = fig.add_subplot(1,len(combos),i+1)
                    ax.scatter(obj_nd[combo[0]], obj_nd[combo[1]])
                    ax.set_xlabel(combo[0])
                    ax.set_ylabel(combo[1])
                    ax.set_xlim([obj_nd[combo[0]].min(), obj_nd[combo[0]].max()])
                    ax.set_ylim([obj_nd[combo[1]].min(), obj_nd[combo[1]].max()])

                plt.tight_layout()
                plt.show()
                plt.pause(0.0001)

    end_overall = timer()
    print('Overall took', dt.timedelta(seconds=round(end_overall - start_overall)))

    if n_obj == 3:
        fig = plt.figure()
        ax = fig.add_subplot(projection='3d')
        ax.scatter([s.objectives[0] for s in results],
                   [s.objectives[1] for s in results],
                   [s.objectives[2] for s in results])
        # ax.set_title(algorithm)
        ax.set_xlabel(objectives[0])
        ax.set_ylabel(objectives[1])
        ax.set_zlabel(objectives[2])
        # ax.set_ylim([0, 1.1])
        # ax.set_zlim([0, 1.1])
        # ax.view_init(elev=30.0, azim=15.0)
        # ax.locator_params(nbins=4)
        plt.show()