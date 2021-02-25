from platypus import *
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from utilities.problems import SacSnowUH_platypus
from timeit import default_timer as timer

if __name__ == '__main__':
    # setup the experiment
    # problem = DTLZ2(3)

    problem = SacSnowUH_platypus(basin='TLMO3',
                        basin_dir='basins/TLMO3-1zone',
                        objectives=['rmse', 'log', 'mae'])

    algorithms = [NSGAII,
                  (NSGAIII, {"divisions_outer": 12}),
                  #(CMAES, {"epsilons": [0.05]}),
                  #GDE3,
                  #IBEA,
                  #(MOEAD, {"weight_generator": normal_boundary_weights, "divisions_outer": 12}),
                  #(OMOPSO, {"epsilons": [0.05]}),
                  #SMPSO,
                  SPEA2,
                  #(EpsMOEA, {"epsilons": [0.05]})
                  ]

    start = timer()
    # run the experiment using Python 3's concurrent futures for parallel evaluation
    with ProcessPoolEvaluator() as evaluator:
        results = experiment(algorithms, problem, seeds=1, nfe=20000, evaluator=evaluator)
    end = timer()
    print('Took: ', (end - start) / 60, ' min')

    # display the results
    fig = plt.figure()

    for i, algorithm in enumerate(six.iterkeys(results)):
        result = results[algorithm]["SacSnowUH"][0]

        ax = fig.add_subplot(2, 2, i + 1, projection='3d')
        ax.scatter([s.objectives[0] for s in result],
                   [s.objectives[1] for s in result],
                   [s.objectives[2] for s in result])
        ax.set_title(algorithm)
        #ax.set_xlim([0, 1.1])
        #ax.set_ylim([0, 1.1])
        #ax.set_zlim([0, 1.1])
        ax.view_init(elev=30.0, azim=15.0)
        ax.locator_params(nbins=4)

    plt.show()