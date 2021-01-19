from utilities.optimization import spot_setup
import spotpy
import pandas as pd
import numpy as np
import sys
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from plotnine import *


# user config
n_var = 31
n_obj = 3

last = None
first = None
n_pop = 20

s = spot_setup('TLMO3','basins/TLMO3-1zone',['rmse','mae','log'])
sampler=spotpy.algorithms.NSGAII(s, dbname='NSGA2', dbformat='csv', save_sim=False)#, parallel='mpc')
sampler.sample(1000,n_obj=n_obj,n_pop=n_pop)

results = sampler.getdata()

fig = plt.figure(1, figsize=(9, 5))
plt.plot(results['like1'])
plt.show()
plt.ylabel('Objective function')
plt.xlabel('Iteration')
fig.savefig('objectivefunction.png', dpi=300)


# Example plot to show the parameter distribution ######
def plot_parameter_trace(ax, results, parameter):
    ax.plot(results['par' + parameter.name], '.')
    ax.set_ylabel(parameter.name)
    ax.set_ylim(parameter.minbound, parameter.maxbound)


def plot_parameter_histogram(ax, results, parameter):
    # chooses the last 20% of the sample
    ax.hist(results['par' + parameter.name][int(len(results) * 0.9):],
            bins=np.linspace(parameter.minbound, parameter.maxbound, 20))
    ax.set_ylabel('Density')
    ax.set_xlim(parameter.minbound, parameter.maxbound)



# Example plot to show remaining parameter uncertainty #
fields = [word for word in results.dtype.names if word.startswith('sim')]
fields = results.dtype.names
fig = plt.figure(3, figsize=(16, 9))
ax11 = plt.subplot(1, 1, 1)
q5, q25, q75, q95 = [], [], [], []
for field in fields:
    q5.append(np.percentile(results[field][-100:-1], 2.5))
    q95.append(np.percentile(results[field][-100:-1], 97.5))
ax11.plot(q5, color='dimgrey', linestyle='solid')
ax11.plot(q95, color='dimgrey', linestyle='solid')
ax11.fill_between(np.arange(0, len(q5), 1), list(q5), list(q95), facecolor='dimgrey', zorder=0,
                  linewidth=0, label='parameter uncertainty')
ax11.plot(sp_setup.evaluation(), 'r.', label='data')
ax11.set_ylim(-50, 450)
ax11.set_xlim(0, 729)
ax11.legend()
fig.savefig('python_hymod.png', dpi=300)
#########################################################


x, y, z = results['like1'][-n_pop:], results['like2'][-n_pop:], results['like3'][-n_pop:]
fig = plt.figure(4)
ax12 = fig.add_subplot(111, projection='3d')
ax12.scatter(x, y, z, marker="o")
ax12.set_xlabel("rmse")
ax12.set_ylabel("mae")
ax12.set_zlabel("log")
plt.show()

(ggplot(results[-n_pop:,])+
 geom_point(aes(x='like1',y='like2')))