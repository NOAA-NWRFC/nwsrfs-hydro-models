from utilities.forcing_adjust import *
import pandas as pd
#import sacsnowuh.poop as poop

n_zones = 1
base_year = 2099
forcing = []
climo = []
for i in range(n_zones):
    forcing.append(pd.read_csv(f'basins/TLMO3-1zone/forcing_TLMO3-{i+1}.csv'))
    climo.append(forcing[i].groupby('month').mean().reset_index())  # [['map_mm', 'mat_degc', 'ptps']])
    climo[i]['year'] = [base_year]*9 + [base_year - 1]*3
    climo[i]['day'] = 15
    climo[i].index = pd.to_datetime(climo[i][['year', 'month', 'day']])
    climo[i] = climo[i].sort_index()

# var_names = {'map':'map_mm', 'mat':'mat_degc', 'ptps':'ptps'}
# vars = {}
# for k,v in var_names.items():
#     var = pd.concat([c[v] for c in climo],axis=1)
#     var.index = pd.to_datetime()
#     v.columns = [c + '_' + str(i + 1) for c, i in zip(map.columns.values, range(n_zones))]
# var_dt =

map = pd.concat([c['map_mm'] for c in climo], axis=1)
mat = pd.concat([c['mat_degc'] for c in climo], axis=1)
ptps = pd.concat([c['ptps'] for c in climo], axis=1)
map.columns = [c + '_' + str(i+1) for c, i in zip(map.columns.values, range(n_zones))]
mat.columns = [c + '_' + str(i+1) for c, i in zip(mat.columns.values, range(n_zones))]
ptps.columns = [c + '_' + str(i+1) for c, i in zip(ptps.columns.values, range(n_zones))]

# single zone
map_single = map[['map_mm_1']]
adj_climo_map_pet(map_single, px_adj=0.9, p_redist=0.1, std=5, shift=0,
                  upper_limit=map_single+0.1 * map_single,
                  lower_limit=map_single-0.1 * map_single)


mat_single = mat[['mat_degc_1']]
adj_climo_mat_ptps(mat_single, px_adj=0.9, p_redist=0.9, std=5, shift=0,
                  upper_limit=mat_single+0.1 * mat_single,
                  lower_limit=mat_single-0.1 * mat_single)

# multi-zone
# adj_climo_map_pet(map, px_adj=0.9, p_redist=1.1, std=0.1, shift=0,
#                   upper_limit=map+0.1*map, lower_limit=map-0.1*map)
