#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
from pysal.lib import weights,examples
from pysal.explore import esda
from esda.moran import Moran_BV, Moran_Local_BV
from splot.esda import moran_scatterplot,plot_moran,plot_moran_bv_simulation, plot_moran_bv,lisa_cluster
import geopandas as gpd

# Python does not like the GPKG output from R, so instead we read the
# polygon file here, and merge with the data from R using the common
# 'dist' column

distGeo170 = gpd.read_file("../data-raw/Districts_170/Ghana_Districts_170.shp")
distGeo170['dist'] = np.array(distGeo170['DIST_CODE'].values).astype(int)
x = pd.read_csv('../data/data_fromR.csv')
distGeo170 = pd.merge(distGeo170, x, left_on='dist', right_on='dist', how='outer')

w = weights.Queen.from_dataframe(distGeo170)

for tempYear in [2010, 2013, 2017]:
    tempVar1 = 'sachet_usage_'+str(tempYear)
    tempVar2 = 'HH_DENSITY_'+str(tempYear)
    moran_loc_bv = Moran_Local_BV(distGeo170[tempVar1],distGeo170[tempVar2], w)
    distGeo170['p_sim_' + str(tempYear)] = moran_loc_bv.p_sim
    distGeo170['q_' + str(tempYear)] = moran_loc_bv.q

dist_geo_170_lisa = distGeo170.copy()
dist_geo_170_lisa.to_file('../data/dist_geo_170_updated.gpkg', driver='GPKG', index=True)

## Spatial autocorrelation of sachet drinking consuming
## regions in 2010, 2013 and 2017

## Calculate spatial autocorrelation for the consumption rate of sachet water
w = weights.Queen.from_dataframe(distGeo170)
w.transform = 'r'
moran_I = []
moran_p = []
moran_z = []
for tempVar in ['sachet_usage_2010','sachet_usage_2013','sachet_usage_2017']:
    y = distGeo170[tempVar].values
    moran = esda.Moran(y,w)
    moran_I.append(moran.I)
    moran_p.append(moran.p_sim)
    moran_z.append(moran.z_norm)

moran_df = pd.DataFrame(
    {'var'   : ['sachet_usage_2010','sachet_usage_2013','sachet_usage_2017'],
     'I'     : moran_I,
     'p_sim' : moran_p,
     'z_norm': moran_z
    }
) 

# Write output to file
moran_df.to_csv("../data/moran_I_data.csv", index=False)

## Bivariate spatial autocorrelation between sachet consumption and
## household density in 2010, 2013 and 2017

## Calculate bivariate spatial autocorrelation between consumption rate
## of sachet water and household density
w = weights.Queen.from_dataframe(distGeo170)
moranBV_I = []
moranBV_p = []
moranBV_z = []
for tempYear in [2010, 2013, 2017]:
    tempVar1 = 'sachet_usage_'+str(tempYear)
    tempVar2 = 'HH_DENSITY_'+str(tempYear)
    moranBV = esda.Moran_BV(distGeo170[tempVar2],distGeo170[tempVar1], w)
    moranBV_I.append(moranBV.I)
    moranBV_p.append(moranBV.p_sim)
    moranBV_z.append(moranBV.z_sim)    

moranBV_df = pd.DataFrame(
    {'var'   : ['sachet_usage_2010','sachet_usage_2013','sachet_usage_2017'],
     'I'     : moranBV_I,
     'p_sim' : moranBV_p,
     'z_norm': moranBV_z
    }
) 

# Write output to file
moranBV_df.to_csv("../data/moranBV_I_data.csv", index=False)
    
