#!/usr/bin/env python

import dynclipy
dataset = dynclipy.main()

import pandas as pd
import sklearn.decomposition

# infer trajectory
pca = sklearn.decomposition.PCA()
dimred = pca.fit_transform(dataset['expression'])
pseudotime = pd.Series(
  dimred[:, dataset['parameters']['component']-1],
  index = dataset['expression'].index
)

# build trajectory
trajectory = dynclipy.wrap_data(cell_ids = dataset['expression'].index)
trajectory.add_linear_trajectory(pseudotime = pseudotime)

# save output
trajectory.write_output(dataset['output'])
