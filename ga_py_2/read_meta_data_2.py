"""
 This modeule contains functions to read the weights file
 and extract the units and entities names and the weights.
"""

import os
import numpy as np
import pandas as pd


def read_data(folder, measurement='max'):
    """ This function reads the weights file and extracts the necessary information

    Args:
        path_to_weights:
        measurement: String  - chosen distance measurements for the weights.
                               'min' or 'max' or 'avg' or 'med'

    Returns: list of unique units (ids), list of unique entities (ids)
             and a weight matrix (of measurements) with a unit in each row and entity in each column

    """
    df_weights = pd.read_csv(os.path.join(folder, 'weights.csv'), dtype={'unit_id':str, 'entity_id': str})
    df_entities = pd.read_csv(os.path.join(folder, 'entities.csv'), dtype={'entity_id': str})
    df_units = pd.read_csv(os.path.join(folder, 'units.csv'), dtype={'unit_id':str})

    # get units and entities ids
    units_id = df_weights['unit_id'].unique()
    entities_id = df_weights['entity_id'].unique()

    # extract weights and transform to matrix
    weights_vec = np.array(df_weights[measurement])
    weights_mat = weights_vec.reshape(len(units_id), len(entities_id))

    # get capacity information
    entities_capacity = df_entities.iloc[:, 1].as_matrix()

    # only use population of units that are in the weights matrix
    df_units = df_units[df_units['unit_id'].isin(units_id)]
    units_population = df_units.iloc[:, 1].as_matrix()
    
    # read adjacencies and construct adjecency matrix
    df_adj = pd.read_csv(os.path.join(folder, 'adjacency.csv'))
    units_id_int = [int(id) for id in units_id]
    df_adj_filtered = df_adj[df_adj['from'].isin(units_id_int) & df_adj['to'].isin(units_id_int)]
    num_units = len(units_id)
    adj_mat = np.zeros((num_units, num_units)).astype(int)
    units_dict = {u_id: i for i, u_id in enumerate(units_id_int)}
    df_adj_mat = df_adj_filtered.replace(units_dict).values.T
    adj_mat[df_adj_mat[0], df_adj_mat[1]] = 1

    return units_id, entities_id, weights_mat, entities_capacity, units_population, adj_mat

