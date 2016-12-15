"""
 This modeule contains functions to read the weights file
 and extract the units and entities names and the weights.
"""

import numpy as np
import pandas as pd


def read_weights(path_to_weights, measurement='max'):
    """ This function reads the weights file and extracts the necessary information

    Args:
        path_to_weights:
        measurement: String  - chosen distance measurements for the weights.
                               'min' or 'max' or 'avg' or 'med'

    Returns: list of unique units (ids), list of unique entities (ids)
             and a weight matrix (of measurements) with a unit in each row and entity in each column

    """
    df_weights = pd.read_csv(path_to_weights)

    # get units and entities ids
    units_id = df_weights['unit_id'].unique()
    entities_id = df_weights['entity_id'].unique()

    # extract weights and transform to matrix
    weights_vec = np.array(df_weights[measurement])
    weights_mat = weights_vec.reshape(len(units_id), len(entities_id))

    return units_id, entities_id, weights_mat
