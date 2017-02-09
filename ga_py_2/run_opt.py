import time

import numpy as np
import pandas as pd

from handle_db import get_instruction, set_assignment, get_input_assignment
from optimizer import *


def write_results(opt):
    """This function matches between entities and units ids based on
    the asignment matrix and writes it to the data base.

    Args:
        best_solution:

    Returns:

    """
    best_solution, score = opt.get_current_solution()

    assigned_entities = []
    for assign in best_solution:
        ent_idx = np.where(assign > 0)[0]

        if len(ent_idx) > 1:
            print('error')

        assigned_entities.append(opt.entities[ent_idx[0]])

    assignment = np.array([opt.units, assigned_entities]).T

    df = pd.DataFrame(columns=['unit_id', 'entity_id'], data=assignment)

    set_assignment(df, score)

    return


def set_init_assignment(assignment_df, opt):
    assignment_df = assignment_df[assignment_df['unit_id'].isin(opt.units)]
    locked = np.where(assignment_df['locked'].as_matrix() > 0)[0]

    num_entities = len(opt.entities)
    num_units = len(opt.units)

    assign_mat = np.zeros((num_units, num_entities))

    for i, entity in enumerate(assignment_df['entity_id']):
        if entity in opt.entities:
            assign_mat[i, list(opt.entities).index(entity)] = 1
        else:
            assign_mat[i, np.random.randint(0, num_entities)] = 1

    opt.initialize_population(assign_mat)
    opt.set_locked(locked)


def run():
    while True:
        time.sleep(0.1)
        instruction = get_instruction()

        if instruction == 'start':
            print('start')
            opt = optimizer()
            set_init_assignment(get_input_assignment(), opt)

            optimize = True

            num_steps = 1

            while optimize:
                print('opt')
                opt.optimization_step(num_steps)
                write_results(opt)

                new_instruction = get_instruction()

                if new_instruction == 'stop':
                    print('stop')
                    optimize = False
                elif new_instruction == 'start':
                    print('restart')
                    num_steps = 1
                    set_init_assignment(get_input_assignment(), opt)

                num_steps += 1
