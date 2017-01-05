import time

import numpy as np
import pandas as pd

from ga_2 import optimization_step, fitness, units, entities
from handle_db import get_instruction, set_assignment, get_input_assignment


def write_results(best_solution, entities, units):
    """This function matches between entities and units ids based on
    the asignment matrix and writes it to the data base.

    Args:
        best_solution:

    Returns:

    """
    score = np.sum(fitness(best_solution))
    assigned_entities = []
    for assign in best_solution:
        ent_idx = np.where(assign > 0)[0]

        if len(ent_idx) > 1:
            print('error')

        assigned_entities.append(entities[ent_idx[0]])

    assignment = np.array([units, assigned_entities]).T

    df = pd.DataFrame(columns=['unit_id', 'entity_id'], data=assignment)

    set_assignment(df, score)

    return


def get_assign_mat_from_df(assignment_df):

    assignment_df = assignment_df[assignment_df['unit_id'].isin(units)]
    locked = np.where(assignment_df['locked'].as_matrix() > 0)[0]

    num_entities = len(entities)
    num_units = len(units)

    assign_mat = np.zeros((num_units, num_entities))

    for i, entity in enumerate(assignment_df['entity_id']):
        if entity in entities:
            assign_mat[i, entities.index(entity)] = 1
        else:
            assign_mat[i, np.random.randint(0, num_entities)] = 1

    return assign_mat, locked


def run():
    while True:
        time.sleep(0.1)
        instruction = get_instruction()

        if instruction == 'start':
            print('start')
            init_population, locked = get_assign_mat_from_df(get_input_assignment())
            init_population = [init_population]
            optimize = True

            num_steps = 1
            new_pop = init_population

            while optimize:
                print('opt')
                new_pop = optimization_step(new_pop, num_steps, locked)
                best_solution = new_pop[0]
                write_results(best_solution, entities, units)

                new_instruction = get_instruction()

                if new_instruction == 'stop':
                    print('stop')
                    optimize = False
                elif new_instruction == 'start':
                    print('restart')
                    num_steps = 1
                    init_population, locked = get_assign_mat_from_df(get_input_assignment())
                    init_population = [init_population]
                    new_pop = init_population

                num_steps += 1
