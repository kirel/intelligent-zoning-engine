import time

import numpy as np
import pandas as pd

from ga_2 import optimization_step, units, entities
from handle_db import get_instruction, set_assignment, get_input_assignment


def write_results(best_solution, entities, units):
    """This function matches between entities and units ids based on
    the asignment matrix and writes it to the data base.

    Args:
        best_solution:

    Returns:

    """
    assigned_entities = []
    for assign in best_solution:
        ent_idx = np.where(assign > 0)[0]

        if len(ent_idx) > 1:
            print('error')

        assigned_entities.append(entities[ent_idx[0]])

    assignment = np.array([units, assigned_entities]).T

    df = pd.DataFrame(columns=['unit_id', 'entity_id'], data=assignment)

    set_assignment(df)

    return


def run():
    while True:
        time.sleep(0.1)
        instruction = get_instruction()

        if instruction == 'start':
            print('start')
            init_population = [get_input_assignment()]
            optimize = True

            num_steps = 1
            new_pop = init_population

            while optimize:
                print('opt')
                new_pop = optimization_step(new_pop, num_steps)
                best_solution = new_pop[0]
                write_results(best_solution, entities, units)

                new_instruction = get_instruction()

                if new_instruction == 'stop':
                    print('stop')
                    optimize = False
                elif new_instruction == 'start':
                    print('restart')
                    num_steps = 1
                    init_population = [get_input_assignment()]
                    new_pop = init_population

                num_steps += 1
