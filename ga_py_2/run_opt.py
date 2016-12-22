import time

import numpy as np
import pandas as pd

from ga_2 import optimization_step, initialize_population, entities, units
from handle_db import get_instruction, set_assignment, get_current_assignment


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


if '__name__' == '__main__':

    population_size = 100

    while True:
        print('yo')
        time.sleep(0.1)
        optimize = get_instruction("'optimize'")[0][1]

        if optimize:
            # print('begin')
            init_population = initialize_population(population_size)
            include_current_assign = get_instruction("'include_current'")[0][1]

            if include_current_assign:
                # print('getting current')
                init_population.append(get_current_assignment())

            num_steps = 1
            new_pop = init_population
            while optimize:
                new_pop = optimization_step(new_pop, num_steps)
                best_solution = new_pop[0]
                write_results(best_solution, entities, units)

                optimize = get_instruction("'optimize'")[0][1]

                if not optimize:
                    print('stop')
