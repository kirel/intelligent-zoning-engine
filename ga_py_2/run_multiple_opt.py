import time
import pickle

import numpy as np

from ga_2 import *
from handle_db import *



def run_opt(new_penalties):
    update_penalties(new_penalties)
    num_units = len(units)
    num_entities = len(entities)
    max_population = 60
    num_steps = 1

    init_pop = [initialize_individual(num_units, num_entities) for i in range(50)]
    pop = init_pop[:]

    iterations_num = 200

    fitness_res = []
    for i in range(iterations_num):
        new_pop = optimization_step_multiprocess(pop[:], num_steps, 1)

        best_solution = new_pop[0]
        score = np.sum(fitness(best_solution))
        fitness_res.append(score)
        pop = new_pop[:]
        num_steps += 1

    return fitness_res, best_solution



if __name__ == '__main__':

    dist_weights = [1 / 1000 ** 2, 1 / 1000, 0, 1, 1 / 1000]
    over_cap_weights = [1 / 200, 1 / 20, 0, 1 , 1 / 10, 1 / 500]
    under_cap_weights = [1 / 200, 1 / 20, 0, 1 , 1 / 10, 1 / 500]
    adj_weight = [1, 1 / 10, 10, 1/ 100, 100, 1000, 1 / 1000]

    for dw in dist_weights:
        for ocw in over_cap_weights:
            for ucw in under_cap_weights:
                for aw in adj_weight:
                    new_penalties = [dw, ocw, ucw, aw]
                    fitness, sol = run_opt(new_penalties)

                    to_save = {'penalties': new_penalties, 'fitness': fitness, 'best_solution': sol}

                    file_name = time.strftime("%Y%m%d-%H%M%S")

                    with open(file_name, 'wb') as f:
                        pickle.dump(to_save, f)

