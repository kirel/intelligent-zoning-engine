import time
import pickle

from optimizer import *

def run_opt(new_penalties):
    num_steps = 1

    opt = optimizer()
    opt.set_penalties(new_penalties)
    opt.initialize_population()
    iterations_num = 5

    fitness_res = []
    for i in range(iterations_num):
        opt.optimization_step(num_steps)
        print(i)
        best_solution, score = opt.get_current_solution()
        fitness_res.append(score)
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