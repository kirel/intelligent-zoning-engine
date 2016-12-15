import numpy as np
import pandas as pd

from read_meta_data_2 import read_data

units, entities, weights, capacity, units_population = read_data('../app-data', 'max')


def clone_population(population):
    new_population = []
    for individual in population:
        new_population.append(np.copy(individual))

    return new_population


def initialize_individual(num_entities, num_units):
    """

    Args:
        num_entities:
        num_units:

    Returns:

    """
    assigns = np.random.randint(0, num_entities, num_units)

    assign_mat = np.zeros((num_units, num_entities))
    assign_mat[np.arange(num_units), assigns] = 1.

    return assign_mat


def fitness(assignment):
    """ Fitness function to be minimized

    Args:
        assignment: assignment of each unit to an entity.
                    sparse matrix with one 1 entry in each row
        weights:

    Returns: the fitness of the individual (represented by A)

    """

    OVER_CAPACITY_PENALTY = 1
    UNDER_CAPACITY_PENALTY = 1

    DIST_WEIGHT = 1 / 2000 ** 2
    OVER_CAPACITY_WEIGHT = 1 / 200
    UNDER_CAPACITY_WEIGHT = 1 / 200

    assignment_population = np.dot(assignment.T, units_population)
    assign_capacity_diff = capacity - assignment_population

    over_cap_vec = (assign_capacity_diff * (assign_capacity_diff > 0) * OVER_CAPACITY_PENALTY) ** 2
    under_cap_vec = (assign_capacity_diff * (assign_capacity_diff < 0) * UNDER_CAPACITY_PENALTY) ** 2

    distance_val = np.sum(np.multiply(assignment, weights) ** 2)

    return distance_val * DIST_WEIGHT + np.sum(over_cap_vec) * OVER_CAPACITY_WEIGHT + \
           np.sum(under_cap_vec) * UNDER_CAPACITY_WEIGHT


def mutation(population, sampled_population_ind, mutation_frac, hueristic_exponent):
    if mutation_frac == 0:
        return population
    num_units, num_entities = population[0].shape
    num_mutations = int(mutation_frac * num_units)

    for ind in sampled_population_ind:
        fitness_vals_normed = np.multiply(population[ind], weights).sum(axis=1) / np.max(weights)
        exploration_fitness_vals = fitness_vals_normed ** hueristic_exponent
        mutation_prob = exploration_fitness_vals / np.sum(exploration_fitness_vals)

        # chose which rows to mutate based on the mutation probability

        mutated_units = np.random.choice(np.arange(num_units), num_mutations, False, mutation_prob)
        new_entities = np.random.randint(0, num_entities, len(mutated_units))
        population[ind][mutated_units, :] = 0.
        population[ind][mutated_units, new_entities] = 1.

    return [population[ind] for ind in sampled_population_ind]


def crossover(ind_a, ind_b):
    diffs = np.logical_not(np.equal(ind_a, ind_b).prod(axis=1))
    num_diffs = np.sum(diffs)

    if num_diffs == 0:
        # print('a')
        return []

    child_a = np.copy(ind_a)
    child_b = np.copy(ind_b)

    fraction = np.random.rand(1)

    num_genes = int(fraction * num_diffs)
    # import ipdb; ipdb.set_trace()
    swap_ind = np.random.choice(np.arange(ind_a.shape[0])[diffs], num_genes)
    child_a[swap_ind] = ind_b[swap_ind]
    child_b[swap_ind] = ind_a[swap_ind]

    return child_a, child_b


def get_prob_from_fitness(population):
    fitness_vals = np.array([fitness(population[ind]) for ind in range(len(population))])
    fitness_vals = -1. * fitness_vals + min(fitness_vals) + max(fitness_vals)
    probs = fitness_vals / sum(fitness_vals)

    return probs


def breed(population, hueristic_exponent, mutation_frac, num_mutants=100, num_pairs=50):
    # import ipdb; ipdb.set_trace()
    # calculate probability to chose each individual in the population
    probs = get_prob_from_fitness(population)

    sampled_population_ind = np.random.choice(np.arange(len(population)), num_mutants, True, probs)
    mutated_population = mutation(clone_population(population), sampled_population_ind, mutation_frac,
                                  hueristic_exponent)

    mating_population = clone_population(population) + clone_population(mutated_population)
    probs_mating = get_prob_from_fitness(mating_population)
    # import ipdb; ipdb.set_trace()
    pairs = [np.random.choice(np.arange(len(mating_population)), 2, False, probs_mating) for i in range(num_pairs)]

    mated = []
    for pair in pairs:
        mating_res = crossover(mating_population[pair[0]], mating_population[pair[1]])
        if len(mating_res) > 0:
            mated.append(mating_res[0])
            mated.append(mating_res[1])

    del mating_population

    return population[:] + mutated_population[:] + mated[:]


def select(population, survival_fraction=1., max_population=50):
    # print(len(population))
    sorted_population = sorted(population, key=fitness)
    num_survivors = min(int(len(population) * survival_fraction), max_population)
    survivors = sorted_population[:num_survivors]
    return survivors


def optimization_step(population, num_steps):
    hueristic_exponent = 20. / num_steps
    mutation_frac = max(0.1, 1. - num_steps / 3.)

    new_population = breed(population[:], hueristic_exponent, mutation_frac)
    survivors = select(new_population)

    # best_solution = new_population[0]
    # write_results(best_solution)

    return survivors


def write_results(best_solution):
    assigned_entities = []
    for assign in best_solution:
        ent_idx = np.where(assign > 0)[0]

        if len(ent_idx) > 1:
            print('error')

        assigned_entities.append(entities[ent_idx[0]])

    assignment = np.array([units, assigned_entities]).T

    df = pd.DataFrame(columns=['units', 'entities'], data=assignment)

    return


if __name__ == '__main__':
    num_units = len(units)
    num_entities = len(entities)

    population_num = 100

    init_pop = [initialize_individual(num_entities, num_units) for i in range(population_num)]
    pop = init_pop[:]
    fitness_res = []
    num_steps = 1
    for i in range(50):
        new_pop = optimization_step(pop[:], num_steps)

        best_solution = new_pop[0]
        fitness_res.append(fitness(best_solution))
        pop = new_pop[:]
        num_steps += 1

    # import ipdb;

    ipdb.set_trace()
    # return res
