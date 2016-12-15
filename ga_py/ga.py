import numpy as np

from read_meta_data import read_weights

units, entities, weights = read_weights('../data (2)/weights.csv', 'max')


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
    return np.sum(np.multiply(assignment, weights))


# TODO: figure out the mutation probability
def mutation(population, sampled_population_ind, mutation_frac, hueristic_exponent):
    if mutation_frac == 0:
        return population

    num_units, num_entities = population[0].shape
    num_mutations = int(mutation_frac * num_units)

    for ind in sampled_population_ind:
        # FixMe
        mutation_prob = 1. / num_units * np.ones(num_units)

        # chose which rows to mutate based on the mutation probability
        #import ipdb; ipdb.set_trace()
        mutated_units = np.random.choice(np.arange(num_units), num_mutations, False, mutation_prob)
        new_entities = np.random.randint(0, num_entities, len(mutated_units))
        population[ind][mutated_units, :] = 0.
        population[ind][mutated_units, new_entities] = 1.

    #import ipdb; ipdb.set_trace()
    return [population[ind] for ind in sampled_population_ind]


def crossover(ind_a, ind_b):
    diffs = np.logical_not(np.equal(ind_a, ind_b).prod(axis=1))
    num_diffs = np.sum(diffs)

    if num_diffs == 0:
        return ind_a, ind_b

    child_a = np.copy(ind_a)
    child_b = np.copy(ind_b)

    fraction = np.random.rand(1)

    num_genes = int(fraction * num_diffs)
    #import ipdb; ipdb.set_trace()
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
    # calculate probability to chose each individual in the population
    probs = get_prob_from_fitness(population)

    sampled_population_ind = np.random.choice(np.arange(len(population)), num_mutants, True, probs)
    mutated_population = mutation(population[:], sampled_population_ind, mutation_frac, hueristic_exponent)

    mating_population = population + mutated_population
    probs_mating = get_prob_from_fitness(mating_population)
    #import ipdb; ipdb.set_trace()
    pairs = [np.random.choice(np.arange(len(mating_population)), 2, False, probs_mating) for i in range(num_pairs)]

    mated = []
    for pair in pairs:
        mated += crossover(mating_population[pair[0]], mating_population[pair[1]])
    import ipdb; ipdb.set_trace()
    return population + mutated_population + mated


def select(population, survival_fraction=0.5, max_population=50):
    sorted_population = sorted(population, key=fitness)
    num_survivors = min(int(len(population) * survival_fraction), max_population)
    survivors = sorted_population[0:num_survivors]
    return survivors


def optimization_step(population, num_steps):
    hueristic_exponent = 20. / num_steps
    mutation_frac = max(0.001, 1. - num_steps / 3.)

    new_population = breed(population, hueristic_exponent, mutation_frac)

    return new_population

if __name__ =='__main__':
    num_units = len(units)
    num_entities = len(entities)

    population_num = 100

    init_pop = [initialize_individual(num_entities, num_units) for i in range(population_num)]

    res = select(breed(init_pop, 3, 0.1))
    import ipdb; ipdb.set_trace()
    #return res
