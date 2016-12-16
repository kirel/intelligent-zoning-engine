import numpy as np
import pandas as pd

from read_meta_data_2 import read_data

units, entities, weights, capacity, units_population = read_data('../app-data', 'max')

OVER_CAPACITY_PENALTY = 1
UNDER_CAPACITY_PENALTY = 1

DIST_WEIGHT = 1 / 2000 ** 2
OVER_CAPACITY_WEIGHT = 1 / 200
UNDER_CAPACITY_WEIGHT = 1 / 200


def clone_population(population):
    """This function performs deep copy of a population

    Args:
        population:

    Returns: copied population

    """
    new_population = []
    for individual in population:
        new_population.append(np.copy(individual))

    return new_population

def initialize_population(population_size):
    num_entities = len(entities)
    num_units = len(units)
    population = []

    for i in range(population_size):
        population.append(initialize_individual(num_units, num_entities))

    return population


def initialize_individual(num_units, num_entities):
    """This function initialize a random assignment

    Args:
        num_entities:
        num_units:

    Returns: random assignment

    """

    assigns = np.random.randint(0, num_entities, num_units)
    assign_mat = np.zeros((num_units, num_entities))
    assign_mat[np.arange(num_units), assigns] = 1.

    return assign_mat


def fitness(assignment):
    """ Fitness function to be minimized.
        The fitness is quadratic in the distance between the units and entities,
        and in over and under capacity. the distance and capacity may have different weights.

    Args:
        assignment: assignment of each unit to an entity.
                    sparse matrix with one 1 entry in each row

    Returns: the fitness of the individual (represented by A)

    """

    # difference between capacity and assignment results
    assignment_population = np.dot(assignment.T, units_population)
    assign_capacity_diff = capacity - assignment_population

    # handle differently over and under capacity
    over_cap_vec = (assign_capacity_diff * (assign_capacity_diff > 0) * OVER_CAPACITY_PENALTY) ** 2
    under_cap_vec = (assign_capacity_diff * (assign_capacity_diff < 0) * UNDER_CAPACITY_PENALTY) ** 2

    distance_val = np.sum(np.multiply(assignment, weights) ** 2)

    return distance_val * DIST_WEIGHT + np.sum(over_cap_vec) * OVER_CAPACITY_WEIGHT + \
           np.sum(under_cap_vec) * UNDER_CAPACITY_WEIGHT


def mutation(population, sampled_population_ind, mutation_frac, hueristic_exponent):
    """This function mutates individuals in the population.

    Args:
        population:
        sampled_population_ind: indices of individuals in the populations to be mutated.
        mutation_frac: fraction of units in the individual to be nutated
        hueristic_exponent:

    Returns: a population of mutated individuals

    """
    if mutation_frac == 0:
        return population

    num_units, num_entities = population[0].shape
    num_mutations = int(mutation_frac * num_units)

    for ind in sampled_population_ind:
        # calculate mutation probability for each unit.
        # hueristic_exponen allows mutating with high prob units with large distances.
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
    """This function mates between two individuals and creates two children using swap strategie.

    Args:
        ind_a:
        ind_b:

    Returns: 2 children

    """

    diffs = np.logical_not(np.equal(ind_a, ind_b).prod(axis=1))
    num_diffs = np.sum(diffs)

    # don't swap if the assignment of the individuals is identical
    if num_diffs == 0:
        return []

    child_a = np.copy(ind_a)
    child_b = np.copy(ind_b)

    # chose randomly which units to swap
    fraction = np.random.rand(1)
    num_genes = int(fraction * num_diffs)
    swap_ind = np.random.choice(np.arange(ind_a.shape[0])[diffs], num_genes)

    child_a[swap_ind] = ind_b[swap_ind]
    child_b[swap_ind] = ind_a[swap_ind]

    return child_a, child_b


def get_prob_from_fitness(population):
    """This function calculates probabilities to be mutated or mated based on the
    individuals' fitness

    Args:
        population:

    Returns: probabilities

    """
    fitness_vals = np.array([fitness(population[ind]) for ind in range(len(population))])
    fitness_vals = -1. * fitness_vals + min(fitness_vals) + max(fitness_vals)
    probs = fitness_vals / sum(fitness_vals)

    return probs


def breed(population, hueristic_exponent, mutation_frac, num_mutants=100, num_pairs=50):
    """This function breeds the current population to generate the new population.
    The new population consists of the old population, the mutations and the children.

    Args:
        population:
        hueristic_exponent: used to determine the probability of a unit to be swaped.
        mutation_frac: what fraction of units in an individual should be mutated
        num_mutants: number of individuals in the population to be mutated
        num_pairs: number of pairs to be mated

    Returns: new population

    """
    # calculate probability to chose each individual in the population for mutation
    probs = get_prob_from_fitness(population)

    sampled_population_ind = np.random.choice(np.arange(len(population)), num_mutants, True, probs)
    mutated_population = mutation(clone_population(population), sampled_population_ind, mutation_frac,
                                  hueristic_exponent)

    mating_population = clone_population(population) + clone_population(mutated_population)

    # calculate probability to chose each individual in the mating population for mating
    probs_mating = get_prob_from_fitness(mating_population)
    pairs = [np.random.choice(np.arange(len(mating_population)), 2, False, probs_mating) for i in range(num_pairs)]

    mated = []
    for pair in pairs:
        mating_res = crossover(mating_population[pair[0]], mating_population[pair[1]])
        if len(mating_res) > 0:
            mated.append(mating_res[0])
            mated.append(mating_res[1])

    del mating_population

    return population[:] + mutated_population[:] + mated[:]


def select(population, survival_fraction=0.5, max_population=50):
    """This function selects the best candidates from the new population

    Args:
        population:
        survival_fraction:
        max_population:

    Returns: the best individuals from the populaiton

    """
    sorted_population = sorted(population, key=fitness)
    num_survivors = min(int(len(population) * survival_fraction), max_population)
    survivors = sorted_population[:num_survivors]
    return survivors


def optimization_step(population, num_steps):
    """This function performs one optimization step - breeds new population,
    select the best candidates from it and writes the best individual to the data base.

    Args:
        population:
        num_steps: current number of optimization steps.

    Returns:

    """
    hueristic_exponent = 20. / num_steps
    mutation_frac = max(0.1, 1. - num_steps / 3.)

    new_population = breed(population[:], hueristic_exponent, mutation_frac)
    survivors = select(new_population)

    return survivors



