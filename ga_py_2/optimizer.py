import multiprocessing
import numpy as np

import igraph

from read_meta_data_2 import read_data


class optimizer:
    def __init__(self):
        self.units, self.entities, self.weights, self.capacity, self.units_population, self.adj_mat = read_data('../app/data', 'max')

        self.DIST_WEIGHT = 1 / 1000 ** 2
        self.OVER_CAPACITY_WEIGHT = 1 / 200
        self.UNDER_CAPACITY_WEIGHT = 1 / 200
        self.ADJ_WEIGHT = 1 / (10000 ** 2)

        self.UNIT_NBR_NUM = np.sum(self.adj_mat, axis=0) + 1.

        self.num_units = len(self.units)
        self.num_entities = len(self.entities)

        self.num_pairs = 20
        self.num_mutants = 50

        self.population_size = 50
        self.survival_fraction = 0.5

        #self.initialize_population()

    def set_penalties(self, new_penalties):
        self.DIST_WEIGHT = new_penalties[0]
        self.OVER_CAPACITY_WEIGHT = new_penalties[0]
        self.UNDER_CAPACITY_WEIGHT = new_penalties[0]
        self.ADJ_WEIGHT = new_penalties[0]

    def get_current_solution(self):
        best_assign = self.population[0]
        return best_assign, self.fitness(best_assign)

    def initialize_population(self):
        """

        Args:
            population_size: number of individuals in the population

        Returns:

        """
        self.population = []

        for i in range(self.population_size):
            self.population.append(self.initialize_individual())

    def initialize_individual(self):
        """This function initialize a random assignment

        Args:
            num_entities:
            num_units:

        Returns: random assignment

        """

        assigns = self.weights.argmin(axis=1)
        assign_mat = np.zeros((self.num_units, self.num_entities))
        assign_mat[np.arange(self.num_units), assigns] = 1.

        return assign_mat

    @staticmethod
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

    def optimization_step(self, num_steps):
        """This function performs one optimization step - breeds new population,
        select the best candidates from it and writes the best individual to the data base.

        Args:
            population:
            num_steps: current number of optimization steps.
            locked: list of indices of "locked" entities whose assignment should not be changed

        Returns:

        """
        self.hueristic_exponent = 20. / num_steps
        self.mutation_frac = max(0.1, 1. - num_steps / 3.)

        self.breed()

    def breed(self):
        """This function breeds the current population to generate the new population.
        The new population consists of the old population, the mutations and the children.

        Args:
            population:
            hueristic_exponent: used to determine the probability of a unit to be swaped.
            mutation_frac: what fraction of units in an individual should be mutated
            locked: list of inbdices of "locked" entities whose assignment should not change
            num_mutants: number of individuals in the population to be mutated
            num_pairs: number of pairs to be mated

        Returns: new population

        """

        # calculate probability to chose each individual in the population for mutation
        probs = self.get_prob_from_fitness(self.fitness, self.population)

        sampled_population_ind = np.random.choice(np.arange(len(self.population)), self.num_mutants, True, probs)
        mutated_population = self.mutation(self.clone_population(self.population), sampled_population_ind)

        mating_population = self.clone_population(self.population) + self.clone_population(mutated_population)

        # calculate probability to chose each individual in the mating population for mating
        probs_mating = self.get_prob_from_fitness(self.fitness, mating_population)

        mated = []
        pairs = [np.random.choice(np.arange(len(mating_population)), 2, False, probs_mating) for i in
                 range(self.num_pairs)]

        for pair in pairs:
            mating_res = self.crossover(mating_population[pair[0]], mating_population[pair[1]])
            if len(mating_res) > 0:
                mated.append(mating_res[0])
                mated.append(mating_res[1])

        del mating_population

        self.population = self.select(self.population[:] + mutated_population[:] + mated[:])

    def fitness(self, assignment):

        """ Fitness function to be minimized.
            The fitness is quadratic in the distance between the units and entities,
            and in over and under capacity. the distance and capacity may have different weights.

        Args:
            assignment: assignment of each unit to an entity.
                        sparse matrix with one 1 entry in each row

        Returns: the fitness of the individual (represented by A)

        """

        # difference between capacity and assignment results
        assignment_population = np.dot(assignment.T, self.units_population)
        assign_capacity_diff = self.capacity - assignment_population

        # handle differently over and under capacity
        over_cap_vec = (assign_capacity_diff * (assign_capacity_diff > 0)) ** 2
        under_cap_vec = (assign_capacity_diff * (assign_capacity_diff < 0)) ** 2

        distance_val = np.sum(np.multiply(assignment, self.weights) ** 2)

        # each cell in the matrix count how many neighbors of unit go to entity if unit goe to entity
        num_neighboring = np.multiply(np.dot(self.adj_mat, assignment), assignment)
        num_neighboring[np.where(assignment > 0)] += 1
        # normalize by the number of the unit's neighbors
        num_neighboring_av = np.divide(num_neighboring.T, self.UNIT_NBR_NUM).T
        adj_val = np.sum((1. - num_neighboring_av) ** 2)

        return distance_val * self.DIST_WEIGHT + np.sum(over_cap_vec) * self.OVER_CAPACITY_WEIGHT + \
               np.sum(under_cap_vec) * self.UNDER_CAPACITY_WEIGHT + self.ADJ_WEIGHT * adj_val

    @staticmethod
    def get_prob_from_fitness(fitness, population):
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

    def select(self, population):
        """This function selects the best candidates from the new population

        Args:
            population:
            survival_fraction:
            max_population:

        Returns: the best individuals from the populaiton

        """
        sorted_population = sorted(population, key=self.fitness)
        num_survivors = min(int(len(population) * self.survival_fraction), self.population_size)
        survivors = sorted_population[:num_survivors]
        return survivors

    def mutation(self, population, sampled_population_ind):
        """This function mutates all individuals in the population.

        Args:
            population:
            sampled_population_ind: indices of individuals in the populations to be mutated.
            mutation_frac: fraction of units in the individual to be nutated
            hueristic_exponent:
            allowed_indices: list of indices of "unlocked" units

        Returns: a population of mutated individuals

        """
        if self.mutation_frac == 0:
            return population

        self.num_mutations = int(self.mutation_frac * self.num_units)

        for ind in sampled_population_ind:
            population[ind] = self.mutate_individual(population[ind])

        return [population[ind] for ind in sampled_population_ind]

    def mutate_individual(self, individual):
        """This function mutates a specific individual in the population
            without changing the locked units.

        Args:
            individual:
            allowed_indices:
            hueristic_exponent:
            num_mutations:

        Returns: The mutated individual (in place)

        """

        # calculate mutation probability for each unit.
        # hueristic_exponen allows mutating with high prob units with large distances.
        fitness_vals_normed = (np.multiply(individual, self.weights).sum(axis=1) / np.max(self.weights))

        exploration_fitness_vals = fitness_vals_normed ** self.hueristic_exponent
        mutation_prob = exploration_fitness_vals / np.sum(exploration_fitness_vals)

        # chose which rows to mutate based on the mutation probability
        mutated_units = np.random.choice(self.num_units, self.num_mutations, False, mutation_prob)
        new_entities = np.random.randint(0, self.num_entities, len(mutated_units))
        individual[mutated_units, :] = 0.
        individual[mutated_units, new_entities] = 1.

        return individual

    @staticmethod
    def crossover(ind_a, ind_b):
        """This function mates between two individuals and creates two children using swap strategie.

        Args:
            ind_a:
            ind_b:

        Returns: 2 children

        """
        #import ipdb; ipdb.set_trace()
        diffs = np.logical_not(np.equal(ind_a, ind_b).prod(axis=1))
        diffs_ind = np.where(diffs == 1)
        num_diffs = diffs_ind[0].shape[0]

        # don't swap if the assignment of the individuals is identical
        if num_diffs == 0:
            return []

        child_a = np.copy(ind_a)
        child_b = np.copy(ind_b)

        # chose randomly which units to swap
        fraction = np.random.rand(1)
        num_genes = int(fraction * num_diffs)
        swap_ind = np.random.choice(np.arange(ind_a.shape[0])[diffs_ind], num_genes)

        child_a[swap_ind] = ind_b[swap_ind]
        child_b[swap_ind] = ind_a[swap_ind]

        return child_a, child_b
