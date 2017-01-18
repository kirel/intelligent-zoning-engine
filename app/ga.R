# takes a list of individuals and breeds new ones in addition
# returns the new population (that includes the old one)
ga_breed = function(population, ga_mutate, ga_crossover, fitness_f, num_pairs = 50, num_mutants = 100, mutation_fraction=0.001, heuristic_exponent=3) {
  # precalculate sampling weights for sampling from the fittest
  fitness = unlist(map(population, fitness_f))
  f_summary = summary(fitness) 
  flog.debug('original population fitness %s', toString(purrr::flatten(purrr::zip2(names(f_summary), f_summary))), name='optimization')
  weights = -fitness+min(fitness)+max(fitness)
  prob = weights/sum(weights)
  
  flog.debug('Mutating %s individuals', num_mutants, name='optimization')
  mutated = population %>%
    sample(num_mutants, replace = T, prob = prob) %>% # sample based on fitness
    map(~ ga_mutate(.x, mutation_fraction, heuristic_exponent))
  if (length(mutated)>0) {
    f_summary = summary(unlist(map(mutated, fitness_f)))
    flog.debug('mutated fitness %s', toString(purrr::flatten(purrr::zip2(names(f_summary), f_summary))), name='optimization')
  }
  
  mating_population = c(population, mutated)
  if (length(mating_population) > 1) {
    mating_fitness = unlist(map(mating_population, fitness_f))
    mating_weights = -mating_fitness+min(mating_fitness)+max(mating_fitness)
    mating_prob = mating_weights/sum(mating_weights)
    
    flog.debug('Mating %s pairs', num_pairs, name='optimization')
    pairs = (1:num_pairs) %>% map(~ sample(mating_population, 2, prob = mating_prob)) # sample based on fitness
    mated = do.call(c, map(pairs, ~ do.call(ga_crossover, .x)))
    f_summary = summary(unlist(map(mated, fitness_f)))
    flog.debug('mated fitness %s', toString(purrr::flatten(purrr::zip2(names(f_summary), f_summary))), name='optimization')
  } else {
    mated = list()
  }
  
  # original population, mutated population and mated population
  new_population = c(
    population,
    mutated,
    mated
  )
}

# selects the fittest individuals according to a fitness function
ga_select = function(population, fitness_f, survival_fraction=1, max_population=50) {
  flog.debug('Population size %s', length(population), name='optimization')
  population = sort_by(unique(population), fitness_f)
  flog.debug('%s unique', length(population), name='optimization')
  num_survivors = min(ceiling(length(population)*survival_fraction), max_population)
  flog.debug('Keeping %s survivors', num_survivors, name='optimization')
  #survivors = population[rank(unlist(fitness), t = 'r') <= num_survivors]
  survivors = population[1:num_survivors]
  survivors
}