custom_function <- function(x) {
  return(x^2 - 4*x + 4)  
}

population_size <- 150
num_generations <- 50000
mutation_rate <- 0.05
crossover_rate <- 0.7
lower_bound <- -10
upper_bound <- 10

initialize_population <- function(size, lower_bound, upper_bound) {
  return(runif(size, lower_bound, upper_bound))
}

evaluate_fitness <- function(population) {
  return(sapply(population, custom_function))
}

select_parents <- function(population, fitness) {
  fitness[is.na(fitness)] <- 0
  selected_indices <- sample(1:length(population), size = length(population), replace = TRUE, prob = fitness)
  return(population[selected_indices])
}

crossover <- function(parent1, parent2) {
  if (runif(1) < crossover_rate) {
    point <- sample(1:length(parent1), 1)
    child1 <- c(parent1[1:point], parent2[(point+1):length(parent2)])
    child2 <- c(parent2[1:point], parent1[(point+1):length(parent1)])
    return(c(child1, child2))
  } else {
    return(c(parent1, parent2))
  }
}

mutate <- function(individual) {
  if (runif(1) < mutation_rate) {
    point <- sample(1:length(individual), 1)
    individual[point] <- runif(1, lower_bound, upper_bound)
  }
  return(individual)
}

genetic_algorithm <- function() {
  population <- initialize_population(population_size, lower_bound, upper_bound)
  for (generation in 1:num_generations) {
    fitness <- evaluate_fitness(population)
    parents <- select_parents(population, fitness)
    new_population <- c()
    for (i in seq(1, length(parents), by = 2)) {
      offspring <- crossover(parents[i], parents[i+1])
      new_population <- c(new_population, mutate(offspring[1]), mutate(offspring[2]))
    }
    population <- new_population
  }
  best_individual <- population[which.max(evaluate_fitness(population))]
  return(best_individual)
}

best_solution <- genetic_algorithm()
cat("Best solution found:", best_solution, "\n")
cat("Maximum value of the function:", custom_function(best_solution), "\n")