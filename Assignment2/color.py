from typing import Tuple

import cv2
import numpy as np

COLOR_POPULATION_SIZE = 8
COLOR_PARENTS = 3
COLOR_BEST_TO_NEW_POPULATION = 2
COLOR_CHILDREN_TO_PRODUCE = COLOR_POPULATION_SIZE - COLOR_PARENTS - COLOR_BEST_TO_NEW_POPULATION
if COLOR_CHILDREN_TO_PRODUCE < 0:
    raise ValueError(f"Keep COLOR_PARENTS({COLOR_PARENTS}) + "
                     f"COLOR_BEST_TO_NEW_POPULATION({COLOR_BEST_TO_NEW_POPULATION}) "
                     f"< COLOR_POPULATION_SIZE({COLOR_POPULATION_SIZE})")
COLOR_MAX_EVOLUTION_STEPS = 5


def color_initiate_population(line_points, opacity: float):
    colors = list(map(int, np.random.randint(low=0, high=256, size=COLOR_POPULATION_SIZE)))
    initial_population = np.repeat(line_points[np.newaxis, ...], COLOR_POPULATION_SIZE, axis=0)
    for i in range(COLOR_POPULATION_SIZE):
        overlay = np.full_like(initial_population[i], colors[i])
        cv2.addWeighted(overlay, opacity, initial_population[i], 1 - opacity, 0, dst=initial_population[i])
    return initial_population


def calculate_color_fitness(target_part, population):
    fitness = np.zeros(population.shape[0])
    for i in range(len(population)):
        fitness[i] = np.sum(np.absolute(target_part - population[i]))
    return fitness


def select_parents_pool(fitness, population):
    minimum_error_idx = fitness.argmin()
    min_error = fitness[minimum_error_idx]
    if min_error == 0:
        return None, minimum_error_idx
    parents = np.zeros((COLOR_PARENTS, *population.shape[1:]))
    error_sum = np.sum(fitness)
    to_max = np.true_divide(error_sum, fitness)
    to_max_cumsum = np.cumsum(to_max)
    to_max_cumsum_norm = np.true_divide(to_max_cumsum, to_max_cumsum[-1])

    for i in range(COLOR_PARENTS):
        p = np.random.rand(1)
        for j in range(len(to_max_cumsum_norm)):
            if p <= to_max_cumsum_norm[j]:
                parents[i] = population[j]
                break
    return parents, None


def perform_crossover(parents):
    crossover = np.zeros((COLOR_CHILDREN_TO_PRODUCE, *parents[0].shape))
    for i in range(COLOR_CHILDREN_TO_PRODUCE):
        p1 = np.random.randint(0, len(parents))
        p2 = np.random.randint(0, len(parents))

        crossover[i] = (parents[p1] + parents[p2]) / 2
    return crossover


def choose_best_color(target_line: np.ndarray, image_line: np.ndarray, opacity: float) \
        -> Tuple[np.ndarray, int]:
    color_evolution_step = 0
    target_line = target_line.astype(np.int16)
    image_line = image_line.astype(np.int16)
    population = color_initiate_population(image_line, opacity)
    fitness = calculate_color_fitness(target_line, population)

    exact_individual = None

    while color_evolution_step < COLOR_MAX_EVOLUTION_STEPS:
        parents, exact_hit_idx = select_parents_pool(fitness, population)
        if exact_hit_idx is not None:
            exact_individual = population[exact_hit_idx]
            break
        crossover = perform_crossover(parents)
        best_ind = np.argsort(fitness)
        best_individuals = np.zeros((COLOR_BEST_TO_NEW_POPULATION, *population[0].shape))
        for i in range(COLOR_BEST_TO_NEW_POPULATION):
            best_individuals[i] = population[best_ind[i]]
        population[0: COLOR_BEST_TO_NEW_POPULATION] = best_individuals
        population[COLOR_BEST_TO_NEW_POPULATION: COLOR_BEST_TO_NEW_POPULATION + COLOR_PARENTS] = parents
        population[COLOR_BEST_TO_NEW_POPULATION + COLOR_PARENTS:] = crossover
        fitness = calculate_color_fitness(target_line, population)
        color_evolution_step += 1

    if exact_individual is None:
        smallest_error_idx = fitness.argmin()
        smallest_error = fitness[smallest_error_idx]
        best = population[smallest_error_idx].astype(np.uint8)
        return best, int(smallest_error)
    else:
        return exact_individual, 0
