from time import time

import cv2
import numpy as np

from utils import generate_population, calculate_population_fitness, \
    select_breeding_pool, do_crossover, mutate

IMAGE_FILE = "./images/apple.jpg"

LINE_LENGTH = 64  # 64 is optimal for big objects
# 240 for cat
# 0 for fox
# 255 for apple
# 38 for pepsi
# 0 for starbucks2
# 100 for plains
BACKGROUND_COLOR = 255

POPULATION_SIZE = 128
BREEDING_INDIVIDUALS = 28
CROSSOVER_INDIVIDUALS = POPULATION_SIZE - BREEDING_INDIVIDUALS


# =======================================================================================

def simulate(target: np.ndarray) -> None:
    """
    Start main GA loop
    :param target: target image
    """
    population = generate_population(POPULATION_SIZE, target.shape, default_color=BACKGROUND_COLOR)
    fitness = calculate_population_fitness(population, target)
    start = time()
    print("Started evolution")

    for step in range(4294967296):  # 2^32
        parents = select_breeding_pool(population, fitness, BREEDING_INDIVIDUALS)
        crossover = do_crossover(parents, BREEDING_INDIVIDUALS, target.shape, CROSSOVER_INDIVIDUALS)
        mutation = mutate(crossover, target.shape, CROSSOVER_INDIVIDUALS, LINE_LENGTH, target)
        population[0:parents.shape[0], :] = parents
        population[parents.shape[0]:, :] = mutation
        fitness = calculate_population_fitness(population, target)

        best_ind_index = np.argmin(fitness)
        best_ind = population[best_ind_index]
        cv2.imshow("Current-min", best_ind)
        cv2.waitKey(1)
        if step % 500 == 0:
            print('%.6d - %.6f seconds' % (step, (time() - start)), end='\t\t')
            print('Fitness -\t{}'.format(np.min(fitness)))

            best_ind_index = np.argmin(fitness)
            best_ind = population[best_ind_index]
            cv2.imwrite(f"./out/{step}-{fitness[best_ind_index]}.jpg", best_ind)


if __name__ == '__main__':
    target_image = cv2.imread(IMAGE_FILE)
    target_image = cv2.cvtColor(target_image, cv2.COLOR_BGR2GRAY)
    cv2.imshow("Target", target_image)
    # cv2.waitKey(0)
    simulate(target_image)
