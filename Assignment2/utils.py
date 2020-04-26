from typing import Tuple

import numba
import numpy as np

from color import choose_best_color

LINE_LENGTH_OFFSET = 5
OPACITY = 0.12


def generate_population(population_size, shape, default_color=255):
    population = np.full(shape=(population_size, *shape), fill_value=default_color, dtype=np.uint8)
    return population


def calculate_population_fitness(population, target):
    fitness = np.zeros(population.shape[0])
    for i in range(len(population)):
        fitness[i] = np.sum(np.absolute(target - population[i]))
    return fitness


def select_breeding_pool(population, fitness, breeding_count):
    parents = np.zeros(shape=(breeding_count, *population.shape[1:]), dtype=np.uint8)

    ind = np.argsort(fitness)
    for i in range(breeding_count):
        parents[i] = population[ind[i]]
    return parents


def do_crossover(parents, breeding_count, image_shape, crossover_count):
    children = np.zeros(shape=(crossover_count, *image_shape), dtype=np.uint8)
    crossover_point = int(image_shape[0] / 2)
    for i in range(crossover_count):
        p1_id = int((breeding_count - 1) * np.random.uniform(0, 1))
        p2_id = int((breeding_count - 1) * np.random.uniform(0, 1))
        children[i, 0:crossover_point, :] = parents[p1_id, 0:crossover_point, :]
        children[i, crossover_point:] = parents[p2_id, crossover_point:]
        # children[i, crossover_point:, :] = 0

        # cv2.imshow("P1", parents[p1_id])
        # cv2.imshow("P2", parents[p2_id])
        # cv2.imshow("Child", children[i])
        # cv2.waitKey(1)
    return children


@numba.njit(parallel=True)
def minmax(a: int, b: int) -> Tuple[int, int]:
    minimum = min(a, b)
    maximum = max(a, b)
    return minimum, maximum


@numba.njit()
# The following function is taken from https://stackoverflow.com/a/25913345/6766934
def get_line(x1, y1, x2, y2):
    points = []
    issteep = abs(y2 - y1) > abs(x2 - x1)
    if issteep:
        x1, y1 = y1, x1
        x2, y2 = y2, x2
    rev = False
    if x1 > x2:
        x1, x2 = x2, x1
        y1, y2 = y2, y1
        rev = True
    deltax = x2 - x1
    deltay = abs(y2 - y1)
    error = int(deltax / 2)
    y = y1
    ystep = None
    if y1 < y2:
        ystep = 1
    else:
        ystep = -1
    for x in range(x1, x2 + 1):
        if issteep:
            points.append((y, x))
        else:
            points.append((x, y))
        error -= deltay
        if error < 0:
            y += ystep
            error += deltax
    # Reverse the list if the coordinates were reversed
    if rev:
        points.reverse()
    return points


def draw_line(image, line_length, image_shape, line_count, target):
    for _ in range(line_count):
        offset = 0
        x_start = np.random.randint(offset, image_shape[1] - offset)
        y_start = np.random.randint(offset, image_shape[0] - offset)

        angle = np.radians(np.random.uniform(0, 360))
        line_length = max(15, np.random.randint(line_length - LINE_LENGTH_OFFSET, line_length + LINE_LENGTH_OFFSET))

        x_end = max(
            min(
                int(line_length * np.cos(angle) + x_start),
                image_shape[1] - 1
            ),
            0
        )
        y_end = max(
            min(
                int(line_length * np.sin(angle) + y_start),
                image_shape[0] - 1
            ),
            0
        )
        line_points = get_line(x_start, y_start, x_end, y_end)
        target_line = np.array([target[y, x] for x, y in line_points])
        image_line = np.array([image[y, x] for x, y in line_points])

        colored, _ = choose_best_color(
            target_line=target_line, image_line=image_line, opacity=OPACITY
        )

        for i in range(len(line_points)):
            x, y = line_points[i]
            color = colored[i]
            image[y, x] = color


def mutate(population, image_shape, population_size, line_length, target):
    for i in range(population_size):
        if np.random.uniform(0, 1) < 0.9:
            line_count = np.random.randint(1, 5)
            draw_line(population[i], line_length, image_shape, line_count, target)
    return population
