import threading
import numpy as np
import time

height = 6
width = 6
num_steps = 10
num_threads = 2

np.random.seed(14)
matrix = np.random.randint(2, size=(height, width))
new_matrix = np.zeros((height, width), dtype=int)

# Для синхронизации граничных строк
row_locks = [threading.Lock() for _ in range(num_threads)]

# Общие переменные для граничных строк
boundary_rows_up = [np.empty(width, dtype=int) for _ in range(num_threads)]
boundary_rows_down = [np.empty(width, dtype=int) for _ in range(num_threads)]

barrier = threading.Barrier(num_threads)


class WorkerThread(threading.Thread):
    def __init__(self, thread_id, matrix, new_matrix, start_row, end_row):
        threading.Thread.__init__(self)
        self.thread_id = thread_id
        self.matrix = matrix
        self.new_matrix = new_matrix
        self.start_row = start_row
        self.end_row = end_row

    def run(self):
        for step in range(num_steps):
            # Граничные строки
            upper_row = self.matrix[self.start_row].copy()
            lower_row = self.matrix[self.end_row - 1].copy()

            # Запись своих граничных строк в общие переменные
            with row_locks[self.thread_id]:
                boundary_rows_up[self.thread_id] = upper_row
                boundary_rows_down[self.thread_id] = lower_row

            # Соседние потоки
            neighbor_up = (self.thread_id - 1) % num_threads
            neighbor_down = (self.thread_id + 1) % num_threads

            barrier.wait()

            # Граничные строки соседей
            with row_locks[neighbor_up]:
                neighbor_up_row = boundary_rows_down[neighbor_up].copy()
            with row_locks[neighbor_down]:
                neighbor_down_row = boundary_rows_up[neighbor_down].copy()

            extended_matrix = np.vstack([
                neighbor_up_row[np.newaxis, :],
                self.matrix[self.start_row:self.end_row],
                neighbor_down_row[np.newaxis, :]
            ])

            # Количество соседей
            left = np.roll(extended_matrix, 1, axis=1)
            right = np.roll(extended_matrix, -1, axis=1)
            up = np.roll(extended_matrix, -1, axis=0)
            down = np.roll(extended_matrix, 1, axis=0)
            up_left = np.roll(up, 1, axis=1)
            up_right = np.roll(up, -1, axis=1)
            down_left = np.roll(down, 1, axis=1)
            down_right = np.roll(down, -1, axis=1)

            neighbor_count = (left + right + up + down +
                              up_left + up_right + down_left + down_right)

            # Верхняя и нижняя строки -- строки соседей
            neighbor_count = neighbor_count[1:-1]
            own_strip = self.matrix[self.start_row:self.end_row]

            self.new_matrix[self.start_row:self.end_row] = (
                ((own_strip == 1) & ((neighbor_count == 2) | (neighbor_count == 3))) |
                ((own_strip == 0) & (neighbor_count == 3))
            ).astype(int)

            barrier.wait()

            self.matrix[self.start_row:self.end_row] = self.new_matrix[self.start_row:self.end_row]

            barrier.wait()

            if self.thread_id == 0:
                barrier.wait()
                print(f"Шаг {step + 1}:")
                print(self.matrix)
            else:
                barrier.wait()

rows_per_thread = height // num_threads
threads = []

for i in range(num_threads):
    start_row = i * rows_per_thread
    end_row = (i + 1) * rows_per_thread if i != num_threads - 1 else height
    thread = WorkerThread(i, matrix, new_matrix, start_row, end_row)
    threads.append(thread)

for thread in threads:
    thread.start()

start_time = time.time()

for thread in threads:
    thread.join()

end_time = time.time()
threaded_time_per_step = (end_time - start_time) / num_steps
print(f"Среднее время на шаг с использованием потоков: {threaded_time_per_step:.6f} секунд")

# Однопоточная версия
matrix_single = np.random.randint(2, size=(height, width))
new_matrix_single = np.zeros((height, width), dtype=int)

def single():
    for step in range(num_steps):
        left = np.roll(matrix_single, 1, axis=1)
        right = np.roll(matrix_single, -1, axis=1)
        up = np.roll(matrix_single, -1, axis=0)
        down = np.roll(matrix_single, 1, axis=0)
        up_left = np.roll(up, 1, axis=1)
        up_right = np.roll(up, -1, axis=1)
        down_left = np.roll(down, 1, axis=1)
        down_right = np.roll(down, -1, axis=1)

        neighbor_count = (left + right + up + down +
                          up_left + up_right + down_left + down_right)

        new_matrix_single[:, :] = (
                ((matrix_single == 1) & ((neighbor_count == 2) | (neighbor_count == 3))) |
                ((matrix_single == 0) & (neighbor_count == 3))
        ).astype(int)

        matrix_single[:, :] = new_matrix_single

        print(f"Однопоточный шаг {step + 1}:")
        print(matrix_single)

start_time_single = time.time()
single()
end_time_single = time.time()
single_threaded_time_per_step = (end_time_single - start_time_single) / num_steps
print(f"Среднее время на шаг без потоков: {single_threaded_time_per_step:.6f} секунд")
