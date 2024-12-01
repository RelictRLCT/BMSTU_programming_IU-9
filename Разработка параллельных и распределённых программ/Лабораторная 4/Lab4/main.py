import threading
import time
import random

SIMULATION_TIME = 100


class Fork:
    def __init__(self, id_):
        self.id = id_
        self.lock = threading.Lock()
        self.owner = None


class Philosopher(threading.Thread):
    def __init__(self, id_, left_fork: Fork, right_fork: Fork, stop_flag, deadlock_flag):
        super().__init__()
        self.id = id_
        self.left_fork = left_fork
        self.right_fork = right_fork
        self.stop_flag = stop_flag
        self.deadlock_flag = deadlock_flag

    def think(self):
        print(f"Философ {self.id} размышляет")
        time.sleep(random.uniform(1, 2))

    def eat(self):
        print(f"Философ {self.id} ест")
        time.sleep(random.uniform(1, 2))

    def run(self):
        while not self.stop_flag.is_set():
            self.think()
            print(f"Философ {self.id} проголодался")

            # Попытка взять вилки
            with self.left_fork.lock:
                self.left_fork.owner = self.id
                print(f"Философ {self.id} взял левую вилку {self.left_fork.id}")
                time.sleep(random.uniform(0.5, 1))

                if self.deadlock_flag.is_set():
                    print(f"Философ {self.id} обнаружил дедлок и отпустил левую вилку {self.left_fork.id}")
                    self.left_fork.owner = None
                    self.deadlock_flag.clear()
                    continue

                with self.right_fork.lock:
                    self.right_fork.owner = self.id
                    print(f"Философ {self.id} взял правую вилку {self.right_fork.id}")
                    self.eat()
                    self.right_fork.owner = None
                self.left_fork.owner = None


class DeadlockMonitor(threading.Thread):
    def __init__(self, forks, deadlock_flag, stop_flag):
        super().__init__()
        self.forks = forks
        self.deadlock_flag = deadlock_flag
        self.stop_flag = stop_flag

    def run(self):
        while not self.stop_flag.is_set():
            time.sleep(2)

            owners = [fork.owner for fork in self.forks if fork.lock.locked()]
            if len(owners) == len(self.forks) and len(set(owners)) == len(owners):
                print("Приключился дедлок!")
                self.deadlock_flag.set()
                time.sleep(2)


def main(num_philosophers=5):
    forks = [Fork(i) for i in range(num_philosophers)]

    stop_flag = threading.Event()
    deadlock_flag = threading.Event()

    philosophers = [
        Philosopher(i, forks[i], forks[(i + 1) % num_philosophers], stop_flag, deadlock_flag)
        for i in range(num_philosophers)
    ]

    monitor = DeadlockMonitor(forks, deadlock_flag, stop_flag)

    monitor.start()
    for philosopher in philosophers:
        philosopher.start()

    time.sleep(SIMULATION_TIME)

    stop_flag.set()
    monitor.join()
    for philosopher in philosophers:
        philosopher.join()


if __name__ == "__main__":
    main(num_philosophers=5)
