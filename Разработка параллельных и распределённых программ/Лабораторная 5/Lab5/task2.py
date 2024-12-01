import threading
import random


class Node:
    def __init__(self, value=None):
        self.value = value
        self.next = None


class LinkedList:
    def __init__(self):
        self.head = None

    def contains(self, value):
        current = self.head
        while current:
            if current.value == value:
                return True
            current = current.next
        return False

    def append(self, value):
        new_node = Node(value)
        if not self.head:
            self.head = new_node
            return
        current = self.head
        while current.next:
            current = current.next
        current.next = new_node

    def to_list(self):
        result = []
        current = self.head
        while current:
            result.append(current.value)
            current = current.next
        return result


class ReadWriteLock:
    def __init__(self):
        self.readers = 0
        self.lock = threading.Lock()
        self.write_lock = threading.Lock()

    def acquire_read(self):
        with self.lock:
            self.readers += 1
            if self.readers == 1:
                self.write_lock.acquire()

    def release_read(self):
        with self.lock:
            self.readers -= 1
            if self.readers == 0:
                self.write_lock.release()

    def acquire_write(self):
        self.write_lock.acquire()

    def release_write(self):
        self.write_lock.release()


class RandomNumberGeneratorThread(threading.Thread):
    def __init__(self, list_, lock, count, thread_id):
        super().__init__()
        self.list = list_
        self.lock = lock
        self.count = count
        self.thread_id = thread_id

    def run(self):
        for _ in range(self.count):
            number = random.randint(0, 1000)

            # Блокировка на чтение для проверки наличия числа
            self.lock.acquire_read()
            exists = self.list.contains(number)
            self.lock.release_read()

            # Если числа нет в списке, блокировка на запись
            if not exists:
                self.lock.acquire_write()
                if not self.list.contains(number):
                    self.list.append(number)
                    print(f"Поток {self.thread_id}: добавлено {number}")
                self.lock.release_write()


def main(n_threads=5, numbers_per_thread=100):
    linked_list = LinkedList()
    lock = ReadWriteLock()

    threads = []
    for i in range(n_threads):
        thread = RandomNumberGeneratorThread(linked_list, lock, numbers_per_thread, i)
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()

    final_list = linked_list.to_list()
    print("Длина списка:", len(final_list))
    print("Уникальных элементов:", len(set(final_list)))
    if len(final_list) == len(set(final_list)):
        print("Повторяющихся нет")


if __name__ == "__main__":
    main(10, 100)
