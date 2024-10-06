#include <mpi.h>
#include <ctime>
#include <cstdlib>
#include <algorithm>

#include "systems/systems_creation.h"
#include "print/print.h"
#include "multiplication/multiplication.h"
#include "min_nev/min_nev.h"

using namespace std;

int main(int argc, char* argv[]) {
    clock_t start = clock();

    int N = 5;
    int rank, size;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Comm new_comm;
        int count = (rank < N) ? 0 : MPI_UNDEFINED;
    MPI_Comm_split(MPI_COMM_WORLD, count, rank, &new_comm);

    if (count == MPI_UNDEFINED) {
        MPI_Finalize();
        return 0;
    }

    int new_rank, new_size;
    MPI_Comm_rank(new_comm, &new_rank);
    MPI_Comm_size(new_comm, &new_size);

    srand(14); // Чтоб при разных запусках матрица была одна и та же

    // Распределяем строки матрицы A и элементы вектора b между процессами
    vector<int> counts(new_size);
    vector<int> displs(new_size);
    distribute_rows(N, new_size, counts, displs);

    int num_rows = counts[new_rank];

    vector<vector<double>> A_local(num_rows, vector<double>(N));
    vector<vector<double>> A_full;
    vector<double> b_full(N);

    if (new_rank == 0) {
        create_system_random(N, A_full, b_full);
        //create_system_test_1(N, A_full, b_full);
    }

    // Преобразуем матрицу в одномерный массив для MPI_Scatterv
    vector<double> A_full_flat;
    if (new_rank == 0) {
        for (int i = 0; i < N; ++i) {
            A_full_flat.insert(A_full_flat.end(), A_full[i].begin(), A_full[i].end());
        }
    }

    vector<double> A_local_flat(num_rows * N);

    vector<int> sendcounts(new_size);
    vector<int> displs_counts(new_size);
    for (int i = 0; i < new_size; ++i) {
        sendcounts[i] = counts[i] * N;
        displs_counts[i] = displs[i] * N;
    }

    MPI_Scatterv(new_rank == 0 ? A_full_flat.data() : NULL, sendcounts.data(), displs_counts.data(), MPI_DOUBLE,
                 num_rows > 0 ? A_local_flat.data() : NULL, num_rows * N, MPI_DOUBLE, 0, new_comm);

    // Преобразуем A_local_flat обратно в A_local
    if (num_rows > 0) {
        for (int i = 0; i < num_rows; ++i) {
            A_local[i].assign(A_local_flat.begin() + i * N, A_local_flat.begin() + (i + 1) * N);
        }
    }

    // Передаем вектор b_full всем процессам
    MPI_Bcast(b_full.data(), N, MPI_DOUBLE, 0, new_comm);

    // Инициализируем x (дублируется на всех процессах)
    vector<double> x(N, 0.0);

    // Подготавливаем counts и displs для Allgatherv
    vector<int> recv_counts = counts;
    vector<int> recv_displs = displs;

    // Решаем систему с использованием параллельной функции min_nev
    min_nev(new_comm, A_local, b_full, N, x, recv_counts, recv_displs);

    // Только процесс с рангом 0 выводит результат
    if (new_rank == 0) {
        cout << "Матрица A:" << endl;
        print_matrix(A_full);
        cout << "Вектор b:" << endl;
        print_vector(b_full);
        cout << "Решение:" << endl;
        print_vector(x);

        clock_t end = clock();
        double seconds = (double)(end - start) / CLOCKS_PER_SEC;
        printf("Время выполнения: %f секунд\n", seconds);
    }

    MPI_Comm_free(&new_comm);
    MPI_Finalize();
    return 0;
}
