#include "min_nev.h"

// Функция для распределения строк между процессами
void distribute_rows(int N, int size, vector<int>& counts, vector<int>& displs) {
    counts.resize(size, 0);
    displs.resize(size, 0);

    int base_count = N / size;
    int remainder = N % size;
    int sum = 0;

    for (int i = 0; i < size; ++i) {
        counts[i] = base_count + (i < remainder ? 1 : 0);
        displs[i] = sum;
        sum += counts[i];
    }
}

// Функция реализации метода минимальных невязок
void min_nev(MPI_Comm comm, const vector<vector<double>>& A_local,
             const vector<double>& b, int N,
             vector<double>& x, const vector<int>& counts,
             const vector<int>& displs) {
    int rank, size;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);

    int num_rows = A_local.size();

    vector<double> y(N, 0.0);
    vector<double> local_y(num_rows);
    vector<double> local_Ay(num_rows);

    double norm_b;

    // Вычисляем норму вектора b
    double local_norm_b_sq = dot_product(b, b);
    double global_norm_b_sq;
    MPI_Allreduce(&local_norm_b_sq, &global_norm_b_sq, 1, MPI_DOUBLE, MPI_SUM, comm);

    norm_b = sqrt(global_norm_b_sq);

    // Проверяем, не равна ли нулю норма b
    if (norm_b == 0.0) {
        if (rank == 0) {
            cerr << "Норма вектора b равна нулю." << endl;
        }
        return;
    }

    int iteration = 0;
    while (true) {
        double local_num = 0.0;
        double local_den = 0.0;
        double local_norm_sq = 0.0;

        if (num_rows > 0) {
            // local_y = A_local * x - b_local
            matrix_vector_mul_local(A_local, x, local_y);
            for (int i = 0; i < num_rows; ++i) {
                local_y[i] -= b[i + displs[rank]]; // используем полный вектор b
            }
        }

        // Собираем local_y в y
        MPI_Allgatherv(num_rows > 0 ? local_y.data() : NULL, num_rows, MPI_DOUBLE, y.data(),
                       counts.data(), displs.data(), MPI_DOUBLE, comm);

        if (num_rows > 0) {
            // Вычисляем local_Ay = A_local * y
            matrix_vector_mul_local(A_local, y, local_Ay);

            // Вычисляем локальный числитель и знаменатель
            local_num = dot_product(local_y, local_Ay);
            local_den = dot_product(local_Ay, local_Ay);

            // Вычисляем локальную норму невязки в квадрате
            for (int i = 0; i < num_rows; ++i) {
                double res = local_y[i];
                local_norm_sq += res * res;
            }
        }

        // Суммируем глобальный числитель и знаменатель
        double global_num, global_den;
        MPI_Allreduce(&local_num, &global_num, 1, MPI_DOUBLE, MPI_SUM, comm);
        MPI_Allreduce(&local_den, &global_den, 1, MPI_DOUBLE, MPI_SUM, comm);

        // Вычисляем глобальную норму невязки в квадрате
        double global_norm_sq;
        MPI_Allreduce(&local_norm_sq, &global_norm_sq, 1, MPI_DOUBLE, MPI_SUM, comm);

        double crit = sqrt(global_norm_sq) / norm_b;
        if (rank == 0) {
            cout << "Итерация " << iteration << ": Норма невязки = " << crit << endl;
        }

        if (crit < epsilon) {
            break;
        }

        // Вычисляем tau
        double tau = global_num / global_den;

        // Обновляем x
        for (int i = 0; i < N; ++i) {
            x[i] -= tau * y[i];
        }

        iteration++;
    }
}