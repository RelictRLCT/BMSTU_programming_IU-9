#include <chrono>
#include <cstdlib>
#include <iostream>
#include <vector>
#include <cmath>
#include "systems/systems_creation.h"
#include "print/print.h"
#include "min_nev/min_nev.h"
#include <omp.h>

using namespace std;

const double epsilon = 1e-3;

int main() {
    int N = 50;
    vector<vector<double>> A_full(N, vector<double>(N, 0.0));
    vector<double> b_full(N, 0.0);
    srand(14);

    create_system_random(N, A_full, b_full);

    vector<double> x(N, 0.0);

    auto start = chrono::high_resolution_clock::now();

    vector<double> A_vec(N * N, 0.0);
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A_vec[i * N + j] = A_full[i][j];
        }
    }

    min_nev(A_vec, b_full, N, x, epsilon);

    auto end = chrono::high_resolution_clock::now();
    chrono::duration<double> elapsed = end - start;

    cout << "Матрица A:" << endl;
    print_matrix(A_full);
    cout << "Вектор b:" << endl;
    print_vector(b_full);
    cout << "Решение x:" << endl;
    print_vector(x);

    cout << "Время выполнения: " << elapsed.count() << " секунд" << endl;

    return 0;
}
