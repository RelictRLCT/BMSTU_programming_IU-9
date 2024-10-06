#include "systems_creation.h"

using namespace std;

// Модельная задача с заданным решением
void create_system_test_1(int N, vector<vector<double>>& A, vector<double>& b) {
    A.resize(N, vector<double>(N, 1.0));
    for (int i = 0; i < N; ++i) {
        A[i][i] = 2.0;
        b[i] = N + 1;
    }
}

// Модельная задача с произвольным решением
void create_system_test_2(int N, vector<vector<double>>& A, vector<double>& b) {
    A.resize(N, vector<double>(N, 1.0));
    for (int i = 0; i < N; ++i) {
        A[i][i] = 2.0;
    }
    vector<double> u(N);
    for (int i = 0; i < N; ++i) {
        u[i] = sin(2.0 * i * M_PI / N);
    }
    b.resize(N);
    // Вычисляем b = A * u
    for (int i = 0; i < N; ++i) {
        b[i] = 0.0;
        for (int j = 0; j < N; ++j) {
            b[i] += A[i][j] * u[j];
        }
    }
}

// Функция для создания случайной симметричной матрицы A и вектора b
void create_system_random(int N, vector<vector<double>>& A, vector<double>& b) {
    A.resize(N, vector<double>(N));
    b.resize(N);
    for (int i = 0; i < N; ++i) {
        b[i] = ((double)rand() / RAND_MAX) * 100.0;
        for (int j = 0; j <= i; ++j) {
            double val = ((double)rand() / RAND_MAX) * 100.0;
            A[i][j] = val;
            A[j][i] = val;  // Делаем матрицу симметричной
        }
    }
}