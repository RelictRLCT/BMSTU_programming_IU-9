#include "multiplication.h"

// Функция для вычисления скалярного произведения двух векторов
double dot_product(const vector<double>& a, const vector<double>& b) {
    double local_sum = 0.0;
    int N = a.size();
    for (int i = 0; i < N; ++i) {
        local_sum += a[i] * b[i];
    }
    return local_sum;
}

// Функция для локального умножения матрицы на вектор
void matrix_vector_mul_local(const vector<vector<double>>& A_local,
                             const vector<double>& x,
                             vector<double>& result) {
    int num_rows = A_local.size();
    int N = x.size();
    result.resize(num_rows);
    for (int i = 0; i < num_rows; ++i) {
        double sum = 0.0;
        for (int j = 0; j < N; ++j) {
            sum += A_local[i][j] * x[j];
        }
        result[i] = sum;
    }
}
