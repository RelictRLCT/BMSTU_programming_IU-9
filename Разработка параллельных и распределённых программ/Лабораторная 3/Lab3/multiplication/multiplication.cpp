#include "multiplication.h"

void matrix_vector_mul(const vector<vector<double>>& A, const vector<double>& x, vector<double>& result) {
    int N = A.size();
#pragma omp parallel for
    for (int i = 0; i < N; ++i) {
        double sum = 0.0;
        for (int j = 0; j < x.size(); ++j) {
            sum += A[i][j] * x[j];
        }
        result[i] = sum;
    }
}

double dot_product(const vector<double>& a, const vector<double>& b) {
    double result = 0.0;
    int N = a.size();
#pragma omp parallel for reduction(+:result)
    for (int i = 0; i < N; ++i) {
        result += a[i] * b[i];
    }
    return result;
}
