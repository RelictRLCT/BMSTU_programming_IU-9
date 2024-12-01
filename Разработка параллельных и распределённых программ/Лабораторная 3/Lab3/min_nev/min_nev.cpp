#include "min_nev.h"
#include <iostream>
#include <vector>
#include <cmath>
#include "../multiplication/multiplication.h"
#include <omp.h>

using namespace std;

void min_nev(const vector<double>& A, const vector<double>& b, const int N, vector<double>& x, double epsilon) {
    vector<double> y(N, 0.0);
    vector<double> Ay(N, 0.0);

    double norm_b = sqrt(dot_product(b, b));
    if (norm_b == 0.0) {
        cerr << "Норма вектора b равна нулю." << endl;
        return;
    }

    int iteration = 0;
    bool converged = false;

    while (!converged) {
#pragma omp parallel
        {
            // y = A * x - b
#pragma omp for //schedule(static)
            for (int i = 0; i < N; ++i) {
                y[i] = -b[i];
                for (int j = 0; j < N; ++j) {
                    y[i] += A[i * N + j] * x[j];
                }
            }

            // Ay = A * y
#pragma omp for //schedule(static)
            for (int i = 0; i < N; ++i) {
                Ay[i] = 0.0;
                for (int j = 0; j < N; ++j) {
                    Ay[i] += A[i * N + j] * y[j];
                }
            }
        }

        double num = dot_product(y, Ay);
        double den = dot_product(Ay, Ay);
        double norm_sys = dot_product(y, y);

        double crit = sqrt(norm_sys) / norm_b;
        //cout << "Итерация " << iteration << ": Норма невязки = " << crit << endl;
        if (crit < epsilon) {
            converged = true;
        } else {
            double tau = num / den;
#pragma omp parallel for //schedule(static)
            for (int i = 0; i < N; ++i) {
                x[i] -= tau * y[i];
            }
        }
        iteration++;
    }
}

