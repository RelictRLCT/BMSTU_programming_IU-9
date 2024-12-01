#include "print.h"

void print_vector(const vector<double>& v) {
    for (double val : v) {
        cout << val << " ";
    }
    cout << endl;
}

void print_matrix(const vector<vector<double>>& A) {
    for (const auto& row : A) {
        for (double val : row) {
            cout << val << " ";
        }
        cout << endl;
    }
}