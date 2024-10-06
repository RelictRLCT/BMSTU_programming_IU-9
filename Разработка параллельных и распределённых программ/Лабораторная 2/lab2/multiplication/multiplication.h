#ifndef LAB2CPP_MULTIPLICATION_H
#define LAB2CPP_MULTIPLICATION_H

#endif //LAB2CPP_MULTIPLICATION_H

#include <vector>

using namespace std;

double dot_product(const vector<double>& a, const vector<double>& b);

void matrix_vector_mul_local(const vector<vector<double>>& A_local,
                             const vector<double>& x,
                             vector<double>& result);

