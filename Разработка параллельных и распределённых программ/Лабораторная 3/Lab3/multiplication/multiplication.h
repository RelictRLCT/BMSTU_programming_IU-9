#ifndef LAB2CPP_MULTIPLICATION_H
#define LAB2CPP_MULTIPLICATION_H

#include <vector>

using namespace std;

double dot_product(const vector<double>& a, const vector<double>& b);

void matrix_vector_mul(const vector<vector<double>>&, const vector<double>&, vector<double>&);

#endif //LAB2CPP_MULTIPLICATION_H