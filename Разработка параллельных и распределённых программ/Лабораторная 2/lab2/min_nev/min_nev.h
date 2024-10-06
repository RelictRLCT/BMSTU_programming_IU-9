#ifndef LAB2CPP_MIN_NEV_H
#define LAB2CPP_MIN_NEV_H

#endif //LAB2CPP_MIN_NEV_H

#include <vector>
#include <mpi.h>
#include <cmath>
#include "../multiplication/multiplication.h"

const double epsilon = 0.001;

using namespace std;

void distribute_rows(int N, int size, vector<int>& counts, vector<int>& displs);

void min_nev(MPI_Comm, const vector<vector<double>>&, const vector<double>&, int,
             vector<double>&, const vector<int>&, const vector<int>&);
