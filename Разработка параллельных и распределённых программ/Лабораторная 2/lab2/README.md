# Компиляция
mpicxx -o lab2 main.cpp systems/systems_creation.cpp multiplication/multiplication.cpp print/print.cpp min_nev/min_nev.cpp 
# Запуск (-np - число процессов)
mpirun -np 8 ./lab2
