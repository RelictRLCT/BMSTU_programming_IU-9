cmake_minimum_required(VERSION 3.10)
project(Lab2cpp)

set(CMAKE_CXX_STANDARD 17)

add_executable(Lab2cpp main.cpp
        systems/systems_creation.h
        systems/systems_creation.cpp
        print/print.h
        print/print.cpp
        multiplication/multiplication.h
        multiplication/multiplication.cpp
        min_nev/min_nev.h
        min_nev/min_nev.cpp)
