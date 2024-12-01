cmake_minimum_required(VERSION 3.10)
project(Lab3)

set(CMAKE_CXX_STANDARD 17)

add_executable(Lab3 main.cpp
        systems/systems_creation.h
        systems/systems_creation.cpp
        print/print.h
        print/print.cpp
        multiplication/multiplication.h
        multiplication/multiplication.cpp
        min_nev/min_nev.h
        min_nev/min_nev.cpp)

find_package(OpenMP REQUIRED)
if(OpenMP_CXX_FOUND)
    target_link_libraries(Lab3 PUBLIC OpenMP::OpenMP_CXX)
endif()