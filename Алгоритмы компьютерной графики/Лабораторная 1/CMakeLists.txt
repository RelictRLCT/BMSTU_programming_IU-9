cmake_minimum_required(VERSION 3.27)
project(LABA1)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA1 main.cpp)

target_link_libraries(LABA1 GL)
target_link_libraries(LABA1 glut)
target_link_libraries(LABA1 glfw)
