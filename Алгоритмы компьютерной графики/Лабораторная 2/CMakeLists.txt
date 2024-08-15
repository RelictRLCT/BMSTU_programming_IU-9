cmake_minimum_required(VERSION 3.27)
project(LABA2)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA2 main.cpp)

target_link_libraries(LABA2 GL)
target_link_libraries(LABA2 glut)
target_link_libraries(LABA2 glfw)
