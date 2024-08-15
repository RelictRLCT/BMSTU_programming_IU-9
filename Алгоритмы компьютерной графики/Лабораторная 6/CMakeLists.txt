cmake_minimum_required(VERSION 3.27)
project(LABA6)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA6 main.cpp)

target_link_libraries(LABA6 GL)
target_link_libraries(LABA6 glut)
target_link_libraries(LABA6 glfw)