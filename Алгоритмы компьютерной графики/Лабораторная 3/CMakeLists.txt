cmake_minimum_required(VERSION 3.27)
project(LABA3)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA3 main.cpp)

target_link_libraries(LABA3 GL)
target_link_libraries(LABA3 glut)
target_link_libraries(LABA3 glfw)
