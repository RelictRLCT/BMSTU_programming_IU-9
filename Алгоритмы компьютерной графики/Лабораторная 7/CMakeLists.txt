cmake_minimum_required(VERSION 3.27)
project(LABA7)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA7 main.cpp)

target_link_libraries(LABA7 GL)
target_link_libraries(LABA7 glut)
target_link_libraries(LABA7 glfw)