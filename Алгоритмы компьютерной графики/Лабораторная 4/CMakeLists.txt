cmake_minimum_required(VERSION 3.27)
project(LABA4)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA4 main.cpp)

target_link_libraries(LABA4 GL)
target_link_libraries(LABA4 glut)
target_link_libraries(LABA4 glfw)
