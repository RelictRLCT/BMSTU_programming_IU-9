cmake_minimum_required(VERSION 3.27)
project(LABA5)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA5 main.cpp)

target_link_libraries(LABA5 GL)
target_link_libraries(LABA5 glut)
target_link_libraries(LABA5 glfw)
