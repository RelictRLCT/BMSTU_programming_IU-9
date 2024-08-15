cmake_minimum_required(VERSION 3.27)
project(LABA8)

set(CMAKE_CXX_STANDARD 17)

add_executable(LABA8 main.cpp)

find_package(glm CONFIG REQUIRED)
target_link_libraries(glm::glm)

target_link_libraries(LABA8 GL)
target_link_libraries(LABA8 glut)
target_link_libraries(LABA8 GLEW)
target_link_libraries(LABA8 glfw)
