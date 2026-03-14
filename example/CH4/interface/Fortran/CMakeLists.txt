cmake_minimum_required(VERSION 3.10)

project(CH4-PIP-NN)

set(CMAKE_BUILD_TYPE "Release")

enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "-Ofast -march=native -static-intel -qmkl=sequential ${CMAKE_Fortran_FLAGS}")

# source files
file(GLOB SRC_FILES
    ${PROJECT_SOURCE_DIR}/src/*.f90
)

add_executable(${PROJECT_NAME} ${SRC_FILES})
