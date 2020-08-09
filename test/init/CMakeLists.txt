cmake_minimum_required(VERSION 3.13)
project(lib_tester  VERSION 0.1  LANGUAGES Fortran)

# Library root
set(ROOT "/Users/alexanderbuccheri/Codes/mpilib20/build")

# Directories used to search for include (module) files
include_directories(${ROOT}/modules)

# Add directories in which the linker will look for libraries.
link_directories(${ROOT})

add_executable(test.exe test_init.f90)
target_link_libraries(test.exe ${LIBS} libmpilib20.1.dylib)


