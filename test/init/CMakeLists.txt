cmake_minimum_required(VERSION 3.13)
project(lib_tester  VERSION 0.1  LANGUAGES Fortran)

# In the absence of pkg-config, hard-code locations

# Library root
set(ROOT "/Users/alexanderbuccheri/Codes/mpilib20/build/mpilib20_library")

# Directories used to search for include (module) files
include_directories(${ROOT}/include/modules)

# Add directories in which the linker will look for libraries.
link_directories(${ROOT}/lib)

add_executable(test.exe test_init.f90)
target_link_libraries(test.exe ${LIBS} libmpilib20.1.dylib)


