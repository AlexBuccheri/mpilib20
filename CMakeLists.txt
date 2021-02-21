cmake_minimum_required(VERSION 3.13)

project(mpilib20 VERSION 0.1.0
                 DESCRIPTION "Modern API and bindings for MPI. A Buccheri and M Williams"
		             LANGUAGES Fortran)

set(PROJECT_URL "https://github.com/AlexBuccheri/mpilib20")

# CMake module directory 
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# Defines build type 
include(cmake/StandardProjectSettings.cmake)

# Compiler flags 
include(cmake/CompilerFlags.cmake)

# Compiler warnings  
include(cmake/CompilerWarnings.cmake)

# --------------------------
# External dependencies 
# --------------------------
set(LIBS "")

# Find MPI
include(cmake/mpi.cmake)

# TODO Add Find openMP implementation in omp.cmake
include(cmake/omp.cmake)

# Python 3, required for generating Zofu test drivers
# We may not use that though 
include(cmake/python3.cmake) 

# Zofu unit testing framework
include(cmake/FindZofu.cmake)

# Ford or doxygen documentation parsers 
include(cmake/Documentation.cmake)

# --------------------------
# Library build 
# --------------------------
# Set output folders for exe, libs and modules
set(CMAKE_Fortran_BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_Fortran_LIB_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

add_subdirectory(src)

# Shared library 
add_library(mpilib20 SHARED ${SOURCE_CODE})
set_target_properties(mpilib20
                      PROPERTIES
		                  VERSION 1 SOVERSION 0
		                  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_Fortran_LIB_DIRECTORY})
target_link_libraries(mpilib20 ${LIBS})

# Optional static library 
option(BUILD_STATIC "Build static version of the library" OFF)

if(BUILD_STATIC)
    add_library(mpilib20-static STATIC ${SOURCE_CODE})
    set_target_properties(mpilib20-static
                          PROPERTIES
		                      VERSION 1
		                      ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_Fortran_LIB_DIRECTORY})
    target_link_libraries(mpilib20-static ${LIBS})
endif()

# TODO(Alex)
# Set fortran preprocessor variables specific to the library target. 
# This should just be done explicitly here so there's no question of doing it 
# before the target exists
# Should also considrer if the library build commands should be put into a module 
# - question of how modular CMake should be 
include(cmake/PreprocessorSettings.cmake)

#TODO 
if(NOT MPI08)
 message(FATAL_ERROR "Library will not run with MPI08=off because getters and setters need to be used in the existing routines")
endif()

# --------------------------
# Library installation
# --------------------------
set(default_install_prefix ${PROJECT_BINARY_DIR}/mpilib20_library)

# Install libraries and modules
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
   set(CMAKE_INSTALL_PREFIX ${default_install_prefix} CACHE STRING
       "Choose the installation directory; by default it installs in ${default_install_prefix}."
        FORCE)
endif()

# ${CMAKE_INSTALL_PREFIX} is implict in install DESTINATION lib 
install(TARGETS mpilib20 LIBRARY DESTINATION lib)

if(BUILD_STATIC) 
  install(TARGETS mpilib20-static ARCHIVE DESTINATION lib)
endif()

# Module installation location 
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
        DESTINATION include)

# TODO(Alex)
# Create pkg-config 

# --------------------------
# Unit tests 
# --------------------------

#TODO(Alex) How does one use ctest with MPI applications? 
# add test needs arguments: https://stackoverflow.com/questions/19278510/cmake-ctest-add-test-command-appends-extra-quotes-to-my-command-parameters

# TODO(Alex) Investigate this
# https://cmake.org/cmake/help/latest/command/ctest_test.html#command:ctest_test

enable_testing()

# List of test drivers 
# Each test comprises of program and a module in the same .f90 file
set(unit_tests 
    src/tests/test_init.f90
   # src/tests/test_init_thread.f90
   )

foreach(unit_test ${unit_tests})

    # Strip directory and file extension
    get_filename_component(test_name ${unit_test} NAME_WE)
    message("${test_name}")

    # Binary
    add_executable(test_${test_name})   

    # Set directory in which unit test is built
    set_target_properties(test_${test_name}
            PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/unit_tests"
            )

    # Attach source file
    target_sources(test_${test_name}
            PRIVATE
            ${PROJECT_SOURCE_DIR}/${unit_test}
            )

    # Ensure required library gets compiled if one attempts to build the unit test executable
    add_dependencies(test_${test_name} mpilib20)

    # Link the libraries 
    target_link_libraries(test_${test_name} ${ZOFU} mpilib20 ${LIBS})

    # Allows test executable to be run with ctest
    # TODO(Alex) Add args so tests run with multiple processes 
    add_test(NAME UNITTEST_${test_name}
             COMMAND ${CMAKE_BINARY_DIR}/unit_tests/test_${test_name})

endforeach()
