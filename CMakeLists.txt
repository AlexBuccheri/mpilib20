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

# Python 3, required for generating Zofu test drivers
include(cmake/python3.cmake) 

# Zofu unit testing framework
include(cmake/FindZofu.cmake)

# Ford or doxygen documentation 
include(cmake/include(Documentation))

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
install(TARGETs mpilib20 LIBRARY DESTINATION lib)

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
enable_testing()

# Functions for generating unit test executables
include(cmake/unit_test_functions.cmake)

# A list containing all unit test modules within a given subdirectory
# TODO(Alex) Maybe in this case we actually do want a driver per test module
# as all (or many) test modules are likely to be in the same subdirectory. 
list(APPEND routines_unit_tests "mpilib20_init_finalise_tests.f90")

# Arguments are a) Tell Zofu the unit test is MPI, b)the relative unit test location SUBDIR 
# and c) a list of UNIT_TESTS
create_unit_test_executable(ENABLE_MPI SUBDIR "routines" UNIT_TESTS ${routines_unit_tests} )

