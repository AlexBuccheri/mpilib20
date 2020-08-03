cmake_minimum_required(VERSION 3.13)

project(mpilib20 VERSION 0.1.0
                 DESCRIPTION "Modern API and bindings for MPI"
                 LANGUAGES Fortran)

# Name of library
set(TARGET mpilib20)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# Defines build type 
include(cmake/StandardProjectSettings.cmake)

# Compiler flags and warnings  
include(cmake/CompilerWarnings.cmake)

# Find MPI. Assigned to LIBS
include(cmake/mpi.cmake)

# TODO(Alex) Add Doxygen and Ford support
# Add Ctest support
# Add unit testing support 

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_Fortran_BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_Fortran_LIB_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

add_subdirectory(src)

add_library(${TARGET} SHARED ${SOURCE_CODE})
add_library(${TARGET}-static STATIC ${SOURCE_CODE})

set_target_properties(${TARGET}
                      PROPERTIES
		      VERSION 1 SOVERSION 0
		      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_Fortran_LIB_DIRECTORY})
target_link_libraries(${TARGET} ${LIBS})

set_target_properties(${TARGET}-static
                      PROPERTIES
		      VERSION 1
		      ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_Fortran_LIB_DIRECTORY})
target_link_libraries(${TARGET}-static ${LIBS})


# TODO(Alex) Add installation 
#set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

