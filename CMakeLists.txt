cmake_minimum_required(VERSION 3.13)

project(mpilib20 VERSION 0.1.0
                 DESCRIPTION "Modern API and bindings for MPI"
                 LANGUAGES Fortran)

# Name of library
set(TARGET mpilib20)

# Defines build type 
include(cmake/StandardProjectSettings.cmake)

# INTERFACE => Has not source files and generates no object. 
# Used to set compile-time options
add_library(project_warnings INTERFACE)

# standard compiler warnings
include(cmake/CompilerWarnings.cmake)
set_project_warnings(project_warnings)

# Find MPI. Assigned to LIBS
include(cmake/mpi.cmake)

# TODO(Alex) Add Doxygen and Ford support
# Add Ctest support
# Add unit testing support 

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_Fortran_BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

# Ignore CMakeLists in src for now
#add_subdirectory(src)

set(SOURCE_CODE
   src/mpilib20.F90
   src/mpilib20_init_finalise.F90
)

# Can't find library at linking stage if shared
add_library(${TARGET} ${SOURCE_CODE})

set_target_properties(${TARGET} PROPERTIES LINK_FLAGS "${project_warnings}")
target_link_libraries(${TARGET} ${LIBS})

# TODO(Alex) Add installation 
#set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})


