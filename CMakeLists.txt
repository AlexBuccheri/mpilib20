cmake_minimum_required(VERSION 3.13)

project(mpilib20 VERSION 0.1.0
                 DESCRIPTION "Modern API and bindings for MPI"
		 LANGUAGES Fortran)

set(PROJECT_URL "https://github.com/AlexBuccheri/mpilib20")

# Name of library
set(TARGET mpilib20)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# Defines build type 
include(cmake/StandardProjectSettings.cmake)

# Compiler flags 
include(cmake/CompilerFlags.cmake)

# Compiler warnings  
include(cmake/CompilerWarnings.cmake)

# Find MPI. Assigned to LIBS
include(cmake/mpi.cmake)

# TODO(Alex)
# Add Doxygen and Ford support
# Add Ctest support
# Add unit testing support 
# Create pkg-config 

# Set output folders for exe, libs and modules
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_Fortran_BIN_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_Fortran_LIB_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(default_install_prefix "${PROJECT_BINARY_DIR}/mpilib20_library")


# Create static and shared libraries 
add_subdirectory(src)
add_library(${TARGET} SHARED ${SOURCE_CODE})
add_library(${TARGET}-static STATIC ${SOURCE_CODE})

# TODO(Alex) Make VERSION and SOVERSION variables 
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


# Install libraries and modules
# Not sure when this is required: include(GNUInstallDirs)

set(CMAKE_INSTALL_PREFIX ${default_install_prefix} CACHE STRING
    "Choose the installation directory; by default it installs in ${default_install_prefix}."
     FORCE)

# No need to specify "${CMAKE_INSTALL_PREFIX}/lib", 
# install() will concatenate CMAKE_INSTALL_PREFIX and lib 
install(TARGETS ${TARGET} ${TARGET}-static
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

# If one specifies "${CMAKE_INSTALL_PREFIX}/include" instead of include
# the module install directory will erroneously be CMAKE_INSTALL_PREFIX/include/modules 
# No need to use GLOB 
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
        DESTINATION include)







