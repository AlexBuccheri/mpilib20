# Set pre-processing variables for shared and static libs

if(CMAKE_BUILD_TYPE MATCHES Debug)
    # Preprocessor variable "DEBUG" in the code
    set_property(TARGET ${TARGET} APPEND PROPERTY
            COMPILE_DEFINITIONS "DEBUG")

    set_property(TARGET ${TARGET}-static APPEND PROPERTY
            COMPILE_DEFINITIONS "DEBUG")
endif()

# Provides an option for the user to select as ON or OFF.
#If <variable> is  already set as a normal or cache variable, then the command does nothing
option(MPI08 "Use mpi_f08 bindings" ON)

if(MPI08)
    # Preprocessor variable "MPI08" in the code
    set_property(TARGET ${TARGET} APPEND PROPERTY
            COMPILE_DEFINITIONS "MPI08")

    set_property(TARGET ${TARGET}-static APPEND PROPERTY
            COMPILE_DEFINITIONS "MPI08")
endif()
