# Set pre-processing variables for the library 

if(CMAKE_BUILD_TYPE MATCHES Debug)
    # Preprocessor variable "USE_ASSERT" in the code
    set_property(TARGET mpilib20 APPEND PROPERTY
            COMPILE_DEFINITIONS "USE_ASSERT")

    set_property(TARGET mpilib20-static APPEND PROPERTY
            COMPILE_DEFINITIONS "USE_ASSERT")
endif()

# Provides an option for the user to select as ON or OFF.
#If <variable> is  already set as a normal or cache variable, then the command does nothing
option(MPI08 "Use mpi_f08 bindings" ON)

# TODO(ALEX) Does one need to set COMPILE_DEFINITIONS? 
if(MPI08)
    # Preprocessor variable "MPI08" in the code
    set_property(TARGET mpilib20 
                 APPEND PROPERTY
                 COMPILE_DEFINITIONS "MPI08")

    if(BUILD_STATIC)
         set_property(TARGET mpilib20-static 
                      APPEND PROPERTY
                      COMPILE_DEFINITIONS "MPI08")
    endif()
endif()
