include_directories("${CMAKE_SOURCE_DIR}/src")

set(SOURCE_CODE
   src/mpilib20.F90
   src/routines/mpilib20_init_finalise.F90
   src/routines/mpilib20_reduce.F90
   src/bindings/mpi_bindings.F90
   src/errors_warnings/asserts.F90 
   src/utilities/internal_utils.F90
   PARENT_SCOPE
   )
