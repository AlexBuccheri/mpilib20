include_directories("${CMAKE_SOURCE_DIR}/src")

set(DIR "src")

set(SOURCE_CODE
   ${DIR}/mpilib20.F90
   ${DIR}/mpilib20_init_finalise.F90
   PARENT_SCOPE
   )

