include_directories("${CMAKE_BINARY_DIR}/src")

set(SOURCE_CODE
   mpilib20.F90
   mpilib20_init_finalise.F90
)

add_library(${TARGET} ${SOURCE_CODE})
