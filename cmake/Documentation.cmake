
# Set documentation parser. 
# 
# Valid options are FORD or DOXYGEN.
# Documentation gets built in ./documentation
# 
# Assumptions
#  * find_program assumes that ford and doyxgen are in the PATH. 
#  * Doxygen requires dot to generate a graph tree 
#
# TODOs
#  * Add ford dependency on graphviz (or dot)
#  * Might be better to refactor this to be a function, as 
#    in Jason Turner's example: https://github.com/lefticus/cpp_starter_project/tree/master/cmake
#  * One could extend finding by adding a Find<NAME>.cmake in the CMAKE_MODULE_PATH 
#    (in this project, cmake/), allowing find_package(Ford REQUIRED) to be used 

# option surprisingly also seems to take string options
# option(ENABLE_DOCS "Enable documentation" OFF)
set(ENABLE_DOCS "" CACHE STRING "Documentation parser")

if (${ENABLE_DOCS} STREQUAL "FORD")

   find_program(FORD ford REQUIRED)
   set(ford_output "documentation/ford")
   file(MAKE_DIRECTORY "${ford_output}")
   message("-- Ford documentation support enabled.")

   # Runs at compile time
   add_custom_target(docs
                     COMMAND ford -o ${ford_output} ${PROJECT_SOURCE_DIR}/project-description.md
		               )
 
elseif (${ENABLE_DOCS} STREQUAL "DOXYGEN")

   set(DOXYGEN_CALLER_GRAPH ON)
   set(DOXYGEN_CALL_GRAPH ON)
   set(DOXYGEN_EXTRACT_ALL ON)
   find_program(DOXYGEN doxygen REQUIRED dot)
   message("-- Doxygen documentation support enabled.")
   file(MAKE_DIRECTORY "documentation/doxy")

   # Runs at compile time
   add_custom_target(docs
                     COMMAND ${DOXYGEN} ${PROJECT_SOURCE_DIR}/Doxyfile
		                )

elseif (NOT ${ENABLE_DOCS} STREQUAL "")
   message(WARNING "--ENABLE_DOCS choice not recognised.")

endif ()
