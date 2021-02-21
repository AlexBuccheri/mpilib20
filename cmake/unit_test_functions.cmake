# A Buccheri 2021. alexanderbuccheri@googlemail.com 

# Create a unit test executable with the name `test_SUBDIR`. 
# For example, all tests in the directory src/maths would be
# run by an executable `test_maths`
#
# Variables accessed globally:
#   'mpilib20' library name under testing
#   ${ZOFU_DRIVER}
#   ${ZOFU}
#
# Assumptions 
#   Assumes the fortran ZOFU unit test framework
#   Source code subdirectories are located in ${CMAKE_SOURCE_DIR}/src/
#
# TODOs
#   Distinguish between test types i.e. [FAST, SLOW]
#   Remove global scope (if possible) 
#   Add check to ensure ZOFU library has been found

# function(create_unit_test_executable)

#     # Define the CMake function signature keywords and their types
#     # Of the general form: set(type KEYWORD)
#     set(options ENABLE_MPI)                        # Binary options
#     set(oneValueArgs SUBDIR)                       # Single-value options
#     set(multiValueArgs UNIT_TESTS)                 # Multi-value options: Multiple arguments or list/s
#     cmake_parse_arguments(FUNC                     # Prefix for all function arguments within function body
#             "${options}"                           # Assign the binary options for the function
#             "${oneValueArgs}"                      # Assign the single-value options for the function
#             "${multiValueArgs}"                    # Assign the multi-value options for the function
#             ${ARGV})                               # ${ARGN} or ${ARGV}. (I think) ${ARGV} means accept a variable
#                                                    # number of arguments, which one wants for a list of no fixed size

#     # Prepend the UNIT_TESTS list with their full file path
#     list(TRANSFORM FUNC_UNIT_TESTS PREPEND "${CMAKE_SOURCE_DIR}/src/${FUNC_SUBDIR}/")

#     # Runs the unix command specified by COMMAND:
#     # Create a unit test driver that runs all tests in the respective subdirectory
#     # using ${ZOFU_DRIVER} (which must be my custom script, not the binary supplied with the library)
#     IF(NOT ${FUNC_ENABLE_MPI})
#         add_custom_command(
#                 OUTPUT ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90
#                 COMMAND ${ZOFU_DRIVER} "-mod" ${FUNC_UNIT_TESTS} "-driver" ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90
#                 COMMENT "Generating ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90")
#     ELSE()
#         add_custom_command(
#                 OUTPUT ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90
#                 COMMAND ${ZOFU_DRIVER} "-mpi -mod" ${FUNC_UNIT_TESTS} "-driver" ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90
#                 COMMENT "Generating ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90")
#     ENDIF()

#     # Create the test driver executable and add module targets:
#     # all unit test modules and the test driver
#     add_executable(test_${FUNC_SUBDIR})

#     # Set directory in which unit tests are built in
#     set_target_properties(test_${FUNC_SUBDIR}
#             PROPERTIES
#             RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/unit_tests"
#             )

#     target_sources(test_${FUNC_SUBDIR}
#             PRIVATE
#             ${FUNC_UNIT_TESTS}
#             ${TEST_DRIVER_DIR}/${FUNC_SUBDIR}_driver.f90
#             )
#     # Ensure required library gets compiled if one attempts to build the unit test executable
#     add_dependencies(test_${FUNC_SUBDIR} mpilib20)

#     # Link the libraries that the unit test executable will dependent on
#     # We assume that ZOFU is built and found by CMake at this point
#     target_link_libraries(test_${FUNC_SUBDIR} ${ZOFU} mpilib20)

#     # Allows test executable `test_${FUNC_SUBDIR}` to be run with ctest
#     # All unit tests assumed to be FAST, hence no specific CONFIGURATIONs
#     add_test(NAME UNITTEST_${FUNC_SUBDIR}
#              COMMAND ${CMAKE_BINARY_DIR}/unit_tests/test_${FUNC_SUBDIR})

# endfunction()


# TODO(Alex) Document me 
function(create_unit_test_executable_per_module)

    set(options ENABLE_MPI)                        
    set(oneValueArgs SUBDIR)                      
    set(oneValueArgs UNIT_TEST)                 
    cmake_parse_arguments(FUNC                     
            "${options}"                          
            "${oneValueArgs}"                     
            "${multiValueArgs}"                  
            ${ARGV})                            
            
    # Get Test name by stripping extension
    get_filename_component(test_name ${FUNC_UNIT_TEST} NAME_WE)       
                    
    #TODO(Alex) FUNC_SUBDIR not passing!
    set(FUNC_SUBDIR "src/tests")


    # Prepend the UNIT_TESTS list with their full file path
    list(TRANSFORM FUNC_UNIT_TEST PREPEND "${CMAKE_SOURCE_DIR}/${FUNC_SUBDIR}/")
    message("FUNC_UNIT_TEST ${FUNC_UNIT_TEST}    ${FUNC_SUBDIR}")

    # Runs the unix command specified by COMMAND:
    # Create a unit test driver that runs all tests in the respective subdirectory using ${ZOFU_DRIVER}
    IF(NOT ${FUNC_ENABLE_MPI})
        add_custom_command(
                OUTPUT ${TEST_DRIVER_DIR}/${test_name}_driver.f90
                COMMAND ${ZOFU_DRIVER} ${FUNC_UNIT_TEST} ${TEST_DRIVER_DIR}/${test_name}_driver.f90
                COMMENT "Generating ${TEST_DRIVER_DIR}/${test_name}_driver.f90")
    ELSE()
        add_custom_command(
                OUTPUT ${TEST_DRIVER_DIR}/${test_name}_driver.f90
                COMMAND ${ZOFU_DRIVER} ${FUNC_UNIT_TEST} ${TEST_DRIVER_DIR}/${test_name}_driver.f90 --mpi
                COMMENT "Generating MPI ${TEST_DRIVER_DIR}/${test_name}_driver.f90")
    ENDIF()

    # Create the test driver executable
    add_executable(test_${test_name})

    # Set directory in which unit test binaries are built in
    set_target_properties(test_${test_name}
            PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/unit_tests"
            )

    target_sources(test_${test_name}
            PRIVATE
            ${FUNC_UNIT_TEST}
            ${TEST_DRIVER_DIR}/${test_name}_driver.f90
            )
    # Ensure required library gets compiled if one attempts to build the unit test executable
    add_dependencies(test_${test_name} mpilib20)

    # Link the libraries that the unit test executable will dependent on

    # TODO (Alex) Check Zofu is found at this point 
    target_link_libraries(test_${test_name} ${ZOFU} mpilib20)

    # Allows test executable `test_${test_name}` to be run with ctest
    # All unit tests assumed to be FAST, hence no specific CONFIGURATIONs
    add_test(NAME UNITTEST_${test_name}
             COMMAND ${CMAKE_BINARY_DIR}/unit_tests/test_${test_name})

endfunction()


#TODO Alex Turn into a function. One test binary per program 
# foreach(unit_test ${unit_tests})

#     # Strip directory and file extension
#     get_filename_component(test_name ${unit_test} NAME_WE)
#     message("${test_name}")

#     # Binary
#     add_executable(test_${test_name})   

#     # Set directory in which unit test is built
#     set_target_properties(test_${test_name}
#             PROPERTIES
#             RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/unit_tests"
#             )

#     # Attach source file
#     target_sources(test_${test_name}
#             PRIVATE
#             ${PROJECT_SOURCE_DIR}/${unit_test}
#             )

#     # Ensure required library gets compiled if one attempts to build the unit test executable
#     add_dependencies(test_${test_name} mpilib20)

#     # Link the libraries 
#     # TODO(Alex) Remove ${LIBS} and use mpi from the test case.
#     target_link_libraries(test_${test_name} ${ZOFU} mpilib20 ${LIBS})

#     # Allows test executable to be run with ctest
#     # TODO(Alex) Add args so tests run with multiple processes 
#     add_test(NAME UNITTEST_${test_name}
#              COMMAND ${CMAKE_BINARY_DIR}/unit_tests/test_${test_name})

# endforeach()