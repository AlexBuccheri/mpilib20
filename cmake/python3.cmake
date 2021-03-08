# Find python 3
# Reference: https://cmake.org/cmake/help/git-stage/module/FindPython3.html
# 
# TODOs
#   * Add `Development` to COMPONENTS

find_package(Python3 3.6 COMPONENTS Interpreter) 
if(Python3_FOUND)
    message("-- Python 3 interpreter version: " ${Python3_VERSION})
else()
    message("-- Python 3 interpreter not found")
endif()