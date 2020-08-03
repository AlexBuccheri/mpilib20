# Add compiler flags to the project
# Include this in the main cmake file 

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
   # Debug
   # Release
   # ReleaseWithDebug

elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
   # Debug
   # Release
   # ReleaseWithDebug

else ()
     message(SEND_ERROR "Warning flags have not been defined for this compiler: \
            ${CMAKE_Fortran_COMPILER_ID}")
endif()


# Sort me
#set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
#set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")

