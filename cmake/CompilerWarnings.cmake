

  # See: https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html
  # TODO(Alex) Add description for each
  set(GCC_WARNINGS
      -Wall  # Contains aliasing, apersand, conversion, surprising, binding-type,
             # intrinsics-std, tabs, intrinsic-shadow, line-truncation,
	     # target-lifetime, integer-division, real-q-constant, unused and undefined-do-loop
      -Warray-bounds  
      -Wcharacter-truncation 
      -Wfunction-elimination # Warn if any calls to impure functions are eliminated by the optimizations enabled by the -ffrontend-optimize option
      -Wimplicit-interface  
      -Wimplicit-procedure 
      -Wuse-without-only  
      -Wno-align-commons  
      -Wno-overwrite-recursive
      -Wunderflow 
      -Wrealloc-lhs 
      -Wrealloc-lhs-all 
      -Wfrontend-loop-interchange
      -pedantic)

  #TODO(Alex) Set Intel warnings

   option(WARNINGS_AS_ERRORS "Treat compiler warnings as error" FALSE)
   if (WARNINGS_AS_ERRORS)
     set(GCC_WARNINGS ${GCC_WARNINGS} -Werror)
   endif()

   string(REPLACE ";" " " GCC_WARNINGS "${GCC_WARNINGS}")

   # TODO(Alex) Add profiling, debug/bounds checks, symbols, etc 
   if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${GCC_WARNINGS}")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${GCC_WARNINGS}")
	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${GCC_WARNINGS}")

   #TODO(Alex) Add Intel support
   else()
     message(WARNING "Compiler warnings not set")
   endif()    

