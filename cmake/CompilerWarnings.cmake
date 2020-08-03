# Add compiler warnings to project 
# TODO(Alex) Add description for each GNU warning 

  # See: https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html
  set(GCC_WARNINGS
      -Wall  # Contains aliasing, apersand, conversion, surprising, binding-type,
             # intrinsics-std, tabs, intrinsic-shadow, line-truncation,
	     # target-lifetime, integer-division, real-q-constant, unused and undefined-do-loop
      -Warray-bounds  
      -Wcharacter-truncation 
      -Wfunction-elimination # Warn if any calls to impure functions are eliminated by the optimizations
                             # enabled by the -ffrontend-optimize option
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

   # See https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-developer-guide-and-reference/top/compiler-reference/compiler-options/compiler-option-details/compiler-diagnostic-options/warn.html
   set(INTEL_WARNINGS
       -warn all     # Enables all warning messages except errors and stderrors
      )

   option(WARNINGS_AS_ERRORS "Treat compiler warnings as error" FALSE)
   if (WARNINGS_AS_ERRORS)
     set(GCC_WARNINGS ${GCC_WARNINGS} -Werror)
     set(INTEL_WARNINGS ${INTEL_WARNINGS} "-warn errors") 
   endif()

   # List to string 
   string(REPLACE ";" " " GCC_WARNINGS "${GCC_WARNINGS}")
   string(REPLACE ";" " " INTEL_WARNINGS "${INTEL_WARNINGS}")

   if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
      set(COMPILER_WARNINGS ${GCC_WARNINGS})
   elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
      set(COMPILER_WARNINGS ${INTEL_WARNINGS})
   else ()
     message(SEND_ERROR "Warning flags have not been defined for this compiler: \
            ${CMAKE_Fortran_COMPILER_ID}")
   endif()    

   set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${COMPILER_WARNINGS}")
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${COMPILER_WARNINGS}")
   set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${COMPILER_WARNINGS}")
