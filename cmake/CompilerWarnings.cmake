
function(set_project_warnings project_name)
  option(WARNINGS_AS_ERRORS "Treat compiler warnings as error" FALSE)

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

   if (WARNINGS_AS_ERRORS)
     set(GCC_WARNINGS ${GCC_WARNINGS} -Werror)
   endif()

   # Add Intel support

   if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
     set(PROJECT_WARNINGS ${GCC_WARNINGS})
   else()
     message(WARNING "Compiler warnings not set")
   endif()    

   target_compile_options(${project_name} INTERFACE ${PROJECT_WARNINGS})
endfunction()