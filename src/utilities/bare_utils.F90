! Shouldn't need this. Handled by the get method of mpi_env_type

! !> Utility routines for bare types.
! !> Takes an input type and returns the type required by the bare MPI routines. 
! !> Return type therefore depends on whether the mpi f90 or mpi f08 bindings are
! !> being used and requires processing. 
! !
! !> Preprocess on the return type, as this is the one that will be used
! !> by the bare MPI routines. 

! module bare_utils
!     use mpi_bindings, only: MPI_Comm
!     implicit none
!     private 

!     interface get
!         module procedure :: get_comm_inputf89, get_comm_inputf08
!     end interface

!     public :: get

! contains

!     !> Given an integer communicator return a communicator of 
!     !> internal type, type(MPI_Comm),  used by this library
!     function get_comm_inputf90(input_comm) result(new_comm)
!         integer, intent(in) :: input_comm
! #ifdef MPI08
!         ! integer -> MPI_Comm 
!         type(MPI_Comm) :: new_comm
!         new_comm%MPI_VAL = old_comm
! #else 
!         ! integer to integer (do nothing)
!         integer :: new_comm
!         new_comm = old_comm   
! #endif 
!     end function 
    
    
!     !> Given a type(MPI_Comm) communicator return a communicator of 
!     !> internal type, type(MPI_Comm),  used by this library
!     function get_comm_inputf08(input_comm) result(new_comm)
!         type(MPI_Comm), intent(in) :: input_comm
! #ifdef MPI08
!         ! MPI_Comm -> MPI_Comm (do nothing)
!         type(MPI_Comm) :: new_comm
!         new_comm = input_comm
! #else 
!         !TODO Throw error with message, as even if the mpi f90 bindings
!         ! are used for the bare MPI calls, we mock type(MPI_Comm)
!         ! for internal use. 
!         stop 

!         ! MPI_Comm -> integer 
!         !integer :: new_comm 
!         !new_comm = old_comm%MPI_VAL 
! #endif 
!     end function 
    

! end module