!> Expose all mpi library data types and subroutines.
!> If the mpi_f08 bindings are not available, emulate its data types.  
!> Note, no support is provided for include mpi.h, which is considered
!> obsolete. 
! https://github.com/open-mpi/ompi/tree/master/ompi/mpi/fortran
module mpi_bindings

    ! Default mpi bindings 
#ifdef MPI08
        ! TODO MIght be easier to get rid of the use statement
        use mpi_f08, only:   &

            ! Routines
            MPI_INIT,        &
            MPI_INIT_THREAD, &
            MPI_COMM_RANK,   &
            MPI_COMM_DUP,    & 
            MPI_ABORT,       &
            MPI_FINALIZE,    &
            MPI_COMM_SIZE,   &
            MPI_GROUP_SIZE,  &

            ! Data types  
            MPI_COMM_WORLD,        &
            MPI_THREAD_SINGLE,     &
            MPI_THREAD_FUNNELED,   &
            MPI_THREAD_SERIALIZED, &
            MPI_THREAD_MULTIPLE,   &

            ! Derived types 
            MPI_Comm,          &
            MPI_Datatype,      &
            MPI_Errhandler,    &
            MPI_File,          &
            MPI_Group,         &
            MPI_Info,          &
            MPI_Message,       &
            MPI_Op,            &
            MPI_Request,       &
            MPI_Win,           &
            MPI_Status

        implicit none
        public 

    ! Support for older mpi bindings     
#else 
        ! TODO Might be easier to get rid of the use statement
        use mpi, only:  &

             ! Routines
             MPI_INIT,        &
             MPI_INIT_THREAD, &
             MPI_COMM_RANK,   &
             MPI_COMM_DUP,    & 
             MPI_ABORT,       &
             MPI_FINALIZE,    &
             MPI_COMM_SIZE,   &
             MPI_GROUP_SIZE,  &
     
             ! Data types  
             MPI_COMM_WORLD, &
             MPI_THREAD_SINGLE, &
             MPI_THREAD_FUNNELED, &
             MPI_THREAD_SERIALIZED, &
             MPI_THREAD_MULTIPLE
  
        implicit none
        public 

        ! Derived types that emulate mpi_f08 types
        ! See: https://github.com/hpc/cce-mpi-openmpi-1.7.1/blob/master/ompi/mpi/fortran/use-mpi-f08-desc/mpi-f08.f90
        
        ! Copy all data types from here
        ! https://github.com/hpc/cce-mpi-openmpi-1.7.1/blob/master/ompi/mpi/fortran/use-mpi-f08-desc/mpi-f08-types.f90

        ! TODO Document each type 
        type MPI_Comm
            integer :: VALUE
        end type 
        type :: MPI_Datatype
            integer :: MPI_VAL
        end type MPI_Datatype

        type :: MPI_Errhandler
            integer :: MPI_VAL
        end type MPI_Errhandler

        type :: MPI_File
           integer :: MPI_VAL
        end type MPI_File
     
        type:: MPI_Group
           integer :: MPI_VAL
        end type MPI_Group
     
        type :: MPI_Info
           integer :: MPI_VAL
        end type MPI_Info
     
        type :: MPI_Message
           integer :: MPI_VAL
        end type MPI_Message
     
        type :: MPI_Op
           integer :: MPI_VAL
        end type MPI_Op
     
        type :: MPI_Request
           integer :: MPI_VAL
        end type MPI_Request
     
        type :: MPI_Win
           integer :: MPI_VAL
        end type MPI_Win
     
        type :: MPI_Status
           integer :: MPI_SOURCE
           integer :: MPI_TAG
           integer :: MPI_ERROR
           ! TODO decide how to deal with these:
           !integer(C_INT),    private :: c_cancelled
           !integer(C_SIZE_T), private :: c_count
        end type MPI_Status

#endif 

end module