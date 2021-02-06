!> Expose all mpi library data types and subroutines.
!> If the mpi_f08 bindings are not available, emulate its data types.  
!> Note, no support is provided for include mpi.h, which is considered
!> obsolete. 
! https://github.com/open-mpi/ompi/tree/master/ompi/mpi/fortran
module mpi_bindings

    ! Default mpi bindings 
    #ifdef MPI08 then
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
            MPI_COMM_WORLD, &
            MPI_THREAD_SINGLE
            MPI_THREAD_FUNNELED
            MPI_THREAD_SERIALIZED
            MPI_THREAD_MULTIPLE

            ! Derived types 
            MPI_comm,          &
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
             MPI_THREAD_SINGLE
             MPI_THREAD_FUNNELED
             MPI_THREAD_SERIALIZED
             MPI_THREAD_MULTIPLE
  
        implicit none
        public 

        ! Derived types that emulate mpi_f08 types
        ! See: https://github.com/hpc/cce-mpi-openmpi-1.7.1/blob/master/ompi/mpi/fortran/use-mpi-f08-desc/mpi-f08.f90

        ! TODO Document each type 
        type MPI_comm
            integer :: VALUE 
        end type 

        type MPI_Group
            ! CHECK ME
            integer :: VALUE 
        end type 

        ! TODO Add all derived types 

    #endif 

end module