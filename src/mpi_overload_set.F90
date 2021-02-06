!> If mpi_f08 is not available this module provides a set of wrappers
!> to the mpi API such that this library can always use strong-typing
!> throughout. In that respect this module emulates what mpi_f08 does

! TODO(ALEX) Give it a better name than overload_set
module mpi_overload_set

#ifdef fortran2008
! todo(alex) In principle, could just remove the only statement 
    use mpi_f08, only: &
        ! types 
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
        MPI_Status,        &
        ! routines 
        mpi_init,          &
        mpi_comm_dup,      &
        ! variables
        MPI_COMM_WORLD

    implicit none
    public 

#else 
    implicit none
    private
    
    !! Dummy types using the mpi_f08 type naming conventions 
    !! but without the C bindings
    ! See: https://github.com/hpc/cce-mpi-openmpi-1.7.1/blob/master/ompi/mpi/fortran/use-mpi-f08-desc/mpi-f08.f90

    type, public :: MPI_comm
        integer :: MPI_VAL
    end type

    type MPI_Datatype
        integer :: MPI_VAL
    end type MPI_Datatype

    type, public :: MPI_Errhandler
        integer :: MPI_VAL
    end type MPI_Errhandler

    type, public :: MPI_File
        integer :: MPI_VAL
    end type MPI_File

    type, public :: MPI_Group
        integer :: MPI_VAL
    end type MPI_Group

    type, public :: MPI_Info
        integer :: MPI_VAL
    end type MPI_Info

    type, public :: MPI_Message
        integer :: MPI_VAL
    end type MPI_Message

    type, public :: MPI_Op
        integer :: MPI_VAL
    end type MPI_Op

    type, public :: MPI_Request
        integer :: MPI_VAL
    end type MPI_Request

    type, public :: MPI_Win
        integer :: MPI_VAL
    end type MPI_Win

    ! type, public :: MPI_Status
    !     integer :: MPI_SOURCE
    !     integer :: MPI_TAG
    !     integer :: MPI_ERROR
    !     integer(C_INT),    private :: c_cancelled
    !     integer(C_SIZE_T), private :: c_count
    ! end type MPI_Status

    !! Expose data 
    type(MPI_comm), public :: mpi_comm_world


    ! todo(alex) Probably need to define the interfaces here 
    public :: mpi_init
    public :: mpi_comm_dup



contains

    !! Subroutine wrappers providing strong-tpying to the f90 API 

    ! TODO(Alex) need to put them in interfaces
    subroutine MPI_INIT_overload(ierror)
        use mpi, only: mpi_init, mpi_comm_world_f90 => mpi_comm_world
        integer, intent(out) :: ierror

        call MPI_INIT(ierror)
        mpi_comm_world%MPI_VAL = mpi_comm_world_f90
        
    end subroutine

    ! TODO(Alex) need to put them in interfaces
    interface mpi_comm_dup
    subroutine MPI_COMM_DUP_overload(mpi_comm_world, new_comm)
        ! external mpi library without f08 API. Only use the routine
        ! being wrapped 
        use mpi, only: MPI_COMM_DUP

        !> Exisiting MPI communicator to duplicate
        type(MPI_comm), intent(in) :: mpi_comm_world
        !> Copy of mpi_comm_world
        type(MPI_comm), intent(out) :: new_comm
        integer :: ierror

        call MPI_COMM_DUP(mpi_comm_world%MPI_VAL, new_comm%MPI_VAL, ierror)

    end subroutine
    end interface


#endif

end module