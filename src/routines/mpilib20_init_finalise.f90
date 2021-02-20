!>  Provides an MPI environment type and type-bound methods that
!>  wrap  MPI_initalise and MPI_finalise. 
!>  Also provides mpilib2020 API for MPI_initalise and MPI_finalise
!>  to be used as free subroutines.
module mpilib20_init_finalise
  use, intrinsic :: iso_fortran_env, only: error_unit
  use mpi_bindings, only : MPI_Comm, MPI_Group, MPI_INIT, MPI_COMM_DUP, &
                           MPI_INIT_THREAD, MPI_COMM_RANK, MPI_ABORT, MPI_COMM_DUP, &
                           MPI_COMM_SIZE, MPI_GROUP_SIZE, MPI_FINALIZE, MPI_THREAD_SINGLE, &
                           MPI_THREAD_FUNNELED, MPI_THREAD_SERIALIZED,  MPI_THREAD_MULTIPLE    
  implicit none
  private 

  !> MPI environment type
  type, public :: mpi_env_type
     !private  TODO(Alex) Make data private. Add getters and setters 
       
     integer,public        :: root_id      !! Process designated as master/root 
     type(MPI_Comm), public  :: comm         !! MPI communicator (integer in older bindings)
     integer, public        :: process      !! Process id (rank)
     integer,public         :: n_processes  !! Total number of processes
     type(MPI_Group), public :: group        !! Group id (integer in older bindings)
     integer, public         :: group_size   !! Number of processes in group
     integer, public         :: ierror       !! Error code

   contains
    private
    ! Public routines 
    procedure, public :: init     => init_mpi_env         !! Initialise instance of MPI environment object
    procedure, public :: finalize => finalise_mpi_env     !! Terminate instance of MPI environment object
    procedure, public :: get_comm => get_communicator     !! Get the communicator 
    ! Private routines
    !procedure  :: set_comm  !! Set the communicator... may not need
  end type mpi_env_type

  !> Free subroutines 
  public :: mpilib20_init, mpilib20_init_thread, mpilib20_finalize
  
contains

  !> Get communicator 
  !> 
  !> If using pre-2008 mpi bindings, return the integer not
  !> the MPI_Comm type. 
  function get_communicator(this) result(comm)
    class(mpi_env_type), intent(inout) :: this
#ifdef MPI08
    type(MPI_Comm) :: comm 
    comm = this%comm
#else 
    integer :: comm 
    comm = this%comm%VALUE
#endif 
  end function

  !--------------------------------------------------------
  ! Free subroutines 
  ! These are used by the type-bound procedures.
  ! Their API requires mpi_env_type, hence they must 
  ! be present in this file to avoid circular dependency.
  !--------------------------------------------------------

  !> Initialise MPI execution environment.
  subroutine mpilib20_init(mpi_env)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env

    call MPI_INIT(mpi_env%ierror)
    !TODO WILL need set comm to work with use mpi OR preprocess this whole block
    call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm, mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,   mpi_env%process ,    mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,   mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,  mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group, mpi_env%group_size,  mpi_env%ierror)
    mpi_env%root_id = 0
  end subroutine mpilib20_init


  !> Initialises the MPI execution environment for use with hybrid MPI/threaded applications.
  !>
  !> MPI_COMM_WORLD is duplicated.
  !>
  !> Threading options 
  !> MPI_THREAD_SINGLE:     Only one thread will execute. Equivalent to calling MPI_init(ierror)
  !> MPI_THREAD_FUNNELED:   If the process is multithreaded, only the thread that called
  !>                        MPI_Init_thread will make MPI calls.
  !> MPI_THREAD_SERIALIZED: If the process is multithreaded, only one thread will make MPI library
  !>                        calls at one time.
  !> MPI_THREAD_MULTIPLE:   If the process is multithreaded, multiple threads may call MPI at once
  !>                        with no restrictions.
  !
  subroutine mpilib20_init_thread(mpi_env, required)
    use mpi_bindings, only: MPI_COMM_WORLD

    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    !> Required level of threading support 
    integer,            intent(in)    :: required
    !> Error code returned to the MPI environment (arbitrary)
    integer, parameter :: errorcode = 0
    !> Threading options 
    integer, parameter, dimension(4) :: thread_options = [MPI_THREAD_SINGLE,     &
                                                          MPI_THREAD_FUNNELED,   &
                                                          MPI_THREAD_SERIALIZED, &
                                                          MPI_THREAD_MULTIPLE]
    !> Level of available (provided) threading support 
    integer :: available
    integer :: process
    integer :: ierror

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif

    call MPI_INIT_THREAD(required, available, ierror)

    if(available < required) then 
       call MPI_COMM_RANK(MPI_COMM_WORLD, process, ierror)
       if(process == mpi_env%root_id)then
          write(error_unit,'(1x,a,I1,a,I1)') 'error - threading support available, ',&
               available, ', is less than is required, ', required
          call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierror)
       endif
    endif

    call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm, mpi_env%ierror)
    call MPI_COMM_RANK(mpi_env%comm,   mpi_env%process ,    mpi_env%ierror)
    call MPI_COMM_SIZE(mpi_env%comm,   mpi_env%n_processes, mpi_env%ierror)
    call MPI_COMM_GROUP(mpi_env%comm,  mpi_env%group,       mpi_env%ierror)
    call MPI_GROUP_SIZE(mpi_env%group, mpi_env%group_size,  mpi_env%ierror)

  end subroutine mpilib20_init_thread


  !> Terminates MPI execution environment.
  subroutine mpilib20_finalize(mpi_env)
    !> Instance of the MPI environment
    type(mpi_env_type), intent(inout) :: mpi_env
    call MPI_FINALIZE(mpi_env%ierror)
  end subroutine mpilib20_finalize


  !---------------
  ! Methods
  !---------------

  !> Initialise an instance of the MPI environment.
  subroutine init_mpi_env(self, required_threading)
    !> An instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self 
    !> Required level of threading support
    integer, optional,   intent(in)    :: required_threading

    if(present(required_threading)) then
       call mpilib20_init_thread(self, required_threading)
    else
       call mpilib20_init(self)
    endif

  end subroutine init_mpi_env

  !> Terminates MPI execution environment. 
  subroutine finalise_mpi_env(self)
    !> Instance of the MPI environment
    class(mpi_env_type), intent(inout) :: self
    call mpilib20_finalize(self)    
  end subroutine finalise_mpi_env


end module mpilib20_init_finalise