!  Provides an MPI environment type, and both wrappers and
!  methods for MPI_initalise and MPI_finalise
!
!  Include 'mpif.h'  ! Needed instead "Use mpi" for some machines                                                
!  Include 'mpiof.h' ! Needed for ScaliMPI   

module mpilib20_init_finalise
  use, intrinsic :: iso_fortran_env, only: error_unit
  use mpi_f08, only: MPI_comm, MPI_Group
  implicit none
  private 

  !> Process designated as master/root 
  integer, public, parameter :: root_id = 0
  
  !> MPI environment type
  type, public :: mpi_env_type
     type(MPI_comm)  :: comm         !> MPI communicator (integer in older bindings)
     integer         :: process      !> Process id (rank)
     integer         :: n_processes  !> Total number of processes
     type(MPI_Group) :: group        !> Group id
     integer         :: ierror       !> Error code   
     
   contains
     procedure :: init => init_mpi_env  !> Initialise instance of MPI environment object 
  end type mpi_env_type

  public :: mpilib20_init, mpilib20_init_thread, mpilib20_finalize
  
contains

  !--------------------------------
  ! Wrappers for free subroutines 
  !--------------------------------
  
  !> \brief Initialise MPI execution environment
  !>
  !> \param[input]  mpi_env  Instance of the MPI environment
  subroutine mpilib20_init(mpi_env)
    use mpi_f08, only: MPI_COMM_WORLD, MPI_INIT, MPI_COMM_DUP
    
    type(mpi_env_type), intent(inout) :: mpi_env

    call MPI_INIT(ierror)
    Call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm, mpi_env%ierror)

  end subroutine mpilib20_init

  
  !> \brief Initialises the MPI execution environment
  !>  for use with hybrid MPI/threaded applications 
  !> 
  !> \param[inout]  mpi_env   Instance of the MPI environment
  !> \param[in]     required  Required level of threading support (see below)
  !>
  !> Threading options (integers)
  !> MPI_THREAD_SINGLE:     Only one thread will execute. Equivalent to calling MPI_init(ierror)
  !> MPI_THREAD_FUNNELED:   If the process is multithreaded, only the thread that called
  !>                        MPI_Init_thread will make MPI calls.
  !> MPI_THREAD_SERIALIZED: If the process is multithreaded, only one thread will make MPI library
  !>                        calls at one time.
  !> MPI_THREAD_MULTIPLE:   If the process is multithreaded, multiple threads may call MPI at once
  !>                        with no restrictions.
  subroutine mpilib20_init_thread(mpi_env, required)
    use mpi_f08, only: MPI_COMM_WORLD, MPI_INIT_THREAD, MPI_COMM_RANK, MPI_ABORT, &
                       MPI_COMM_DUP
    
    type(mpi_env_type), intent(inout) :: mpi_env
    integer,            intent(in)    :: required

    !> Error code returned to the MPI environment (arbitrary)
    integer, parameter :: errorcode = 0
    integer            :: ierror, provided, process
    integer, dimension(4) :: thread_options = [MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED,  &
                                               MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE]

    if(.not. any(thread_options == required)) then
       write(error_unit,'(1x,a,I1)') 'required thread is not a valid choice:', required
    endif
    
    call MPI_INIT_THREAD(required, provided, ierror)
    
    if(provided < required) then 
       call MPI_COMM_RANK(MPI_COMM_WORLD, process, ierror)
       if(process == root_id)then
          write(error_unit,'(1x,a,I1,a,I1)') 'error - threading support available, ',&
               provided, ', is less than is required, ', required
          call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierror)
       endif
    endif

    call MPI_COMM_DUP(MPI_COMM_WORLD, mpi_env%comm, mpi_env%ierror)
    
  end subroutine mpilib20_init_thread

  
  !> \brief Terminates MPI execution environment.
  !>
  !> \param[inout]  mpi_env   Instance of the MPI environment
  subroutine mpilib20_finalize(mpi_env)
    use mpi_f08, only: MPI_FINALIZE
    type(mpi_env_type), intent(inout) :: mpi_env
    call MPI_FINALIZE(mpi_env%comm, mpi_env%ierror)
  end subroutine mpilib20_finalize

  
  !---------------
  ! Methods
  !---------------

  !> \brief Initialise an instance of the MPI environment
  !>
  !> param[inout]  self  An instance of the MPI environment
  !> param[in]     required_threading  Required level of threadig support
  subroutine init_mpi_env(self, required_threading)
    use mpi_f08, only:  MPI_COMM_RANK, MPI_COMM_SIZE, MPI_GROUP_SIZE
    class(mpi_env_type), intent(inout) :: self 
    integer, optional,   intent(in)    :: required_threading

    if(present(required_threading)) then
       call mpilib20_init_thread(self, required_threading)
    else
       call mpilib20_init(self)
    endif

    call MPI_COMM_RANK(self%comm,  self%process ,    self%ierr)
    call MPI_COMM_SIZE(self%comm,  self%n_processes, self%ierr)
    call MPI_GROUP_SIZE(self%comm, self%group,       self%ierr)
    
  end subroutine init_mpi_env

  
  !> \brief Terminates MPI execution environment.
  !>
  !> \param[inout]  mpi_env   Instance of the MPI environment
  subroutine finalise_mpi_env(self)
    class(mpi_env_type), intent(inout) :: self
    call mpilib20_finalize(self)    
  end subroutine finalise_mpi_env


end module mpilib20_init_finalise
