!mpirun -np 1 ./test.exe
program test
  use mpilib20
  implicit none

  !Decs
  type(mpi_env_type) :: mpi_env
  
  !Main
  call mpi_env%init()
  write(*,*) 'n processes: ', mpi_env%n_processes
  call mpi_env%finalize()
  
end program test
