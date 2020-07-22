project('mpilib20', 'fortran',
        default_options: 'fortran_std=f2008')

subdir('src') # add src files to src variable

mpi = dependency('openMP', language: 'fortran')

executable('mpilib20', src, dependencies : mpi)
