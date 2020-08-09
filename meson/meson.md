```bash
FC=path/to/fortran/compiler \
meson builddir # Initialise build into mpilib20/build
cd build
meson compile
```
# `meson builddir`
## Command Line Flags
-`--buildtype BUILDTYPE`
    - `plain`, `debug`, `debugoptimized`, `release`\
- '--prefix PREFIX'
    - default /usr/local
## Environment Variables
`FC` - path/to/fortran/compiler (openMP etc)