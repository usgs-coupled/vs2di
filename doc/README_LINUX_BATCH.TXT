To build/install openmp version on linux:
  tar xvzf vs2drt-1.4.1-14001.tar.gz
  cd vs2drt-1.4.1-14001
  mkdir _openmp
  cd _openmp/
  ../configure --help   # instructions
  ../configure --prefix=$HOME  # to install in home directory (non-root)
  make -j 10  # build using 10 processes
  make install


To build/install mpi version on linux:
  cd vs2drt-1.4.1-14001
  mkdir _mpi
  cd _mpi/
  ../configure --help   # instructions
  ../configure --with-mpi --prefix=$HOME  # to install in home directory (non-root)
  make -j 10  # build using 10 processes
  make install

