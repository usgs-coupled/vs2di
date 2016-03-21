c      if (allocated(idBF)) deallocate(idBF)
c      if (allocated(numcellsBF)) deallocate(numcellsBF)
c      if (allocated(nodenum)) deallocate(nodenum)
c      if (allocated(totalBF)) deallocate(totalBF)
c      if (allocated(currentBF)) deallocate(currentBF)
      allocate(idBF(numBF))
      allocate(numcellsBF(numBF))
      allocate(nodenum(numBF,maxnumcells))
      allocate(totalBF(numBF,2))
      allocate(currentBF(numBF,4))


