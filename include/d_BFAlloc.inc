!      if (allocated(idBF)) deallocate(idBF)
!      if (allocated(numcellsBF)) deallocate(numcellsBF)
!      if (allocated(nodenum)) deallocate(nodenum)
!      if (allocated(totalBF)) deallocate(totalBF)
!      if (allocated(currentBF)) deallocate(currentBF)
      allocate(idBF(numBF))
      allocate(numcellsBF(numBF))
      allocate(nodenum(numBF,maxnumcells))
      allocate(totalBF(numBF,2))
      allocate(currentBF(numBF,4))


