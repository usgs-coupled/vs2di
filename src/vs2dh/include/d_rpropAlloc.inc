c      if (allocated(HK)) deallocate(HK)
c      if (allocated(HT)) deallocate(HT)
c      if (allocated(ANIZ)) deallocate(ANIZ)
      allocate(HK(NTEX,NPROP))
      allocate(HT(NTEX,11))
      allocate(ANIZ(NTEX))

