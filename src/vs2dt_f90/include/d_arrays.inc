c     allocate arrays
      allocate(DELZ(NLY),DZZ(NLY),DXR(NXR),RX(NXR))
      allocate(HX(NNODES),NTYP(NNODES))
      allocate(THETA(NNODES),THLST(NNODES))
      allocate(P(NNODES),PXXX(NNODES))
      allocate(Q(NNODES),QQ(NNODES))
      allocate(HCND(NNODES),HKLL(NNODES),HKTT(NNODES))
      allocate(A(NNODES),B(NNODES),C(NNODES),D(NNODES),E(NNODES),
     &    RHS(NNODES),XI(NNODES))
      allocate(JTEX(NNODES))
      allocate(DUM(NNODES))
      allocate(DPTH(NNODES),RT(NNODES))
      allocate(DX1(NNODES),DX2(NNODES),DZ1(NNODES),DZ2(NNODES),
     &    VX(NNODES),VZ(NNODES),CC(NNODES),COLD(NNODES),CS(NNODES),
     &    QT(NNODES),NCTYP(NNODES),RET(NNODES))
      allocate(AO(NNODES),BO(NNODES),CO(NNODES),DO(NNODES),EO(NNODES))
      allocate(PITT(NNODES))
      allocate(DEL(NNODES),ETA(NNODES),V(NNODES))

