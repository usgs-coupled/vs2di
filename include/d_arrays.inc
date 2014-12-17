!     allocate arrays
      allocate(DELZ(NLY),DZZ(NLY),DXR(NXR),RX(NXR))
      allocate(HX(NNODES),NTYP(NNODES))
      allocate(THETA(NNODES),THLST(NNODES),SATUR(NNODES),POROSITY(NNODES))
      allocate(P(NNODES),PXXX(NNODES),THEAD(NNODES))
      allocate(Q(NNODES),QQ(NNODES))
      allocate(HCND(NNODES),HKLL(NNODES),HKTT(NNODES))
      allocate(A(NNODES),B(NNODES),C(NNODES),D(NNODES),E(NNODES),  &
         RHS(NNODES),XI(NNODES))
     allocate(AS(NNODES),BS(NNODES),CS(NNODES),DS(NNODES),ES(NNODES),  &
         RHSS(NNODES),XIS(NNODES))
      allocate(JTEX(NNODES))
      allocate(DUM(NNODES),PDUM(KNXR))
      allocate(DPTH(NNODES),RT(NNODES))
      allocate(DX1(NNODES),DX2(NNODES),DZ1(NNODES),DZ2(NNODES),  &
          TT(NNODES),TTOLD(NNODES),TS(NNODES),  &
         QT(NNODES),NHTYP(NNODES))
     allocate(VX(NNODES),VZ(NNODES))
     allocate(RHO(NNODES),RHOOLD(NNODES))
     allocate(DXS1(NNODES),DXS2(NNODES),DZS1(NNODES),DZS2(NNODES),  &
          CC(Nsol,NNODES),CCOLD(Nsol,NNODES),CSS(Nsol,NNODES),  &
          QS(NNODES),NCTYP(NNODES),CONC(Nsol))
      allocate(AO(NNODES),BO(NNODES),CO(NNODES),DO(NNODES),EO(NNODES))
      allocate(AOC(NNODES),BOC(NNODES),COC(NNODES),DOC(NNODES),EOC(NNODES))
      allocate(PITT(NNODES))
      allocate(DEL(NNODES),ETA(NNODES),V(NNODES))
      allocate(TempC(NNODES))
      allocate(XNODE(NNODES),ZNODE(NNODES))
      allocate(phreeC(Nodesol))
      allocate(CMIXFARC(7,NNODES),BCSOL(Nsol),INDSOL1(7,NNODES),INDSOL2(7,NNODES))
	  allocate(ic1_reordered(nnodes,7))
      allocate(NPRCHEM(NNODES),NPRCHXZ(NNODES))
      allocate(BLSOL(Nsol,36),bl62I(Nsol),bl62IT(Nsol),bl62O(Nsol),bl62OT(Nsol), &
             bcmtt(Nsol),bcmt(Nsol),bcmtr(Nsol))
      allocate(COMPNAME(Nsol))       
      allocate(CCBR(Nsol,NNODES),CCAR(Nsol,NNODES))
  
      