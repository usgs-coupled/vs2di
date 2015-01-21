      SUBROUTINE VSSIP
!
!*****
!VSSIP
!*****
!
!     PURPOSE: TO SOLVE THE  MATRIX EQUATIONS USING THE
!     STRONGLY IMPLICIT METHOD
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use press
      use equat
      use jtxx
      use trxxh
      use sip
      use rpropsh
      use scon
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
      DIMENSION IORDER(21)
      DIMENSION TEMP(100),HM(30)
      SAVE HM,W1,W9,L2,NTH
      DATA W1/0.0D0/
!
!-------------------------------------------------------------------
!
      DATA IORDER/1,2,3,4,5,1,2,3,4,5,11*1/
!
!     COMPUTE ITERATION PARAMETERS
!
      J2=NXR-2
      I2=NLY-2
      L2=5
      PL2=L2-1
      W=0.0D0
      PIE=0.0D0
      W9=100.0D0
!
! COMPUTE MAXIMUM PARAMETER
!
      DO 10 I=2,NLYY
      DO 10 J=2,NXRR
      N=NLY*(J-1)+I
      IF(HX(N).GT.0.0D0) THEN
      IM1=JTEX(N)
      PIE=PIE+1.0D0
      DX=DXR(J)/RX(NXR)
      DY=DELZ(I)/DZZ(NLY)
      DX3=DX*DX
      DY2=DY*DY
      W=W+1.0D0-DMIN1((DX3+DX3)/(1.0D0+ANIZ(IM1)*DX3/DY2),(DY2+DY2)/&
      (1.0D0+DY2/(ANIZ(IM1)*DX3)))
      END IF
   10 CONTINUE
      W=W/PIE
!
! COMPUTE PARAMETERS IN GEOMETRIC SEQUENCE
!
      PJ=-1.0D0
      DO 20 I=1,L2
      PJ=PJ+1.0D0
   20 TEMP(I)=1.0D0 -(1.0D0 -W)**(PJ/PL2)
!
! ORDER SEQUENCE OF PARAMETERS
!
      DO 30 J=1,L2
   30 HM(J)=TEMP(IORDER(J))
!      WRITE (06,4000) L2,(HM(J),J=1,L2)
      RETURN
!
! STRONGLY IMPLICIT ALGORITHM
!
      ENTRY SLVSIP
      I2=NLY-2
      J2=NXR-2
!      SELECT ITERATION PARAMETER.  INITIALIZE ARRAYS
!
      IF(TRANS1) THEN
!
!  IF TRANS1=T  TRANSPORT EQUATION IS SOLVED
!           =F  FLOW EQUATION IS SOLVED
!
      NT=NIT1
      ELSE
      NT=NIT
      END IF
      IF(MOD(NT,L2).EQ.0.OR.NT.EQ.1)NTH=0
      NTH=NTH+1
      W=HM(NTH)
      ITEST=0
      DO 40 I=1,NNODES
      DEL(I)=0.0D0
      ETA(I)=0.0D0
      V(I)=0.0D0
   40 XI(I)=0.0D0
      BIGI=0.0D0
      BIGI1=0.0D0
!
! CHOOSE SIP NORMAL OR REVERSE ALGORITHM
!
      IF(MOD(NT,2)) 50,80,50
! ......................................................................
! ORDER EQUATIONS WITH ROW 1 FIRST  -  3X3 EXAMPLE:
!    1 2 3
!    4 5 6
!    7 8 9
! ......................................................................
   50 DO 60 I=2,NLYY
      DO 60 J=2,NXRR
      N=I+NLY*(J-1)
!
!   ---- SKIP COMPUTATIONS OF NODE IS OUTSIDE OF SOLUTION DOMAIN
!
      IF(HX(N).EQ.0.0D0) GO TO 60
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS1)).OR.(TRANS1.AND.(NHTYP(N).EQ.1)))GO TO 60
      NL=N-NLY
      NA=N-1
      NB=N+1
!
!     --- SIP "NORMAL" ALGORITHM-----
!     --- FORWARD SUBSTITUTE, COMPUTING INTERMEDIATE VECTOR V --
!
      CH=DEL(NA)*B(N)/(1.0D0 +W*DEL(NA))
      GH=ETA(NL)*A(N)/(1.0D0 +W*ETA(NL))
      BH=B(N)-W*CH
      DH=A(N)-W*GH
      EH=E(N)+W*CH+W*GH
      FH=C(N)-W*CH
      HH=D(N)-W*GH
      ALFA=BH
      BETA=DH
      GAMA=EH-ALFA*ETA(NA)-BETA*DEL(NL)
      DEL(N)=FH/GAMA
      ETA(N)=HH/GAMA
      RES=RHS(N)
      V(N)=(HMAX*RES-ALFA*V(NA)-BETA*V(NL))/GAMA
   60 CONTINUE
!
!  ---BACK SUBSTITUTE FOR VECTOR XI
!
      DO 70 I=1,I2
      I3=NLY-I
      DO 70 J=1,J2
      J3=NXR-J
      N=I3+NLY*(J3-1)
      IF(HX(N).EQ.0.0D0) GO TO 70
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS1)).OR.(TRANS1.AND.(NHTYP(N).EQ.1)))GO TO 70
      XI(N)=V(N)-DEL(N)*XI(N+NLY)-ETA(N)*XI(N+1)
!
!      FIND MAXIMUM HEAD CHANGE
!
      TCHK=DABS(XI(N))
      IF(TCHK.GE.BIGI) THEN
      BIGI=TCHK
      BIGI1=XI(N)
      END IF
   70 CONTINUE
      GO TO 110
!
!.......................................................................
!  ---ORDER EQUATIONS WITH THE LAST ROW FIRST  -  3X3 EXAMPLE
!           7 8 9
!           4 5 6
!           1 2 3
!......................................................................
!
   80 DO 90 II=1,I2
      I=NLY-II
      DO 90 J=2,NXRR
      N=I+NLY*(J-1)
      NL=N-NLY
      NA=N-1
      NB=N+1
!
!  -- SKIP COMPUTATIONS IF NODE IS OUTSIDE OF SOLUTION DOMAIN
!
      IF(HX(N).EQ.0.0D0) GO TO 90
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS1)).OR.(TRANS1.AND.(NHTYP(N).EQ.1)))GO TO 90
!
!------ SIP "REVERSE" ALGORITHM
! --- FORWARD SUBSTITUTE, COMPUTING INTERMEDIATE VECTOR V
!
      CH=DEL(NB)*D(N)/(1.0D0 +W*DEL(NB))
      GH=ETA(NL)*A(N)/(1.0D0 +W*ETA(NL))
      BH=D(N)-W*CH
      DH=A(N)-W*GH
      EH=E(N)+W*CH+W*GH
      FH=C(N)-W*CH
      HH=B(N)-W*GH
      ALFA=BH
      BETA=DH
      GAMA=EH-ALFA*ETA(NB)-BETA*DEL(NL)
      DEL(N)=FH/GAMA
      ETA(N)=HH/GAMA
      RES=RHS(N)
      V(N)=(HMAX*RES-ALFA*V(NB)-BETA*V(NL))/GAMA
   90 CONTINUE
!
! --- BACK SUBSTITUTE FOR VECTOR XI
!
      DO 100 I3=2,NLYY
      DO 100 J=1,J2
      J3=NXR-J
      N=I3+NLY*(J3-1)
      IF(HX(N).EQ.0.0D0) GO TO 100
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS1)).OR.(TRANS1.AND.(NHTYP(N).EQ.1)))GO TO 100
      XI(N)=V(N)-DEL(N)*XI(N+NLY)-ETA(N)*XI(N-1)
!
!      FIND MAXIMUM HEAD CHANGE
!
      TCHK=DABS(XI(N))
      IF(TCHK.GE.BIGI) THEN
      BIGI=TCHK
      BIGI1=XI(N)
      END IF
  100 CONTINUE
!
!      COMPUTE RELAXATION PARAMETER W FOR HEAD CHANGES.  ALGORITHM
!      IS FROM COOLEY (1983)
!
  110 S=1.0D0
      IF(NT.GT.1.AND.W1.NE.0.0D0) S=BIGI1/W1
      S1=DABS(S)
      IF(S.LT.-1.0D0) THEN
      W=1.0D0/(S1+S1)
      ELSE
      W=(3.0D0+S)/(3.0D0+S1)
      END IF
      IF(W.EQ.W9) W=0.9D0*W
      W1=W*BIGI
      IF(W1.GT.DSMAX) W=DSMAX/BIGI
      IF(BIGI1.LT.0.0D0) W1=-W1
!
!     ADD CHANGES TO MATRIX.
!
      W9=W
      IF(TRANS1) THEN
      DO 120 N=NLY+1,NNODES
      IF(NHTYP(N).NE.1.AND.HX(N).GT.0.0D0) TT(N)=TT(N)+W*XI(N)
  120 CONTINUE
      IF(BIGI.GT.EPS1) ITEST=1
      ELSE
      DO 130 N=NLY+1,NNODES
      IF(HX(N).GT.0.0D0.AND.NTYP(N).NE.1) P(N)=P(N)+W*XI(N)
  130 CONTINUE
!
!      COMPARE MAXIMUM HEAD CHANGE TO CLOSURE CRITERION.
!
      IF(BIGI.GT.EPS) ITEST=1
      DHMX(NIT)=BIGI
      END IF
      RETURN
! 4000 FORMAT(1X,I5,25HSIP ITERATION PARAMETERS:,6D15.7/(28X,6D15.7/))
      END
                  SUBROUTINE VSSIPSOL
!*****
!VSSIPSOL
!*****
!
!     PURPOSE: TO SOLVE THE  MATRIX EQUATIONS USING THE
!     STRONGLY IMPLICIT METHOD
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use press
      use equats
      use jtxx
      use trxx
      use sip
      use rpropsh
      use scon
      use tempcc
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
      COMMON/TCON1/NIS,NIS1,NIS3
      COMMON/SCON1/ITESTS
      DIMENSION IORDERS(21)
      DIMENSION TEMPS(100),HMS(30)
      SAVE HMS,W1S,WS9,LS2,NTHS
      DATA W1S/0.0D0/
!
!-------------------------------------------------------------------
!
      DATA IORDERS/1,2,3,4,5,1,2,3,4,5,11*1/
!
!     COMPUTE ITERATION PARAMETERS
!
      J2=NXR-2
      I2=NLY-2
      LS2=5
      PL2=LS2-1
      WS=0.0D0
      PIE=0.0D0
      WS9=100.0D0
!
!  COMPUTE MAXIMUM PARAMETER
!
      DO 10 I=2,NLYY
      DO 10 J=2,NXRR
      N=NLY*(J-1)+I
      IF(HX(N).GT.0.0D0) THEN
      IM1=JTEX(N)
      PIE=PIE+1.0D0
      DX=DXR(J)/RX(NXR)
      DY=DELZ(I)/DZZ(NLY)
      DX3=DX*DX
      DY2=DY*DY
      WS=WS+1.0D0-DMIN1((DX3+DX3)/(1.0D0+ANIZ(IM1)*DX3/DY2),(DY2+DY2)/&
      (1.0D0+DY2/(ANIZ(IM1)*DX3)))
      END IF
  10  CONTINUE
      WS=WS/PIE
!
! COMPUTE PARAMETERS IN GEOMETRIC SEQUENCE
!
      PJ=-1.0D0
      DO 20 I=1,LS2
      PJ=PJ+1.0D0
  20  TEMPS(I)=1.0D0 -(1.0D0 - WS)**(PJ/PL2)
!
! ORDER SEQUENCE OF PARAMETERS
!
      DO 30 J=1,LS2
      HMS(J)=TEMPS(IORDERS(J))
  30  CONTINUE 
!      WRITE (06,4000) LS2,(HMS(J),J=1,LS2)
      RETURN
!
! STRONGLY IMPLICIT ALGORITHM
!
      ENTRY SLVSIPSOL
      I2=NLY-2
      J2=NXR-2
!
!      SELECT ITERATION PARAMETER.  INITIALIZE ARRAYS
!      
      IF(TRANS2) THEN
!
!  IF TRANS =T  SOLUTE TRANSPORT EQUATION IS SOLVED
!           =F  FLOW EQUATION IS SOLVED
!
      NT=NIS1
      ELSE
      NT=NIT
      END IF
      IF(MOD(NT,LS2).EQ.0.OR.NT.EQ.1)NTHS=0
      NTHS=NTHS+1
      WS=HMS(NTHS)
      ITESTS=0
      DO 40 I=1,NNODES
      DEL(I)=0.0D0
      ETA(I)=0.0D0
      V(I)=0.0D0
  40  XIS(I)=0.0D0
      BIGIS=0.0D0
      BIGIS1=0.0D0
!
! CHOOSE SIP NORMAL OR REVERSE ALGORITHM
!
      IF(MOD(NT,2)) 50,80,50
! ......................................................................
! ORDER EQUATIONS WITH ROW 1 FIRST  -  3X3 EXAMPLE:
!    1 2 3
!    4 5 6
!    7 8 9
! ......................................................................
  50  DO 60 I=2,NLYY
      DO 60 J=2,NXRR
      N=I+NLY*(J-1)
!
!   ---- SKIP COMPUTATIONS OF NODE IS OUTSIDE OF SOLUTION DOMAIN
!
!1      IF(HX(N).EQ.0.0D0 .OR. NTYP(N).EQ.1.0) GO TO 60
      IF(HX(N).EQ.0.0D0) GO TO 60
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS2)).OR.(TRANS2.AND.(NCTYP(N).EQ.1)))GO TO 60   
      NL=N-NLY
!      NR=N+NLY 
      NA=N-1
      NB=N+1
!
!     --- SIP "NORMAL" ALGORITHM-----
!     --- FORWARD SUBSTITUTE, COMPUTING INTERMEDIATE VECTOR V --
!
      CH=DEL(NA)*BS(N)/(1.0D0 +WS*DEL(NA))
      GH=ETA(NL)*AS(N)/(1.0D0 +WS*ETA(NL))
      BH=BS(N)-WS*CH
      DH=AS(N)-WS*GH
      EH=ES(N)+WS*CH+WS*GH
      FH=CS(N)-WS*CH
      HH=DS(N)-WS*GH
      ALFAS=BH
      BETAS=DH
      GAMAS=EH-ALFAS*ETA(NA)-BETAS*DEL(NL)	
      DEL(N)=FH/GAMAS
      ETA(N)=HH/GAMAS
      RES=RHSS(N)
      V(N)=(HMAX*RES-ALFAS*V(NA)-BETAS*V(NL))/GAMAS
 
  60  CONTINUE
     
!
!  ---BACK SUBSTITUTE FOR VECTOR XI
!
      
      DO 70 I=1,I2
      I3=NLY-I
      DO 70 J=1,J2
      J3=NXR-J
      N=I3+NLY*(J3-1)
      IF(HX(N).EQ.0.0D0) GO TO 70
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS2)).OR.(TRANS2.AND.(NCTYP(N).EQ.1)))GO TO 70
      XIS(N)=V(N)-DEL(N)*XIS(N+NLY)-ETA(N)*XIS(N+1)
!
!      FIND MAXIMUM HEAD CHANGE
!
      TCHKS=DABS(XIS(N))
      IF(TCHKS.GE.BIGIS) THEN
      BIGIS=TCHKS
      BIGIS1=XIS(N)
      END IF
  70  CONTINUE
      GO TO 110
!
!.......................................................................
!  ---ORDER EQUATIONS WITH THE LAST ROW FIRST  -  3X3 EXAMPLE
!           7 8 9
!           4 5 6
!           1 2 3
!.......................................................................
!
   80 DO 90 II=1,I2
      I=NLY-II
      DO 90 J=2,NXRR
      N=I+NLY*(J-1)
      NL=N-NLY
!      NR=N+NLY  
      NA=N-1
      NB=N+1
!
!  -- SKIP COMPUTATIONS IF NODE IS OUTSIDE OF SOLUTION DOMAIN
!
      IF(HX(N).EQ.0.0D0) GO TO 90
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS2)).OR.(TRANS2.AND.(NCTYP(N).EQ.1)))GO TO 90
!
! ------ SIP "REVERSE" ALGORITHM
! --- FORWARD SUBSTITUTE, COMPUTING INTERMEDIATE VECTOR V
!
      CH=DEL(NB)*DS(N)/(1.0D0 +WS*DEL(NB))
      GH=ETA(NL)*AS(N)/(1.0D0 +WS*ETA(NL))
      BH=DS(N)-WS*CH
      DH=AS(N)-WS*GH
      EH=ES(N)+WS*CH+WS*GH
      FH=CS(N)-WS*CH
      HH=BS(N)-WS*GH
      ALFAS=BH
      BETAS=DH
      GAMAS=EH-ALFAS*ETA(NB)-BETAS*DEL(NL)
      DEL(N)=FH/GAMAS
      ETA(N)=HH/GAMAS
      RES=RHSS(N)
      V(N)=(HMAX*RES-ALFAS*V(NB)-BETAS*V(NL))/GAMAS

  90  CONTINUE
!
! --- BACK SUBSTITUTE FOR VECTOR XI
!
      DO 100 I3=2,NLY
      DO 100 J=1,J2
      J3=NXR-J
      N=I3+NLY*(J3-1)
      IF(HX(N).EQ.0.0D0) GO TO 100
      IF((NTYP(N).EQ.1.AND.(.NOT.TRANS2)).OR.(TRANS2.AND.(NCTYP(N).EQ.1)))GO TO 100  
      XIS(N)=V(N)-DEL(N)*XIS(N+NLY)-ETA(N)*XIS(N-1)
!
!      FIND MAXIMUM HEAD CHANGE
!
      TCHKS=DABS(XIS(N))
      IF(TCHKS.GE.BIGIS) THEN
      BIGIS=TCHKS
      BIGIS1=XIS(N)
      END IF
  100 CONTINUE
!
!      COMPUTE RELAXATION PARAMETER W FOR HEAD CHANGES.  ALGORITHM
!      IS FROM COOLEY (1983)
!
  110 S=1.0D0
      IF(NT.GT.1.AND.W1S.NE.0.0D0) S=BIGIS1/W1S
      S1=DABS(S)
      IF(S.LT.-1.0D0) THEN
      WS=1.0D0/(S1+S1)
      ELSE
      WS=(3.0D0+S)/(3.0D0+S1)
      END IF
      IF(WS.EQ.WS9) WS=0.9D0*WS
      W1S=WS*BIGIS
      IF(W1S.GT.DSMAX) WS=DSMAX/BIGIS
      IF(BIGIS1.LT.0.0D0)W1S=-W1S
!
!      ADD CHANGES TO MATRIX.
!
      WS9=WS
      IF(TRANS2) THEN
      
      DO 120 N=NLY+1,NNODES
      IF(NCTYP(N).NE.1.AND.HX(N).GT.0.0D0)TempC(N)= TempC(N)+WS*XIS(N)
  
  120 CONTINUE
!
!      COMPARE MAXIMUM HEAD CHANGE TO CLOSURE CRITERION.
!
      IF(BIGIS.GT.EPS3) ITESTS=1
        ELSE
          DO 130 N=NLY+1,NNODES
            IF(HX(N).GT.0.0D0 .AND.NTYP(N).NE.1)P(N)=P(N)+WS*XIS(N)
  130  CONTINUE
      IF(BIGIS.GT.EPS)ITESTS=1
      DHMX(NIT)=BIGIS
      end if    
      RETURN
! 4000 FORMAT(1X,I5,25HSIPS ITERATION PARAMETER:,6D15.7/(28X,6D15.7/))
      END
      SUBROUTINE VSCOEF
!******
!VSCOEF
!******
!     PURPOSE: TO COMPUTE ALL VALUES OF NONLINEAR COEFFICIENTS
!              USING THE MOST RECENT VALUES OF PRESSURE HEAD
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use mprop
      use press
      use hcon
      use jtxx
      use rpropsh
      use trxxh
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
      integer hydraulicFunctionType
      common/functiontype/ hydraulicFunctionType
!
!-------------------------------------------------------------------
      if(hydraulicFunctionType.eq.1) then
!
!       Van Genuchten functions are used
!
      DO 10 J=2,NLYY
      DO 10 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).GT.0.0D0) THEN
      J1=JTEX(IN)
      HCND(IN)=0.0D0
!
!       COMPUTE PRESSURE HEADS TO USE IN FUNCTIONS
!
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(HEAT)THEN
      HCND(IN)=VSHKUVGH(PTMP,TT(IN),J1)
      ELSE
      HCND(IN)=VSHKUVG(PTMP,J1)
      END IF
      THETA(IN)=VSTHUVG(PTMP,J1)
      END IF
   10 CONTINUE
      RETURN
      else
      if(hydraulicFunctionType.eq.2) then
!
!       Haverkamp functions
!
      DO 11 J=2,NLYY
      DO 11 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).GT.0.0D0) THEN
      J1=JTEX(IN)
      HCND(IN)=0.0D0
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(HEAT)THEN
      HCND(IN)=VSHKUHKH(PTMP,TT(IN),J1)  
      ELSE   
      HCND(IN)=VSHKUHK(PTMP,J1)
      END IF
      THETA(IN)=VSTHUHK(PTMP,J1)
      END IF
 11   CONTINUE
      RETURN
      else
!
!       Brooks-Corey functions
!
      if(hydraulicFunctionType.eq.0) then
      DO 12 J=2,NLYY
      DO 12 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).GT.0.0D0) THEN
      J1=JTEX(IN)
      HCND(IN)=0.0D0
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(HEAT)THEN
      HCND(IN)=VSHKUBCH(PTMP,TT(IN),J1)
      ELSE  
      HCND(IN)=VSHKUBC(PTMP,J1)
      END IF
      THETA(IN)=VSTHUBC(PTMP,J1)
      END IF
 12   CONTINUE
      RETURN
!
!       Tabular functions
!
      else
      if(hydraulicFunctionType.eq.3) then
      DO 13 J=2,NLYY
      DO 13 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).GT.0.0D0) THEN
      J1=JTEX(IN)
      HCND(IN)=0.0D0
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(HEAT)THEN
      HCND(IN)=VSHKUTABH(PTMP,TT(IN),J1)
      ELSE  
      HCND(IN)=VSHKUTAB(PTMP,J1)
      END IF
      THETA(IN)=VSTHUTAB(PTMP,J1)
      END IF
 13   CONTINUE
      RETURN
      else
!
!  user supplied other function type
!
      DO 14 J=2,NLYY
      DO 14 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).GT.0.0D0) THEN
      J1=JTEX(IN)
      HCND(IN)=0.0D0
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
!
!  revisions for Rossi-Nimmo
!
!      HCND(IN)=VSHKUOT(PTMP,J1)
      THETA(IN)=VSTHUOT(PTMP,J1)
      IF(HEAT)THEN
      HCND(IN)=VSHKUOTH(THETA(IN),TT(IN),J1)
      ELSE
      HCND(IN) = VSHKUOT(THETA(IN),J1)
      END IF      
!
!  end revsions
!
      END IF
 14   CONTINUE
      RETURN
      end if
      end if
      end if
      end if
      END
      
      SUBROUTINE VSHCMP
!******
!VSHCMP
!******
!
!   PURPOSE: TO COMPUTE INTERCELL CONDUCTANCES
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use hcon
      use jtxx
      use rpropsh
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
!
!----------------------------------------------------------------------
!
!    COMPUTE HARMONIC MEANS OF KSAT AND GRID SPACING
!
      DO 10 J=2,NLY
      DO 10 N=2,NXR
      IN=NLY*(N-1)+J
      JM1=IN-1
      NM1=IN-NLY
      A1=ANIZ(JTEX(IN))
      A2=ANIZ(JTEX(JM1))
      IF(HX(IN).EQ.0.0D0) GO TO 10
      AREA=DXR(N)
      IF(RAD)AREA=PI2*RX(N)*DXR(N)
!
!   VERTICAL CONDUCTANCE
!   THROUGH TOP
!
      HKTT(IN)=2.0D0*A1*A2*AREA*HX(IN)*HX(JM1)/(A2*HX(JM1)*DELZ(J)+&
      A1*HX(IN)*DELZ(J-1))
      AREA=DELZ(J)
      IF(RAD)AREA=PI2*DELZ(J)*(RX(N)-0.5D0 *DXR(N))
!
!   HORIZONTAL OR RADIAL CONDUCTANCE
!   THROUGH LEFT-HAND SIDE
!
      HKLL(IN)=2.0D0*AREA*HX(IN)*HX(NM1)/(HX(NM1)*DXR(N)+HX(IN)*DXR(N-1))
   10 CONTINUE
      RETURN
      END
      SUBROUTINE VSFLUX
!******
!VSFLUX
!******
!
!      PURPOSE: TO COMPUTE FLUXES AND MASS BALANCE
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use mprop
      use press
      use disch
      use dumm
      use jtxx
      use equat
      use equats
      use trxx
      use trxy1
      use plott
      use rpropsh
      use scon
      use BF
      use ptet
      use temp
      use pit
      use trxy2
      use trxxh
      use solmass
      use compnam
      use react
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      COMMON/JCON/JSTOP,JFLAG,jflag1
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      LOGICAL F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG2/F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      integer hydraulicFunctionType
      common/functiontype/ hydraulicFunctionType
      character*20 label9(99,3)
      CHARACTER*80 TITL
      CHARACTER*6 ZUNIT,CUNX,HUNX
      CHARACTER*7 TUNIT
      COMMON/SCHAR/TITL,ZUNIT,TUNIT,CUNX,HUNX
      common/elimit/elimit1,elimit2
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
      COMMON/JCONF/JFLAG2
      CHARACTER*10 SCOMPNAME(50)
      COMMON/MASSB/BL(99),bcmft,bcmht,bl29I,bl29IT,bl29O,bl29OT, &
      bl95I,bl95IT,bl95O,bl95OT
      common/massb1/bcmf,bcmh, &
      bltemp69,bltemp72,bltemp75,bltemp78,bltemp91
      common/massb2/label9
     
!-------------------------------------------------------------------
!
!   INITIALIZE MASS BALANCE VARIABLES USED FOR
!   ENTIRE SIMULATION.
!
!
!  05/02 new variables created:
!   bcmf is water mass added to system by change in flow BC
!   bcmt is solute mass added to system by change in flow BC
!
      IF(KTIM.EQ.1) THEN
      DO 10 I=1,99
      BL(I)=0.0D0
   10 CONTINUE
      if(solute)then
        do 100 N = 1,Nsol
        dO 108 I = 1,36
        BLSOL(N,I)=0.0D0
   108  continue
        bl62I(N)=0.0d0
        bl62IT(N)=0.0d0
        bl62O(N)=0.0d0
        bl62OT(N)=0.0d0
        bcmtt(N)=0.0d0
        bcmt(N)=0.0d0
        bcmtr(N)=0.0d0
        bltemp36(N) = 0.0d0
        bltemp39(N) = 0.0d0
        bltemp42(N) = 0.0d0
        bltemp45(N) = 0.0d0
        bltemp60(N) = 0.0d0   
 100  CONTINUE     
      end if 
      if (f7p) then
       do 11 i = 1,numBF
        totalBF(i,1) = 0.0d0
        totalBF(i,2) = 0.0d0
        currentBF(i,4) = 0.0d0
 11     continue
      end if

      bltemp69 = 0.0d0
      bltemp72 = 0.0d0
      bltemp75 = 0.0d0
      bltemp78 = 0.0d0
      bltemp91 = 0.0d0
      bcmft = 0.0d0
      bcmht= 0.0d0    
      bl95I= 0.0d0
      bl95IT= 0.0d0
      bl95O= 0.0d0
      bl95OT= 0.0d0
!
!   label9 is array of headings for file 9 
!
      label9(1,1) = ' FLOW IN '
      LABEL9(2,1) = LABEL9(1,1)
      LABEL9(3,1) = LABEL9(1,1)
      LABEL9(4,1) = ' FLOW OUT '
      LABEL9(5,1) = LABEL9(4,1)
      LABEL9(6,1) = LABEL9(4,1)
      LABEL9(7,1) = LABEL9(1,1)
      LABEL9(8,1) = LABEL9(1,1)
      LABEL9(9,1) = LABEL9(1,1)
      LABEL9(10,1) = LABEL9(4,1)
      LABEL9(11,1) = LABEL9(4,1)
      LABEL9(12,1) = LABEL9(4,1)
      LABEL9(13,1) = ' TOTAL '
      LABEL9(14,1) = LABEL9(13,1)
      LABEL9(15,1) = LABEL9(13,1)
      LABEL9(16,1) = LABEL9(13,1)
      LABEL9(17,1) = LABEL9(13,1)
      LABEL9(18,1) = LABEL9(13,1)
      LABEL9(19,1) = ' EVAP- '
      LABEL9(20,1) = LABEL9(19,1)
      LABEL9(21,1) = LABEL9(19,1)
      LABEL9(22,1) = ' TRANS- '
      LABEL9(23,1) = LABEL9(22,1)
      LABEL9(24,1) = LABEL9(22,1)
      LABEL9(25,1) = ' EVAP + '
      LABEL9(26,1) = LABEL9(25,1)
      LABEL9(27,1) = LABEL9(25,1)
      LABEL9(28,1) = ' FLUID '
      LABEL9(29,1) = LABEL9(28,1)
      LABEL9(30,1) = LABEL9(28,1)
      LABEL9(31,1) = LABEL9(28,1)
      LABEL9(32,1) = LABEL9(28,1)
      LABEL9(33,1) = LABEL9(28,1)
      LABEL9(34,1) = ' SOLUTE IN '
      LABEL9(35,1) = LABEL9(34,1)
      LABEL9(36,1) = LABEL9(34,1)
      LABEL9(37,1) = ' SOLUTE OUT'
      LABEL9(38,1) = LABEL9(37,1)
      LABEL9(39,1) = LABEL9(37,1)
      LABEL9(40,1) = LABEL9(34,1)
      LABEL9(41,1) = LABEL9(34,1)
      LABEL9(42,1) = LABEL9(34,1)
      LABEL9(43,1) = LABEL9(37,1)
      LABEL9(44,1) = LABEL9(37,1)
      LABEL9(45,1) = LABEL9(37,1)
      LABEL9(46,1) = ' DISPERSION'
      LABEL9(47,1) = LABEL9(46,1)
      LABEL9(48,1) = LABEL9(46,1)
      LABEL9(49,1) = LABEL9(46,1)
      LABEL9(50,1) = LABEL9(46,1)
      LABEL9(51,1) = LABEL9(46,1)
      LABEL9(52,1) = ' TOTAL '
      LABEL9(53,1) = LABEL9(52,1)
      LABEL9(54,1) = LABEL9(52,1)
      LABEL9(55,1) = LABEL9(52,1)
      LABEL9(56,1) = LABEL9(52,1)
      LABEL9(57,1) = LABEL9(52,1)
      LABEL9(58,1) = ' SOLUTE OUT'
      LABEL9(59,1) = LABEL9(58,1)
      LABEL9(60,1) = LABEL9(58,1)
      LABEL9(61,1) = ' SOLUTE '
      LABEL9(62,1) = LABEL9(61,1)
      LABEL9(63,1) = LABEL9(61,1)
      LABEL9(64,1) = LABEL9(61,1)
      LABEL9(65,1) = LABEL9(61,1)
      LABEL9(66,1) = LABEL9(61,1)
      LABEL9(67,1) = ' ENERGY IN '
      LABEL9(68,1) = LABEL9(61,1)
      LABEL9(69,1) = LABEL9(61,1)
      LABEL9(70,1) = ' ENERGY OUT '
      LABEL9(71,1) = LABEL9(61,1)
      LABEL9(72,1) = LABEL9(61,1)
      LABEL9(73,1) = LABEL9(61,1)
      LABEL9(74,1) = LABEL9(61,1)
      LABEL9(75,1) = LABEL9(61,1)
      LABEL9(76,1) = LABEL9(61,1)
      LABEL9(77,1) = LABEL9(61,1)
      LABEL9(78,1) = LABEL9(61,1)
      LABEL9(79,1) = ' DISPERSION '
      LABEL9(80,1) = LABEL9(61,1)
      LABEL9(81,1) = LABEL9(61,1)
      LABEL9(82,1) = LABEL9(61,1)
      LABEL9(83,1) = LABEL9(61,1)
      LABEL9(84,1) = LABEL9(61,1)
      LABEL9(85,1) = ' TOTAL '
      LABEL9(86,1) = LABEL9(61,1)
      LABEL9(87,1) = LABEL9(61,1)
      LABEL9(88,1) = LABEL9(61,1)
      LABEL9(89,1) = LABEL9(61,1)
      LABEL9(90,1) = LABEL9(61,1)
      LABEL9(91,1) = ' ENERGY '
      LABEL9(92,1) = LABEL9(61,1)
      LABEL9(93,1) = LABEL9(61,1)
      LABEL9(94,1) = LABEL9(61,1)
      LABEL9(95,1) = LABEL9(61,1)
      LABEL9(96,1) = LABEL9(61,1)
      LABEL9(97,1) = ' ENERGY OUT '
      LABEL9(98,1) = LABEL9(61,1)
      LABEL9(99,1) = LABEL9(61,1)
!
      LABEL9(1,2) = ' SP HEAD '
      LABEL9(2,2) = LABEL9(1,2)
      LABEL9(3,2) = LABEL9(1,2)
      LABEL9(4,2) = LABEL9(1,2)
      LABEL9(5,2) = LABEL9(1,2)
      LABEL9(6,2) = LABEL9(1,2)
      LABEL9(7,2) = ' SP FLUX '
      LABEL9(8,2) = LABEL9(7,2)
      LABEL9(9,2) = LABEL9(7,2)
      LABEL9(10,2) = LABEL9(7,2)
      LABEL9(11,2) = LABEL9(7,2)
      LABEL9(12,2) = LABEL9(7,2)
      LABEL9(13,2) = ' FLOW IN '
      LABEL9(14,2) = LABEL9(13,2)
      LABEL9(15,2) = LABEL9(13,2)
      LABEL9(16,2) = ' FLOW OUT '
      LABEL9(17,2) = LABEL9(16,2)
      LABEL9(18,2) = LABEL9(16,2)
      LABEL9(19,2) = ' ORATION '
      LABEL9(20,2) = LABEL9(19,2)
      LABEL9(21,2) = LABEL9(19,2)
      LABEL9(22,2) = ' PIRATION '
      LABEL9(23,2) = LABEL9(22,2)
      LABEL9(24,2) = LABEL9(22,2)
      LABEL9(25,2) = ' TRANS '
      LABEL9(26,2) = LABEL9(25,2)
      LABEL9(27,2) = LABEL9(25,2)
      LABEL9(28,2) = ' STORAGE '
      LABEL9(29,2) = LABEL9(28,2)
      LABEL9(30,2) = LABEL9(28,2)
      LABEL9(31,2) = ' VOL BAL '
      LABEL9(32,2) = LABEL9(31,2)
      LABEL9(33,2) = LABEL9(31,2)
      LABEL9(34,2) = LABEL9(1,2)
      LABEL9(35,2) = LABEL9(1,2)
      LABEL9(36,2) = LABEL9(1,2)
      LABEL9(37,2) = LABEL9(1,2)
      LABEL9(38,2) = LABEL9(1,2)
      LABEL9(39,2) = LABEL9(1,2)
      LABEL9(40,2) = LABEL9(7,2)
      LABEL9(41,2) = LABEL9(7,2)
      LABEL9(42,2) = LABEL9(7,2)
      LABEL9(43,2) = LABEL9(7,2)
      LABEL9(44,2) = LABEL9(7,2)
      LABEL9(45,2) = LABEL9(7,2)
      LABEL9(46,2) = ' IN '
      LABEL9(47,2) = LABEL9(46,2)
      LABEL9(48,2) = LABEL9(46,2)
      LABEL9(49,2) = ' OUT '
      LABEL9(50,2) = LABEL9(49,2)
      LABEL9(51,2) = LABEL9(49,2)
      LABEL9(52,2) = LABEL9(34,1)
      LABEL9(53,2) = LABEL9(34,1)
      LABEL9(54,2) = LABEL9(34,1)

      LABEL9(55,2) = LABEL9(37,1)
      LABEL9(56,2) = LABEL9(37,1)
      LABEL9(57,2) = LABEL9(37,1)
      LABEL9(58,2) = ' ET '
      LABEL9(59,2) = LABEL9(58,2)
      LABEL9(60,2) = LABEL9(58,2)
      LABEL9(61,2) = LABEL9(28,2)
      LABEL9(62,2) = LABEL9(28,2)
      LABEL9(63,2) = LABEL9(28,2)
      LABEL9(64,2) = ' MASS BAL  '
      LABEL9(65,2) = LABEL9(64,2)
      LABEL9(66,2) = LABEL9(64,2)

      LABEL9(67,2) = LABEL9(1,2)
      LABEL9(68,2) = LABEL9(1,2)
      LABEL9(69,2) = LABEL9(1,2)
      LABEL9(70,2) = LABEL9(1,2)
      LABEL9(71,2) = LABEL9(1,2)
      LABEL9(72,2) = LABEL9(1,2)
      LABEL9(73,2) = LABEL9(7,2)
      LABEL9(74,2) = LABEL9(7,2)
      LABEL9(75,2) = LABEL9(7,2)
      LABEL9(76,2) = LABEL9(7,2)
      LABEL9(77,2) = LABEL9(7,2)
      LABEL9(78,2) = LABEL9(7,2)
      LABEL9(79,2) = ' IN '
      LABEL9(80,2) = LABEL9(79,2)
      LABEL9(81,2) = LABEL9(79,2)
      LABEL9(82,2) = ' OUT '
      LABEL9(83,2) = LABEL9(49,2)
      LABEL9(84,2) = LABEL9(49,2)
      LABEL9(85,2) = LABEL9(67,1)
      LABEL9(86,2) = LABEL9(67,1)
      LABEL9(87,2) = LABEL9(67,1)
      LABEL9(88,2) = LABEL9(70,1)
      LABEL9(89,2) = LABEL9(70,1)
      LABEL9(90,2) = LABEL9(70,1)
      LABEL9(91,2) = LABEL9(28,2)
      LABEL9(92,2) = LABEL9(28,2)
      LABEL9(93,2) = LABEL9(28,2)
      LABEL9(94,2) = ' MASS BAL '
      LABEL9(95,2) = LABEL9(61,1)
      LABEL9(96,2) = LABEL9(61,1)
      LABEL9(97,2) = ' ET '
      LABEL9(98,2) = LABEL9(61,1)
      LABEL9(99,2) = LABEL9(61,1)
      LABEL9(1,3) = ' TOTAL '
      LABEL9(2,3) = ' TIME STEP '
      LABEL9(3,3) = ' RATE '
      DO 771 K3 = 3,69,3
      LABEL9(K3+1,3) = LABEL9(1,3)
      LABEL9(K3+2,3) = LABEL9(2,3)
      LABEL9(K3+3,3) = LABEL9(3,3)
 771  CONTINUE
      END IF
!
!   INITIALIZE MASS BALANCE VARIABLES USED FOR CURRENT
!   TIME STEP
!
      BLTEMP=0.0D0
      BL(3)=0.0D0
      BL(6)=0.0D0
      BL(9)=0.0D0
      BL(12)=0.0D0
      BL(27)=0.0D0
      BL(29)=0.0D0
      BL(36)=0.0D0
      BL(39)=0.0D0
      BL(42)=0.0D0
      BL(45)=0.0D0
      BL(60)=0.0D0
      BL(62)=0.0D0
      BL(51)=0.0D0
      BL(48)=0.0D0
      BL(69)=0.0D0
      BL(72)=0.0D0
      BL(75)=0.0D0
      BL(78)=0.0D0
      BL(81)=0.0D0
      BL(84)=0.0D0
      BL(92)=0.0D0
      bcmf = 0.0D0
      bcmh = 0.0D0
      bltemp2 = 0.0D0
      Do 111 M=1,nsol
      BLSOL(M,3) = 0.0D0
      BLSOL(M,6) = 0.0D0
      BLSOL(M,9) = 0.0D0
      BLSOL(M,12) = 0.0D0
      BLSOL(M,15) = 0.0D0
      BLSOL(M,18) = 0.0D0
      BLSOL(M,27) = 0.0D0
      BLSOL(M,29) = 0.0D0
      BLSOL(M,35) = 0.0D0
      BCMT(M) = 0.0D0
   
 111  continue
      if (f7p) then
       do 12 i=1,numBF
        currentBF(i,1) = 0.0d0
        currentBF(i,2) = 0.0d0
        currentBF(i,3) = 0.0d0
 12     continue
      end if
      DO 21 J=2,NLYY
      DO 21 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(HX(IN).EQ.0.0D0) GO TO 21
      JM1=IN-1
      JP1=IN+1
      NM1=IN-NLY
      NP1=IN+NLY
      VOL=DXR(N)*DELZ(J)
      IF(RAD)VOL=PI2*RX(N)*DXR(N)*DELZ(J)
!
!     SUM CHANGE IN STORAGE
!
      IF(HEAT)THEN
      GSF=VOL*(THETA(IN)*RHO(IN)-THLST(IN)*RHOOLD(IN))
      ELSE  
      GSF=VOL*(THETA(IN)-THLST(IN))
      END IF
      JJ=JTEX(IN)
      SS=HK(JJ,2)/HK(JJ,3)
      IF(HEAT)THEN
      GSS=VOL*THETA(IN)*SS*RHO(IN) 
      ELSE  
      GSS=VOL*THETA(IN)*SS
      END IF
      bltemp2 = (GSF+GSS*(P(IN)-PXXX(IN)))
      bl(29) = bl(29) + bltemp2
      if(bltemp2.ge.0.0d0) then
       if (.not.(ntyp(in).eq.1.and.jflag1.eq.1))&
        bl29I = bl29I + bltemp2
       else
       if (.not.(ntyp(in).eq.1.and.jflag1.eq.1))&
        bl29O = bl29O + bltemp2
      end if
      if(ntyp(in).eq.1.and.jflag1.eq.1)& 
       bcmf = bcmf + bltemp2
     
      IF(HEAT) THEN
!
!   FOR TRANSPORT SUM CHANGE IN STORAGE AND DIFFUSIVE/DISPERSIVE
!   FLUXES
!
!     IF(NCTYP(IN).NE.1) BL(68)=BL(68)+VOL*(
!
!  CHANGE 8-12-91 FOR STORAGE
!
!    *CC(IN)*THETA(IN)*(1+SS*P(IN))-COLD(IN)*THLST(IN)*(1+SS*PXXX(IN)))
!      bltemp2=VOL*(
!      IF(NCTYP(IN).NE.1) BL(68)=BL(68)+VOL*(
!     *CC(IN)*(HT(JJ,5)+THETA(IN)*RHO(IN)*HT(JJ,11)*(2.0d0+
!     *SS*(P(IN)-PXXX(IN))))-cc(in)*thlst(in)*rho(in)*ht(jj,11)
!     *-COLD(IN)*(theta(IN)*RHOOLD(IN)*HT(JJ,11)+ht(jj,5)))

!*****************************
! following changes made 7-3-04 to correct way that dctheta/dt is
! calculated
!*****************************
      bltemp2=VOL*( &
      TT(IN)*(HT(JJ,5)+THETA(IN)*RHO(IN)*HT(JJ,6)*(1.0d0+ &
      SS*(P(IN)-PXXX(IN))))-ttold(in)*(thlst(in)*RHO(IN)*ht(jj,6)&
      +ht(jj,3)))
      bl(95) = bl(95) + bltemp2
      if (jflag1.eq.1) then
       if(ntyp(in).eq.1) bcmt = bcmt + TT(IN)*(theta(in) - thlst(in)) &
         *RHOOLD(IN)*HT(JJ,6)*vol
       if(nhtyp(in).eq.1) bcmt = bcmt + (theta(in)*RHOOLD(IN)*HT(JJ,6) &
         +ht(jj,3))*(TT(IN) - TTOLD(IN))*vol
      end if
      if(bltemp2.ge.0.0D0) then
       if (.not.((jflag1.eq.1).and.(ntyp(in).eq.1.or.nhtyp(in).eq.1))) &
         bl95I = bl95I + bltemp2
      else
       if (.not.((jflag1.eq.1).and.(ntyp(in).eq.1.or.nhtyp(in).eq.1))) &
         bl95O = bl95O + bltemp2
      end if
      END IF
!
!
!   
      IF(SOLUTE) THEN
        DO 20 M=1,Nsol
        bltemp2=VOL*(&
      CC(M,IN)*THETA(IN)*(1.0d0+SS*(P(IN)-PXXX(IN)))-ccold(m,in)&
      *thlst(in))
      BLSOL(M,29)=BLSOL(M,29)+ bltemp2
      BLSOL(M,35)= BLSOL(M,35)+ vol*theta(in)*(CC(M,IN)-CCBR(M,IN)) 
      if (jflag2.eq.1) then
       if(ntyp(in).eq.1.or.nctyp(in).eq.1) bcmt(M) =bcmt(M) + bltemp2
!      if(nctyp(in).eq.1)bcmt(N)=bcmt(N)+(theta(in)*(cc(m,in)- &
!       ccold(m,in)))*vol
      end if
      if(bltemp2.ge.0.0D0) then
      if (.not.((jflag2.eq.1).and.(ntyp(in).eq.1.or.nctyp(in)&
       .eq.1)))  bl62I(M) = bl62I(M) + bltemp2
      else
       if (.not.((jflag2.eq.1).and.(ntyp(in).eq.1.or.nctyp(in)&
      .eq.1)))bl62O(M) = bl62O(M) + bltemp2
      end if 
  20  continue
      end if    
      IF(NHTYP(IN).EQ.2) THEN
      IF(TS(IN).LT.0.0D0) THEN
      BL(84)=BL(84)+TS(IN)
      ELSE
      BL(81)=BL(81)+TS(IN)
      END IF
      END IF
      IF(NCTYP(IN).EQ.2) THEN
        do 22 M1=1,Nsol
      IF(CSS(M1,IN).LT.0.0D0) THEN
!      BL(51)=BL(51)+CSS(M1,IN)
      BLSOL(M1,18)=BLSOL(M1,18)+ CSS(M1,IN)
      ELSE
!      BL(48)=BL(48)+CSS(M1,IN)
      BLSOL(M1,15)=BLSOL(M1,15)+CSS(M1,IN)
      END IF
  22  CONTINUE
      END IF
      IF(NHTYP(IN).EQ.1) THEN

      IF (CIT) THEN
       T5=A(IN)*(TT(IN)-TT(NM1))+ao(in)*(TTOLD(IN)-TTOLD(NM1))+&
      B(IN)*(TT(IN)-TT(JM1))+bo(in)*(TTOLD(IN)-TTOLD(JM1))+&
      C(IN)*(TT(IN)-TT(NP1))+co(in)*(TTOLD(IN)-TTOLD(NP1))+&
      D(IN)*(TT(IN)-TT(JP1))+do(in)*(TTOLD(IN)-TTOLD(JP1))

       ao(in) = a(in)
       bo(in) = b(in)
       co(in) = c(in)
       do(in) = d(in)
         
        IF(JFLAG1.EQ.1) THEN
        AO(IN)=0.5D0*AO(IN)
        BO(IN)=0.5D0*BO(IN)
        CO(IN)=0.5D0*CO(IN)
        DO(IN)=0.5D0*DO(IN)
        END IF
      ELSE
       T5=A(IN)*(TT(IN)-TT(NM1))+B(IN)*(TT(IN)-TT(JM1))+&
      C(IN)*(TT(IN)-TT(NP1))+D(IN)*(TT(IN)-TT(JP1))
      END IF

      if (f7p) then
       do 14 ib1 = 1,numBF
       do 14 ib2 = 1,numcellsBF(ib1)
        if (in.eq.nodenum(ib1,ib2)) then
           currentBF(ib1,3) = currentBF(ib1,3) + T5
           go to 13
        end if
 14     continue
      end if
 13   continue
      IF(T5.LT.0.0D0) THEN
      BL(84)=BL(84)+T5
      ELSE
      BL(81)=BL(81)+T5
      END IF
      END IF
      IF(NCTYP(IN).EQ.1) THEN
        DO 23 M=1,Nsol
        IF (CIT) THEN
      T6=AS(IN)*(CC(M,IN)-CC(M,NM1))+aoc(in)*(CCOLD(M,IN)-&
      CCOLD(M,NM1))+BS(IN)*(CC(M,IN)-CC(M,JM1))+boc(in)*(CCOLD(M,IN)-&
      CCOLD(M,JM1))+CS(IN)*(CC(M,IN)-CC(M,NP1))+coc(in)*(CCOLD(M,IN)&
      -CCOLD(M,NP1))+DS(IN)*(CC(M,IN)-CC(M,JP1))+doc(in)*(CCOLD(M,IN)&
      -CCOLD(M,JP1))
      if(M.EQ.Nsol) then
       aoc(in) = as(in)
       boc(in) = bs(in)
       coc(in) = cs(in)
       doc(in) = ds(in)
       IF(JFLAG2.EQ.1) THEN
        AOC(IN)=0.5D0*AOC(IN)
        BOC(IN)=0.5D0*BOC(IN)
        COC(IN)=0.5D0*COC(IN)
        DOC(IN)=0.5D0*DOC(IN)          
       END IF
       END IF
      ELSE
      T6=AS(IN)*(CC(M,IN)-CC(M,NM1))+BS(IN)*(CC(M,IN)-CC(M,JM1))+&
      CS(IN)*(CC(M,IN)-CC(M,NP1))+DS(IN)*(CC(M,IN)-CC(M,JP1))
      END IF
      if (f7p) then
       do 16 ib1 = 1,numBF
       do 16 ib2 = 1,numcellsBF(ib1)
        if (in.eq.nodenum(ib1,ib2)) then
           currentBF(ib1,3) = currentBF(ib1,3) + T6
           go to 15
        end if
 16     continue
      end if
 15   continue 
      IF(T6.LT.0.0D0) THEN
!      BL(51)=BL(51)+T6
      BLSOL(M,18)=BLSOL(M,18)+ T6
      ELSE
!      BL(48)=BL(48)+T6
      BLSOL(M,15)=BLSOL(M,15)+T6 
      END IF
  23  continue    
      END IF
       
!
!  FLUX FOR NEUMANN CELLS
!
      IF(NTYP(IN).EQ.2.or.ntyp(in).eq.7) THEN
      IF(QQ(IN).LE.0.0D0) THEN
      IF((HEAT.AND.SOLUTE).OR.HEAT) THEN
      BL(12)=BL(12)+QQ(IN)*RHO(IN)
      ELSE  
      BL(12)=BL(12)+QQ(IN)
      END IF
      IF(HEAT) BL(78)=BL(78)+QQ(IN)*TT(IN)*RHO(IN)*HT(JJ,6)
      IF(SOLUTE)then
      do 24 M2=1,Nsol  
!      BL(45)=BL(45)+QQ(IN)*CC(M2,IN)
      BLSOL(M2,12)=BLSOL(M2,12)+QQ(IN)*CC(M2,IN)
  24  continue  
      end if
      ELSE
      IF((HEAT.AND.SOLUTE).OR.HEAT) THEN  
      BL(9)=BL(9)+QQ(IN)*RHO(IN)
      ELSE
      BL(9)=BL(9)+QQ(IN)
      END IF
      IF(HEAT)BL(75)=BL(75)+QQ(IN)*TS(IN)*RHO(IN)*HT(JJ,6)
      IF(SOLUTE)THEN
      do 25 M3=1,Nsol  
!      BL(42)=BL(42)+QQ(IN)*CSS(M3,IN)
      BLSOL(M3,9)=BLSOL(M3,9)+ QQ(IN)*CSS(M3,IN)
  25  continue
      END IF
      END IF
      ELSE
!
!  FLUX FOR DIRICHLET CELLS
!
      IF(NTYP(IN).EQ.1) THEN
!      IF(TRANS) THEN
!      QX=QT(IN)
!      ELSE
!      QX=VSFLX1(IN)
!      END IF
      if (.not. trans) qt(in) = vsflx1(in)
!      qx = qt(in)
      if(SOLUTE) then
         qx = qs(in)
        else
         qx = qt(in)
      end if
      IF(QX.LT.0.0D0) THEN  
      BL(3)=BL(3)-QX
      IF(HEAT) BL(69)=BL(69)-QX*TS(IN)*HT(JJ,6)
      IF(SOLUTE)then
        do 26 M4=1,Nsol  
!      BL(36)=BL(36)-QX*CSS(M4,IN)
      BLSOL(M4,3)=BLSOL(M4,3)-QX*CSS(M4,IN)
  26  continue  
      end if
      ELSE
      BL(6)=BL(6)-QX
      IF(HEAT) BL(72)=BL(72)-QX*TT(IN)*HT(JJ,6)
      IF(SOLUTE)then
        do 27 M5=1,Nsol
!      BL(39)=BL(39)-QX*CSS(M5,IN)
      BLSOL(M5,6)=BLSOL(M5,6)-QX*CC(M5,IN)
  27  continue  
      end if
      END IF
      ELSE
!
!    SUM SOURCES AND SINKS
!     
      IF((HEAT.AND.SOLUTE).OR.HEAT) THEN
      BL(27)=BL(27)+Q(IN)*RHO(IN)
      ELSE
      BL(27)=BL(27)+Q(IN)
      END IF  
      if(NPV.ge.0) then
      IF(HEAT.AND.NTYP(IN).NE.5) BL(93)=BL(93)+Q(IN)*TT(IN)*RHO(IN)&
      *HT(JJ,6)  
      IF(SOLUTE.AND.NTYP(IN).NE.5)then
        do 28 M6=1, Nsol
!      BL(60)=BL(60)+Q(IN)*CC(M6,IN)
      BLSOL(M6,27)=BLSOL(M6,27)+Q(IN)*CC(M6,IN)
  28  continue    
      end if
      end if
      END IF
      end if
   21 CONTINUE
!
!   ACCUMULATE VALUES FOR TOTAL ELAPSED SIMULATION TIME
!
      if (cit) then
       if(jflag1.eq.1) then
        bltemp69 = bl(69)
        bltemp72 = bl(72)
        bltemp75 = bl(75)
        bltemp78 = bl(78)
        bltemp91 = bl(91)
       else
        bltemp1 = 0.5d0*(bl(69)+bltemp69)
        bltemp69 = bl(69)
        bl(69) = bltemp1
        bltemp1 = 0.5d0*(bl(72)+bltemp72)
        bltemp72 = bl(72)
        bl(72) = bltemp1
        bltemp1 = 0.5d0*(bl(75)+bltemp75)
        bltemp75 = bl(75)
        bl(75) = bltemp1
        bltemp1 = 0.5d0*(bl(78)+bltemp78)
        bltemp78 = bl(78)
        bl(78) = bltemp1
        bltemp1 = 0.5d0*(bl(91)+bltemp91)
        bltemp91 = bl(91)
        bl(91) = bltemp1
       end if
       IF (SOLUTE) THEN
         do 30 N=1,Nsol
       if(JFLAG2.eq.1) then
        bltemp36(N) = BLSOL(N,3)
        bltemp39(N) = BLSOL(N,6)
        bltemp42(N) = BLSOL(N,9)
        bltemp45(N) = BLSOL(N,12)
        bltemp60(N) = BLSOL(N,27)
      else
        bltemp1 = 0.5d0*(BLSOL(N,3)+bltemp36(N))
        bltemp36(N) = BLSOL(N,3)
        BLSOL(N,3) = bltemp1
        bltemp1 = 0.5d0*(BLSOL(N,6)+bltemp39(N))
        bltemp39(N) = BLSOL(N,6)
        BLSOL(N,6) = bltemp1
        bltemp1 = 0.5d0*(BLSOL(N,9)+bltemp42(N))
        bltemp42(N) = BLSOL(N,9)
        BLSOL(N,9) = bltemp1
        bltemp1 = 0.5d0*(BLSOL(N,12)+bltemp45(N))
        bltemp45(N) = BLSOL(N,12)
        BLSOL(N,12) = bltemp1
        bltemp1 = 0.5d0*(BLSOL(N,27)+bltemp60(N))
        bltemp60(N) = BLSOL(N,27)
        BLSOL(N,27) = bltemp1
       end if
  30   continue
       END IF
      end if
      BL(24)=ETOUT
       BL(21)=ETOUT1
      BL(30)=BL(29)/DELT
      BL(15)=BL(3)+BL(9)
      BL(18)=BL(6)+BL(12)
      DO 31 I=2,26,3
      BL(I)=DELT*BL(I+1)
   31 CONTINUE
      BL(19)=BL(19)+BL(20)
      BL(22)=BL(22)+BL(23)
      BL(1)=BL(1)+BL(2)
      BL(4)=BL(4)+BL(5)
      BL(10)=BL(10)+BL(11)
      BL(13)=BL(13)+BL(14)
      BL(7)=BL(7)+BL(8)
      BL(16)=BL(16)+BL(17)
      BL(25)=BL(25)+BL(26)
      BL(28)=BL(28)+BL(29)
      BL(32)=BL(14)+BL(17)+BL(26)-BL(29) + bcmf
      BL(33)=BL(32)/DELT
      BL(31)=BL(31)+BL(32)
      bcmft = bcmft + bcmf
      bcmfr = bcmf/delt
      bl29IT = bl29IT + bl29I
      bl29OT = bl29OT + bl29O
      IF(HEAT) THEN
!
!   TRANSPORT MASS BALANCE COMPONENTS
!
      BL(94)=BL(94)+BL(95)
      BL(96)=BL(95)/DELT
      BL(87)=BL(69)+BL(75)+BL(81)
      BL(90)=BL(72)+BL(78)+BL(84)
      DO 40 I=68,92,3
      BL(I)=DELT*BL(I+1)
   40 CONTINUE
      BL(82)=BL(82)+BL(83)
      BL(79)=BL(79)+BL(80)
      BL(91)=BL(91)+BL(92)
      BL(94)=BL(94)+BL(95)
      BL(70)=BL(70)+BL(71)
      BL(76)=BL(76)+BL(77)
      BL(85)=BL(85)+BL(86)
      BL(73)=BL(73)+BL(74)
      BL(88)=BL(88)+BL(89)
      BL(98)=BL(86)+BL(89)+BL(92)-BL(95)+ bcmh
      BL(99)=BL(98)/DELT
      BL(97)=BL(97)+BL(98)
      bcmht = bcmht + bcmh
      bcmhr = bcmh/delt
      bl95IT = bl95IT + bl95I
      bl95OT = bl95OT + bl95O
      bl95T = bl95T
      END IF
      IF(SOLUTE) THEN
!
!   TRANSPORT MASS BALANCE COMPONENTS
!
      do 50 N=1,Nsol
      BLSOL(N,28)=BLSOL(N,28)+BLSOL(N,29)
      BLSOL(N,30)=BLSOL(N,29)/DELT
      BLSOL(N,21)=BLSOL(N,3)+BLSOL(N,9)+BLSOL(N,15)
      BLSOL(N,24)=BLSOL(N,6)+BLSOL(N,12)+BLSOL(N,18)
      DO 41 I=2,26,3
      BLSOL(N,I)=DELT*BLSOL(N,I+1)
   41 CONTINUE
      BLSOL(N,16)=BLSOL(N,16)+BLSOL(N,17)
      BLSOL(N,13)=BLSOL(N,13)+BLSOL(N,14)
      BLSOL(N,25)=BLSOL(N,25)+BLSOL(N,26)
      BLSOL(N,1)=BLSOL(N,1)+BLSOL(N,2)
      BLSOL(N,4)=BLSOL(N,4)+BLSOL(N,5)
      BLSOL(N,10)=BLSOL(N,10)+BLSOL(N,11)
      BLSOL(N,19)=BLSOL(N,19)+BLSOL(N,20)
      BLSOL(N,7)=BLSOL(N,7)+BLSOL(N,8)
      BLSOL(N,22)=BLSOL(N,22)+BLSOL(N,23)
      BLSOL(N,32)=BLSOL(N,20)+BLSOL(N,23)+BLSOL(N,26) &
      -BLSOL(N,29)+ BLSOL(N,35) + bcmt(N)
      BLSOL(N,33)=BLSOL(N,32)/DELT
      BLSOL(N,31)=BLSOL(N,31)+BLSOL(N,32)
      BLSOL(N,36)=BLSOL(N,35)/DELT
      BLSOL(N,34)=BLSOL(N,34)+BLSOL(N,35)
      bcmtt(N) = bcmtt(N) + bcmt(N)
      bcmtr(N) = bcmt(N)/delt
      bl62IT (N)= bl62IT(N) + bl62I(N)
      bl62OT (N)= bl62OT (N)+ bl62O(N)
 50   continue     
      END IF
!
!   WRITE RESULTS TO FILE 9
!
      IF(F9P) then
       if(ktim.eq.1) then
        if(o13p) then
         write(09,4002) ' TIME ',(label9(mb9(im),1), im=1,nmb9)
         write(09,4002) '      ',(label9(mb9(im),2), im=1,nmb9)
         write(09,4002) '      ',(label9(mb9(im),3), im=1,nmb9)
        else
         write(09,4001) ' TIME ',(label9(mb9(im),1), im=1,nmb9)
         write(09,4001) '      ',(label9(mb9(im),2), im=1,nmb9)
         write(09,4001) '      ',(label9(mb9(im),3), im=1,nmb9)
        end if
       end if
       if(.not.o9p.or.jplt.eq.1) then
        do 42 IM = 1,NMB9
         if(dabs(BL(MB9(IM))).lt.elimit1) then
          dum(IM) = elimit2
         else
          dum(IM) = BL(MB9(IM))
         end if
 42      continue
        if(o13p) then
!         WRITE(09,4003) STIM,(BL(MB9(IM)),IM=1,NMB9)
         WRITE(09,4003) STIM,(dum(IM),IM=1,NMB9)
        else
         WRITE(09,4000) STIM,(dum(IM),IM=1,NMB9)
!         WRITE(09,4000) STIM,(BL(MB9(IM)),IM=1,NMB9)
        end if
       end if
      end if
!
!  revision Aug 2008 to print boundary segment fluxes
!
      if(f7p) then
       if (ktim.eq.1) then
	  if (o13p) then
         write (7,4014)
	  else
         write (7,4011)
	  end if
       end if
       do 46 ib1 = 1,numBF
        iflag7 = 0
        do 45 ib2 = 1,numcellsBF(ib1)
         in1 = nodenum(ib1,ib2)
         if(ntyp(in1).eq.1) then
          iflag7 = 1
!          qx = qt(in1)
          if(SOLUTE) then
	       qx = qs(in1)
	      else
	       qx = qt(in1)
          end if
          currentBF(ib1,1) = currentBF(ib1,1) - qx
          if (HEAT) then
           if (qt(in1).lt.0.0) then
            currentBF(ib1,2) = currentBF(ib1,2) - qx*TS(in1)
           else
            currentBF(ib1,2) = currentBF(ib1,2) - qx*TT(in1)
           end if
          end if
         else
          if (ntyp(in1).eq.2.or.ntyp(in1).eq.7) then
           iflag7 = 1
           currentBF(ib1,1) = currentBF(ib1,1) + qq(in1)
           if (HEAT) then
            if (qq(in1).lt.0.0) then
             currentBF(ib1,2) = currentBF(ib1,2) + qq(in1)*TT(in1)
            else
             currentBF(ib1,2) = currentBF(ib1,2) + qq(in1)*TS(in1)
            end if
           end if
          end if
         end if
 45     continue
        bl_flux = currentBF(ib1,1)*delt
        totalBF(ib1,1) = totalBF(ib1,1) + bl_flux
        if(nctyp(in1).eq.1.or.nctyp(in1).eq.2) iflag7 = 1
!
!  revision 10-15-10 for centered in time
!
        if (CIT) then
         if (jflag1.eq.1) then
          bl_mass = currentBF(ib1,2)
          currentBF(ib1,4) = bl_mass
          bl_mass = bl_mass + currentBF(ib1,3)
         else
          bltemp1 = currentBF(ib1,2)
          bl_mass = 0.5*(bltemp1 + currentBF(ib1,4)) + currentBF(ib1,3)
          currentBF(ib1,4) = bltemp1
         end if
        else
         bl_mass = currentBF(ib1,2) + currentBF(ib1,3)
        end if
        bltemp1 = bl_mass*delt
        totalBF(ib1,2) = totalBF(ib1,2) + bltemp1
!
!  end revision 10-15-10
!
!       bl_mass = currentBF(ib1,2)*delt
!        totalBF(ib1,2) = totalBF(ib1,2) + bl_mass
        if((iflag7.eq.1).and.(.not.o9p.or.jplt.eq.1)) then
         if(dabs(totalBF(ib1,1)).lt.elimit1)totalBF(ib1,1)=elimit2
         if(dabs(bl_flux).lt.elimit1) bl_flux = elimit2
         if(dabs(currentBF(ib1,1)).lt.elimit1)currentBF(ib1,1)=elimit2
         if(dabs(bltemp1).lt.elimit1)bltemp1 = elimit2
         if(dabs(bl_mass).lt.elimit1) bl_mass = elimit2
         if(dabs(totalBF(ib1,2)).lt.elimit1)totalBF(ib1,2)=elimit2
        if (o13p) then
         write(7,4013) stim,idBF(ib1),totalBF(ib1,1),bl_flux, &
          currentBF(ib1,1),totalBF(ib1,2),bltemp1,bl_mass
        else
         write(7,4012) stim,idBF(ib1),totalBF(ib1,1),bl_flux, &
          currentBF(ib1,1),totalBF(ib1,2),bltemp1,bl_mass
        end if   
        end if
 46     continue
      end if
!
!  end Aug 2008 revision
!
      IF(.NOT.F6P.AND.JPLT.NE.1.AND.JSTOP.NE.1.AND.JFLAG.NE.1) GO TO 51
!
!    WRITE RESULTS OF MASS BALANCE TO FILE 6
!
      do 47 m = 13,72
       if(dabs(bl(m)).lt.elimit1) bl(m) = elimit2
 47    continue
      WRITE (06,4010) KTIM,KP,STIM,TUNIT,ZUNIT,ZUNIT,ZUNIT,TUNIT,(BL(M),&
      M=1,12)
      WRITE(06,4020) (BL(M),M=13,27),bcmft,bcmf,bcmfr,(bl(m),m=28,33)
      IF(HEAT) WRITE(06,4030) HUNX,HUNX,HUNX,TUNIT,(BL(M),M=67,93),&
      bcmht,bcmh,bcmhr,(BL(M), M=94,99)
      IF(SOLUTE)THEN
      DO 230 N=1,Nsol
      SCOMPNAME(N)=COMPNAME(N)
      WRITE(06,4031)SCOMPNAME(N)
      WRITE(06,4032) CUNX,CUNX,CUNX,TUNIT,(BLSOL(N,I),I=1,27),&
      (BLSOL(N,I),I=34,36),bcmtt(N),bcmt(N),bcmtr(N), &
      (BLSOL(N,I),I=28,33)  
 230  CONTINUE       
      END IF  
      WRITE(06,4040)
   51 CONTINUE
      jflag1 = jflag
      jflag2 = jflag
      RETURN
 4000 FORMAT(1pe14.6,73(1PE11.3))
 4003 FORMAT(1pe18.10,73(1PE21.13))
 4001 format(a14,1X,73a11)
 4002 format(a18,1X,73a21)
 4010 FORMAT(21X,10(1H-),1X,'MASS BALANCE SUMMARY FOR TIME STEP', &
       I9,1X,10(1H-)/25X,'RECHARGE PERIOD NUMBER ',I9/25X, &
      'TOTAL ELAPSED SIMULATION TIME = ',1PE14.6,1X,A4 &
      //2X,128('+')/2X,'+',126X,'+'/ &
      2X,'+',90X,' TOTAL THIS',10X,'RATE THIS',6X,'+'/2X,'+', &
      33X,'VOLUMETRIC FLOW BALANCE', &
      18X,'TOTAL   ',9X,'TIME STEP',11X,' TIME STEP',5X,'+'/ &
      2X,'+',72X,A4,'**3',13X,A4,'**3',11X,A4,'**3/',A4,4X,'+'/ &
      2X,'+',4X,'FLUX INTO DOMAIN ACROSS SPECIFIED PRESSURE HEAD', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',2X,'FLUX OUT OF DOMAIN ACROSS SPECIFIED PRESSURE HEAD', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',13X,'FLUX INTO DOMAIN ACROSS SPECIFIED FLUX BOUNDARIES', &
      1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',11X,'FLUX OUT OF DOMAIN ACROSS SPECIFIED FLUX', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+')
 4011 FORMAT('     TIME      BNDY    WATER      WATER      WATER', &
      '      SOLUTE     SOLUTE     SOLUTE'/,15X,'FACE     FLUX', &
      '       FLUX       FLUX       FLUX       FLUX       FLUX',/ &
      23X,'TOTAL      TIME STEP   RATE      TOTAL      TIME STEP', &
      '   RATE')
 4012 FORMAT(1pe14.6,i6,73(1PE11.3))
 4013 FORMAT(1pe18.10,i6,73(1PE21.13))
 4014 FORMAT(5x,'TIME',11x,'BNDY',4x,3('WATER',16x),1x,3('HEAT',17x),/ &
      20x,'FACE',5x,6('FLUX',17x),/,28x,2('TOTAL',16x,'TIME STEP',13x,'RATE',16x))
 4020 FORMAT(1H ,1X,'+',40X,'TOTAL FLUX INTO DOMAIN --  ',2(1PE15.5,5X), &
       1PE15.5,4X,'+'/2X,'+',38X,'TOTAL FLUX OUT OF DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',51X,'EVAPORATION --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &  
      2X,'+',49X,'TRANSPIRATION --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/  &
      2X,'+',38X,'TOTAL EVAPOTRANSPIRATION', &
      1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',30X,'      FLUID FROM BOUNDARY CHANGE --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/ &
     2X,'+',30X,'CHANGE IN FLUID STORED IN DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/2X,'+',42X,'FLUID VOLUME BALANCE' &
      ,1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/2X,'+',126X,'+')
 4030 FORMAT(2X,'+',126X,'+',/,2X,'+',35X,'ENERGY MASS BALANCE', &
      72X,'+',/,2X,'+',74X,A4,16X,A4,14X,A4,'/',A4,5X,'+',/, &
      2X,'+',4X,'FLUX INTO DOMAIN ACROSS SPECIFIED PRESSURE HEAD', & 
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',2X,'FLUX OUT OF DOMAIN ACROSS SPECIFIED PRESSURE HEAD', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',13X,'FLUX INTO DOMAIN ACROSS SPECIFIED FLUX BOUNDARIES', &
      1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',11X,'FLUX OUT OF DOMAIN ACROSS SPECIFIED FLUX', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ , &
      2X,'+',25X,'          CONDUCTIVE FLUX INTO DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/2X, &
      '+',23X,'          CONDUCTIVE FLUX OUT OF DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/, &
      1H ,1X,'+',40X,'TOTAL FLUX INTO DOMAIN --  ',2(1PE15.5,5X), &
       1PE15.5,4X,'+'/2X,'+',38X,'TOTAL FLUX OUT OF DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',38X,'TOTAL EVAPOTRANSPIRATION', &
      1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      '  +',35X,'ENERGY FROM BOUNDARY CHANGE', &
      ' --',1PE17.5,2(1PE20.5),4X,'+'/ &
      2X,'+',29X,'CHANGE IN ENERGY STORED IN DOMAIN --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/2X,'+',43X,'ENERGY MASS BALANCE' &
      ,1X,'--  ',2(1PE15.5,5X),1PE15.5,4X,'+'/2X,'+',126X,'+') 
 4031 FORMAT(A10)       
 4032 FORMAT(2X,'+',126X,'+',/,2X,'+',35X,'SOLUTE MASS BALANCE', & 
      72X,'+',/,2X,'+',74X,A4,16X,A4,14X,A4,'/',A4,5X,'+',/, &
      2X,'+',4X,'FLUX INTO DOMAIN ACROSS SPECIFIED PRESSURE HEAD', &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2X,'+',2X,'FLUX OUT OF DOMAIN ACROSS SPECIFIED PRESSURE HEAD' &
      1X,'BOUNDARIES --  ',2(1PE15.5,5X),1PE15.5,4X,'+'/ &
      2x,'+',13X,'FLUX INTO DOMAIN ACROSS SPECIFIED FLUX BOUNDARIES --' &
      ,'  ', &
      2(1PE15.5,5X),1PE15.5,4x,'+',/,2X,'+',11X, &
      'FLUX OUT OF DOMAIN ACROSS SPECIFIED FLUX BOUNDARIES --  ', &
      2(1PE15.5,5X),1PE15.5,4X,'+'/,2X,'+',25X, &
      'DIFFUSIVE/DISPERSIVE FLUX INTO DOMAIN --  ',2(1PE15.5,5X),1PE15.5 &
      ,4X,'+'/2X,'+',23X,'DIFFUSIVE/DISPERSIVE FLUX OUT OF DOMAIN --', &
      1PE17.5,2(1PE20.5),4X,'+'/'  +',40X,'TOTAL FLUX INTO DOMAIN --', & 
      1PE17.5,2(1PE20.5),4X,'+'/'  +',38X,'TOTAL FLUX OUT OF DOMAIN --', &
      1PE17.5,2(1PE20.5),4X,'+'/'  +',38X,'TOTAL EVAPOTRANSPIRATION --', &
      1PE17.5,2(1PE20.5),4X,'+'/'  +',21X,'CHANGE IN SOLUTE DUE TO', &
      1x,'CHEMICAL REACTION --', &
      1PE17.5,2(1PE20.5),4X,'+'/'  +',35X,'SOLUTE FROM BOUNDARY CHANGE', &
      ' --',1PE17.5,2(1PE20.5),4X,'+'/ &
      '  +',29X,'CHANGE IN SOLUTE STORED IN', &
      ' DOMAIN --',1PE17.5,2(1PE20.5),4X,'+'/'  +',43X, &
      'SOLUTE MASS BALANCE --',1PE17.5,2(1PE20.5),4X,'+'/'  +',126X,'+')
 4040 FORMAT( 2X,128('+'))
      END
      DOUBLE PRECISION FUNCTION VSFLX1(IN)
!******
!VSFLX1
!******
!   PURPOSE: TO COMPUTE INTERCELL MASS FLUX RATES FOR DIRICHLET
!   BOUNDARY NODES
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use kcon
      use press
      use hcon
      use equat
      use temp
      use pit
      use isdumm
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/WGT/WUS,WDS
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
!----------------------------------------------------------------------
!
!
!   COMPUTE FLUXES ON ALL FOUR SIDES OF EACH CONSTANT HEAD NODE
!
      JM1=IN-1
      JP1=IN+1
      NP1=IN+NLY
      NM1=IN-NLY
!
!   COMPUTE A,B,C,D
!
      IF(WUS.EQ.0.0D0) THEN
      IF(HEAT)THEN
      A(IN)=HKLL(IN)*DSQRT(HCND(NM1)*RHO(NM1)*HCND(IN)*RHO(IN))
      B(IN)=HKTT(IN)*DSQRT(HCND(JM1)*RHO(JM1)*HCND(IN)*RHO(IN))
      C(IN)=HKLL(NP1)*DSQRT(HCND(NP1)*RHO(NP1)*HCND(IN)*RHO(IN))
      D(IN)=HKTT(JP1)*DSQRT(HCND(JP1)*RHO(JP1)*HCND(IN)*RHO(IN))
      ELSE  
      A(IN)=HKLL(IN)*DSQRT(HCND(NM1)*HCND(IN))
      B(IN)=HKTT(IN)*DSQRT(HCND(JM1)*HCND(IN))
      C(IN)=HKLL(NP1)*DSQRT(HCND(NP1)*HCND(IN))
      D(IN)=HKTT(JP1)*DSQRT(HCND(JP1)*HCND(IN))
      END IF
      ELSE
      IF(P(NM1).GT.P(IN).AND.HX(NM1).NE.0.0D0) THEN
      ALA=WUS
      BTA=WDS
      ELSE
      ALA=WDS
      BTA=WUS
      END IF
      IF(P(JM1).GT.P(IN).AND.HX(JM1).NE.0.0D0) THEN
      ALB=WUS
      BTB=WDS
      ELSE
      ALB=WDS
      BTB=WUS
      END IF
      IF(P(NP1).GT.P(IN).AND.HX(NP1).NE.0.0D0) THEN
      ALC=WUS
      BTC=WDS
      ELSE
      ALC=WDS
      BTC=WUS
      END IF
      IF(P(JP1).GT.P(IN).AND.HX(JP1).NE.0.0D0) THEN
      ALD=WUS
      BTD=WDS
      ELSE
      ALD=WDS
      BTD=WUS
      END IF
     
!
!   DETERMINE FLUXES
!     
      IF(HEAT)THEN
      A(IN)=(ALA*HCND(NM1)*RHO(NM1)+BTA*HCND(IN)*RHO(IN))*HKLL(IN)
      B(IN)=(ALB*HCND(JM1)*RHO(JM1)+BTB*HCND(IN)*RHO(IN))*HKTT(IN)
      C(IN)=(ALC*HCND(NP1)*RHO(NP1)+BTC*HCND(IN)*RHO(IN))*HKLL(NP1)
      D(IN)=(ALD*HCND(JP1)*RHO(JP1)+BTD*HCND(IN)*RHO(IN))*HKTT(JP1)
      ELSE  
      A(IN)=(ALA*HCND(NM1)+BTA*HCND(IN))*HKLL(IN)
      B(IN)=(ALB*HCND(JM1)+BTB*HCND(IN))*HKTT(IN)
      C(IN)=(ALC*HCND(NP1)+BTC*HCND(IN))*HKLL(NP1)
      D(IN)=(ALD*HCND(JP1)+BTD*HCND(IN))*HKTT(JP1)
      END IF
      END IF
      
      if(ntyp(in).eq.1) then
       if(ntyp(nm1).eq.1) a(in) = 0.0d0
       if(ntyp(jm1).eq.1) b(in) = 0.0d0
       if(ntyp(np1).eq.1) c(in) = 0.0d0
       if(ntyp(jp1).eq.1) d(in) = 0.0d0
      end if
      QL=-A(IN)*(P(IN)-P(NM1))
      QA=-B(IN)*(P(IN)-P(JM1))
      QR=-C(IN)*(P(IN)-P(NP1))
      QB=-D(IN)*(P(IN)-P(JP1))
!
!    COMPUTE NET FLUX IN (+) OR OUT (-)
!
      VSFLX1=QL+QR+QA+QB
      RETURN
      END FUNCTION VSFLX1
      SUBROUTINE VSOUTP
!*****
!VSOUTP
!******
!
!   PURPOSE: TO OUTPUT RESULTS AFTER EACH TIME STEP.
!
!-----------------------------------------------------------------------
!
!        SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use mprop
      use press
      use jtxx
      use dumm
      use dumm3
      use trxx
      use trxxh
      use trxv
      use plott
      use rpropsh
      use scon
      use disch
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      COMMON/JCON/JSTOP,JFLAG,jflag1
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      LOGICAL THPT,SPNT,PPNT,HPNT,VPNT
      COMMON/LOG2/F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      COMMON/LOG4/THPT,SPNT,PPNT,HPNT,VPNT
      CHARACTER*80 TITL
      CHARACTER*6 ZUNIT,CUNX,HUNX
      CHARACTER*7 TUNIT
      COMMON/SCHAR/TITL,ZUNIT,TUNIT,CUNX,HUNX
      common/elimit/elimit1,elimit2
      LOGICAL HEAT,SOLUTE
      COMMON/TRANSTYPE/HEAT,SOLUTE
      COMMON/TCON1/NIS,NIS1,NIS3
     
!-------------------------------------------------------------------
!
!   OUTPUT RESULTS TO FILE 11 AT EACH TIME STEP
!
      IF(JSTOP.GT.1) RETURN
      if(.not.o11p.or.jplt.eq.1) then
      IF(F11P) THEN
      IF(.not.(TRANS.OR.VPNT).AND..NOT.SSTATE) CALL VTVELO
        
      DO 10 J=1,NOBS
      N=IJOBS(J)
      I=N/NLY+1
      J1=MOD(N,NLY)
      IF(HX(N).NE.0.0D0) THEN
       if(dabs(p(n)).lt.elimit1) p(n) = elimit2
         IF(HEAT)THEN
            if(dabs(TT(n)).lt.elimit1) TT(n) = elimit2
         END IF
         IF(SOLUTE)THEN
          DO 11 M=1,Nsol
           if(dabs(cc(M,n)).lt.elimit1) cc(M,n) = elimit2  
  11      continue          
         END IF  
       if(dabs(vx(n)).lt.elimit1) vx(n) = elimit2
       if(dabs(vz(n)).lt.elimit1) vz(n) = elimit2
       if(dabs(q(n)).lt.elimit1) q(n) = elimit2
      PPR=HK(JTEX(N),3)
      IF(PPR.EQ.0.0D0)PPR=1.0D0
      SAT=THETA(N)/PPR
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J1)
      ELSE
      Z1=DZZ(J1)*CS1+RX(I)*CS2
      END IF
      PHD=P(N)+Z1
      if (o13p) then
      IF(HEAT.AND.SOLUTE) THEN
      WRITE(11,4021) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT,TT(N), &
      vx(n),vz(n),q(n),(CC(M,N),M=1,Nsol)
      else if(HEAT.AND.(.NOT.SOLUTE))then 
      WRITE(11,4021) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT,TT(N), &
      vx(n),vz(n),q(n)
      else if((.NOT.HEAT).AND.SOLUTE)then  
      WRITE(11,4021) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT, &
      vx(n),vz(n),q(n),(CC(M,N),M=1,Nsol)
      ELSE
      WRITE (11,4021) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT, &
       vx(n),vz(n),q(n)
      END IF
      else
      IF(HEAT.AND.SOLUTE) THEN
      WRITE(11,4020) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT,TT(N), &
      vx(n),vz(n),q(n),(CC(M,N),M=1,Nsol)
      else if(HEAT.AND.(.NOT.SOLUTE))then 
      WRITE(11,4020) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT,TT(N), &
      vx(n),vz(n),q(n)
      else if((.NOT.HEAT).AND.SOLUTE)then 
      WRITE(11,4020) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT, &
      vx(n),vz(n),q(n),(CC(M,N),M=1,Nsol)
      ELSE
      WRITE (11,4020) STIM,n,RX(I),DZZ(J1),P(N),PHD,THETA(N),SAT, &
       vx(n),vz(n),q(n)
      END IF
      end if
      END IF
   10 CONTINUE
      end if
      END IF
      IF(KTIM.EQ.0)  GO TO 20
!
!    WRITE TIME STEP HEADER TO FILE 6
!
!    WRITE MAXIMUM HEAD CHANGE EACH TIME STEP TO FILE 7
!
!      IF(F7P) THEN
!      WRITE(07,4040) KTIM,STIM,NIT,NIT1,NIS1
!     WRITE(07,4030) (DHMX(M2),M2=1,NIT)
!      END IF
      WRITE(06,4040) KTIM,STIM,NIT,NIT1,NIS1
      IF(JSTOP.EQ.1.OR.JPLT.EQ.1) GO TO 20
      IF(.NOT.PRNT.AND.JFLAG.EQ.0) RETURN
   20 WRITE (6,4050) TITL,STIM,TUNIT,KTIM
!
!    PRINT SOLUTION FOR CURRENT TIME STEP
!
      IF(JPLT.EQ.1) THEN
!     if (f8p) then
!
!   WRITE PRESSURE HEADS TO FILE 8 AT OBSERVATION TIMES.
!
      WRITE (8,4000) STIM,TUNIT
!       if (o12p) then
!      if (heat.and.solute) then
!       write (12) stim, p, tt,cc
 !      else if(heat.and.(.not.solute))then
 !      write (12) stim, p, tt
 !      else if((.not.heat).and.solute)then
 !        write (12) stim, p,cc
 !     else
 !      write (12) stim, p
 !     end if
 !     end if 
      DO 40 J=1,NLY
      DO 30 N=1,NXR
      IN=NLY*(N-1)+J
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      DUM(IN)=P(IN)+Z1
      if(dabs(dum(in)).lt.elimit1) dum(in) = elimit2
 30   continue
      if(o13p) then
       WRITE(8,4011) (DUM(N),N=J,NNODES-NLY+J,NLY)
      else
       WRITE(8,4010) (DUM(N),N=J,NNODES-NLY+J,NLY)
      end if
 40   continue
!
!  WRITE TEMPERATURE TO FILE 8
!
      IF(HEAT) THEN
      DO 50 J=1,NLY
      do 45 N=1,NXR
      IN = NLY*(N-1)+J
      DUM(IN) = TT(IN)
      if(dabs(dum(in)).lt.elimit1) dum(in) = elimit2
 45   continue
      if (o13p) then
       WRITE(08,4011) (DUM(N),N=J,NNODES-NLY+J,NLY)
      else
       WRITE(08,4010) (DUM(N),N=J,NNODES-NLY+J,NLY)
      end if
  50  CONTINUE
      END IF
!      end if
      
!
!  WRITE CONCENTRATION TO FILE 8
!
!      IF(SOLUTE) THEN
!      use dumm3Alloc
!     DO 51 J=1,NLY   
!      do 46 N=1,NXR
!      IN = NLY*(N-1)+J
!      DO 43 M=1,Nsol  
!      DUM3(M,IN)= CC(M,IN)
!      if(dabs(DUM3(M,IN)).lt.elimit1) DUM3(M,IN) = elimit2
! 43   continue       
! 46   continue
! 51   continue
!      do 52, K=1,NLY 
!      do 52, M=1,Nsol  
!      if (o13p) then  
!       WRITE(08,4011) (DUM3(M,N),N=K,NNODES-NLY+K,NLY)
!      else
!       WRITE(08,4010) (DUM3(M,N),N=K,NNODES-NLY+K,NLY)
!     end if
!52  CONTINUE 
!     use dumm3Dealloc    
!      END IF
      END IF
!      end if     
!
!    PRINT TOTAL HEADS
!
      IF(HPNT) THEN
      WRITE (6,4060)
      CALL VSOUT(1,P)
      END IF
!
!  PRINT PRESSURE HEADS
!
      IF(PPNT) THEN
!      IF(JPLT.NE.1.or..not.f8p) THEN
      DO 60 J=2,NLYY
      DO 60 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      DUM(IN)=P(IN)+Z1
      IF(HX(IN).EQ.0.0D0)DUM(IN)=0.0D0
      THEAD(IN)=DUM(IN)
   60 CONTINUE
!      END IF
      WRITE (6,4070)
      CALL VSOUT(1,DUM)
      END IF
!
!  PRINT SATURATIONS
!
      IF(SPNT) THEN
      DO 70 J=2,NLYY
      DO 70 N=2,NXRR
      IN=NLY*(N-1)+J
      TTX=HK(JTEX(IN),3)
      IF(TTX.EQ.0.0D0) THEN
      DUM(IN)=0.0D0
      ELSE
      DUM(IN)=THETA(IN)/TTX
      END IF
      SATUR(IN)=DUM(IN)
   70 CONTINUE
      WRITE (6,4080)
      CALL VSOUT(2,SATUR)
      END IF
!
!  PRINT MOISTURE CONTENTS
!
      IF(THPT) THEN
      WRITE (6,4090)
      CALL VSOUT(2,THETA)
      END IF
!
!  PRINT VELOCITIES
!
      IF(VPNT.AND.KTIM.GT.0) THEN
      if(nxr.gt.3) then
       WRITE(06,4100)
       CALL VSOUT(1,VX)
      end if
      if(nly.gt.3) then
       WRITE(06,4110)
       CALL VSOUT(1,VZ)
      end if
      END IF
!
!  PRINT temperature
!
      IF(HEAT) THEN
      WRITE(6,4120)
      CALL VSOUT(1,TT)
      END IF
!
!  PRINT CONCENTRATIONS
!
      IF(SOLUTE) THEN
      WRITE(6,4121)
      CALL VSOUTS(1,CC)
      END IF      
      CONTINUE
      RETURN
 4000 FORMAT(/,8H TIME = ,E15.6,1X,A4/)
 4010 FORMAT(99999(1PE15.7))
 4011 FORMAT(99999(1PE21.13))
 4020 FORMAT(1pe14.6,1x,i8,2x,100(1x,1PE12.5))
 4021 FORMAT(1pe18.10,1x,i8,2x,100(1PE21.13))
!
!following change made 6/29/09 for Marie S to print
!  3 digits in exponent
!
! 4020 FORMAT(1pe14.6,1x,i8,2x,12(1x,1PE12.4E3))
! 4021 FORMAT(1pe18.10,1x,i8,2x,12(1PE21.12E3))
 4030 FORMAT(7E11.4)
 4040 FORMAT(' TIME STEP ',I9,'  TIME = ',E14.6,'  NIT = ',I5, &
     '  NIT1 = ',I5,'  NIS1 = ',I5)
 4050 FORMAT(6X,A80/5X,20HTOTAL ELAPSED TIME =,1PE14.6,1X,A4/5X, &
      10HTIME STEP ,I9,//)
 4060 FORMAT(1H ,50X,10HTOTAL HEAD)
 4070 FORMAT(1H ,50X,13HPRESSURE HEAD)
 4080 FORMAT(1H ,50X,10HSATURATION)
 4090 FORMAT(1H ,50X,16HMOISTURE CONTENT)
 4100 FORMAT(51X,'X-VELOCITY')
 4110 FORMAT(51X,'Z-VELOCITY')
 4120 FORMAT(51X,'TEMPERATURE')
 4121 FORMAT(51X,'CONCENTRATION')
      END
      SUBROUTINE VSOUT(IV,VPRNT)
!*****
!VSOUT
!*****
!
!    PURPOSE: TO PRINT TWO DIMENSIONAL ARRAYS
!
!-----------------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
!
      use press
      use rspac
      use kcon
      use dumm1
      use trxxh
      use trxv
      use mprop
      use rpropsh
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      CHARACTER*80 TITL
      CHARACTER*6 ZUNIT,CUNX,HUNX
      CHARACTER*7 TUNIT 
      COMMON/SCHAR/TITL,ZUNIT,TUNIT,CUNX,HUNX 
      common/elimit/elimit1,elimit2
      LOGICAL F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      COMMON/LOG2/F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,  &
      o13p,F14P,F15P,F16P,F17P,F18P,F19P
      LOGICAL THPT,SPNT,PPNT,HPNT,VPNT
      COMMON/LOG4/THPT,SPNT,PPNT,HPNT,VPNT
      DIMENSION VPRNT(1)
!
!-------------------------------------------------------------------
!
      !!@@include 'd_dumm1Alloc.inc'
      allocate(DUM1(NNODES))
      WRITE (06,4000) ZUNIT,ZUNIT
      WRITE (06,4010)(RX(K),K=2,NXRR)
      DO 30 J=2,NLYY
      DO 10 N=2,NXRR
      IN=NLY*(N)-(J-1)
      DUM1(N)=VPRNT(IN)
      IF(HX(IN).EQ.0.0D0) DUM1(N)=0.0D0
      if(dabs(dum1(n)).lt.elimit1) dum1(n) = elimit2
   10 CONTINUE
      IF(IV.GT.1) GO TO 20
      WRITE (06,4020) DZZ(NLY-J+1),(DUM1(N),N=2,NXRR)
      IF((VPRNT(IN).EQ.VX(IN)).OR.(VPRNT(IN).EQ.VZ(IN)))THEN
      IF(J.EQ.2)WRITE (17,*) 
      WRITE (17,4021) (DUM1(N),N=2,NXRR)
      END IF
      IF(VPRNT(IN).EQ.TT(IN))THEN
      IF(J.EQ.2)WRITE (16,*)  
      WRITE (16,4021) (DUM1(N),N=2,NXRR)
      END IF 
      IF(VPRNT(IN).EQ.THETA(IN))THEN
      IF(J.EQ.2)WRITE (18,*)  
      WRITE (18,4021) (DUM1(N),N=2,NXRR)
      END IF
      IF((VPRNT(IN).EQ.P(IN)).OR.(VPRNT(IN).EQ.THEAD(IN)))THEN
      IF(J.EQ.2)WRITE (14,*)  
      WRITE (14,4021) (DUM1(N),N=2,NXRR) 
      END IF
      IF(VPRNT(IN).EQ.SATUR(IN)) THEN
      IF(J.EQ.2)WRITE (19,*)  
      WRITE (19,4021) (DUM1(N),N=2,NXRR) 
      END IF  
      GO TO 30
   20 WRITE (06,4030) DZZ(NLY-J+1),(DUM1(N),N=2,NXRR)
      IF((VPRNT(IN).EQ.VX(IN)).OR.(VPRNT(IN).EQ.VZ(IN)))THEN
      IF(J.EQ.2)WRITE (17,*)  
      WRITE (17,4031) (DUM1(N),N=2,NXRR)
      END IF
      IF(VPRNT(IN).EQ.TT(IN))THEN
      IF(J.EQ.2)WRITE (16,*) 
      WRITE (16,4031) (DUM1(N),N=2,NXRR) 
      END IF
      IF(VPRNT(IN).EQ.THETA(IN))THEN
      IF(J.EQ.2)WRITE (18,*)  
      WRITE (18,4021) (DUM1(N),N=2,NXRR)
      END IF
      IF((VPRNT(IN).EQ.P(IN)).OR.(VPRNT(IN).EQ.THEAD(IN)))THEN
      IF(J.EQ.2)WRITE (14,*)  
      WRITE (14,4031) (DUM1(N),N=2,NXRR) 
      END IF
      IF(VPRNT(IN).EQ.SATUR(IN))THEN
      IF(J.EQ.2)WRITE (19,*)  
      WRITE (19,4031) (DUM1(N),N=2,NXRR)   
      END IF  
   30 CONTINUE
      !!@@include 'd_dumm1Dealloc.inc'
      deallocate(DUM1)
      RETURN
 4000 FORMAT(1H ,1X,5HZ, IN/2X,A4,20X,20HX OR R DISTANCE, IN ,A4)
 4010 FORMAT(1H ,9X,99999(F11.3)/(9X,13(F9.2)))
 4020 FORMAT(1X,F9.3,99999(1X,1PE10.3)/(9X,13(1PE9.2)))
 4021 FORMAT(1X,99999(1X,1PE10.3)/(9X,13(1PE9.2)))
 4030 FORMAT(1X,F9.3,99999(1X,F10.4)/(9X,13F9.3))
 4031 FORMAT(1X,99999(1X,F10.4)/(9X,13F9.3))
      END
      SUBROUTINE VSOUTS(IV,VPRNTS)
!XXXXXXXXXXXXXXXXXXXXXXXXX
!
!VSOUTS
!******
!      PURPOSE: TO PRINT TWO DIMENSIONAL ARRAY
!-------------------------------------------------------
!
      use rspac
      use kcon
      use dumm2
      use compnam
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      CHARACTER*80 TITL
      CHARACTER*6 ZUNIT,CUNX,HUNX
      CHARACTER*7 TUNIT 
      CHARACTER*10 SCOMPNAME(50)
      COMMON/SCHAR/TITL,ZUNIT,TUNIT,CUNX,HUNX 
      common/elimit/elimit1,elimit2
      LOGICAL F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,o13p, &
      F14P,F15P,F16P,F17P,F18P,F19P
      COMMON/LOG2/F7P,F11P,F8P,F9P,F6P,PRNT,o9p,o11p,o12p,  &
      o13p,F14P,F15P,F16P,F17P,F18P,F19P
      DIMENSION VPRNTS(Nsol,NNODES)
      !!@@include 'd_dumm2Alloc.inc'
      allocate(DUM2(Nsol,NNODES))
!-----------------------------------------------------------------      
      WRITE (06,4000) ZUNIT,ZUNIT 
      WRITE (06,4010) (RX(K),K=2,NXRR) 
      DO 30 M=1,Nsol
      SCOMPNAME(M)=COMPNAME(M)  
      DO 30 J=2,NLYY
      DO 10 N=2,NXRR 
      IN=NLY*(N)-(J-1)
      DUM2(M,N)=VPRNTS(M,IN) 
      IF(HX(IN).EQ.0.0D0) DUM2(M,N)=0.d0 
   10 CONTINUE 
      IF(IV.GT.1) GO TO 20 
      IF(J.EQ.2) then
        WRITE (06,4001)SCOMPNAME(M)
        WRITE (15,*)
      end if
      WRITE (06,4020) DZZ(NLY-J+1),(DUM2(M,N),N=2,NXRR)
      WRITE (15,4021)(DUM2(M,K),K=2,NXRR) 
      GO TO 30 
   20 CONTINUE
      IF(J.EQ.2) then
      WRITE (06,4001) COMPNAME(M)
      WRITE (15,*)
      end if
      WRITE (06,4030) DZZ(NLY-J+1),(DUM2(M,N),N=2,NXRR)
      WRITE (15,4031)(DUM2(M,K),K=2,NXRR) 
   30 CONTINUE 
      !!@@include 'd_dumm2Dealloc.inc'
      deallocate(DUM2)
      return
 4000 FORMAT(1H ,1X,5HZ, IN/2X,A4,20X,20HX OR R DISTANCE, IN ,A4) 
 4001 FORMAT(A10)
 4010 FORMAT(1H ,8X,99999(F9.2)/(9X,13(F9.2))) 
 4020 FORMAT(1X,F8.2,99999(1PE9.2)/(9X,13(1PE9.2))) 
 4021 FORMAT(1X,99999(1PE9.2)/(9X,13(1PE9.2))) 
 4030 FORMAT(1X,F8.2,99999(F9.3)/(9X,13F9.3)) 
 4031 FORMAT(1X,99999(F9.3)/(9X,13F9.3))
      END      
      SUBROUTINE VSPOND(IFET,IFET1,IFET2)
!******
!CVSPOND
!******
!
!  UPDATED 10-88
!
!   PURPOSE:  TO DETERMINE IF PONDING OR UNPONDING HAS OCCURRED, AND
!             IF SO TO CHANGE BOUNDARY CONDITIONS AT THOSE NODES FROM
!              NEUMAN TO DIRICHLET OR VICE VERSA
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use press
      use disch
      use equat
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/PND/POND
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      COMMON/TCON1/NIS,NIS1,NIS3
!
!--------------------------------------------------------------------
!
!   IFET1 INDICATES WHETHER THERE ARE ANY NEUMAN BOUNDARIES REMAINING
!   IFET2 INDICATES WHETHER ANY SPECIFIC FLUX NODES HAVE BEEN CONVERTED
!     TO SPECIFIED HEAD NODES.  BECAUSE OF THE CAPILLARY BARRIER
!     EFFECT, THESE NODES MAY NEED TO REVERT TO SPECIFIED FLUX NODES.
!   IFET INDICATES WHETHER PONDING OCCURRED OR DISAPPEARED
!
      IF(IFET1.EQ.0 .AND. IFET2 .EQ. 0) RETURN
      IFET=0
      IFET1=0
      IFET2=0
      IF(CS1.EQ.1.0D0) THEN
      DZ1=DZZ(2)
      ELSE
      IF(CS2.LT.0.0D0) THEN
      DZ1=DZZ(2)*CS1+RX(NXRR)*CS2
      ELSE
      DZ1=DZZ(2)*CS1+RX(2)*CS2
      END IF
      END IF
      DO 20 I=2,NXRR
      DO 10 J=2,NLYY
      IN=NLY*(I-1)+J
      IF(HX(IN).NE.0.0D0) THEN
      IF(NTYP(IN).EQ.2.AND.QQ(IN).GT.0.0D0) THEN
      IFET1=1
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(I)*CS2
      END IF
      IF(POND.GE.0.0D0) THEN
!
!    DZ2 IS MAXIMUM ALLOWABLE TOTAL HEAD
!
      DZ2=POND-Z1
      ELSE
      DZ2=-DMIN1(Z1,DZ1-POND)
      END IF
      IF(P(IN).GT.DZ2) THEN
!
!   IF COMPUTED HEAD EXCEEDS MAXIMUM THEN SET P=DZ2
!    AND CHANGE BOUNDARY TYPE TO CONSTANT HEAD
!
      P(IN)=DZ2
      NTYP(IN)=1
      IFET=1
      IFET2=1
      WRITE(6,4000) J,I,KTIM,NIT
      END IF
      ELSE
      IF(NTYP(IN).EQ.1.AND.QQ(IN).GT.0.0D0) THEN
      IFET2=1
!
!  change 6/09 to prevent reconverting if p is not less
!   than DZ2
!
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(I)*CS2
      END IF
      IF(POND.GE.0.0D0) THEN
!
!    DZ2 IS MAXIMUM ALLOWABLE TOTAL HEAD
!
      DZ2=POND-Z1
      ELSE
      DZ2=-DMIN1(Z1,DZ1-POND)
      END IF
      if (p(in).lt.DZ2) then
!
!  end 6/09 change
!
      JP1=IN+1
      IM1=IN+NLY
      IP1=IN-NLY
      TEST=(P(IN)-P(JP1))*D(IN)
      IF(HX(IM1).NE.0.0D0) TEST=TEST+(P(IN)-P(IM1))*C(IN)
      IF(HX(IP1).NE.0.0D0) TEST=TEST+(P(IN)-P(IP1))*A(IN)
      TEST=TEST/QQ(IN)
      IF (TEST .GE. 1.01D0)THEN
!
!  IF FLUX FROM THE CONVERTED NODE IS GREATER THAN THE SPECIFIED
!  FLUX RATE, THE NODE IS RECONVERTED TO A SPECIFIED FLUX NODE.
!
      NTYP(IN)=2
      IFET=1
      IFET1=1
      WRITE(06,4010)J,I,KTIM,NIT
      END IF
      end if
      END IF
      END IF
      GO TO 20
      END IF
   10 CONTINUE
   20 CONTINUE
      RETURN
 4000 FORMAT(//,6X,17H PONDING AT NODE ,2I5,17H DURING TIME STEP, &
      I9,'  ITERATION ',I5)
 4010 FORMAT(//,6X,' PONDING ENDED AT NODE ',2I5, &
     ' DURING TIME STEP ',I9,'  ITERATION ',I5)
      END
      SUBROUTINE VSSFAC
!******
!VSSFAC
!******
!
!  REVISED 10-88
!
!    PURPOSE:  TO COMPUTE POSITION OF SEEPAGE FACE BOUNDARIES
!
!     HEIGHT OF SEEPAGE FACE IS LOWERED IF THERE IS FLUX INTO SYSTEM
!     THRU FACE.
!    HEIGHT IS RAISED IF PRESSURE HEADS ARE POSITIVE ABOVE FACE.
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use press
      use spfc
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
!
!-------------------------------------------------------------------
!
      DO 90 K=1,NFCS
      NFX=NFC(K)
      JFST=0
      JLST=JLAST(K)
!
!   CHECK FOR POSITIVE PRESSURES ABOVE SEEPAGE FACE
!
      DO 10 J=NFX,1,-1
      IN=JSPX(1,J,K)
      JJ=JSPX(2,J,K)
      NN=JSPX(3,J,K)
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(JJ)
      ELSE
      Z1=DZZ(JJ)*CS1+RX(NN)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(PTMP.LT.0.0D0) GO TO 10
      JFST=J
      GO TO 20
   10 CONTINUE
   20 CONTINUE
!
!   CHECK FOR FLOW INTO DOMAIN THROUGH SEEPAGE FACE
!
      IF(JFST.GT.JLST) GO TO 60
      DO 40 I=JLST,1,-1
      IN=JSPX(1,I,K)
      IM1=IN-NLY
      JM1=IN-1
      IP1=IN+NLY
      JP1=IN+1
      IF(HX(IM1).EQ.0.0D0) THEN
      IF(HX(IP1).NE.0.0D0.AND.P(IP1).LT.P(IN)) GO TO 30
      END IF
      IF(HX(JM1).EQ.0.0D0) THEN
      IF(HX(JP1).NE.0.0D0.AND.P(JP1).LT.P(IN)) GO TO 30
      END IF
      IF(HX(IP1).EQ.0.0D0) THEN
      IF(HX(IM1).NE.0.0D0.AND.P(IM1).LT.P(IN)) GO TO 30
      END IF
      IF(HX(JP1).EQ.0.0D0) THEN
      IF(HX(JM1).NE.0.0D0.AND.P(JM1).LT.P(IN)) GO TO 30
      END IF
      GO TO 50
   30 NTYP(IN)=3
   40 CONTINUE
      I=0
   50 IF(I.EQ.JLST) GO TO 60
!
!   RESET SEEPAGE FACE HEIGHT AND BOUNDARIES
!
      JLAST(K)=I
      GO TO 80
   60 IF(JFST.EQ.JLST) GO TO 80
      DO 70 I=1,JFST
      IN=JSPX(1,I,K)
      JJ=JSPX(2,I,K)
      NN=JSPX(3,I,K)
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(JJ)
      ELSE
      Z1=DZZ(JJ)*CS1+RX(NN)*CS2
      END IF
      NTYP(IN)=1
      P(IN)=-Z1
   70 CONTINUE
      JLAST(K)=JFST
   80 CONTINUE
   90 CONTINUE
      END
      SUBROUTINE VSEVAP
!******
!VSEVAP
!******
!
!  PURPOSE: TO COMPUTE SURFACE EVAPORATION RATES
!
!
!------------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use hcon
      use press
      use disch
      use ptet
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
!
!-----------------------------------------------------------------
!
      ETOUT1=0.0D0
      do 6 i = nly,nnodes
      q(i) = 0.0d0
 6    continue
      IF(SRES.EQ.0.0D0) RETURN
      DO 10 N=2,NXRR
      ETR = 0.0D0
      dzEvapCells = 0.0D0
      nIndex = nly*(n - 1)
      AREA=DXR(N)
      IF(RAD)AREA=PI2*RX(N)*DXR(N)
      PETT=PEV*AREA
      DO 7 J=2,NLYY
      IN = nIndex + J
      IF(NTYP(IN).EQ.5) then
        dzEvapCells = dzEvapCells + delz(j)
      end if
 7    continue
      DO 8 J=2,NLYY
      IN = nIndex + J
      IF(NTYP(IN).EQ.5) THEN
!
!    COMPUTE TEMPORARY EVAP RATE, CHECK AGAINST MAX AND
!    CORRECT IF NECESSARY
!
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(N)*CS2
      END IF
      PTMP=P(IN)+Z1
      HKX=HCND(IN)*HX(IN)
      EV=HKX*SRES*(HA-PTMP)*AREA*delz(j)/dzEvapCells
      IF(EV.GT.0.0D0) EV=0.0D0
      Q(IN)=EV
      ETR = ETR + Q(IN)
      end if
 8    continue
      if (ETR.lt.pett) then
       r1 = pett/ETR
       ETR = PETT
       DO 9 J=2,NLYY
       IN = nIndex + J
       IF(NTYP(IN).EQ.5) q(in) = q(in)*r1
 9     continue
      end if
      ETOUT1=ETOUT1+ETR
   10 CONTINUE
      RETURN
      END
      SUBROUTINE VSPLNT
!******
!VSPLNT
!******
!
!   THIS SUBROUTINE COMPUTES ACTUAL ET AS A FUNCTION OF A ROOT
!        ACTIVITY FUNCTION, HYDRAULIC CONDUCTIVITY OF THE SOIL,
!        AND THE DIFFERENCE IN PRESSURE HEAD BETWEEN THE ROOTS AND
!        THE SOIL
!
! ----------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      use rspac
      use kcon
      use press
      use disch
      use hcon
      use ptet
      use dumm
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
!
!   SUM TRANSPIRATION FOR EACH COLUMN
!
      ETOUT=0.0D0
      if(.not.bcit) then
      do 6 i = nly,nnodes
      q(i) = 0.0d0
 6    continue
      end if
      IF(PET.GE. 0.0D0)RETURN
      DO 50 I=2,NXRR
      ETR=0.0D0
      AREA=DXR(I)
      IF (RAD) AREA=PI2*RX(I)*DXR(I)
      PETT=AREA*PET
      iIndex = nly*(i-1)
      DO 10 J=2,NLYY
!
!   COMPUTE TRANSPIRATION FOR EACH NODE IN COLUMN
!
      IN=iIndex+J
      IF(NTYP(IN).EQ.0.AND.HX(IN).GT.0.0D0) THEN
      VOL=AREA*DELZ(J)
      IF(DPTH(IN).GT.RTDPTH) GO TO 20
!
!   TRANSPIRATION IS ZERO IF NTYP IS NOT 0, NODE IS DEEPER
!   THAN RTDPTH, OR PRESSURE IS LESS THAN HROOT
!
      IF(CS1.EQ.1.0D0) THEN
      Z1=DZZ(J)
      ELSE
      Z1=DZZ(J)*CS1+RX(I)*CS2
      END IF
      PTMP=P(IN)+Z1
      IF(PTMP.LE.HROOT) THEN
      dum(IN)=0.0D0
      ELSE
      HXX=HCND(IN)*HX(IN)*RT(IN)*VOL
!
!   Q IS TRANSPIRATION FOR EACH NODE.  ETR IS TOTAL FOR COLUMN
!
      dum(IN)=(HROOT-PTMP)*HXX
      ETR=ETR+dum(IN)
      END IF
      END IF
   10 CONTINUE
 20   continue
      ETR1 = ETR
      IF(ETR.LT.PETT) THEN
!
!   IF TOTAL TRANSPIRATION FOR COLUMN IS GREATER
!   THAN POTENTIAL THEN ADJUST TRANSPIRATION VALUES
!
      R1=PETT/ETR
      ETR1=PETT
      end if
      DO 30 K=2,J
      IN=iIndex+K
      IF(HX(IN).GT.0.0D0.AND.NTYP(IN).EQ.0) THEN
      IF(DPTH(IN).GT.RTDPTH) GO TO 40
      if(etr.lt.pett) then
       Q(IN)= Q(IN) + dum(IN)*R1
      else
       Q(IN) = Q(IN) + dum(in)
      end if
      END IF
   30 CONTINUE
   40 CONTINUE
      ETOUT=ETOUT+ETR1
   50 CONTINUE
      RETURN
      END
      