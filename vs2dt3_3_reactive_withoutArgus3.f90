      SUBROUTINE VSPET
!*****
!VSPET
!*****
!
!   PURPOSE: TO COMPUTE VALUES OF PEV,SRES,HA,PET,RTDPTH,RTBOT,RTTOP,
!             AND HROOT FOR EVAPORATION AND TRANSPIRATION CALCULATIONS.
!             VALUES ARE DETERMINED BY LINEAR INTERPOLATION IN TIME
!            BETWEEN EVAPOTRANSPIRATION PERIODS.
!
!------------------------------------------------------------------
!
!    SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      include 'd_ptet.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
!
!--------------------------------------------------------------
!
      npv1 = npv
      if(npv.lt.0) npv = -npv
      IF (NPV.EQ.1) THEN
!
!   IF ONLY 1 PERIOD THEN ALL VALUES ARE CONSTANT
!
      IF(BCIT) THEN
      PEV=-PEVAL(1)
      SRES=RDC(1,1)
      HA=RDC(2,1)
      END IF
      IF(ETSIM) THEN
      PET=-PTVAL(1)
      RTDPTH=RDC(3,1)
      RTBOT=RDC(4,1)
      RTTOP=RDC(5,1)
      HROOT=RDC(6,1)
      END IF
      ELSE
!
!   DETERMINE WHICH PERIOD TO USE
!
      ETCYC1=NPV*ETCYC
      SITY=DMOD(STIM,ETCYC1)
      I=(SITY/ETCYC)+2
      if (i.gt.npv) i = 1
      IF(I.EQ.1) THEN
      K=NPV
      ELSE
      K=I-1
      END IF
!
!   LINEARLY INTERPOLATE
!
      FRPER=(DMOD(SITY,ETCYC))/ETCYC
      IF (BCIT) THEN
      PEV=-PEVAL(K)-(PEVAL(I)-PEVAL(K))*FRPER
      SRES=RDC(1,K)+(RDC(1,I)-RDC(1,K))*FRPER
      HA=RDC(2,K)+(RDC(2,I)-RDC(2,K))*FRPER
      END IF
      IF (ETSIM) THEN
      PET=-PTVAL(K)-(PTVAL(I)-PTVAL(K))*FRPER
      RTDPTH=RDC(3,K)+(RDC(3,I)-RDC(3,K))*FRPER
      RTBOT=RDC(4,K)+(RDC(4,I)-RDC(4,K))*FRPER
      RTTOP=RDC(5,K)+(RDC(5,I)-RDC(5,K))*FRPER
      HROOT=RDC(6,K)+(RDC(6,I)-RDC(6,K))*FRPER
      END IF
      END IF
      npv = npv1
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSRDF(Z1,Z2)
!*****
!VSRDF
!*****
!
!     PURPOSE: TO DETERMINE THE ROOT ACTIVITY AT EACH NODE WITHIN
!              THE ROOT ZONE FOR EACH TIME STEP
!
!
!-------------------------------------------------------------------
!
      include 'd_ptet.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!     include 'c_ptet.inc'
!
!------------------------------------------------------------
!
!
!   LINEARLY INTERPOLATE USING DEPTH OF NODE AND MAXIMUM ROOT DEPTH
!
      IF(RTDPTH.GT.Z1.AND.RTDPTH.GT.0.0D0)THEN
      IF(RTDPTH.GE.Z1+Z2)THEN
      ZZ=Z1+0.5D0*Z2
      ZZ1=1.0D0
      ELSE
      ZZ=(Z1+RTDPTH)*0.5D0
      ZZ1=(RTDPTH-Z1)/Z2
      END IF
      VSRDF=ZZ1*(ZZ*RTBOT+(RTDPTH-ZZ)*RTTOP)/RTDPTH
      ELSE
      VSRDF=0.0D0
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSDTHUVG(P,I)
! ******
!VSDTHUVG
!******
!
!    FIRST DERIVATIVE OF MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!
!   VAN GENUCHTEN FUNCTION
!
!          HK(I,1)=SATURATED HYDRAULIC CONDUCTIVITY
!          HK(I,2)=SPECIFIC STORAGE
!          HK(I,3)=POROSITY
!          HK(I,4)=ALPHA PRIME
!          HK(I,5)=RESIDUAL MOISTURE CONTENT
!          HK(I,6)=BETA PRIME
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSDTHUVG=0.0D0
      IF(P.GE.0.0D0)RETURN
      SE=HK(I,3)-HK(I,5)
      EN=HK(I,6)
      EM=2.0D0-1.0D0/EN
!
!   NOTE -- following change made 12/1/98 to change definition of van
!           Genuchten alpha from original definition used in VS2DT.
!           New definition corresponds to that given by van Genuchten
!           (1980) in terms of inverse length.
!
!      ALPH=HK(I,4)
      ALPH=-1.0D0/HK(I,4)
      A=P/ALPH
      VSDTHUVG=-(EN-1.0D0)*SE*A**(EN-1.0D0)/(ALPH*(1.0D0+A**EN)**EM)
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHNVVG(V,I)
!******
!VSTHNVVG
!******
!
!    INITIAL UNSATURATED PRESSURE HEAD AS A FUNCTION OF VOLUMETRIC
!    MOISTURE CONTENT
!
!    VAN GENUCHTEN FUNCTION
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/JCON/JSTOP,JFLAG,jflag1
!      include 'c_rpropsh.inc'
      VSTHNVVG=0.0D0
      IF(V.GE.HK(I,3)) RETURN
      IF(V.GT.HK(I,5)) GO TO 10
      WRITE(6,4000) V,I
      jstop=6
      return
   10 SE=(V-HK(I,5))/(HK(I,3)-HK(I,5))
      EN=HK(I,6)
      EM=1.0D0-1.0D0/EN
!
!   NOTE -- following change made 12/1/98 to change definition of van
!           Genuchten alpha from original definition used in VS2DT.
!           New definition corresponds to that given by van Genuchten
!           (1980) in terms of inverse length.
!
!      ALPH=HK(I,4)
      ALPH=-1.0D0/HK(I,4)
      VSTHNVVG=ALPH*(1.0D0/SE**(1.0D0/EM)-1.0D0)**(1.0D0-EM)
      RETURN
 4000 FORMAT(/,'INITIAL MOISTURE CONTENT OF ',F7.3,'IS LESS THAN RES' &
      ,'IDUAL MOISTURE CONTENT FOR CLASS ',I4,/, &
      'PROGRAM HALTED')
      END
      DOUBLE PRECISION FUNCTION VSTHUVG(P,I)
!*****
!VSTHUVG
!*****
!
!    MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!
! VAN GENUCHTEN FUNCTION
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSTHUVG=HK(I,3)
      IF(P .GE. 0.0D0)RETURN
      EN=HK(I,6)
!     EM=-(1.0D0-1.0D0/EN)
      EM=(1.0D0-1.0D0/EN)
      A=HK(I,3)-HK(I,5)
!
!   NOTE -- following change made 12/1/98 to change definition of van
!           Genuchten alpha from original definition used in VS2DT.
!           New definition corresponds to that given by van Genuchten
!           (1980) in terms of inverse length.
!
!      ALPH=HK(I,4)
      ALPH=-1.0D0/HK(I,4)
      VSTHUVG=HK(I,5)+A/(1.0D0+(P/ALPH)**EN)**EM
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUVG(P,I)
!*****
!VSHKUVG
!*****
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!   VAN GENUCHTEN FUNCTION
!
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rprop.inc'
      VSHKUVG=1.0D0
      IF(P.GE.0.0D0)RETURN
      EN=HK(I,6)
      EM=1.0D0-1.0D0/EN
!
!   NOTE -- following change made 12/1/98 to change definition of van
!           Genuchten alpha from original definition used in VS2DT.
!           New definition corresponds to that given by van Genuchten
!           (1980) in terms of inverse length.
!
!      A=P/HK(I,4)
      A=-P*HK(I,4)
      TOP=A**EN
      DEN=(1.0D0+TOP)**(EM/2.0D0)
!     TOP=1.0D0-TOP/A*(1.0D0+TOP)**(-EM)
      TOP=1.0D0-TOP/((1.0D0+TOP)**EM*A)
      VSHKUVG=TOP*TOP/DEN
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUVGH(P,TT,I)
!*****
!VSHKUVG
!****
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!  VAN GENUCHTEN FUNCTION
!
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      if (trans) then
      R2=10.0D0**(247.80D0*(1.0D0/(293.160D0-140.0D0)-1.0D0/&
      (TT+273.160D0-140.0D0)))     
      VSHKUVGH=R2
      else
       VSHKUVGH= 1.0D0
      end if
      IF(P.GE.0.0D0)RETURN
      EN=HK(I,6)
      EM=1.0D0-1.0D0/EN
!
!   NOTE -- following change made 12/1/98 to change definition of van
!           Genuchten alpha from original definition used in VS2DH.
!           New definition corresponds to that given by van Genuchten
!           (1980) in terms of inverse length.
!
!      A=P/HK(I,4)
      A=-P*HK(I,4)
      TOP=A**EN
      DEN=(1.0D0+TOP)**(EM/2.0D0)
!     TOP=1.0D0-TOP/A*(1.0D0+TOP)**(-EM)
      TOP=1.0D0-TOP/((1.0D0+TOP)**EM*A)
      VSHKUVGH=VSHKUVGH*TOP*TOP/DEN
      RETURN
      END

      DOUBLE PRECISION FUNCTION VSDTHUBC(P,I)
!******
!VSDTHUBC
!******
!
!    FIRST DERIVATIVE OF MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!
!    BROOKS AND COREY, CSU HYDROLOGY PAPER NO. 17 PP.3-4
!
!          HK(I,1)=SATURATED HYDRAULIC CONDUCTIVITY
!          HK(I,2)=SPECIFIC STORAGE
!          HK(I,3)=POROSITY
!          HK(I,4)=BUBBLING PRESSURE
!          HK(I,5)=RESIDUAL MOISTURE CONTENT
!          HK(I,6)=LAMBDA
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSDTHUBC=0.0D0
      IF(P.GE.HK(I,4))RETURN
      VSDTHUBC=-((HK(I,3)-HK(I,5))*HK(I,6)*(HK(I,4)/P)**HK(I,6))/P
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHNVBC(V,I)
!******
!VSTHNVBC
!******
!
!    INITIAL UNSATURATED PRESSURE HEAD AS A FUNCTION OF VOLUMETRIC
!    MOISTURE CONTENT
!
!    BROOKS AND COREY, CSU HYDROLOGY PAPER NO. 17 , PP.3-4
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/JCON/JSTOP,JFLAG,jflag1
!      include 'c_rpropsh.inc'
      VSTHNVBC=HK(I,4)
      IF(V.GE.HK(I,3)) RETURN
      IF(V.GT.HK(I,5)) GO TO 1
      WRITE(6,100) V,I
  100 FORMAT(/,'INITIAL MOISTURE CONTENT OF ',F7.3,'IS LESS THAN RES' &
      ,'IDUAL MOISTURE CONTENT FOR CLASS ',I4,/, &
       'PROGRAM HALTED')
      jstop=6
      return
  1   SE=(V-HK(I,5))/(HK(I,3)-HK(I,5))
      VSTHNVBC=HK(I,4)/(SE**(1.0D0/HK(I,6)))
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHUBC(P,I)
!*****
!VSTHUBC
!*****
!
!    MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD BELOW BUBBLING
!   PRESSURE: = POROSITY ELSEWHERE
!
!    BROOKS AND COREY, CSU HYDROLOGY PAPER NO.17, PP.3-4
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSTHUBC=HK(I,3)
      IF(P.GE.HK(I,4))RETURN
      VSTHUBC=HK(I,5)+(HK(I,3)-HK(I,5))*(HK(I,4)/P)**HK(I,6)
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUBC(P,I)
!*****
!VSHKUBC
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!    BROOKS AND COREY, CSU HYDROLOGY PAPER NO. 3
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSHKUBC=1.0D0
      IF(P.GE.HK(I,4))RETURN
      VSHKUBC=(HK(I,4)/P)**(2.0D0+3.0D0*HK(I,6))
      IF(VSHKUBC.LT.1.D-38)VSHKUBC=0.0D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUBCH(P,TT,I)
!*****
!CVSHKUBCH
!C*****
!
!   RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!    BROOKS AND COREY, CSU HYDROLOGY PAPER NO. 3
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      if (trans) then
      R2=10.0D0**(247.80D0*(1.0D0/(293.160D0-140.0D0)-1.0D0/&
      (TT+273.160D0-140.0D0)))
      VSHKUBCH=R2
      else
       VSHKUBCH = 1.0D0
      end if
      IF(P.GE.HK(I,4))RETURN
      VSHKUBCH=VSHKUBCH*(HK(I,4)/P)**(2.0D0+3.0D0*HK(I,6))
      IF(VSHKUBCH.LT.1.D-38)VSHKUBCH=0.0D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSDTHUHK(P,I)
!******
!VSDTHUHK
!******
!
!    FIRST DERIVATIVE OF MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!
!   HAVERKAMP FUNCTION
!
!         HK(I,1)=SATURATED HYDRAULIC CONDUCTIVITY
!          HK(I,2)=SPECIFIC STORAGE
!          HK(I,3)=POROSITY
!          HK(I,4)=A PRIME
!          HK(I,5)=RESIDUAL MOISTURE CONTENT
!          HK(I,6)=B PRIME
!          HK(I,7)=ALPHA
!          HK(I,8)=BETA
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSDTHUHK=0.0D0
      IF(P.GE.0.0D0)RETURN
      SE=HK(I,3)-HK(I,5)
      ALPH=HK(I,7)
      EM=HK(I,8)
      TOP=P/ALPH
      DEN=1.0D0+TOP**EM
      DEN=DEN*DEN
      VSDTHUHK=-SE*EM*TOP**(EM-1.0D0)/(ALPH*DEN)
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHNVHK(V,I)
!******
!VSTHNVHK
!******
!
!    INITIAL UNSATURATED PRESSURE HEAD AS A FUNCTION OF VOLUMETRIC
!    MOISTURE CONTENT
!
!    HAVERKAMP FUNCTION
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/JCON/JSTOP,JFLAG,jflag1
!      include 'c_rpropsh.inc'
      VSTHNVHK=0.0D0
      IF(V.GE.HK(I,3)) RETURN
      IF(V.GT.HK(I,5)) GO TO 1
      WRITE(6,100) V,I
  100 FORMAT(/,'INITIAL MOISTURE CONTENT OF ',F7.3,'IS LESS THAN RES' &
      ,'IDUAL MOISTURE CONTENT FOR CLASS ',I4,/, &
      'PROGRAM HALTED')
      jstop=6
      return
  1   SE=(V-HK(I,5))/(HK(I,3)-HK(I,5))
      VSTHNVHK=HK(I,7)*(1.0D0/SE-1.0D0)**(1.0D0/HK(I,8))
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHUHK(P,I)
!******
!VSTHUHK
!******
!
!   MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!
!  HAVERKAMP FUNCTION
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!     include 'c_rpropsh.inc'
      VSTHUHK=HK(I,3)
      IF(P .GE. 0.0D0)RETURN
      VSTHUHK=HK(I,5)+(HK(I,3)-HK(I,5))/((P/HK(I,7))**HK(I,8)+1.0D0)
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUHK(P,I)
!*****
!VSHKUHK
!****
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!  HAVERKAMP FUNCTION
!
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      VSHKUHK=1.0D0
      IF(P.GE.0.0D0)RETURN
      VSHKUHK=1.0D0/((P/HK(I,4))**HK(I,6)+1.0D0)
      RETURN
      END
       DOUBLE PRECISION FUNCTION VSHKUHKH(P,TT,I)
!*****
!VSHKUHKH
!*****
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!
!  HAVERKAMP FUNCTION
!
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      if (trans) then
      R2=10.0D0**(247.80D0*(1.0D0/(293.160D0-140.0D0)-1.0D0/&
      (TT+273.160D0-140.0D0)))
      VSHKUHKH=R2
      else
       VSHKUHKH = 1.0D0
      end if
      IF(P.GE.0.0D0)RETURN
      VSHKUHKH=VSHKUHKH/((P/HK(I,4))**HK(I,6)+1.0D0)
      RETURN
      END     

      SUBROUTINE INTERP (P,I)
!******
!INTERP
!******
!
!C   THIS SUBROUTINE PERFORMS LINEAR INTERPOLATION OF PRESSURE
!   HEADS FOR RELATIVE HYDRAULIC CONDUCTIVITY (VSHKU), VOLUMETRIC
!   MOISTURE CONTENT (VSTHU), AND MOISTURE CAPACITY (VSDTHU).
!
!
!   TO USE THIS METHOD FOR EVALUATING THE NONLINEAR FUNCTIONS,
!   THE USER MUST ENTER A TABLE OF PRESSURE HEADS
!   AND VALUES OF RELATIVE
!   CONDUCTIVITIES,AND MOISTURE CONTENTS
!   WHICH CORRESPOND TO EACH PRESSURE HEAD INTO ARRAY HK ON
!   B-7 CARDS FOR EACH TEXTURAL CLASS.  SET NPROP (CARD B-5) EQUAL
!   TO 3*(NUMBER OF PRESSURE HEADS IN TABLE + 1).
!   BEGINNING WITH HK(ITEX,4), ENTER ALL PRESSURE HEADS IN DESCENDING
!   ORDER STARTING WITH THE HIGHEST VALUE,
!   NEXT ENTER THE NUMBER 99,
!   NEXT ENTER THE RELATIVE HYDRAULIC
!   CONDUCTIVITY FOR EACH PRESSURE HEAD,
!   NEXT ENTER THE NUMBER 99,
!   NEXT ENTER THE VOLUMETRIC MOISTURE CONTENT FOR EACH PRESSURE
!   HEAD, FINALLY ENTER THE NUMBER 99.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/hinterp/ DELPP,nprop,I1,I2,I3,I4,I5,I6
!     IF (I2.GT.0) GO TO 1
      I2=4
      DO 2 J=I2,nprop
      IF (HK(I,J).LT.99.0D0) GO TO 2
      I3=J-I2+1
      I1=I3+I3
      GO TO 1
   2  CONTINUE
   1  IF(HK(I,I2).LE.P) THEN
      DELPP=0.0D0
      I5=I2
      I6=I2
      ELSE
      I4=I2+I3-2
      IF(HK(I,I4).GE.P)THEN
      I5=I4-1
      I6=I4
      DELPP=0.0D0
      ELSE
!      I4=I4-1
      DO 3 J=I2+1,I4
      IF(HK(I,J).GT.P) GO TO 3
      I5=J-1
      I6=J
      DELPP=(P-HK(I,I6))/(HK(I,I5)-HK(I,I6))
      RETURN
   3  CONTINUE
      END IF
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUTAB (P,I)
!*****
!VSHKUTAB
!*****
!
!  RELATIVE HYDRAULIC CONDUCTIVITY AS A FUNCTION OF PRESSURE HEAD
!  DETERMINED BY LINEAR INTERPOLATION OF KR VS HP TABLE WHICH IS
!   INPUT BY USER.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/hinterp/ DELPP,nprop,I1,I2,I3,I4,I5,I6
      CALL INTERP (P,I)
      IF(I5.EQ.I6)THEN
      VSHKUTAB=HK(I,I3+I5)
      RETURN
      ELSE
      VSHKUTAB=HK(I,I3+I6)+(HK(I,I3+I5)-HK(I,I3+I6))*DELPP
      RETURN
      END IF
      END
      DOUBLE PRECISION FUNCTION VSHKUTABH (P,TT,I)
!*****
!VSHKUTAB
!*****
!
!   RELATIVE HYDRAULIC CONDUCTIVITY AS A FUNCTION OF PRESSURE HEAD
!  DETERMINED BY LINEAR INTERPOLATION OF KR VS HP TABLE WHICH IS
!   INPUT BY USER.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/hinterp/ DELPP,nprop,I1,I2,I3,I4,I5,I6
      R2=10.0D0**(247.80D0*(1.0D0/(293.160D0-140.0D0)-1.0D0/ &
      (TT+273.160D0-140.0D0)))
      CALL INTERP (P,I)
      IF(I5.EQ.I6)THEN
      VSHKUTABH=R2*HK(I,I3+I5)
      RETURN
      ELSE
      VSHKUTABH=HK(I,I3+I6)+(HK(I,I3+I5)-HK(I,I3+I6))*DELPP
      VSHKUTABH=VSHKUTABH*R2
      RETURN
      END IF
      END
      DOUBLE PRECISION FUNCTION VSDTHUTAB(P,I)
!******
!VSDTHUTAB
!******
!
!   MOISTURE CAPACITY AS A FUNCTION OF PRESSURE HEAD AS
!   DETERMINED FROM TABLE OF THETA VS HP WHICH IS INPUT
!   BY USER.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/hinterp/ DELPP,nprop,I1,I2,I3,I4,I5,I6
      CALL INTERP (P,I)
      IF (I5.EQ.I6) THEN
      VSDTHUTAB=0.0D0
      RETURN
      ELSE
      VSDTHUTAB=(HK(I,I1+I5)-HK(I,I1+I6))/(HK(I,I5)-HK(I,I6))
      RETURN
      END IF
      END
      DOUBLE PRECISION FUNCTION VSTHUTAB (P,I)
!*****
!VSTHUTAB
!*****
!
!   VOLUMETRIC MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!   AS DETERMINED BY LINEAR INTERPOLATION OF THETA VS HP TABLE
!   WHICH IS INPUT BY USER.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/hinterp/ DELPP,nprop,I1,I2,I3,I4,I5,I6
      IF (DELPP.EQ.0.0D0) THEN
      VSTHUTAB=HK(I,I1+I6)
      ELSE
      VSTHUTAB=HK(I,I1+I6)+(HK(I,I1+I5)-HK(I,I1+I6))*DELPP
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHNVTAB(P,I)
!******
!VSTHNVTAB
!******
!
!     NOTE -- THIS FUNCTION IS NOT OPERATIVE WHEN USING INTERPOLATION
!             ROUTINES.  INITIAL CONDITIONS MUST BE INPUT IN TERMS OF
!              PRESSURE HEADS NOT MOISTURE CONTENTS.
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/JCON/JSTOP,JFLAG,jflag1
!     include 'c_rpropsh.inc'
      WRITE(6,100)
      jstop=7
      vsthnvtab=hk(i,3)
      return
  100 FORMAT(5X,'INPUT OF MOISTURE CONTENT FOR INITIAL CONDITIONS IS ', &
      'NOT ALLOWED WHEN USING TABULAR DATA '/ &
      5X,'FOR MOISTURE RETENTION AND CONDUCTIVITY CURVES',/ &
      5X,'SIMULATION TERMINATED')
      END

      DOUBLE PRECISION FUNCTION VSDTHUOT(P,I)
!******
!VSDTHUOT
!******
!     
!  modified for Rossi-Nimmo retention curve may 2006
!
!    FIRST DERIVATIVE OF MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD
!         USER-SUPPLIED FUNCTION
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      COMMON/JCON/JSTOP,JFLAG,jflag1
      vsdthuot=0.0D0
      h = -p
      if (h.le.0.0D0) then
       return
      else
       if (h.le.hk(i,7)) then
        vsdthuot = hk(i,3)*hk(i,8)*(h+h)/hk(i,4)**2.0D0
       else
        if (h.le.hk(i,10)) then
         vsdthuot = hk(i,3)*(hk(i,4)**hk(i,6))*hk(i,6)*h**(-1.0D0-hk(i,6))
        else
         vsdthuot = hk(i,3)*hk(i,9)/h
        end if
       end if
      end if
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHNVOT(V,I)
!******
!VSTHNVOT
!******
!
!  revised for Rossi-Nimmo 
!
!    INITIAL UNSATURATED PRESSURE HEAD AS A FUNCTION OF VOLUMETRIC
!    MOISTURE CONTENT
!        USER-SUPPLIED FUNCTION

      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      vsthnvot=0.0D0
      p = V
      if (p.ge.hk(i,3)) then
       return
      else
       if (p.ge.hk(i,11)) then
        vsthnvot = -((hk(i,4)**(2.0D0))*((-p/hk(i,3) + &
          1.0D0)/hk(i,8)))**(.50D0)
       else
        if (p.ge.hk(i,12)) then
         vsthnvot = -(hk(i,4)**hk(i,6)*hk(i,3)/p)**(1.0D0/hk(i,6))
        else
         vsthnvot = -hk(i,5)*exp(-p/(hk(i,3)*hk(i,9)))
        end if
       end if
      end if
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSTHUOT(P,I)
!*****
!VSTHUOT
!*****
!
!  
!  revised for Rossi-Nimmo
!
!    MOISTURE CONTENT AS A FUNCTION OF PRESSURE HEAD BELOW BUBBLING
!   PRESSURE: = POROSITY ELSEWHERE
!        USER-SUPPLIED FUNCTION

      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      vsthuot=hk(i,3)
      h = -p
      if (h.le.0.0D0) then
       return
      else
       if (h.le.hk(i,7)) then
        vsthuot = hk(i,3)*(1.0D0 - hk(i,8)*(h/hk(i,4))**2.0D0)
       else
        if (h.le.hk(i,10)) then
         vsthuot = hk(i,3)*(hk(i,4)/h)**hk(i,6)
        else
         if (h.le.hk(i,5)) then
          vsthuot = hk(i,3)*hk(i,9)*log(hk(i,5)/h)
         else
          vsthuout = 0.0D0
        end if
       end if
      end if
      end if
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUOT(P,I)
!*****
!VSHKUOT
!*****
!
!  revised for Rossi-Nimmo
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
 !     USER-SUPPLIED FUNCTION
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!     include 'c_rpropsh.inc'
      vshkuot=1.0D0
      if (p.ge.hk(i,3)) then
       return
      else
       r1 = p/hk(i,3)
       if (p.lt.hk(i,12)) then
        xi3 = hk(i,9)*(exp(r1/hk(i,9)) - 1.0D0)/hk(i,5)
        vshkuot = r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
       else
        if (p.le.hk(i,11)) then
         xi3 = hk(i,18) + (r1**hk(i,14) - hk(i,15))/(hk(i,4)*hk(i,14))        
         vshkuot = r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
        else
         xi3 = hk(i,19) + hk(i,16)*(hk(i,17) - (1.0D0 - r1)**(.50D0))
         vshkuot = r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
        end if
       end if
      end if
      RETURN
      END
      DOUBLE PRECISION FUNCTION VSHKUOTH(P,TT,I)
!*****
!VSHKUOTH
!*****
!
!  revised for Rossi-Nimmo
!
!    RELATIVE HYDRAULIC CONDUCTIVITY WITH RESPECT TO PRESSURE HEAD
!      USER-SUPPLIED FUNCTION
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_rpropsh.inc'
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      if (trans) then
      R2=10.0D0**(247.80D0*(1.0D0/(293.160D0-140.0D0)-1.0D0/&
      (CC+273.160D0-140.0D0)))
      VSHKUoth=R2
      else
       VSHKUoth = 1.0D0
      end if
      if (p.ge.hk(i,3)) then
       return
      else
       r1 = p/hk(i,3)
       if (p.lt.hk(i,12)) then
        xi3 = hk(i,9)*(exp(r1/hk(i,9)) - 1.0D0)/hk(i,5)
        vshkuoth = vshkuoth*r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
       else
        if (p.le.hk(i,11)) then
         xi3 = hk(i,18) + (r1**hk(i,14) - hk(i,15))/(hk(i,4)*hk(i,14))        
         vshkuoth = vshkuoth*r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
        else
         xi3 = hk(i,19) + hk(i,16)*(hk(i,17) - (1.0D0 - r1)**(.50D0))
         vshkuoth = vshkuoth*r1**(0.50D0)*xi3**(2.0D0)/hk(i,13)
        end if
       end if
      end if
      RETURN
      END
      SUBROUTINE VTVELO
!******
!VTVELO
!******
!
!    ROUTINE TO CALCULATE VELOCITIES AT BOUNDARIES OF ADJACENT CELLS
!    VX IS VELOCITY IN X-DIRECTION BETWEEN CURRENT NODE AND NODE TO
!    THE LEFT.
!    VZ IS VELOCITY IN Z-DIRECTION BETWEEN CURRENT NODE AND NODE
!    ABOVE.
!
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_press.inc'
      include 'd_hcon.inc'
      include 'd_trxx.inc'
      include 'd_trxv.inc'
      include 'd_temp.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/WGT/WUS,WDS
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      LOGICAL HEAT,SOLUTE,FLOW
      COMMON/TRANSTYPE/HEAT,SOLUTE
      IF(HEAT) then
       RHOMAX=0.0D0
      end if
      DO 10 I=2,NXRR
      N1=NLY*(I-1)
      DO 10 J=2,NLYY
      N=N1+J
      VX(N)=0.0D0
      VZ(N)=0.0D0
      IF(HX(N).NE.0.0D0) THEN
      JM1=N-1
      IM1=N-NLY
      IF(HX(JM1).NE.0.0D0) THEN
!
!   CALCULATE VERTICAL VELOCITY
!
      if(ntyp(n).eq.1.and.ntyp(jm1).eq.1) then
       vz(n) = 0.0d0
      else
      if(heat)then
        RHO2=VZ(N)
       end if
      AREA=DXR(I)
      IF (RAD) AREA=PI2*RX(I)*DXR(I)
      GRAD=P(JM1)-P(N)
      THETA1=0.5D0*(THETA(N)+THETA(JM1))*AREA
      IF(WUS.EQ.0.0D0) THEN
      VZ(N)=HKTT(N)*DSQRT(HCND(N)*HCND(JM1))*GRAD/THETA1
      ELSE
      IF(P(JM1).GT.P(N))THEN
      ALA=WUS
      BTA=WDS
      ELSE
      ALA=WDS
      BTA=WUS
      END IF
      VZ(N)=HKTT(N)*(ALA*HCND(JM1)+BTA*HCND(N))*GRAD/THETA1
      END IF
      if(heat) then
      RHO2=DABS(RHO2-VZ(N))
      IF(RHO2.GT.RHOMAX) RHOMAX=RHO2
      end if
      END IF
      ELSE
      VZ(N)=0.0D0
      end if
      IF(HX(IM1).NE.0.0D0) THEN
!
!   CALCULATE HORIZONTAL VELOCITY
!
      if(ntyp(n).eq.1.and.ntyp(im1).eq.1) then
       vx(n) = 0.0d0
      else
      if(heat)then
      RHO2=VX(N)
      end if  
      GRAD=P(IM1)-P(N)
      AREA=DELZ(J)
      IF (RAD) AREA=PI2*AREA*(RX(I)-0.5D0*DXR(I))
      THETA1=0.5D0*(THETA(N)+THETA(IM1))*AREA
      IF(WUS.EQ.0.0D0) THEN
      VX(N)=HKLL(N)*DSQRT(HCND(N)*HCND(IM1))*GRAD/THETA1
      ELSE
      IF(P(IM1).GT.P(N)) THEN
      ALA=WUS
      BTA=WDS
      ELSE
      ALA=WDS
      BTA=WUS
      END IF
      VX(N)=HKLL(N)*(ALA*HCND(IM1)+BTA*HCND(N))*GRAD/THETA1
      END IF
      if(heat)then
      RHO2=DABS(RHO2-VX(N))
      IF(RHO2.GT.RHOMAX) RHOMAX=RHO2
      end if  
      END IF
      ELSE
      VX(N)=0.0D0
      END IF
      end if
   10 CONTINUE
      RETURN
      END
      SUBROUTINE VTDCOEF
!*******
!VTDCOEF
!******
!
!    ROUTINE TO CALCULATE DISPERSION COEFFICIENTS AS FUNCTIONS
!    OF DISPERSIVITIES AND VELOCITIES.  DIAGNOL TERMS ARE
!   CONTAINED IN ARRAYS DX1 AND DZ1.  CROSS PRODUCT TERMS
!    ARE IN DX2 AND DZ2
!
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_jtxx.inc'
      include 'd_trxv.inc'
      include 'd_rpropsh.inc'
      include 'd_trxxh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      DO 10 I=2,NXRR
      N1=NLY*(I-1)
      DO 10 J=2,NLYY
      N=N1+J
      DX1(N)=0.0D0
      DX2(N)=0.0D0
      DZ1(N)=0.0D0
      DZ2(N)=0.0D0
      PEX=0.0D0
      PEZ=0.0D0
      IMX=0
      JMX=0
      IMZ=0
      JMZ=0
      IF(HX(N).NE.0.0D0) THEN
      N2=JTEX(N)
      AL=HT(N2,1)
      AT=HT(N2,2)
!      DM=HS(N2,3)
      V1=VX(N)
      V2=VZ(N)
      JM1=N-1
      IM1=N-NLY
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IF(HX(JM1).NE.0.0D0) THEN
      V3=0.25D0*(V1+VX(IP1)+VX(IP2)+VX(JM1))
      V32=V3*V3
      V22=V2*V2
      VV2=V32+V22
!
!   CALCULATE DZ1 AND DZ2
!
      N2=JTEX(JM1)
      AL1=DSQRT(AL*HT(N2,1))
      AT1=DSQRT(AT*HT(N2,2))
!      DM1=DSQRT(DM*HS(N2,3))
       DM1=0.0D0
      AREA=DXR(I)
      IF(RAD) AREA=PI2*AREA*RX(I)
      T1=0.5D0*(THETA(JM1)+THETA(N))
      DD1=(DZZ(J)-DZZ(J-1))/AREA
      T2=T1/DD1
      IF(VV2.EQ.0.0D0) THEN
      DZ1(N)=DM1
      ELSE
      VAVE=DSQRT(VV2)
      DL=AL1*VAVE
      DT=AT1*VAVE
      DZ1(N)=(DL*V22+DT*V32)/VV2
!C#
      IF(HX(IP2).GT.0.0D0 .AND.HX(IP1).GT.0.0D0) THEN
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(IM1).GT.0.0D0) THEN
        DD1=(RX(I+1)-RX(I-1))/AREA
       ELSE
        DD1=(RX(I+1)-RX(I))/AREA
       END IF
      ELSE
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(IM1).GT.0.0D0) THEN
        DD1=(RX(I)-RX(I-1))/AREA
       ELSE
        DD1=1.0D+14
       END IF
      END IF
!C#
      DZ2(N)=T1*(DL-DT)*V2*V3/(DD1*VV2)
      END IF
!
! CALCULATE VERTICAL CELL PECLET NUMBER
!
      IF(DZ1(N).LE.0.0D0) THEN
      PE=0.0D0
      ELSE
      PE=DABS(VZ(N))*(DZZ(J)-DZZ(J-1))/DZ1(N)
      END IF
      DZ1(N)=T2*DZ1(N)
      IF(NTYP(N).EQ.0.OR.NTYP(N).EQ.2.or.ntyp(N).eq.7) THEN
      IF(PE.GT.PEZ) THEN
      PEZ=PE
      IMZ=I
      JMZ=J
      END IF
      END IF
      END IF
      IF(HX(IM1).NE.0.0D0) THEN
      V3=0.25D0*(V2+VZ(JP1)+VZ(IM1)+VZ(IM2))
      V32=V3*V3
      V12=V1*V1
      VV2=V12+V32
!
!   CALCULATE DX1 AND DX2
!
      N2=JTEX(IM1)
      AL1=DSQRT(AL*HT(N2,1))
      AT1=DSQRT(AT*HT(N2,2))
!      DM1=DSQRT(DM*HS(N2,3))
      DM1=0.0D0
      AREA=DELZ(J)
      IF(RAD) AREA=PI2*AREA*(RX(I)-0.5D0*DXR(I))
      DD1=(RX(I)-RX(I-1))/AREA
      T1=0.5D0*(THETA(IM1)+THETA(N))
      T2=T1/DD1
      IF(VV2.EQ.0.0D0) THEN
      DX1(N)=DM1
      ELSE
      VAVE=DSQRT(VV2)
      DL=AL1*VAVE
      DT=AT1*VAVE
      DX1(N)=(DL*V12+DT*V32)/VV2
!C#
      IF(HX(JP1).GT.0.0D0 .AND.HX(JP1-NLY).GT.0.0D0) THEN
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(JM1).GT.0.0D0) THEN
        DD1=(DZZ(J+1)-DZZ(J-1))/AREA
       ELSE
        DD1=(DZZ(J+1)-DZZ(J))/AREA
       END IF
      ELSE
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(JM1).GT.0.0D0) THEN
        DD1=(DZZ(J)-DZZ(J-1))/AREA
       ELSE
        DD1=1.0D+14
       END IF
      END IF
!C#
      DX2(N)=T1*(DL-DT)*V1*V3/(VV2*DD1)
      END IF
!C
!C  CALCULATE HORIZONTAL CELL PECLET NUMBER
!C

      IF(DX1(N).LE.0.0D0) THEN
      PE=0.0D0
      ELSE
      PE=DABS(VX(N))*(RX(I)-RX(I-1))/DX1(N)
      END IF
      DX1(N)=DX1(N)*T2
      IF(NTYP(N).EQ.0.OR.NTYP(N).EQ.2.or.ntyp(N).eq.7) THEN
      IF(PE.GT.PEX) THEN
      PEX=PE
      IMX=I
      JMX=J
      END IF
      END IF
      END IF
      END IF
   10 CONTINUE
!
!  WRITE MAXIMUM CELL PECLET NUMBERS
!
!      WRITE(6,4000) PEX,JMX,IMX,PEZ,JMZ,IMZ
      RETURN
! 4000 FORMAT(4X,'  MAXIMUM CELL PECLET NUMBER  --  HORIZONTAL  ',E14.5,
!     &'     ROW ',I4,'  COLUMN ',I4,/,38X,'VERTICAL    ',E14.5,
!     &'     ROW ',I4,'  COLUMN ',I4)
      END
      SUBROUTINE VTDCOEFSOL
!*******
!VTDCOEFSOL
!*******
!
!   ROUTINE TO CALCULATE DISPERSION COEFFICIENTS AS FUNCTIONS
!    OF DISPERSIVITIES AND VELOCITIES.  DIAGNOL TERMS ARE
!   CONTAINED IN ARRAYS DX1 AND DZ1.  CROSS PRODUCT TERMS
!    ARE IN DX2 AND DZ2
!
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_jtxx.inc'
      include 'd_trxv.inc'
      include 'd_rpropsh.inc'
      include 'd_trxx.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV   
      DO 10 I=2,NXRR
      N1=NLY*(I-1)
      DO 10 J=2,NLYY
      N=N1+J
      DXS1(N)=0.0D0
      DXS2(N)=0.0D0
      DZS1(N)=0.0D0
      DZS2(N)=0.0D0
      PEXS=0.0D0
      PEZS=0.0D0
      IMX=0
      JMX=0
      IMZ=0
      JMZ=0
      IF(HX(N).NE.0.0D0) THEN
      N2=JTEX(N)
      AL=HS(N2,1)
      AT=HS(N2,2)
      DM=HS(N2,3)
      V1=VX(N)
      V2=VZ(N)
      JM1=N-1
      IM1=N-NLY
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IF(HX(JM1).NE.0.0D0) THEN
      V3=0.25D0*(V1+VX(IP1)+VX(IP2)+VX(JM1))
      V32=V3*V3
      V22=V2*V2
      VV2=V32+V22
!
!   CALCULATE DZ1 AND DZ2
!
      N2=JTEX(JM1)
      AL1=DSQRT(AL*HS(N2,1))
      AT1=DSQRT(AT*HS(N2,2))
      DM1=DSQRT(DM*HS(N2,3))
      AREA=DXR(I)
      IF(RAD) AREA=PI2*AREA*RX(I)
      T1=0.5D0*(THETA(JM1)+THETA(N))
      DD1=(DZZ(J)-DZZ(J-1))/AREA
      T2=T1/DD1
      IF(VV2.EQ.0.0D0) THEN
      DZS1(N)=DM1
      ELSE
      VAVE=DSQRT(VV2)
      DL=AL1*VAVE
      DT=AT1*VAVE
      DZS1(N)=(DL*V22+DT*V32)/VV2+DM1
!C#
      IF(HX(IP2).GT.0.0D0 .AND.HX(IP1).GT.0.0D0) THEN
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(IM1).GT.0.0D0) THEN
        DD1=(RX(I+1)-RX(I-1))/AREA
       ELSE
        DD1=(RX(I+1)-RX(I))/AREA
       END IF
      ELSE
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(IM1).GT.0.0D0) THEN
        DD1=(RX(I)-RX(I-1))/AREA
       ELSE
        DD1=1.0D+14
       END IF
      END IF
!      DZS2(N)=T1*(DL-DT)*V2*V3/(DD1*VV2)
      END IF
!C
!C  CALCULATE VERTICAL CELL PECLET NUMBER
!C
      IF(DZS1(N).LE.0.0D0) THEN
      PE=0.0D0
      ELSE
      PE=DABS(VZ(N))*(DZZ(J)-DZZ(J-1))/DZS1(N)
      END IF
      DZS1(N)=T2*DZS1(N)
      IF(NTYP(N).EQ.0.OR.NTYP(N).EQ.2) THEN
      IF(PE.GT.PEZS) THEN
      PEZS=PE
      IMZ=I
      JMZ=J
      END IF
      END IF
      END IF
      IF(HX(IM1).NE.0.0D0) THEN
      V3=0.25D0*(V2+VZ(JP1)+VZ(IM1)+VZ(IM2))
      V32=V3*V3
      V12=V1*V1
      VV2=V12+V32
!C
!C   CALCULATE DX1 AND DX2
!C
      N2=JTEX(IM1)
      AL1=DSQRT(AL*HS(N2,1))
      AT1=DSQRT(AT*HS(N2,2))
      DM1=DSQRT(DM*HS(N2,3))
      AREA=DELZ(J)
      IF(RAD) AREA=PI2*AREA*(RX(I)-0.5D0*DXR(I))
      DD1=(RX(I)-RX(I-1))/AREA
      T1=0.5D0*(THETA(IM1)+THETA(N))
      T2=T1/DD1
      IF(VV2.EQ.0.0D0) THEN
      DXS1(N)=DM1
      ELSE
      VAVE=DSQRT(VV2)
      DL=AL1*VAVE
      DT=AT1*VAVE
      DXS1(N)=(DL*V12+DT*V32)/VV2+DM1
!C#
      IF(HX(JP1).GT.0.0D0 .AND.HX(JP1-NLY).GT.0.0D0) THEN
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(JM1).GT.0.0D0) THEN
        DD1=(DZZ(J+1)-DZZ(J-1))/AREA
       ELSE
        DD1=(DZZ(J+1)-DZZ(J))/AREA
       END IF
      ELSE
       IF(HX(IM1-1).GT.0.0D0 .AND.HX(JM1).GT.0.0D0) THEN
        DD1=(DZZ(J)-DZZ(J-1))/AREA
       ELSE
        DD1=1.0D+14
       END IF
      END IF
!C#
      DXS2(N)=T1*(DL-DT)*V1*V3/(VV2*DD1)
      END IF
!C
!C  CALCULATE HORIZONTAL CELL PECLET NUMBER
!C
      IF(DXS1(N).LE.0.0D0) THEN
      PE=0.0D0
      ELSE
      PE=DABS(VX(N))*(RX(I)-RX(I-1))/DXS1(N)
      END IF
      DXS1(N)=DXS1(N)*T2
      IF(NTYP(N).EQ.0.OR.NTYP(N).EQ.2) THEN
      IF(PE.GT.PEXS) THEN
      PEXS=PE
      IMX=I
      JMX=J
      END IF
      END IF
      END IF
      END IF
  10  CONTINUE 
!
!  WRITE MAXIMUM CELL PECLET NUMBERS
!

!       WRITE(6,4000) PEXS,JMX,IMX,PEZS,JMZ,IMZ
      RETURN

! 4000 FORMAT(4X,'  MAXIMUM CELL PECLET NUMBER  --  HORIZONTAL  ',E14.5
!     &'     ROW ',I4,'  COLUMN ',I4,/,38X,'VERTICAL    ',E14.5,
!     &'     ROW ',I4,'  COLUMN ',I4)
      END
      
      SUBROUTINE VTSETUP
!*******
!VTSETUP
!*******
!
!    ROUTINE TO ASSEMBLE MATRIX EQUATIONS FOR ADVECTION-DISPERSION
!    EQUATIONS AND TO CALL MATRIX SOLVER.
!
      include 'd_press.inc'
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_dumm.inc'
      include 'd_disch.inc'
      include 'd_equat.inc'
      include 'd_jtxx.inc'
      include 'd_trxxh.inc'
      include 'd_trxy1.inc'
      include 'd_rpropsh.inc'
      include 'd_ptet.inc'
      include 'd_scon.inc'
      include 'd_trxv.inc'
      include 'd_temp.inc'
      include 'd_pit.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/JCON/JSTOP,JFLAG,jflag1
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      integer hydraulicFunctionType
      common/functiontype/ hydraulicFunctionType
     
!............................................................................
!      
    
      IF(jflag1.EQ.1) THEN
      DO 10 N=1,NNODES
      AO(N)=0.0D0
      BO(N)=0.0D0
      CO(N)=0.0D0
      DO(N)=0.0D0
      EO(N)=0.0D0
   10 CONTINUE
      END IF
!
!   INITIALIZE VARIABLES
!
      do 50 it=1,itmax
      DO 20 I=2,NXRR
      N1=NLY*(I-1)
      DO 20 J=2,NLYY
      N=N1+J
      A(N)=0.0D0
      B(N)=0.0D0
      C(N)=0.0D0
      D(N)=0.0D0
      E(N)=0.0D0
      RHS(N)=0.0D0
      if(it.eq.1) then
        TTOLD(N)=TT(N)
       IF(NTYP(N).EQ.1) then
        if(nit3.eq.0) dum(n) = qt(n)
        QT(N)=VSFLX1(N)
       else
        qt(n) = 0.0d0
       end if
      end if
!     end if
     IF(HX(N).NE.0.0D0) THEN
      N2=JTEX(N)
!      RHO1=RHO(N)
      RHO(N)=VTRHO(TT(N),N2)
!      RHO2=DABS(RHO1-RHO(N))
!      IF(RHO2.GT.RHOMAX)RHOMAX=RHO2
!      RET(N)=VTRET(CC(N),N2)
!      ret(n)=0.0D0
      IM1=N-NLY
      JM1=N-1
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IM3=IM1-1
      IP3=IP1+1
      IF(RAD) THEN
      AREAX=PI2*DELZ(J)*(RX(I)-0.5D0*DXR(I))
      AREAX1=PI2*DELZ(J)*(RX(I)+0.5D0*DXR(I))
      AREAZ=PI2*DXR(I)*RX(I)
      ELSE
      AREAX=DELZ(J)
      AREAX1=AREAX
      AREAZ=DXR(I)
      END IF
      areax_kt = areax
      areax1_kt = areax1
      areaz_kt = areaz
      VOL=AREAZ*DELZ(J)
      AREAX=AREAX*0.5D0*(THETA(IM1)*RHO(IM1)+THETA(N)*RHO(N))
      AREAX1=AREAX1*0.5D0*(THETA(IP1)*RHO(IP1)+THETA(N)*RHO(N))
      AREAZ1=AREAZ*0.5D0*(THETA(JP1)*RHO(JP1)+THETA(N)*RHO(N))
      AREAZ=AREAZ*0.5D0*(THETA(JM1)*RHO(JM1)+THETA(N)*RHO(N))
!
!   CALCULATE LHS OF MATRIX EQUATION
!
      SS=THETA(N)*(P(N)-PXXX(N))*HK(N2,2)/HK(N2,3)
      E(N)=-DX1(N)*(RHO(N)+RHO(IM1))-DZ1(N)*(RHO(N)+RHO(JM1))&
      -DX1(IP1)*(RHO(N)+RHO(IP1))-DZ1(JP1)*(RHO(N)+RHO(JP1))
!    &-VOL*(HT(N2,4)*(THETA(N)+RET(N)))
      E(N)=0.5D0*E(N)
!
! CHANGE ADDED 8-12-91 TO CORRECT STORAGE TERM
!
!      SS1=HT(N2,4)*(THETA(N)+RET(N))
!
!******************
!  following change made 7-3-04 to correct dctheta/dt 
!   calculation - see written notes
!
!******************
!      if(jflag1.ne.1.or.ntyp(n).ne.1) then
!       SS=THETA(N)+SS-THLST(N)
!      end if
!******************
      TC=THERMC(THETA(N),N2)
      TCA=0.0D0
      TCB=0.0D0
      TCC=0.0D0
      TCD=0.0D0
!C#
      A(N)=DX1(N)
      B(N)=DZ1(N)
      C(N)=DX1(IP1)
      D(N)=DZ1(JP1)
!C#TOP
      IF(HX(IM1).NE.0.0D0) THEN
!c      TCA=0.5D0*(THERMC(THETA(IM1),JTEX(IM1))+TC)*DELZ(J)/
      TCA=0.5D0*(THERMC(THETA(IM1),JTEX(IM1))+TC)*AREAX_kt/&
      (RX(I)-RX(I-1))
!C#    A(N)=0.5D0*(DX1(N)*(RHO(N)+RHO(IM1))+DZ2(N)-DZ2(JP1))
      IF(.NOT.CIS) THEN
      IF(VX(N).GT.0.0D0) THEN
      A(N)=A(N)+AREAX*VX(N)
      ELSE
      E(N)=E(N)+AREAX*VX(N)
      END IF
      ELSE
      VV=AREAX*0.5D0*VX(N)
      A(N)=A(N)+VV
      E(N)=E(N)+VV
      END IF
!C#
      TEMPP=0.5D0*DX2(N)
      IF(HX(IM3).GT.0.0D0.AND.HX(JM1).GT.0.0D0) THEN
       IF(HX(IM2).GT.0.0D0.AND.HX(JP1).GT.0.0D0) THEN
        B(N)=B(N)+TEMPP
        D(N)=D(N)-TEMPP
        RHS(N)=RHS(N)+TEMPP*(TT(IM2)-TT(IM3))
       ELSE
        A(N)=A(N)-TEMPP
        B(N)=B(N)+TEMPP
        E(N)=E(N)-TEMPP
        RHS(N)=RHS(N)-TEMPP*TT(IM3)
       END IF
      ELSE
       IF(HX(IM2).GT.0.0D0.AND.HX(JP1).GT.0.0D0) THEN
        A(N)=A(N)+TEMPP
        D(N)=D(N)-TEMPP
        E(N)=E(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*TT(IM2)
       END IF
!C#
      END IF
      END IF
!c# Left      
      IF(HX(JM1).NE.0.0D0) THEN
!c      TCB=0.5D0*(THERMC(THETA(JM1),JTEX(JM1))+TC)*DXR(I)/
      TCB=0.5D0*(THERMC(THETA(JM1),JTEX(JM1))+TC)*AREAZ_kt/&
      (DZZ(J)-DZZ(J-1))
!C#    B(N)=0.5D0*(DZ1(N)*(RHO(N)+RHO(JM1))+DX2(N)-DX2(IP1))
      IF(.NOT.CIS) THEN
      IF(VZ(N).GT.0.0D0) THEN
      B(N)=B(N)+AREAZ*VZ(N)
      ELSE
      E(N)=E(N)+AREAZ*VZ(N)
      END IF
      ELSE
      VV=0.5D0*AREAZ*VZ(N)
      B(N)=B(N)+VV
      E(N)=E(N)+VV
      END IF
!C#
      TEMPP=0.5D0*DZ2(N)
      IF(HX(IP2).GT.0.0D0.AND.HX(IP1).GT.0.0D0) THEN
       IF(HX(IM3).GT.0.0D0.AND.HX(IM1).GT.0.0D0) THEN
        A(N)=A(N)+TEMPP
        C(N)=C(N)-TEMPP
        RHS(N)=RHS(N)+TEMPP*(TT(IP2)-TT(IM3))
       ELSE
        B(N)=B(N)+TEMPP
        C(N)=C(N)-TEMPP
        E(N)=E(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*TT(IP2)
       END IF
      ELSE
       IF(HX(IM3).GT.0.0D0.AND.HX(IM1).GT.0.0D0) THEN
        A(N)=A(N)+TEMPP
        B(N)=B(N)-TEMPP
        E(N)=E(N)-TEMPP
        RHS(N)=RHS(N)-TEMPP*TT(IM3)
       END IF
      END IF
!C#
      END IF
!C# BOTTOM      
      IF(HX(IP1).NE.0.0D0) THEN
!c      TCC=0.5D0*(THERMC(THETA(IP1),JTEX(IP1))+TC)*DELZ(J)/
      TCC=0.5D0*(THERMC(THETA(IP1),JTEX(IP1))+TC)*AREAX1_kt/&
      (RX(I+1)-RX(I))
!C#    C(N)=0.5D0*(DX1(IP1)*(RHO(N)+RHO(IP1))-DZ2(N)+DZ2(JP1))
      IF(.NOT.CIS) THEN
      IF(VX(IP1).LT.0.0D0) THEN
      C(N)=C(N)-AREAX1*VX(IP1)
      ELSE
      E(N)=E(N)-AREAX1*VX(IP1)
      END IF
      ELSE
      VV=0.5D0*AREAX1*VX(IP1)
      C(N)=C(N)-VV
      E(N)=E(N)-VV
      END IF
!C#
      TEMPP=0.5D0*DX2(IP1)
      IF(HX(JP1).GT.0.0D0.AND.HX(IP3).GT.0.0D0) THEN
       IF(HX(IP2).GT.0.0D0.AND.HX(JM1).GT.0.0D0) THEN
        B(N)=B(N)-TEMPP
        D(N)=D(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*(TT(IP2)-TT(IP3))
       ELSE
        C(N)=C(N)-TEMPP
        D(N)=D(N)+TEMPP
        E(N)=E(N)-TEMPP
        RHS(N)=RHS(N)-TEMPP*TT(IP3)
       END IF
      ELSE
       IF(HX(IP2).GT.0.0D0.AND.HX(JM1).GT.0.0D0) THEN
        B(N)=B(N)-TEMPP
        C(N)=C(N)+TEMPP
        E(N)=E(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*TT(IP2)
       END IF
      END IF
!#
      END IF
!C# RIGHT      
      IF(HX(JP1).NE.0.0D0) THEN
!c      TCD=0.5D0*(THERMC(THETA(JP1),JTEX(JP1))+TC)*DXR(I)/
      TCD=0.5D0*(THERMC(THETA(JP1),JTEX(JP1))+TC)*AREAZ_kt/&
      (DZZ(J+1)-DZZ(J))
!C#    D(N)=0.5D0*(DZ1(JP1)*(RHO(N)+RHO(JP1))-DX2(N)+DX2(IP1))
      IF(.NOT.CIS) THEN
      IF(VZ(JP1).LT.0.0D0) THEN
      D(N)=D(N)-AREAZ1*VZ(JP1)
      ELSE
      E(N)=E(N)-AREAZ1*VZ(JP1)
      END IF
      ELSE
      VV=0.5D0*AREAZ1*VZ(JP1)
      D(N)=D(N)-VV
      E(N)=E(N)-VV
      END IF
!C#
      TEMPP=0.5D0*DZ2(JP1)
      IF(HX(IM2).GT.0.0D0.AND.HX(IM1).GT.0.0D0) THEN
       IF(HX(IP1).GT.0.0D0.AND.HX(IP3).GT.0.0D0) THEN
        A(N)=A(N)-TEMPP
        C(N)=C(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*(TT(IM2)-TT(IP3))
       ELSE
        A(N)=A(N)-TEMPP
        D(N)=D(N)+TEMPP
        E(N)=E(N)+TEMPP
        RHS(N)=RHS(N)+TEMPP*TT(IM2)
       END IF
      ELSE
       IF(HX(IP1).GT.0.0D0.AND.HX(IP3).GT.0.0D0) THEN
        C(N)=C(N)+TEMPP
        D(N)=D(N)-TEMPP
        E(N)=E(N)-TEMPP
        RHS(N)=RHS(N)-TEMPP*TT(IP3)
       END IF
      END IF
!C#
      END IF
!      IF(Q(N).LT.0.) E(N)=E(N)+Q(N)*RHO(N)
      IF(QQ(N).LT.0.0D0) E(N)=E(N)+QQ(N)*RHO(N)
      IF(QT(N).GT.0.0D0) E(N)=E(N)-QT(N)
!C
!C  CENTERED-IN-TIME DIFFERENCING CAN BE USED ONLY AFTER THE
!C  FIRST TIME STEP IN ANY RECHARGE PERIOD.
!C
      RHS(N) = RHS(N)*HT(N2,6)
      IF(CIT.AND.JFLAG1.NE.1) THEN
       FACT=0.5D0*HT(N2,6)
       FACT1=0.5D0
      ELSE
       FACT=HT(N2,6)
       FACT1=1.
      END IF
      A(N)=FACT*A(N)+FACT1*TCA
      B(N)=FACT*B(N)+FACT1*TCB
      C(N)=FACT*C(N)+FACT1*TCC
      D(N)=FACT*D(N)+FACT1*TCD
      E(N)=FACT*E(N)+fact1*(-TCA-TCB-TCC-TCD)
!c      END IF
      E(N)=E(N)-VOL*(((THETA(N)+SS)*RHO(N)*HT(N2,6)+HT(N2,3))/DELT)
      end if
!c
!c  above end if moved here 12/2010
!c
   20 CONTINUE
!C
!C  BEGIN LOOP TO CALCULATE RHS AND CALL MATRIX SOLVER
!C
      DO 30 I=2,NXRR
      N1=NLY*(I-1)
      DO 30 J=2,NLYY
      N=N1+J
      IM1=N-NLY
      JM1=N-1
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IM3=IM1-1
      IP3=IP1+1
      IF(RAD) THEN
      VOL=PI2*DELZ(J)*DXR(I)*RX(I)
      ELSE
      VOL=DELZ(J)*DXR(I)
      END IF
      N2=JTEX(N)

      if (ntyp(n).eq.1) then
      RHS(N)=RHS(N)-VOL*(THETA(N)*RHO(N)*HT(N2,6)+HT(N2,3))&
      *TTOLD(N)/DELT&
      -A(N)*TT(IM1)-B(N)*TT(JM1)&
      -C(N)*TT(IP1)-D(N)*TT(JP1)-E(N)*TT(N)
      else
      RHS(N)=RHS(N)-VOL*(THLST(N)*RHO(N)*HT(N2,6)+HT(N2,3))&
      *TTOLD(N)/DELT&
      -A(N)*TT(IM1)-B(N)*TT(JM1)&
      -C(N)*TT(IP1)-D(N)*TT(JP1)-E(N)*TT(N)
      end if
!#
      IF (CIT.AND.JFLAG1.NE.1) RHS(N)=RHS(N)-AO(N)*TTOLD(IM1)-BO(N)&
      *TTOLD(JM1)-CO(N)*TTOLD(IP1)-DO(N)*TTOLD(JP1)-EO(N)*TTOLD(N)
      IF(QQ(N).GT.0.0D0.and.ntyp(n).ne.1) RHS(N)=RHS(N)-QQ(N)*TS(N)&
      *RHO(N)*HT(N2,6)
      IF(QT(N).LT.0.0D0.AND.NHTYP(N).EQ.0) then
       if(cit.and.jflag1.ne.1) then
        RHS(N)=RHS(N)+0.5d0*(dum(n)+QT(N))*TS(N)*HT(N2,6)
       else
        rhs(n) = rhs(n) + qt(n)*TS(n)*ht(n2,6)
       end if
      end if
      IF(QT(N).LE.0.0D0.AND.NHTYP(N).EQ.2) RHS(N)=RHS(N)-TS(N)
   30 CONTINUE
      NIT1=NIT1+1
!
!   CALL MATRIX SOLVER
!
      CALL SLVSIP
      IF(ITEST.EQ.0) THEN
      RETURN
      END IF
   50 CONTINUE
      WRITE(6,4000)
      IF (.NOT.ITSTOP) RETURN
      JSTOP=10
      JFLAG=1
      PRINT*, 'ERROR: MAXIMUM NUMBER OF ITERATIONS EXCEEDED FOR HEAT '&
                ,' TRANSPORT EQUATION'
      WRITE(6,4010)
      RETURN
 4000 FORMAT(' MAXIMUM NUMBER OF ITERATIONS EXCEEDED FOR HEAT TRANSPORT'&
      ,' EQUATION')
 4010 FORMAT(' Simulation terminated')
      END
      SUBROUTINE VTSETUPSOL
!*******
!VTSETUPSOL
!*******
!
!    ROUTINE TO ASSEMBLE MATRIX EQUATIONS FOR ADVECTION-DISPERSION
!    EQUATIONS AND TO CALL MATRIX SOLVER.
!
      include 'd_press.inc'
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_dumm.inc'
      include 'd_disch.inc'
      include 'd_equats.inc'
      include 'd_jtxx.inc'
      include 'd_trxx.inc'
      include 'd_trxy2.inc'
      include 'd_rpropsh.inc'
      include 'd_scon.inc'
      include 'd_trxv.inc'
      include 'd_temp.inc'
      include 'd_pit.inc'
      include 'd_tempcc.inc'
      include 'd_react.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      COMMON/JCON/JSTOP,JFLAG,jflag1
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      integer hydraulicFunctionType
      common/functiontype/ hydraulicFunctionType
      COMMON/TCON1/NIS,NIS1,NIS3
      COMMON/SCON1/ITESTS     
      COMMON/JCONF/JFLAG2
!
!...........................................................................
!
      do 60 M=1,Nsol
      NIS1=0
        
      IF(jflag2.EQ.1) THEN
      DO 10 N=1,NNODES
      AOC(N)=0.0D0
      BOC(N)=0.0D0
      COC(N)=0.0D0
      DOC(N)=0.0D0
      EOC(N)=0.0D0
   10 CONTINUE
      END IF
!
!   INITIALIZE VARIABLES
!
       
      do 50 it=1,itmax
      DO 20 I=2,NXRR
      N1=NLY*(I-1)
      DO 20 J=2,NLYY
      N=N1+J
      AS(N)=0.0D0
      BS(N)=0.0D0
      CS(N)=0.0D0
      DS(N)=0.0D0
      ES(N)=0.0D0
      RHSS(N)=0.0D0
      if(it.eq.1) then
       CCOLD(M,N)=CC(M,N)
       IF(NTYP(N).EQ.1) then
        dum(n) = qs(n)
        QS(N)=VSFLX1(N)
       else
        qs(n) = 0.0d0
       end if
      end if
      TempC(N)=CC(M,N)
!      WRITE(6,*)'TempC Before ########### ',M
!     CALL VSOUTS(1,TempC(N))
      IF(HX(N).NE.0.0D0) THEN
      N2=JTEX(N)
!      RHO1=RHO(N)
!      RHO(N)=VTRHO(CC(N),N2)
!      RHO2=DABS(RHO1-RHO(N))
!      IF(RHO2.GT.RHOMAX)RHOMAX=RHO2
!      RET(N)=VTRET(CC(N),N2)
!      ret(n)=0.0D0
      IM1=N-NLY
      JM1=N-1
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IM3=IM1-1
      IP3=IP1+1
      IF(RAD) THEN
      AREAX=PI2*DELZ(J)*(RX(I)-0.5D0*DXR(I))
      AREAX1=PI2*DELZ(J)*(RX(I)+0.5D0*DXR(I))
      AREAZ=PI2*DXR(I)*RX(I)
      ELSE
      AREAX=DELZ(J)
      AREAX1=AREAX
      AREAZ=DXR(I)
      END IF
      VOL=AREAZ*DELZ(J)
      AREAX=AREAX*0.5D0*(THETA(IM1)+THETA(N))
      AREAX1=AREAX1*0.5D0*(THETA(IP1)+THETA(N))
      AREAZ1=AREAZ*0.5D0*(THETA(JP1)+THETA(N))
      AREAZ=AREAZ*0.5D0*(THETA(JM1)+THETA(N))
!
!   CALCULATE LHS OF MATRIX EQUATION
!
      SS=THETA(N)*(P(N)-PXXX(N))*HK(N2,2)/HK(N2,3)
      ES(N)=-DXS1(N)-DZS1(N)-DXS1(IP1)-DZS1(JP1)
!   &-VOL*(HT(N2,4)*(THETA(N)+RET(N)))
!
!  CHANGE ADDED 8-12-91 TO CORRECT STORAGE TERM
!
!      SS1=HT(N2,4)*(THETA(N)+RET(N))
!
!c******************
!  following change made 7-3-04 to correct dctheta/dt 
!   calculation - see written notes
!
!******************
!      if(jflag1.ne.1.or.ntyp(n).ne.1) then
!       SS=THETA(N)+SS-THLST(N)
!     end if
!******************
     
!C#
      AS(N)=DXS1(N)
      BS(N)=DZS1(N)

      CS(N)=DXS1(IP1)
      DS(N)=DZS1(JP1)
!C#    TOP(	n,j-1)
      IF(HX(IM1).NE.0.0D0) THEN
!C#    A(N)=0.5D0*(DX1(N)*(RHO(N)+RHO(IM1))+DZ2(N)-DZ2(JP1))
      IF(.NOT.CIS) THEN
      IF(VX(N).GT.0.0D0) THEN
      AS(N)=AS(N)+AREAX*VX(N)
      ELSE
      ES(N)=ES(N)+AREAX*VX(N)
      END IF
      ELSE
      VV=AREAX*0.5D0*VX(N)
      AS(N)=AS(N)+VV
      ES(N)=ES(N)+VV
      END IF
!C#
      TEMPP=0.5D0*DXS2(N)
      IF(HX(IM3).GT.0.0D0 .AND. HX(JM1).GT.0.0D0) THEN
       IF(HX(IM2).GT.0.0D0 .AND. HX(JP1).GT.0.0D0) THEN
        BS(N)=BS(N)+TEMPP
        DS(N)=DS(N)-TEMPP
        RHSS(N)=RHSS(N)+TEMPP*(CC(M,IM2)-CC(M,IM3))
       ELSE
        AS(N)=AS(N)-TEMPP
        BS(N)=BS(N)+TEMPP
        ES(N)=ES(N)-TEMPP
        RHSS(N)=RHSS(N)-TEMPP*CC(M,IM3)
       END IF
      ELSE
       IF(HX(IM2).GT.0.0D0 .AND. HX(JP1).GT.0.0D0) THEN
        AS(N)=AS(N)+TEMPP
        DS(N)=DS(N)-TEMPP
        ES(N)=ES(N)+TEMPP
        RHSS(N)=RHSS(N)+TEMPP*CC(M,IM2)
       END IF
!C#
      END IF
      END IF

!C#    left (n-1,j)      
      IF(HX(JM1).NE.0.0D0) THEN
!C#    B(N)=0.5D0*(DZ1(N)*(RHO(N)+RHO(JM1))+DX2(N)-DX2(IP1))
      IF(.NOT.CIS) THEN
      IF(VZ(N).GT.0.0D0) THEN
      BS(N)=BS(N)+AREAZ*VZ(N)
      ELSE
      ES(N)=ES(N)+AREAZ*VZ(N)
      END IF
      ELSE
      VV=0.5D0*AREAZ*VZ(N)
      BS(N)=BS(N)+VV
      ES(N)=ES(N)+VV
      END IF
!C#
      TEMPP=0.5D0*DZS2(N)
      IF(HX(IP2).GT.0.0D0 .AND. HX(IP1).GT.0.0D0) THEN
       IF(HX(IM3).GT.0.0D0 .AND. HX(IM1).GT.0.0D0) THEN
        AS(N)=AS(N)+TEMPP
        CS(N)=CS(N)-TEMPP
        RHSS(N)=RHSS(N)+TEMPP*(CC(M,IP2)-CC(M,IM3))
       ELSE
        BS(N)=BS(N)+TEMPP
        CS(N)=CS(N)-TEMPP
        ES(N)=ES(N)+TEMPP
        RHSS(N)=RHSS(N)+TEMPP*CC(M,IP2)
       END IF
      ELSE
       IF(HX(IM3).GT.0.0D0 .AND. HX(IM1).GT.0.0D0) THEN
        AS(N)=AS(N)+TEMPP
        BS(N)=BS(N)-TEMPP
        ES(N)=ES(N)-TEMPP
        RHSS(N)=RHSS(N)-TEMPP*CC(M,IM3)
       END IF
      END IF
!C#
      END IF

!C#  Bottom (n,j+1)      
      IF(HX(IP1).NE.0.0D0) THEN
!C#    C(N)=0.5D0*(DX1(IP1)*(RHO(N)+RHO(IP1))-DZ2(N)+DZ2(JP1))
      IF(.NOT.CIS) THEN
      IF(VX(IP1).LT.0.0D0) THEN
      CS(N)=CS(N)-AREAX1*VX(IP1)
      ELSE
      ES(N)=ES(N)-AREAX1*VX(IP1)
      END IF
      ELSE
      VV=0.5D0*AREAX1*VX(IP1)
      CS(N)=CS(N)-VV
      ES(N)=ES(N)-VV
      END IF
!C#
      TEMPP=0.5D0*DXS2(IP1)
      IF(HX(JP1).GT.0.0D0 .AND. HX(IP3).GT.0.0D0) THEN
       IF(HX(IP2).GT.0.0D0 .AND. HX(JM1).GT.0.0D0) THEN
        BS(N)=BS(N)-TEMPP
        DS(N)=DS(N)+TEMPP
        RHSS(N)=RHSS(N)+TEMPP*(CC(M,IP2)-CC(M,IP3))
       ELSE
        CS(N)=CS(N)-TEMPP
        DS(N)=DS(N)+TEMPP
        ES(N)=ES(N)-TEMPP
        RHSS(N)=RHSS(N)-TEMPP*CC(M,IP3)
       END IF
      ELSE
       IF(HX(IP2).GT.0.0D0 .AND. HX(JM1).GT.0.0D0) THEN
        BS(N)=BS(N)-TEMPP
        CS(N)=CS(N)+TEMPP
        ES(N)=ES(N)+TEMPP
        RHSS(N)=RHSS(N)+TEMPP*CC(M,IP2)
       END IF
      END IF
!C#
      END IF

!C#  right (n+1,j)      
      IF(HX(JP1).NE.0.0D0) THEN
!C#    D(N)=0.5D0*(DZ1(JP1)*(RHO(N)+RHO(JP1))-DX2(N)+DX2(IP1))
      IF(.NOT.CIS) THEN
      IF(VZ(JP1).LT.0.0D0) THEN
      DS(N)=DS(N)-AREAZ1*VZ(JP1)
      ELSE
      ES(N)=ES(N)-AREAZ1*VZ(JP1)
      END IF
      ELSE
      VV=0.5D0*AREAZ1*VZ(JP1)
      DS(N)=DS(N)-VV
      ES(N)=ES(N)-VV
      END IF
!C#
      TEMPP=0.5D0*DZS2(JP1)
      IF(HX(IM2).GT.0.0D0 .AND. HX(IM1).GT.0.0D0) THEN
       IF(HX(IP1).GT.0.0D0 .AND. HX(IP3).GT.0.0D0) THEN
        AS(N)=AS(N)-TEMPP
        CS(N)=CS(N)+TEMPP      
 !     CALL VSOUTS(1,TempC(N))
        RHSS(N)=RHSS(N)+TEMPP*(CC(M,IM2)-CC(M,IP3))
       ELSE
        AS(N)=AS(N)-TEMPP
        DS(N)=DS(N)+TEMPP
        ES(N)=ES(N)+TEMPP
        RHSS(N)=RHSS(N)+TEMPP*CC(M,IM2)
       END IF
      ELSE
       IF(HX(IP1).GT.0.0D0 .AND. HX(IP3).GT.0.0D0) THEN
        CS(N)=CS(N)+TEMPP
        DS(N)=DS(N)-TEMPP
        ES(N)=ES(N)-TEMPP
        RHSS(N)=RHSS(N)-TEMPP*CC(M,IP3)
       END IF
      END IF
!C#
      END IF
      if (NPV.ge.0) then
      IF(Q(N).LT.0.0D0 .AND. NTYP(N) .NE. 5) ES(N)=ES(N)+Q(N)
      end if  
      IF(QQ(N).LT.0.0D0) ES(N)=ES(N)+QQ(N)
      IF(QS(N).GT.0.0D0) ES(N)=ES(N)-QS(N)
!C
!C  CENTERED-IN-TIME DIFFERENCING CAN BE USED ONLY AFTER THE
!C  FIRST TIME STEP IN ANY RECHARGE PERIOD.
!C
   
      IF(CIT.AND.JFLAG2.NE.1) THEN
      AS(N)=0.5D0*AS(N)
      BS(N)=0.5D0*BS(N)
      CS(N)=0.5D0*CS(N)
      DS(N)=0.5D0*DS(N)
      ES(N)=0.5D0*ES(N)
      END IF
      ES(N)=ES(N)-VOL*(THETA(N)+SS)/DELT 
      END IF
   20 CONTINUE
 !     WRITE(6,*)'TempC Before ########### ',M
 !     CALL VSOUTS(1,TempC(N))
   
!
!  BEGIN LOOP TO CALCULATE RHS AND CALL MATRIX SOLVER
!
      
      DO 30 I=2,NXRR
       N1=NLY*(I-1)
      DO 30 J=2,NLYY
      N=N1+J
      IM1=N-NLY
      JM1=N-1
      JP1=N+1
      IP1=N+NLY
      IP2=IP1-1
      IM2=IM1+1
      IM3=IM1-1
      IP3=IP1+1
      IF(RAD) THEN
      VOL=PI2*DELZ(J)*DXR(I)*RX(I)
      ELSE
      VOL=DELZ(J)*DXR(I)
      END IF
      N2=JTEX(N)

      if (ntyp(n).eq.1) then
      RHSS(N)=RHSS(N)-VOL*THETA(N)*CCOLD(M,N)/DELT-AS(N)*CC(M,IM1)&
      -BS(N)*CC(M,JM1)-CS(N)*CC(M,IP1)-DS(N)*CC(M,JP1)-ES(N)*CC(M,N)
      else
      RHSS(N)=RHSS(N)-VOL*THLST(N)*CCOLD(M,N)/DELT-AS(N)*CC(M,IM1)&
      -BS(N)*CC(M,JM1)-CS(N)*CC(M,IP1)-DS(N)*CC(M,JP1)-ES(N)*CC(M,N)
      end if
!C#
      IF (CIT.AND.JFLAG2.NE.1) RHSS(N)=RHSS(N)-AOC(N)*CCOLD(M,IM1)&
      -BOC(N)*CCOLD(M,JM1)-COC(N)*CCOLD(M,IP1)-DOC(N)*CCOLD(M,JP1)&
      -EOC(N)*CCOLD(M,N)
      IF(QQ(N).GT.0.0D0 .and. ntyp(n).ne.1)RHSS(N)=RHSS(N)-QQ(N)&
      *CSS(M,N)
      IF(QS(N).LT.0.0D0 .AND. NCTYP(N).EQ.0) then
       if(cit.and.jflag2.ne.1) then
        RHSS(N)=RHSS(N)+0.5d0*(QS(N)+dum(n))*CSS(M,N)
       else
        RHSS(N) = RHSS(N)+ QS(N)*CSS(M,N)
       end if
      end if
      IF(QS(N).LE.0.0D0 .AND.NCTYP(N).EQ.2)RHSS(N)=RHSS(N)-CSS(M,N)
       
   30 CONTINUE

      NIS1=NIS1+1
!
!   CALL MATRIX SOLVER
!
      CALL SLVSIPSOL
      DO 31	I=2,NXRR
       N1=NLY*(I-1)
      DO 31 J=2,NLYY
      N=N1+J
      CC(M,N)=TempC(N)
  31  CONTINUE   
!C      WRITE(6,*)'TempC After ########### ',M
!      CALL VSOUTS(1,TempC(N))
      IF(ITESTS.EQ.0) THEN
      IF (CIT) THEN
      DO 40 I=2,NXRR
      N1=NLY*(I-1)
      DO 40 J=2,NLYY
      N=N1+J
      IF(HX(N).EQ.0.0D0) GO TO 40
      if(nctyp(n).ne.1) then
       AOC(N)=AS(N)
       BOC(N)=BS(N)
       COC(N)=CS(N)
       DOC(N)=DS(N)
      end if
      IF(RAD) THEN
      AREAZ=PI2*DXR(I)*RX(I)
      ELSE
      AREAZ=DXR(I)
      END IF
      VOL=AREAZ*DELZ(J)
      N2=JTEX(N)
      SS=theta(n)*(P(N)-PXXX(N))*HK(N2,2)/HK(N2,3)
!
!  CHANGE 8-12-91 FOR STORAGE
!
!      SS1=HT(N2,4)*(THETA(N)+RET(N))
!
!****************
!c  following change made 7-3-04 to correct dctheta/dt
!   calculation
!
!****************
!      if(jflag1.ne.1.or.ntyp(n).ne.1) then
!       SS=THETA(N)+SS-THLST(N)
!      end if
!****************
      EOC(N)=ES(N)+VOL*(THETA(N)+SS)/DELT
      IF(JFLAG1.EQ.1) THEN
       if(nctyp(n).ne.1) then
        AOC(N)=0.5D0*AOC(N)
        BOC(N)=0.5D0*BOC(N)
        COC(N)=0.5D0*COC(N)
        DOC(N)=0.5D0*DOC(N)
       end if
       EOC(N)=0.5D0*EOC(N)
      END IF
   40 CONTINUE
      END IF
      go to 60
      END IF

   50 CONTINUE
      WRITE(6,4000)
      IF (.NOT.ITSTOP) RETURN
      JSTOP=10
      JFLAG=1
      PRINT*, 'ERROR: MAXIMUM NUMBER OF ITERATIONS EXCEEDED FOR SOLUTE '&
                ,' TRANSPORT EQUATION'
      WRITE(6,4010)
   60 CONTINUE   
      RETURN
 4000 FORMAT('MAXIMUM NUMBER OF ITERATIONS EXCEEDED FOR SOLUTE '&
      ,' TRANSPORT EQUATION')
 4010 FORMAT(' Simulation terminated')
      END
            DOUBLE PRECISION FUNCTION VTRHO(TT,I)
!*****
!VTRHO
!*****
!
! DENSITY AS A FUNCTION OF TEMPERATURE CC
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_rpropsh.inc'
!c      VTRHO=HT(I,6)+HT(I,8)*(CC-HT(I,7))
      vtrho=1.0D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION THERMC(TT,I)
!*****
!THERMC
!*****
!
! THERMAL CONDUCTIVITY AS FUNCTION OF THETA
!
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      integer hydraulicFunctionType
      common/functiontype/ hydraulicFunctionType
!c      include 'c_rpropsh.inc'
      if (hydraulicFunctionType.lt.3) then
       T1=HK(I,5)
      else
       T1 = 0.d0
      end if
      T2=HK(I,3)
      IF(TT.LE.T1) THEN
       THERMC=HT(I,4)
      ELSE
       IF(TT.GE.HK(I,3)) THEN
        THERMC=HT(I,5)
       ELSE
        W1=(TT-HK(I,5))/(HK(I,3)-T1)
        W2=1-W1
        THERMC=W1*HT(I,4)+W2*HT(I,5)
       END IF
      END IF
      RETURN
      END

      SUBROUTINE vsgrav_dr
!******
!CVSgrav_dr
!******
!C
!  PURPOSE: TO COMPUTE flows from gravity drainage boundaries
!
!
!------------------------------------------------------------------
!
!   SPECIFICATIONS FOR ARRAYS AND SCALARS
!
      include 'd_kcon.inc'
      include 'd_hcon.inc'
      include 'd_disch.inc'
      include 'd_press.inc'
      include 'd_rspac.inc'
      include 'd_trxx.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)

      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      LOGICAL RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
      COMMON/LOG1/RAD,BCIT,ETSIM,SEEP,ITSTOP,CIS,CIT,GRAV
!
!-----------------------------------------------------------------
!
      DO 10 J=2,NLYY
      DO 10 N=2,NXRR
      IN=NLY*(N-1)+J
      IF(NTYP(IN).EQ.7) then
        AREA=DXR(N)
        IF(RAD)AREA=PI2*RX(N)*DXR(N)
        qq(in) = -hx(in)*hcnd(in)*AREA
      END IF
   10 CONTINUE
      RETURN
      END



!
!   Following are subroutines that are required for connecting
!  VS2DT fortran program to VS2DTI post processor
!
      SUBROUTINE GETNX(NX)
! *** GET THE NUMBER OF CELLS IN THE X DIRECTION
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      NX = NXR
      RETURN
      END

      SUBROUTINE GETNZ(NZ)
! *** GET THE NUMBER OF CELLS IN THE Z DIRECTION
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      NZ = NLY
      RETURN
      END

      SUBROUTINE GETDX(DX, NX)
!C *** GET THE CELL SIZES IN THE X DIRECTION AND RETURN IT IN THE
!C *** ARRAY DX PROVIDED BY THE CALLING PROGRAM
      include 'd_rspac.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_rspac.inc'
      DIMENSION DX(NX)
      DO 10 I=1, NX
         DX(I) = DXR(I)
  10  CONTINUE
      RETURN
      END

      SUBROUTINE GETDZ(DZ, NZ)
!C *** GET THE CELL SIZES IN THE Z DIRECTION AND RETURN IT IN THE
!C *** ARRAY DZ PROVIDED BY THE CALLING PROGRAM
      include 'd_rspac.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_rspac.inc'
      DIMENSION DZ(NZ)
      DO 10 I=1, NZ
         DZ(I) = DELZ(I)
  10  CONTINUE
      RETURN
      END

      SUBROUTINE GETCONC(C, N,NC)
!C *** GET THE CONCENTRATION ARRAY AND PUT IT IN C, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_trxx.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_trxx.inc'
      DIMENSION C(N,NC)
      DO 10 I=1,NC
      DO 10 N=1,Nsol  
         C(N,I) = CC(N,I)    
   10 CONTINUE
      RETURN
      END

      SUBROUTINE GETKSAT(HX1, NN)
!C *** GET THE SATURATED HYDRAULIC CONDUCTIVITY AND RETURN IT IN HX1, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_kcon.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_kcon.inc'
      DIMENSION HX1(NN)
      DO 10 I=1,NN
         HX1(I) = HX(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE GETTEX(JT, NN)
!C *** GET THE TEXTURAL CLASS MAP AND RETURN IT IN JT, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_jtxx.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_jtxx.inc'
      DIMENSION JT(NN)
      DO 10 I=1,NN
         JT(I) = JTEX(I)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE GETMOIST(THETA1, NN)
!C *** GET THE MOISTURE CONTENTS AND RETURN IT IN THETA1, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_mprop.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!      include 'c_mprop.inc'
      DIMENSION THETA1(NN)
      DO 10 I=1,NN
         THETA1(I) = THETA(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE GETSAT(SAT, NN)
!C *** GET THE SATURATION AND RETURN IT IN SAT, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_mprop.inc'
      include 'd_jtxx.inc'
      include 'd_rpropsh.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_mprop.inc'
!c      include 'c_jtxx.inc'
!c      include 'c_rpropsh.inc'
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      DIMENSION SAT(NN)
      DO 70 J=2,NLYY
        DO 70 N=2,NXRR
          IN=NLY*(N-1)+J
          TTX=HK(JTEX(IN),3)
          IF(TTX.EQ.0.0D0) THEN
            SAT(IN)=0.0D0
          ELSE
            SAT(IN)=THETA(IN)/TTX
          END IF
   70 CONTINUE
      RETURN
      END

      SUBROUTINE GETPHEAD(PHEAD, NN)
!C *** GET THE PRESSURE HEAD AND RETURN IT IN PHEAD, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_press.inc'
      include 'd_rspac.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_press.inc'
!c      include 'c_rspac.inc'
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      DIMENSION PHEAD(NN)
      DO 70 J=1,NLY
        DO 70 N=1,NXR
          IN=NLY*(N-1)+J
          IF(CS1.EQ.1.0D0) THEN
            Z1=DZZ(J)
          ELSE
            Z1=DZZ(J)*CS1+RX(N)*CS2
          END IF
          PHEAD(IN) = P(IN)+Z1
   70 CONTINUE
      RETURN
      END

      SUBROUTINE GETSTIME(STIME)
!C *** GET THE SIMULATION TIME AND RETURN IT IN STIME
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      STIME = STIM
      RETURN
      END

      SUBROUTINE GETSTEP(KTIME)
!C *** GET THE TIME STEP AND RETURN IT IN KTIME
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      KTIME = KTIM
      RETURN
      END

      SUBROUTINE CLOSEIO
!C *** CLOSE ALL IO UNITS
      CLOSE(2)
      CLOSE(5)
      CLOSE(6)
      CLOSE(7)
      CLOSE(8)
      CLOSE(9)
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)
      CLOSE(15)
      CLOSE(16)
      CLOSE(17)
      CLOSE(18)
      CLOSE(19)
      RETURN
      END

      SUBROUTINE RELEASEMEMORY
      include 'd_rspac.inc'
      include 'd_kcon.inc'
      include 'd_mprop.inc'
      include 'd_press.inc'
      include 'd_disch.inc'
      include 'd_hcon.inc'
      include 'd_equat.inc'
      include 'd_equats.inc'
      include 'd_temp.inc'
      include 'd_jtxx.inc'
      include 'd_dumm.inc'
      include 'd_dumm1.inc'
      include 'd_dumm2.inc'
      include 'd_dumm3.inc'
      include 'd_ptet.inc'
      include 'd_trxx.inc'
      include 'd_trxxh.inc'
      include 'd_trxv.inc'
      include 'd_trxy1.inc'
      include 'd_trxy2.inc'
      include 'd_pit.inc'
      include 'd_sip.inc'
      include 'd_idumm.inc'
      include 'd_plott.inc'
      include 'd_rpropsh.inc'
      include 'd_spfc.inc'
      include 'd_scon.inc'
      include 'd_ptet.inc'
      include 'd_tempcc.inc'
      include 'd_coordin.inc'
      include 'd_solindex.inc'
      include 'd_phreecc.inc'
      include 'd_pricont.inc'
      include 'd_solmassb.inc'
      include 'd_compnamm.inc'
      include 'd_BF.inc'
      include 'd_isdum.inc'
      include 'd_ihdum.inc'
      include 'd_itemblo.inc'
      include 'd_itemtxb.inc'
      include 'd_react.inc'
      include 'd_cleanup.inc'
      RETURN
      END

      SUBROUTINE GETFLOWMBERR(ERR)
!C *** GET TOTAL AND RATE FLOW MASS BALANCE ERRORS AND RETURN IN ERR
      include 'd_scon.inc'
      include 'd_solmassb.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c     include 'c_scon.inc'      
      COMMON/MASSB/BL(99),bcmft,bcmht,bl29I,bl29IT,bl29O,bl29OT, &
      bl95I,bl95IT,bl95o,bl95OT
      common/massb1/bcmf,bcmh, &
      bltemp69,bltemp72,bltemp75,bltemp78,bltemp91
 !     common/massb2/label9
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      DIMENSION ERR(2)
      IF(KTIM.EQ.0) THEN
        ERR(1) = 0
        ERR(2) = 0
        RETURN
      END IF
      FMB1 = BL(13)
      FMB2 = BL(16) + BL(25)
      FMB3 = bl29IT
      FMB4 = bl29OT
      FMB5 = BL(15)
      FMB6 = BL(18) + BL(27)
      FMB7 = bl29I/DELT
      FMB8 = bl29O/DELT
      E1 = FMB1 - FMB4
      E2 = -FMB2 + FMB3
      D = (E1 + E2)/2
      IF (D.NE.0) THEN
        ERR(1) = 100 * (E1 - E2)/D
      ELSE
        ERR(1) = 0
      END IF
      E1 = FMB5 - FMB8
      E2 = -FMB6 + FMB7
      D = (E1 + E2)/2
      IF (D.NE.0) THEN
        ERR(2) = 100 * (E1 - E2)/D
      ELSE
        ERR(2) = 0
      END IF
      RETURN
      END

      SUBROUTINE GETHEATTRANSMBERR(ERR)
!C *** GET TOTAL AND RATE CHEMICAL MASS BALANCE ERRORS AND RETURN IN ERR
      include 'd_scon.inc'
      include 'd_solmassb.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_scon.inc'
      COMMON/MASSB/BL(99),bcmft,bcmht,bl29I,bl29IT,bl29O,bl29OT, &
      bl95I,bl95IT,bl95o,bl95OT
      common/massb1/bcmf,bcmh, &
      bltemp69,bltemp72,bltemp75,bltemp78,bltemp91
      COMMON/TCON/STIM,DSMAX,KTIM,NIT,NIT1,KP,NIT3
      DIMENSION ERR(2)
      IF(KTIM.EQ.0) THEN
        ERR(1) = 0
        ERR(2) = 0
        RETURN
      END IF
      TMB1 = BL(85)
      TMB2 = BL(88)+BL(91)
      TMB3 = bl68IT
      TMB4 = bl68OT
      TMB5 = BL(54)
      TMB6 = BL(90)+BL(93)
      TMB7 = bl95I/DELT
      TMB8 = bl95O/DELT
      E1 = TMB1 - TMB4
      E2 = -TMB2 + TMB3
      D = (E1 + E2)/2
      IF (D.NE.0) THEN
        ERR(1) = 100 * (E1 - E2)/D
      ELSE
        ERR(1) = 0
      END IF
      E1 = TMB5 - TMB8
      E2 = -TMB6 + TMB7
      D = (E1 + E2)/2
      IF (D.NE.0) THEN
        ERR(2) = 100 * (E1 - E2)/D
      ELSE
        ERR(2) = 0
      END IF
      RETURN
      END

      SUBROUTINE GETVX(VELX, NN)
!C *** GET THE X VELOCITY AND RETURN IT IN VELX, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_kcon.inc'
      include 'd_trxv.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_kcon.inc'
!c      include 'c_trxx.inc'
     LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      DIMENSION VELX(NN)
      IF(.NOT.TRANS) CALL VTVELO
      DO 70 J=1,NLY
        DO 70 N=1,NXR
          IN=NLY*(N-1)+J
          IF(HX(IN).EQ.0.0D0) THEN
            VELX(IN)=0.0D0
          ELSE
            VELX(IN)=(VX(IN)+VX(IN+NLY))/2
          END IF
   70 CONTINUE
      RETURN
      END

      SUBROUTINE GETVZ(VELZ, NN)
!C *** GET THE Z VELOCITY AND RETURN IT IN VELZ, WHICH IS PROVIDED
!C *** BY THE CALLING PROGRAM
      include 'd_kcon.inc'
      include 'd_trxv.inc'
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
!c      include 'c_kcon.inc'
!c      include 'c_trxx.inc'
      COMMON/ISPAC/NLY,NLYY,NXR,NXRR,NNODES,Nsol,Nodesol
      DIMENSION VELZ(NN)
      DO 70 J=1,NLY
        DO 70 N=1,NXR
          IN=NLY*(N-1)+J
          IF(HX(IN).EQ.0.0D0) THEN
            VELZ(IN)=0.0D0
          ELSE
            VELZ(IN)=(VZ(IN)+VZ(IN+1))/2
          END IF
   70 CONTINUE
      RETURN
      END

      SUBROUTINE DOTRANS(IFLAG)
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      LOGICAL TRANS,TRANS1,TRANS2,SSTATE
      COMMON/TRXY/EPS1,EPS2,EPS3,TRANS,TRANS1,TRANS2,SSTATE,MB9(99),NMB9
      IF (TRANS) THEN
        IFLAG=1
      ELSE
        IFLAG=0
      ENDIF
      RETURN
      END
