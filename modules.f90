      module PLOTT
      double precision, allocatable::PLTIM(:)
      integer, allocatable::IJOBS(:)
      integer JPLT,NPLT,NOBS,KPLT
      end module PLOTT

      module IDUMM
      integer, allocatable::IDUM(:)
      end module IDUMM
      
      module ITEXS
      integer, allocatable:: ITEXSOL(:,:)
      end module ITEXS
      
      module ITEMBLO
      integer, allocatable::ITEMBL(:),ITBDUM(:)
      end module ITEMBLO
       
      module ITEMTXB
      integer, allocatable::ITEMTX(:),ITDUM(:)
      end module ITEMTXB 

      module IHDUMM
      integer, allocatable::IHDUM(:)
      end module IHDUMM

      module ISDUMM
      integer, allocatable::ISDUM(:)
      end module ISDUMM  

      module DUMM1
      double precision, allocatable::DUM1(:)
      end module DUMM1

      module DUMM2
      double precision, allocatable::DUM2(:,:)
      end module DUMM2
      module DUMM3
      double precision, allocatable::DUM3(:,:)
      end module DUMM3

      module SPFC
      integer, dimension(:,:,:), allocatable::JSPX
      integer, allocatable::NFC(:),JLAST(:)
      integer NFCS
      end module SPFC

      module RPROPSH
      double precision, dimension(:,:), allocatable::HK,HT,HS
      double precision, allocatable::ANIZ(:)
      end module RPROPSH

      module RSPAC
      double precision, allocatable::DELZ(:),DZZ(:),DXR(:),RX(:)
      double precision PI2
      end module RSPAC

      module KCON
      double precision, allocatable::HX(:)
      integer, allocatable::NTYP(:)
      end module KCON

      module MPROP
      double precision, allocatable::THETA(:),THLST(:),SATUR(:),POROSITY(:)
      end module MPROP

      module PRESS
      double precision, allocatable::P(:),PXXX(:),THEAD(:)
      double precision CS1,CS2
      end module PRESS

      module DISCH
      double precision, allocatable::Q(:),QQ(:)
      double precision ETOUT,ETOUT1
      end module DISCH

      module HCON
      double precision, allocatable::HCND(:),HKLL(:),HKTT(:)
      end module HCON

      module EQUAT
      double precision, allocatable::A(:),B(:),C(:),D(:),E(:), &
        RHS(:),XI(:)
      end module EQUAT
      
      module EQUATS
      double precision, allocatable::AS(:),BS(:),CS(:),DS(:),  &
           ES(:),RHSS(:),XIS(:)
      end module EQUATS

      module JTXX
      integer, allocatable::JTEX(:)
      end module JTXX

      module DUMM
      double precision, allocatable::DUM(:)
      double precision, allocatable::PDUM(:)
      end module DUMM

      module PTET
      double precision, allocatable, dimension(:) :: DPTH, RT, PEVAL, PTVAL
      double precision, dimension(:,:), allocatable :: RDC
      double precision ETCYC, PET,PEV,HROOT,HA,SRES,RTDPTH,RTBOT,RTTOP
      integer :: NPV = 0
      end module PTET

      module TRXV
      double precision, allocatable::VX(:),VZ(:)
      end module TRXV

      module TRXX
      double precision, allocatable::DXS1(:),DXS2(:),DZS1(:),DZS2(:),  &
          CC(:,:),CCOLD(:,:),CSS(:,:),QS(:)
      integer, allocatable::NCTYP(:)
      character*10, allocatable::CONC(:)
      end module TRXX

      module TRXXH
      double precision, allocatable::DX1(:),DX2(:),DZ1(:),DZ2(:),  &
          TT(:),TTOLD(:),TS(:),QT(:)
      integer, allocatable::NHTYP(:)
      end module TRXXH

      module TRXY1
      double precision, allocatable::AO(:),BO(:),CO(:),DO(:),EO(:)
      end module TRXY1
      
      module TRXY2
      double precision, allocatable::AOC(:),BOC(:),COC(:),DOC(:),EOC(:)
      end module TRXY2

      module PIT
      double precision, allocatable::PITT(:)
      end module PIT

      MODULE REDUCE_TIME
      LOGICAL REDUCE_TIME_STEP
      END MODULE REDUCE_TIME
    
      module SIP
      double precision, allocatable::DEL(:),ETA(:),V(:)
      end module SIP

      module SCON
      double precision, allocatable::DHMX(:)
      double precision DELT, HMAX, TMAX, EPS
      integer NUMT, ITMAX, MINIT, ITEST
      end module SCON

      module gmres1
      double precision, allocatable::a_gmr(:)
      double precision, allocatable::rhs_gmr(:)
      double precision, allocatable::x3(:)
      integer (kind=4), allocatable::ia_gmr(:)
      integer (kind=4), allocatable::ja_gmr(:)
      end module gmres1

      module TEMP
      double precision, allocatable::RHO(:),RHOOLD(:)
      double precision RHOMAX
      end module TEMP

      module PRICON
      integer, allocatable::NPRCHEM(:),NPRCHXZ(:),NPRCHOBS(:)
      integer NPRCONC,NPSCRN,IPOUT
      end module PRICON

      module TEMPCC
      double precision, allocatable::TempC(:)
      end module TEMPCC

      module COORDIN
      !double precision, allocatable:: XNODE(:),ZNODE(:)
      integer, allocatable:: XNODE(:),ZNODE(:)
      end module COORDIN

      module SOLINDEX
      double precision, allocatable:: CMIXFARC(:,:),BCSOL(:)
      integer, allocatable:: INDSOL1(:,:),INDSOL2(:,:), ic1_reordered(:,:)
      end module SOLINDEX

      module PHREECC
      double precision, allocatable:: phreeC(:)
      end module PHREECC

      module SOLMASS
      double precision, allocatable::BLSOL(:,:),bl62I(:), &
           bl62IT(:),bl62O(:),bl62OT(:), bcmtt(:),bcmt(:),bcmtr(:),&
           bltemp36(:),bltemp39(:),bltemp42(:),bltemp45(:), &
           bltemp60(:)
      end module SOLMASS

      Module COMPNAM
      character(10) ,allocatable:: COMPNAME(:)
      end module COMPNAM

      MODULE REACT
      double precision, allocatable ::CCBR(:,:),CCAR(:,:)
      END MODULE REACT
      module BF
      double precision, allocatable::totalBF(:,:)
      double precision, allocatable::currentBF(:,:)
      integer numBF, maxnumbercells
      integer, allocatable::idBF(:)
      integer, allocatable::numcellsBF(:)
      integer, allocatable::nodenum(:,:)
      end module BF
