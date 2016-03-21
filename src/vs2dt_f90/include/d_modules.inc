      module PLOTT
      double precision, allocatable::PLTIM(:)
      integer, allocatable::IJOBS(:)
      integer JPLT,NPLT,NOBS,KPLT
      end module PLOTT

      module IDUMM
      integer, allocatable::IDUM(:)
      end module IDUMM     

      module DUMM1
      double precision, allocatable::DUM1(:)
      end module DUMM1

      module SPFC
      integer, dimension(:,:,:), allocatable::JSPX
      integer, allocatable::NFC(:),JLAST(:)
      integer NFCS
      end module SPFC

      module RPROP
      double precision, dimension(:,:), allocatable::HK,HT
      double precision, allocatable::ANIZ(:)
      end module RPROP

      module RSPAC
      double precision, allocatable::DELZ(:),DZZ(:),DXR(:),RX(:)
      double precision PI2
      end module RSPAC

      module KCON
      double precision, allocatable::HX(:)
	  integer, allocatable::NTYP(:)
      end module KCON

      module MPROP
      double precision, allocatable::THETA(:),THLST(:)
      end module MPROP

      module PRESS
      double precision, allocatable::P(:),PXXX(:)
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
      double precision, allocatable::A(:),B(:),C(:),D(:),E(:),
     &    RHS(:),XI(:)
      end module EQUAT

      module JTXX
      integer, allocatable::JTEX(:)
      end module JTXX

      module DUMM
      double precision, allocatable::DUM(:)
      end module DUMM

      module PTET
      double precision, allocatable::DPTH(:),RT(:),PEVAL(:),PTVAL(:)
      double precision, dimension(:,:), allocatable::RDC
      double precision ETCYC,
     &    PET,PEV,HROOT,HA,SRES,RTDPTH,RTBOT,RTTOP
	  integer NPV
      end module PTET

      module TRXX
      double precision, allocatable::DX1(:),DX2(:),DZ1(:),DZ2(:),
     &    VX(:),VZ(:),CC(:),COLD(:),CS(:),QT(:),RET(:)
	  integer, allocatable::NCTYP(:)
      end module TRXX

      module TRXY1
      double precision, allocatable::AO(:),BO(:),CO(:),DO(:),EO(:)
      end module TRXY1

      module PIT
      double precision, allocatable::PITT(:)
      end module PIT

      module SIP
      double precision, allocatable::DEL(:),ETA(:),V(:)
      end module SIP

      module SCON
      double precision, allocatable::DHMX(:)
      double precision DELT, HMAX, TMAX, EPS
      integer NUMT, ITMAX, MINIT, ITEST
      end module SCON

      module BF
      double precision, allocatable::totalBF(:,:)
      double precision, allocatable::currentBF(:,:)
      integer numBF, maxnumbercells
      integer, allocatable::idBF(:)
      integer, allocatable::numcellsBF(:)
      integer, allocatable::nodenum(:,:)
      end module BF
