C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE HDIFF
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    HDIFF       HORIZONTAL DIFFUSION
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
C     
C ABSTRACT:
C     HDIFF CALCULATES THE CONTRIBUTION OF THE HORIZONTAL DIFFUSION
C     TO THE TENDENCIES OF TEMPERATURE, SPECIFIC HUMIDITY, WIND
C     COMPONENTS, AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE
C     VARIABLES.  A SECOND-ORDER NONLINEAR SCHEME SIMILAR TO
C     SMAGORINSKYS IS USED WHERE THE DIFFUSION COEFFICIENT IS
C     A FUNCTION OF THE DEFORMATION FIELD AND OF THE TURBULENT
C     KINETIC ENERGY.
C     
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-28  BLACK      - ADDED EXTERNAL EDGE
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL HDIFF FROM MAIN PROGRAM EBU
C
C   INPUT ARGUMENT LIST:
C       NONE     
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  MASKS
C                  PHYS
C                  VRBLS
C                  PVRBLS
C                  INDX
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
                             P A R A M E T E R
     & (EPSQ2=0.2,FCDIF=1.0,RFCP=.25/1004.6,CP=1004.6,CAPA=0.28589641)
                             P A R A M E T E R
     & (ELWV=2.50E6,ELIV=2.834E6,CLIMIT=1.E-12)
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LP1=LM+1,KSMUD=1)
                             P A R A M E T E R
     &(JAM=6+2*(JM-10),JAMD=(JAM*2-10)*3)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,HEAT
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAMD.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & HKNE (IDIM1:IDIM2,JDIM1:JDIM2),HKSE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VKNE (IDIM1:IDIM2,JDIM1:JDIM2),VKSE (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             D I M E N S I O N
     & TNE  (IDIM1:IDIM2,JDIM1:JDIM2),TSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QNE  (IDIM1:IDIM2,JDIM1:JDIM2),QSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2NE (IDIM1:IDIM2,JDIM1:JDIM2),Q2SE (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UNE  (IDIM1:IDIM2,JDIM1:JDIM2),USE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,VNE  (IDIM1:IDIM2,JDIM1:JDIM2),VSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TDIF (IDIM1:IDIM2,JDIM1:JDIM2),QDIF (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDIF (IDIM1:IDIM2,JDIM1:JDIM2),VDIF (IDIM1:IDIM2,JDIM1:JDIM2)
     &,Q2DIF(IDIM1:IDIM2,JDIM1:JDIM2)
     &,DEF  (IDIM1:IDIM2,JDIM1:JDIM2),CKE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APEL (IDIM1:IDIM2,JDIM1:JDIM2)
     &,IWL  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,QH   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TH   (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C***
C***  DIFFUSING Q2 AT GROUND LEVEL DOESNT MATTER, USTAR2 IS RECALCULATED
C***
C-----------------------------------------------------------------------
      HEAT=.FALSE.
C-----------------------------------------------------------------------
                             DO 600 KS=1,KSMUD
C-----------------------------------------------------------------------
C---------------------MAIN VERTICAL INTEGRATION LOOP--------------------
C-----------------------------------------------------------------------
!$omp parallel do 
!$omp& private(cke,def,defsk,deftk,hkne,hkse,hmskl,q2dif,q2l,q2ne,q2se,
!$omp&         qdif,qne,qse,tdif,tne,tse,udif,une,use,utk,vdif,vkne,
!$omp&         vkse,vne,vse,vtk)
C-----------------------------------------------------------------------
      CALL ZERO2(IWL)
C-----------------------------------------------------------------------
                             DO 500 L=1,LM
C-----------------------------------------------------------------------
      CALL ZERO2(DEF)
      CALL ZERO2(Q2NE)
      CALL ZERO2(Q2SE)
      CALL ZERO2(QNE)
      CALL ZERO2(QSE)
      CALL ZERO2(TNE)
      CALL ZERO2(TSE)
      CALL ZERO2(UNE)
      CALL ZERO2(USE)
      CALL ZERO2(VSE)
      CALL ZERO2(VNE)
      CALL ZERO2(VSE)
      CALL ZERO2(TDIF)
      CALL ZERO2(QDIF)
      CALL ZERO2(UDIF)
      CALL ZERO2(VDIF)
      CALL ZERO2(Q2DIF)
C-----------------------------------------------------------------------
      DO 210 J=MYJS_P1,MYJE_P1
      DO 210 I=MYIS_P1,MYIE_P1
      PDSL=PD(I,J)*RES(I,J)
      APESTS=PDSL*AETA(L)+PT
      IF(.NOT.HYDRO)THEN
        APESTS=0.5*(PINT(I,J,L)+PINT(I,J,L+1))
      ENDIF
      APEL(I,J)=(1.E5/APESTS)**CAPA
      IF(CWM(I,J,L).GT.CLIMIT)THEN
        IF(T(I,J,L).LT.258.15)THEN
          IWL(I,J)=1
        ELSEIF(T(I,J,L).GE.273.15)THEN
          IWL(I,J)=0
        ENDIF
      ELSE
        IWL(I,J)=0
      ENDIF
      FIW=FLOAT(IWL(I,J))
      ELOCP=((1.-FIW)*ELWV+FIW*ELIV)/CP
      TH(I,J)=(T(I,J,L)-ELOCP*CWM(I,J,L))*APEL(I,J)
      QH(I,J)=Q(I,J,L)+CWM(I,J,L)
  210 CONTINUE
C--------------DEFORMATIONS---------------------------------------------
      DO 220 J=MYJS1_P1,MYJE1_P1
      DO 220 I=MYIS_P1,MYIE1_P1
C
      DEFTK=U(I+IHE(J),J,L)-U(I+IHW(J),J,L)-V(I,J+1,L)+V(I,J-1,L)
      DEFSK=U(I,J+1,L)-U(I,J-1,L)+V(I+IHE(J),J,L)-V(I+IHW(J),J,L)
      DEF(I,J)=DEFTK*DEFTK+DEFSK*DEFSK
      DEF(I,J)=SQRT(DEF(I,J)+DEF(I,J))*HBM2(I,J)
 220  CONTINUE
C--------------T,Q, Q2 DIAGONAL CONTRIBUTIONS---------------------------
      DO 250 J=MYJS_P1,MYJE1_P1
      DO 250 I=MYIS_P1,MYIE1_P1
      HKNE(I,J)=(DEF(I,J)+DEF(I+IHE(J),J+1))
     1          *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
      TNE (I,J)=(TH(I+IHE(J),J+1  )-TH(I,J  ))*HKNE(I,J)
      QNE (I,J)=(QH(I+IHE(J),J+1  )-QH(I,J  ))*HKNE(I,J)
      Q2NE(I,J)=(Q2(I+IHE(J),J+1,L)-Q2(I,J,L))*HKNE(I,J)
  250 CONTINUE
C
      DO 260 J=MYJS1_P1,MYJE_P1
      DO 260 I=MYIS_P1,MYIE1_P1
      HKSE(I,J)=(DEF(I+IHE(J),J-1)+DEF(I,J))
     1          *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
      TSE (I,J)=(TH(I+IHE(J),J-1  )-TH(I,J  ))*HKSE(I,J)
      QSE (I,J)=(QH(I+IHE(J),J-1  )-QH(I,J  ))*HKSE(I,J)
      Q2SE(I,J)=(Q2(I+IHE(J),J-1,L)-Q2(I,J,L))*HKSE(I,J)
  260 CONTINUE
C-----------------------------------------------------------------------
      DO 270 J=MYJS1,MYJE1
      DO 270 I=MYIS1,MYIE
      TDIF (I,J)=(TNE (I,J)-TNE (I+IHW(J),J-1)
     1           +TSE (I,J)-TSE (I+IHW(J),J+1))*HDAC(I,J)/APEL(I,J)
      QDIF (I,J)=(QNE (I,J)-QNE (I+IHW(J),J-1)
     1           +QSE (I,J)-QSE (I+IHW(J),J+1))*HDAC(I,J)*FCDIF
      Q2DIF(I,J)=(Q2NE(I,J)-Q2NE(I+IHW(J),J-1)
     1           +Q2SE(I,J)-Q2SE(I+IHW(J),J+1))*HDAC(I,J)
  270 CONTINUE
C--------------2-ND ORDER DIFFUSION-------------------------------------
      DO 280 J=MYJS2,MYJE2
      DO 280 I=MYIS1,MYIE1
      T(I,J,L)=T(I,J,L)+TDIF(I,J)
      Q(I,J,L)=Q(I,J,L)+QDIF(I,J)
  280 CONTINUE
C-----------------------------------------------------------------------
      IF(L.NE.LM)THEN
        DO 290 J=MYJS2,MYJE2
        DO 290 I=MYIS1,MYIE1
        Q2(I,J,L)=Q2(I,J,L)+Q2DIF(I,J)*HTM(I,J,L+1)
  290   CONTINUE
      ENDIF
C--------------U,V, DIAGONAL CONTRIBUTIONS------------------------------
  360 DO 410 J=MYJS_P1,MYJE1_P1
      DO 410 I=MYIS_P1,MYIE1_P1
      VKNE(I,J)=(DEF(I+IVE(J),J)+DEF(I,J+1))
     1          *VTM(I,J,L)*VTM(I+IVE(J),J+1,L)
      UNE(I,J)=(U(I+IVE(J),J+1,L)-U(I,J,L))*VKNE(I,J)
      VNE(I,J)=(V(I+IVE(J),J+1,L)-V(I,J,L))*VKNE(I,J)
  410 CONTINUE
C
      DO 420 J=MYJS1_P1,MYJE_P1
      DO 420 I=MYIS_P1,MYIE1_P1
      VKSE(I,J)=(DEF(I,J-1)+DEF(I+IVE(J),J))
     1          *VTM(I+IVE(J),J-1,L)*VTM(I,J,L)
      USE(I,J)=(U(I+IVE(J),J-1,L)-U(I,J,L))*VKSE(I,J)
      VSE(I,J)=(V(I+IVE(J),J-1,L)-V(I,J,L))*VKSE(I,J)
  420 CONTINUE
C-----------------------------------------------------------------------
      DO 430 J=MYJS1,MYJE1
      DO 430 I=MYIS,MYIE1
      UDIF(I,J)=(UNE(I,J)-UNE(I+IVW(J),J-1)
     1          +USE(I,J)-USE(I+IVW(J),J+1))*HDACV(I,J)
      VDIF(I,J)=(VNE(I,J)-VNE(I+IVW(J),J-1)
     1          +VSE(I,J)-VSE(I+IVW(J),J+1))*HDACV(I,J)
  430 CONTINUE
C--------------2-ND ORDER DIFFUSION-------------------------------------
      DO 440 J=MYJS2,MYJE2
      DO 440 I=MYIS1,MYIE1
      UTK=U(I,J,L)
      VTK=V(I,J,L)
      U(I,J,L)=U(I,J,L)+UDIF(I,J)
      V(I,J,L)=V(I,J,L)+VDIF(I,J)
      CKE(I,J)=0.5*(U(I,J,L)*U(I,J,L)-UTK*UTK
     1             +V(I,J,L)*V(I,J,L)-VTK*VTK)
  440 CONTINUE
C-----------------------------------------------------------------------
      IF(HEAT)THEN
        DO 480 J=MYJS2,MYJE2
        DO 480 I=MYIS1,MYIE1
        T(I,J,L)=-RFCP*(CKE(I+IHE(J),J)+CKE(I,J+1)
     1                 +CKE(I+IHW(J),J)+CKE(I,J-1))*HBM2(I,J)
     2           +T(I,J,L)
  480   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
  500                        CONTINUE
  600                        CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
