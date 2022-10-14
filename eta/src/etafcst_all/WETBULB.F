      SUBROUTINE WETBULB(T,Q,PDSL,HTM,KARR,TWET)
C
C     FILE: WETBULB.f
C     WRITTEN: 10 SEPTEMBER 1993, MICHAEL BALDWIN
C     REVISIONS: 
C     CONVERSION TO 2-D: 12 JUNE 1998 (T BLACK)
C     MPI VERSION: 04 Jan 2000 ( JIM TUCCILLO )
C
C-----------------------------------------------------------------------
C     ROUTINE TO COMPUTE WET BULB TEMPERATURES USING THE LOOK UP TABLE
C     APPROACH THAT IS USED IN CUCNVC
C  
C     FOR A GIVEN POINT K AND LAYER L:
C      THETA E IS COMPUTED FROM THETA AND Q BY LIFTING THE PARCEL TO
C      ITS SATURATION POINT.
C      THEN THE WET BULB TEMPERATURE IS FOUND BY FOLLOWING THE THETA E
C      DOWN TO THE ORIGINAL PRESSURE LEVEL (USING SUBROUTINE TTBLEX).     
C
C   
C
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
      PARAMETER(IMJM=IM*JM-JM/2,LP1=LM+1,IMJM_LOC=IDIM2*JDIM2)
      INCLUDE "cuparm"
      INCLUDE "PHYS.comm"
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
C  LIST OF VARIABLES NEEDED
C    PARAMETERS:
C      INCLUDED IN "cuparm" AND "parm.tbl"
C    INPUT:
C      T,Q,HTM (3-D),PDSL,KARR (2-D),KNUM
C    OUTPUT: 
C      TWET (3-D)
C    INTERNAL:
C      THESP,KLRES,KHRES,QQ,PP,IPTB,ITHTB
C      PLUS SOME INCLUDED IN COMMON BLOCK "PHYS"
C    SUBROUTINES CALLED:
C      TTBLEX
C
       DIMENSION 
     1 THESP(idim1:idim2,jdim1:jdim2)
     2,TWET(idim1:idim2,jdim1:jdim2,LM)
     3,T(idim1:idim2,jdim1:jdim2,LM)
     4,Q(idim1:idim2,jdim1:jdim2,LM)
     5,PDSL(idim1:idim2,jdim1:jdim2)
     6,HTM(idim1:idim2,jdim1:jdim2,LM)
     7,KARR(idim1:idim2,jdim1:jdim2)
                             D I M E N S I O N
     1 ILRES (IMJM_LOC),JLRES (IMJM_LOC)
     2,IHRES (IMJM_LOC),JHRES (IMJM_LOC)
     3,QQ(idim1:idim2,jdim1:jdim2),PP(idim1:idim2,jdim1:jdim2)
     4,IPTB(idim1:idim2,jdim1:jdim2),ITHTB(idim1:idim2,jdim1:jdim2)
C
C--------------COMPUTE WET BULB TEMPERATURES----------------------------
!$omp  parallel do
!$omp& private(apebtk,apespk,bqk,bqs00k,bqs10k,iq,iqtbk,it,ittb1,ittbk,
!$omp&         karr,khres,klres,knumh,knuml,p00k,p01k,p10k,p11k,ppk,
!$omp&         presk,qbtk,qqk,sqk,sqs00k,sqs10k,tbtk,thesp,tpspk,
!$omp&         tqk,tthbtk,tthk)
C-----------------------------------------------------------------------
                             DO 300 L=1,LM
      DO 125 J=MYJS,MYJE
      DO 125 I=MYIS,MYIE
      IF(KARR(I,J).GT.0)THEN
        IF (HTM(I,J,L).LT.1.0) THEN
          THESP(I,J)=273.15
          GOTO 125
        ENDIF
        TBTK  =T(I,J,L)
        QBTK  =Q(I,J,L)
        PRESK=PDSL(I,J)*AETA(L)+PT
        APEBTK=(1.E5/PRESK)**CAPA
        IF(QBTK.LT.EPSQ) QBTK=HTM(I,J,L)*EPSQ
C--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
        TTHBTK  =TBTK*APEBTK
        TTHK    =(TTHBTK-THL)*RDTH
        QQK     =TTHK-AINT(TTHK)
        ITTB1   =INT(TTHK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(ITTB1.LT.1) THEN
          ITTB1  =1
          QQK    =0.
        ENDIF
C
        IF(ITTB1.GE.JTB) THEN
        ITTB1  =JTB-1
        QQK    =0.
        ENDIF
C--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
        ITTBK=ITTB1
        BQS00K=QS0(ITTBK)
        SQS00K=SQS(ITTBK)
        BQS10K=QS0(ITTBK+1)
        SQS10K=SQS(ITTBK+1)
C--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
        BQK=(BQS10K-BQS00K)*QQK+BQS00K
        SQK=(SQS10K-SQS00K)*QQK+SQS00K
        TQK=(QBTK-BQK)/SQK*RDQ
        PPK=TQK-AINT(TQK)
        IQTBK=INT(TQK)+1
C--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(IQTBK.LT.1) THEN
          IQTBK =1
          PPK   =0.
        ENDIF
C
        IF(IQTBK.GE.ITB) THEN
          IQTBK=ITB-1
          PPK  =0.
        ENDIF
C--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
        IQ=IQTBK
        IT=ITTB1
        P00K=PTBL(IQ  ,IT  )
        P10K=PTBL(IQ+1,IT  )
        P01K=PTBL(IQ  ,IT+1)
        P11K=PTBL(IQ+1,IT+1)
C--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
        TPSPK=P00K+(P10K-P00K)*PPK+(P01K-P00K)*QQK
     1          +(P00K-P10K-P01K+P11K)*PPK*QQK
        APESPK=(1.E5/TPSPK)**CAPA
        THESP(I,J)=TTHBTK*EXP(ELOCP*QBTK*APESPK/TTHBTK)
      ENDIF
  125 CONTINUE
C--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      KNUML=0
      KNUMH=0
C
      DO 270 J=MYJS2,MYJE2
      DO 270 I=MYIS1,MYIE1
      PRESK=PDSL(I,J)*AETA(L)+PT
      IF(PRESK.LT.PLQ)THEN
        KNUML=KNUML+1
        ILRES(KNUML)=I
        JLRES(KNUML)=J
      ELSE
        KNUMH=KNUMH+1
        IHRES(KNUMH)=I
        JHRES(KNUMH)=J
      ENDIF
270   CONTINUE
C***
C*** COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
C**
      IF(KNUML.GT.0)THEN
        CALL TTBLEX(TWET(IDIM1,JDIM1,L),TTBL,ITB,JTB,KNUML
     1, ILRES,JLRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2, PT,PL,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3, RDP,THE0,STHE,RDTHE
     4, THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5, ITHTB(IDIM1,JDIM1))
      ENDIF
C***
C*** COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
C**
      IF(KNUMH.GT.0)THEN
        CALL TTBLEX(TWET(IDIM1,JDIM1,L),TTBLQ,ITBQ,JTBQ,KNUMH
     1, IHRES,JHRES,PDSL,AETA(L),HTM(IDIM1,JDIM1,L)
     2, PT,PLQ,QQ(IDIM1,JDIM1),PP(IDIM1,JDIM1)
     3, RDPQ,THE0Q,STHEQ,RDTHEQ
     4, THESP(IDIM1,JDIM1),IPTB(IDIM1,JDIM1)
     5, ITHTB(IDIM1,JDIM1))
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
  300 CONTINUE
      RETURN
      END
