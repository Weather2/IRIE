C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOH 
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOH       UPDATE MASS POINTS ON BOUNDARY
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     TEMPERATURE, SPECIFIC HUMIDITY, AND SURFACE PRESSURE
C     ARE UPDATED ON THE DOMAIN BOUNDARY BY APPLYING THE
C     PRE-COMPUTED TENDENCIES AT EACH TIME STEP.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D in HORIZONTAL
C   96-12-13  BLACK      - FINAL MODIFICATION FOR NESTED RUNS
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C   00-01-06  BLACK      - MODIFIED FOR JANJIC NONHYDROSTATIC CODE
C   00-09-14  BLACK      - MODIFIED FOR DIRECT ACCESS READ
C     
C USAGE: CALL BOCOH FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C     NONE
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
C                  VRBLS
C                  PVRBLS
C                  CLDWTR
C                  BOCO
C                  MAPOT
C                  DYNAM
C                  NHYDRO
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,LP1=LM+1,JAM=6+2*(JM-10))
                             P A R A M E T E R
     & (ISIZ1=2*IM*JM,ISIZ2=2*IM*JM*LM,CRELAX=0./(18.*3600.))
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,NEST
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "TEMPV.comm"
C-----------------------------------------------------------------------
      INCLUDE "MAPOT.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
      C O M M O N /BOCO_IVAN/
     & PDB(IDIM1:IDIM2,JDIM1:JDIM2,2)
     &,TB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,QB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,UB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,VB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,Q2B(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,CWMB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUNB, LTIME
                             I N T E G E R
     & IDATB(3),IDATV(3),IAA(4),JAA(4)
                             C H A R A C T E R
     & CED*3, FFNAME*128, FN_NBC*128
                             R E A L
     & PDBG(IM,JM,2)
     &,TBG(IM,JM,LM,2)
     &,QBG(IM,JM,LM,2)
     &,UBG(IM,JM,LM,2)
     &,VBG(IM,JM,LM,2)
     &,Q2BG(IM,JM,LM,2)
     &,CWMBG(IM,JM,LM,2)
C***********************************************************************
C--------------READ FRESH BOUNDARY DATA IF NECESSARY--------------------
      IF((MOD(NTSD-1,NBOCO).EQ.0).AND.(NTSD.LT.NTSTM.OR.NTSD.EQ.1))THEN
C
        IF(MYPE.EQ.0)THEN
          iwait=0
          FN_NBC = "bndy."
          WRITE(CED,'(I3.3)')(NTSD-1)/NBOCO+1
          ISTR=INDEX(FN_NBC,' ')
          FFNAME(1:ISTR-1)=FN_NBC(1:ISTR-1)
          FFNAME(ISTR:ISTR+2)=CED
 389      CONTINUE
          write(6,*) "TRYING TO OPEN:",FFNAME(1:ISTR+2)
          write(0,*) "TRYING TO OPEN:",FFNAME(1:ISTR+2)
          IF(NTSD.EQ.1)THEN
            REWIND NBC
          ELSE
            OPEN(UNIT=NBC,FILE=FFNAME(1:ISTR+2)
     &         ,STATUS='OLD',FORM='UNFORMATTED',ERR=390)
          ENDIF
          READ(NBC,END=390,ERR=390) RUNB,IDATB,IHRSTB
          READ(NBC,END=390,ERR=390)
          READ(NBC,END=390,ERR=390)PDBG
          READ(NBC,END=390,ERR=390)TBG
          READ(NBC,END=390,ERR=390)QBG
          READ(NBC,END=390,ERR=390)UBG
          READ(NBC,END=390,ERR=390)VBG
          READ(NBC,END=390,ERR=390)Q2BG
          READ(NBC,END=390,ERR=390)CWMBG
          CLOSE(UNIT=NBC)
          IHR=NTSD/TSPH+0.5
          CALL VALID(IDATB,IHRSTB,IHR,IDATV,IHRV,IDAYWK)
          PRINT *, "WEATHER FOR:",IDATV(3),IDATV(2),IDATV(1),IHRV,IHR
          write(6,*) "READ FRESH BOUNDARY DATA NTSD=",NTSD
          write(0,*) "READ FRESH BOUNDARY DATA NTSD=",NTSD
          write(6,*) "READ FROM FILE:",FFNAME(1:ISTR+2)
          write(0,*) "READ FROM FILE:",FFNAME(1:ISTR+2)
C
          GOTO 391
 390      IF(NTSD.NE.1)CLOSE(UNIT=NBC)
          iwait=iwait+1
          IF(iwait.EQ.120)STOP
          write(6,*) "FILE DOESN'T EXIST WAIT 2 min"
          write(0,*) "FILE DOESN'T EXIST WAIT 2 min"
          CALL SLEEP(120)
          GOTO 389
 391    ENDIF
C
        DO K=1,2
          CALL DSTRB(PDBG(:,:,K),PDB(:,:,K),1,1,1)
          CALL DSTRB(TBG(:,:,:,K),TB(:,:,:,K),LM,LM,1)
          CALL DSTRB(QBG(:,:,:,K),QB(:,:,:,K),LM,LM,1)
          CALL DSTRB(UBG(:,:,:,K),UB(:,:,:,K),LM,LM,1)
          CALL DSTRB(VBG(:,:,:,K),VB(:,:,:,K),LM,LM,1)
          CALL DSTRB(Q2BG(:,:,:,K),Q2B(:,:,:,K),LM,LM,1)
          CALL DSTRB(CWMBG(:,:,:,K),CWMB(:,:,:,K),LM,LM,1)
        ENDDO
C
        DO K=1,2
          CALL EXCH(PDB(:,:,K),1,1,1)
          CALL EXCH(TB(:,:,:,K),LM,1,1)
          CALL EXCH(QB(:,:,:,K),LM,1,1)
          CALL EXCH(UB(:,:,:,K),LM,1,1)
          CALL EXCH(VB(:,:,:,K),LM,1,1)
          CALL EXCH(Q2B(:,:,:,K),LM,1,1)
          CALL EXCH(CWMB(:,:,:,K),LM,1,1)
        ENDDO
C
      ENDIF
C-----------------------------------------------------------------------
           DO J=MYJS_P1,MYJE_P1
           DO I=MYIS_P1,MYIE_P1
      AN=1.-HBM2(I,J)
      AY=AN+(1.-AN)*DT*CRELAX
      PDB(I,J,1)=PDB(I,J,1)+DT*PDB(I,J,2)
      P0(I,J)=P0(I,J)-PD(I,J)
      PD(I,J)=PDB(I,J,1)*AY+PD(I,J)*(1.-AY)
      P0(I,J)=P0(I,J)+PD(I,J)
           DO L=1,LM
      TB  (I,J,L,1)=TB  (I,J,L,1)+DT*TB  (I,J,L,2)
      QB  (I,J,L,1)=QB  (I,J,L,1)+DT*QB  (I,J,L,2)
      Q2B (I,J,L,1)=Q2B (I,J,L,1)+DT*Q2B (I,J,L,2)
      CWMB(I,J,L,1)=CWMB(I,J,L,1)+DT*CWMB(I,J,L,2)
      IF(CWMB(I,J,L,1).LT.0.)CWMB(I,J,L,1)=0.
      T0  (I,J,L)=T0  (I,J,L)     -T   (I,J,L)
      T   (I,J,L)=TB  (I,J,L,1)*AY+T   (I,J,L)*(1.-AY)
      T0  (I,J,L)=T0  (I,J,L)     +T   (I,J,L)
      Q0  (I,J,L)=Q0  (I,J,L)     -Q   (I,J,L)
      Q   (I,J,L)=QB  (I,J,L,1)*AN+Q   (I,J,L)*(1.-AN)
      Q0  (I,J,L)=Q0  (I,J,L)     +Q   (I,J,L)
      Q2  (I,J,L)=Q2B (I,J,L,1)*AN+Q2  (I,J,L)*(1.-AN)
      CWM (I,J,L)=CWMB(I,J,L,1)*AN+CWM (I,J,L)*(1.-AN)
      PINTB=PD(I,J)*RES(I,J)*ETA(L+1)+PT
      PINT(I,J,L+1)=PINTB*AN+PINT(I,J,L+1)*(1.-AN)
           ENDDO
           ENDDO
           ENDDO
C-----------------------------------------------------------------------
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
C--------------------------------------------------------------------
                             DO 135 L=1,LM
C--------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 131 I=MYIS,MYIE1
        SHTM=HTM(I,1,L)+HTM(I+1,1,L)+HTM(I,3,L)+HTM(I+1,3,L)
        IF(SHTM.GT.0.)THEN
        RHTM=1./SHTM
        T(I,2,L)=(T(I,1,L)*HTM(I,1,L)+T(I+1,1,L)*HTM(I+1,1,L)
     1           +T(I,3,L)*HTM(I,3,L)+T(I+1,3,L)*HTM(I+1,3,L))*RHTM
        Q(I,2,L)=(Q(I,1,L)*HTM(I,1,L)+Q(I+1,1,L)*HTM(I+1,1,L)
     1           +Q(I,3,L)*HTM(I,3,L)+Q(I+1,3,L)*HTM(I+1,3,L))*RHTM
        Q2(I,2,L)=(Q2(I,1,L)*HTM(I,1,L)+Q2(I+1,1,L)*HTM(I+1,1,L)
     1            +Q2(I,3,L)*HTM(I,3,L)+Q2(I+1,3,L)*HTM(I+1,3,L))*RHTM
        CWM(I,2,L)=(CWM(I,1,L)*HTM(I,1,L)+CWM(I+1,1,L)*HTM(I+1,1,L)
     1             +CWM(I,3,L)*HTM(I,3,L)+CWM(I+1,3,L)*HTM(I+1,3,L))
     2             *RHTM
        ENDIF
  131   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 132 I=MYIS,MYIE1
        SHTM=HTM(I,JJM-2,L)+HTM(I+1,JJM-2,L)
     1          +HTM(I,JJM,L)+HTM(I+1,JJM,L)
        IF(SHTM.GT.0.)THEN
        RHTM=1./SHTM
        T(I,JJM-1,L)=(T(I,JJM-2,L)*HTM(I,JJM-2,L)
     1              +T(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2              +T(I,JJM,L)*HTM(I,JJM,L)
     3              +T(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        Q(I,JJM-1,L)=(Q(I,JJM-2,L)*HTM(I,JJM-2,L)
     1              +Q(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2              +Q(I,JJM,L)*HTM(I,JJM,L)
     3              +Q(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        Q2(I,JJM-1,L)=(Q2(I,JJM-2,L)*HTM(I,JJM-2,L)
     1               +Q2(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2               +Q2(I,JJM,L)*HTM(I,JJM,L)
     3               +Q2(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        CWM(I,JJM-1,L)=(CWM(I,JJM-2,L)*HTM(I,JJM-2,L)
     1                +CWM(I+1,JJM-2,L)*HTM(I+1,JJM-2,L)
     2                +CWM(I,JJM,L)*HTM(I,JJM,L)
     3                +CWM(I+1,JJM,L)*HTM(I+1,JJM,L))*RHTM
        ENDIF
  132   CONTINUE
      ENDIF
C
      IF(ILCOL.EQ.1)THEN
        DO 133 J=4,JM-3,2
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          SHTM=HTM(1,JJ-1,L)+HTM(2,JJ-1,L)
     1            +HTM(1,JJ+1,L)+HTM(2,JJ+1,L)
          IF(SHTM.GT.0.)THEN
          RHTM=1./SHTM
          T(1,JJ,L)=(T(1,JJ-1,L)*HTM(1,JJ-1,L)
     1              +T(2,JJ-1,L)*HTM(2,JJ-1,L)
     2              +T(1,JJ+1,L)*HTM(1,JJ+1,L)
     3              +T(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          Q(1,JJ,L)=(Q(1,JJ-1,L)*HTM(1,JJ-1,L)
     1              +Q(2,JJ-1,L)*HTM(2,JJ-1,L)
     2              +Q(1,JJ+1,L)*HTM(1,JJ+1,L)
     3              +Q(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          Q2(1,JJ,L)=(Q2(1,JJ-1,L)*HTM(1,JJ-1,L)
     1               +Q2(2,JJ-1,L)*HTM(2,JJ-1,L)
     2               +Q2(1,JJ+1,L)*HTM(1,JJ+1,L)
     3               +Q2(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          CWM(1,JJ,L)=(CWM(1,JJ-1,L)*HTM(1,JJ-1,L)
     1                +CWM(2,JJ-1,L)*HTM(2,JJ-1,L)
     2                +CWM(1,JJ+1,L)*HTM(1,JJ+1,L)
     3                +CWM(2,JJ+1,L)*HTM(2,JJ+1,L))*RHTM
          ENDIF
        ENDIF
  133   CONTINUE
      ENDIF
C
      IF(IRCOL.EQ.1)THEN
        DO 134 J=4,JM-3,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          SHTM=HTM(IIM-1,JJ-1,L)+HTM(IIM,JJ-1,L)
     1            +HTM(IIM-1,JJ+1,L)+HTM(IIM,JJ+1,L)
          IF(SHTM.GT.0.)THEN
          RHTM=1./SHTM
          T(IIM-1,JJ,L)=(T(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                +T(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                +T(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                +T(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          Q(IIM-1,JJ,L)=(Q(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                +Q(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                +Q(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                +Q(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          Q2(IIM-1,JJ,L)=(Q2(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                 +Q2(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                 +Q2(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                 +Q2(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          CWM(IIM-1,JJ,L)=(CWM(IIM-1,JJ-1,L)*HTM(IIM-1,JJ-1,L)
     1                  +CWM(IIM,JJ-1,L)*HTM(IIM,JJ-1,L)
     2                  +CWM(IIM-1,JJ+1,L)*HTM(IIM-1,JJ+1,L)
     2                  +CWM(IIM,JJ+1,L)*HTM(IIM,JJ+1,L))*RHTM
          ENDIF
        ENDIF
  134   CONTINUE
      ENDIF
  135                        CONTINUE
C--------------------------------------------------------------------
C
C------- SPACE INTERPOLATION OF PD AND T AT THE INNER BOUNDARY ------
C
C--------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 121 I=MYIS,MYIE1
C
        IA0=I
        JA0=2
C
        IAA(1)=I
        IAA(2)=I+1
        IAA(3)=I
        IAA(4)=I+1
C
        JAA(1)=1
        JAA(2)=1
        JAA(3)=3
        JAA(4)=3
C
        PDY=0.
C
        DO K=1,4
          IA1=IAA(K)
          JA1=JAA(K)
C
          IF(FIS(IA1,JA1).LE.FIS(IA0,JA0))THEN
            PBIN=PD(IA1,JA1)+PT
            PHBI=FIS(IA1,JA1)
            DPOSP=0.
            L=LMH(IA1,JA1)
C
            DO WHILE(PHBI.LT.FIS(IA0,JA0))
              PTIN=PBIN-DETA(L)*PD(IA1,JA1)*RES(IA1,JA1)
              TRTV=2.*R*T(IA1,JA1,L)*(1.+0.608*Q(IA1,JA1,L))
              PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
              PBIN=PTIN
              PHBI=PHTI
              DPOSP=(PHBI-FIS(IA0,JA0))/TRTV
              L=L-1
            ENDDO
C
            PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
            PDX=PBIN-PT
          ELSE
            PBIN=PD(IA0,JA0)+PT
            PHBI=FIS(IA0,JA0)
            DPOSP=0.
            L=LMH(IA0,JA0)
C
            DO WHILE(PHBI.LT.FIS(IA1,JA1))
              PTIN=PBIN-DETA(L)*PD(IA0,JA0)*RES(IA0,JA0)
              TRTV=2.*R*T(IA0,JA0,L)*(1.+0.608*Q(IA0,JA0,L))
              PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
              PBIN=PTIN
              PHBI=PHTI
              DPOSP=(PHBI-FIS(IA1,JA1))/TRTV
              L=L-1
            ENDDO
C
            PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
            SCALE=(PD(IA0,JA0)+PT)/PBIN
C
            PDX=(PD(IA1,JA1)+PT)*SCALE-PT
          ENDIF
C
          PDY=PDY+0.25*PDX
C
        ENDDO
C
        PD(IA0,JA0)=PDY
C
        DO L=1,LM
          PINT(IA0,JA0,L+1)=PD(IA0,JA0)*RES(IA0,JA0)*ETA(L+1)+PT
        ENDDO
  121   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 122 I=MYIS,MYIE1
C
        IA0=I
        JA0=JJM-1
C
        IAA(1)=I
        IAA(2)=I+1
        IAA(3)=I
        IAA(4)=I+1
C
        JAA(1)=JJM-2
        JAA(2)=JJM-2
        JAA(3)=JJM
        JAA(4)=JJM
C
        PDY=0.
C
        DO K=1,4
          IA1=IAA(K)
          JA1=JAA(K)
C
          IF(FIS(IA1,JA1).LE.FIS(IA0,JA0))THEN
            PBIN=PD(IA1,JA1)+PT
            PHBI=FIS(IA1,JA1)
            DPOSP=0.
            L=LMH(IA1,JA1)
C
            DO WHILE(PHBI.LT.FIS(IA0,JA0))
              PTIN=PBIN-DETA(L)*PD(IA1,JA1)*RES(IA1,JA1)
              TRTV=2.*R*T(IA1,JA1,L)*(1.+0.608*Q(IA1,JA1,L))
              PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
              PBIN=PTIN
              PHBI=PHTI
              DPOSP=(PHBI-FIS(IA0,JA0))/TRTV
              L=L-1
            ENDDO
C
            PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
            PDX=PBIN-PT
          ELSE
            PBIN=PD(IA0,JA0)+PT
            PHBI=FIS(IA0,JA0)
            DPOSP=0.
            L=LMH(IA0,JA0)
C
            DO WHILE(PHBI.LT.FIS(IA1,JA1))
              PTIN=PBIN-DETA(L)*PD(IA0,JA0)*RES(IA0,JA0)
              TRTV=2.*R*T(IA0,JA0,L)*(1.+0.608*Q(IA0,JA0,L))
              PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
              PBIN=PTIN
              PHBI=PHTI
              DPOSP=(PHBI-FIS(IA1,JA1))/TRTV
              L=L-1
            ENDDO
C
            PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
            SCALE=(PD(IA0,JA0)+PT)/PBIN
C
            PDX=(PD(IA1,JA1)+PT)*SCALE-PT
          ENDIF
C
          PDY=PDY+0.25*PDX
C
        ENDDO
C
        PD(IA0,JA0)=PDY
C
        DO L=1,LM
          PINT(IA0,JA0,L+1)=PD(IA0,JA0)*RES(IA0,JA0)*ETA(L+1)+PT
        ENDDO
  122   CONTINUE
      ENDIF
C
      IF(ILCOL.EQ.1)THEN
        DO 123 J=4,JM-3,2
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
C
          IA0=1
          JA0=JJ
C
          IAA(1)=1
          IAA(2)=2
          IAA(3)=1
          IAA(4)=2
C
          JAA(1)=JJ-1
          JAA(2)=JJ-1
          JAA(3)=JJ+1
          JAA(4)=JJ+1
C
          PDY=0.
C
          DO K=1,4
            IA1=IAA(K)
            JA1=JAA(K)
C
            IF(FIS(IA1,JA1).LE.FIS(IA0,JA0))THEN
              PBIN=PD(IA1,JA1)+PT
              PHBI=FIS(IA1,JA1)
              DPOSP=0.
              L=LMH(IA1,JA1)
C
              DO WHILE(PHBI.LT.FIS(IA0,JA0))
                PTIN=PBIN-DETA(L)*PD(IA1,JA1)*RES(IA1,JA1)
                TRTV=2.*R*T(IA1,JA1,L)*(1.+0.608*Q(IA1,JA1,L))
                PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
                PBIN=PTIN
                PHBI=PHTI
                DPOSP=(PHBI-FIS(IA0,JA0))/TRTV
                L=L-1
              ENDDO
C
              PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
              PDX=PBIN-PT
            ELSE
              PBIN=PD(IA0,JA0)+PT
              PHBI=FIS(IA0,JA0)
              DPOSP=0.
              L=LMH(IA0,JA0)
C
              DO WHILE(PHBI.LT.FIS(IA1,JA1))
                PTIN=PBIN-DETA(L)*PD(IA0,JA0)*RES(IA0,JA0)
                TRTV=2.*R*T(IA0,JA0,L)*(1.+0.608*Q(IA0,JA0,L))
                PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
                PBIN=PTIN
                PHBI=PHTI
                DPOSP=(PHBI-FIS(IA1,JA1))/TRTV
                L=L-1
              ENDDO
C
              PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
              SCALE=(PD(IA0,JA0)+PT)/PBIN
C
              PDX=(PD(IA1,JA1)+PT)*SCALE-PT
            ENDIF
C
            PDY=PDY+0.25*PDX
C
          ENDDO
C
          PD(IA0,JA0)=PDY
C
          DO L=1,LM
            PINT(IA0,JA0,L+1)=PD(IA0,JA0)*RES(IA0,JA0)*ETA(L+1)+PT
          ENDDO
        ENDIF
  123   CONTINUE
      ENDIF
C
      IF(IRCOL.EQ.1)THEN
        DO 124 J=4,JM-3,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
C
          IA0=IIM-1
          JA0=JJ
C
          IAA(1)=IIM-1
          IAA(2)=IIM
          IAA(3)=IIM-1
          IAA(4)=IIM
C
          JAA(1)=JJ-1
          JAA(2)=JJ-1
          JAA(3)=JJ+1
          JAA(4)=JJ+1
C
          PDY=0.
C
          DO K=1,4
            IA1=IAA(K)
            JA1=JAA(K)
C
            IF(FIS(IA1,JA1).LE.FIS(IA0,JA0))THEN
              PBIN=PD(IA1,JA1)+PT
              PHBI=FIS(IA1,JA1)
              DPOSP=0.
              L=LMH(IA1,JA1)
C
              DO WHILE(PHBI.LT.FIS(IA0,JA0))
                PTIN=PBIN-DETA(L)*PD(IA1,JA1)*RES(IA1,JA1)
                TRTV=2.*R*T(IA1,JA1,L)*(1.+0.608*Q(IA1,JA1,L))
                PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
                PBIN=PTIN
                PHBI=PHTI
                DPOSP=(PHBI-FIS(IA0,JA0))/TRTV
                L=L-1
              ENDDO
C
              PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
              PDX=PBIN-PT
            ELSE
              PBIN=PD(IA0,JA0)+PT
              PHBI=FIS(IA0,JA0)
              DPOSP=0.
              L=LMH(IA0,JA0)
C
              DO WHILE(PHBI.LT.FIS(IA1,JA1))
                PTIN=PBIN-DETA(L)*PD(IA0,JA0)*RES(IA0,JA0)
                TRTV=2.*R*T(IA0,JA0,L)*(1.+0.608*Q(IA0,JA0,L))
                PHTI=PHBI+TRTV*(PBIN-PTIN)/(PBIN+PTIN)
                PBIN=PTIN
                PHBI=PHTI
                DPOSP=(PHBI-FIS(IA1,JA1))/TRTV
                L=L-1
              ENDDO
C
              PBIN=PBIN*(1.+DPOSP)/(1.-DPOSP)
C
              SCALE=(PD(IA0,JA0)+PT)/PBIN
C
              PDX=(PD(IA1,JA1)+PT)*SCALE-PT
            ENDIF
C
            PDY=PDY+0.25*PDX
C
          ENDDO
C
          PD(IA0,JA0)=PDY
C
          DO L=1,LM
            PINT(IA0,JA0,L+1)=PD(IA0,JA0)*RES(IA0,JA0)*ETA(L+1)+PT
          ENDDO
        ENDIF
  124   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                             RETURN
                             END
      SUBROUTINE VALID(IDAT,IHRST,IHR,IDATV,IHRV,IDAYWK)
      INTEGER IDAT(3),IHRST,IHR,IDATV(3),IHRV

      IJULIAN=IW3JDN(IDAT(3),IDAT(1),IDAT(2))
      IADD=IHRST+IHR
      IADDAY=INT((IHRST+IHR)/24)
      IHRV=IADD-24*IADDAY
      IJULIAN=IJULIAN+IADDAY
      CALL W3FS26(IJULIAN,IDATV(3),IDATV(2),IDATV(1),IDAYWK,IDAYYR)
      RETURN
      END
