C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE BOCOV
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BOCOV       UPDATE WIND POINTS ON BOUNDARY
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     U AND V COMPONENTS OF THE WIND ARE UPDATED ON THE
C     DOMAIN BOUNDARY BY APPLYING THE PRE-COMPUTED
C     TENDENCIES AT EACH TIME STEP.  AN EXTRAPOLATION FROM
C     INSIDE THE DOMAIN IS USED FOR THE COMPONENT TANGENTIAL
C     TO THE BOUNDARY IF THE NORMAL COMPONENT IS OUTWARD.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
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
C                  BOCO
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R
     & (D06666=.06666666,CRELAX=0./(18.*3600.))
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,LTIME
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C----------------------------------------------------------------------
      C O M M O N /BOCO_IVAN/
     & PDB(IDIM1:IDIM2,JDIM1:JDIM2,2)
     &,TB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,QB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,UB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,VB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,Q2B(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
     &,CWMB(IDIM1:IDIM2,JDIM1:JDIM2,LM,2)
C***********************************************************************
           DO J=MYJS_P1,MYJE_P1
           DO I=MYIS_P1,MYIE_P1
      VN=1.-VBM2(I,J)
      VY=VN+(1.-VN)*DT*CRELAX
           DO L=1,LM
      UB(I,J,L,1)=UB(I,J,L,1)+DT*UB(I,J,L,2)
      VB(I,J,L,1)=VB(I,J,L,1)+DT*VB(I,J,L,2)
      U (I,J,L)=UB(I,J,L,1)*VY+U(I,J,L)*(1.-VY)
      V (I,J,L)=VB(I,J,L,1)*VY+V(I,J,L)*(1.-VY)
           ENDDO
           ENDDO
           ENDDO
C------------- TIME INTERPOLATION OF U AND V AT THE OUTER BOUNDARY -----
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
C-----------------------------------------------------------------------
C--------------EXTRAPOLATION OF TANGENTIAL VELOCITY AT OUTFLOW POINTS---
C-----------------------------------------------------------------------
                             DO 125 L=1,LM
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 121 I=MYIS1_P1,MYIE2_P1
        IF(V(I,1,L).LT.0.)U(I,1,L)=(VTM(I,5,L)+1.)*U(I,3,L)
     1                             -VTM(I,5,L)    *U(I,5,L)
  121   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 122 I=MYIS1_P1,MYIE2_P1
        IF(V(I,JJM,L).GT.0.)
     1      U(I,JJM,L)=(VTM(I,JJM-4,L)+1.)*U(I,JJM-2,L)
     2                 -VTM(I,JJM-4,L)    *U(I,JJM-4,L)
  122   CONTINUE
      ENDIF
C
      DO 123 J=4,JM-3,2
      IF(ILCOL.EQ.1)THEN
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          IF(U(1,JJ,L).LT.0.)
     1        V(1,JJ,L)=(VTM(3,JJ,L)+1.)*V(2,JJ,L)
     2                  -VTM(3,JJ,L)    *V(3,JJ,L)
        ENDIF
      ENDIF
  123 CONTINUE
C
      DO 124 J=4,JM-3,2
      IF(IRCOL.EQ.1)THEN
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          IF(U(IIM,JJ,L).GT.0.)
     1        V(IIM,JJ,L)=(VTM(IIM-2,JJ,L)+1.)*V(IIM-1,JJ,L)
     1                    -VTM(IIM-2,JJ,L)    *V(IIM-2,JJ,L)
        ENDIF
      ENDIF
  124 CONTINUE
  125                        CONTINUE
C-----------------------------------------------------------------------
C------------- SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY ----
C-----------------------------------------------------------------------
                             DO 140 L=1,LM
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,2,L)=D06666*(4.*(U(1,1,L)+U(2,1,L)+U(2,3,L))
     1                     + U(1,2,L)+U(1,4,L)+U(2,4,L))
        V(2,2,L)=D06666*(4.*(V(1,1,L)+V(2,1,L)+V(2,3,L))
     1                      +V(1,2,L)+V(1,4,L)+V(2,4,L))
        U(2,2,L)=U(2,2,L)*VTM(2,2,L)
        V(2,2,L)=V(2,2,L)*VTM(2,2,L)
      ENDIF
C
      IF(IBROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,2,L)=D06666*(4.*(U(IIM-2,1,L)+U(IIM-1,1,L)
     1                          +U(IIM-2,3,L))
     2                          +U(IIM,2,L)+U(IIM,4,L)+U(IIM-1,4,L))
        V(IIM-1,2,L)=D06666*(4.*(V(IIM-2,1,L)+V(IIM-1,1,L)
     1                          +V(IIM-2,3,L))
     2                          +V(IIM,2,L)+V(IIM,4,L)+V(IIM-1,4,L))
        U(IIM-1,2,L)=U(IIM-1,2,L)*VTM(IIM-1,2,L)
        V(IIM-1,2,L)=V(IIM-1,2,L)*VTM(IIM-1,2,L)
      ENDIF
C
      IF(ITROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,JJM-1,L)=D06666*(4.*(U(1,JJM,L)+U(2,JJM,L)+U(2,JJM-2,L))
     1                          +U(1,JJM-1,L)+U(1,JJM-3,L)
     2                          +U(2,JJM-3,L))
        V(2,JJM-1,L)=D06666*(4.*(V(1,JJM,L)+V(2,JJM,L)+V(2,JJM-2,L))
     1                          +V(1,JJM-1,L)+V(1,JJM-3,L)
     2                          +V(2,JJM-3,L))
        U(2,JJM-1,L)=U(2,JJM-1,L)*VTM(2,JJM-1,L)
        V(2,JJM-1,L)=V(2,JJM-1,L)*VTM(2,JJM-1,L)
      ENDIF
C
      IF(ITROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,JJM-1,L)=
     1    D06666*(4.*(U(IIM-2,JJM,L)+U(IIM-1,JJM,L)+U(IIM-2,JJM-2,L))
     2               +U(IIM,JJM-1,L)+U(IIM,JJM-3,L)+U(IIM-1,JJM-3,L))
        V(IIM-1,JJM-1,L)=
     1    D06666*(4.*(V(IIM-2,JJM,L)+V(IIM-1,JJM,L)+V(IIM-2,JJM-2,L))
     2               +V(IIM,JJM-1,L)+V(IIM,JJM-3,L)+V(IIM-1,JJM-3,L))
        U(IIM-1,JJM-1,L)=U(IIM-1,JJM-1,L)*VTM(IIM-1,JJM-1,L)
        V(IIM-1,JJM-1,L)=V(IIM-1,JJM-1,L)*VTM(IIM-1,JJM-1,L)
      ENDIF
C-----------------------------------------------------------------------
C--------------SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY-----
C-----------------------------------------------------------------------
      IF(IBROW.EQ.1)THEN
        DO 131 I=MYIS2,MYIE2
        U(I,2,L)=(U(I-1,1,L)+U(I,1,L)+U(I-1,3,L)+U(I,3,L))*0.25
        V(I,2,L)=(V(I-1,1,L)+V(I,1,L)+V(I-1,3,L)+V(I,3,L))*0.25
        U(I,2,L)=U(I,2,L)*VTM(I,2,L)
        V(I,2,L)=V(I,2,L)*VTM(I,2,L)
  131   CONTINUE
      ENDIF
C
      IF(ITROW.EQ.1)THEN
        DO 132 I=MYIS2,MYIE2
        U(I,JJM-1,L)=(U(I-1,JJM-2,L)+U(I,JJM-2,L)
     1               +U(I-1,JJM,L)+U(I,JJM,L))*0.25
        V(I,JJM-1,L)=(V(I-1,JJM-2,L)+V(I,JJM-2,L)
     1               +V(I-1,JJM,L)+V(I,JJM,L))*0.25
        U(I,JJM-1,L)=U(I,JJM-1,L)*VTM(I,JJM-1,L)
        V(I,JJM-1,L)=V(I,JJM-1,L)*VTM(I,JJM-1,L)
  132   CONTINUE
      ENDIF
C
      DO 133 J=3,JM-2,2
      IF(ILCOL.EQ.1)THEN
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                    AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(1,JJ,L)=(U(1,JJ-1,L)+U(2,JJ-1,L)
     1              +U(1,JJ+1,L)+U(2,JJ+1,L))*0.25
          V(1,JJ,L)=(V(1,JJ-1,L)+V(2,JJ-1,L)
     1              +V(1,JJ+1,L)+V(2,JJ+1,L))*0.25
          U(1,JJ,L)=U(1,JJ,L)*VTM(1,JJ,L)
          V(1,JJ,L)=V(1,JJ,L)*VTM(1,JJ,L)
        ENDIF
      ENDIF
  133 CONTINUE
C
      IF(IRCOL.EQ.1)THEN
        DO 134 J=3,JM-2,2
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1.
     1                     AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(IIM-1,JJ,L)=0.25*(U(IIM-1,JJ-1,L)+U(IIM,JJ-1,L)
     1                     +U(IIM-1,JJ+1,L)+U(IIM,JJ+1,L))
          V(IIM-1,JJ,L)=0.25*(V(IIM-1,JJ-1,L)+V(IIM,JJ-1,L)
     1                     +V(IIM-1,JJ+1,L)+V(IIM,JJ+1,L))
          U(IIM-1,JJ,L)=U(IIM-1,JJ,L)*VTM(IIM-1,JJ,L)
          V(IIM-1,JJ,L)=V(IIM-1,JJ,L)*VTM(IIM-1,JJ,L)
        ENDIF
  134   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
  140                        CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
