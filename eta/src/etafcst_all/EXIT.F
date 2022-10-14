      SUBROUTINE EXIT(NAME,IRET)
C--------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "cuparm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
C--------------------------------------------------------------
      PARAMETER (JAM=6+2*(JM-10),LB=2*IM+JM-3
     1,          LM1=LM-1,LP1=LM+1,IMT=2*IM-1)
C--------------------------------------------------------------
      CHARACTER NAME*(*)
C--------------------------------------------------------------
C
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "MAPOT1.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
      INCLUDE "Z0EFFT.comm"
      INCLUDE "TEMPV.comm"
C--------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A)
      IEND=MYIE
      IF(MOD(MYPE+1,INPES).EQ.0)IEND=MYIE-1
C
      DO J=MYJS,MYJE
      DO I=MYIS,IEND
        IF(PD(I,J).GT.110000.OR.PD(I,J).LT.40000.)THEN
          WRITE(6,100)NAME
          WRITE(6,120)I,J,PD(I,J),MYPE
  120     FORMAT(' I=',I3,' J=',I3,' PD=',E12.5
     1,          ' MYPE=',I3)
          IRET=666
          CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
C
      DO 500 L=1,LM
      DO 490 J=MYJS,MYJE
      DO 490 I=MYIS,IEND
      IF(HTM(I,J,L).GT.0.5)THEN
        IF(T(I,J,L).LT.150..OR.T(I,J,L).GT.325.)THEN
          WRITE(6,100)NAME
          WRITE(6,200)I,J,L,T(I,J,L),MYPE
  200     FORMAT(' I=',I3,' J=',I3,' L=',I2,' T=',E12.5
     1,          ' MYPE=',I3)
          IRET=666
          CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,J,L).LT.-1.E-4.OR.Q(I,J,L).GT.30.E-3)THEN
          WRITE(6,100)NAME
          WRITE(6,210)I,J,L,Q(I,J,L),MYPE
  210     FORMAT(' I=',I3,' J=',I3,' L=',I2,' Q=',E12.5
     1,          ' MYPE=',I3)
          IRET=666
          CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q2(I,J,L).GT.200.)THEN
          WRITE(6,100)NAME
          WRITE(6,220)I,J,L,Q2(I,J,L),MYPE
  220     FORMAT(' I=',I3,' J=',I3,' L=',I2,' Q2=',E12.5
     1,          ' MYPE=',I3)
          IRET=666
          CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(CWM(I,J,L).GT.1.E-2)THEN
          WRITE(6,100)NAME
          WRITE(6,230)I,J,L,CWM(I,J,L),MYPE
  230     FORMAT(' I=',I3,' J=',I3,' L=',I2,' CWM=',E12.5
     1,          ' MYPE=',I3)
          IRET=666
          CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDIF
  490 CONTINUE
  500 CONTINUE
      DO 700 L=1,LM
      DO 690 J=MYJS,MYJE
      DO 690 I=MYIS,IEND
      IF(ABS(U(I,J,L)).GT.125..OR.ABS(V(I,J,L)).GT.125.)THEN
        WRITE(6,100)NAME
        WRITE(6,610)I,J,L,U(I,J,L),V(I,J,L),MYPE
  610   FORMAT(' I=',I3,' J=',I3,' L=',I2,' U=',E12.5,' V=',E12.5
     1,          ' MYPE=',I3)
        IRET=666
        CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
      ENDIF
  690 CONTINUE
  700 CONTINUE
C
      RETURN
      END
