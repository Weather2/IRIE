      SUBROUTINE SST14K (SST,SM,GLAT,GLON)
C
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE "parmeta"
C
      PARAMETER  (IMY=1041,JMY=441)
C
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
      PARAMETER  (RAD2D=57.29578E0,PI=3.141592654)
C
C
C
      DIMENSION    SST14   (IMY,JMY)
C
      DIMENSION  SST(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
      DIMENSION HLON(IM,JM)
C
C
      DATA   INSST/44/
C
C**************************  BEGIN EXECUTION ***********************
C
C
C  READ HI RESOLUTION 14 KM OPC SST ANALYSIS, CONVERT To KELVIN
C
      DO 25 I = 1, IMY
       READ (INSST,ERR=200,END=210) (SST14(I,J),J=1,JMY)
   25 CONTINUE
      CALL C2K(IMY,JMY,SST14)
C
C PUT LONGITUDE IN CORRECT ORDER
C
      DO J = 1,JM
      DO I = 1,IM
       HLON(I,J) = 2.0*PI - GLON(I,J)
      ENDDO
      ENDDO
C
C
C----  INTERPOLATE 1/8 DEG GLOBAL SATELLITE SST TO ETA GRID  -------
C
      DO J = 1,JM
      DO I = 1,IM
       IF((GLAT(I,J)*RAD2D).LT.65.0.AND.(GLAT(I,J)*RAD2D).GT.10.0.AND.
     &  (HLON(I,J)*RAD2D).LT.325.0.AND.(HLON(I,J)*RAD2D).GT.195.0)
     &  CALL ZTERPG(I,J,SST14,SST,GLAT,HLON)
      ENDDO
      ENDDO
C
C   REACHING HERE MEANS 14KM SST READ OK
C
      RETURN
C
C   REACHING HERE MEANS SOMETHING IS WRONG
C
  200 CONTINUE      !  SOME KIND OF ERROR READING FILE
      WRITE(6,555) INSST
  210 CONTINUE      !  HIT UNEXPECTED END O' FILE
      WRITE(6,556) INSST
  555 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN 14 KM SST        ',
     1             'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
  556 FORMAT ('0', 'HIT UNEXPECTED END OF FILE READING 14K SST',
     1             'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
      RETURN
      END