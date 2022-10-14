      SUBROUTINE E2TLL(HTLAT,HTLON,VTLAT,VTLON)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    E2TLL       COMPUTE ETA TRNSFM (LAT,LON)
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE TRANSFORMED LATITUDE AND 
C     LONGITUDE OF MASS (H) AND VELOCITY (V) POINTS FOR
C     GIVEN ETA E-GRID SPECIFICATIONS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  DAVID PLUMMER - ROUTINE ETATLL IN ETAPACKC
C   92-12-23  RUSS TREADON  - GENERALIZED FOR ETAPOST USE.
C   98-06-04  BLACK - CONVERSION TO 2-D
C     
C USAGE:    CALL E2TLL(HTLAT,HTLON,VTLAT,VTLON)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     HTLAT    - TRANSFORMED LATITUDE OF H POINTS
C     HTLON    - TRANSFORMED LONGITUDE OF H POINTS
C     VTLAT    - TRANSFORMED LATITUDE OF V POINTS
C     VTLON    - TRANSFORMED LONGITUDE OF V POINTS
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - EGRID
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     INCLUDE ETA GRID SPECIFICATIONS.  DERIVE OTHER PARAMETERS.
C------------------------------------------------------------------     
      INCLUDE "parmeta"
C------------------------------------------------------------------     
      PARAMETER (ICEN=2*IM/2,JCEN=(JM+1)/2)
C------------------------------------------------------------------     
C     
C     DECLARE VARIABLES.
C     
      LOGICAL NORTH
      REAL ALONVT, DLAM, DPHI, LAM0, PHI0, POLEI, POLEJ
      REAL SINPH0, TANPH0, WLONC, XMESHL, YLATC
      REAL HTLAT(IM,JM),HTLON(IM,JM)
      REAL VTLAT(IM,JM),VTLON(IM,JM)
C------------------------------------------------------------------     
      EQUIVALENCE (YLATC,ALONVT), (DPHI, POLEI)
      EQUIVALENCE (WLONC, POLEJ), (DLAM,XMESHL)
C------------------------------------------------------------------     
      INCLUDE "EGRID.comm"
C************************************************************************
C***
C***  TRANFORMED LONGITUDE OF WESTERN BOUNDARY AND
C***  TRANSFORMED LATITUDE OF SOUTHERN BOUNDARY
C***
      WBD=-1*((IM-1)*DLAM)
      SBD=-1*((JM-1)/2*DPHI)
C     
C     COMPUTE TRANSFORMED (LAT,LON) OF THE HEIGHTS POINTS
C     ON THE ETA GRID.
C
      DO J=1,JM
      ELON=WBD+MOD(J+1,2)*DLAM-2.*DLAM
      ELAT=SBD+(J-1)*DPHI
      DO I=1,IM
        ELON=ELON+2.*DLAM
        HTLON(I,J)=ELON
        HTLAT(I,J)=ELAT
      ENDDO
      ENDDO
C     
C     COMPUTE TRANSFORMED (LAT,LON) OF THE VELOCITY POINTS 
C     ON THE ETA GRID.
C
      DO J=1,JM
      ELON=WBD+MOD(J,2)*DLAM-2.*DLAM
      ELAT=SBD+(J-1)*DPHI
      DO I=1,IM
        ELON=ELON+2.*DLAM
        VTLON(I,J)=ELON
        VTLAT(I,J)=ELAT
      ENDDO
      ENDDO
C     
C     END OF ROUTINE.
C     
      RETURN
      END

