      SUBROUTINE TRNSEG
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    TRNSEG      
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-20       
C     
C ABSTRACT:
C     THIS ROUTINE LOADS COMMON BLOCK EGRID WITH ETA
C     GRID SPECIFICATIONS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  DAVID PLUMMER - SUBROUTINE TRNSEG IN ETAPACKC
C   92-12-20  RUSS TREADON - MODIFIED ETAPACKC TRNSEG FOR
C                            USE IN ETA POST PROCESSOR.
C     
C USAGE:    CALL TRNSEG
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     STDOUT     - RUN-TIME STANDARD OUT.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - MAPOT
C                  EGRID
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     INCLUDE ETA E-GRID DIMENSIONS.
      INCLUDE "parmeta"
      PARAMETER(LP1=LM+1)
C     
C     DECLARE VARIABLES.
C
      LOGICAL NORTH
      REAL ALONVT, DLAM, DPHI, LAM0, PHI0, POLEI, POLEJ
      REAL SINPH0, TANPH0, WLONC, XMESHL, YLATC
C     
C     INCLUDE NECESSARY COMMON BLOCKS.
      INCLUDE "MAPOT.comm"
      INCLUDE "EGRID.comm"
      INCLUDE "IOUNIT.comm"
C
      EQUIVALENCE (YLATC,ALONVT), (DPHI, POLEI)
      EQUIVALENCE (WLONC, POLEJ), (DLAM,XMESHL)
C
      DATA CONV2R/0.017453293/,CONV2D/57.2958/
      DATA EARTHR/6371.2/
C     
C     SET COMMON BLOCK VARIABLES.
C     
      IF (TPH0D.GT.0) THEN
         NORTH = .TRUE.
      ELSE
         NORTH = .FALSE.
      ENDIF
      YLATC  = TPH0D
      DPHI   = DPHD
      WLONC  = ABS(TLM0D)
      DLAM   = DLMD
      IDIM   = 2*IM-1
      JDIM   = JM

      PHI0   = YLATC * CONV2R
      LAM0   = WLONC * CONV2R
      COSPH0 = COS(PHI0)
      SINPH0 = SIN(PHI0)
      TANPH0 = TAN(PHI0)
C     
      WRITE(STDOUT,*)'TRNSEG:  COMMON BLOCK EGRID BELOW'
      WRITE(STDOUT,*)'  NORTH        :  ',NORTH
      WRITE(STDOUT,*)'  ALONVT,XMESHL:  ',ALONVT,XMESHL
      WRITE(STDOUT,*)'  POLEI,POLEJ  :  ',POLEI,POLEJ
      WRITE(STDOUT,*)'  IDIM,JDIM    :  ',IDIM,JDIM
      WRITE(STDOUT,*)'  PHI0,LAM0    :  ',PHI0,LAM0
      WRITE(STDOUT,*)'  COS,SIN,TAN0 :  ',COSPH0,SINPH0,TANPH0
C     
      RETURN
      END
