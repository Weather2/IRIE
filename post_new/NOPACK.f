      SUBROUTINE NOPACK(IFLD,ILVL,GRID,IMOUT,JMOUT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    NOPACK      POST FIELDS IN CRAY BINARY
C   PRGRMMR: TREADON         ORG: W/NP2 
C     
C ABSTRACT:
C     THIS ROUTINE POSTS THE DATA IN THE PASSED ARRAY GRID
C     TO THE OUTPUT FILE AS AN UNFORMATTED CRAY BINARY FILE.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C     
C USAGE:    CALL NOPACK(IFLD,ILVL,GRID,IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IFLD     - FIELD ID TAG.
C     ILVL     - INTEGER TAG FOR LEVEL OF FIELD.
C     GRID     - FIELD TO BE POSTED IN CRAY BINARY.
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C     LUNOUT  - UNIT DATA IS POSTED TO.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       MINMAX   - DETERMINES MIN/MAX VALUES IN AN ARRAY.
C     LIBRARY:
C       COMMON   - CTLBLK
C                  MAPOT
C                  RQSTFLD
C                  IOUNIT
C                  OUTGRD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C
C
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "parmout"
      PARAMETER (LP1=LM+1,D50=0.5E0)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESRT,SIGMA,OLDRD,STDRD
      LOGICAL NORTH,HEADER
      CHARACTER*4  RESTHR
      CHARACTER*6  PROJ,BLANK
      CHARACTER*8  DESCR2
      CHARACTER*50 ENVAR
      CHARACTER*80 FNAME
      REAL GRID(IMOUT,JMOUT)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "OUTGRD.comm"
      DATA BLANK/'      '/
C     
C
C****************************************************************************
C     START NOPACK HERE.
C
C     SET OUTPUT GRID TYPE.  WE ASSUME KGYTPE HOLDS THE ON84
C     ID FOR THE OUTPUT GRID.
C     UNIQUE ON84 LABELS FOR VARIOUS ETA DOMAINS BELOW:
C              90 = STAGGERED 80KM E-GRID 
C              91 = FILLED    80KM E-GRID
C              92 = STAGGERED 40KM E-GRID
C              93 = FILLED    40KM E-GRID
C              94 = STAGGERED 30KM E-GRID
C              95 = FILLED    30KM E-GRID
C              96 = STAGGERED "BIG" 40KM E-GRID (EDAS)
C     
      KGTYP = KGTYPE
C     IF NEED BE, OPEN A UNIT TO THE OUTPUT FILE.  IF REQUESTED,
C     WRITE DATE AND GRID INFO IF REQUESTED.
C     
      IF (RITEHD) THEN
C
C        APPEND FORECAST HOUR TO SUFFIX FOR SEQUENTIAL BINARY FILE.
         IHR = NTSD/TSPH + 0.5
C     
C        GET FULL PATH FOR OUTPUT FILE FROM ENVIRONMENT VARIABLE
C        COMSP WHICH IS SET IN THE SCRIPT RUNNING THE MODEL.
C     
C        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         ENVAR = ' '
         CALL GETENV('COMSP',ENVAR)
         IF (ENVAR(1:6).EQ.BLANK) THEN
          WRITE(DESCR2,1000) IHR
 1000     FORMAT('.SbinF',I2.2)
          KDAT = INDEX(DATSET,' ') -1
          IF (KDAT.LE.0) KDAT = LEN(DATSET)
          FNAME = DATSET(1:KDAT) // DESCR2
         ELSE
          CALL GETENV('tmmark',RESTHR)
          WRITE(DESCR2,1011) IHR
 1011     FORMAT(I2.2)
          KENV = INDEX(ENVAR,' ') -1
          IF (KENV.LE.0) KENV = LEN(ENVAR)
          KDAT = INDEX(DATSET,' ') -1
          IF (KDAT.LE.0) KDAT = LEN(DATSET)
          FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:2)
     &              //'.'// RESTHR
         ENDIF
C
C        ASSIGN AND OPEN UNIT FOR DATA FILE.
         CLOSE(LUNOUT)
C        CALL ASNUNIT(LUNOUT,'-F cos -N ibm',ier)
C        IF (IER.NE.0) WRITE(STDOUT,*)
C    X        'NOPACK:  ASNUNIT ERROR IER=',IER
         OPEN(LUNOUT,FILE=FNAME,FORM='UNFORMATTED')
         REWIND(LUNOUT)
C
         RITEHD = .FALSE.
         WRITE(STDOUT,*)'NOPACK:  OPENED ',LUNOUT,' ',FNAME,
     X        ' ',DATSET
C      
C        IF REQUESTED, THE FIRST TWO "RECORDS" INCLUDE DATE 
C        AND OUTPUT GRID INFORMATION.  WE WRITE THEM ONLY 
C        ONCE TO EACH OUTPUT GRID.  
C     
         IF (INDEX(DATSET,'NOHEAD').EQ.0) THEN
            IHH = NTSD/TSPH+D50
            IYY = IDAT(3)-1900
            IMM = IDAT(1)
            IDD = IDAT(2)
            WRITE(LUNOUT) IHRST,IMM,IDD,IYY,IHH
            WRITE(LUNOUT) KGTYP,PROJ,NORTH,IMOUT,JMOUT,
     X           POLEI,POLEJ,ALATVT,ALONVT,XMESHL
            WRITE(STDOUT,*)'NOPACK:  WROTE DATE AND GRID INFO'
            WRITE(STDOUT,1010) IHRST,IMM,IDD,IYY,IHH
            WRITE(STDOUT,1020) KGTYP,PROJ,NORTH,IMOUT,JMOUT
            WRITE(STDOUT,1030) POLEI,POLEJ,ALATVT,ALONVT
            WRITE(STDOUT,1030) XMESHL
 1010       FORMAT(5(I5,1X))
 1020       FORMAT(I5,1X,A6,1X,L1,1X,I5,1X,I5)
 1030       FORMAT(4(G12.6,1X))
         ENDIF
      ENDIF
C     
C     SET SURFACE VALUE FOR DATA.
C
      SFC = 0.
      IF (IS(IDENT(IFLD)).EQ.8) THEN
         SFC = SPL(ILVL)
      ELSEIF (IS(IDENT(IFLD)).EQ.149) THEN
C
C   TKE IS ON THE ETA INTERFACE AT THE BOTTOM OF THE LAYER ILVL
C
         SFC = AETA(ILVL)
         IF (IQ(IDENT(IFLD)).EQ.158) SFC = ETA(ILVL+1)
      ENDIF
C
C     IF DATSET CONTAINS "NOHEAD" WE SKIP THE HEADER STUFF 
C     THAT FOLLOWS AND GO DIRECTLY TO THE DATA.  A USER 
C     WOULD USED THIS OPTION IF (S)HE WANTS TO DIRECTLY
C     CONTOUR DATA USING NMCIDAS.
C     
      HEADER = .FALSE.
      IF (INDEX(DATSET,'NOHEAD').NE.0) GOTO 10
      WRITE(LUNOUT) FIELD(IFLD),SFC
      HEADER = .TRUE.
C     
C     WRITE DATA TO OUTPUT FILE.
C     
 10   CONTINUE
      WRITE(LUNOUT) GRID
      CALL MINMAX(GRID,IMOUT,JMOUT,FMIN,FMAX)
      WRITE(STDOUT,1040) IFLD,FIELD(IFLD),SFC,FMIN,FMAX,
     X     KGTYP,HEADER
 1040 FORMAT('NOPACK:',I3,1X,A20,1X,3(G12.6,1X),I5,1X,L1)
C     
C     END OF ROUTINE.
C     
      RETURN
      END
