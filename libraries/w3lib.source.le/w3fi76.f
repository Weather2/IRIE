      SUBROUTINE W3FI76(PVAL,KEXP,KMANT,KBITS)
CDIR$ INTEGER=64
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI76        CONVERT TO IBM370 FLOATING POINT
C   PRGMMR: REJONES          ORG: NMC421      DATE:92-11-16
C
C ABSTRACT: CONVERTS FLOATING POINT NUMBER FROM MACHINE
C   REPRESENTATION TO GRIB REPRESENTATION (IBM370 32 BIT F.P.).
C
C PROGRAM HISTORY LOG:
C   85-09-15  JOHN HENNESSY  ECMWF
C   92-09-23  JONES,R. E.    CHANGE NAME, ADD DOC BLOCK
C   93-10-27  JONES,R. E.    CHANGE TO AGREE WITH HENNESSY CHANGES
C
C USAGE:    CALL W3FI76 (FVAL, KEXP, KMANT, NBITS)
C   INPUT ARGUMENT LIST:
C     PVAL     - FLOATING POINT NUMBER TO BE CONVERTED
C     KBITS    - NUMBER OF BITS IN COMPUTER WORD (32 OR 64)
C
C   OUTPUT ARGUMENT LIST:
C     KEXP     -  8 BIT SIGNED EXPONENT
C     KMANT    - 24 BIT  MANTISSA  (FRACTION)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: IBM370 VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS 9000, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
C
C********************************************************************
C*
C*    NAME      : CONFP3
C*
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
C*                REPRESENTATION TO GRIB REPRESENTATION.
C*
C*    INPUT     : PVAL  - FLOATING POINT NUMBER TO BE CONVERTED.
C*    NBIT      : NBITS - NUMBER OF BITS IN COMPUTER WORD
C*
C*    OUTPUT    : KEXP  - 8 BIT SIGNED EXPONENT
C*                KMANT - 24 BIT MANTISSA
C*                PVAL  - UNCHANGED.
C*
C*    JOHN HENNESSY , ECMWF   18.06.91
C*
C********************************************************************
C
C
C     IMPLICIT NONE
C
      INTEGER IEXP
      INTEGER ISIGN
C
      INTEGER KBITS
      INTEGER KEXP
      INTEGER KMANT
C
      REAL PVAL
      REAL ZEPS
      REAL ZREF
C
      SAVE
C
C     TEST FOR FLOATING POINT ZERO
C
      IF (PVAL.EQ.0.0) THEN
        KEXP  = 0
        KMANT = 0
        GO TO 900
      ENDIF
C
C     SET ZEPS TO 1.0E-12 FOR 64 BIT COMPUTERS (CRAY)
C     SET ZEPS TO 1.0E-8  FOR 32 BIT COMPUTERS
C
      IF (KBITS.EQ.32) THEN
        ZEPS = 1.0E-8
      ELSE
        ZEPS = 1.0E-12
      ENDIF
      ZREF = PVAL
C
C     SIGN OF VALUE
C
      ISIGN = 0
      IF (ZREF.LT.0.0) THEN
        ISIGN =   128
        ZREF    = - ZREF
      ENDIF
C
C     EXPONENT
C
      IEXP = INT(ALOG(ZREF)*(1.0/ALOG(16.0))+64.0+1.0+ZEPS)
C
      IF (IEXP.LT.0  ) IEXP = 0
      IF (IEXP.GT.127) IEXP = 127
C
C     MANTISSA
C
C     CLOSEST NUMBER IN GRIB FORMAT TO ORIGINAL NUMBER
C     (EQUAL TO, GREATER THAN OR LESS THAN ORIGINAL NUMBER).
C
      KMANT = NINT (ZREF/16.0**(IEXP-70))
C
C     CHECK THAT MANTISSA VALUE DOES NOT EXCEED 24 BITS
C     16777215 = 2**24 - 1
C
      IF (KMANT.GT.16777215) THEN
         IEXP  = IEXP + 1
C
C     CLOSEST NUMBER IN GRIB FORMAT TO ORIGINAL NUMBER
C     (EQUAL TO, GREATER THAN OR LESS THAN ORIGINAL NUMBER).
C
         KMANT = NINT (ZREF/16.0**(IEXP-70))
C
C        CHECK MANTISSA VALUE DOES NOT EXCEED 24 BITS AGAIN
C
         IF (KMANT.GT.16777215) THEN
           PRINT *,'BAD MANTISSA VALUE FOR PVAL = ',PVAL
         ENDIF
      ENDIF
C
C     ADD SIGN BIT TO EXPONENT.
C
      KEXP = IEXP + ISIGN
C
  900 CONTINUE
C
      RETURN
      END
