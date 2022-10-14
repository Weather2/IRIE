      SUBROUTINE W3FI01(LW)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI01        DETERMINES MACHINE WORD LENGTH IN BYTES
C   PRGMMR: KEYSER           ORG: NMC22       DATE:92-07-30
C
C ABSTRACT: DETERMINES THE NUMBER OF BYTES IN A FULL WORD FOR THE
C   PARTICULAR MACHINE (IBM OR CRAY).
C
C PROGRAM HISTORY LOG:
C   92-01-10  R. KISTLER (W/NMC23)
C   92-05-22  D. A. KEYSER -- DOCBLOCKED/COMMENTED
C
C USAGE:    CALL W3FI01(LW)
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     LW       - MACHINE WORD LENGTH IN BYTES
C
C   OUTPUT FILES:
C     FT06F001 - PRINTOUT
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: IBM AIX XL FORTRAN Compiler/6000
C   MACHINE:  IBM RS/6000 MODEL 530, 560
C
C$$$
C
C     CHARACTER*4  CPRINT(2)
      CHARACTER*8  CTEST1,CTEST2
C
      INTEGER  ITEST1,ITEST2
C
      SAVE
C
      EQUIVALENCE (CTEST1,ITEST1),(CTEST2,ITEST2)
C
      DATA  CTEST1/'12345678'/
C
      ITEST2 = ITEST1
CCCCC PRINT *,' CTEST1 = ',CTEST1,' CTEST2 = ',CTEST2
      IF (CTEST1 .EQ. CTEST2) THEN
CCCCC   PRINT*,' MACHINE WORD LENGTH IS 8 BYTES'
        LW = 8
      ELSE
CCCCC   PRINT*,' MACHINE WORD LENGTH IS 4 BYTES'
        LW = 4
      ENDIF
      RETURN
      END
