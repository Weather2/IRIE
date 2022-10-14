      SUBROUTINE CLTEND (ICLTEND)
!-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CLTEND      TEMPERATURE CHANGE BY CLOUD PROCESSES
C   PRGRMMR: Ferrier         ORG: W/NP22     DATE: 26 Sept 2001
C     
C ABSTRACT:
C     CLTEND gradually updates temperature tendencies from convection
C     grid-scale microphysics, and precipitation assimilation
C     
C USAGE: CALL CLTEND FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C     ICLTEND - flag set to -1 prior to physics calls, 0 after physics
C               calls, and 1 for updating temperatures every time step
C  
C   OUTPUT ARGUMENT LIST:  NONE
C     
C   OUTPUT FILES:  NONE
C     
C   SUBPROGRAMS CALLED:  NONE
C  
C   UNIQUE: NONE
C  
C   LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  VRBLS
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
!-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
!-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      REAL, SAVE :: T_adj (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,             T_old (IDIM1:IDIM2,JDIM1:JDIM2,LM)
C-----------------------------------------------------------------------
!
!--- FORTRAN 90 extension of whole array operations
!
      IF (ICLTEND .LT. 0) THEN
        T_old = T
      ELSE IF (ICLTEND .EQ. 0) THEN
        T_adj = T - T_old
        T = T_old
      ELSE
        DELTPH=1./FLOAT(NPHS)
        T = T + DELTPH*T_adj
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
