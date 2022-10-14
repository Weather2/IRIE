!
!###############################################################################
!---------------------- Driver of the new microphysics -------------------------
!###############################################################################
!
      SUBROUTINE GSMDRIVE
!
!-------------------------------------------------------------------------------
!----- NOTE:  Code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:  Grid-scale microphysical processes - condensation & precipitation
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: February 2001
!  2001-04-xx   Ferrier     - Beta-tested version
!  2001-05-21   Ferrier     - Added gradual latent heating to remove external waves
!  2001-05-30   Ferrier     - Changed default to uniform maritime conditions for testing
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Merges original GSCOND & PRECPD subroutines.   
!   * Code has been substantially streamlined and restructured.
!   * Exchange between water vapor & small cloud condensate is calculated using
!     the original Asai (1965, J. Japan) algorithm.  See also references to
!     Yau and Austin (1979, JAS), Rutledge and Hobbs (1983, JAS), and Tao et al.
!     (1989, MWR).  This algorithm replaces the Sundqvist et al. (1989, MWR)
!     parameterization.  
!-------------------------------------------------------------------------------
! Prior PROGRAM HISTORY LOG:
!
! *** Heritage as Subroutine GSCOND:
!   94-~??  ZHAO         - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-03-28  BLACK      - ADDED EXTERNAL EDGE
!   98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!
! *** Heritage as Subroutine PRECPD:
!   94-~??  ZHAO       - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-11-20  ABELES     - PARALLEL OPTIMIZATION
!   96-03-29  BLACK      - REMOVED SCRCH COMMON
!   96-07-18  ZHAO       - NEW WMIN CALCULATION
!   96-09-25  BALDWIN    - NEW SR CALCULATION
!   98-11-02  BLACK      - MODIFICATION FOR DISTRIBUTED MEMORY
!-------------------------------------------------------------------------------
!     
! USAGE: CALL GSMDRIVE FROM MAIN PROGRAM EBU
!
!   INPUT ARGUMENT LIST:
!       NONE     
!  
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
! Subprograms & Functions called:
!   GSMCONST  - initialize rain & ice lookup tables, read from external file;
!               initialize constants
!   GSMCOLUMN - cloud microphysics calculations over vertical columns
!
! UNIQUE: NONE
!  
! LIBRARY: NONE
!  
!--- COMMON BLOCKS (input for microphysics):
!       CTLBLK, LOOPS, MASKS, PHYS, VRBLS, CLDWTR, PVRBLS, ACMCLH, PPTASM
!
!--- COMMON BLOCKS ("triggers" for microphysics & statistics):
!       CMICRO_START, CMICRO_STATS
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!------------------------------------------------------------------------
!
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
#include "sp.h"
!
!----------------------------------------------------------------------
      INTEGER, PARAMETER :: JAM=6+2*(JM-10), LP1=LM+1
!-----------------------------------------------------------------------
      LOGICAL :: RUN,FIRST,RESTRT,SIGMA,NOZ
!----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "PPTASM.comm"
!
!------------------------------------------------------------------------- 
!-----  Key parameters passed to column microphysics (COLUMN_MICRO) ------
!------------------------------------------------------------------------- 
!
!--- Flag from INIT.F at start of model run, used in initiating statistics
!
      COMMON /CMICRO_START/ MICRO_START
      LOGICAL :: MICRO_START, AETA_01
!
!--- This variable is for debugging purposes (if .true.)
!
      LOGICAL, PARAMETER :: PRINT_diag=.TRUE.
!
!--- The following variables are for microphysical statistics (non-essential)
!
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40, ITHILO=ITHI-ITLO+1,
     & ITHILO_N=ITHILO*4, ITHILO_QM=ITHILO*5, ITHILO_QT=ITHILO*22
      COMMON /CMICRO_STATS/ NSTATS(ITLO:ITHI,4), QMAX(ITLO:ITHI,5),
     & QTOT(ITLO:ITHI,22)
      INTEGER :: NSTATS, NSTATS_0(ITLO:ITHI,4)
      REAL :: QMAX, QTOT, QMAX_0(ITLO:ITHI,5),QTOT_0(ITLO:ITHI,22)
      REAL, SAVE :: Thour_print, 
     &  PRECmax(2),PRECtot(2),PRECmax_0(2),PRECtot_0(2)
      REAL, PARAMETER :: DThour_print=3.     ! Print statistics every 3 h
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ BEGIN section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      COMMON /C_FRACTIONS/ F_ice(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,                    F_rain(LM,IDIM1:IDIM2,JDIM1:JDIM2)
     &,                    F_RimeF(LM,IDIM1:IDIM2,JDIM1:JDIM2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ Saved values use REAL (REAL*4) arrays rather than INTEGER*2 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      REAL F_ice, F_rain, F_RimeF, Fice, Frain, DUM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ Saved values use INTEGER*2 arrays rather than REAL*4
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!--- Parameters used in converting from real to integer*2 fields for
!      fraction of ice (F_ice), fraction of rain (F_rain), and mass
!      ratio of rimed ice ("rime factor", F_RimeF).
!
!!      REAL, PARAMETER :: FSCALE=10000., RFSCALE=1./FSCALE, 
!!     &  RIMESCALE=100., RIMEROUND=.5/RIMESCALE
!!      INTEGER*2, PARAMETER :: IFMIN=0, IFMAX=FSCALE, IRimeF=RIMESCALE
!!
!!      INTEGER*2 :: F_ice, F_rain, F_RimeF, Fice, Frain, I2DUM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ END section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!-----------------------------------------------------------------------
!-------------- Local arrays & parameters in GSMDRIVE -----------------
!-----------------------------------------------------------------------
!
!---- Comments on 23 August 2001
!    * EPSQ=1.E-20 is the lower limit for specific humidity and cloud 
!      condensate.  The value of EPSQ will need to be changed in the other 
!      subroutines in order to make it consistent throughout the Eta code.  
!
      REAL, PARAMETER :: EPSQ=1.E-20, GRAV=9.81, RHOL=1000., T0C=273.15, 
     & T_ICE=-10., T_ICEK=T0C+T_ICE, RRHOL=1./RHOL, CLIMIT=1.E-12
!
      REAL ARAIN, ASNOW, P_col(LM), QI_col(LM), QR_col(LM),
     & QV_col(LM), QW_col(LM), RimeF_col(LM), T_col(LM), THICK_col(LM), 
     & WC_col(LM)
!
!------------------------------------------------------------------------
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!------------------------------------------------------------------------
!---------------------- Microphysical constants -------------------------
!------------------------------------------------------------------------
!
      DTPH=DTQ2                   ! Physics time step (s)
!
!------------------------------------------------------------------------
!--------------- Initialize constants for statistics --------------------
!------------------------------------------------------------------------
!
      IF (MICRO_START) THEN
        MICRO_START=.FALSE.       ! No need to calculate these parameters again
   !
   !--- Begin: Initializing arrays, lookup tables, & constants used in
   !           microphysics and radiation   (6/21/01)
   !
        F_ice=0.                  ! Initialize ice fraction array
        F_rain=0.                 ! Initialize rain fraction array
        F_RimeF=1.                ! Initialize rime factor array
        CALL GSMCONST (DTPH)      ! Initialize lookup tables & constants
        DO L=1,LM
          IF (AETA(L) .GT. .1) THEN
            AETA_01=.FALSE.
          ELSE
            AETA_01=.TRUE.        ! Remove initial clouds at very high levels
          ENDIF
          DO J=MYJS2,MYJE2
            DO I=MYIS,MYIE
              IF (CWM(I,J,L).LE.CLIMIT .OR. AETA_01) CWM(I,J,L)=0.
              IF (T(I,J,L) .LE. T_ICEK) F_ice(L,I,J)=1.
            ENDDO
          ENDDO
        ENDDO
        Thour_print=-DTPH/3600.
        IF (PRINT_diag) THEN
      !
      !-------- Total and maximum quantities
      !
          NSTATS=0      ! Microphysical statistics dealing w/ grid-point counts
          QMAX=0.       ! Microphysical statistics dealing w/ hydrometeor mass
          QTOT=0.       ! Microphysical statistics dealing w/ hydrometeor mass
          PRECmax=0.    ! Maximum precip rates (rain, snow) at surface (mm/h)
          PRECtot=0.    ! Total precipitation (rain, snow) accumulation at surface
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!------------------- Loop horizontally through domain ------------------
!-----------------------------------------------------------------------
!
      DO 100 J=MYJS2,MYJE2
        DO 100 I=MYIS,MYIE
          LSFC=LMH(I,J)                      ! "L" of surface
          PDSL=PD(I,J)*RES(I,J)              ! (Psfc-Ptop)/ETA_sfc
          CONST=PDSL/GRAV                    ! (Psfc-Ptop)/(g*ETA_sfc), used for THICK below
   !
   !--- Initialize column data (1D arrays)
   !
          IF (CWM(I,J,1) .LE. CLIMIT) CWM(I,J,1)=EPSQ
          F_ice(1,I,J)=1.
          F_rain(1,I,J)=0.
          F_RimeF(1,I,J)=1.
          DO L=1,LSFC
      !
      !--- Pressure (Pa) = (Psfc-Ptop)*(ETA/ETA_sfc)+Ptop
      !
            P_col(L)=PDSL*AETA(L)+PT
      !
      !--- Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
      !
            THICK_col(L)=CONST*DETA(L)
            T_col(L)=T(I,J,L)
            TC=T_col(L)-T0C
            QV_col(L)=max(EPSQ, Q(I,J,L))
            IF (CWM(I,J,L) .LE. CLIMIT) THEN
              WC_col(L)=0.
              IF (TC .LT. T_ICE) THEN
                F_ice(L,I,J)=1.
              ELSE
                F_ice(L,I,J)=0.
              ENDIF
              F_rain(L,I,J)=0.
              F_RimeF(L,I,J)=1.
            ELSE
              WC_col(L)=CWM(I,J,L)
            ENDIF
      !
      !--- Determine composition of condensate in terms of 
      !      cloud water, ice, & rain
      !
            WC=WC_col(L)
            QI=0.
            QR=0.
            QW=0.
            Fice=F_ice(L,I,J)
            Frain=F_rain(L,I,J)
      !
      !--- REAL*4 array storage
      !
            IF (Fice .GE. 1.) THEN
              QI=WC
            ELSE IF (Fice .LE. 0.) THEN
              QW=WC
            ELSE
              QI=Fice*WC
              QW=WC-QI
            ENDIF
            IF (QW.GT.0. .AND. Frain.GT.0.) THEN
              IF (Frain .GE. 1.) THEN
                QR=QW
                QW=0.
              ELSE
                QR=Frain*QW
                QW=QW-QR
              ENDIF
            ENDIF
            RimeF_col(L)=F_RimeF(L,I,J)               ! (real)
!!      !
!!      !--- INTEGER*2 array storage
!!      !
!!            IF (TC.LE.T_ICE .OR. Fice.GE.IFMAX) THEN
!!              QI=WC
!!            ELSE IF (Fice .LE. IFMIN) THEN
!!              QW=WC
!!            ELSE
!!              QI=RFSCALE*Fice*WC
!!              QW=WC-QI
!!            ENDIF
!!            IF (QW.GT.0. .AND. Frain.GT.0) THEN
!!              IF (Frain .EQ. IFMAX) THEN
!!                QR=QW
!!                QW=0.
!!              ELSE
!!                QR=RFSCALE*Frain*QW
!!                QW=QW-QR
!!              ENDIF
!!            ENDIF
!!            RimeF_col(L)=F_RimeF(L,I,J)/RIMESCALE
      !
            QI_col(L)=QI
            QR_col(L)=QR
            QW_col(L)=QW
          ENDDO
!
!#######################################################################
   !
   !--- Perform the microphysical calculations in this column
   !
          I_index=I
          J_index=J
          CALL GSMCOLUMN ( ARAIN, ASNOW, DTPH, I_index, J_index, LSFC,
     & P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col, 
     & THICK_col, WC_col )
   !
!#######################################################################
!
   !
   !--- Update storage arrays
   !
          DO L=1,LSFC
            TRAIN(I,J,L)=(T_col(L)-T(I,J,L))/DTPH
            T(I,J,L)=T_col(L)
            Q(I,J,L)=QV_col(L)
!            TLATGS(I,J,L)=T_col(L)-T(I,J,L)
            CWM(I,J,L)=WC_col(L)
      !
      !--- REAL*4 array storage
      !
            F_RimeF(L,I,J)=MAX(1., RimeF_col(L))
            IF (QI_col(L) .LE. CLIMIT) THEN
              F_ice(L,I,J)=0.
              IF (T_col(L) .LT. T_ICEK) F_ice(L,I,J)=1.
            ELSE
              F_ice(L,I,J)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
            ENDIF
            IF (QR_col(L) .LE. CLIMIT) THEN
              DUM=0
            ELSE
              DUM=QR_col(L)/(QR_col(L)+QW_col(L))
            ENDIF
            F_rain(L,I,J)=DUM
!!      !
!!      !--- INTEGER*2 array storage
!!      !
!!            IF (RimeF_col(L) .LE. 1.) THEN
!!              I2DUM=IRimeF
!!              I2DUM=IRimeF
!!            ELSE
!!              I2DUM=(RimeF_col(L)+RIMEROUND)*RIMESCALE
!!            ENDIF
!!            F_RimeF(L,I,J)=I2DUM
!!            IF (QI_col(L) .LE. CLIMIT) THEN
!!              I2DUM=0
!!            ELSE
!!              I2DUM=FSCALE*QI_col(L)/WC_col(L)
!!              I2DUM=MAX( IFMIN, MIN(IFMAX, I2DUM) )
!!            ENDIF
!!            F_ice(L,I,J)=I2DUM
!!            IF (QR_col(L) .LE. CLIMIT) THEN
!!              I2DUM=0
!!            ELSE
!!              I2DUM=FSCALE*QR_col(L)/(QR_col(L)+QW_col(L))
!!              I2DUM=MAX( IFMIN, MIN(IFMAX, I2DUM) )
!!            ENDIF
!!            F_rain(L,I,J)=I2DUM
      !
          ENDDO
   !
   !--- Update accumulated precipitation statistics
   !
   !--- Surface precipitation statistics; SR is fraction of surface 
   !    precipitation (if >0) associated with snow
   !
        APREC(I,J)=(ARAIN+ASNOW)*RRHOL       ! Accumulated surface precip (depth in m)  !<--- Ying
        PREC(I,J)=PREC(I,J)+APREC(I,J)
        ACPREC(I,J)=ACPREC(I,J)+APREC(I,J)
        IF(APREC(I,J) .LT. 1.E-8) THEN
          SR(I,J)=0.
        ELSE
          SR(I,J)=RRHOL*ASNOW/APREC(I,J)
        ENDIF
   !
   !--- Debug statistics 
   !
        IF (PRINT_diag) THEN
          PRECtot(1)=PRECtot(1)+ARAIN
          PRECtot(2)=PRECtot(2)+ASNOW
          PRECmax(1)=MAX(PRECmax(1), ARAIN)
          PRECmax(2)=MAX(PRECmax(2), ASNOW)
        ENDIF
!#######################################################################
!#######################################################################
!
100   CONTINUE                          ! End "I" & "J" loops
!
!#######################################################################
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------- END of main microphysics loop -------------------
!-----------------------------------------------------------------------
!
      time_model=float(NTSD-1)*DT/3600.
      IF (PRINT_diag .AND. time_model.GE.Thour_print) THEN
        CALL MPI_REDUCE(NSTATS,NSTATS_0,ITHILO_N,MPI_INTEGER,MPI_SUM,0,
     &                  MPI_COMM_COMP,IRTN)
        CALL MPI_REDUCE(QMAX,QMAX_0,ITHILO_QM,MPI_REAL,MPI_MAX,0,
     &                  MPI_COMM_COMP,IRTN)
        CALL MPI_REDUCE(PRECmax,PRECmax_0,2,MPI_REAL,MPI_MAX,0,
     &                  MPI_COMM_COMP,IRTN)
        CALL MPI_REDUCE(QTOT,QTOT_0,ITHILO_QT,MPI_REAL,MPI_SUM,0,
     &                  MPI_COMM_COMP,IRTN)
        CALL MPI_REDUCE(PRECtot,PRECtot_0,2,MPI_REAL,MPI_SUM,0,
     &                  MPI_COMM_COMP,IRTN)
       IF (MYPE .EQ. 0) THEN
        HDTPH=3600./DTPH            ! Convert precip rates to mm/h
        DO K=ITLO,ITHI
          QMAX_0(K,1)=1000.*QMAX_0(K,1)
          QMAX_0(K,2)=1000.*QMAX_0(K,2)
          QMAX_0(K,3)=1000.*QMAX_0(K,3)
          QMAX_0(K,4)=HDTPH*QMAX_0(K,4)
          QMAX_0(K,5)=HDTPH*QMAX_0(K,5)
        ENDDO
        PRECmax_0(1)=HDTPH*PRECmax_0(1)
        PRECmax_0(2)=HDTPH*PRECmax_0(2)
   !
        WRITE(6,"(A,F5.2,4(A,G11.4))") '{ Time(h)=',time_model,
     & '  TRAIN_sfc=',PRECtot_0(1),'  TSNOW_sfc=',PRECtot_0(2),
     & '  RRmax_sfc(mm/h)=',PRECmax_0(1),
     & '  SRmax_sfc(mm/h)=',PRECmax_0(2)
   !
        WRITE(6,"(3A)") '{ (C) <--------- Counts ----------> ',
     & '<----------- g/kg ----------> <----- mm/h ------>',
     & ' <---- kg/m**2 * # grids ---->'
        WRITE(6,"(3A)") '{  T     NCICE  NCMIX  NCWAT NCRAIN  ',
     & 'QIMAX     QWMAX     QRMAX     SRMAX     RRMAX     QITOT     ',
     & 'QWTOT     QRTOT'
        DO K=ITLO,ITHI
          WRITE(6,"(A,I3,I9,3I7,8G10.4)") 
     &      '{ ',K,(NSTATS_0(K,II), II=1,4),
     &      (QMAX_0(K,JJ), JJ=1,5),(QTOT_0(K,KK), KK=1,3)
        ENDDO
   !
        WRITE(6,"(3A)")
     & '{  T   TCOND     TICND     TIEVP     TIDEP     TREVP     ',
     & 'TRAUT     TRACW     TIMLT     TIACW     TIACWI    TIACWR    ',
     & 'TIACR'
        DO K=ITLO,ITHI
          WRITE(6,"(A,I3,12G10.4)") '{ ',K,(QTOT_0(K,II), II=4,15)
        ENDDO
   !
        WRITE(6,"(2A)")
     & '{  T   DEL_QT   TVDIF   DEL_HYD        TWDIF  TIDIF       ',
     & 'TRDIF    DARAIN   DASNOW    RimeF'
        DO K=ITLO,ITHI
          DEL_HYD=0.
          DO II=17,19
            DEL_HYD=DEL_HYD+QTOT_0(K,II)
          ENDDO
          DEL_QT=0.
          DO II=16,21
            DEL_QT=DEL_QT+QTOT_0(K,II)
          ENDDO
          IF (QTOT_0(K,22) .GT. 0.) THEN
            RimeF_bulk=QTOT_0(K,1)/QTOT_0(K,22)
            ELSE
            RimeF_bulk=1.
          ENDIF
          WRITE(6,"(A,I3,9G10.4)") '{ ',K,DEL_QT,QTOT_0(K,16),
     &      DEL_HYD,(QTOT_0(K,II), II=17,21),RimeF_bulk
        ENDDO
   !
       ENDIF
       NSTATS=0      ! Microphysical statistics dealing w/ grid-point counts
       QMAX=0.       ! Microphysical statistics dealing w/ hydrometeor mass
       QTOT=0.       ! Microphysical statistics dealing w/ hydrometeor mass
       PRECmax=0.    ! Maximum precip rates (rain, snow) at surface (mm/h)
       PRECtot=0.    ! Total precipitation (rain, snow) accumulation at surface
       Thour_print=Thour_print+DThour_print
      ENDIF
!
!-----------------------------------------------------------------------
!------------------------ Return to main program -----------------------
!-----------------------------------------------------------------------
!
      RETURN
!-----------------------------------------------------------------------
200   format(a2,i5,f6.2,4(1x,a10,g11.4))
210   format(a2,i5,f6.2,4(1x,a10,i7))
!-----------------------------------------------------------------------
      END
