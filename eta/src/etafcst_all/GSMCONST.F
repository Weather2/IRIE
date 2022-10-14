!
!#######################################################################
!------- Initialize constants & lookup tables for microphysics ---------
!#######################################################################
!
      SUBROUTINE GSMCONST (DTPH)
!
!-------------------------------------------------------------------------------
!---  SUBPROGRAM DOCUMENTATION BLOCK
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: February 2001
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Reads various microphysical lookup tables used in COLUMN_MICRO
!   * Lookup tables were created "offline" and are read in during execution
!   * Creates lookup tables for saturation vapor pressure w/r/t water & ice
!-------------------------------------------------------------------------------
!     
! USAGE: CALL GSMCONST FROM SUBROUTINE GSMDRIVE AT MODEL START TIME
!
!   INPUT ARGUMENT LIST:
!       DTPH - physics time step (s)
!  
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBROUTINES:
!     MY_GROWTH_RATES - lookup table for growth of nucleated ice
!     GPVS            - lookup tables for saturation vapor pressure (water, ice)
!
!   UNIQUE: NONE
!  
!   LIBRARY: NONE
!  
!   COMMON BLOCKS:
!     CMICRO_CONS - constants used in GSMCOLUMN
!     CMY600       - lookup table for growth of ice crystals in 
!                    water saturated conditions (Miller & Young, 1979)
!     IVENT_TABLES - lookup tables for ventilation effects of ice
!     IACCR_TABLES - lookup tables for accretion rates of ice
!     IMASS_TABLES - lookup tables for mass content of ice
!     IRATE_TABLES - lookup tables for precipitation rates of ice
!     IRIME_TABLES - lookup tables for increase in fall speed of rimed ice
!     MAPOT        - Need lat/lon grid resolution
!     RVENT_TABLES - lookup tables for ventilation effects of rain
!     RACCR_TABLES - lookup tables for accretion rates of rain
!     RMASS_TABLES - lookup tables for mass content of rain
!     RVELR_TABLES - lookup tables for fall speeds of rain
!     RRATE_TABLES - lookup tables for precipitation rates of rain
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!-----------------------------------------------------------------------
!
!--- INCLUDE COMMON BLOCKS
!
      INCLUDE "parmeta"
      INTEGER, PARAMETER :: LP1=LM+1
      INCLUDE "MAPOT.comm"
!
!------------------------------------------------------------------------- 
!-------------- Parameters & arrays for lookup tables -------------------- 
!------------------------------------------------------------------------- 
!
!--- Common block of constants used in column microphysics
!
      COMMON /CMICRO_CONS/ ABFR, CBFR, CIACW, CIACR, C_N0r0, 
     & CN0r0, CN0r_DMRmin, CN0r_DMRmax, CRACW, CRAUT, ESW0, 
     & QAUT0, RFmax, RHgrd, RQR_DR1, RQR_DR2, RQR_DR3, RQR_DRmin, 
     & RQR_DRmax, RR_DRmin, RR_DR1, RR_DR2, RR_DR3, RR_DRmax
!
!--- Common block for lookup table used in calculating growth rates of
!    nucleated ice crystals growing in water saturated conditions
!
      INTEGER, PARAMETER :: MY_T1=1, MY_T2=35
      COMMON /CMY600/ MY_GROWTH(MY_T1:MY_T2)
      REAL MY_GROWTH
!
!--- Mean ice particle diameters vary from 50 microns to 1000 microns
!
      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, DelDMI=1.e-6,
     &  XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax
!
!--- Various ice lookup tables
!
      COMMON /IACCR_TABLES/ ACCRI(MDImin:MDImax)
      COMMON /IMASS_TABLES/ MASSI(MDImin:MDImax)
      COMMON /SDENS_TABLES/ SDENS(MDImin:MDImax)
      REAL MASSI
      COMMON /IRATE_TABLES/ VSNOWI(MDImin:MDImax)
      COMMON /IVENT_TABLES/ VENTI1(MDImin:MDImax), VENTI2(MDImin:MDImax)
!
!--- Common block for riming tables
!
      INTEGER, PARAMETER :: Nrime=40
      COMMON /IRIME_TABLES/ VEL_RF(2:9,0:Nrime)
!
!--- Mean rain drop diameters vary from 50 microns to 450 microns 
!
      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=.45E-3, DelDMR=1.E-6,
     & XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax
!
!--- Various rain lookup tables
!
      COMMON /RACCR_TABLES/ ACCRR(MDRmin:MDRmax)
      COMMON /RMASS_TABLES/ MASSR(MDRmin:MDRmax)
      REAL MASSR
      COMMON /RRATE_TABLES/ RRATE(MDRmin:MDRmax)
      COMMON /RVELR_TABLES/ VRAIN(MDRmin:MDRmax)
      COMMON /RVENT_TABLES/ VENTR1(MDRmin:MDRmax), VENTR2(MDRmin:MDRmax)
!
!--- Parameters & data statement for local calculations
!
      REAL, PARAMETER :: C1=1./3., DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3, 
     & N0r0=8.E6, N0s0=4.E6, RHOL=1000., RHOS=100., T0C=273.15, 
     & XMR1=1.e6*DMR1, XMR2=1.e6*DMR2, XMR3=1.e6*DMR3
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
!
!------------------------------------------------------------------------
!--------- Constants passed through /CMICRO_CONS/ common block ----------
!------------------------------------------------------------------------
!
!--- 9/1/01:  Assume the following functional dependence for 5 - 100 km resolution:
!       RHgrd=0.90 for dx=100 km, 0.98 for dx=5 km, where
!       RHgrd=0.90+0.08*[(100.-dX)/95.]**.5
!
      DX=111.*(DPHD**2+DLMD**2)**.5         ! Model resolution at equator (km)
      DX=MIN(100., MAX(5., DX) )
      RHgrd=0.90+.08*((100.-DX)/95.)**.5
!
!--- Create lookup tables for saturation vapor pressure w/r/t water & ice
!
      CALL GPVS
!
!--- Read in various lookup tables
!
      OPEN (UNIT=1,FILE="eta_micro_lookup.dat",FORM="UNFORMATTED")
      READ(1) VENTR1
      READ(1) VENTR2
      READ(1) ACCRR
      READ(1) MASSR
      READ(1) VRAIN
      READ(1) RRATE
      READ(1) VENTI1
      READ(1) VENTI2
      READ(1) ACCRI
      READ(1) MASSI
      READ(1) VSNOWI
      READ(1) VEL_RF
!      read(1) my_growth    ! Applicable only for DTPH=180 s
      CLOSE (1)
!
!--- Calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
      CALL MY_GROWTH_RATES (DTPH)
!
      PI=ACOS(-1.)
!
!--- Constants associated with Biggs (1953) freezing of rain, as parameterized
!    following Lin et al. (JCAM, 1983) & Reisner et al. (1998, QJRMS).
!
      ABFR=-0.66
      BBFR=100.
      CBFR=20.*PI*PI*BBFR*RHOL*1.E-42
!
!--- CIACW is used in calculating riming rates
!      The assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
      CIACW=DTPH*0.25*PI*0.5*(1.E5)**C1
!
!--- CIACR is used in calculating freezing of rain colliding with large ice
!      The assumed collection efficiency is 1.0
!
      CIACR=PI*DTPH
!
!--- Based on rain lookup tables for mean diameters from 0.05 to 0.45 mm
!    * Four different functional relationships of mean drop diameter as 
!      a function of rain rate (RR), derived based on simple fits to 
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.  
!
      RR_DRmin=N0r0*RRATE(MDRmin)     ! RR for mean drop diameter of .05 mm
      RR_DR1=N0r0*RRATE(MDR1)         ! RR for mean drop diameter of .10 mm
      RR_DR2=N0r0*RRATE(MDR2)         ! RR for mean drop diameter of .20 mm
      RR_DR3=N0r0*RRATE(MDR3)         ! RR for mean drop diameter of .32 mm
      RR_DRmax=N0r0*RRATE(MDRmax)     ! RR for mean drop diameter of .45 mm
!
      RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
      RQR_DR1=N0r0*MASSR(MDR1)        ! Rain content for mean drop diameter of .10 mm
      RQR_DR2=N0r0*MASSR(MDR2)        ! Rain content for mean drop diameter of .20 mm
      RQR_DR3=N0r0*MASSR(MDR3)        ! Rain content for mean drop diameter of .32 mm
      RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm
      C_N0r0=PI*RHOL*N0r0
      CN0r0=1.E6/C_N0r0**.25
      CN0r_DMRmin=1./(PI*RHOL*DMRmin**4)
      CN0r_DMRmax=1./(PI*RHOL*DMRmax**4)
!
!--- CRACW is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
      CRACW=DTPH*0.25*PI*1.0
!
      ESW0=1000.*FPVS0(T0C)     ! Saturation vapor pressure at 0C
      RFmax=1.1**Nrime          ! Maximum rime factor allowed
!
!------------------------------------------------------------------------
!--------------- Constants passed through argument list -----------------
!------------------------------------------------------------------------
!
!--- Important parameters for self collection (autoconversion) of 
!    cloud water to rain. 
!
!--- CRAUT is proportional to the rate that cloud water is converted by
!      self collection to rain (autoconversion rate)
!
      CRAUT=1.-EXP(-1.E-3*DTPH)
!
!--- QAUT0 is the threshold cloud content for autoconversion to rain 
!      needed for droplets to reach a diameter of 20 microns (following
!      Manton and Cotton, 1977; Banta and Hanson, 1987, JCAM)
!--- QAUT0=1.2567, 0.8378, or 0.4189 g/m**3 for droplet number concentrations
!          of 300, 200, and 100 cm**-3, respectively
!
      XNCW=200.E6                 ! 300 cm**-3 droplet concentration
      QAUT0=PI*RHOL*XNCW*(20.E-6)**3/6.
!
!--- For calculating snow optical depths by considering bulk density of
!      snow based on emails from Q. Fu (6/27-28/01), where optical 
!      depth (T) = 1.5*SWP/(Reff*DENS), SWP is snow water path, Reff 
!      is effective radius, and DENS is the bulk density of snow.
!
!    SWP (kg/m**2)=(1.E-3 kg/g)*SWPrad, SWPrad in g/m**2 used in radiation
!    T = 1.5*1.E3*SWPrad/(Reff*DENS)
!  
!    See derivation for MASSI(INDEXS), note equal to RHO*QSNOW/NSNOW
!
!    SDENS=1.5e3/DENS, DENS=MASSI(INDEXS)/[PI*(1.E-6*INDEXS)**3]
!
      DO I=MDImin,MDImax
        SDENS(I)=PI*1.5E-15*FLOAT(I*I*I)/MASSI(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!#######################################################################
!--- Sets up lookup table for calculating initial ice crystal growth ---
!#######################################################################
!
      SUBROUTINE MY_GROWTH_RATES (DTPH)
!
!--- Below are tabulated values for the predicted mass of ice crystals
!    after 600 s of growth in water saturated conditions, based on 
!    calculations from Miller and Young (JAS, 1979).  These values are
!    crudely estimated from tabulated curves at 600 s from Fig. 6.9 of
!    Young (1993).  Values at temperatures colder than -27C were 
!    assumed to be invariant with temperature.  
!
!--- Used to normalize Miller & Young (1979) calculations of ice growth
!    over large time steps using their tabulated values at 600 s.
!    Assumes 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!
      integer, parameter :: MY_T1=1, MY_T2=35
      COMMON /CMY600/ MY_GROWTH(MY_T1:MY_T2)
      REAL MY_GROWTH, MY_600(MY_T1:MY_T2)
!
      DATA MY_600 /
     & 5.5e-8, 1.4E-7, 2.8E-7, 6.E-7, 3.3E-6,     !  -1 to  -5 deg C
     & 2.E-6, 9.E-7, 8.8E-7, 8.2E-7, 9.4e-7,      !  -6 to -10 deg C
     & 1.2E-6, 1.85E-6, 5.5E-6, 1.5E-5, 1.7E-5,   ! -11 to -15 deg C
     & 1.5E-5, 1.E-5, 3.4E-6, 1.85E-6, 1.35E-6,   ! -16 to -20 deg C
     & 1.05E-6, 1.E-6, 9.5E-7, 9.0E-7, 9.5E-7,    ! -21 to -25 deg C
     & 9.5E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7,        ! -26 to -30 deg C
     & 9.E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7 /        ! -31 to -35 deg C
!
!-----------------------------------------------------------------------
!
      DT_ICE=(DTPH/600.)**1.5
      MY_GROWTH=DT_ICE*MY_600
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!#######################################################################
!-- Lookup tables for the saturation vapor pressure w/r/t water & ice --
!#######################################################################
!
      SUBROUTINE GPVS
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    GPVS        COMPUTE SATURATION VAPOR PRESSURE TABLE
C   AUTHOR: N PHILLIPS       W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF
C   TEMPERATURE FOR THE TABLE LOOKUP FUNCTION FPVS.
C   EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
C   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH
C   OF 7501 FOR TEMPERATURES RANGING FROM 180.0 TO 330.0 KELVIN.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL
C   94-12-30  IREDELL             EXPAND TABLE
C   96-02-19  HONG                ICE EFFECT
C
C USAGE:  CALL GPVS
C
C SUBPROGRAMS CALLED:
C   (FPVSX)  - INLINABLE FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
C
C COMMON BLOCKS:
C   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS(NX),TBPVS0(NX)
      COMMON/COMPVS0/ C1XPVS0,C2XPVS0,TBPVS0
      COMMON/COMPVS/ C1XPVS,C2XPVS,TBPVS
C----------------------------------------------------------------------
      XMIN=180.0
      XMAX=330.0
      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC
C
      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO
C 
      RETURN
      END
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
                           FUNCTION FPVS(T)
C-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    FPVS        COMPUTE SATURATION VAPOR PRESSURE
C   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
C   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE
C   COMPUTED IN GPVS. SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
C   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
C   THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
C   ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
C   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
C   94-12-30  IREDELL             EXPAND TABLE
C   96-02-19  HONG                ICE EFFECT
C
C USAGE:   PVS=FPVS(T)
C
C   INPUT ARGUMENT LIST:
C     T        - REAL TEMPERATURE IN KELVIN
C
C   OUTPUT ARGUMENT LIST:
C     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
C
C COMMON BLOCKS:
C   COMPVS   - SCALING PARAMETERS AND TABLE COMPUTED IN GPVS.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C-----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS(NX)
      COMMON/COMPVS/ C1XPVS,C2XPVS,TBPVS
C-----------------------------------------------------------------------
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                         FUNCTION FPVS0(T)
C-----------------------------------------------------------------------
      PARAMETER(NX=7501)
      REAL TBPVS0(NX)
      COMMON/COMPVS0/ C1XPVS0,C2XPVS0,TBPVS0
C-----------------------------------------------------------------------
      XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
      JX1=MIN(XJ1,NX-1.)
      FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))
C
      RETURN
      END
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
                         FUNCTION FPVSX(T)
C-----------------------------------------------------------------------
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    FPVSX       COMPUTE SATURATION VAPOR PRESSURE
C   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
C
C ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
C   THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS
C   FOR GAS AND LIQUID, AND NEGLECTS THE VOLUME OF THE LIQUID.
C   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT
C   OF CONDENSATION WITH TEMPERATURE.  THE ICE OPTION IS NOT INCLUDED.
C   THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT
C   TO GET THE FORMULA
C       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))
C   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS
C   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
C
C PROGRAM HISTORY LOG:
C   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
C   94-12-30  IREDELL             EXACT COMPUTATION
C   96-02-19  HONG                ICE EFFECT 
C
C USAGE:   PVS=FPVSX(T)
C REFERENCE:   EMANUEL(1994),116-117
C
C   INPUT ARGUMENT LIST:
C     T        - REAL TEMPERATURE IN KELVIN
C
C   OUTPUT ARGUMENT LIST:
C     FPVSX    - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C-----------------------------------------------------------------------
      PARAMETER(CP=1.0046E+3,RD=287.04,RV=4.6150E+2 
     1,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2
     2,         CLIQ=4.1855E+3,CVAP= 1.8460E+3
     3,         CICE=2.1060E+3,HSUB=2.8340E+6)
      PARAMETER(PSATK=PSAT*1.E-3)
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))
      PARAMETER(DLDTI=CVAP-CICE,XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP))
C-----------------------------------------------------------------------
      TR=TTP/T
C
      IF(T.GE.TTP)THEN
        FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
      ELSE
        FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
      ENDIF
C 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                        FUNCTION FPVSX0(T)
C-----------------------------------------------------------------------
      PARAMETER(CP=1.0046E+3,RD=287.04,RV=4.6150E+2 
     1,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2
     2,         CLIQ=4.1855E+3,CVAP=1.8460E+3 
     3,         CICE=2.1060E+3,HSUB=2.8340E+6)
      PARAMETER(PSATK=PSAT*1.E-3)
      PARAMETER(DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP))
      PARAMETER(DLDTI=CVAP-CICE,XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP))
C-----------------------------------------------------------------------
      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
C
      RETURN
      END
