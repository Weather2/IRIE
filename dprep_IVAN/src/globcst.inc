c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                     GLOBCST.INC                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c#######################################################################
c
c     PURPOSE:
c
c     Include file 'globcst.inc' for ARPS 
c
c     This file contains most of the model run control parameters
c     and parameters used by various physics packages.
c
c     These parameters are allocated in named common blocks and are 
c     accessible to subroutines that include this file.
c
c#######################################################################
c 
c     AUTHOR: Ming Xue
c     10/01/1991
c
c     MODIFICATION HISTORY:
c
c     5/03/1992 (M. Xue and K. Droegemeier)
c       Major restructuring. Some parameter names changed.
c
c     7/21/1992 (M. Xue)
c       Added parameters for energy statistics calculations, initial 
c       random perturbation option, model run controls and additional 
c       history dump format.
c
c     3/10/1993 (M. Xue)
c       Added parameters for advection options, and for terrain
c       specifications.
c
c     2/12/94 (Y. Liu)
c       Added parameters for the surface energy budget model.
c
c     9/10/94 (Weygandt & Y. Lu)
c       Cleaned up documentation.
c
c     1/28/95 (G. Bassett)
c       Added new parameter, buoyopt
c
c     02/07/1995 (Yuhe Liu)
c       Deleted the table data array, veg(14) and added an input
c       parameter, veg0.
c
c     2/2/96 (Donghai Wang & yuhe Liu)
c       Added parameters for the map projection factor.
c
c     03/26/96 (Yuhe Liu)
c       Added parameters for radiation computing.
c
c     4/2/96 (Donghai Wang, X. Song and M. Xue)
c       Added parameters for the vertically implicit mixing.
c
c     5/7/96 (Donghai Wang and M. Xue)
c        Added a new option for Rayleigh damping.
c
c     10/21/97 (Donghai Wang)
c        Added two new options, buoy2nd and rhofctopt.
c
c#######################################################################
c
      integer mgrdmax         ! Maximum of mgrid
      parameter ( mgrdmax = 100 )

      integer mgrid           ! Grid identifier
      integer nestgrd         ! Nested grid option
                              ! = 0, no grid nesting
                              ! = 1, grid nesting 
      common /arpsc001/ mgrid, nestgrd
c
c#######################################################################
c
c     runname: A character string containing the pertinent information
c     for this run. The initial characters before a blank space, a comma
c     or otherwise the first 6 characters will be used to construct
c     history data and restart data dump file names.  This string is
c     written into the output data file.
c
c#######################################################################
c
      character runname*80    ! Name of this run
      integer   lfnkey        ! Length of string (no. of characters) 
                              ! to be used for naming files (file name 
                              ! keys).

      integer nocmnt          ! Number of comment lines
      character cmnt(50)*80   ! String of comments on this model run

      common /arpsc010/ runname, cmnt
      common /arpsc011/ lfnkey, nocmnt
c
c#######################################################################
c
c      Model initialization control parameters. 
c
c#######################################################################
c
      integer runmod          ! Model configuration option.
                              ! = 1, model run in 3-D mode 
                              ! = 2, model run in 2-D mode (x-z plane) 
                              ! = 3, model run in 2-D mode (y-z plane) 
                              ! = 4, model run in 1-D mode (z direction) 

      common /arpsc020/ runmod
c
c#######################################################################
c
c     Model initialization control parameters. 
c
c#######################################################################
c
      integer initopt         ! Model initialization option.
                              ! = 1, self initialization 
                              !      (e.g. analytical field).
                              ! = 2, restart run, initialize from a 
                              !      restart file. 
                              ! = 3, initialize from an external data 
                              !      set.

      integer timeopt         ! Option to check the consistence between
                              ! the user specified time and data time
                              ! = 1, check, but continue with the
                              !      specified time (see initime and tstart)
                              ! = 2, check, but continue with data time
                              ! else, check and stop for inconsistence

      integer inibasopt       ! Initialization option for base state 
                              ! fields.
                              ! = 1, Initialize the base state using a
                              !      single sounding.
                              ! = 2, isentropic atmosphere
                              ! = 3, isothermal atmosphere
                              ! = 4, atmosphere with a constant static
                              !      stability.
                              ! = 5, an analytical thermodynamic 
                              !      sounding profile after Weisman and
                              !      Klemp (1982, WMR)
                              ! For options rather than 1, additional
                              ! parameters are hardwired inside 
                              ! subroutine INIBASE.

      integer viniopt         ! Initialization option for base state
                              ! velocity when inibasopt.ne.1.
                              ! = 1, ubar= ubar0, vbar= vbar0
                              ! = 2, ubar0 and vbar0 are specified by user 
                              !      inside code. By default, ubar0=vbar0=0.0.

      integer soilinitopt     ! Initialization option for soil variables
                              ! = 1, integrate soil model using initial
                              !      initial atmospheric forcing

      integer pt0opt          ! Initial potential temperature 
                              ! perturbation  
                              ! option for initopt=1.
                              ! = 1, initial perturbation is 
                              !      bubble-shaped,
                              ! = 2, initial perturbation has a random 
                              !      distribution.
                              ! = 3, random initial perturbation symmetric 
                              !      about x and y axis. 
                              ! = 4, initial perturbation as given in 
                              !      Skamarock and Klemp (1994)

      integer inifmt          ! Format option for the history dump type 
                              ! restart file
                              ! See hdmpfmt.
      character inifile*80 
                              ! Filename of t-dependent variable 
                              ! initialization for initopt=3. 
      character inigbf*80 
                              ! Filename of base&grid variable 
                              ! initialization for initopt=3. 
      integer inifunt         ! Fortran I/O unit number for initial 
                              ! data input.

      integer buoyopt         ! Flag for turning off buoyancy terms
                              ! (buoyopt = 0 dissables buoyancy).

      integer buoy2nd         ! Option for the second order terms in 
                              ! the linerized buoyancy terms
                              ! = 1, including the 2nd-order terms 
                              ! = 0, only the 1st-order terms 

      integer rhofctopt       ! Option for removing the density approximation
                              ! in the pressure gradient force(PGF) terms
                              ! = 1, removing the approximation
                              ! = 0, using the approximation 

      integer bsnesq          ! Bousinessq approximation?
                              ! = 1, yes
                              ! = 0, no

      common /arpsc030/ initopt,inibasopt,viniopt,pt0opt,inifunt,
     :                  inifmt,soilinitopt,timeopt
      common /arpsc031/ inifile,inigbf,buoyopt,buoy2nd,rhofctopt,
     :                  bsnesq

      real soiltintv          ! Time interval in soil model initialization
                              ! integration

      real ptpert0            ! The magnitude of initial potential 
                              ! temperature perturbation (K)

      real pt0radx            ! The radius of bubble in x-direction. 
      real pt0rady            ! The radius of bubble in y-direction.
      real pt0radz            ! The radius of bubble in z-direction.
      real pt0ctrx            ! The x coordinate of the bubble center.
      real pt0ctry            ! The y coordinate of the bubble center.
      real pt0ctrz            ! The z coordinate of the bubble center.

      real ubar0              ! User specified base state wind in x-dir.
      real vbar0              ! User specified base state wind in y-dir.

      common /arpsc040/ ptpert0, pt0radx, pt0rady, pt0radz,
     :                  pt0ctrx, pt0ctry, pt0ctrz, ubar0,vbar0,
     :                  soiltintv
c
c#######################################################################
c
c     Coordinate transforamtion and terrain option parameters:
c
c#######################################################################
c
      integer crdtrns         ! Coordinate transformation option
                              ! = 0, no coordinate transforamtion at all
                              ! = 1, with coordinate transforamtion.

      integer ternopt         ! Model terrain option.
                              ! = 0, no terrain, the ground is flat;
                              ! = 1, bell-shaped mountain;
                              ! = 2, terrain data read from terrain data 
                              !      base (not implemented yet)
      integer mntopt          ! Option for choosing idealized mountain 
                              ! type.
      real    hmount          ! The mountain height (m)
      real    mntwidx         ! The half-width of bell-shaped mountain 
                              ! in x-dir.
      real    mntwidy         ! The half-width of bell-shaped mountain 
                              ! in y-dir.
      real    mntctrx         ! The x coordinate of the bell-shaped 
                              ! mountain center.
      real    mntctry         ! The y coordinate of the bell-shaped 
                              ! mountain center.
      character terndta*80    ! Name of the terrain data file

      common /arpsc045/ crdtrns, ternopt, mntopt
      common /arpsc046/ hmount, mntwidx, mntwidy, mntctrx, mntctry
      common /arpsc047/ terndta
c
c#######################################################################
c
c     Environmental sounding file name and Fortran I/O input unit.
c
c#######################################################################
c
      character sndfile*80    ! Name of the input sounding file.
      integer sfunit          ! Fortran I/O unit used by sounding input.

      common /arpsc050/ sndfile
      common /arpsc051/ sfunit
c
c#######################################################################
c
c     Model grid parameters.
c
c#######################################################################
c
      real dx                 ! Grid spacing in x-direction in 
                              ! computational as well as in physical 
                              ! space (m)
      real dy                 ! Grid spacing in y-direction in 
                              ! computational as well as in physical 
                              ! space (m)

      integer strhopt         ! Vertical grid stretching option.
                              ! = 0, no stretching in vectical.
                              ! = 1, vertical stretching with function 
                              !      dzp=a+b*k**3 being used to specify 
                              !      dz as a function of k.
                              ! = 2, vertical stretching with function 
                              !      dzp=c+a*tanh(b*k) being used to 
                              !      specify dz as a function of k.

      real dz                 ! Averaged grid spacing in vertical 
                              ! direction in transformed computataional 
                              ! space (m).
      real dzmin              ! Minimun grid spacing in vertical 
                              ! direction in physcal space (m). 
      real zrefsfc            ! The reference height of the surface 
                              ! (ground level) (m)

      real dlayer1            ! The depth of the lower layer with uniform 
                              ! (dz=dzmin) vertical spacing (m)

      real dlayer2            ! The depth of the mid layer with stetched 
                              ! vertical spacing (m)

      real strhtune           ! Tuning parameter for stretching option 2
                              ! A Value between 0.2 and 5.0 is appropriate.
                              ! A larger value gives a more linear stretching.

      real zflat              ! The height at which the grid levels 
                              ! becomes flat in the terrain-following
                              ! coordinate transformation (m).

      real dxinv              ! Reciprocal of dx (i.e. 1/dx)
      real dyinv              ! Reciprocal of dy (i.e. 1/dy)
      real dzinv              ! Reciprocal of dz (i.e. 1/dz)

      common /arpsc060/ strhopt
      common /arpsc061/ dx, dy, dz, dzmin, dxinv, dyinv, dzinv 
      common /arpsc062/ zrefsfc,dlayer1,dlayer2,strhtune,zflat
 
      real xl                 ! Length of the physical model domain in 
                              ! x-direction (m). In uniform spacing case
                              ! ,xl = (nx-3)*dx. 
      real yl                 ! Length of the physical model domain in 
                              ! y-direction (m). In uniform spacing case
                              ! ,yl = (ny-3)*dy.
      real zh                 ! Length of the physical model domain in 
                              ! z-direction (m). In uniform spacing case
                              ! , zh = (nz-3)*dz.

      common /arpsc070/ xl, yl, zh
 
      real ctrlat             ! Latitude of the southwest corner of the
                              ! physical model domain (deg. N)
      real ctrlon             ! Longitude of the southwest corner of the
                              ! physical model domain (deg. E)
      real xorig              ! x-coordinate origin of the model 
                              ! physical domain 
      real yorig              ! y-coordinate origin of the model 
                              ! physical domain 
      real zorig              ! z-coordinate origin of the model 
                              ! physical domain 

      common /arpsc071/ ctrlat, ctrlon, xorig, yorig, zorig

      real xgrdorg            ! x-coordinate of model grid in 
                              ! absolute space
      real ygrdorg            ! y-coordinate of model grid in 
                              ! absolute space
      common /arpsc075/ xgrdorg, ygrdorg 
c
c#######################################################################
c
c     Options for solution used in w equation
c
c     Acoustic/sound wave speed definition used in the pressure  
c     equation. 
c
c#######################################################################
c
      integer vimplct         ! Vertical implicit option for 
                              ! the w and p equations.
                              ! = 1, implicit solution
                              ! = 0, explicit solution

      integer ptsmlstp        ! Option to solve potential temperature 
                              ! eqaution.
                              ! = 0, solve potential temperature eq.
                              !      outside small time steps
                              ! = 1, solve potential temperature eq.
                              !      within small time steps

      integer peqopt          ! Option parameter of p-equation formulations
                              ! = 1 or 2 (1 is as in 4.0 User's guide).


      integer csopt           ! Sound wave speed option used in the  
                              ! pressure equation. 
                              ! = 1, csound = cp/cv * rd * T
                              ! = 2, csound = cp/cv * rd * T * csfactr
                              ! = 3, csound = specfied constant. 
      real    csfactr         ! A multiplication factor for the sound 
                              ! speed.
      real    csound          ! User specified constant sound speed. 

      common /arpsc080/ vimplct, ptsmlstp,csopt, csfactr, csound,peqopt
c
c#######################################################################
c
c     Model integration time step and integration control parameters.
c
c#######################################################################
c
      real dtbig              ! Model integration time step.
      real dtsml              ! Small time step for the acoustic wave 
                              ! integrations.

      real    tstart          ! Time when the model is initialized. 
      real    tstop           ! Time when the integration is to be 
                              ! stoped.
      real    curtim          ! Current model time.
      real    tacoef          ! Time average weighting coefficient used
                              ! in the vertically implicit solver.

      integer nstep           ! Current number of time steps taken.
      integer nsteps          ! The total number of time steps in 
                              ! this run.
      integer nsmstp          ! The number of small acoustic wave time 
                              ! integration steps per large time step.

      common /arpsc090/ dtbig, tstart, tstop, curtim, dtsml, tacoef
      common /arpsc100/ nstep, nsteps, nsmstp
c
c#######################################################################
c
c     Parameters of the reference calendar day and real time
c
c#######################################################################
c
      integer year            ! Year of the reference calendar day
      integer month           ! Month of the reference calendar day
      integer day             ! Day of the reference calendar day
      integer hour            ! Hours of the reference real time
      integer minute          ! Minutes of the reference real time
      integer second          ! Seconds of the reference real time

      integer jday            ! Julian day starting from Jan. 1 of the 
                              ! year

      common /arpsc101/ year, month, day, hour, minute, second, jday
c
c#######################################################################
c
c     Robert-Asselin time filter coefficient for leapfrog time 
c     integration:
c
c#######################################################################
c
      real flteps             ! Robert-Asselin time filter coefficient 
                              ! (non-dimensional).

      common /arpsc110/ flteps
c
c#######################################################################
c
c     Option parameters for momentum and scalar advections. 
c
c#######################################################################
c
      integer madvopt         ! Option parameter for momentum advection
                              ! = 1, 2nd order centerd;
                              ! = 2, 4th order centerd;
                              ! = 3, ... 

      integer sadvopt         ! Option parameter for scalar advection
                              ! = 1, 2nd order centerd;
                              ! = 2, 4th order centerd;
                              ! = 3, Flux-corrected transport (FCT)

      integer fctorderopt     ! Option of the spactial order of accuracy of 
                              ! the FCT advection (sadvopt=4) and MPDCD 
                              ! advection schemes (sadvopt=5)
                              ! = 1  2nd order
                              ! = 2  4th order
 
      integer fctadvptprt     ! Option for FCT advection for potential temperature. 
                              ! Used only when sadvopt=4.
                              ! = 0, FCT scheme is applied to ptbar+ptprt.
                              ! = 1, FCT scheme is applied to ptprt only.
                              ! = 2, FCT scheme is applied to 
                              !      ptbar+ptprt-min(ptbar+ptprt).

      common /arpsc111/ madvopt, sadvopt, fctorderopt, fctadvptprt
c
c#######################################################################
c
c     Subgrid-scale turbulent mixing parameters.
c
c#######################################################################
c
      integer tmixopt         ! Control parameter for turbulent mixing 
                              ! options.
                              ! = 0, zero turbulent mixing
                              ! = 1, constant mixing coefficient, 
                              !      km = tmixcst
                              ! = 2, Smagorinsky mixing coefficient.
                              ! = 3, Smagorinsky mixing coefficient 
                              !      plus a constant coeffcient, tmixcst
      integer trbisotp        ! Option for isotropic subgrid scale 
                              ! turbulence.
                              ! = 0, the turbulence is assumed to be 
                              !      anisotropic;
                              ! = 1, the turbulence is assumed to be 
                              !      isotropic.
      integer tkeopt          ! Option for 1.5 order TKE formulation
                              ! used by tmixopt=4
                              ! = 1, after Wyngaard;
                              ! = 2, after Deardroff;
                              ! = 3, after Sun and Chang.
      integer trbvimp         ! Option parameter for the vertically
                              ! implicit mixing
                              ! = 0, vertical explicit
                              ! = 1, vertical implicit
      integer tmixvert        ! Option parameter for the computing
                              ! only the vertical turb. mixing terms
                              ! = 0, all terms computed
                              ! = 1, only vertical terms computed.

      real alfcoef            ! Time average weighting coefficient used
                              ! in the vertically implicit mixing

      real prantl             ! Turbulent prantl number for Smagorinsky turb.
      real tmixcst            ! Value of the constant mixing coefficient

      real kmlimit            ! Constant used to set a limit of km
                              !
 
      common /arpsc120/ tmixopt,trbisotp,tkeopt,trbvimp,tmixvert
      common /arpsc121/ prantl, tmixcst, kmlimit,alfcoef
c
c#######################################################################
c
c     Computational mixing parameters.
c
c#######################################################################
c
      integer cmix2nd         ! 2nd order computational mixing option.
                              ! = 0, mixing off 
                              ! = 1, mixing on 
      real cfcm2h             ! 2nd order horizontal computational mixing 
                              ! coefficient caled by horizontal grid 
                              ! spacing (1/s).
      real cfcm2v             ! 2nd order vertical computational mixing 
                              ! coefficient scaled by vertical grid 
                              ! spacing (1/s).
      real cfcmh2             ! Dimensional 2nd order horizontal 
                              ! computational mixing coefficient (m**2/s)
      real cfcmv2             ! Dimensional 2nd order vertical 
                              ! computational mixing coefficient (m**2/s)

      integer cmix4th         ! 4th order computational mixing option
                              ! = 0, mixing off
                              ! = 1, mixing on
      real cfcm4h             ! 4th order horizontal computational mixing 
                              ! coefficient scaled by horizontal grid 
                              ! spacing (1/s).
      real cfcm4v             ! 4th order vertical computational mixing 
                              ! coefficient scaled by vertical grid 
                              ! spacing (1/s).
      real cfcmh4             ! Dimensional 4th order horizontal 
                              ! computational mixing coefficient (m**2/s)
      real cfcmv4             ! Dimensional 4th order vertical 
                              ! computational mixing coefficient (m**2/s)
      real scmixfctr          ! Reduction factor of the computational 
                              ! mixing coefficients for scalars relative to
                              ! those of velocities.

      common /arpsc130/ cmix2nd, cmix4th
      common /arpsc131/ cfcm2h, cfcm2v, cfcm4h, cfcm4v
      common /arpsc132/ cfcmh2, cfcmv2, cfcmh4, cfcmv4
      common /arpsc133/ scmixfctr 
c
c#######################################################################
c
c     Upper level Rayleigh damping parameters.
c
c#######################################################################
c
      integer raydmp          ! Rayleigh damping option.
                              ! = 0, damping off.
                              ! = 1, damping the difference between the
                              !      total and base state fields.
                              ! = 2, damping the difference between the
                              !      total and external fields defined
                              !      in the EXBC file.
      real zbrdmp             ! Height of the bottom of Rayleigh damping 
                              ! (sponge) layer (m).
      real cfrdmp             ! Rayleigh damping coefficient (1/second).
 
      common /arpsc140/ raydmp, zbrdmp, cfrdmp
c
c#######################################################################
c
c     Acoustic wave divergence damping parameters.       
c
c#######################################################################
c
      integer divdmp          ! Acoustic wave divergence damping option. 
                              ! = 0, divergence damping off 
                              ! = 1, divergence damping on
      real cdvdmph            ! Divergence damping coefficient (m**2/s)
                              ! in horizontal
      real cdvdmpv            ! Divergence damping coefficient (m**2/s)
                              ! in vertical
      real divdmpndh          ! Non-dimensional divergence damping 
                              ! coefficient in horizontal
      real divdmpndv          ! Non-dimensional divergence damping 
                              ! coefficient in vertical
 
      common /arpsc150/ divdmp
      common /arpsc151/ cdvdmph,cdvdmpv, divdmpndh,divdmpndv
c
c#######################################################################
c
c     Moisture/ice microphysics parameters. 
c
c#######################################################################
c
      integer mphyopt         ! Microphysics option. 
                              ! = 0, no microphysics. 
                              ! = 1, Kessler warm rain microphysics.
                              ! = 2, Ice microphysics. 

      integer moist           ! Moist processes option. 
                              ! = 0, dry run, all processes 
                              !      related to moisture are turned off
                              ! = 1, moist processes are activated

      integer cnvctopt        ! Option for convective cumulus
                              ! parameterization
                              ! = 0, no convective parameterization
                              ! = 1, Kuo scheme
                              ! = 2, Kuo scheme and warm rain microphysics
                              ! = 3, Kain and Fristch
      integer ice             ! Ice microphysics option. 
                              ! = 0, ice processes are turned off 
                              ! = 1, ice processes are activated.
 
      common /arpsc160/ mphyopt, moist, cnvctopt, ice
c
c#######################################################################
c
c     Cumulus physics parameters.
c
c#######################################################################
c
      real kffbfct            ! A feedback factor for K-F scheme.
      real wcldbs             ! Vertical motion needed at cloud base for
                              ! convection.

      real confrq             ! Frequency of conv param. updates in sec.

      real qpfgfrq            ! Frequency of grid param. updates in sec.

      integer idownd          ! Downdraft flag
                              ! = 0, no downdrafts.
                              ! = 1, simple downdraft model.

      common /arpsc161/ kffbfct,wcldbs,confrq,qpfgfrq,idownd

c
c#######################################################################
c
c     Microphysics constants.
c
c#######################################################################
c
      real autotr             ! Cloud water mixing ratio threshold for 
                              ! autoconversion from cloud to rain water 
                              ! (kg/kg). 
      parameter ( autotr = 1.0e-3 )

      real autort             ! Autoconversion rate (1/seconds). 
      parameter ( autort = 1.0E-3 )

      real accrrt             ! Accretion rate (1/seconds). 
      parameter ( accrrt = 2.2 )

      real rho0               ! Reference density (kg/meter**3). )
      parameter ( rho0 = 1.225 )
  
c
c#######################################################################
c
c     Saturation specific humidity parameters used in enhanced Teten's
c     formula. (See A. Buck, JAM 1981)
c
c#######################################################################
c
      real satfwa, satfwb
      parameter ( satfwa = 1.0007 )
      parameter ( satfwb = 3.46e-8 )  ! for p in Pa

      real satewa, satewb, satewc
      parameter ( satewa = 611.21 )   ! es in Pa
      parameter ( satewb = 17.502 )
      parameter ( satewc = 32.18 )

      real satfia, satfib
      parameter ( satfia = 1.0003 )
      parameter ( satfib = 4.18e-8 )  ! for p in Pa

      real sateia, sateib, sateic
      parameter ( sateia = 611.15 )   ! es in Pa
      parameter ( sateib = 22.452 )
      parameter ( sateic = 0.6 )
c
c#######################################################################
c
c     The x and y component of the domain translation speed 
c     (ground-relative).
c
c#######################################################################
c
      real umove              ! Model domain translation speed in x 
                              ! direction.
      real vmove              ! Model domain translation speed in y 
                              ! direction.
      common /arpsc170/ umove, vmove 
c
c#######################################################################
c
c     Surface physics parameters. 
c
c#######################################################################
c
      integer sfcphy          ! Surface physics option. 
                              ! = 0, no surface physics parameterization
                              ! = 1, simple bulk aerodynamic formulation 
      integer landwtr         ! Flag to indicate if land and water will be 
                              ! distinquished when the surface roughness 
                              ! length is calculated.  If yes,the 
                              ! vagetation data (in sfcdtfl) has to be 
                              ! read in for all active options of sfcopt
                              ! = 0, no distinquishing.
                              ! = 1, surface roughness length is
                              !      calculated differently for land 
                              !      and water.

      integer  cdhwtropt      ! Option to set constant value of cdh
                              ! (cdq) over water.

      integer  wtrexist       ! Flag to check if any grid point is
                              ! covered by water.

 
      real cdmlnd             ! Bulk aerodynamic momentum drag 
                              ! coefficient. 
      real cdmwtr             ! Bulk aerodynamic momentum drag 
                              ! coefficient. 
      real cdhlnd             ! Bulk aerodynamic heat flux drag 
                              ! coefficient.  
      real cdhwtr             ! Bulk aerodynamic heat flux drag 
                              ! coefficient.  
      real cdqlnd             ! Bulk aerodynamic moisture flux drag 
                              ! coefficient.  
      real cdqwtr             ! Bulk aerodynamic moisture flux drag 
                              ! coefficient.  
                              ! cdm, cdh and cdq are all non-dimensional
      integer pbldopt         ! Option for determining PBL depth.
      real    pbldpth0        ! User-specified PBL depth.
      real    lsclpbl0        ! Length scale
      integer sflxdis         ! Option for distributing fluxes in PBL
                              ! = 0, without flux distribution;
                              ! = 1, with flux distribution for heat,
                              !      moist, and momentum;
                              ! = 2, with flux distribution for heat, moist.
      integer tqflxdis        ! Option for distributing heat and moisture 
                              ! fluxes qudratically in a specified
                              ! depth dtqflxdis 
                              ! = 0, no distribution;
                              ! = 1, with distribution
      real dtqflxdis          ! Depth of flux distribution for tqflxdis=1
                              ! 200 m recommended.

      integer smthflx         ! Option to smooth surface fluxes
                              ! = 0, no smoothing
                              ! = 1, smoothing
      integer numsmth         ! Number of smoothing passes
                              ! >=1 when smthflx = 1

      integer sfcdiag         ! Flag of surface diagnotic calculation

      common /arpsc180/ sfcphy,landwtr,cdhwtropt,sfcdiag,pbldopt,
     :                  wtrexist,sflxdis,tqflxdis,smthflx,numsmth
      common /arpsc181/ cdmlnd,cdmwtr, cdhlnd,cdhwtr, 
     :                  cdqlnd,cdqwtr,pbldpth0,lsclpbl0,dtqflxdis
 
      integer sfcdat          ! Surface data input flag.
      integer soilinit        ! Soil model varialble initialization flag.
      integer sfcunit         ! Fortran I/O unit number for surface data 
                              ! input.
      real dtsfc              ! Time step for surface energy budget 
                              ! model
      integer nsfcst          ! Number  of steps for surface energy 
                              ! budget model

      common /arpsc182/ sfcdat, soilinit, sfcunit, nsfcst
      common /arpsc183/ dtsfc

      integer nstyp           ! Number of soil types
      integer styp            ! Soil type
      integer vtyp            ! Vegetation type
      real lai0               ! Leaf Area Index
      real roufns0            ! Surface roughness
      real veg0               ! Vegetation fraction

      real ptslnd0            ! Initial ground-level soil potential
                              ! temperature over land
      real ptswtr0            ! Initial ground-level soil potential
                              ! temperature over water
      real tsoil0             ! Initial deep soil temperature
      real wetsfc0            ! Initial surface soil moisture
      real wetdp0             ! Initial deep soil moisture
      real wetcanp0           ! Initial canopy moisture
      integer snowcvr0        ! Snow cover

      real tsprt              ! Offset of tsfc from sfc air temperature
      real t2prt              ! Offset of tsoil from sfc air temperature
      real wgrat              ! Saturation rate of sfc soil moisture
      real w2rat              ! Saturation rate of deep soil moisture

      common /arpsc184/ nstyp,styp,vtyp
      common /arpsc185/ lai0,roufns0,veg0, ptslnd0,ptswtr0,tsoil0,
     :                  wetsfc0,wetdp0,wetcanp0,snowcvr0,
     :                  tsprt,t2prt,wgrat,w2rat

      character sfcdtfl*80    ! File name of surface data.
      character soilinfl*80   ! File name of surface initialization data.

      common /arpsc186/ sfcdtfl, soilinfl
c
c#######################################################################
c
c     Coriolis parameters
c
c#######################################################################
c
      integer coriopt         ! Coriolis term option.
                              ! = 0, no coriolis terms 
                              ! = 1, coriolis terms involving horizontal
                              !      wind 
                              ! = 2, coriolis terms involving both 
                              !      horizontal and vertical wind 

      integer coriotrm        ! A flag indicating if total or perturbation
                              ! wind is used in the Coriolis terms 
                              ! in the horizontal momemtum equations.  
                              ! = 1, total wind,
                              ! = 2, perturbation wind.

      real latitud            ! Model center latitude (degrees). 
      real longitud           ! Model center longitude (degrees). 

      common /arpsc190/ coriopt,coriotrm
      common /arpsc191/ latitud, longitud

c
c#######################################################################
c
c     The name of the directory into which output files will be written:
c
c#######################################################################
c
      character dirname*80    ! The name of output directory
      integer   ldirnam       ! The length of the directory name string

      common /arpsc200/ ldirnam
      common /arpsc201/ dirname
c
c#######################################################################
c
c     Model output parameters. 
c
c#######################################################################
c
      integer nfmtprt         ! time steps    between formatted 
                              !               print out
      real    tfmtprt         ! time interval between formatted 
                              !               print out

      integer nmaxmin         ! time steps    between max/min 
                              !               statistics calc.
      real    tmaxmin         ! time interval between max/min 
                              !               statistics calc.

      integer nenergy         ! time steps    between energy 
                              !               statistics calc.
      real    tenergy         ! time interval between energy 
                              !               statistics calc.

      integer nrstout         ! time steps    between restart data dumps
      real    trstout         ! time interval between restart data dumps

      common /arpsc210/ nfmtprt, nmaxmin, nenergy, nrstout
      common /arpsc220/ tfmtprt, tmaxmin, tenergy, trstout
 
      integer totout          ! Flag to indicate that the history dump
                              ! contains total values instead of
                              ! perturbation
                              ! = 0, Perturbations
                              ! = 1, total values
 
      integer grdout          ! Grid output option.
                              ! = 0, no grid output 
                              ! = 1, grid output 
      integer basout          ! Base state field output option.
                              ! = 0, no base state fields output 
                              ! = 1, base state fields output 
      integer varout          ! Mass & perturbation wind output option. 
                              ! = 0, no mass or perturbation wind output  
                              ! = 1, mass & perturbation wind output 
      integer mstout          ! Moist variable output option.
                              ! = 0, no moisture variables output 
                              ! = 1, qv, qc, qr, qi, qs and qh output.
      integer rainout         ! Rain variable output option.
                              ! = 0, no rain variables output 
                              ! = 1, raing and rainc output.
      integer prcout          ! Precipitation rates output option.
                              ! = 0, no rain variables output 
                              ! = 1, prcrate output.
      integer iceout          ! Ice variable ouptu option.
                              ! = 0, no ice variables output 
                              ! = 1, qi, qs and qh output 
      integer tkeout          ! TKE output option.
                              ! = 0, no output
                              ! = 1, output  
      integer trbout          ! Turbulence field (km) output option.
                              ! = 0, km not output
                              ! = 1, km output  
      integer sfcout          ! Surface variable output option.
                              ! = 0, no surface variables output
                              ! = 1, tsfc,tsoil,wetsfc,wetdp,wetsfc output
      integer snowout         ! Snow cover output option
                              ! = 0, no snow cover output
                              ! = 1, snow cover output
      integer landout         ! Surface property array output option.
                              ! = 0, no surface property array output
                              ! = 1, soiltyp,vegtyp,lai,roufns,veg output
      integer radout          ! Radiation arrays output option.
                              ! = 0, no radiation arrays output
                              ! = 1, radfrc, radsw, rnflx output
      integer flxout          ! Surface fluxes output option.
                              ! = 0, no surface fluxes output
                              ! = 1, usflx, vsflx, ptsflx, qvsflx output

      integer exbcdmp         ! Flag to dump the ARPS fields into an
                              ! additional file to make the ARPS
                              ! external boundary data set.
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if lbcopt=2

      integer extdadmp        ! Flag to dump the fields that contains
                              ! external data array interpolated to the
                              ! current model time. When lbcopt.ne.2,
                              ! reset extdadmp to 0.
                              ! = 0, no dumping
                              ! = 1, dump.

      integer qcexout         ! Flag to dump qc into EXBC file
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if exbcdmp=1

      integer qrexout         ! Flag to dump qr into EXBC file
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if exbcdmp=1

      integer qiexout         ! Flag to dump qi into EXBC file
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if exbcdmp=1

      integer qsexout         ! Flag to dump qs into EXBC file
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if exbcdmp=1

      integer qhexout         ! Flag to dump qh into EXBC file
                              ! = 0, no EXBC dumping
                              ! = 1, EXBC dumping if exbcdmp=1

      common /arpsc230/ grdout, basout, varout, mstout, rainout,
     :                  prcout, iceout, tkeout, trbout, sfcout,
     :                  landout,radout, flxout, totout, snowout,
     :                  exbcdmp,extdadmp,
     :                  qcexout,qrexout,qiexout,qsexout,qhexout

c
c#######################################################################
c
c     History data dump parameters.
c
c#######################################################################
c
      integer hdmpopt         ! History data dump option.
                              ! = 1, dump with constant time interval
                              ! = 2, dump at the time specified by user

      integer hdmpfmt         ! History data dump format option.
                              ! = 0, No data dump is produced 
                              ! = 1, Unformatted binary data dump
                              ! = 2, Formatted ascii data dump
                              ! = 3, NCSA HDF format data dump
                              ! = 4, Compressed binary data dump 
                              ! = 5, Dump for Savi3D visualization 
                              !      package 
                              ! = 6, binary allowing data point skip
                              ! = 7, Uncompressed NetCDF format dump. 
                              ! = 8, Compressed NetCDF format dump.
                              ! = 9, GrADS data dump
                              ! = 10, GRIB data dump
 
      integer grbpkbit        ! Bit length to pack floating data
                              ! in GRIB format

      real thisdmp            ! time interval between history data dumps
      integer nhisdmp         ! time steps    between history data dumps

      real tstrtdmp           ! time to start history data dump
      integer nstrtdmp        ! time steps from beginning to tstrtdmp

      integer numhdmp         ! number of history dumps specified by user
      integer hdmpmax         ! maximum number of history dumps allowed
                              ! in user specifying option
      parameter ( hdmpmax = 100 )

      real hdmptim(hdmpmax)   ! times of model specified by user to
                              ! dump out history data
      integer hdmpstp(hdmpmax)
                              ! steps from beginning of model specified
                              ! by user to dump out history data
                              ! hdmpstp = nint( hdmptim/dtbig )

      integer    nchdmp       ! Fortran I/O channel for history 
                              ! data dump.
      character  hdmpfn*80    ! History data dump file name.
      integer    ldmpf        ! Length of the history data dump 
                              ! file name

      integer    istager      ! Flag to dump vectors on vector grids.
                              ! = 0, no stagering, dump on scalar grid
                              ! = 1, stagering, dump vectors on vectors grids

      common /arpsc240/ hdmpfn
      common /arpsc241/ ldmpf,nchdmp
      common /arpsc242/ grbpkbit,istager
      common /arpsc243/ hdmpopt,hdmpfmt,nhisdmp,nstrtdmp,numhdmp,hdmpstp
      common /arpsc244/ thisdmp,tstrtdmp,hdmptim
c
c#######################################################################
c
c     More output control parameters
c
c#######################################################################
c


      integer imgopt          ! HDF image dumping option, 0 or 1
      integer nimgdmp         ! time steps    between HDF image dumps
      real    timgdmp         ! time interval between HDF image dumps

      integer pltopt          ! Graphic plotting option, 0 or 1
      integer nplots          ! time steps    between graphic plotting 
                              !               calls
      real    tplots          ! time interval between graphic plotting 
                              !               calls
      integer filcmprs        ! Option for output file compression, 
                              ! 0 or 1.

      common /arpsc305/ imgopt, pltopt
      common /arpsc310/ nimgdmp, nplots
      common /arpsc320/ timgdmp, tplots
      common /arpsc321/ filcmprs
c
c#######################################################################
c
c     Parameters for grid translation.
c
c#######################################################################
c
      integer cltkopt         ! Option for cell tracking.
                              ! = 0, no cell-tracking,
                              ! = 1, cell-tracking is turned on.
      integer grdtrns         ! Domain translation option.
                              ! = 0, no change in domain translation.
                              ! = 1, domain translation based on cell 
                              !      tracking.
                              ! = 2, automatic domain translation via 
                              !      optimal procedure.
      real chkdpth            ! Depth (m) over which the optimal domain 
                              ! translation speed is calculated for the 
                              ! automatic domain translation.
      real twindow            ! Time window (s) between update of grid 
                              ! translation components for automatic 
                              ! domain translation.
      integer nceltrk         ! time steps    between cell tracking 
                              !               calls
      real    tceltrk         ! time interval between cell tracking 
                              !               calls
      real    tcrestr         ! time required to restore cell center
                              !               to domain center

      common /arpsc330/ cltkopt,grdtrns,nceltrk
      common /arpsc331/ chkdpth,twindow,tceltrk,tcrestr
c
c#######################################################################
c
c     Debug information printing level. 
c
c#######################################################################
c
      integer lvldbg          ! Level of debug information printing.
                              ! =0, no printing;
                              ! =1, Print variables in big t-step; 
                              ! =2, Print forcings in big t-step; 
                              ! =3, Print variables in small t-step; 
                              ! =4, Print forcings in small t-step; 
                              ! =5, Print individual forcing terms 
                              !     and misc. info;

      common /arpsc325/ lvldbg
c
c#######################################################################
c
c     Model restart data dump parameters. 
c
c#######################################################################
c
      integer restrt          ! Restart run option.
                              ! = 0, non-restart run;
                              ! = 1, restart run; 

      integer rstiunt         ! Fortran I/O unit for restart data read.
      character rstinf*80     ! File name of restart input data.

      integer rstount         ! Fortran I/O unit for restart data dump.
      character rstoutf*80    ! File name of restart data dump.
 
      common /arpsc260/ restrt, rstount, rstiunt
      common /arpsc270/ rstoutf
      common /arpsc271/ rstinf
c
c#######################################################################
c
c     The Fortran I/O channel (unit) used for max/min statistics output.
c
c#######################################################################
c
      integer nchmax          ! Fortran I/O channel for max/min 
                              ! statistics output.
      common /arpsc280/ nchmax 
c
c#######################################################################
c
c     The Fortran I/O channel (unit) used by energy statistics output 
c
c#######################################################################
c 
      integer ncheng          ! Fortran I/O channel for energy 
                              ! statistics output.
      common /arpsc290/ ncheng 
c
c#######################################################################
c
c     Parameters used by Savi3D. 
c
c#######################################################################
c
      integer grafh           ! Grafhandle used by savi3D
      integer gridh           ! Gridhandle used by savi3D
      common /arpsc300/ grafh, gridh
      integer dsindex
      integer gridid
      common /savicmn/dsindex, gridid

c
c#######################################################################
c
c     Parameters for map projections
c
c#######################################################################
c
      real    eradius         ! Mean earth radius in meters
      parameter ( eradius = 6371000. )
c
      integer mapproj         ! Type of map projection in the model 
                              ! grid:
                              ! modproj = 0  No projection. 
                              !         = 1  Polar Stereographic 
                              !              projection.
                              !         = 2  Mercator projection.
                              !         = 3  Lambert projection.

      real    trulat1         ! 1st real true latitude of map projection
      real    trulat2         ! 2nd real true latitude of map projection
      real    trulon          ! Real true longitude of map projection
      real    sclfct          ! Map scale factor.
                              ! Distance on map, between two latitudes 
                              ! trulat1 and trulat2, 
                              ! is = (Distance on earth)*sclfct
                              ! For ARPS model runs, 
                              ! generally this is 1.0

      integer mpfctopt        ! Option parameter for map factor
                              ! = 0, map factor set to 1
                              ! = 1, map factor calculated according to
                              !      mapproj

      integer mptrmopt        ! Option parameter for map factor terms in
                              ! momentum advection
                              ! = 0, ignore the terms
                              ! = 1, include the terms

      integer maptest         ! Option parameter for map factor testing
                              ! with a symmetric field.
                              ! = 0, no test
                              ! = 1, symmetric field applied

      real    swlatu          ! latitude  at southwest corner for u-grid
      real    swlonu          ! longitude at southwest corner for u-grid
      real    nelatu          ! latitude  at northeast corner for u-grid
      real    nelonu          ! longitude at northeast corner for u-grid

      real    swlatv          ! latitude  at southwest corner for v-grid
      real    swlonv          ! longitude at southwest corner for v-grid
      real    nelatv          ! latitude  at northeast corner for v-grid
      real    nelonv          ! longitude at northeast corner for v-grid

      real    swlats          ! latitude  at southwest corner for s-grid
      real    swlons          ! longitude at southwest corner for s-grid
      real    nelats          ! latitude  at northeast corner for s-grid
      real    nelons          ! longitude at northeast corner for s-grid

      common  /arps400/ mapproj, mpfctopt, mptrmopt, maptest
      common  /arps401/ trulat1, trulat2, trulon, sclfct
      common  /arps402/ swlatu, swlonu, nelatu, nelonu,
     :                  swlatv, swlonv, nelatv, nelonv,
     :                  swlats, swlons, nelats, nelons

c
c#######################################################################
c
c     Parameters for radiation
c
c#######################################################################
c
      integer radopt          ! Option to switch on/off radiation physics
                              ! = 0, No radiation physics;
                              ! = 1, Radiation physics;

      integer radstgr         ! Option to switch on/off radiation
                              ! computing at staggering points
                              ! = 0, No staggering
                              ! = 1, staggering

      integer rlwopt          ! Option for longwave schemes
                              ! = 0, for high = .false.
                              ! = 1, for high = .true.

      integer raddiag         ! Option to dump variables calculated in
                              ! radiation transfer equations
                              ! = 0, no dump
                              ! = 1, dump to GrADS format

      integer nradstp         ! Time steps to update radiation
                              ! calculation

      real dtrad              ! Time interval to update radiation
                              ! calculation

      real cldh2m             ! Height in meter that separates high cloud
                              ! and middle cloud
      real cldm2l             ! Height in meter that separates middle cloud
                              ! and low cloud

      integer ict             ! Vertical indices of height cldh2m
      integer icb             ! Vertical indices of height cldm2l

      parameter ( cldh2m = 5500.0, cldm2l = 3000.0 )   ! in meter

      common  /arps500/ radopt,radstgr, rlwopt,raddiag,nradstp,
     :                  ict,icb
      common  /arps501/ dtrad

      character arpsversion*20 ! A string containing the version number of ARPS.
      common /arps601/ arpsversion

c
c     Accumulated CPU time usage by various processes or terms in the
c     equations.
c
      real total_cpu_time, init_cpu_time,output_cpu_time,
     :     rad_cpu_time,sfcphy_cpu_time,soil_cpu_time,tinteg_cpu_time,
     :     tmix_cpu_time,cmix_cpu_time,raydmp_cpu_time,advuvw_cpu_time,
     :     advs_cpu_time,coriol_cpu_time,buoy_cpu_time,tkesrc_cpu_time,
     :     bc_cpu_time,qpfgrd_cpu_time,kuocum_cpu_time,kfcum_cpu_time,
     :     smlstp_cpu_time,warmrain_cpu_time,linice_cpu_time,
     :     nemice_cpu_time,qfall_cpu_time,misc_cpu_time

      common /arps701/ total_cpu_time, init_cpu_time,output_cpu_time,
     :     rad_cpu_time,sfcphy_cpu_time,soil_cpu_time,tinteg_cpu_time,
     :     tmix_cpu_time,cmix_cpu_time,raydmp_cpu_time,advuvw_cpu_time,
     :     advs_cpu_time,coriol_cpu_time,buoy_cpu_time,tkesrc_cpu_time,
     :     bc_cpu_time,qpfgrd_cpu_time,kuocum_cpu_time,kfcum_cpu_time,
     :     smlstp_cpu_time,warmrain_cpu_time,linice_cpu_time,
     :     nemice_cpu_time,qfall_cpu_time,misc_cpu_time
