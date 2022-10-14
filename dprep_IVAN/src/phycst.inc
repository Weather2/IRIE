c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                      PHYCST.INC                      ######
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
c     Include file 'phycst.inc' for ARPS 
c
c     This file initializes values of some physical constants
c     that remain unchanged throughout a model run
c
c#######################################################################
c
c     AUTHOR: Ming Xue
c     10/01/1991
c
c     MODIFICATION HISTORY:
c
c     9/10/94 (Weygandt & Y. Lu)
c     Cleaned up documentation.
c
c     2/24/97 (Jinxing Zong, Ming Xue and Yuhe Liu)
c     Added five pwr arrays and a common block TAOICECST for look-up 
c     tables to replace fractional power calculations in Tao ice scheme.
c
c#######################################################################
c
      real rd        ! Gas constant for dry air  (m**2/(s**2*K))
      parameter( rd     = 287.0 )

      real rv        ! Gas constant for water vapor  (m**2/(s**2*K)).
      parameter( rv     = 461.0 )

      real cv        ! Specific heat of dry air at constant volume 
                     ! (m**2/(s**2*K)).
      parameter( cv     = 717.0 )

      real cp        ! Specific heat of dry air at constant pressure
                     ! (m**2/(s**2*K)).
      parameter( cp     = 1004.0 )

      real cpdcv     ! cp/cv used in sound wave speed formula 
      parameter( cpdcv  = cp/cv )

      real rddcp   
      parameter( rddcp  = rd/cp )

      real rvdrd    
      parameter( rvdrd  = rv/rd )

      real rddrv    
      parameter( rddrv  = rd/rv )

      real lathv     ! Latent heat of vaporization at 0 degrees Celsius
                     ! (m**2/(s**2)).
      parameter( lathv  = 2.500780e6 )

      real latheatv(151)
      real pwr1364(0:1001), pwr875 (0:1001)
      real pwr525 (0:1001), pwr2046(0:1001)
      common /microcst/ latheatv, pwr1364, pwr525, pwr875, pwr2046

      real pwr81(0:151), pwr1666(0:10001)
      real pwr2(0:10001), pwr0625(0:10001), pwr15625(0:10001)
      common /taoicecst/ pwr81, pwr1666, pwr2, pwr0625, pwr15625

      real lathf     ! Latent heat of fusion at 0 degrees Celsius
                     ! (m**2/(s**2)).
      parameter( lathf  = 3.4e5 )

      real g         ! Acceleration due to gravity at the earth surface.
                     ! (m/(s**2))
      parameter( g      = 9.8 )     

      real p0        ! Surface reference pressure, is 100000 Pascal. 
      parameter( p0     = 1.0e5 )

      real rhow      ! Liquid water reference density (kg/m**3)
      parameter( rhow = 1.0e3 )

      real omega     ! The angular velocity of earth rotation (1/s)
      parameter( omega=7.292e-5 )

      real solarc    ! Solar constant
      parameter ( solarc = 1353.0 )

      real sbcst     ! Stefen-Boltzmann constant
      parameter ( sbcst = 5.67E-8 )

      real emissg    ! Emissivity of the ground
      parameter ( emissg = 0.995 )

      real emissa    ! Emissivity of the atmosphere
      parameter ( emissa = 0.725 )
