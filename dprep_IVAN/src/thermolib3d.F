c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_ES                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_ES( p, t )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the saturation specific humidity using enhanced Teten's
c     formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/08/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c
c     OUTPUT:
c
c       f_es     Saturation water vapor pressure (Pa)
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_es      ! Saturation water vapor pressure (Pa)
c
c#######################################################################
c
c     Function f_es and inline directive for Cray PVP
c
c#######################################################################
c
      real f_esl, f_esi

c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF ( t.ge.273.15 ) THEN      ! for water
        f_es = f_esl( p,t )
      ELSE                            ! for ice
        f_es = f_esi( p,t )
      ENDIF

      RETURN
      END

c
c#######################################################################
c
c     Calculate the saturation water vapor over liquid water using
c     enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_ESL( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_esl     ! Saturation water vapor pressure over liquid water

      real f

      include 'globcst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      f = satfwa + satfwb * p
      f_esl = f * satewa * exp( satewb*(t-273.15)/(t-satewc) )

      RETURN
      END
c
c#######################################################################
c
c     Calculate the saturation water vapor over ice using enhanced
c     Teten's formula.
c
c#######################################################################
c
      FUNCTION F_ESI( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_esi     ! Saturation water vapor pressure over ice (Pa)

      real f

      include 'globcst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      f = satfia + satfib * p
      f_esi = f * sateia * exp( sateib*(t-273.15)/(t-sateic) )

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_DESDT                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_DESDT( t )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate d(es)/dt/es using enhanced Teten's formula. See function
c     f_es.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/12/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       t        Temperature (K)
c
c     OUTPUT:
c
c       f_desdt  d(es)/dt/es (1/K)
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      real t         ! Temperature (K)
      real f_desdt   ! d(es)/dt/es (1/K)
c
c#######################################################################
c
c     Function f_desdtl and f_desdti and inline directive for Cray PVP
c
c#######################################################################
c
      real f_desdtl, f_desdti


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF ( t.ge.273.15 ) THEN      ! for water
        f_desdt = f_desdtl(t)
      ELSE                         ! for ice
        f_desdt = f_desdti(t)
      ENDIF

      RETURN
      END
c
c#######################################################################
c
c     Calculate d(esl)/dt/esl using enhanced Teten's formula.
c     See function f_esl.
c
c#######################################################################
c
      FUNCTION F_DESDTL( t )

      implicit none
 
      real t         ! Temperature (K)
      real f_desdtl  ! d(esl)/dt/esl (1/K)

      include 'globcst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      f_desdtl = satewb*(273.15-satewc)/(t-satewc)**2

      RETURN
      END
c
c#######################################################################
c
c     Calculate d(esi)/dt/esi using enhanced Teten's formula.
c     See function f_esi.
c
c#######################################################################
c
      FUNCTION F_DESDTI( t )

      implicit none
 
      real t         ! Temperature (K)
      real f_desdti  ! d(esi)/dt/esi (1/K)

      include 'globcst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      f_desdti = sateib*(273.15-sateic)/(t-sateic)**2

      RETURN
      END
c
c#######################################################################
c
c     Calculate d(qvs)/dt/qvs using enhanced Teten's formula.
c     See function f_es.
c
c              Rd
c     rddrv = ----
c              Rv
c
c     qvs = rddrv * es / (p - ( 1.0 - rddrv ) * es )
c
c       1    d(qvs)           1 - rddrv             1    d(es)
c     ----- -------- = ( 1 + ----------- * qvs ) * ---- -------
c      qvs     dT               rddrv               es    dT
c
c#######################################################################
c
      FUNCTION F_DQVSDT( p, t )

      implicit none

      real p         ! Pressure (Pa)
      real t         ! Temperature (K)
      real f_dqvsdt  ! d(esi)/dt/esi (1/K)

      real qvsat, desdt
c
c#######################################################################
c
c     Function f_desdtl and f_desdti and inline directive for Cray PVP
c
c#######################################################################
c
      real f_desdt, f_qvsat


      include 'phycst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      qvsat = f_qvsat( p,t )
      desdt = f_desdt( t )
      f_dqvsdt = ( 1. + qvsat*(1.-rddrv)/rddrv ) * desdt

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_QVSAT                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_QVSAT( p, t )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the saturation specific humidity using enhanced Teten's
c     formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/08/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c
c     OUTPUT:
c
c       f_qvsat  Saturation water vapor specific humidity (kg/kg).
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_qvsat   ! Saturation water vapor specific humidity (kg/kg)
c
c#######################################################################
c
c     Include files:
c
c#######################################################################
c
      include 'phycst.inc'
c
c#######################################################################
c
c     Function f_es and inline directive for Cray PVP
c
c#######################################################################
c
      real f_es

c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      f_qvsat = rddrv * f_es(p,t) / (p-(1.0-rddrv)*f_es(p,t))

      RETURN
      END
c
c#######################################################################
c
c     Calculate the saturation specific humidity over liquid water using
c     enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_QVSATL( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_qvsatl  ! Saturation specific humidity over liquid
                     ! water (kg/kg)

      real fesl

      include 'phycst.inc'

      real f_esl


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      fesl = f_esl( p, t )
      f_qvsatl = rddrv * fesl / (p-(1.0-rddrv)*fesl)

      RETURN
      END
c
c#######################################################################
c
c     Calculate the saturation specific humidity over ice using
c     enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_QVSATI( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_qvsati  ! Saturation specific humidity over ice (kg/kg)

      real fesi

      include 'phycst.inc'

      real f_esi


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      fesi = f_esi( p, t )
      f_qvsati = rddrv * fesi / (p-(1.0-rddrv)*fesi)

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_MRSAT                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_MRSAT( p, t )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the saturation water vapor mixing ratio using enhanced
c     Teten's formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/08/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c
c     OUTPUT:
c
c       f_mrsat  Saturation water vapor mixing ratio (kg/kg).
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_mrsat   ! Saturation water vapor mixing ratio (kg/kg)
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      real fes
c
c#######################################################################
c
c     Include files:
c
c#######################################################################
c
      include 'phycst.inc'
c
c#######################################################################
c
c     Function f_es and inline directive for Cray PVP
c
c#######################################################################
c
      real f_es


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      fes = f_es( p,t )
      f_mrsat = rddrv * fes / (p-fes)

      RETURN
      END
c
c#######################################################################
c
c     Calculate the saturated water vapor mixing ratio over liquid water
c     using enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_MRSATL( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_mrsatl  ! Saturation water vapor mixing ratio over liquid
                     ! water (kg/kg)

      real fesl

      include 'phycst.inc'

      real f_esl


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      fesl = f_esl( p,t )
      f_mrsatl = rddrv * fesl / (p-fesl)

      RETURN
      END
c
c#######################################################################
c
c     Calculate the saturated water vapor mixing ratio over ice
c     using enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_MRSATI( p, t )

      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real f_mrsati  ! Saturation water vapor mixing ratio over ice 
                     ! (kg/kg)

      real fesi

      include 'phycst.inc'

      real f_esi


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      fesi = f_esi( p,t )
      f_mrsati = rddrv * fesi / (p-fesi)

      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_TDEW                    ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_TDEW( p, t, qv )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the dew point temperature using reversed enhanced
c     Tetan's formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/09/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c       qv       Specific humidity (kg/kg)
c
c     OUTPUT:
c
c       f_tdew   Dew point temperature (K)
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      real p         ! Pressure (Pascal)
      real t         ! Temperature (K)
      real qv        ! Specific humidity (kg/kg)
      real f_tdew    ! Dew point temperature (K)
c
c#######################################################################
c
c     Include file
c
c#######################################################################
c
      include 'globcst.inc'
c
c#######################################################################
c
c     Function f_tdewl and f_tdewi and inline directive for Cray PVP
c
c#######################################################################
c
      real f_tdewl, f_tdewi


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF ( t.ge.273.15 ) THEN         ! for water
        f_tdew = f_tdewl( p,qv )
        IF ( f_tdew.lt.273.15 ) THEN  ! using ice formula
          f_tdew = f_tdewi( p,qv )
        ENDIF
      ELSE                            ! for ice
        f_tdew = f_tdewi( p,qv )
      ENDIF

      RETURN
      END
c
c#######################################################################
c
c     Calculate the dew point temperature over liquid water using the
c     reversed enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_TDEWL( p, qv )

      implicit none
 
      real p         ! Pressure (Pascal)
      real qv        ! Specific humidity (kg/kg)
      real f_tdewl   ! Dew point temperature over liquid water (K)

      real qvs, esl, f, ln

      include 'globcst.inc'
      include 'phycst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      qvs = max( qv, 1.e-8 )

      f = satfwa + satfwb * p
      esl = p*qvs/(rddrv+(1-rddrv)*qvs)
      ln = alog( esl/(f*satewa) )

      f_tdewl = ( ln*satewc - 273.15*satewb ) / ( ln - satewb )

      RETURN
      END
c
c#######################################################################
c
c     Calculate the dew point temperature over ice using the
c     reversed enhanced Teten's formula.
c
c#######################################################################
c
      FUNCTION F_TDEWI( p, qv )

      implicit none
 
      real p         ! Pressure (Pascal)
      real qv        ! Specific humidity (kg/kg)
      real f_tdewi   ! Dew point temperature over ice (K)

      real qvs, esi, f, ln

      include 'globcst.inc'
      include 'phycst.inc'
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      qvs = max( qv, 1.e-8 )

      f = satfia + satfib * p
      esi = p*qvs/(rddrv+(1-rddrv)*qvs)
      ln = alog( esi/(f*sateia) )

      f_tdewi = ( ln*sateic - 273.15*sateib ) / ( ln - sateib )

      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE F_PT2PTE                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c

        FUNCTION F_PT2PTE( p, pt, qv )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the equivalent potential temperature of an air
c     parcel that is either saturated or unsaturated.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/14/1998  Re-created from subroutine EQUIPT
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT:
c
c       p        Base state pressure (Pascals)
c       pt       Potential temperature (degrees Kelvin)
c       qv       Water vapor specific humidity (kg/kg)
c
c     OUTPUT:
c
c       f_pt2pte Equivalent potential temperature (K)
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none

      real p         ! Pressure (Pascals)
      real pt        ! Potential temperature (degrees Kelvin)
      real qv        ! Water vapor specific humidity (kg/kg)

      real f_pt2pte  ! Equivalent potential temperature (K)
c
c#######################################################################
c
c     Misc. local variables
c
c#######################################################################
c
      real t       ! Temperature
      real tdew    ! Dew point temperature
c
c#######################################################################
c
c     Include files:
c
c#######################################################################
c
      include 'globcst.inc'
      include 'phycst.inc'
c
c#######################################################################
c
c     Function f_tdew and inline directive for Cray PVP
c
c#######################################################################
c
      real f_tdew


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t = pt * (p/p0)**rddcp

      tdew = f_tdew( p,t,qv )    ! Dew point temperture.

      f_pt2pte = pt * exp( lathv*qv/(cp*tdew) )

      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE GETQVS                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE GETQVS( nx,ny,nz, ibgn,iend,jbgn,jend,kbgn,kend,
     :           p, t, qvsat )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the saturation specific humidity using Teten's formula.
c
c#######################################################################
c
c     AUTHOR: Ming Xue
c     3/17/1991.
c
c     MODIFICATION HISTORY:
c
c     5/02/92 (M. Xue)
c     Added full documentation.
c
c     5/03/92 (M. Xue)
c     Further documentation. 
c
c     2/10/93 (K. Droegemeier)
c     Cleaned up documentation.
c
c     9/10/94 (Weygandt & Y. Lu)
c     Cleaned up documentation.
c
c     12/11/1997 (Yuhe Liu)
c     Rewrote the subroutine calling function F_QVSAT to calculate the
c     saturated specific humidity for array.
c
c#######################################################################
c
c     INPUT :
c
c       nx       Array dimension in x-direction
c       ny       Array dimension in y-direction
c       nz       Array dimension in z-direction
c       ibgn     Starting index in x-direction
c       jbgn     Starting index in y-direction
c       kbgn     Starting index in z-direction
c       iend     Last index in x-direction
c       jend     Last index in y-direction
c       kend     Last index in z-direction
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c
c     OUTPUT:
c
c       qvsat    Saturation water vapor specific humidity (kg/kg).
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer nx,ny,nz         ! Input array dimension

      integer ibgn             ! Starting index in x-direction
      integer jbgn             ! Starting index in y-direction
      integer kbgn             ! Starting index in z-direction
      integer iend             ! Last index in x-direction
      integer jend             ! Last index in y-direction
      integer kend             ! Last index in z-direction

      real p  (nx,ny,nz)       ! Pressure (Pascal)
      real t  (nx,ny,nz)       ! Temperature (K)
      real qvsat(nx,ny,nz)     ! Saturation water vapor specific 
                               ! humidity (kg/kg)
c
c#######################################################################
c
c     Include file
c
c#######################################################################
c
      include 'globcst.inc'
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j,k
c
c#######################################################################
c
c     Function f_qvsat and inline directives for Cray PVP
c
c#######################################################################
c
      real f_qvsat


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      DO 10 k=kbgn,kend
      DO 10 j=jbgn,jend
      DO 10 i=ibgn,iend
        qvsat(i,j,k) = f_qvsat( p(i,j,k), t(i,j,k) )
10    CONTINUE

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE GETMRS                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE GETMRS( nx,ny,nz, ibgn,iend,jbgn,jend,kbgn,kend,
     :           p, t, mrsat )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the saturation mixing ratio using Teten's formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/09/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       nx       Array dimension in x-direction
c       ny       Array dimension in y-direction
c       nz       Array dimension in z-direction
c       ibgn     Starting index in x-direction
c       jbgn     Starting index in y-direction
c       kbgn     Starting index in z-direction
c       iend     Last index in x-direction
c       jend     Last index in y-direction
c       kend     Last index in z-direction
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c
c     OUTPUT:
c
c       mrsat    Saturation water vapor Mixing ratio (kg/kg).
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer nx,ny,nz         ! Input array dimension

      integer ibgn             ! Starting index in x-direction
      integer jbgn             ! Starting index in y-direction
      integer kbgn             ! Starting index in z-direction
      integer iend             ! Last index in x-direction
      integer jend             ! Last index in y-direction
      integer kend             ! Last index in z-direction

      real p  (nx,ny,nz)       ! Pressure (Pascal)
      real t  (nx,ny,nz)       ! Temperature (K)
      real mrsat(nx,ny,nz)     ! Saturated water vapor mixing ratio (kg/kg)
c
c#######################################################################
c
c     Include file
c
c#######################################################################
c
      include 'globcst.inc'
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j,k
c
c#######################################################################
c
c     Function f_mrsat and inline directive for Cray PVP
c
c#######################################################################
c
      real f_mrsat


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      DO 10 k=kbgn,kend
      DO 10 j=jbgn,jend
      DO 10 i=ibgn,iend
        mrsat(i,j,k) = f_mrsat( p(i,j,k), t(i,j,k) )
10    CONTINUE

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE GETDEW                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE GETDEW( nx,ny,nz, ibgn,iend,jbgn,jend,kbgn,kend,
     :           p, t, qv, tdew )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the dew point temperature (K) using enhanced Teten's
c     formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/09/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       nx       Array dimension in x-direction
c       ny       Array dimension in y-direction
c       nz       Array dimension in z-direction
c       ibgn     Starting index in x-direction
c       jbgn     Starting index in y-direction
c       kbgn     Starting index in z-direction
c       iend     Last index in x-direction
c       jend     Last index in y-direction
c       kend     Last index in z-direction
c
c       p        Pressure (Pascal)
c       t        Temperature (K)
c       qv       Specific humidity (kg/kg)
c
c     OUTPUT:
c
c       tdew     Dew point temperature (K)
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer nx,ny,nz         ! Input array dimension

      integer ibgn             ! Starting index in x-direction
      integer jbgn             ! Starting index in y-direction
      integer kbgn             ! Starting index in z-direction
      integer iend             ! Last index in x-direction
      integer jend             ! Last index in y-direction
      integer kend             ! Last index in z-direction

      real p  (nx,ny,nz)       ! Pressure (Pascal)
      real t  (nx,ny,nz)       ! Temperature (K)
      real qv (nx,ny,nz)       ! Specific humidity (kg/kg)
      real tdew(nx,ny,nz)      ! Dew point temperature (K)
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j,k
c
c#######################################################################
c
c     Function f_mrsat and inline directive for Cray PVP
c
c#######################################################################
c
      real f_tdew


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      DO 10 k=kbgn,kend
      DO 10 j=jbgn,jend
      DO 10 i=ibgn,iend
        tdew(i,j,k) = f_tdew( p(i,j,k), t(i,j,k), qv(i,j,k) )
10    CONTINUE

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE PT2PTE                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE PT2PTE( nx,ny,nz, ibgn,iend,jbgn,jend,kbgn,kend,
     :           p, pt, qv, pte )
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the dew point temperature (K) using enhanced Teten's
c     formula.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     01/09/1998
c
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c     INPUT :
c
c       nx       Array dimension in x-direction
c       ny       Array dimension in y-direction
c       nz       Array dimension in z-direction
c       ibgn     Starting index in x-direction
c       jbgn     Starting index in y-direction
c       kbgn     Starting index in z-direction
c       iend     Last index in x-direction
c       jend     Last index in y-direction
c       kend     Last index in z-direction
c
c       p        Pressure (Pascal)
c       pt       Temperature (K)
c       qv       Specific humidity (kg/kg)
c
c     OUTPUT:
c
c       pte      Equivalent potential temperature (K)
c
c#######################################################################
c

c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
      integer nx,ny,nz         ! Input array dimension

      integer ibgn             ! Starting index in x-direction
      integer jbgn             ! Starting index in y-direction
      integer kbgn             ! Starting index in z-direction
      integer iend             ! Last index in x-direction
      integer jend             ! Last index in y-direction
      integer kend             ! Last index in z-direction

      real p  (nx,ny,nz)       ! Pressure (Pascal)
      real pt (nx,ny,nz)       ! Temperature (K)
      real qv (nx,ny,nz)       ! Specific humidity (kg/kg)
      real pte(nx,ny,nz)       ! Equivalent potential temperature (K)
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      integer i,j,k
c
c#######################################################################
c
c     Function f_pt2pte and inline directive for Cray PVP
c
c#######################################################################
c
      real f_pt2pte


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      DO 10 k=kbgn,kend
      DO 10 j=jbgn,jend
      DO 10 i=ibgn,iend
        pte(i,j,k) = f_pt2pte( p(i,j,k), pt(i,j,k), qv(i,j,k) )
10    CONTINUE

      RETURN
      END
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_PCCL                    ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_PCCL(pm,p,t,td,mrbar,n)
c
c#######################################################################
c
c     PURPOSE:
c
c     this function returns the pressure at the convective condensation
c     level given the appropriate sounding data.
c
c     the algorithm is decribed on p.17 of stipanuk, g.s.,1973:
c     "algorithms for generating a skew-t log p diagram and computing
c     selected meteorological quantities," atmospheric sciences labora-
c     tory, u.s. army electronics command, white sands missile range, new
c     mexico 88002.
c
c#######################################################################
c
c     AUTHOR:
c       g.s. stipanuk     1973            original version.
c       reference stipanuk paper entitled:
c            "algorithms for generating a skew-t, log p
c            diagram and computing selected meteorological
c            quantities."
c            atmospheric sciences laboratory
c            u.s. army electronics command
c            white sands missile range, new mexico 88002
c            33 pages
c       baker, schlatter  17-may-1982
c
c
c     MODIFICATIONS:
c
c     01/30/1998 (Yuhe Liu)
c     Modified to use Kelvin and Pascal as units of temperature and
c     pressure
c
c#######################################################################
c
c     INPUT:
c
c       p       pressure (Pa). note that p(i).gt.p(i+1).
c       t       temperature (K)
c       td      dew point (K)
c       n       number of levels in the sounding and the dimension of
c               p, t and td
c       pm      pressure (Pa) at upper boundary of the layer for
c               computing the mean mixing ratio. p(1) is the lower
c               boundary.
c
c     OUTPUT:
c
c       mrbar   mean mixing ratio (kg/kg) in the layer bounded by
c               pressures p(1) at the bottom and pm at the top
c       f_pccl  pressure (Pa) at the convective condensation level
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none

      integer n

      real t(n),p(n),td(n)
      real pm

      real mrbar
      real f_pccl
c
c#######################################################################
c
c     Misc. local variables
c
c#######################################################################
c
      integer i,j,k,l

      real pc,tq,del,x,a
      real mrsat1,mrsat2
c
c#######################################################################
c
c     Function f_mrsat and inline directive for Cray PVP
c
c#######################################################################
c
      real f_mrsat
      real f_tmr


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF (pm.ne.p(1)) GO TO 5

      mrbar = f_mrsat( p(1),td(1) )
      pc= pm

      IF (abs(t(1)-td(1)).lt.0.05) GO TO 45
      GO TO 25

  5   CONTINUE

      mrbar = 0.
      k = 0

 10   CONTINUE

      k = k+1
      IF (p(k).gt.pm) GO TO 10
      k = k-1
      j = k-1
      IF (j.lt.1) GO TO 20

c   compute the average mixing ratio....alog = natural log

      DO 15 i= 1,j
        l = i+1
        mrsat1 = f_mrsat( p(i),td(i) )
        mrsat2 = f_mrsat( p(l),td(l) )
        mrbar = (mrsat1+mrsat2)*alog(p(i)/p(l)) + mrbar
 15   CONTINUE

 20   CONTINUE
      l= k+1

c   estimate the dew point at pressure pm.

      mrsat1 = f_mrsat( p(k),td(k) )
      tq= td(k)+(td(l)-td(k))*(alog(pm/p(k)))/(alog(p(l)/p(k)))
      mrsat2 = f_mrsat( pm,tq )
      mrbar=  mrbar+(mrsat1+mrsat2 )*alog(p(k)/pm)
      mrbar=  mrbar/(2.*alog(p(1)/pm))


c   find level at which the mixing ratio line  mrbar crosses the
c   environmental temperature profile.

 25   CONTINUE

      DO 30 j= 1,n
        i = n-j+1
        IF (p(i).lt.300.) GO TO 30

c   f_tmr = temperature (celsius) at pressure p (mb) along a mixing
c           ratio line given by  mrbar (g/kg)

        x = f_tmr(mrbar,p(i))-t(i)
        IF (x.le.0.) GO TO 35
 30   CONTINUE

      f_pccl = 0.0

      RETURN

c  set up bisection routine

 35   l = i
      i = i+1
      del = p(l)-p(i)
      pc = p(i)+.5*del
      a = (t(i)-t(l))/alog(p(l)/p(i))
      DO 40 j = 1,10
        del = del/2.
        x = f_tmr(mrbar,pc)-t(l)-a*(alog(p(l)/pc))

c   the sign function replaces the sign of the first argument
c   with that of the second.

        pc = pc+sign(del,x)
 40   CONTINUE

 45   f_pccl = pc

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_CT                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      FUNCTION F_CT(mrbar,pc,ps)
c
c#######################################################################
c
c     PURPOSE:
c
c     This function returns the convective temperature ct (K)
c     given the mean mixing ratio mrbar (kg/kg) in the surface layer,
c     the pressure pc (Pa) at the convective condensation level (ccl)
c     and the surface pressure ps (Pa).
c
c#######################################################################
c
c     AUTHOR: G.S. Stipanuk     1973            original version.
c
c       reference stipanuk paper entitled:
c            "algorithms for generating a skew-t, log p
c            diagram and computing selected meteorological
c            quantities."
c            atmospheric sciences laboratory
c            u.s. army electronics command
c            white sands missile range, new mexico 88002
c            33 pages
c       baker, schlatter  17-may-1982
c
c     MODIFICATION HISTORY:
c
c     01/30/1998 (Yuhe Liu)
c     Modified for ARPS to use the units of Kelvin, Pascal and kg/kg for
c     temperature, pressure and mixing ratio respectively
c
c#######################################################################
c
c     INPUT :
c
c       mrbar    Mean mixing ratio (kg/kg)
c       pc       Pressure at CCL (Pa)
c       ps       Pressure at surface (Pa)
c
c     OUTPUT:
c
c       f_ct     Convective temperature (K)
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none

      real mrbar
      real pc
      real ps

      real f_ct

      real tc
c
c#######################################################################
c
c     Include file
c
c#######################################################################
c
      include 'phycst.inc'
c
c#######################################################################
c
c     Function f_tmr and inline directive for Cray PVP
c
c#######################################################################
c
      real f_tmr


c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c     compute the temperature (K) at the ccl.

      tc= f_tmr(mrbar,pc)
c
c#######################################################################
c
c     compute the potential temperature (K) at ccl, i.e., the dry
c     adiabat through the ccl.
c
c     ptc = tc*((100000./pc)**rddcp)
c
c     and then compute the surface temperature on the same dry adiabat.
c
c     f_ct = ptc*((ps/100000.)**rddcp)
c
c#######################################################################
c
      f_ct = tc*((ps/pc)**rddcp)

      RETURN
      END
c
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   FUNCTION F_TMR                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
        FUNCTION F_TMR(p,mr)
c
c#######################################################################
c
c     PURPOSE:
c
c     This function returns the temperature (Kelvin) on a mixing
c     ratio line mr (kg/kg) at pressure p (Pa). The formula is given in
c     table 1 on page 7 of Stipanuk (1973).
c
c#######################################################################
c
c     AUTHOR: G.S. Stipanuk, 1973            original version.
c
c       reference stipanuk paper entitled:
c            "algorithms for generating a skew-t, log p
c            diagram and computing selected meteorological
c            quantities."
c            atmospheric sciences laboratory
c            u.s. army electronics command
c            white sands missile range, new mexico 88002
c            33 pages
c       baker, schlatter  17-may-1982
c
c     MODIFICATIONS:
c
c     01/30/1998 (Yuhe Liu)
c     Modified to use the units of Kelvin, Pascal and kg/kg for
c     temperature, pressure and mixing ratio respectively.
c
c#######################################################################
c
c     INPUT:
c
c       p        Pressure (Pa)
c       mr       Mixing ratio (kg/kg)
c
c     OUTPUT:
c
c       f_tmr    Temperature (K)
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none

      real p              ! Pressure (Pa)
      real mr             ! Mixing ratio (kg/kg)

      real f_tmr          ! Temperature (K)
c
c#######################################################################
c
c     Local variables
c
c#######################################################################
c
      real x, pmb, mrg

      real c1,c2,c3,c4,c5,c6

      data c1/.0498646455/
      data c2/2.4082965/
      data c3/7.07475/
      data c4/38.9114/
      data c5/.0915/
      data c6/1.2035/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c     Beginning of executable code
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      pmb = p/100.
      mrg = mr*1000.

      x= alog10(mrg*pmb/(622.+mrg))
      f_tmr= 10.**(c1*x+c2)-c3+c4*((10.**(c5*x)-c6)**2.)

      RETURN
      END
