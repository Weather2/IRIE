      program grib2model_new

      character(LEN=255):: root, root_g

      OPEN(UNIT=12,FILE='paramroot',STATUS='OLD')
      READ(12,'(a)') root
      CLOSE (12)
      OPEN(UNIT=12,FILE='paramavn',STATUS='OLD')
      READ(12,'(a)') root_g
      CLOSE (12)

      CALL DEGRIB(root(1:len_trim(root)),root_g(1:len_trim(root_g)))

      END
c
c===============================================================================
c
      SUBROUTINE DEGRIB(root,root_g)
C-----------------------------------------------------------------------
      parameter (lmll=26, lmllRH=21)
C-----------------------------------------------------------------------
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: HGT
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: U
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: V
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TMP
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RH
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: Q
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CWM
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: QNEW
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: QETA
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: slp
C-----------------------------------------------------------------------
      real vrt            (lmll)
      real vrtRH          (lmllRH)
      data vrt   /1000, 975, 950, 925, 900, 850, 800, 750, 700, 650, 600,
     &            550, 500, 450, 400, 350, 300, 250, 200, 150, 100,  70,
     &            50,  30,  20, 10 /
      data vrtRH /1000, 975, 950, 925, 900, 850, 800, 750, 700, 650, 600,
     &            550, 500, 450, 400, 350, 300, 250, 200, 150, 100 /
C-----------------------------------------------------------------------
      real*4 xe,mrsat
      real*4 esat,es
      common /estab/esat(15000:45000),es(15000:45000)
C-----------------------------------------------------------------------
      character*8 cdate8
      character*3 cstart, cend, cint, chr3
      character*2 chr2
      character *2 gproj
      character *(*) root, root_g
      character(LEN=255):: gribfile
C-----------------------------------------------------------------------
      integer nx,ny,nz,kgds(200),kpds(200),gdsinfo(9),n !Degrib grid dimensions
      integer iunit,istart,iend,iint,ihr,id7,irotate,igds
      dimension id7(7)
C-----------------------------------------------------------------------
      if (iargc() .lt. 4) then
         write(*,*)'Usage: grads2pnt_avn_new date8 start end int'
         stop
      end if
      call getarg (1, cdate8)
      call getarg (2, cstart)
      call getarg (3, cend)
      call getarg (4, cint)
      call cdate2id7 (cdate8,id7)

      open(11,file=root//"/data/prep/cdate11")
      write(11,'(i4,3i2)') id7(1)+2000,id7(2),id7(3),id7(4)
      close (11)

      read(cstart,*)istart
      read(cend,*)iend
      read(cint,*)iint

      id7(1)=id7(1)+2000 ! RGRIB2 NEED FULL YEAR
C-----------------------------------------------------------------------
      call es_ini
C
C--- Create lookup tables for saturation vapor pressure w/r/t water & ice
C
      CALL GPVS
C-----------------------------------------------------------------------
      iunit=11
      DO IHR=ISTART,IEND,IINT
C-----------------------------------------------------------------------
      ID7(5)=IHR

      if (IHR.lt.100) then
        write(chr2,"(I2.2)")IHR
        gribfile=root_g//cdate8(7:8)//"/gblav2."//cdate8//chr2
      else
        write(chr3,"(I3.3)")IHR
        gribfile=root_g//cdate8(7:8)//"/gblav2."//cdate8//chr3
      end if

      n=index(gribfile,' ')-1
      iunit=iunit+1
      call get_gds2(iunit,gribfile(1:n),gdsinfo,kpds)

      imll=gdsinfo(1)
      jmll=gdsinfo(2)

      ALLOCATE( HGT(1:imll,1:jmll,1:lmll))
      ALLOCATE(   U(1:imll,1:jmll,1:lmll))
      ALLOCATE(   V(1:imll,1:jmll,1:lmll))
      ALLOCATE( TMP(1:imll,1:jmll,1:lmll))
      ALLOCATE(  RH(1:imll,1:jmll,1:lmll))
      ALLOCATE(   Q(1:imll,1:jmll,1:lmll))
      ALLOCATE( CWM(1:imll,1:jmll,1:lmll))
      ALLOCATE(QNEW(1:imll,1:jmll,1:lmll))
      ALLOCATE(QETA(1:imll,1:jmll,1:lmll))
      ALLOCATE( slp(1:imll,1:jmll,1:  12))

      irotate=0
      if (gdsinfo(5).lt.gdsinfo(3)) then
         irotate=1
         igds = gdsinfo(5)
         gdsinfo(5) = gdsinfo(3)
         gdsinfo(3) = igds
      endif

      print *, gdsinfo(1), gdsinfo(4), gdsinfo(6), gdsinfo(7)
      print *, gdsinfo(2), gdsinfo(3), gdsinfo(5), gdsinfo(8)

      kgds(1) = 0
      kgds(2) = gdsinfo(1)
      kgds(3) = gdsinfo(2)
      kgds(4) = gdsinfo(3)
      kgds(5) = gdsinfo(4)
      kgds(7) = gdsinfo(5)
      kgds(8) = gdsinfo(6)
      kgds(9) = gdsinfo(7)
      kgds(10) = gdsinfo(8)
      kgds(11) = gdsinfo(9)
      kgds(12) = 0

C-----------------------------------------------------------------------
      CALL RGRIB2 (iunit,id7, 0,3,  5,100,0,  0,255,0,  0,imll,jmll,  lmll,  vrt,HGT,irotate)
      CALL RGRIB2 (iunit,id7, 0,2,  2,100,0,  0,255,0,  0,imll,jmll,  lmll,  vrt,  U,irotate)
      CALL RGRIB2 (iunit,id7, 0,2,  3,100,0,  0,255,0,  0,imll,jmll,  lmll,  vrt,  V,irotate)
      CALL RGRIB2 (iunit,id7, 0,0,  0,100,0,  0,255,0,  0,imll,jmll,  lmll,  vrt,TMP,irotate)
      CALL RGRIB2 (iunit,id7, 0,1,  1,100,0,  0,255,0,  0,imll,jmll,lmllRH,vrtRH, RH,irotate)
      CALL RGRIB2 (iunit,id7, 0,1, 22,100,0,  0,255,0,  0,imll,jmll,lmllRH,vrtRH,CWM,irotate)
C-----------------------------------------------------------------------

      call BACLOSE(iunit,IRET)
      if (IRET.ne.0) then
         write(6,*) 'BACLOSE: IRET =', IRET
         stop
      end if

C-----------------------------------------------------------------------
C-ODREDJIVANJE SPECIFICNE VLAGE
      DO L = lmll, 1, -1
        P=vrt(L)
        DO I=1,imll
        DO J=1,jmll 
          TMT0=TMP(I,J,L)-273.16
          TKL=TMP(I,J,L)
          PP=P*100
          IF (P.lt.100) THEN
            Q(I,J,L)=1.E-6
            CWM(I,J,L)=0.
          ELSE
            qs=q_tp(TMT0,P)
C-----------------------------------------------------------------------
            Q(I,J,L)=MAX(0.01*RH(I,J,L)*qs,1.E-6)
C-----------------------------------------------------------------------
          END IF
        END DO
        END DO
      END DO
C-----------------------------------------------------------------------

          DO l=1, lmll
          print '(i3,7f9.1,f9.2)',l,VRT(l),HGT(IMLL/2,JMLL/2,L)
     & ,U(IMLL/2,JMLL/2,L),V(IMLL/2,JMLL/2,L),TMP(IMLL/2,JMLL/2,L)-273.16
     & ,1.e5*CWM(IMLL/2,JMLL/2,L)
     & ,1.e5*Q(IMLL/2,JMLL/2,L),0.01*RH(IMLL/2,JMLL/2,L)
          END DO

C-----------------------------------------------------------------------

      write(chr3,'(i3.3)') IHR
      open(11,file=root//"/data/prep/ua."//chr3, form='unformatted')
      nsfcfld=0
      nx = imll
      ny = jmll
      nz = lmll
      gproj='LL'
      write (11) nz
      write (11) U
      write (11) V
      write (11) HGT
      write (11) Q
      write (11) CWM
      write (11) VRT
      write (11) slp
      CLOSE (11)

C-----------------------------------------------------------------------

      open(12,file=root//"/data/prep/gdsinfo.ETA_avn", form='unformatted')
      write (12) kgds 
      print *,' kgds=',(kgds(k),k=1,12)
      close (12)

C-----------------------------------------------------------------------
      DEALLOCATE( HGT)
      DEALLOCATE(   U)
      DEALLOCATE(   V)
      DEALLOCATE( TMP)
      DEALLOCATE(  RH)
      DEALLOCATE(   Q)
      DEALLOCATE( CWM)
      DEALLOCATE(QNEW)
      DEALLOCATE(QETA)
      DEALLOCATE( slp)
C-----------------------------------------------------------------------
      END DO
      RETURN
      END
c
c===============================================================================
c
      subroutine RGRIB2(iunit,id7,jdisc,j1,j2,j3,j4,j5,j6,j7,j8,imll,jmll,lmll,vrt,var3d,irotate)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use grib_mod
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer :: iunit,id7(7),j1,j2,j3,j4,j5,j6,j7,j8,imll,jmll,lmll,irotate
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      integer :: jdisc,jpdtn,jgdtn,m,mrot,iret,j,i,jrot,k
      integer :: indstart,indend
      logical :: unpack=.true.
      real vrt (lmll)
      real, dimension(imll,jmll,lmll) :: var3d
      logical :: lprint=.false.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Set GRIB2 field identification values to search for
      jids=-9999
      jpdtn=0
      jpdt=-9999
      jgdtn=-1
      jgdt=-9999
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C TIME bits
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      jids(6)=id7(1)
      jids(7)=id7(2)
      jids(8)=id7(3)
      jids(9)=id7(4)
      jpdt(9)=id7(5)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C VAR bits
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      jpdt(1)=j1
      jpdt(2)=j2
      jpdt(10)=j3
      jpdt(11)=j4
      jpdt(12)=j5
      jpdt(13)=j6
      jpdt(14)=j7
      jpdt(15)=j8
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      indstart=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      do m=1,lmll
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      mrot=lmll-m+1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(lmll.gt.1)jpdt(12)=vrt(mrot)*100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Get field from file
      call getgb2(iunit,0,indstart,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &            unpack,indend,gfld,iret)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(iret.ne.0)then
        if(iret.eq.96)then
          print *,'ERROR READING INDEX'
        elseif(iret.eq.97)then
          print *,'ERROR READING GRIB FILE'
        elseif(iret.eq.99)then
          print *,'REQUEST NOT FOUND'
        else
          print *,'GF_GETFLD GRIB2 UNPACKER RETURN CODE'
        endif
        STOP 'ERROR - CHECK!'
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(lprint)then
        print *,'discipline',gfld%discipline
        print *,'idsectlen',gfld%idsectlen

        do j=1,gfld%idsectlen
          print *,j,gfld%idsect(j)
        enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        print *,'ipdtnum',gfld%ipdtnum
        print *,'ipdtlen',gfld%ipdtlen

        do j=1,gfld%ipdtlen
          print *,j,gfld%ipdtmpl(j)
        enddo

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        print *,'igdtnum',gfld%igdtnum
        print *,'igdt',gfld%igdtlen

        do j=1,gfld%igdtlen
          print *,j,gfld%igdtmpl(j)
        enddo
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      do j=1,jmll
      do i=1,imll
        if(irotate.eq.1)then
          jrot=jmll-j+1
        else
          jrot=j
        endif
        k=(j-1)*imll+i
        if(gfld%ibmap.eq.0)then
          if(gfld%bmap(k))then
            var3d(i,jrot,mrot)=gfld%fld(k)
          else
            var3d(i,jrot,mrot)=9.999e+20
          endif
        else
          var3d(i,jrot,mrot)=gfld%fld(k)
        endif
      end do
      end do
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Free memory when done with field
      call gf_free(gfld)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
c
c===============================================================================
c
      subroutine get_gds2(iunit,gribfile,gdsinfo,kpds)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use grib_mod
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      character*(*) gribfile
      integer :: iunit
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      integer,dimension(200) :: kpds
      integer,dimension(9) :: gdsinfo
      integer :: jdisc,jpdtn,jgdtn,m,mrot,iret,j,i,jrot,k,len
      integer :: indstart,indend
      logical :: unpack=.false.
      logical :: lprint=.false.

  ! Set GRIB2 field identification values to search for
      jdisc=0
      jids=-9999
      jpdtn=0
      jpdt=-9999
      jgdtn=0
      jgdt=-9999

      len=index(gribfile//' ',' ')-1

      write(6,*) 'want to open ', gribfile(1:len)
      call baopenr(iunit,gribfile(1:len),IRET)
      if (IRET.ne.0) then
         write(6,*) 'BAOPEN in get_gds: ', IRET
         stop
      end if
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      indstart=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Get field from file
      call getgb2(iunit,0,indstart,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &            unpack,indend,gfld,iret)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(iret.ne.0)then
        if(iret.eq.96)then
          print *,'ERROR READING INDEX'
        elseif(iret.eq.97)then
          print *,'ERROR READING GRIB FILE'
        elseif(iret.eq.99)then
          print *,'REQUEST NOT FOUND'
        else
          print *,'GF_GETFLD GRIB2 UNPACKER RETURN CODE'
        endif
        STOP 'ERROR - CHECK!'
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(lprint)then
        print *,'discipline',gfld%discipline
        print *,'idsectlen',gfld%idsectlen

        do j=1,gfld%idsectlen
          print *,j,gfld%idsect(j)
        enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        print *,'ipdtnum',gfld%ipdtnum
        print *,'ipdtlen',gfld%ipdtlen

        do j=1,gfld%ipdtlen
          print *,j,gfld%ipdtmpl(j)
        enddo

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        print *,'igdtnum',gfld%igdtnum
        print *,'igdt',gfld%igdtlen

        do j=1,gfld%igdtlen
          print *,j,gfld%igdtmpl(j)
        enddo
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      gdsinfo(1)=gfld%igdtmpl( 8)
      gdsinfo(2)=gfld%igdtmpl( 9)
      gdsinfo(3)=gfld%igdtmpl(12)/1000
      gdsinfo(4)=gfld%igdtmpl(13)/1000
      gdsinfo(5)=gfld%igdtmpl(15)/1000
      gdsinfo(6)=gfld%igdtmpl(16)/1000
      gdsinfo(7)=gfld%igdtmpl(17)/1000
      gdsinfo(8)=gfld%igdtmpl(18)/1000
      gdsinfo(9)=0
      if (gdsinfo(4).gt.180000) gdsinfo(4)=gdsinfo(4)-360000
      if (gdsinfo(6).gt.180000) gdsinfo(6)=gdsinfo(6)-360000

      kpds=0
      kpds( 8)=gfld%idsect (6) ! YEAR
      kpds( 9)=gfld%idsect (7) ! MONTH OF YEAR
      kpds(10)=gfld%idsect (8) ! DAY OF MONTH
      kpds(11)=gfld%idsect (9) ! HOUR OF DAY
      kpds(14)=gfld%ipdtmpl(9) ! FORECAST HOUR
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Free memory when done with field
      call gf_free(gfld)

      return
      end
c
c===============================================================================
c
      subroutine cdate2id7 (cdate8,id7)
      character *8 cdate8
      dimension id7 (7)
      id7 (5) = 0
      id7 (6) = 0
      id7 (7) = 0
      do j = 1, 4
        j1 = (j-1)*2 + 1
        read (cdate8(j1:j1+1),'(i2)') id7 (j)
      enddo
      return
      end

c
c===============================================================================
c
         function q_tp (t,p)
                             P A R A M E T E R
     & (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516)
C
          TKL=t+273.16
          TMT15=AMIN1(t,-15.)
          PP=P*100.
          q_tp = PQ0/PP*EXP(A2*(TKL-A3)/(TKL-A4))
C
          AI=0.008855
          BI=1.
          if (t.le.-20) then
           AI=0.007225
           BI=0.9674
          endif
          q_led=q_tp*(BI+AI*t)
          q_int=q_tp*(1.-0.00032*TMT15*(TMT15+15.))
          if (t.lt.-15.) q_int=q_led
          q_tp=q_int
C
         return
         end

c      
c===============================================================================
c
      subroutine es_ini
c
      common /estab/esat(15000:45000),es(15000:45000)
c
c *** Create tables of the saturation vapour pressure with up to
c        two decimal figures of accuraccy:
c
      do it=15000,45000
         t=it*0.01
         p1 = 11.344-0.0303998*t
         p2 = 3.49149-1302.8844/t
         c1 = 23.832241-5.02808*alog10(t)
         esat(it) = 10.**(c1-1.3816E-7*10.**p1+
     .               8.1328E-3*10.**p2-2949.076/t)
         es(it) = 610.78*exp(17.269*(t-273.16)/(t-35.86))
      enddo
c
      return
      end
c      
c===============================================================================
c
      function q_wrf ( p_pa, t_k , rh )
C
      IMPLICIT NONE
      REAL t_k, p_pa, rh, q_wrf
      REAL e, es, ep_3, SVPT0
      PARAMETER (ep_3=0.622, SVPT0=273.15)
C
        IF (t_k.GE.SVPT0)THEN
          es = 1.E2*6.1121*EXP((17.502*t_k-4780.6713)/(t_k-32.18))
        ELSE
          es = 1.E2*6.1115*EXP((22.452*t_k-6132.7638)/(t_k-0.60))
        ENDIF
C
        IF (p_pa.GE.80000.) es = es * 1.004
C
        e = rh * .01 * es
        q_wrf = 0.622 * e / (p_pa - e )
C
      return
      end
c      
c===============================================================================
c
      function rh_wrf ( p_pa, t_k , qv )
C
      IMPLICIT NONE
      REAL t_k, p_pa, qv, rh_wrf
      REAL e, es, ep_3, SVPT0
      PARAMETER (ep_3=0.622, SVPT0=273.15)
C
        IF (t_k.GE.SVPT0)THEN
          es = 1.E2*6.1121*EXP((17.502*t_k-4780.6713)/(t_k-32.18))
        ELSE
          es = 1.E2*6.1115*EXP((22.452*t_k-6132.7638)/(t_k-0.60))
        ENDIF
C
        IF (p_pa.GE.80000.) es = es * 1.004
C
        rh_wrf = qv*p_pa/(es*(qv+0.622))
C
      return
      end
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
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
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

