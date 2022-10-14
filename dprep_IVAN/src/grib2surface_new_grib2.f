      program grib2surface_new

      character(LEN=255):: root, root_g

      OPEN(UNIT=12,FILE='paramroot',STATUS='OLD')
      READ(12,'(a)') root
      CLOSE (12)
      OPEN(UNIT=12,FILE='paramavn',STATUS='OLD')
      READ(12,'(a)') root_g
      CLOSE (12)

      CALL DEGRIBSURF(root(1:len_trim(root)),root_g(1:len_trim(root_g)))

      END
c
c===============================================================================
c
      SUBROUTINE DEGRIBSURF(root,root_g)
C
      include "../include/all_new.inc"
C
      character *(*) root, root_g
C
CIVAN-SURFACE
C
      REAL,ALLOCATABLE,DIMENSION(:,:) :: LAND
      REAL,ALLOCATABLE,DIMENSION(:,:) :: ICEC
      REAL,ALLOCATABLE,DIMENSION(:,:) :: SNOW
      REAL,ALLOCATABLE,DIMENSION(:,:) :: PMSL
      REAL,ALLOCATABLE,DIMENSION(:,:) :: PRES
      REAL,ALLOCATABLE,DIMENSION(:,:) :: ZSFC
      REAL,ALLOCATABLE,DIMENSION(:,:) :: TMPSFC
      REAL,ALLOCATABLE,DIMENSION(:,:) :: TSoil1
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Wsoil1
      REAL,ALLOCATABLE,DIMENSION(:,:) :: TSoil2
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Wsoil2
      REAL,ALLOCATABLE,DIMENSION(:,:) :: TSoil3
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Wsoil3
      REAL,ALLOCATABLE,DIMENSION(:,:) :: TSoil4
      REAL,ALLOCATABLE,DIMENSION(:,:) :: Wsoil4
C
      real HGT(IM,JM)
      real SM(IM,JM)
      real DUMMY(IM,JM)
C
      real SOILW1(IM,JM)
      real SOILW2(IM,JM)
      real SOILW3(IM,JM)
      real SOILW4(IM,JM)
      real SOILT1(IM,JM)
      real SOILT2(IM,JM)
      real SOILT3(IM,JM)
      real SOILT4(IM,JM)
      real SST(IM,JM)
      real SNO(IM,JM)
      real CICE(IM,JM)
      real STC(IM,JM,4)
      real SMC(IM,JM,4)
      real TG(IM,JM)

C-----------------------------------------------------------------------
                 d i m e n s i o n
     & coh(3,im,jm),inh(4,im,jm),jnh(4,im,jm)
     &,cov(3,im,jm),inv(4,im,jm),jnv(4,im,jm)
     &,ald(im,jm),apd(im,jm)
C-----------------------------------------------------------------------
      real*4 xe,mrsat
      real*4 esat,es,fs
      common /estab/esat(15000:45000),es(15000:45000)
C-----------------------------------------------------------------------
      character*8 cdate8
      character*3 cstart, cend, cint, chr3
      character*3 cswitch
      character*2 chr2
      character *2 gproj
      character(LEN=255):: gribfile
C-----------------------------------------------------------------------
      integer nx,ny,nz,kgds(200),kpds(200),gdsinfo(9),n !Degrib grid dimensions
      integer iunit,istart,iend,iint,ihr,id7,iswitch,irotate,it,idate8
      dimension id7(7)
C-----------------------------------------------------------------------
      dimension zzsoil(4)
      data zzsoil / 0.05,0.25, 0.75, 1.5 /, TI0 /271.16/, TIW /274.15/
C-----------------------------------------------------------------------
      iswitch=0
      if (iargc() .lt. 1) then
         write(*,*)'Usage: grads2pnt_avn_new date8 cswitch'
         stop
      else if (iargc() .eq. 2) then
         call getarg (2, cswitch)
         read(cswitch,*) iswitch
      end if
      call getarg (1, cdate8)
      call cdate2id7 (cdate8,id7)

      open(11,file=root//"/data/prep/cdate11")
      write(11,'(i4,3i2)') id7(1)+2000,id7(2),id7(3),id7(4)
      close (11)

      id7(1)=id7(1)+2000 ! RGRIB2 NEED FULL YEAR
C-----------------------------------------------------------------------
      call es_ini
C-----------------------------------------------------------------------
      gribfile=root//"/eta/runs/etatopo.dat"
      print *,'READ HGT,SM FROM:',gribfile
      n=index(gribfile,' ')-1
      OPEN(UNIT=22,FILE=gribfile(1:n),STATUS='OLD',FORM='UNFORMATTED')
      READ(22)HGT,SM
      CLOSE(22)
      IF (ISWITCH.NE.1) THEN
        gribfile=root//"/eta/input_data/hgt_sm.dat"
        print *,'READ HGT FROM:',gribfile
        n=index(gribfile,' ')-1
        OPEN(UNIT=22,FILE=gribfile(1:n),STATUS='OLD',FORM='UNFORMATTED')
        READ(22)HGT,DUMMY
        CLOSE(22)
      ENDIF
C-----------------------------------------------------------------------
      iunit=11
      IHR=0
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

      ALLOCATE(  LAND(1:imll,1:jmll))
      ALLOCATE(  ICEC(1:imll,1:jmll))
      ALLOCATE(  SNOW(1:imll,1:jmll))
      ALLOCATE(  PMSL(1:imll,1:jmll))
      ALLOCATE(  PRES(1:imll,1:jmll))
      ALLOCATE(  ZSFC(1:imll,1:jmll))
      ALLOCATE(TMPSFC(1:imll,1:jmll))
      ALLOCATE(TSoil1(1:imll,1:jmll))
      ALLOCATE(Wsoil1(1:imll,1:jmll))
      ALLOCATE(TSoil2(1:imll,1:jmll))
      ALLOCATE(Wsoil2(1:imll,1:jmll))
      ALLOCATE(TSoil3(1:imll,1:jmll))
      ALLOCATE(Wsoil3(1:imll,1:jmll))
      ALLOCATE(TSoil4(1:imll,1:jmll))
      ALLOCATE(Wsoil4(1:imll,1:jmll))

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

      read(cdate8,*)idate8
      if (idate8.ge.15011412) then
         it=2
         fs=1.
      else
         it=0
         fs=2.
      endif
C-----------------------------------------------------------------------
      CALL RGRIB2 (iunit,id7, 2,0,  0,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,    LAND,irotate) ! LANDSEA
      CALL RGRIB2 (iunit,id7,10,2,  0,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,    ICEC,irotate) ! SEAICE
      CALL RGRIB2 (iunit,id7, 0,1, 13,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,    SNOW,irotate) ! SNOW
      CALL RGRIB2 (iunit,id7, 0,3,  1,101,0,  0,255,0,  0,imll,jmll,     1,   0.,    PMSL,irotate) ! PMSL
      CALL RGRIB2 (iunit,id7, 0,3,  0,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,    PRES,irotate) ! PSFC
      CALL RGRIB2 (iunit,id7, 0,3,  5,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,    ZSFC,irotate) ! SOILHGT
      CALL RGRIB2 (iunit,id7, 0,0,  0,  1,0,  0,255,0,  0,imll,jmll,     1,   0.,  TMPSFC,irotate) ! SKINTEMP
      CALL RGRIB2 (iunit,id7,it,0, it,106,2,  0,106,2, 10,imll,jmll,     1,   0.,  TSoil1,irotate) ! SOILT   0-10
      CALL RGRIB2 (iunit,id7, 2,0,192,106,2,  0,106,2, 10,imll,jmll,     1,   0.,  WSoil1,irotate) ! SOILW   0-10
      CALL RGRIB2 (iunit,id7,it,0, it,106,2, 10,106,2, 40,imll,jmll,     1,   0.,  TSoil2,irotate) ! SOILT  10-40
      CALL RGRIB2 (iunit,id7, 2,0,192,106,2, 10,106,2, 40,imll,jmll,     1,   0.,  WSoil2,irotate) ! SOILW  10-40
      CALL RGRIB2 (iunit,id7,it,0, it,106,2, 40,106,2,100,imll,jmll,     1,   0.,  TSoil3,irotate) ! SOILT  40-100
      CALL RGRIB2 (iunit,id7, 2,0,192,106,2, 40,106,2,100,imll,jmll,     1,   0.,  WSoil3,irotate) ! SOILW  40-100
      CALL RGRIB2 (iunit,id7,it,0, it,106,2,100,106,2,200,imll,jmll,     1,   0.,  TSoil4,irotate) ! SOILT 100-200
      CALL RGRIB2 (iunit,id7, 2,0,192,106,2,100,106,2,200,imll,jmll,     1,   0.,  WSoil4,irotate) ! SOILW 100-200
C-----------------------------------------------------------------------

      call BACLOSE(iunit,IRET)
      if (IRET.ne.0) then
         write(6,*) 'BACLOSE: IRET =', IRET
         stop
      end if

C-----------------------------------------------------------------------
      if (kgds(1) .eq. 3) gproj='LC'
      if (kgds(1) .eq. 5) gproj='PS'
      if (kgds(1) .eq. 0) gproj='LL'
      call gtll(coh,inh,jnh,cov,inv,jnv,ald,apd,imll,jmll,gproj,kgds)
C-----------------------------------------------------------------------
      HGT=HGT*(1.-SM)
C KOREKCIJA GPM U METRE 
      ZSFC=ZSFC/9.81*10.
C PROBLEM SA TEMPERATUROM MORA
      DO I=1,imll
      DO J=1,jmll
        IF (LAND(I,J).eq.0.and.ZSFC(I,J).gt.1.) ZSFC(I,J)=100.
        IF (TSoil4(I,J).EQ.9.999e+20.AND.LAND(I,J).GT.0)LAND(I,J)=-1.
      END DO
      END DO
C-----------------------------------------------------------------------
      DO I=1,IM
      DO J=1,JM

       I0=inh(1,I,J)
       J0=jnh(1,I,J)
       I1=I0+1
       J1=J0+1
       PQ=coh(1,I,J)
       P=coh(2,I,J)
       Q=coh(3,I,J)
       IP=I0+P+0.5
       JQ=J0+Q+0.5
       MAXRAD=3

       IF (SM(I,J).LT.0.5) THEN
        HDIF_MIN=10000.
        call findland(imll,jmll,IP,JQ,ZSFC,HGT(I,J),LAND,IA,IB,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR LAND POINTS - UNDEFINED" 
       ELSE
        HDIF_MIN=50.
        IF (HGT(I,J).GT.0.) STOP "HGT SEA > 0 CHECK SEA MASK"
        call  findsea(imll,jmll,     IP,     JQ,ZSFC,HGT(I,J),LAND, IA, IB,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR SEA POINTS - UNDEFINED"  
C-----------------------------------------------------------------------
        call  findsea(imll,jmll,     I0,     J0,ZSFC,HGT(I,J),LAND,I00,J00,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR SEA POINTS - UNDEFINED"   
        call  findsea(imll,jmll,     I1,     J0,ZSFC,HGT(I,J),LAND,I10,J10,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR SEA POINTS - UNDEFINED"   
        call  findsea(imll,jmll,     I0,     J1,ZSFC,HGT(I,J),LAND,I01,J01,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR SEA POINTS - UNDEFINED"   
        call  findsea(imll,jmll,     I1,     J1,ZSFC,HGT(I,J),LAND,I11,J11,IVDEF,MAXRAD,HDIF_MIN)
        IF (IVDEF.EQ.0) STOP "ERROR FOR SEA POINTS - UNDEFINED"   
        SST(I,J)=TMPSFC(I00,J00)+P*(TMPSFC(I10,J10)-TMPSFC(I00,J00))
     &                     +Q*(TMPSFC(I01,J01)-TMPSFC(I00,J00))
     &   +PQ*(TMPSFC(I00,J00)-TMPSFC(I10,J10)-TMPSFC(I01,J01)+TMPSFC(I11,J11))
C-----------------------------------------------------------------------
       END IF

       CICE(I,J)=SM(I,J)*(0.5+SIGN(0.5,ICEC(IA,IB)-0.5))
       IF (CICE(I,J).EQ.1) THEN
         IF (TMPSFC(IA,IB).LT.TIW) THEN
CIVAN-ICE
           SST(I,J)=TMPSFC(IA,IB)
           TG(I,J)=TI0
         ELSE
CIVAN-ICE-TO-WATTER
           CICE(I,J)=0.
           SST(I,J)=AMAX1(TI0,SST(I,J))
           TG(I,J)=273.16
         ENDIF
       ELSEIF (SM(I,J).EQ.1) THEN
         SST(I,J)=AMAX1(TI0,SST(I,J))
         TG(I,J)=273.16
       END IF

       SOILW1(I,J)=(1-SM(I,J))*WSoil1(IA,IB)+SM(I,J)*1.0
       SOILW2(I,J)=(1-SM(I,J))*WSoil2(IA,IB)+SM(I,J)*1.0
       SOILW3(I,J)=(1-SM(I,J))*WSoil3(IA,IB)+SM(I,J)*1.0
       SOILW4(I,J)=(1-SM(I,J))*WSoil4(IA,IB)+SM(I,J)*1.0
       SOILT1(I,J)=(1-SM(I,J))*TSoil1(IA,IB)+SM(I,J)*SST(I,J)
       SOILT2(I,J)=(1-SM(I,J))*TSoil2(IA,IB)+SM(I,J)*SST(I,J)
       SOILT3(I,J)=(1-SM(I,J))*TSoil3(IA,IB)+SM(I,J)*SST(I,J)
       SOILT4(I,J)=(1-SM(I,J))*TSoil4(IA,IB)+(SM(I,J)-CICE(I,J))*SST(I,J)+CICE(I,J)*TG(I,J)
       SST(I,J)=(1-SM(I,J))*TMPSFC(IA,IB)+SM(I,J)*SST(I,J)
       SNO(I,J)=(1-SM(I,J)+CICE(I,J))*SNOW(IA,IB)*fs/1000.

       STC(I,J,1)=SOILT1(I,J)
       STC(I,J,2)=SOILT2(I,J)
       STC(I,J,3)=SOILT3(I,J)
       STC(I,J,4)=SOILT4(I,J)
       SMC(I,J,1)=SOILW1(I,J)
       SMC(I,J,2)=SOILW2(I,J)
       SMC(I,J,3)=SOILW3(I,J)
       SMC(I,J,4)=SOILW4(I,J)

       IF (SM(I,J).LT.0.5) TG(I,J)=SOILT4(I,J)

       DO K=1,4
         IF ( I.eq.2*IM/3.and.J.eq.2*JM/3) THEN
           print *,K,STC(I,J,K),SMC(I,J,K)
         END IF
       END DO
       IF ( I.eq.2*IM/3.and.J.eq.2*JM/3) THEN
         print *,'TG=',TG(I,J),'TSFC=',TMPSFC(IA,IB)
         print *,'TSoil1=',SOILT1(I,J),'TSoil2=',SOILT2(I,J),'TSoil3=',SOILT3(I,J),'TSoil4=',SOILT4(I,J)
         print *,'WSoilT1=',SOILW1(I,J),'WSoilT2=',SOILW2(I,J),'WSoilT3=',SOILW3(I,J),'WSoilT4=',SOILW4(I,J)
       END IF

      END DO
      END DO

      gribfile=root//"/eta/init_out/sfc.dat"
      n=index(gribfile,' ')-1

      OPEN(12,file=gribfile(1:n),form='unformatted')
      WRITE (12) CICE 
      WRITE (12) SNO 
      WRITE (12) SST 
      WRITE (12) STC 
      WRITE (12) SMC 
      WRITE (12) TG 
      CLOSE (12)

      DEALLOCATE(  LAND)
      DEALLOCATE(  ICEC)
      DEALLOCATE(  SNOW)
      DEALLOCATE(  PMSL)
      DEALLOCATE(  PRES)
      DEALLOCATE(  ZSFC)
      DEALLOCATE(TMPSFC)
      DEALLOCATE(TSoil1)
      DEALLOCATE(Wsoil1)
      DEALLOCATE(TSoil2)
      DEALLOCATE(Wsoil2)
      DEALLOCATE(TSoil3)
      DEALLOCATE(Wsoil3)
      DEALLOCATE(TSoil4)
      DEALLOCATE(Wsoil4)

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
         function q_tp (t,p,qs1)
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
          q_led = q_tp*(BI+AI*t)
          q_int = q_tp*(1.-0.00032*TMT15*(TMT15+15.))
          q_tp=q_int
          IF (t.lt.-15) q_tp=q_led
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

C-----------------------------------------------------------------------
      SUBROUTINE FINDLAND(IMLL,JMLL,I1,J1,HGT,ALT,LAND,IA,IB,IVDEF,MAXRAD,HDIF_MIN)

      REAL LAND(IMLL,JMLL),HGT(IMLL,JMLL)
      RMASKA=1.E20
C
          NRAD=1
          IBR=1
          NP=0
 256      DO JK=1,NRAD
          DO IK=1,NRAD
             NKORAK=(NRAD-1)/2+1
             I=I1-NKORAK+IK
             J=J1-NKORAK+JK
             IF (I.GE.1.AND.I.LE.IMLL.AND.J.GE.1.AND.J.LE.JMLL) THEN
             IF (LAND(I,J).LT.RMASKA) THEN
               ILA=LAND(I,J)
             ELSE
               ILA=0
             END IF
             IF (ILA.EQ.1) THEN
                HDIF=MAX(ALT,200.)-HGT(I,J)
                IF (HDIF.GT.0.AND.HDIF.LT.HDIF_MIN) THEN
                   NP=NP+1
                   HDIF_MIN=ABS(ALT-HGT(I,J))
                   IA=I
                   IB=J
                END IF
             END IF
             END IF
          END DO
          END DO

          IF (NP.EQ.0.AND.NRAD.LT.MAXRAD) THEN
            NRAD=NRAD+2
            GOTO 256
          END IF

          IF (NP.EQ.0.AND.NRAD.GE.MAXRAD) THEN
            IBR=IBR+1
            HDIF_MIN=HDIF_MIN*FLOAT(IBR)
            IF(IBR.eq.5)THEN
              IBR=1
              NRAD=NRAD+2
              HDIF_MIN=300.
            ENDIF
            GOTO 256
          END IF

 300      IVDEF=NP

      RETURN   
      END
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      SUBROUTINE FINDSEA(IMLL,JMLL,I1,J1,HGT,ALT,LAND,IA,IB,IVDEF,MAXRAD,HDIF_MIN)

      REAL LAND(IMLL,JMLL),HGT(IMLL,JMLL)
      RMASKA=1.E20
C
          NRAD=1
          NP=0
 256      DO JK=1,NRAD
          DO IK=1,NRAD
             NKORAK=(NRAD-1)/2+1
             I=I1-NKORAK+IK
             J=J1-NKORAK+JK
             IF (I.GE.1.AND.I.LE.IMLL.AND.J.GE.1.AND.J.LE.JMLL) THEN
             IF (LAND(I,J).LT.RMASKA) THEN
               ILA=LAND(I,J)
             ELSE
               ILA=-1
             END IF
             IF (ILA.EQ.0) THEN
                HDIF=ABS(HGT(I,J)-ALT)
                IF (HDIF.LE.HDIF_MIN) THEN
                   NP=NP+1
                   IA=I
                   IB=J
                   GO TO 320 
                END IF
             END IF
             END IF
          END DO
          END DO

          IF (NP.EQ.0.) THEN
            NRAD=NRAD+2
            GOTO 256
          END IF

 320      CONTINUE
          IVDEF=NP

      RETURN
      END
C-----------------------------------------------------------------------
      subroutine ngc (x0,x1,x2,v0,v1,v2,x,v)
      df2 = v2-2*v1+v0
      if ( x.gt.x1 ) then
           s   = (x-x1)/(x2-x1)
           f0  = v1
           df0 = v2-v1
      else
           s   = (x-x0)/(x1-x0)
           f0  = v0
           df0 = v1-v0
      endif
      v = f0 + s*df0+0.5*s*(s-1.)*df2
      return
      end
C-----------------------------------------------------------------------

