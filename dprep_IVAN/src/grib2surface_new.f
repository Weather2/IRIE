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
        gribfile=root_g//cdate8(7:8)//"/gblav."//cdate8//chr2
      else
        write(chr3,"(I3.3)")IHR
        gribfile=root_g//cdate8(7:8)//"/gblav."//cdate8//chr3
      end if

      n=index(gribfile,' ')-1
      iunit=iunit+1
      call get_gds(iunit,gribfile(1:n),gdsinfo,kpds)

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
         it=85
         fs=1.
      else
         it=11
         fs=2.
      endif
C-----------------------------------------------------------------------
      call RGRIB (iunit,id7,  81,   1,    0, 10,imll,jmll, 1, 0.,    LAND,irotate)
      call RGRIB (iunit,id7,  91,   1,    0, 10,imll,jmll, 1, 0.,    ICEC,irotate)
      call RGRIB (iunit,id7,  65,   1,    0, 10,imll,jmll, 1, 0.,    SNOW,irotate)
      call RGRIB (iunit,id7,   2, 102,    0, 10,imll,jmll, 1, 0.,    PMSL,irotate)
      call RGRIB (iunit,id7,   1,   1,    0, 10,imll,jmll, 1, 0.,    PRES,irotate)
      call RGRIB (iunit,id7,   7,   1,    0, 10,imll,jmll, 1, 0.,    ZSFC,irotate)
      call RGRIB (iunit,id7,  11,   1,    0, 10,imll,jmll, 1, 0.,  TMPSFC,irotate)
      call RGRIB (iunit,id7,  it, 112,   10, 10,imll,jmll, 1, 0.,  TSoil1,irotate)
      call RGRIB (iunit,id7, 144, 112,   10, 10,imll,jmll, 1, 0.,  WSoil1,irotate)
      call RGRIB (iunit,id7,  it, 112, 2600, 10,imll,jmll, 1, 0.,  TSoil2,irotate)
      call RGRIB (iunit,id7, 144, 112, 2600, 10,imll,jmll, 1, 0.,  WSoil2,irotate)
      call RGRIB (iunit,id7,  it, 112,10340, 10,imll,jmll, 1, 0.,  TSoil3,irotate)
      call RGRIB (iunit,id7, 144, 112,10340, 10,imll,jmll, 1, 0.,  WSoil3,irotate)
      call RGRIB (iunit,id7,  it, 112,25800, 10,imll,jmll, 1, 0.,  TSoil4,irotate)
      call RGRIB (iunit,id7, 144, 112,25800, 10,imll,jmll, 1, 0.,  WSoil4,irotate)
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
        IF (TSoil4(I,J).EQ.-99999.00.AND.LAND(I,J).GT.0)LAND(I,J)=-1.
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
       SNO(I,J)=(1-SM(I,J)+CICE(I,J))*SNOW(IA,IB)*fs/1000. ! NCEP GFS weasd is one-half of the NAM value.

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
      SUBROUTINE RGRIB (iunit,id7,ip,itl,il,itr,
     &       imll,jmll,lmll,pr,var3d,irotate)

      implicit none
      
      integer iunit,id7(7),ip,itl,il,itr,imll,jmll,lmll
      real pr(lmll)
      real var3d(imll,jmll,lmll)
      integer iret,irotate

      integer i,j,l
      integer jpds(200),jgds(200),nwords,knum,kgds(200),kpds(50)

      real var2d(imll,jmll)
      logical*1 bitmap(imll,jmll)
c
      jpds=-1
      jgds=-1

      jpds(8)   = id7(1) ! YEAR INCLUDING (CENTURY-1)
      jpds(9)   = id7(2) ! MONTH OF YEAR
      jpds(10)  = id7(3) ! DAY OF MONTH
      jpds(11)  = id7(4) ! HOUR OF DAY
      if (itr.eq.10) then
      jpds(14)  = id7(5) ! TR1
      jpds(15)  = -1     ! TR2
      else
      jpds(14)  = -1     ! TR1
      jpds(15)  = id7(5) ! TR2
      end if
      jpds(16)  = itr    ! TIME RANGE FLAG

      jpds(5)=ip
      jpds(6)=itl

      do l=1,lmll

        if (lmll.gt.1) then
           jpds(7)=nint(pr(l))
        else
           jpds(7)=il
        end if

        call getgb(iunit,0,imll*jmll,0,jpds,jgds,nwords,knum,kpds,kgds,bitmap,var2d,iret)

        if (iret .ne. 0) then
           print *,'PDS(5)=',jpds(5)
           print *,'PDS(6)=',jpds(6)
           print *,'PDS(7)=',jpds(7)
           print *,'imll=',imll
           print *,'jmll=',jmll
           print *,'imll*jmll=',imll*jmll
           if (iret.eq.96) then
              print *,'ERROR READING INDEX FILE'
           elseif (iret.eq.97) then
              print *,'ERROR READING GRIB FILE'
           elseif (iret.eq.98) then
              print *,'NUMBER OF DATA POINTS GREATER THAN imll*jmll'
           elseif (iret.eq.99) then
              print *,'REQUEST NOT FOUND'
           else
              print *,'OTHER W3FI63 GRIB UNPACKER RETURN CODE=',iret
           endif
           STOP "ERROR RGRIB"
        endif
c
        do j=1,jmll
        do i=1,imll
           if (.not.bitmap(i,j)) then
           var2d(i,j)=-99999.
           end if
        end do
        end do
c
        do j=1,jmll
        do i=1,imll
           if (irotate.eq.1) then
             var3d(i,j,l)=var2d(i,jmll-j+1)
           else
             var3d(i,j,l)=var2d(i,j)
           endif
        end do
        end do
c
      end do

      return
      end
c
c===============================================================================
c
      subroutine get_gds(iunit,gribfile,gdsinfo,kpds)

      implicit none

      character*(*) gribfile
      character*1 pds(50)

      integer*4 kgds(200),kpds(200),len,kerr
     .         ,lenpds,lenkgds,nwords,kpdsl
     .         ,j,k,gdsinfo(9),iret,knum,nxny,iunit
     .         ,gdsav,IRETO,JGDS(200),JPDS(200)
      parameter(nxny=1)
      real tmp(nxny)
      logical*1 bitmap(nxny)

C     GDSINFO   KGDS
C       (01)    (02)   - N(I) NR POINTS ON LATITUDE CIRCLE
C       (02)    (03)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
C       (03)    (04)   - LA(1) LATITUDE OF ORIGIN
C       (04)    (05)   - LO(1) LONGITUDE OF ORIGIN
C       (05)    (07)   - LA(2) LATITUDE OF EXTREME POINT
C       (06)    (08)   - LO(2) LONGITUDE OF EXTREME POINT
C       (07)    (09)   - DI LONGITUDINAL DIRECTION OF INCREMENT
C       (08)    (10)   - DJ LATITUDINAL DIRECTION INCREMENT
C       (09)    (11)   - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)

      JPDS=-1
      JGDS=-1

      len=index(gribfile//' ',' ')-1

      write(6,*) 'want to open ', gribfile(1:len)
      call baopenr(iunit,gribfile(1:len),IRETO)
      if (IRETO.ne.0) then
         write(6,*) 'BAOPEN in get_gds: ', IRETO
         stop
      end if

      if (IRETO .ne. 0) then
         print *,'Error opening unit=11, file name = ',gribfile(1:len)
     &          ,' iostat = ',kerr
         stop
      endif

      jpds(5)=7
      jpds(6)=100
      jpds(7)=500

      call getgb(iunit,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,BITMAP,tmp,iret)

      if (iret .ne. 0 .AND. iret .ne. 98) then
         print *,'PDS(5)=',jpds(5)
         print *,'PDS(6)=',jpds(6)
         print *,'PDS(7)=',jpds(7)
         if (iret.eq.96) then
            print *,'ERROR READING INDEX FILE'
         elseif (iret.eq.97) then
            print *,'ERROR READING GRIB FILE'
         elseif (iret.eq.99) then
            print *,'REQUEST NOT FOUND'
         else
            print *,'OTHER W3FI63 GRIB UNPACKER RETURN CODE',iret
         endif
         STOP "ERROR GET_GDS"
      endif

      gdsinfo(1)=KGDS(2)
      gdsinfo(2)=KGDS(3)
      gdsinfo(3)=KGDS(4)
      gdsinfo(4)=KGDS(5)
      gdsinfo(5)=KGDS(7)
      gdsinfo(6)=KGDS(8)
      gdsinfo(7)=KGDS(9)
      gdsinfo(8)=KGDS(10)
      gdsinfo(9)=KGDS(11)
c      write(6,*) "gdsinfo=",gdsinfo
c      write(6,*) "kgds=",kgds(1:20)
c      write(6,*) "kpds=",kpds(1:20)

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

