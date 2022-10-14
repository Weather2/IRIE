      PROGRAM IVAN VEGTEXTURE
C-----------------------------------------------------------------------
C
C     CODE TO PRODUCE VEG TEXTURE FOR ETA
C     I.RISTIC
C
C-----------------------------------------------------------------------
      include "../../include/all_new.inc"
C
      PARAMETER(DLD=1./120.,IMJM=IM*JM-JM/2)
      PARAMETER(IROW=360/DLD,JCOL=180/DLD,ratio=0.5,NTYPE=27)
      INTEGER*1 RTX
      INTEGER*1,ALLOCATABLE:: RTEX(:,:)
      INTEGER KBD(NTYPE)
C
      DIMENSION DUMMY(IM,JM)
      DIMENSION SM(IM,JM)
      DIMENSION ITEX(IM,JM)
      INTEGER IP,JP
C
      INCLUDE "mount_nl.nml"
      INCLUDE "soil_nl.nml"
C-----------------------------------------------------------------------
      DATA DTR/.01745329/
C-----------------------------------------------------------------------
      OPEN (UNIT=11,FILE='../../namelists/name.list',FORM='FORMATTED')
      READ(11,MOUNT_NL)
      READ(11,SOIL_NL)
      CLOSE(11)
C-----------------------------------------------------------------------
      OPEN(UNIT=22,FILE=FN_MOUNT13
     &    ,STATUS='UNKNOWN',FORM='UNFORMATTED')
      READ(22)DUMMY,SM
      CLOSE(22)
C-----------------------------------------------------------------------
      DO I=1,IM
      DO J=1,JM
      ITEX(I,J)=0
      END DO
      END DO
C-----------------------------------------------------------------------
      IRECL=1
      OPEN(UNIT=30,FILE='../input/VEG-USGS.30s',ACCESS='DIRECT'
     &,    RECL=IRECL,STATUS='UNKNOWN')
C
      call corners(im,jm,imjm,tph0d,tlm0d,dlmd,dphd,apmn,almn,apmx,almx)
C
      print *,'ETA grid window:'
      print *,'   Northwest =',apmx,almn
      print *,'   Southeast =',apmn,almx
      print *,' '
C
      cshift=DLD/2.
      ofset=5.
      IP1=NINT(( (almn-ofset)-cshift+180.)/DLD)+1
      IP1=MAX(1,IP1)
      IP2=NINT(( (almx+ofset)-cshift+180.)/DLD)+1
      IP2=MIN(IROW,IP2)
      JP1=NINT((-(apmx+ofset)-cshift+ 90.)/DLD)+1
      JP1=MAX(1,JP1)
      JP2=NINT((-(apmn-ofset)-cshift+ 90.)/DLD)+1
      JP2=MIN(JCOL,JP2)
      ALLOCATE(RTEX(IP1:IP2,JP1:JP2))
      DO J=JP1,JP2
      DO I=IP1,IP2
        IREC=(J-1)*43200+I
        READ(30,REC=IREC)RTEX(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
      wb=wbd*dtr
      sb=sbd*dtr
      tph0=tph0d*dtr
      tlm0=tlm0d*dtr
      ctph0=cos(tph0)
      stph0=sin(tph0)
      dlm=dlmd*dtr
      dph=dphd*dtr
      tdlm=dlm+dlm
      tph=sb-dph
C
C     HERE STARTS PROCESSING ROW BY ROW
C
      DO J=1,JM
        tph=tph+dph
        tlm=wb-tdlm+mod(j+1,2)*dlm
      DO I=1,IM
        tlm=tlm+tdlm

        krad=MAX(INT(dlmd*ratio/DLD),1)

        DO jj=1,NTYPE
          KBD(jj)=0
        ENDDO

        IF(SM(I,J).eq.0)THEN

          call rtll(tlm,tph,tlm0d,dtr,ctph0,stph0,almd,aphd)

          IP=NINT(( ALMD-cshift+180.)/DLD)+1
          IP=MIN(IP,IROW)
          IP=MAX(IP,1)

          JP=NINT((-APHD-cshift+ 90.)/DLD)+1
          JP=MIN(JP,JCOL)
          JP=MAX(JP,1)

  100     CONTINUE

          DO jj=JP-krad,JP+krad
          DO ii=IP-krad,IP+krad
            itt=min(ii,IROW)
            itt=max(itt,1)
            jtt=min(jj,JCOL)
            jtt=max(jtt,1)
            IF(RTEX(itt,jtt).GT.0.AND.RTEX(itt,jtt).LE.NTYPE)
     1      KBD(RTEX(itt,jtt))=KBD(RTEX(itt,jtt))+1
          ENDDO
          ENDDO

          ITEX(I,J)=0
          KMAX=0
          DO jj=1,NTYPE
            IF(KBD(jj).GT.KMAX.AND.jj.ne.16)THEN
              ITEX(I,J)=jj
              KMAX=KBD(jj)
            ENDIF
          ENDDO

          IF(ITEX(I,J).EQ.0)THEN
            IF(krad.gt.8*MAX(INT(dlmd*ratio/DLD),1))THEN
              ITEX(I,J)=2
            ELSE
              krad=krad*2.
              GOTO 100
            ENDIF
          ENDIF
        ENDIF

      END DO
      END DO

      OPEN(UNIT=23,FILE=FN_VEG
     &        ,STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(23)ITEX
      CLOSE (23)

      WRITE(6,*) 'sample VEG values'
      DO J=JM,1,-JM/30
        WRITE(6,'(31(I4,1x))') (ITEX(I,J),I=1,IM,IM/12)
      ENDDO

      END
C-----------------------------------------------------------------------

       subroutine CORNERS(im,jm,IMJM,tph0d,tlm0d,dlmd,dphd,minlat,wlon,
     +                                                maxlat,elon)
C
C
C     *  ROUTINE TO FIND EARTH LATITUDE/LONGITUDE FOR THE CORNER       *
C     *               POINTS OF AN ETA MODEL GRID                      *
C

C-----------------------------------------------------------------------
C                             D I M E N S I O N
C     & KHL0  (JM),KHH0  (JM), GLAT(IMJM),GLON(IMJM)

        REAL,ALLOCATABLE::GLAT(:),GLON(:)
        INTEGER,ALLOCATABLE::KHL0(:),KHH0(:)

                             D A T A
     & PI/3.141592654/

        REAL DLMD,DPHD,WBD,SBD,TPH0D,TLM0D,minlat,wlon,maxlat,elon

C*******************************
        WBD=-(float(IM)-1.)*DLMD
        SBD=(-(float(JM)-1.)/2.)*DPHD

      DTR=PI/180.
      TPH0=TPH0D*DTR
      WB=WBD*DTR
      SB=SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
C
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
C
        ALLOCATE(KHL0(JM),KHH0(JM))
        ALLOCATE(GLAT(IMJM),GLON(IMJM))

         DO 100 J=1,JM
      KHL0(J)=IM*(J-1)-(J-1)/2+1
      KHH0(J)=IM*J-J/2
C     WRITE(6,9999) J, KHL0(J), KHH0(J)
C9999 FORMAT(2X,3(I10,1X))
  100 CONTINUE
C--------------GEOGRAPHIC LAT AND LONG OF TLL GRID POINTS---------------
              TPH=SB-DPH
        maxlat=-999.
        minlat=99.
              DO 200 J=1,JM
              KHL=KHL0(J)
              KHH=KHH0(J)
C
              TLM=WB-TDLM+MOD(J+1,2)*DLM
              TPH=TPH+DPH
              STPH=SIN(TPH)
              CTPH=COS(TPH)
          DO 200 K=KHL,KHH
      TLM=TLM+TDLM
      SPH=CTPH0*STPH+STPH0*CTPH*COS(TLM)
      GLAT(K)=ASIN(SPH)
      CLM=CTPH*COS(TLM)/(COS(GLAT(K))*CTPH0)-TAN(GLAT(K))*TAN(TPH0)
          IF(CLM.GT.1.)      CLM=1.
      FACT=1.
          IF(TLM.GT.0.)      FACT=-1.
      GLON(K)=(-TLM0D*DTR+FACT*ACOS(CLM))/DTR

Cmp     at this point GLON is in DEGREES WEST
        if (GLON(K) .lt. 0) GLON(K)=GLON(K)+360.
        if (GLON(K) .gt. 360.) GLON(K)=GLON(K)-360.
        if (GLON(K) .lt. 180) GLON(K)=-GLON(K)         ! make WH negative
        if (GLON(K) .gt. 180) GLON(K)=360.-GLON(K)     ! make EH

        GLAT(K)=GLAT(K)/DTR

        if (glat(k) .gt. maxlat) maxlat=glat(k)
        if (glat(k) .lt. minlat) minlat=glat(k)

  200 CONTINUE

        DEALLOCATE(KHL0,KHH0)


        if (TPH0D .ge. 0) then
        wlon=glon(imjm-im+1)
        elon=glon(imjm)
        else
        wlon=glon(1)
        elon=glon(im)
        endif

C       write(6,*) 'raw lon values (w,e) ', wlon, elon
        if (tlm0d .lt. 0 .and. wlon .gt. 0) wlon=wlon-360.
        if (tlm0d .gt. 0 .and. elon .lt. 0) elon=elon+360.


        DEALLOCATE(GLAT,GLON)

      RETURN
      END
