      subroutine bilinb(cob,inb,jnb,im,jm,ime,jme,ww,wfbc)
!     **********************************************
!     *                                            *
!     *  routine for bilinear interpolation        *
!     *  of avn parameters into hibu bndry pts     *
!     *                                            *
!     **********************************************
      dimension cob(3,im,jm),inb(4,im,jm),jnb(4,im,jm)
      dimension ww(ime,jme),wfbc(im,jm)
!
              do j=1,jm
          do i=1,im

!
      i00=inb(1,i,j)
      i10=inb(2,i,j)
      i01=inb(3,i,j)
      i11=inb(4,i,j)
!
      j00=jnb(1,i,j)
      j10=jnb(2,i,j)
      j01=jnb(3,i,j)
      j11=jnb(4,i,j)
!
      p=cob(2,i,j)
      q=cob(3,i,j)
      pq=cob(1,i,j)
!
      wfbc(i,j)=ww(i00,j00)+p*(ww(i10,j10)-ww(i00,j00))
     &                     +q*(ww(i01,j01)-ww(i00,j00))
     &   +pq*(ww(i00,j00)-ww(i10,j10)-ww(i01,j01)+ww(i11,j11))
!
	if (i .eq. 1 .and. j .eq. 1) then
C	write(6,*) 'cob ', (cob(l,i,j),l=1,3)
C	write(6,*) 'inb ', (inb(l,i,j),l=1,4)
C	write(6,*) 'jnb ', (jnb(l,i,j),l=1,4)
	if (abs(wfbc(i,j)).gt.1 .and. abs(wfbc(i,j)).lt.100) then
C	write(6,*) 'ww values ',
C     +  ww(i00,j00),ww(i10,j10),
C     +  ww(i01,j01),ww(i11,j11)
C	write(6,*) 'output value: ', wfbc(i,j)
	endif

	endif
          enddo
              enddo
!-----------------------------------------------------------------------
      return
      end

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c===============================================================================
C
C
      subroutine gtll(coh,inh,jnh,cov,inv,jnv,ald,apd,ime,jme,gproj,gds)
!-----------------------------------------------------------------------
      include '../include/all_new.inc'
      parameter(dtr=3.141592654/180.)
!
      character*2 gproj
	integer gds(200)
                 d i m e n s i o n
     & coh(3,im,jm),inh(4,im,jm),jnh(4,im,jm)
     &,cov(3,im,jm),inv(4,im,jm),jnv(4,im,jm)
     &,ald(im,jm),apd(im,jm)
!--------------- hibu domain geometry-----------------------------------

      print *,' gtll = gtll ', ime,jme
	write(6,*) 'gds in  dutil: ', (gds(i),i=1,14)
      wb=wbd*dtr
      sb=sbd*dtr
      tph0=tph0d*dtr
      tlm0=tlm0d*dtr
      ctph0=cos(tph0)
      stph0=sin(tph0)
      dlm=dlmd*dtr
      dph=dphd*dtr
      tdlm=dlm+dlm
!-------------- entry to the hibu i,j loop -----------------------------
!               hibu height pts
!-----------------------------------------------------------------------
      tph=sb-dph
              do j=1,jm
          tph=tph+dph
          tlm=wb-tdlm+mod(j+1,2)*dlm
!
          do i=1,im
      tlm=tlm+tdlm
!------------- tll to ll conversion ------------------------------------
      call  rtll(tlm,tph,tlm0d,dtr,ctph0,stph0,almd,aphd)
!-------------conversion from -180,180 range to 0,360 range-------------
!      if(almd.lt.0.) almd=360.+almd
!--------------check if hibu pt is out of avn domain--------------------
	
	if (gproj .eq. 'PS') then
  	  call str_ij(aphd,almd,x,y,gds)
	elseif (gproj .eq. 'LC') then
	  call lcc_ij(aphd,almd,x,y,gds)
	elseif(gproj .eq. 'LL') then
 	  call ced_ij(aphd,almd,x,y,gds)
        endif

!
            if (x .lt. 1. .or. x .gt. float(ime) .or.
     .          y .lt. 1. .or. y. gt. float(jme)) then

        if (ime*gds(9).eq. 360000) then
        if (x .lt. 1) x=x+ime
        if (x .gt. ime) x=x-ime
        else
               print *,'hibupt outside domain i,j=',i,j,aphd,almd,x,y
               stop
	endif

             endif
!
!-----------------------------------------------------------------------
C      coh(2,i,j)=x/delon
C      coh(3,i,j)=y/delat
            coh(2,i,j)=x-float(int(x))
            coh(3,i,j)=y-float(int(y))

      coh(1,i,j)=coh(2,i,j)*coh(3,i,j)
!-----------------------------------------------------------------------
      inh(1,i,j)=int(x)
        if (inh(1,i,j) .eq. 0 ) then
           inh(1,i,j)=ime
        end if
      inh(2,i,j)=inh(1,i,j)+1
      inh(3,i,j)=inh(1,i,j)
      inh(4,i,j)=inh(1,i,j)+1
!
C
C
          if(inh(1,i,j).eq.ime) then
      inh(2,i,j)=1
      inh(4,i,j)=1
          endif
!
      jnh(1,i,j)=int(y)
      jnh(2,i,j)=jnh(1,i,j)
      jnh(3,i,j)=jnh(1,i,j)+1
      jnh(4,i,j)=jnh(1,i,j)+1
          enddo
              enddo
!-----------------------------------------------------------------------
!                      wind points
!-----------------------------------------------------------------------
      tph=sb-dph
              do j=1,jm
          tph=tph+dph
          tlm=wb-tdlm+mod(j,2)*dlm
!
          do i=1,im
      tlm=tlm+tdlm
!--------------------- tll to ll conversion ----------------------------
      call  rtll(tlm,tph,tlm0d,dtr,ctph0,stph0,almd,aphd)
      ald(i,j)=almd
      apd(i,j)=aphd
!-------------conversion from -180,180 range to 0,360 range-------------
!      if(almd.lt.0.) almd=360.+almd
!-----------------------------------------------------------------------
!
!
	if (gproj .eq. 'PS') then
  	  call str_ij(aphd,almd,x,y,gds)
	elseif (gproj .eq. 'LC') then
	  call lcc_ij(aphd,almd,x,y,gds)
	elseif(gproj .eq. 'LL') then
	  call ced_ij(aphd,almd,x,y,gds)
        endif

            if (x .lt. 1. .or. x .gt. float(ime) .or.
     .          y .lt. 1. .or. y. gt. float(jme)) then
        if (ime*gds(9).eq. 360000) then
        if (x .lt. 1) x=x+ime
        if (x .gt. ime) x=x-ime
        else
           print *,'wind point outside domain i,j=',i,j,aphd,almd,x,y
               stop
        endif
            endif

!
!-----------------------------------------------------------------------
      cov(2,i,j)=x-float(int(x))
      cov(3,i,j)=y-float(int(y))
      cov(1,i,j)=cov(2,i,j)*cov(3,i,j)
!-----------------------------------------------------------------------
      inv(1,i,j)=int(x)
	if (inv(1,i,j) .eq. 0) then
	   inv(1,i,j)=ime
	endif
      inv(2,i,j)=inv(1,i,j)+1
      inv(3,i,j)=inv(1,i,j)
      inv(4,i,j)=inv(1,i,j)+1
!
          if(inv(1,i,j).eq.ime) then
      inv(2,i,j)=1
      inv(4,i,j)=1
          endif
!
      jnv(1,i,j)=int(y)
      jnv(2,i,j)=jnv(1,i,j)
      jnv(3,i,j)=jnv(1,i,j)+1
      jnv(4,i,j)=jnv(1,i,j)+1
          enddo
              enddo
!-----------------------------------------------------------------------
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	 subroutine str_ij(RLAT,RLON,XPTS,YPTS,KGDS)
      INTEGER KGDS(200)
      REAL XPTS,YPTS,RLON,RLAT
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C
C       Hardwired for Grid 104
C      data kgds/005,147,110,-268,-139475,8,-105000,90755,90755,
C     +0,64,0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	fill=-999

        IM=KGDS(2)
        JM=KGDS(3)
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        IROT=MOD(KGDS(6)/8,2)
        ORIENT=KGDS(7)*1.E-3
        DX=KGDS(8)
        DY=KGDS(9)
        IPROJ=MOD(KGDS(10)/128,2)
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        H=(-1.)**IPROJ
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DXS=DX*HI
        DYS=DY*HJ
        DE=(1.+SIN(60./DPR))*RERTH
        DR=DE*COS(RLAT1/DPR)/(1+H*SIN(RLAT1/DPR))
        XP=1-H*SIN((RLON1-ORIENT)/DPR)*DR/DXS
        YP=1+COS((RLON1-ORIENT)/DPR)*DR/DYS
        DE2=DE**2
        XMIN=0
        XMAX=IM+1
        YMIN=0
        YMAX=JM+1
        NRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
        IOPT=-1
        IF(IOPT.EQ.-1) THEN
            IF(ABS(RLON).LE.360.AND.ABS(RLAT).LE.90.AND.
     &                                 H*RLAT.NE.-90) THEN
              DR=DE*TAN((90-H*RLAT)/2/DPR)
C       write(6,*) 'DE,DR ', de,dr
              XPTS=XP+H*SIN((RLON-ORIENT)/DPR)*DR/DXS
              YPTS=YP-COS((RLON-ORIENT)/DPR)*DR/DYS
C       write(6,*) 'xpts,ypts ', xpts,ypts
              IF(XPTS.GE.XMIN.AND.XPTS.LE.XMAX.AND.
     &           YPTS.GE.YMIN.AND.YPTS.LE.YMAX) THEN
                NRET=NRET+1
              ELSE
                XPTS=FILL
                YPTS=FILL
              ENDIF
            ELSE
              XPTS=FILL
              YPTS=FILL
            ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        return
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine lcc_ij(RLAT,RLON,XPTS,YPTS,KGDS)

C
C	based on iplib GDSWIZ03 by M. Iredell

C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     RLON     - REAL EARTH LONGITUDE IN DEGREES E
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL EARTH LATITUDE IN DEGREES N 
C                (ACCEPTABLE RANGE: -90. TO 90.)
C

C    OUTPUT ARGUMENT LIST:
C     XPTS     - REAL GRID X POINT COORDINATE 
C     YPTS     - REAL GRID Y POINT COORDINATE

	INTEGER KGDS(200)
	REAL XPTS,YPTS,RLON,RLAT
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)

Cmp	hardwired gds for 212 for the moment
C	data kgds/003,185,129,12190,-133459,8,-95000,40365,
C     +		40365,0,64,25000,25000,187*-1/

	fill=-999

	
        IM=KGDS(2)
        JM=KGDS(3)
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        IROT=MOD(KGDS(6)/8,2)
        ORIENT=KGDS(7)*1.E-3
        DX=KGDS(8)
        DY=KGDS(9)
        IPROJ=MOD(KGDS(10)/128,2)
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        RLATI1=KGDS(12)*1.E-3
        RLATI2=KGDS(13)*1.E-3
        H=(-1.)**IPROJ
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DXS=DX*HI
        DYS=DY*HJ
        IF(RLATI1.EQ.RLATI2) THEN
          AN=SIN(H*RLATI1/DPR)
        ELSE
          AN=LOG(COS(RLATI1/DPR)/COS(RLATI2/DPR))/
     &       LOG(TAN((H*RLATI1+90)/2/DPR)/TAN((H*RLATI2+90)/2/DPR))
        ENDIF
        DE=RERTH*COS(RLATI1/DPR)*TAN((H*RLATI1+90)/2/DPR)**AN/AN
        IF(H*RLAT1.EQ.90) THEN
          XP=1
          YP=1
        ELSE
          DR=DE/TAN((H*RLAT1+90)/2/DPR)**AN
          DLON1=MOD(RLON1-ORIENT+180+3600,360.)-180
          XP=1-H*SIN(AN*DLON1/DPR)*DR/DXS
          YP=1+COS(AN*DLON1/DPR)*DR/DYS
        ENDIF
        ANTR=1/(2*AN)
        DE2=DE**2
        XMIN=0
        XMAX=IM+1
        YMIN=0
        YMAX=JM+1
        NRET=0
C
C  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
C

            IF(ABS(RLON).LE.360.AND.ABS(RLAT).LE.90.AND.
     &                                 H*RLAT.NE.-90) THEN
              DR=DE*TAN((90-H*RLAT)/2/DPR)**AN
              DLON=MOD(RLON-ORIENT+180+3600,360.)-180
              XPTS=XP+H*SIN(AN*DLON/DPR)*DR/DXS
              YPTS=YP-COS(AN*DLON/DPR)*DR/DYS
C	write(6,*) 'executed... ', xpts,ypts
              IF(XPTS.GE.XMIN.AND.XPTS.LE.XMAX.AND.
     &           YPTS.GE.YMIN.AND.YPTS.LE.YMAX) THEN
                NRET=NRET+1
              ELSE
                XPTS=FILL
                YPTS=FILL
              ENDIF
            ELSE
              XPTS=FILL
              YPTS=FILL
            ENDIF

	return
	end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine ced_ij(RLAT,RLON,XPTS,YPTS,KGDS)

	integer kgds(200)


	fill=-999
	LROT=-999
	
	IM=KGDS(2)
        JM=KGDS(3)
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        RLAT2=KGDS(7)*1.E-3
        RLON2=KGDS(8)*1.E-3
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DLON=HI*(MOD(HI*(RLON2-RLON1)-1+3600,360.)+1)/(IM-1)
        DLAT=(RLAT2-RLAT1)/(JM-1)
        XMIN=0
        XMAX=IM+1
        IF(IM.EQ.NINT(360/ABS(DLON))) XMAX=IM+2
        YMIN=0
        YMAX=JM+1
        NRET=0

            IF(ABS(RLON).LE.360.AND.ABS(RLAT).LE.90) THEN
              XPTS=1+HI*MOD(HI*(RLON-RLON1)+3600,360.)/DLON
              YPTS=1+(RLAT-RLAT1)/DLAT
              IF(XPTS.GE.XMIN.AND.XPTS.LE.XMAX.AND.
     &           YPTS.GE.YMIN.AND.YPTS.LE.YMAX) THEN
                NRET=NRET+1
                IF(LROT.EQ.1) THEN
                  CROT=1
                  SROT=0
                ENDIF
              ELSE
                XPTS=FILL
                YPTS=FILL
              ENDIF
            ELSE
              XPTS=FILL
              YPTS=FILL
            ENDIF

	return
	end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine rtll(tlm,tph,tlm0d,dtr,ctph0,stph0,almd,aphd)
!     ****************************************************************
!     *                                                              *
!     *  programer: z. janjic, shmz, feb. 1981                       *
!     *  ammended:  z. janjic, ncep, jan. 1996                       *
!     *                                                              *
!     *  transformation from rotated lat-lon to lat-lon coordinates  *
!     ****************************************************************
!     ****************************************************************
!     *  tlm   - transformed longitude, rad.                         *
!     *  tph   - transformed latitude, rad.                          *
!     *  tlm0d - the angle of rotation of the transformed lat-lon    *
!     *          system in the longitudinal direction, degs          *
!     *  ctph0 - cos(tph0), tph0 is the angle of rotation of the     *
!     *          transformed lat-lon systemn in the latitudinal      *
!     *          direction, precomputed                              *
!     *  stph0 - sin(tph0), tph0 is the angle of rotation of the     *
!     *          transformed lat-lon systemn in the latitudinal      *
!     *          direction, precomputed                              *
!     *  almd  - geographical longitude, degs, range -180.,180       *
!     *  aphd  - geographical latitude,  degs, range - 90., 90.,     *
!     *          poles are singular                                  *
!     ****************************************************************
!
      stlm=sin(tlm)
      ctlm=cos(tlm)
      stph=sin(tph)
      ctph=cos(tph)
!
      sph=ctph0*stph+stph0*ctph*ctlm
      aph=asin(sph)
      aphd=aph/dtr
      anum=ctph*stlm
      denom=(ctlm*ctph-stph0*sph)/ctph0
      relm=atan2(anum,denom)
      almd=relm/dtr+tlm0d
!
      if(almd.gt. 180.)    almd=almd-360.
      if(almd.lt.-180.)    almd=almd+360.
!
      return
      end
