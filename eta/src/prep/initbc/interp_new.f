      subroutine eta_interp_new(nnn)
c
      implicit none

      include 'ecommons.h'
c
      integer*4 nxin,nyin,nzin
     .         ,jday,idat(5)
     .         ,i,l,n,ll,NN,nsfcfld,j,nnn
c
	integer gds(200)
C
      real,allocatable:: htin(:,:,:)
     .,mrin(:,:,:),crin(:,:,:),uwin(:,:,:),vwin(:,:,:),sfcgrid(:,:,:)

	real prin(nzin),dtr
c
      real*4 hthi(im,jm,nzin)
     .      ,mrhi(im,jm,nzin)
     .      ,crhi(im,jm,nzin)
     .      ,uwhi(im,jm,nzin)
     .      ,vwhi(im,jm,nzin)
     .	    ,sfcgridhi(im,jm,12),qmn,qmx

C	real coh(3,im,jm),inh(4,im,jm),jnh(4,im,jm)
C     &,cov(3,im,jm),inv(4,im,jm),jnv(4,im,jm)
C     &,ald(im,jm),apd(im,jm),hgt(im,jm),sm(im,jm)

	real,allocatable:: hgt(:,:)
	real sm(im,jm)



	parameter(dtr=.017453293)
c
      character*2 gproj
c
      logical first
      data first/.true./
c
      common /sectorsize/nxin,nyin,nzin
c
      save idat
c_______________________________________________________________________________
c
      print*, imjm,nzin,im,jm
c
c *** Get time increment for boundary conditions.
c
      if (first) then
         idat(5)=-nint(tboco)
         first=.false.
c        print *,'first:',idat(5)
      endif
c
c *** Fill date stamps.
c        idat(1) = month
c        idat(2) = day
c        idat(3) = year
c        idat(4) = start time (UTC)
c        idat(5) = fcst hour
c
      idat(1)=imonth
      idat(2)=idate
      idat(3)=iyear
      idat(4)=istrtim
      idat(5)=idat(5)+nint(tboco)
      idat(5)=(nnn-1)*tboco
c
c *** Read in input upper air gridded data set.
c
	ALLOCATE(HTIN(NXIN,NYIN,NZIN))
	ALLOCATE(UWIN(NXIN,NYIN,NZIN))
	ALLOCATE(VWIN(NXIN,NYIN,NZIN))
	ALLOCATE(MRIN(NXIN,NYIN,NZIN))
	ALLOCATE(CRIN(NXIN,NYIN,NZIN))
	ALLOCATE(SFCGRID(NXIN,NYIN,12))
      call getdata(init_in(nnn),prin,uwin,vwin,htin,mrin,crin,sfcgrid)

C	write(6,*) 'Q after getdata'
	do L=1,NZIN
	qmn=99999.
	qmx=-99999.
	DO J=1,NYIN
	do I=1,NXIN
	if (mrin(i,j,l) .gt. qmx) qmx=mrin(i,j,l)
	if (mrin(i,j,l) .lt. qmn) qmn=mrin(i,j,l)
	enddo
	enddo
C	write(6,*) 'raw mrin extremes: ', L,qmn,qmx
	enddo
	

C	write(6,*) 'back from getdata, prin(20) ', prin(20)
C	write(6,*) 'some ht vals ', htin(50,50,20)
c
c *** Horizontally interpolate input data from native grid to ETA grid.
c
	n=index(init_gdsdir,' ')-1

	write(6,*) 'trying to open ', init_gdsdir(1:n)

	open(unit=14,file=init_gdsdir(1:n),form='unformatted',
     +	access='sequential')
	rewind 14

	read(14) GDS

	write(6,*) 'gds(1-14): ', (gds(I),I=1,14)


	call hinterp(htin,gproj,hthi,nzin,1,gds)
	DEALLOCATE(HTIN)
	call hinterp(mrin,gproj,mrhi,nzin,2,gds)
	DEALLOCATE(MRIN)
	call hinterp(crin,gproj,crhi,nzin,2,gds)
	DEALLOCATE(CRIN)
Cmp
      do J=1,nzin
C      write(6,*) 'J,mrhi(16,53,J): ', J,mrhi(16,53,J)
      enddo
Cmp

	
	write(6,*) 'calling wind interp'
	call hinterp_wind(uwin,vwin,gproj,uwhi,vwhi,nzin,5,gds)
	DEALLOCATE(UWIN)
	DEALLOCATE(VWIN)


	call hinterp(sfcgrid,gproj,sfcgridhi,12,6,gds)

C
	ALLOCATE(HGT(IM,JM))
      open(1,file=topo_out,status='old',form='unformatted')
      rewind(1)
      read(1) hgt,sm
      close(1)
	DEALLOCATE(HGT)
C

	if (.not. REAN .and. GRIBSOIL .and. idat(5) .eq. 0) then
	write(6,*) 'calling coastal!!!!!!!'
	call coastalfix(sfcgridhi,sfcgrid,sm,im,jm,nxin,nyin,dphd,dlmd,
     +		wbd,sbd,tph0d,tlm0d,gds)
	write(6,*) 'return coastal'
	endif
	DEALLOCATE(SFCGRID)

c
c *** Vertically interpolate input data from native grid to ETA grid.
c *** And write output files.
c

	if (SIGMA) then
      call pusi   (prin,hthi,mrhi,crhi,uwhi,vwhi,sfcgridhi,nzin,idat)
	else
      call vinterp(prin,hthi,mrhi,crhi,uwhi,vwhi,sfcgridhi,nzin,idat)
	endif
c
      return
      end


C ********************* END MAIN ROUTINE ************************************
C
C
	subroutine hinterp(fieldin,gproj,fieldint,nzmax,mark,gds)
c
      implicit none
c
      include 'ecommons.h'
c
c *** Input data on native grid.
c
      integer*4 nxsec,nysec,nzsec,mark,L,nzmax,n
c
      real*4 fieldin(nxsec,nysec,nzsec)
c
c *** Input data on horizontal eta grid, native vertical grid.
c
      real*4 fieldint(im,jm,nzsec)

	REAL,ALLOCATABLE:: tmpout(:,:),tmp(:,:)
c
      real*4 coh(3,im,jm),inh(4,im,jm),jnh(4,im,jm)     
     .      ,cov(3,im,jm),inv(4,im,jm),jnv(4,im,jm)
     .      ,ald(im,jm),apd(im,jm)

      integer i,j,k
	integer GDS(200)
      character*2 gproj
c
      common /sectorsize/nxsec,nysec,nzsec
c
      print *,'Horizontally interpolate data to ETA grid.... ', mark
c
	if (gds(1) .eq. 3) gproj='LC'
	if (gds(1) .eq. 5) gproj='PS'
	if (gds(1) .eq. 0) gproj='LL'

	write(6,*) 'calling with gproj= ', gproj

      call gtll(coh,inh,jnh,cov,inv,jnv,ald,apd,nxsec,nysec,gproj,gds)
c
c *** Horizontally interpolate input data (ht, mr, u, and v) to ETA grid 
c        using bilinear interpolation.
c

	DO L=1,nzmax

	ALLOCATE (TMP(NXSEC,NYSEC))

	do j=1,nysec
	do I=1,nxsec
	tmp(i,J)=fieldin(i,j,l)
	enddo
	enddo

	ALLOCATE (TMPOUT(IM,JM))
	if (mark .ne. 4 .and. mark .ne. 5) then
        	call bilinb(coh,inh,jnh,im,jm,nxsec,nysec,tmp,tmpout) 
	else
C		write(6,*) 'interpolating winds '
		call bilinb(cov,inv,jnv,im,jm,nxsec,nysec,tmp,tmpout)
	endif

	DEALLOCATE(TMP)

	do j=1,jm
        do I=1,im
        fieldint(i,j,l)=tmpout(i,j)
        enddo
        enddo
	
	DEALLOCATE(TMPOUT)

	ENDDO
c
c *** Check that mixing ratio is not less than zero.
c
	if (mark .eq. 2) then
	write(6,*) 'checking mixing ratio for neg values'
        do k=1,nzsec
	do j=1,jm
        do i=1,im
         fieldint(i,j,k)=max(0.,fieldint(i,j,k))
        enddo
        enddo
	enddo
	endif
c 
      return
      end
c
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	subroutine hinterp_wind(uin,vin,gproj,uint,vint,nzmax,mark,gds)
c
      implicit none
c
      include 'ecommons.h'
c
c *** Input data on native grid.
c
      integer*4 nxsec,nysec,nzsec,mark,L,nzmax,n
c
      real*4 uin(nxsec,nysec,nzsec),vin(nxsec,nysec,nzsec)
c
c *** Input data on horizontal eta grid, native vertical grid.
c
      real*4 uint(im,jm,nzsec),vint(im,jm,nzsec)

	REAL,ALLOCATABLE:: utmpout(:,:),utmp(:,:)
	REAL,ALLOCATABLE:: vtmpout(:,:),vtmp(:,:)
	REAL,ALLOCATABLE:: utmpout1(:,:)
	REAL,ALLOCATABLE:: vtmpout1(:,:)
c
      real*4 coh(3,im,jm),inh(4,im,jm),jnh(4,im,jm)     
     .      ,cov(3,im,jm),inv(4,im,jm),jnv(4,im,jm)
     .      ,ald(im,jm),apd(im,jm),dtr

	parameter(dtr=.017453293)

      integer i,j,k
	integer GDS(200)
      character*2 gproj
c
      common /sectorsize/nxsec,nysec,nzsec
c
      print *,'Horizontally interpolate wind data to ETA grid.... '
c
	if (gds(1) .eq. 3) gproj='LC'
	if (gds(1) .eq. 5) gproj='PS'
	if (gds(1) .eq. 0) gproj='LL'

	write(6,*) 'calling with gproj= ', gproj

      call gtll(coh,inh,jnh,cov,inv,jnv,ald,apd,nxsec,nysec,gproj,gds)
c
c *** Horizontally interpolate input data (ht, mr, u, and v) to ETA grid 
c        using bilinear interpolation.
c

	DO L=1,nzmax

	ALLOCATE (UTMP(NXSEC,NYSEC))
	ALLOCATE (VTMP(NXSEC,NYSEC))

Cold	DO N=1,2

	do j=1,nysec
	do I=1,nxsec
	utmp(i,j)=uin(i,j,l)
	vtmp(i,j)=vin(i,j,l)
	enddo
	enddo

	ALLOCATE (UTMPOUT(IM,JM))
	ALLOCATE (VTMPOUT(IM,JM))
	ALLOCATE (UTMPOUT1(IM,JM))
	ALLOCATE (VTMPOUT1(IM,JM))

		call bilinb(cov,inv,jnv,im,jm,nxsec,nysec,utmp,utmpout)
		call bilinb(cov,inv,jnv,im,jm,nxsec,nysec,vtmp,vtmpout)

	DEALLOCATE(UTMP)
	DEALLOCATE(VTMP)

	 call ltlwin(ald,apd,utmpout,vtmpout,dtr,utmpout1,vtmpout1)

	do j=1,jm
        do I=1,im
	  uint(i,j,l)=utmpout1(i,j)
	  vint(i,j,l)=vtmpout1(i,j)
        enddo
        enddo

Cold	ENDDO

	DEALLOCATE(UTMPOUT)
	DEALLOCATE(VTMPOUT)
	DEALLOCATE(UTMPOUT1)
	DEALLOCATE(VTMPOUT1)

	ENDDO
c
        return
	end

c==========================================================================
C
      subroutine vinterp(spl,h,q,cr,u,v,sfcgrid,ldm,idat)
c
c *** Code to vertically interpolate gridded init data from
c        native vertical grid to ETA vertical grid.
c
c *** Original code received from the U. of Athens and modfied at FSL.
c
c *** Setting up of the vertical grid variables, and              
c     vertical interpolation, pressure to eta surfaces           
c     Fedor Mesinger                                            
c
      implicit none
c
      include 'ecommons.h'
      include 'econstants.h'
c
C	integer imxjm
      real*4 a2,a3,a4,pq0
      parameter (a2=17.2693882E0,a3=273.16E0,a4=35.86E0
     .          ,pq0=379.90516)
C
C	parameter(imxjm=im*jm)

      integer*4 ldm,negsum,ll,NN
c

C	ldm
	REAL H(IM,JM,LDM),U(IM,JM,LDM),V(IM,JM,LDM),Q(IM,JM,LDM)
	REAL CR(IM,JM,LDM)

C	lm
	REAL, ALLOCATABLE:: HETA(:,:,:),QETA(:,:,:),UETA(:,:,:)
	REAL, ALLOCATABLE:: CETA(:,:,:)
	REAL, ALLOCATABLE:: VETA(:,:,:),TETA(:,:,:),PDVP(:,:),PHIS(:,:)

	REAL, ALLOCATABLE:: HGT(:,:),SM(:,:),PD(:,:)

      real*4 
     .      ext(lm,2)
     .      ,hetaij
     .      ,sfcgrid(im,jm,12)

	
      real   uetaij(lm),vetaij(lm)
     .      ,href(ldm),eta(lmp1),peta(lmp1)
     .      ,pdb(kb,2),tb(kb,lm,2),ub(kb,lm,2),vb(kb,lm,2)
     .      ,qb(kb,lm,2),cb(kb,lm,2)
     .      ,etal(lm),zeta(lmp1),ref(im,jm)
     .      ,hfp(2,2),hfpa(im,jm),hrsd(im,jm),dz(im,jm)
     .      ,etas(im,jm),wm(im,jm)
     .      ,seta(4),deta(lm),dum(lm)
     .      ,alpq,bq
     .      ,q3,q3m1,q3m2,hldm,cq,rheta,tresh
     .      ,c3,c3m1,c3m2,bc,cc,c1,c2b
     .      ,tetam,cf,uld,vld,qseta,tetamax
     .      ,phub,phlb,q1,q2b,prfs,tmj2,gorg,rgog
     .      ,pt,dpd,dpu,x,h1b,h2,h3,rspl,splm,gfac,alpt
     .	    ,t1,t2,t3,t3m1,t3m2,bt,ct,zdif,adjfac,tsavetmp,
     .		ukb,vkb

Cmp
	real(8) alpeta,alpetai(im,jm), alp(ldm), alpr(ldm+1),alpsq(ldm)
	real(8) dlt(lm),alpub,alplb,b,c,alpgt,h3m1,h3m2,alpetk
c
      integer*4 lhgt(im,jm),lnew(im,jm),ldt1(lm),ldum(lm)
     .         ,idat(5),lmin(im,jm)
     .         ,mi2,ip1,jp1,jm1b,ldm1,ivi
     .         ,is,js,if,jf,l1,l2,l3,ldmm1,nk
     .         ,kt
     .         ,lnbmx,lmnn,lmne,lmns,lmnw,ld,lij,lrais
     .         ,mmx,ntsd,i,j,l,m,n,ihw(jm),ihe(jm),ivw(jm),ive(jm)
     .	       ,ivl,ivh,ihl,ihh,mnknt,idxx(im*jm),jdxx(im*jm),numk
     .	       ,kntx,idx(im*jm),jdx(im*jm),kntin,kp1
c
      character*3 fname
c
      logical wndls,hmask(im,jm),vmask(im,jm),run
c
      real*4 pdmin,pdmax, spl(ldm)

c_______________________________________________________________________________
c
      print *,'Vertically interpolate data to ETA grid.'
	write(6,*) 'input levels, output levels', LDM,LM
	negsum=0
      ldmm1=ldm-1
      tresh=0.95
c
      kt=idat(5)

Cmp
	pt=ptinp
Cmp
	write(6,*) 'in interp....pt= ', pt
	write(6,*) 'in interp....ptinp= ', ptinp
Czj
!-------------indirect indices for tom's set-up-------------------------
          do j=1,jm
      ihw(j)=-1+mod(j+1,2)
      ihe(j)=ihw(j)+1
      ivw(j)=-1+mod(j,2)
      ive(j)=ivw(j)+1
          enddo
!----------------------------------------------------------------------
Czj
c
c-----------------------------------------------------------------------
c     print=.true.
c        mountains to be four-point ('regular' or 'silhouette')
c        averaged, so as to have the same ground topography at groups
c        of four neighboring height points, or not four-point averaged
      fpmnts=.false.
c     siluet=.true.
c        hour00 fields to be created for a sigma mode/not a sigma mode
c        experiment
c     sigma=.false.
c        if a sigma mode experiment, mountains to be removed 6 grids
c        lines along the boundary/not to be removed
c     mrmsxl=.false.
cda      mrmsxl=.not.sigma
c        no mountains experiment/not a no mountains experiment
c     nomnts=.false.
c
c *** Define some constants needed for vertical interpolation.
c
      gorg=g/(r*gamma)
      rgog=r*gamma/g
      alpt=alog(pt)

c------- eta at the interfaces of the model layers ------------------

C	read in Eta levels from file

	open(unit=16,file='deta',form='unformatted',access=
     +				'sequential')

	REWIND 16
	READ(16)DETA,LDUM

        eta(1)=0.0

Cmp	DO N=1,LM-1
	DO N=1,LM
	ETA(N+1)=ETA(N)+DETA(N)
	write(6,*) 'DETA,ETA ', DETA(N),ETA(N+1)
	ENDDO

      eta(lmp1)=1.0
c
c *** Compute ldt1.
c
      do l=1,lm
         i=1
         peta(l)=(prf0-pt)*eta(l)+pt
Cnew
C	write(6,*) 'peta(l), spl(i), i ', peta(l),spl(i),i
         do while (peta(l) .ge. spl(i) .and. i .lt. ldm)
            peta(l)=(prf0-pt)*eta(l)+pt
            i=i+1
         enddo
c
         if (i .le. 2) then
            ldt1(l)=1
         elseif (i .eq. ldm) then
            ldt1(l)=ldm-2
         else
            dpu=abs(spl(i-2)-peta(l))
            dpd=abs(spl(i+1)-peta(l))
            if (dpu .gt. dpd) then
               ldt1(l)=i-1
            else
               ldt1(l)=i-2
            endif
         endif
      enddo
c
c *** Compute half eta values.
c
      do l=1,lm
         etal(l)=0.5*(eta(l)+eta(l+1))
      enddo
c
c *** Compute eta level heights.
c
	write(6,*) 'lowest zeta values'
      do l=1,lmp1
         zeta(l)=t0*(1.-((pt+eta(l)*(prf0-pt))/prf0)**rgog)/gamma
	if (l .ge. lmp1-10) then
	write(6,*) 'zeta(l)... ', l, zeta(l)
	endif
      enddo
c
c *** Read topo heights and sea masks.
C
C     This version expects hgt, sm to be (im,jm) and NOT (imt,jmt)
C	make sure etatopo.f handles this properly!!!!
C
	ALLOCATE(HGT(IM,JM))
	ALLOCATE(SM(IM,JM))
      open(1,file=topo_out,status='old',form='unformatted')
      rewind(1)
      read(1) hgt,sm
      close(1)


c *** Zero out heights, if no mountains option has been selected.
c
C      if (nomnts) then
C         do j=1,jm
C         do i=1,im
C            if (hgt(i,j).gt.0.) hgt(i,j)=0.004
C            ref(i,j)=1.
C         enddo
C         enddo
C         goto 922
C      endif

 702  if (sigma .and. .not. fpmnts) goto 922
c
      do 902 j=1,jm
      do 902 i=1,im
         prfs=prf0*((t0-gamma*hgt(i,j))/t0)**gorg
         etas(i,j)=(prfs-pt)/(prf0-pt)
C
C
Cmp	through this point hgt represents the unchanged raw hgt value
C
         do 903 ivi=1,lm
            l=lmp1-ivi
            if (etas(i,j).lt.eta(l)) goto 903
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            if (etas(i,j).lt.etal(l)) then
c           if (etas(i,j).lt.0.10*(9.*etal(l)+eta(l-1))) then
caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
               ref (i,j)= eta(l)
               hgt (i,j)=zeta(l)
               lhgt(i,j)=     l
            else
               ref (i,j)=eta(l+1)
               if (l+1 .eq. lmp1 .and. hgt(i,j) .gt. 0.) then
                  hgt(i,j)=0.004
               else
                  hgt(i,j)=zeta(l+1)
               endif
               lhgt(i,j)=    l+1
            endif
c
            goto 902
 903     continue
 902  continue
c
c
 922  continue
c
c     if(.not.fpmnts):
c     find points having no neighboring wind point because of mountains
c     on all four sides (sunken points), and
c     *  if land, just raise to reach the lowest wind;
c     *  if an isolated water point, or a water point with only one
c          neighboring water point and at sea level: raise, and declare
c          land (in order not to trigger convection over artificially
c          elevated warm water);
c     *  otherwise:
c          remove (level off) land at one of the four neighboring wind
c          points -- the one having the smallest three-point averaged
c          elevation -- so as to restore one of the wind points
c
c     n.b.: i am in the "old" i,j indexing, with i being incremented
c     in each column
c
      if (.not. fpmnts) then
c
c     Save first the sea mask, sm, into a back-up mask ("water mask",
c     wm) to use in testing the neighbors, since sm will be changed
c     along the way; and initialize lmin, level beyond which points
c     should not be raised because they have participated in leveling
c     off at that level, and lnew
c
         do j=1,jm
         do i=1,im
            wm  (i,j)=sm  (i,j)
            lmin(i,j)=2
            lnew(i,j)=lhgt(i,j)
         enddo
         enddo
c
        do j=3,jm-2
          ihl=2
          ihh=im-1-mod(j+1,2)

          do i=ihl,ihh

c
c ************ Find the maximum elevation (minimum l) of four neighboring wind
c              points -- to the west, east, south and north
c
      lmnw=min(lhgt(i+ihw(j),j-1),lhgt(i-1,j),lhgt(i+ihw(j),j+1))
      lmne=min(lhgt(i+ihe(j),j-1),lhgt(i+1,j),lhgt(i+ihe(j),j+1))
      lmns=min(lhgt(i,j-2),lhgt(i+ihw(j),j-1),lhgt(i+ihe(j),j-1))
      lmnn=min(lhgt(i+ihw(j),j+1),lhgt(i+ihe(j),j+1),lhgt(i,j+2))
c
c ************ Which one is the lowest of the four maximum elevations?
c
               lnbmx=max(lmnw,lmne,lmns,lmnn)
c
c ************ Is the point's original elevation below that of the lowest
c              available original wind, and, at the same time, in case it
c              participated in leveling off is it still below the level of the
c              lowest wind created by leveling off?
c
               if (lhgt(i,j) .gt. lnbmx .and. 
     .             ref(i,j) .gt. eta(lmin(i,j))) then
                  lrais=max0(lnbmx,lmin(i,j))
                  if (sm(i,j) .eq. 0.) then
                     ref (i,j)= eta(lrais)
                     hgt (i,j)=zeta(lrais)
                     lnew(i,j)=     lrais
c
             else if(wm(i+ihw(j),j-1)+wm(i+ihe(j),j-1)
     &               +wm(i+ihw(j),j+1)+wm(i+ihe(j),j+1).eq.0..or.
     &                wm(i+ihw(j),j-1)+wm(i+ihe(j),j-1)
     &               +wm(i+ihw(j),j+1)+wm(i+ihe(j),j+1).eq.1..and.
     &                  lhgt(i,j).eq.lm+1) then

                     ref (i,j)= eta(lrais)
                     hgt (i,j)=zeta(lrais)
                     lnew(i,j)=     lrais
c
                  else
c
c *************** There is a neighboring water point, and if it is only a
c                 single neighboring water point the water points are above sea
c                 level.  denote wind points to the east, north, west
c                 and south by 1, 2, 3, and 4, respectively, and check the
c                 three-point volume at each of them.
c
      seta(1)=etas(i+ihe(j),j-1)+etas(i+1,j)+etas(i+ihe(j),j+1)
      seta(2)=etas(i+ihw(j),j+1)+etas(i+ihe(j),j+1)+etas(i,j+2)
      seta(3)=etas(i+ihw(j),j-1)+etas(i-1,j)+etas(i+ihw(j),j+1)
      seta(4)=etas(i,j-2)+etas(i+ihw(j),j-1)+etas(i+ihe(j),j-1)
c
                     mmx=1
                     do m=2,4
                        if(seta(m) .gt. seta(mmx)) mmx=m
                     enddo
c
c ****************** n.b.: When removing now the obstructing land around the 
c                    wind point vm, lhgt is not changed.  Thus, hopefully (?) 
c                    resulting topography does not depend on the direction of 
c                    the sweep within the current loop.
c
                     lij=lhgt(i,j)
                     if (mmx .eq. 1 .or. mmx .eq. 4) then
             		if(ref(i+ihe(j),j-1).lt.eta(lij)) then
             			ref(i+ihe(j),j-1)=eta(lij)
             			hgt(i+ihe(j),j-1)=zeta(lij)
             			lnew(i+ihe(j),j-1)=lij
		     	endif
         		lmin(i+ihe(j),j-1)=max(lmin(i+ihe(j),j-1),lij)
                     endif
c
                     if (mmx .eq. 1) then
                        if (ref(i+1,j) .lt. eta(lij)) then
                           ref(i+1,j    )=   eta(lij)
                           hgt(i+1,j    )=  zeta(lij)
                           lnew(i+1,j    )=       lij
                        endif
                        lmin(i+1,j)=max(lmin(i+1,j),lij)
                     endif
c
                      if (mmx .eq. 1 .or. mmx .eq. 2) then
       	              if(ref(i+ihe(j),j+1).lt.eta(lij)) then
       			       ref(i+ihe(j),j+1)=eta(lij)
              		       hgt(i+ihe(j),j+1)=zeta(lij)
              	               lnew(i+ihe(j),j+1)=lij
                      endif
          	      lmin(i+ihe(j),j+1)=max(lmin(i+ihe(j),j+1),lij)
                      endif
c
                     if (mmx .eq. 2) then
                        if (ref(i,j+1) .lt. eta(lij)) then
                           ref(i  ,j+2  )=   eta(lij)
                           hgt(i  ,j+2  )=  zeta(lij)
                          lnew(i  ,j+2  )=       lij
                        endif
                        lmin(i,j+2)=max(lmin(i,j+2),lij)
                     endif
c
                     if (mmx .eq. 2 .or. mmx .eq. 3) then
                 	if(ref(i+ihw(j),j+1).lt.eta(lij)) then
             		  ref(i+ihw(j),j+1)=eta(lij)
              		  hgt(i+ihw(j),j+1)=zeta(lij)
              		  lnew(i+ihw(j),j+1)=lij
                  	endif
          		lmin(i+ihw(j),j+1)=max(lmin(i+ihw(j),j+1),lij)
                     endif
c
                     if (mmx .eq. 3) then
                        if (ref(i-1,j) .lt. eta(lij)) then
                           ref(i-1,j)=   eta(lij)
                           hgt(i-1,j)=  zeta(lij)
                           lnew(i-1,j)=       lij
                        endif
                        lmin(i-1,j)=max(lmin(i-1,j),lij)
                     endif
c
                     if (mmx .eq. 3 .or. mmx .eq. 4) then
                 if(ref(i+ihw(j),j-1).lt.eta(lij)) then
              ref(i+ihw(j),j-1)=eta(lij)
              hgt(i+ihw(j),j-1)=zeta(lij)
              lnew(i+ihw(j),j-1)=lij
                        endif
          lmin(i+ihw(j),j-1)=max(lmin(i+ihw(j),j-1),lij)
                     endif
c
                     if (mmx .eq. 4) then
                  if(ref(i,j-2).lt.eta(lij)) then
              ref(i,j-2)=eta(lij)
              hgt(i,j-2)=zeta(lij)
              lnew(i,j-2)=lij
                  endif
          lmin(i,j-2)=max(lmin(i,j-2),lij)
                     endif
c
                  endif
               endif
            enddo
         enddo
c
c ****** Any windless points left or inadvertently created?
c
         wndls=.false.
	do j=3,jm-2
          ihl=2
          ihh=im-1-mod(j+1,2)
!
          do i=ihl,ihh
!

c ************ Find the maximum elevation (minimum l) of four neighboring wind
c              points -- to the west, east, south and north
c
      lmnw=min(lnew(i+ihw(j),j-1),lnew(i-1,j),lnew(i+ihw(j),j+1))
      lmne=min(lnew(i+ihe(j),j-1),lnew(i+1,j),lnew(i+ihe(j),j+1))
      lmns=min(lnew(i,j-2),lnew(i+ihw(j),j-1),lnew(i+ihe(j),j-1))
      lmnn=min(lnew(i+ihw(j),j+1),lnew(i+ihe(j),j+1),lnew(i,j+2))
c
c ************ Which one is the lowest of the four maximum elevations?
c
               lnbmx=max(lmnw,lmne,lmns,lmnn)
c
c ************ Is the point's new elevation below that of the lowest available
c              new wind?
c
               if (lnew(i,j) .gt. lnbmx) then
                  wndls=.true.
                  print *,'WINDLESS',sm(i,j),I,J,lnbmx
                  ref (i,j)= eta(lnbmx)
                  hgt (i,j)=zeta(lnbmx)
                  lnew(i,j)=     lnbmx
               endif
            enddo
         enddo
c
      endif
c
      if (sigma) then
         do j=1,jm
         do i=1,im
            ref(i,j)=1.
         enddo
         enddo
      endif
c
c *** Masks.
      do j=1,jm
      do i=1,im
         hmask(i,j)=.false.
         vmask(i,j)=.false.
      enddo
      enddo
!-----------------------------------------------------------------------
      do i=1,im
         hmask(i,1)=.true.
         hmask(i,jm)=.true.
      enddo
      do j=1,jm,2
         hmask(1,j)=.true.
         hmask(im,j)=.true.
      enddo
!-----------------------------------------------------------------------
      do i=1,im
         vmask(i,1)=.true.
         vmask(i,jm)=.true.
      enddo
      do j=2,jm-1,2
         vmask(1,j)=.true.
         vmask(im,j)=.true.
      enddo
!-----------------------------------------------------------------------
c
c *** Vertical interpolation: quadratic interpolation of
c        heights (equivalent to temperature being linear in ln p)
c        and linear interpolation of winds.
c
c *** Computation of the 'sea level' pressure difference.
c
      splm=spl(ldm)
      rspl=splm/spl(ldmm1)
      do ld=1,ldmm1
         alpr(ld)=alog(spl(ld+1)/spl(ld))
      enddo

      do ld=1,ldm
Cmp	ln of preset pressure levels
         alp(ld)=alog(spl(ld))
         alpsq(ld)=alp(ld)**2
      enddo

      do l=1,lm
         l1=ldt1(l)
Cmakesworse    dlt(l)=alpr(l1+1)*alog(spl(l1+2)/spl(l1))*(-alpr(l1))
         dlt(l)=alpr(l1+1)*(alp(l1+2)-alp(l1))*(-alpr(l1))
      enddo
c

Cmp *************** START FIRST SET OF LOOPS ********************

	ALLOCATE(PD(IM,JM))

	do j=1,jm
      do i=1,im

Cmp	approach started 2/5/99....use lhgt to only use the closest
Cmp	to ground surface data...not subground data

C            l=lm   
	    l=min(lm,lhgt(i,j))

            l1=ldt1(l)
            l2=l1+1
            l3=l1+2
            h1b=h(i,j,l1)
            h2 =h(i,j,l2)
            h3 =h(i,j,l3)

            h3m2=h3-h2
            h3m1=h3-h1b
csd
Cmp reinstate
Cremove            if (kt .eq. 0 .or. hmask(i,j)) then
csd
               b=(h3m2*(alpsq(l3)-alpsq(l1))
     .           -h3m1*(alpsq(l3)-alpsq(l2)))/dlt(l)
               c=(h3m1*(alp(l3)-alp(l2))
     .           -h3m2*(alp(l3)-alp(l1)))/dlt(l)


cdule          if (abs(c) .lt .0.00001) goto 532
               if (abs(c) .lt. 0.1) then
		write(6,*) 'small abs(c).... ', i,j,c
		 goto 532
		endif
Cmp
        if (b**2-4.*c*(-c*alpsq(l3)-b*alp(l3)+h3-hgt(i,j)).lt.0) then
	negsum=negsum+1
	goto 532
	endif

               alpgt=(-b-sqrt(b**2-4.*c
     .              *(-c*alpsq(l3)-b*alp(l3)+h3-hgt(i,j))))/(2.*c)
		
C	pd for "normal" points

Cmp	want to save the pd value to an array so have when do rest
Cmp	of calculations below

	pd(i,j)=(exp(alpgt)-pt)/ref(i,j)

               goto 533
  532          hldm=h(i,j,ldm)

Cmp vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
               pd(i,j)=(splm*rspl**((hldm-hgt(i,j))
     .              /(h(i,j,ldmm1)-hldm))-pt)
     .              /ref(i,j)

Cmp ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

	write(6,*) 'special case.... ', i,j,pd(i,j)
	write(6,969) hldm,hgt(i,j),h(i,j,ldmm1),ref(i,j)
  969	format(' hldm,hgt,h,ref ',3(f8.2,x),f7.5)
	write(6,*) '.........................................'

Cmp *** when pd(i,j) is large, alpeta is larger than normal
Cmp ***	then when take differences with alp() and alpsq() which are functions
Cmp *** of the input data set pressure levels get values of hetaij which do
Cmp *** not match well with surrounding regions

  533          continue 
Cmp	if (i .ge. IM-1) write(6,*) 'i,j,pd,ref ', i,j,pd(i,j),ref(i,j)
Cmp	reinstate
Cremove	endif
         enddo
      enddo

C*************************************************************
C	NEW STUFF                 ****************************
C*************************************************************

	ALLOCATE(HETA(IM,JM,LM))
	ALLOCATE(QETA(IM,JM,LM))
	ALLOCATE(CETA(IM,JM,LM))

	do l=lm,1,-1
         do j=1,jm
      do i=1,im

            l1=ldt1(l)
Cmp
C	if (l1 .eq. ldm-2)  then
C	write(6,*) 'changing L1!!!!!!'
C	l1=l1-1
C	endif
Cmp
            l2=l1+1
            l3=l1+2
            h1b=h(i,j,l1)
            h2 =h(i,j,l2)
            h3 =h(i,j,l3)

Cmp
	if (I .eq. 51 .and. J .eq. 15) then
C	do LL=1,ldm
C	write(6,*) 'L1, input Z ',L1,h1b,h2,h3
C	enddo
	endif
Cmp

            h3m2=h3-h2
            h3m1=h3-h1b

Cremove       if (kt .eq. 0 .or. hmask(i,j)) then

               b=(h3m2*(alpsq(l3)-alpsq(l1))
     .           -h3m1*(alpsq(l3)-alpsq(l2)))/dlt(l)
               c=(h3m1*(alp(l3)-alp(l2))
     .           -h3m2*(alp(l3)-alp(l1)))/dlt(l)

c ************ Computation of heights at upper boundaries of eta layers.
c
               q1 =q(i,j,l1)
               q2b=q(i,j,l2)
               q3 =q(i,j,l3)
               q3m2=q3-q2b
               q3m1=q3-q1

               bq=(q3m2*(alpsq(l3)-alpsq(l1))
     .            -q3m1*(alpsq(l3)-alpsq(l2)))/dlt(l)
               cq=(q3m1*(alp(l3)-alp(l2))
     .            -q3m2*(alp(l3)-alp(l1)))/dlt(l)

               c1 =cr(i,j,l1)
               c2b=cr(i,j,l2)
               c3 =cr(i,j,l3)
               c3m2=c3-c2b
               c3m1=c3-c1

               bc=(c3m2*(alpsq(l3)-alpsq(l1))
     .            -c3m1*(alpsq(l3)-alpsq(l2)))/dlt(l)
               cc=(c3m1*(alp(l3)-alp(l2))
     .            -c3m2*(alp(l3)-alp(l1)))/dlt(l)


C              alpeta=alog(pt+pd(i,j)*eta(l))
              alpeta=alog(pt+pd(i,j)*eta(l))
             alpq=alog(pt+pd(i,j)*(eta(l)+0.5*deta(l)))
         hetaij=h2+b*(alpeta-alp(l2))+c*(alpeta**2-alpsq(l2))

Ctest	heta(i,j,l)=amax1(hetaij,0.01)
	heta(i,j,l)=hetaij
         qeta(i,j,l) = q2b+bq*(alpq-alp(l2)) +cq*(alpq**2-alpsq(l2))
         ceta(i,j,l) = c2b+bc*(alpq-alp(l2)) +cc*(alpq**2-alpsq(l2))

        if (heta(i,j,l) .lt. -120)  then
C      write(6,*) 'NEG heta:', i,j,l,heta(i,j,l),h1b,h2,h3,pt+pd(i,j)
C      write(6,611) alpeta, alp(l2), b, c
C      write(6,*) '---------------------------------------------'
	endif

Cremove        endif

	enddo
	enddo
	enddo
Cmp
C       write(6,*) 'Q values after vinterp: '
 
       do J=1,LM
C       write(6,*) 'J,Q(16,53,J): ', J,QETA(16,53,J)
       enddo
Cmp

  611	format('alpeta, alp(l2), b, c ',4(f14.6,1x))

C	STOP

C ****************************************************************
C****************** end of new stuff *****************************
C*****************************************************************

	pdmax=0.
	pdmin=99999.

	do J=1,JM
	do I=1,IM
	if (pd(I,J)*ref(I,J).lt.pdmin .and. pd(I,J).gt.1000.)
     +	 pdmin=pd(I,J)*ref(I,J)
	if (pd(I,J)*ref(I,j).gt.pdmax) then
	pdmax=pd(I,J)*ref(I,J)
C	write(6,*) 'I,J,pd,ref ',i,j,pd(i,j),ref(i,j),pdmax
	endif
	enddo
	enddo

	write(6,*) 'in interp, pdmin (surface based)= ', pdmin
	write(6,*) 'in interp, pdmax (surface based)= ', pdmax
c
c *** Computation of winds within eta layers.
c
C      do 519 j=1,jm
C	    ivl=1
C            ivh=im-mod(j,2)
Cmytest            ivh=im
C      do 519 i=ivl,ivh
C            if (j .eq. 1  .or. j .eq. jm) goto 520
Corig            if ( (i .eq.1 .and. mod(j+1,2).eq.0) .or. i.eq.im) goto 521
C        if ( (i .eq.1 .and. mod(j,2).eq.0) .or. i.eq.im) goto 521
C	    pdvp=0.25*(pd(i,j-1)+pd(i+ivw(j),j)+pd(i+ive(j),j)+pd(i,j+1))
C            goto 522
C  520       pdvp = 0.50*(pd(i+ivw(j),j)+pd(i+ive(j),j))
C            goto 522
C  521       pdvp = 0.50*(pd(i,j-1)+pd(i,j+1))
c

	ALLOCATE(PDVP(IM,JM))

      DO J=2,JM-1
       DO I=2,IM-1
          PDVP(I,J)=0.25*(PD(I+IVE(J),J)+PD(I+IVW(J),J)
     1                 +PD(I,J+1)+PD(I,J-1))
       ENDDO
      ENDDO
C***
C***  COMPUTE NORTH AND SOUTH EDGES AND RECOMPUTE EAST AND WEST SIDES
C***
      DO I=1,IM-1
        PDVP(I,1) =0.5*(PD(I+IVE(1),1)+PD(I+IVW(1),1))
        PDVP(I,JM)=0.5*(PD(I+IVE(JM),JM)+PD(I+IVW(JM),JM))
      ENDDO
      DO J=2,JM-1,2
        PDVP(1,J) =0.5*(PD(1,J+1)+PD(1,J-1))
        PDVP(IM,J)=0.5*(PD(IM,J+1)+PD(IM,J-1))
      ENDDO
      DO J=1,JM,2
        PDVP(IM,J)=PDVP(IM-1,J)
      ENDDO
      DO J=3,JM-2,2
        PDVP(1,J)=0.25*(PD(1+IVE(J),J)+PD(1+IVW(J),J)
     1                 +PD(1,J+1)+PD(1,J-1))
      ENDDO
C***
C***  COMPUTE WINDS (SINCE FIX IS HARDWIRED TO 0. FOR ALL K,
C***                 SKIP CHECKING THE VALUE OF FIX AND PROCEED)
C***

	ALLOCATE(UETA(IM,JM,LM))
	ALLOCATE(VETA(IM,JM,LM))

	write(6,*) 'past wind allocates'

      DO J=1,JM
      DO I=1,IM
        NN=(J-1)*IM+I
        IDXX(NN)=I
        JDXX(NN)=J
      ENDDO
      ENDDO

      NUMK=IM*JM
C      DO NN=1,NK
C        IDX(NN)=IDXX(NN)
C        JDX(NN)=JDXX(NN)
C      ENDDO

      MNKNT=1

      DO 400 L=1,LM
C      KNTX=MNKNT
C      KNTIN=KNTX
C      KP1=MIN(LM,KNTIN+1)
C      NUMK=0
C        ALPETAI(I,J)=ALOG(PT+PDVP(I,J)*ETAL(L))
C
cdir$ ivdep
      DO N=1,NUMK
        I=IDXX(N)
        J=JDXX(N)
        ALPETAI(I,J)=ALOG(PT+PDVP(I,J)*ETAL(L))
        ALPETK=ALPETAI(I,J)


	do 524 ld=2,ldm
	if (ALPETK .GT. ALP(LD) .AND. LD .LT. LDM) GOTO 524
          ULD=U(I,J,LD)
          VLD=V(I,J,LD)
          CF=(ALP(LD)-ALPETK)/ALPR(LD-1)
          UKB=ULD+(U(I,J,LD-1)-ULD)*CF
          VKB=VLD+(V(I,J,LD-1)-VLD)*CF
          UETA(I,J,L)=UKB
          VETA(I,J,L)=VKB
	GOTO 523
 524  continue
 523  continue
	ENDDO
  400 CONTINUE

	DEALLOCATE(PDVP)

	write(6,*) 'past PDVP deallocate'


C	do j=1,jm
C	ivh=im-mod(j,2)
C	do i=1,ivh
C        do l=1,lm
C               if (l .eq. 1) alplb=alpt
C               alpub=alplb
C               alplb=alog(pt+pdvp(i,j)*eta(l+1))
C               alpeta=sqrt(0.5*(alpub**2+alplb**2))
C               do 524 ld=2,ldm
C                  if (alpeta .gt. alp(ld) .and. ld .lt. ldm) goto 524
C                  uld=u(i,j,ld)
C                  vld=v(i,j,ld)
C                  ldm1=ld-1
C                  cf=(alp(ld)-alpeta)/alpr(ldm1)
C                  uetaij(l)=uld+(u(i,j,ldm1)-uld)*cf
C                  vetaij(l)=vld+(v(i,j,ldm1)-vld)*cf
C	if (abs(uetaij(l)) .gt. 100.) then
C	write(6,*) uld, u(i,j,ldm1), cf
C	endif
C                  goto 523
C  524          continue
C  523       continue

C	do l=1,lm
C	ueta(i,j,l)=uetaij(l)
C	veta(i,j,l)=vetaij(l)
C	enddo

C	enddo
C	enddo
c
c
c *** Ground surface heights converted to geopotentials, and 
c        geopotential to temperature inversion.
c
	ALLOCATE(PHIS(IM,JM))
	ALLOCATE(TETA(IM,JM,LM))
      do j=1,jm
C	write(6,*) 'looping for J=  ', J
      do i=1,im
csd
Cremove            if (kt .eq. 0 .or. hmask(i,j)) then
csd
               phis(i,j)=g*hgt(i,j)
c           
               phub=0.
               if (sigma) phub=phis(i,j)
              do ivi=1,lm
                  l=lmp1-ivi
                  phlb=phub
                  phub=g*heta(i,j,l)
                  teta(i,j,l)=-(phlb-phub)*(pt+etal(l)*pd(i,j))
     +                        /(r*deta(l)*pd(i,j))

cmp

Cmp	if (phub .lt. phlb) write(6,*) '<heta HOSED>: ', phub,phlb
	if (teta(i,j,l) .lt. 250 .and.L.ge.33.and.hgt(i,j).lt.90.) then
Cmp       write(6,264)I,J, L,phlb,phub,hgt(i,j),pd(i,j),teta(i,j,l)
	endif
  264	format('L,phlb,phub,hgt,pd,teta ',3(I2,1x),f5.1,1x,
     +  f7.1,1x,f6.1,1x,
     +  f8.1,1x,f7.1)


Cmp	diagnostic trying to find why lowest level temps suck
Cmp	basically everything is okay except for first level....too much
Cmp	of a jump from phi=0 to g*heta(i,j,lm)

	if (I .eq. 51 .and. J .eq. 15) then
C	write(6,*) ' '
C	write(6,*) 'L, teta, hgtlb,hgtub= > ', l,teta(i,j,l),
C     +	phlb/9.81,phub/9.81
C	write(6,*) 'phlb,phub ', phlb,phub
C	write(6,*) ' '
C	write(6,*) '................................'
	ENDIF

C**************************************************************
Cmp	substitute the TETA values at lowest level?
	if (ivi .eq. lm) then
	tsavetmp=teta(i,j,lm)	
CTEST	teta(i,j,lm)=teta(i,j,lm-1)
	endif
Cmp

	if (teta(i,j,l) .gt. 2000) then
        write(6,*) 'BIG TETA,phub,pd ',teta(i,j,l),i,j,l,phub/g,pd(i,J)
	endif

	if (TETA(I,J,L).lt.100.and.L.le.LM-1.and.L.ge.20) then
C	write(6,*) 'SMALL TETA:lev,lowZ,upZ,pd,etal ', teta(i,j,l),l,phlb/g,
C     +	phub/g,pd(i,J),etal(l)
C	write(6,*) 'TETA (below), deta(l) ', teta(I,J,L+1),deta(l)
	endif

      if (teta(i,j,l).gt.tetamax) tetamax=teta(i,j,l)


Cmp
	teta(i,j,l)=amin1(teta(i,j,l),325.)
	teta(i,j,l)=amax1(teta(i,j,l),150.)
Cmp
C
csn************
                  qeta(i,j,l)=max(0.,qeta(i,j,l))
                  ceta(i,j,l)=max(0.,ceta(i,j,l))
                  teta(i,j,l)=teta(i,j,l)/(qeta(i,j,l)*0.61+1)
               enddo             
c
C************************
c ************ Now redefine pd to have it equal to ps-pt.
C**************************
c
               pd(i,j)=ref(i,j)*pd(i,j)
C
Cmp	write(6,301) pd(i,j)
  301	format('redefined pd',f9.2)
C****************************
C**************************
csd
Cmp	reinstate
Cremove            endif
csd
	do L=1,LM
C	if (I .eq. 93 .and. J .eq. 17) then
C	write(6,*) 'WHY: I,J,L, teta(i,j,l)= > ', i,j,l,
C     +     	teta(i,j,l)
C	ENDIF
	enddo
         enddo
      enddo

	DEALLOCATE(HETA)
	DEALLOCATE(HGT)

	write(6,*) 'heta deallocated'

c
c *** Set u, v and t equal to zero at points below the ground.
c
      DO L=1,LM
       DO J=1,JM
        DO I=1,IM-1+MOD(J,2)
         IF(ETA(L+1).GT.REF(I,J)) THEN
          IF(I+IHW(J).GE.1.AND.I+IHW(J).LE.IM)ueta(i+ihw(j),j,l)=0.
          IF(I+IHW(J).GE.1.AND.I+IHW(J).LE.IM)veta(i+ihw(j),j,l)=0.
          IF(I+IHE(J).GE.1.AND.I+IHE(J).LE.IM)ueta(i+ihe(j),j,l)=0.
          IF(I+IHE(J).GE.1.AND.I+IHE(J).LE.IM)veta(i+ihe(J),j,l)=0.
          IF(J-1.GE.1.AND.J-1.LE.JM)ueta(i,j-1,l)=0.
          IF(J-1.GE.1.AND.J-1.LE.JM)veta(i,j-1,l)=0.
          IF(J+1.GE.1.AND.J+1.LE.JM)ueta(i,j+1,l)=0.
          IF(J+1.GE.1.AND.J+1.LE.JM)veta(i,j+1,l)=0.
         ENDIF
        ENDDO
       ENDDO
      ENDDO
c
c *** The separation of the boundary values.
c
C
C
C	write(6,*) 'sample wind values, lev=20 '
C	write(6,*) 'U,V,spd(1,1) ', ueta(1,1,20),veta(1,1,20),
C     +		(ueta(1,1,20)**2.+veta(1,1,20)**2.)**0.5
C	write(6,*) 'U,V,spd(35,35) ', ueta(35,35,20),veta(35,35,20),
C     +		(ueta(35,35,20)**2.+veta(35,35,20)**2.)**0.5
C	write(6,*) 'U,V,spd(im,4) ', ueta(im,4,20),veta(im,4,20),
C     +		(ueta(im,4,20)**2.+veta(im,4,20)**2.)**0.5
      n = 1 
      do i=1,im
         pdb(n,1)=pd(i, 1)
         pdb(n,2)=0.
         do l=1,lm
            tb(n,l,1)=teta(i, 1,l)
            tb(n,l,2)=0.
            qb(n,l,1)=qeta(i, 1,l)
            qb(n,l,2)=0.
            cb(n,l,1)=ceta(i, 1,l)
            cb(n,l,2)=0.
         enddo
         n=n+1
      enddo
      do i=1,im
         pdb(n,1)=pd(i,jm)
         pdb(n,2)=0.
         do l=1,lm
            tb(n,l,1)=teta(i,jm,l)
            tb(n,l,2)=0.
            qb(n,l,1)=qeta(i,jm,l)
            qb(n,l,2)=0.
            cb(n,l,1)=ceta(i,jm,l)
            cb(n,l,2)=0.
         enddo
         n=n+1
      enddo
      do j=3,jm-2,2
         pdb(n,1)=pd( 1,j)
         pdb(n,2)=0.
         do l=1,lm
            tb(n,l,1)=teta( 1,j,l)
            tb(n,l,2)=0.
            qb(n,l,1)=qeta( 1,j,l)
            qb(n,l,2)=0.
            cb(n,l,1)=ceta( 1,j,l)
            cb(n,l,2)=0.
         enddo
         n=n+1
      enddo
      do j=3,jm-2,2
         pdb(n,1)=pd(im,j)
         pdb(n,2)=0.
         do l=1,lm
            tb(n,l,1)=teta(im,j,l)
            tb(n,l,2)=0.
            qb(n,l,1)=qeta(im,j,l)
            qb(n,l,2)=0.
            cb(n,l,1)=ceta(im,j,l)
            cb(n,l,2)=0.
         enddo
         n=n+1
      enddo
c
C       SOUTH BOUNDARY - V POINTS
c
      n=1 
         do i=1,im-1
         do l=1,lm
            ub(n,l,1)=ueta(i, 1,l)
            ub(n,l,2)=0.
            vb(n,l,1)=veta(i, 1,l)
            vb(n,l,2)=0.
         enddo
         n=n+1
      enddo
C
C       NORTH BOUNDARY - V POINTS
C
      do i=1,im-1
         do l=1,lm
            ub(n,l,1)=ueta(i,jm,l)
            ub(n,l,2)=0.
            vb(n,l,1)=veta(i,jm,l)
            vb(n,l,2)=0.
         enddo
         n=n+1
      enddo
C
C       WEST BOUNDARY - V POINTS
C
      do j=2,jm-1,2
         do l=1,lm
            ub(n,l,1)=ueta( 1,j,l)
            ub(n,l,2)=0.
            vb(n,l,1)=veta( 1,j,l)
            vb(n,l,2)=0.
         enddo
         n=n+1
      enddo
C
C       EAST BOUNDARY - V POINTS
C
      do j=2,jm-1,2
         do l=1,lm
            ub(n,l,1)=ueta(im,j,l)
            ub(n,l,2)=0.
            vb(n,l,1)=veta(im,j,l)
            vb(n,l,2)=0.
         enddo
         n=n+1
      enddo
c
      if (kt .eq. 0) then
c
	write(6,*) 'THE LARGEST TETA VALUE IS ', TETAMAX
Cmp
         l=index(init_out//' ',' ')-1
         if (init_out(l:l) .ne. '/') then
            l=l+1
            init_out(l:l)='/'
         endif
c
         open(1,file=init_out(1:l)//'preproc.init'
     .       ,status='unknown',form='unformatted')
         rewind(1) 
         run=.true.
         ntsd=0
         print *,'Writing to file: ',init_out(1:l)//'preproc.init'

C	write(6,*) 'temps'
C	call levext(teta,im,jm,lm)
C	write(6,*) 'uwnd'
C	call levext(ueta,im,jm,lm)
C	write(6,*) 'vwnd'
C	call levext(veta,im,jm,lm)
C	write(6,*) 'Z'
C	call levext(heta,im,jm,lm)
C	write(6,*) 'pd'
C	call levext(pd,im,jm,1)

C	write(6,*) 'pd'
	do J=jm,jm/2,-1
C	write(6,666) (pd(i,j)/100.,i=im-10,im)
	enddo
C	write(6,*) 'ueta at lm=19'
	do J=jm,jm/2,-1
C        write(6,666) (ueta(i,j,19),i=im-10,im)
        enddo
C	write(6,*) 'veta at lm=19'
	do J=jm,jm/2,-1
C        write(6,666) (veta(i,j,19),i=im-10,im)
        enddo
C	write(6,*) 'HMASK (NE corner) '
	do J=jm,jm/2,-1
C        write(6,667) (hmask(i,j),i=im-10,im)
        enddo
C	write(6,*) 'VMASK (NE corner) '
	do J=jm,jm/2,-1
C        write(6,667) (vmask(i,j),i=im-10,im)
        enddo

  666	format(20(f5.0,1x))	
  667	format(20(L2,1x))	
         write(1) run,idat(1),idat(2),idat(3),idat(4),ntsd,ueta,veta
         write(1) teta,qeta,ceta,pd,phis,sm,ref,eta,pt,deta,etal,zeta
Ctst
C        open(unit=4,file=init_out(1:l)//'eta_temps.fil',
C     + status='unknown',form='unformatted')
C        write(4) teta
Ctst


	write(6,*) 'ueta, veta, level 20'
         print *,ueta(1,1,20),ueta(11,11,20),ueta(21,21,20)
         print *,veta(1,1,20),veta(11,11,20),veta(21,21,20)
	
Cmp ***** write out the surface fields

	write(1) sfcgrid

         close(1)
c
      endif

c
      l=index(init_out//' ',' ')-1
      if (init_out(l:l) .ne. '/') then
         l=l+1
         init_out(l:l)='/'
      endif
      write(fname,'(i3.3)') idat(5)
      open (1,file=init_out(1:l)//'preproc.bc.'//fname
     .     ,status='unknown',form='unformatted')
CIVAN	if (idat(5) .eq. 0) then
CIVAN      open (11,file=init_out(1:l)//'bndy.newstyle'
CIVAN     .     ,status='unknown',form='unformatted')
CIVAN	endif
      write(1) run,idat(1),idat(2),idat(3),idat(4),
     +         pd,teta,qeta,ueta,veta,ceta
Cmp	write(11) run,idat(1),idat(2),idat(3),idat(4)
CIVAN	write(11) pdb,tb,qb,ub,vb
	write(6,*) 'in interp, pdb(1,1),pdb(1,2): ', pdb(1,1),
     +                           pdb(1,2)
      close(1)
  671   format('L,U,V(LEV=20) ',I3,1x,f6.2,1x,f6.2,1x,f6.2)

      write(6,*) 'took square of neg number at ', negsum,' points'
c
        DEALLOCATE(PD)
        DEALLOCATE(PHIS)
        DEALLOCATE(SM)
        DEALLOCATE(TETA)
        DEALLOCATE(QETA)
        DEALLOCATE(UETA)
        DEALLOCATE(VETA)
c
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine levext(array,idim,jdim,kdim)

	real array(idim,jdim,kdim)

	do K=1,KDIM
	armax=-9999999.
	armin=9999999.
	imax=-9999.
	jmax=-9999.
C	do J=1,jdim
C	do I=1,idim
	do J=2,jdim-1
	do I=2,idim-1
	if (array(I,J,K).gt.armax) then
	armax=array(I,J,K)
	imax=i
	jmax=j
	endif
	if (array(I,J,K).lt.armin) armin=array(I,J,K)
	enddo
	enddo
	write(6,*) 'extremes for LEV: ', K, 'are  ', armin,armax, 
     +	'imax,jmax ',
     +	 imax,jmax
	enddo	
	return
	end


