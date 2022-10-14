      subroutine eta_boco_new(ibc)
c
c *** Creates the eta boundary condition file.
c
c *** Original code obtained from U. of Athens and modified at FSL.
c
      implicit none
c
      include 'ecommons.h'
c
      real*4
     .    pdba(im,jm   ),pdbb(im,jm   )
     .   ,tba (im,jm,lm),tbb (im,jm,lm)
     .   ,qba (im,jm,lm),qbb (im,jm,lm)
     .   ,uba (im,jm,lm),ubb (im,jm,lm)
     .   ,vba (im,jm,lm),vbb (im,jm,lm)
     .   ,cba (im,jm,lm),cbb (im,jm,lm)
     .   ,rtboco,run
     .   ,pdb(im,jm,   2)
     .   ,tb (im,jm,lm,2)
     .   ,qb (im,jm,lm,2)
     .   ,ub (im,jm,lm,2)
     .   ,vb (im,jm,lm,2)
     .   ,cb (im,jm,lm,2)
     .   ,q2b(im,jm,lm,2)
c
      integer*4 itboco,ibc,nibc,idat(3),ihrst,i,j,k,l,len,n
c
      character*255 fname1,fname2,fname
      character*8   ftime
      character*3   ced
c
      real*4 epsq2
      parameter (epsq2=0.2)
c_______________________________________________________________________________
c
      print *, ' '
      print *, 'Creating eta boundary condition files...'
      print *, 'these are dimensioned as lb, which is= ', lb
      print *, 'kb,im,jm,imjm,imt,jmt= ',kb,im,jm,imjm,imt,jmt
      print *, 'n=',n,' tboco=',tboco
c
      len=index(init_out,' ')-1
      if (init_out(len:len) .ne. '/') then
         len=len+1
         init_out(len:len)='/'
      endif
c
c *** Loop through for all boco files.
c
      rtboco=1./(tboco*3600.)                                             
      itboco=nint(tboco)
      nibc=nhour/itboco
c
      write(ced,'(i3.3)') (ibc-1)*itboco
      l=len+14
      fname1(1:l)=init_out(1:len)//'preproc.bc.'//ced
      print *,'Read from file: ',fname1(1:l)
      open(1,file=fname1(1:l),status='old',form='unformatted')
      read(1) run,idat,ihrst,pdbb,tbb,qbb,ubb,vbb,cbb
      close(1)
      write(6,*) 'done reading from file ',fname1(1:l)
c
      write(ced,'(i3.3)') ibc*itboco
      l=len+14
      fname2(1:l)=init_out(1:len)//'preproc.bc.'//ced
      print *,'Read from file: ',fname2(1:l)
      open(1,file=fname2(1:l),status='old',form='unformatted')
      read(1) run,idat,ihrst,pdba,tba,qba,uba,vba,cba
      close(1)
      write(6,*) 'done reading from file ',fname2(1:l)
c
      do i=1,im
      do j=1,jm
         pdb(i,j,1)=pdbb(i,j)
         pdb(i,j,2)=(pdba(i,j)-pdbb(i,j))*rtboco
      enddo
      enddo
c
      do l=1,lm                              
      do i=1,im
      do j=1,jm
         tb (i,j,l,1)=tbb(i,j,l)                                              
         qb (i,j,l,1)=qbb(i,j,l)                                              
         cb (i,j,l,1)=cbb(i,j,l)                                              
         ub (i,j,l,1)=ubb(i,j,l)                                              
         vb (i,j,l,1)=vbb(i,j,l)                                              
         q2b(i,j,l,1)=epsq2
         tb (i,j,l,2)=(tba(i,j,l)-tbb(i,j,l))*rtboco                              
         qb (i,j,l,2)=(qba(i,j,l)-qbb(i,j,l))*rtboco                              
         cb (i,j,l,2)=(cba(i,j,l)-cbb(i,j,l))*rtboco                              
         ub (i,j,l,2)=(uba(i,j,l)-ubb(i,j,l))*rtboco                              
         vb (i,j,l,2)=(vba(i,j,l)-vbb(i,j,l))*rtboco                             
         q2b(i,j,l,2)=0.
      enddo
      enddo
      enddo
c
c *** Open boundary condition file.
c
      write(ftime,'(i3.3)') ibc
      l=len+8
      fname(1:l)=init_out(1:len)//'bndy.'//ftime
      print *,'Open BC file  : ',fname(1:l)
      open(2,file=fname(1:l),status='unknown',form='unformatted')
      write(2) run,idat,ihrst,tboco
      write(2) float(ibc-1)*tboco
      write(2) pdb
      write(2) tb
      write(2) qb
      write(2) ub
      write(2) vb
      write(2) q2b
      write(2) cb
      write(2) float(nibc)*tboco
      close(2)
      print *,'Close BC file : ',fname(1:l)
c
      return
      end                                        
