      program data_prep_new
c
c *** Program to prepare ETA initialization fields from any gridded
c        data set.
c
c *** Original code obtained from U. of Athens and modified at FSL.
c
      implicit none
c
      include 'ecommons.h'
c
      integer*4 n, ihr
      character*3 carg1

      integer iargc
c_______________________________________________________________________________
c 
c *** Fill eta model common blocks.
c
      call eta_commons
c
c *** Interpolate data to eta grid from each input init gridded dataset.
c
      if (iargc() .ne. 1) then
         print *, " error iargc() .ne. 1 "
         stop
      end if

      call getarg(1,carg1)
      read(carg1,*) ihr
      print *, " init_new : ihr = ",ihr

      n=ihr/tboco+1

      print *, " init_new : n = ",n

      if (n.lt.1.or.n.gt.ninit) then
         print *, " error n.lt.1.or.n.gt.ninit "
         stop
      end if

c
c ****** Get the native grid dimensions of the input data.
c           (Dims are filled into named common block - sectorsize).
c
	write(6,*) 'call get_sector_size'
	write(6,*) 'file= ', init_gdsdir
	call get_sector_size(init_in(n),init_gdsdir)
c
	write(6,*) 'calling eta_interp n=',n
	call eta_interp_new(n)
c
	if (n.eq.1) then
c
c *** Create model init and constants files.
c
          write(6,*) 'calling eta_const'
          call eta_const
        end if

        if (n.gt.1) then
c
c *** Create model boundary condition files.
c
          write(6,*) 'calling eta_boco_new n-1=',n-1
          call eta_boco_new(n-1)
        end if

      end
