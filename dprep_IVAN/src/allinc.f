        program allinc
c       pgf90 allinc.f -I../src/include ../lib/lib*.a

        include "ecommons.h"

        open (10,file='../src/cdate11')
        write (10,'(a)') '2002032200'
        close(10)

        call eta_commons
        print *,' wbd, sbd =',wbd,sbd
        print *,' LM =',LM, ' DT=',DT
        PTT = PTINP
        IM=-WBD/DLMD+1.5
        JM=-2*SBD/DPHD+1.5

1       format ("      PARAMETER ( tlm0d = ",f10.5," ) "  )
2       format ("      PARAMETER ( tph0d = ",f10.5," ) "  )
3       format ("      PARAMETER ( wbd   = ",f10.5," ) "  )
4       format ("      PARAMETER ( sbd   = ",f10.5," ) "  )
5       format ("      PARAMETER ( dlmd  = ",f10.5," ) "  )
6       format ("      PARAMETER ( dphd  = ",f10.5," ) "  )
7       format ("      PARAMETER ( DT    = ",f10.5," ) "  )
8       format ("      PARAMETER ( LM    = ",I3   ," ) "  )
9       format ("      PARAMETER ( PTT   = ",f10.2," ) "  )
10      format ("      PARAMETER ( IM    = ",I4   ," ) "  )
11      format ("      PARAMETER ( JM    = ",I4   ," ) "  )

        open (10,file='../include/all_new.inc')
        write ( 10, 1) tlm0d
        write ( 10, 2) tph0d
        write ( 10, 3) wbd
        write ( 10, 4) sbd
        write ( 10, 5) dlmd
        write ( 10, 6) dphd 
        write ( 10, 7) DT
        write ( 10, 8) LM
        write ( 10, 9) PTT
        write ( 10,10) IM
        write ( 10,11) JM
        close(10) 

21       format ("      PARAMETER ( tlm0d = ",f10.5," ) "  )
12       format ("      PARAMETER ( tph0d = ",f10.5," ) "  )
13       format ("      PARAMETER ( wbd   = ",f10.5," ) "  )
14       format ("      PARAMETER ( sbd   = ",f10.5," ) "  )
15       format ("      PARAMETER ( dlmd  = ",f10.5," ) "  )
16       format ("      PARAMETER ( dphd  = ",f10.5," ) "  )
17       format ("      PARAMETER ( DTB   = ",f10.5," ) "  )
18       format ("      PARAMETER ( LM    = ",I3   ," ) "  )

        open (10,file='../include/all.inc')
        write ( 10,21) tlm0d
        write ( 10,12) tph0d
        write ( 10,13) wbd
        write ( 10,14) sbd
        write ( 10,15) dlmd
        write ( 10,16) dphd
        write ( 10,17) DT
        write ( 10,18) LM
        close(10) 

        end
