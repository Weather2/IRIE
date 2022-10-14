	 Program readwrite

        parameter (layers=60)

	parameter (ISTART=1,IEND=39)

        real*4 deta(layers), ldt1(layers)
        real*4 detanew(layers)

        REWIND 11
        read(11)DETA,LDT1

	detanew=deta

	sumnew=0.
	
	detanew(40)=.0140
	detanew(41)=.0131
	detanew(42)=.0122
	detanew(43)=.0113
	detanew(44)=.0105
	detanew(45)=.0096
	detanew(46)=.0089
	detanew(47)=.0083
	detanew(48)=.0078
	detanew(49)=.0074
	detanew(50)=.0071
	detanew(51)=.0068
	detanew(52)=.0065
	detanew(53)=.0062
	detanew(54)=.0059
	detanew(55)=.0056
	detanew(56)=.0053
	detanew(57)=.0050
	detanew(58)=.0047
	detanew(59)=.0037

	do I=40,59
	sumnew=sumnew+(detanew(I)-deta(I))
	enddo

	write(6,*) 'have a total of ', sumnew , 'to play with'

	rinc=sumnew/(IEND-ISTART+1)
	write(6,666) sumnew/(IEND-ISTART+1)

  103	format(I2,1x,f9.6,1x,f9.6,1x,f9.6)
  666	format(f12.8)

	do I=ISTART,IEND
	detanew(I)=detanew(I)-rinc
	enddo

	sum=0.
	sumtwo=0.
	do I=1,layers
	write(6,103) I,detanew(I),deta(I),detanew(I)-deta(I)
	sum=sum+deta(I)
	sumtwo=sumtwo+detanew(I)
	enddo

	write(6,*) 'old, new deta sums: ', sum,sumtwo

        write(51)DETANEW,ldt1

        end
