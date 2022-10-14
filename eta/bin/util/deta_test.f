	 Program readwrite

        parameter (layers=60)
        real*4 deta(layers), ldt1(layers)

        REWIND 11
        read(11)DETA,LDT1

        sum=0.
        do I=1,layers
        sum=sum+deta(I)
	if (I .ge. 2) then
	diff=deta(I)-deta(I-1)
	else
	diff=0.
	endif
        write(6,103)I, DETA(I), sum, diff
        enddo

  103	format(I2,1x,f9.6,1x,f9.6,1x,f10.7)

        write(6,*) 'sum of deta= ', sum

C        write(51)DETA

        end

