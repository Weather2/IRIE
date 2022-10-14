	program mdeta

	parameter(NLEV=38)

	real deta(NLEV)
	integer ldum(NLEV)

	detasmp= 1./float(NLEV)
	ldum=5

	write(6,*) 'detasmp= ', detasmp

	deta=detasmp

	write(24) deta,ldum

	end
