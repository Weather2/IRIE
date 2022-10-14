	program mdeta

	parameter
     &  (NLEV=45,LMP1=NLEV+1
     &  ,PT=2500.,G=9.80,R=287.04,GAMMA=0.0065,PRF0=101325.,T0=288.) 

	real    deta(NLEV),ETA(LMP1),ETAL(NLEV),ZETA(LMP1)
	integer ldum(NLEV)

        RGOG=R*GAMMA/G

C---- COMPUTING OF ETA LEVELS ACCORDING TO EMPIRICAL FORMULA ...
        DO I=1,NLEV
          X=FLOAT(I-1)/FLOAT(NLEV)
          ETA(I)=0.8*X+1.03643*X**2-0.973*X**3+0.13673*X**4-0.00016*X**5
        END DO
        ETA(LMP1)=1.0
C---- COMPUTING DETA ...
        DO I=1,NLEV
          deta(I)=ETA(I+1)-ETA(I)
        END DO

        DO L=1,NLEV
          ETAL(L)=0.5*(ETA(L)+ETA(L+1))
        END DO

        DO L=1,LMP1
          ZETA(L)=T0*(1.-((PT+ETA(L)*(PRF0-PT))/PRF0)**RGOG)/GAMMA
        END DO

	ldum=5

	write(6,*) 'deta= ', deta
	write(6,*) 'zeta= ', ZETA

        write(24) deta,ldum

	end
