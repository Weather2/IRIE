                             SUBROUTINE GSCOND                          
C***********************************************************************
                             P A R A M E T E R
     & (A1=610.78,A2=17.2693882,A3=273.16,A4=35.86
     &, PQ0=379.90516,TRESH=.95
     &, CP=1004.6,ELWV=2.50E6,ELIV=2.834E6,ROW=1.E3,G=9.8
     &, EPSQ=1.E-12,DLDT=2274.0,TM10=263.16
     &, R=287.04,RV=461.5,EPS1=RV/R-1.
     &, CPR=CP*R,RCPR=1./(CPR),ELIW=ELIV-ELWV,CH2O=4.2E6)
                             P A R A M E T E R
     & (ARCP=A2*(A3-A4)/CP,RCP=1./CP,PQ0C=PQ0*TRESH,RROG=1./(ROW*G)
     &, RROW=1./ROW)                                                    
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "mpp.h"
#include "sp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, LP1=LM+1,LTOP=1,LBOT=LM)
C
                             P A R A M E T E R
     & (LDA=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1))
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,NOZ
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "TEMPV.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMCLH.comm"
C-----------------------------------------------------------------------
       REAL, SAVE :: F_snow(IDIM1:IDIM2,JDIM1:JDIM2,LM)=0.
      DOUBLE PRECISION DD(0:4),sol,P,DIS
C-----------------------------------------------------------------------
C-----PREPARATORY CALCULATIONS------------------------------------------
C-----------------------------------------------------------------------
      DTPH =NPHS*DT
      RDTPH=1./DTPH
      TWODT=DTPH
      RTWODT=1./TWODT
      US=1.
      EPS=0.622
      CCLIMIT=1.0E-3
      CLIMIT=EPSQ
C-------------------PADDING CLOUD MIXING RATIO IF TOO SMALL-------------
      DO L=1,LM
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        IF(CWM(I,J,L).LT.0.)CWM(I,J,L)=0.
C------------------PADDING SPECIFIC HUMIDITY IF TOO SMALL---------------
        IF(Q(I,J,L).LT.EPSQ)Q(I,J,L)=EPSQ*HTM(I,J,L)
      ENDDO
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-----START OF MICROPHISICS---------------------------------------------
C-----------------------------------------------------------------------
      DO 100 J=MYJS2,MYJE2
      DO 100 I=MYIS,MYIE
C
      IF(HBM2(I,J).LT.1.)GOTO 100
C
      LMHIJ=LMH(I,J)
      LML=LM-LMHIJ
      U00IJ=U00(I,J)
      RESIJ=RES(I,J)
      PDSLIJ=RES(I,J)*PD(I,J)
      PDSLIJ_0=RES(I,J)*P0(I,J)
C
      CONST=PDSLIJ/G*TWODT
      PRECRL1=0.
      PRECSL1=0.
      PRECIL1=0.
C--------------FIND THE HEIGHT OF THE PBL-------------------------------
      EPSQ2=0.20
      FH=1.01
C
      LMHM=LMHIJ-1
      LMHP=LMHIJ+1
C
      LPBL=LMHIJ
          DO 105 IVI=1,LMHM
      L=LMHIJ-IVI
      IF(Q2(I,J,L).LE.EPSQ2*FH)THEN
        LPBL=L
        GO TO 110
      ENDIF
 105  CONTINUE
      LPBL=1
 110  CONTINUE
C
      CAPA=0.28589641
      DCLPS=667.
      CLAPSE=-0.001
      CLPSE=CLAPSE-1./DCLPS
C
      DO 90 L=2,LM
C
      IF(HTM(I,J,L).LT.1.)GOTO 90
C
      IF(HBM2(I,J).GT.0..AND.L.GE.LPBL.AND.L.LE.LMHIJ)THEN
        WMSK=VTM(I+IHE(J),J,L)
     1      +VTM(I+IHW(J),J,L)
     2      +VTM(I,J+1,L)
     3      +VTM(I,J-1,L)
        IF(WMSK.GT.0.)THEN
          RWMSK=1./WMSK
          UCOL=(U(I+IHE(J),J,L)*VTM(I+IHE(J),J,L)
     1         +U(I+IHW(J),J,L)*VTM(I+IHW(J),J,L)
     2         +U(I,J+1,L)*VTM(I,J+1,L)
     3         +U(I,J-1,L)*VTM(I,J-1,L))*RWMSK
          VCOL=(V(I+IHE(J),J,L)*VTM(I+IHE(J),J,L)
     1         +V(I+IHW(J),J,L)*VTM(I+IHW(J),J,L)
     2         +V(I,J+1,L)*VTM(I,J+1,L)
     3         +V(I,J-1,L)*VTM(I,J-1,L))*RWMSK
        ELSE
          STOP "CHECK MOUNTAINS"
        ENDIF
        wspd=SQRT(UCOL*UCOL+VCOL*VCOL)
        THL1=T(I,J,L)*(1.E5/(AETA(L)*PDSLIJ+PT))**CAPA
        THL2=T(I,J,L-1)*(1.E5/(AETA(L-1)*PDSLIJ+PT))**CAPA
        DTHDP=(THL2-THL1)/(PDSLIJ*(AETA(L-1)-AETA(L)))
C
        IF(DTHDP.GE.CLAPSE.OR.wspd.GT.2.0)THEN
          FINV=1.
        ELSEIF(DTHDP.GT.CLPSE)THEN
          FINV=1.+DCLPS*(DTHDP-CLAPSE)
        ELSE
          FINV=0.
        ENDIF
      ELSE
        FINV=1.
      ENDIF
C
      TKL=T(I,J,L)
      QKL=Q(I,J,L)
      CWMKL=CWM(I,J,L)
      FSNOW=F_snow(I,J,L)
      IF(CWMKL.LE.CLIMIT)FSNOW=0.
      PKL=PDSLIJ*AETA(L)+PT
      TMT0=TKL-273.16
      FCLOUD=0.
      FWI=0.
C
      TKL_0=T0(I,J,L)
      QKL_0=Q0(I,J,L)
      PKL_0=PDSLIJ_0*AETA(L)+PT
C-----------------------------------------------------------------------
C--------------CIRRUS CLOUD ICE SUPERSATURATION-------------------------
C-----------------------------------------------------------------------
      IF(PKL.LT.35000..AND.TMT0.LT.-38.)THEN
        FCI=1.25
      ELSE
        FCI=1.
      ENDIF
C-----------------------------------------------------------------------
C-----QCi---------------------------------------------------------------
C-----------------------------------------------------------------------
      TT=TKL
      QQ=QKL
      PP=PKL
      ELV=ELIV
      FIW=1.
      CALL FSLOPE
      QCi=QW
      TMT0i=TG1-273.16
C-----------------------------------------------------------------------
C-----ICE-WATER ID NUMBER IW--------------------------------------------
C-----------------------------------------------------------------------
      IF(TMT0i.LT.-15.)THEN
        FIW=1.
      ELSEIF(TMT0i.GE.0.)THEN
        FIW=0.
      ELSE
        FIW=1.
        U00KL=U00IJ+UL(L+LML)*(0.95-U00IJ)*FINV
        FI=QKL-U00KL*QSAT(PKL,TKL,FIW,FCI)
        IF(PRECIL1.LE.0..OR.FI.LE.0.)FIW=0.
      ENDIF
C
      FWI=1.-FIW
C-----------------------------------------------------------------------
C-----SNOW IS NOT PART OF CLOUD WATER CLOUDS----------------------------
C-----------------------------------------------------------------------
      THICK=PDSLIJ/G*DETA(L)
      RTHICK=1./THICK
      CPDR=FSNOW*CWMKL*(1.-FIW)
      PRECAK=CPDR*THICK
      CWMKL=CWMKL-CPDR
      FSNOW=FSNOW*FIW
C-----------------------------------------------------------------------
C-----ELV---------------------------------------------------------------
C-----------------------------------------------------------------------
      ELV=(1.-FIW)*ELWV+FIW*ELIV
C-----------------------------------------------------------------------
C-----QC, QC_0----------------------------------------------------------
C-----------------------------------------------------------------------
      TT=TKL
      QQ=QKL
      PP=PKL
      CALL FSLOPE
      QC=QW
      TMT0=TG1-273.16
C
      TT=TKL_0
      QQ=QKL_0
      PP=PKL_0
      CALL FSLOPE
      QC_0=QW
C-----------------------------------------------------------------------
C-----U00KL-------------------------------------------------------------
C-----------------------------------------------------------------------
      U00KL=U00IJ+UL(L+LML)*(0.95-U00IJ)*FINV
C
      TT=TKL
      QQ=QKL
      PP=PKL/U00KL
      CALL FSLOPE
      U00KL=QW/QC
C-----------------------------------------------------------------------
C-----RQKL_0, RQKL_1----------------------------------------------------
C-----------------------------------------------------------------------
      RQKL_1=QKL/QC
      RQKL_C=CWMKL/QC
      RQKL_0=AMAX1(AMIN1(QKL_0/QC_0,RQKL_1),U00KL)
      RQKL_C=AMIN1(RQKL_C,AMAX1(0.,RQKL_1+RQKL_C-U00KL))
      RQKL_1=AMAX1(RQKL_1,U00KL)
C-----------------------------------------------------------------------
C-----AB_0--------------------------------------------------------------
C-----------------------------------------------------------------------
      AB_0=(RQKL_1+RQKL_C-U00KL)/US
C-----------------------------------------------------------------------
C-----COEF--------------------------------------------------------------
C-----------------------------------------------------------------------
      COEF=2./US
C-----------------------------------------------------------------------
C-----CCRKL_1-----------------------------------------------------------
C-----------------------------------------------------------------------
      AC=US-RQKL_1
      AB=AB_0+COEF*RQKL_C
C
      IF(AB.LE.0.)THEN
        CCRKL_1=0.
      ELSEIF(AC.LE.0.)THEN
        CCRKL_1=1.
      ELSE
        DD(0)=-RQKL_1*COEF*RQKL_C
        DD(2)=US*AB_0
        DD(1)=-DD(0)-DD(2)+AC*(1.+AB)
        IF(DD(2).NE.0.D+0)THEN
          P=5.D-1*DD(1)/DD(2)
          DIS=P**2-DD(0)/DD(2)
          sol=-P+SQRT(DIS)
        ELSE
          sol=-DD(0)/DD(1)
        ENDIF
        CCRKL_1=sol
      ENDIF
C-----------------------------------------------------------------------
C-----DRQ0--------------------------------------------------------------
C-----------------------------------------------------------------------
      RQKL_0=RQKL_1-CCRKL_1*(RQKL_1-RQKL_0)
      DRQ0=RQKL_1-RQKL_0
C-----------------------------------------------------------------------
C-----CCRKL, CONE0------------------------------------------------------
C-----------------------------------------------------------------------
      AC=US-RQKL_0
      AB=CCRKL_1*AB_0+COEF*(DRQ0+RQKL_C)
C
      IF(CCRKL_1.LE.0.)THEN
        CCRKL=0.
        RQKL=U00KL
      ELSEIF(AC.LE.0.)THEN
        CCRKL=1.
        RQKL=US
      ELSE
        DD(1)=AC*(CCRKL_1+AB)-US*AB*(1.-CCRKL_1)
        IF(DD(1).GE.0.D+0)THEN
          DRQ0=DRQ0+RQKL_C
          RQKL_C=0.
        ENDIF
        DD(0)=-RQKL_1*COEF*RQKL_C
        DD(2)=DRQ0*CCRKL_1+(CCRKL_1*US+DRQ0)*(CCRKL_1*AB_0+COEF*DRQ0)
        DD(1)=-DD(0)-DD(2)+DD(1)
        IF(DD(2).NE.0.D+0)THEN
          P=5.D-1*DD(1)/DD(2)
          DIS=P**2-DD(0)/DD(2)
          sol=-P+SQRT(DIS)
        ELSE
          sol=-DD(0)/DD(1)
        ENDIF
        CCRKL=sol*CCRKL_1
        RQKL=RQKL_0+(1.-sol)*DRQ0
      ENDIF
C
      CONE0=(QKL/QC-RQKL)*QC
C
      IF(CONE0+CWMKL.LE.CLIMIT.OR.CCRKL.LE.0.)THEN
        CCRKL=0.
        RQKL=U00KL
        CONE0=-CWMKL
        FCLOUD=1.
        FWI=1.
      ELSE
      RQKL_0=(QKL-CONE0)/QC
      RQKL_C=(CWMKL+CONE0)/QC
C-----------------------------------------------------------------------
C-----CCRKL_0-----------------------------------------------------------
C-----------------------------------------------------------------------
      AC=US-RQKL_0
      AB=CCRKL*AB_0+COEF*RQKL_C
      IF(AC.LE.0.)THEN
        CCRKL_0=1.
      ELSE
        CCRKL_0=RQKL_0*AB/(AC+US*AB)
      ENDIF
      IF(ABS(CCRKL-CCRKL_0).GT.0.00001)
     &print *,"GRESKA",ABS(CCRKL-CCRKL_0),ABS(RQKL-RQKL_0),RQKL_0
      ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
C     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
C
      CWM(I,J,L)=CONE0+CWMKL
      CLD(I,J,L)=CCRKL
C
      TRAIN(I,J,L)=TRAIN(I,J,L)+ELV*RCP*CONE0*RDTPH
      T(I,J,L)=ELV*RCP*CONE0+TKL
      Q(I,J,L)=-CONE0+QKL
C-----------------------------------------------------------------------
C-------------------SAVE T, Q FOR THIS STEP-----------------------------
C-----------------------------------------------------------------------
      Q0(I,J,L)=Q(I,J,L)
      T0(I,J,L)=T(I,J,L)
C-----------------------------------------------------------------------
C----------CHOOSE THE POINTS WHERE PRECIPITATION CAN BE PRODUCED--------
C-----------------------------------------------------------------------
      C0=0.15
      CWS=0.025
      CR=0.4*RTWODT
      AA2=1.0*RTWODT
      EXPF=EXP(0.025*TMT0)
      WMINK=0.1E-3*EXPF
C
      PRECRL=0.
      PRECSL=0.
      PRECIL=0.
      PRAUT=0.
      PSAUT=0.
      PRACW=0.
      PSACI=0.
      ERR  =0.
      ERS  =0.
      PSM  =0.
      PSM1 =0.
      PSM2 =0.
      PPR  =0.
      PPS  =0.
      CPDR =0.
C
      TT=T(I,J,L)
      QQ=Q(I,J,L)
      WW=CWM(I,J,L)
C
      CONDE=CONST*DETA(L)
      RCONDE=1./CONDE
C
      PP=PKL
      PSFC=PD(I,J)+PT
      RHO=PP/(R*TT*(1.+0.608*QQ))
      DIFFUS=8.794E-5*TT**1.81/PP
      EK=100.
C
      THICK=PDSLIJ/G*DETA(L)
      RTHICK=1./THICK
      CPDR=FSNOW*WW
      PRECAK=CPDR*THICK+PRECAK
      WW=WW-CPDR
      FSNOW=0.
C
      PRECRK=AMAX1(0.,PRECRL1)
      PRECSK=AMAX1(0.,PRECSL1)+PRECAK
      PRECIK=AMAX1(0.,PRECIL1)
      IF(WW.LT.CLIMIT.AND.(PRECRK+PRECSK+PRECIK).EQ.0.)THEN
        PID=0.
      ELSE
        PID=1.
      ENDIF
C-----------------------------------------------------------------------
C-----SEDIMENTATION OF ICE----------------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IF(FWI.EQ.0..AND.PP+25.E2.LT.PSFC)THEN
          PRECIK=AMAX1(0.,PRECIL1)+WW*THICK
          RHO=PP/(R*TT*(1.+0.608*QQ))
          VICE=0.15*(PP/300.E2)**(-0.178)*(TT/233.)**(-0.394)
          AA1=RHO*VICE*RTHICK
          FLUNI=AA1*TWODT/(1.+AA1*TWODT)
          WW=(1.-FLUNI)*PRECIK*RTHICK
          PRECIL=FLUNI*PRECIK
        ELSE
          PRECSL1=PRECSL1+PRECIL1
          PRECIL1=0.
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C------------------PRECIPITATION PRODUCTION RATES-----------------------
C------------------AUTO-CONVERT RATES-----------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=AMAX1(0.,WW-CLIMIT)
        IF(FWI.EQ.0.)THEN
          EXPF=EXP(0.025*TMT0)
          IF(TT.LE.250.16)THEN
            AA1=1.E-3*EXPF
          ELSE
            AA1=0.167E-3*(1.+0.5*SQRT(AMAX1(0.,268.-TT)))
          ENDIF
          PSAUT=AA1*AMAX1(0.,CWMK-WMINK)
          CPDR=-PSAUT*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PSAUT=-CPDR*RTWODT
          ENDIF
        ELSE
          AMAXCM=AMAX1(0.,CWMK-WMINK)
          PRAUT=C0*AMAXCM*AMAXCM
          CPDR=-PRAUT*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PRAUT=-CPDR*RTWODT
          ENDIF
        ENDIF
        PPR=PRAUT*CONDE
        PPS=PSAUT*CONDE
      ENDIF
C
      IF(PID.EQ.1.)THEN
        WW=CPDR+WW
        PRECRL=PRECRL1+PPR
        PRECSL=PRECSL1+PRECAK+PPS
      ENDIF
C-----------------------------------------------------------------------
C-----------------------ACCRETIONS--------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=WW
        PRECRK=AMAX1(0.,PRECRL1)
        PRECSK=AMAX1(0.,PRECSL1)
        IF(FWI.EQ.0.)THEN
          EXPF=EXP(0.025*TMT0)
          CS=AA2*EXPF
          PSACI=CS*AMAX1(0.,CWMK)*PRECSK
          CPDR=-PSACI*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PSACI=-CPDR*RTWODT
          ENDIF
        ELSE
          PSM2=CWS*CR*AMAX1(0.,CWMK)*PRECSK
          PRACW=CR*AMAX1(0.,CWMK)*(PRECRK+PRECSK)
          CPDR=-PRACW*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PRACW=-CPDR*RTWODT
          ENDIF
        ENDIF
        PPR=PRACW*CONDE
        PPS=PSACI*CONDE
      ENDIF
C
      IF(PID.EQ.1.)THEN
        WW=CPDR+WW
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
C-----------------------------------------------------------------------
C-----EVAPORATION/CONDENSATION OF PRECIPITATION-------------------------
C***** ERR & ERS POSITIVE--EVAPORATION
C***** ERR & ERS NEGTIVE---CONDENSATION
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        PRECRK=AMAX1(0.,PRECRL)
        PRECSK=AMAX1(0.,PRECSL)
C-----------------------------------------------------------------------
C INCREASE THE EVAPORATION/CONDENSATION FOR STRONG/LIGHT PREC
C-----------------------------------------------------------------------
        ELV=ELWV
        FIW=0.
        CALL FSLOPE
        QCw=QW
C
        ELV=ELIV
        FIW=1.
        CALL FSLOPE
        QCi=QW
        TMT0i=TG1-273.16
C
        VENTR=EK*SQRT(1.*SQRT(RHO)*PRECRK*RTWODT)
        VENTS=EK*SQRT(4.*SQRT(RHO)*PRECSK*RTWODT)
C
        VENTR=VENTR*AMAX1(1.,AMIN1(2.,VENTR))
        VENTS=VENTS*AMAX1(1.,AMIN1(2.,VENTS))
C
        EVPR=DIFFUS*VENTR
        EVPS=DIFFUS*VENTS
C
        ERR=EVPR*AMAX1(0.,QCw-QQ)
        ERS=EVPS*AMAX1(0.,QCi-QQ)
C
        IF(TMT0i.GT.0.)THEN
          CRS=EVPS*AMIN1(0.,QCi-QQ)
        ELSE
          CRS=0.
        ENDIF
C
        ERK=AMAX1(0.,U00KL*QC-QQ)*RTWODT-CRS
C
        IF(ERR+ERS.GT.ERK)THEN
          factor=ERK/(ERR+ERS)
          ERR=ERR*factor
          ERS=ERS*factor
        ENDIF
C
        ERS=ERS+CRS
C
        PPR=-ERR*CONDE
        PPS=-ERS*CONDE
C
        IF(-PPR.GE.PRECRK)THEN
          PPR=-PRECRK
          ERR=-PPR*RCONDE
        ENDIF
C
        IF(-PPS.GE.PRECSK)THEN
          PPS=-PRECSK
          ERS=-PPS*RCONDE
        ENDIF
C
      ENDIF
C
      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
C-----------------------------------------------------------------------
C--------------------MELTING OF THE SNOW--------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=WW
        AMAXPS=AMAX1(0.,PRECSL)
C
        IF(TMT0i.GT.0..AND.AMAXPS.GT.0.)THEN
          PSM1=EVPS*CP/ELIW*TMT0i
          PSM=PSM1+PSM2
        ELSE
          PSM1=0.
          PSM2=0.
          PSM=0.
        ENDIF
C
        PPR=PSM*CONDE
        PPS=-PSM*CONDE
C
        IF(-PPS.GE.AMAXPS)THEN
          PPS=-AMAXPS
          PPR=AMAXPS
          PSM1=-PPS*RCONDE
          PSM2=0.
          PSM=PSM1
        ENDIF
C
      ENDIF
C
      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
C-----------------------------------------------------------------------
C---------------UPDATE T AND Q------------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        TT=-RCP*(ELWV*ERR+ELIV*ERS+ELIW*PSM1)*TWODT+TT
        QQ=(ERR+ERS)*TWODT+QQ
      ENDIF
C-----------------------------------------------------------------------
C-----SEDIMENTATION OF SNOW---------------------------------------------
C-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IF(FWI.EQ.0..AND.PP+25.E2.LT.PSFC)THEN
          PRECSK=AMAX1(0.,PRECSL)
          RHO=PP/(R*TT*(1.+0.608*QQ))
          VSNOW=1.0*EXP(0.025*TMT0)
          AA1=RHO*VSNOW*RTHICK
          FLUNI=AA1*TWODT/(1.+AA1*TWODT)
          CPDR=(1.-FLUNI)*PRECSK*RTHICK
          WW=CPDR+WW
          PRECSL=FLUNI*PRECSK
          IF(CPDR.GT.0.)FSNOW=CPDR/WW
        ENDIF
      ENDIF
C
      PRECRL1=PRECRL
      PRECSL1=PRECSL
      PRECIL1=PRECIL
C
C     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
C     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
C     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
C
      TRAIN(I,J,L)=TRAIN(I,J,L)+(TT-T(I,J,L))*RDTPH
      T(I,J,L)=TT
      Q(I,J,L)=QQ
      CWM(I,J,L)=WW
      F_snow(I,J,L)=FSNOW
C
   90 CONTINUE
C-----------------------------------------------------------------------
C-------------------THE PRECIPITATION ON SFC----------------------------
C-----------------------------------------------------------------------
      PRECRS=PRECRL1*RROW
      PRECSS=PRECSL1*RROW
C
      APREC=PRECRS+PRECSS
      PREC(I,J)=PREC(I,J)+PRECRS+PRECSS
      ACPREC(I,J)=ACPREC(I,J)+APREC
C-----------------------------------------------------------------------
C---------------THE SNOW AND RAIN RATIO OF SFC PREC---------------------
C----SR IS THE RATIO OF SNOW TO THE TOTAL PRECIP------------------------
C----IF TOTAL PRECIP IS ZERO, SR IS ZERO--------------------------------
C-----------------------------------------------------------------------
      TOTPPT=PRECRS+PRECSS
      IF (TOTPPT.GT.1.E-8) THEN
       SR(I,J)=PRECSS/TOTPPT
      ELSE
       SR(I,J)=0.
      ENDIF
C-----------------------------------------------------------------------
C-------------------SAVE P FOR THIS STEP--------------------------------
C-----------------------------------------------------------------------
      P0(I,J)=PD(I,J)
C-----------------------------------------------------------------------
  100 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
      CONTAINS
      SUBROUTINE FSLOPE
      RP=TT+RCP*ELV*QQ
      TGS=TT
      QW=QSAT(PP,TGS,FIW,FCI)
      IF(FCLOUD*FWI*FIW*TT.GT.A3)QW=AMIN1(QQ,QW)
      FO=TGS+RCP*ELV*QW-RP
      TG1=TGS-.5*FO
      TGUESS=TGS
      ITCNT=0
   10 QW=QSAT(PP,TG1,FIW,FCI)
      IF(FCLOUD*FWI*FIW*TT.GT.A3)QW=AMIN1(QQ,QW)
      F1=TG1+RCP*ELV*QW-RP
      IF(ABS(F1).LT..001.OR.ABS(F1-FO).LT.1.E-10.OR.ITCNT.GT.30)GOTO 25
      ITCNT=ITCNT+1
      DTG=F1*(TG1-TGUESS)/(F1-FO)
      TGUESS=TG1
      FO=F1
      TG1=TG1-DTG
      GOTO 10
   25 CONTINUE
      END SUBROUTINE FSLOPE
                             END
C-----------------------------------------------------------------------
      REAL FUNCTION QSAT(PP,TT,FIW,FCI)
C-----------------------------------------------------------------------
      REAL, PARAMETER :: PQ0=379.90516,A2=17.2693882,A3=273.16,A4=35.86
C-----------------------------------------------------------------------
C-----AI, BI------------------------------------------------------------
C-----------------------------------------------------------------------
      TMT0=TT-A3
C
      IF(TMT0.LT.-20.)THEN
        AI=0.007225
        BI=0.9674
      ELSEIF(TMT0.LT.0.)THEN
        AI=0.008855
        BI=1.
      ELSE
        AI=0.
        BI=1.
        IF(FIW.EQ.1.)TMT0=0.
      ENDIF
C
      AI=FIW*AI*FCI
      BI=1.-FIW+FIW*BI*FCI
C
      QSAT=PQ0/PP*EXP(A2*TMT0/(TT-A4))*(BI+AI*TMT0)
C-----------------------------------------------------------------------
      RETURN
      END
