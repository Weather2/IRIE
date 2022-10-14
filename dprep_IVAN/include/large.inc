C-------- start of large.inc -------------------------------------------
#include "all.inc"
                        P A R A M E T E R
     &(IM=-WBD/DLMD+1.5,JM=-2*SBD/DPHD+1.5)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &( IMJM=IM*JM-JM/2,KB=IM*2+JM-3,IMJM1=IMJM-1
C     &, LDM=10,LDMM1=LDM-1,LDMP1=LDM+1
CWesl=12 Nivoa     &, LDM=12,LDMM1=LDM-1,LDMP1=LDM+1       !for Wesley
     &, LDM=20,LDMM1=LDM-1,LDMP1=LDM+1       !for Wesley
C-----------------------------------------------------------------------
C TBOCO- BOUNDARY CONDITIONS TIME STEP
C NHOUR- NUMBER OF HOURS ON WHICH USED LATERAL BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
cdaca     &, TBOCO=3.E0,NHOUR=48)
C    &, TBOCO=12.E0,NHOUR=720)					!for CLAM
     &, TBOCO=6.E0,NHOUR=24)
C-----------------------------------------------------------------------
                           P A R A M E T E R 
C-----------------------------------------------------------------------
     & (ISTERM=0,IDACA=24./TBOCO+0.5,ITERM=NHOUR/TBOCO+1.5+ISTERM
     &, ITERM1=ITERM-1,LNFM=20,LNF=LNFM+10,LNFP1=LNF+1+ISTERM
     &, ITBC=ITERM1-ISTERM
C--------------TIME DIFFERENCING CONSTANTS------------------------------
     &, DT=DTB,W=.25,IDTAD=2, PI=3.141592654,DTR=PI/180. )  
C-----------------------------------------------------------------------
                             P A R A M E T E R                        
     & (IMT=2*IM-1,JMT=JM/2+1,IMM1=IMT-1, IMM6=IMT-6
     &, IMM5=IMT-5,IMM3=IMT-3,IMM12=IMT-12
     &, IMM7=IMT-7,IMM8=IMT-8, IMM9=IMT-9, IMM10=IMT-10, IMM13=IMT-13      
     &, JMM1=JMT-1,JMM3=JMT-3, JMM4=JMT-4, JMM5=JMT-5,JMM2=JMT-2
     &, LMM1=LM-1, LMP1=LM+1
     &, ITBQ=152,JTBQ=440
     &, ITB=076,JTB=134)           
                             P A R A M E T E R                          
     & (IM2=IM-2,JM1=JM-1,JM2=JM-2,JM3=JM-3,JM4=JM-4,JM5=JM-5 
     &, JAM=6+2*(JM-10)
     &, KVN=IM,KVS=-IM+1      
     &, KHN=IM-1,KHS=-IM
     &, KNE=IM    ,KNW=IM-1 ,KSW=-IM    ,KSE=-IM+1 
     &, KHL00=1                    ,KHH00=IM*JM-JM/2 
     &, LP1=LM+1,LM1=LM-1)                                              
C-----------------------------------------------------------------------
                             P A R A M E T E R                          
     & (LBB=2*IM+JM-3,LB=2*JMT+IMT-3,IMTM1=IMT-1,JMTM1=JMT-1)
                             P A R A M E T E R                          
     & (LIST=03,MNTSTP=09,NCIN=12
C----------INPUT FILES FOR PREPROCESING---------------------------------
     &, MOUNT=13,IANSURF=11,IANSPL1=LNFP1
     &, NFCSTE=15,NBCE=17
C----------INPUT FILES FOR HIBU-----------------------------------------
     &, NFCST=16,NBC=18,NHIBU=14
c    &, NFCST2D=116       ,NHIBU2D=114)
     &, NFCST2D=46       ,NHIBU2D=44)
c    &, NFCST2D=76       ,NHIBU2D=74)
C-----------------------------------------------------------------------
                             P A R A M E T E R                          
C--------------STEREOGRAPHIC MAP CONSTANTS------------------------------
C                                                                       
C        MAP GEOMETRY CONSTANTS                                         
C   --   UNIT OF DISTANCE OF THE IMMAGE SURFACE X1,Y1 AND X,Y SYSTEMS IS
C           THE SHORTER SIDE OF THE PRINTING CELL (2.54/10. CM)         
C   --   X1P,Y1P ARE THE NORTH POLE COORDINATES                         
C   --   DP30 IS THE DISTANCE, ON THE OVERLAY MAP, BETWEEN THE          
C           NORTH POLE AND 30 DEG N LATITUDE CIRCLE, IN CM              
C           DP30 =18.02 * 2.54
C   --   CMLD IS THE CENTRAL MAP MERIDIAN LONGITUDE, IN DEG 
c
C-----------------------------------------------------------------------
     & (CMLD=TLM0D,DP30=45.78,X1P=57.,Y1P=172.,IXM=65,IYM=82)
C-----------------------------------------------------------------------
                             P A R A M E T E R                          
     & (A1=610.78E0,A2=17.2693882E0,A3=273.16E0,A4=35.86E0              
     &, PQ0=379.90516E0,D61=.61E0
     &, TRESH=.95E0)                                                    
                             P A R A M E T E R                          
     & (D00=0.E0,D5=5.E-1,D01=1.00E-2,H1=1.E0,HM1=-1.E0                 
     &, H90=90.E0,H360=360.E0,EPS=1.E-10)                               
C-----------------------------------------------------------------------
                             L O G I C A L                              
     &  PRINT, TEST, RUN, HMONLY, IDTBLS, FPMNTS, SILUET, SIGMA, MRMSXL 
     &, NOMNTS,RUNQ,NOZ,LPI8,NOCYL
C-----------------------------------------------------------------------
      DIMENSION IDAT(3),SPL(LDM),DETA(LM),LDT1(LM)
C-----------------------------------------------------------------------
                             P A R A M E T E R                          
     &(G=9.80,R=287.04,GAMMA=0.0065,PRF0=101325.,T0=288.)
C-----------------------------------------------------------------------
C .............
C
                             L O G I C A L
     & SURFI,CLIMSST,SURFT,SURFW,SURFSNO
                             P A R A M E T E R
     &(SURFI=.FALSE.,CLIMSST=.TRUE.,SURFSNO=.TRUE.)
                             P A R A M E T E R
     &(SURFT=.TRUE.,SURFW=.TRUE.)
                             L O G I C A L
     & SOILI,VEGI
                             P A R A M E T E R
     &(SOILI=.TRUE.,VEGI=.TRUE.)
                             P A R A M E T E R
     &(SIGMA=.FALSE.)
C    &(SIGMA=.TRUE.)
C-------- end of   large.inc -------------------------------------------
