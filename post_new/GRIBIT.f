      SUBROUTINE GRIBIT(IFLD,ILVL,GRID,IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GRIBIT      POST FIELDS IN GRIB1
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-06-18       
C     
C ABSTRACT:
C     THIS ROUTINE POSTS THE DATA IN THE PASSED ARRAY GRID
C     TO THE OUTPUT FILE IN GRIB1 FORMAT.
C     
C PROGRAM HISTORY LOG:
C   93-06-18  RUSS TREADON
C   93-11-23  RUSS TREADON - REMOVED CODE GENERATING GRIB INDEX FILE.
C   98-07-17  MIKE BALDWIN - REMOVED LABL84, NOW USING ID
C     
C USAGE:    CALL GRIBIT(IFLD,ILVL,GRID,IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IFLD     - FIELD ID TAG.
C     ILVL     - INTEGER TAG FOR LEVEL OF FIELD.
C     GRID     - FIELD TO BE POSTED IN GRIB.
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     
C   OUTPUT FILES:
C     STDOUT    - RUN TIME STANDARD OUT.
C     LUNOUT+1  - UNIT TO RECEIVE GRIB1 DATA.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     GETENV   - CRAY SUBROUTINE TO GET VALUE OF ENVIRONMENT VARIABLE.
C     MINMAX   - DETERMINES MIN/MAX VALUES IN AN ARRAY.
C     WRYTE    - WRITE DATA OUT BY BYTES.
C     GET_BITS   - COMPUTE NUMBER OF BITS 
C     VARIOUS W3LIB ROUTINES
C     LIBRARY:
C       COMMON   - CTLBLK
C                  MAPOT
C                  RQSTFLD
C                  IOUNIT
C                  OUTGRD
C                  GRBDAT
C                  AVBLFLDS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C
C
C     INCLUDE GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "parmout"
      PARAMETER (LP1=LM+1,D01=0.01,D50=0.5E0)
      PARAMETER (IMT=2*IM-1,JMT=JM,IMJMT=IMT*JMT)
C     
C     GRIB1 PARAMETERS.
C        MNBIT  = MINIMUM NUMBER OF BITS TO USE IN PACKING.
C        MXBIT  = MAXIMUM NUMBER OF BITS TO USE IN PACKING.
C        LENPDS = LENGTH OF GRIB1 PDS.
C        LENGDS = LENGTH OF GRIB1 GDS.
C     
      PARAMETER (MNBIT=0,MXBIT=16,LENPDS=28,LENGDS=32)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESRT,SIGMA,OLDRD,STRD
      LOGICAL NORTH
      CHARACTER*1  KBUF(30+LENPDS+LENGDS+IMOUT*JMOUT*(MXBIT+2)/8)
      CHARACTER*1  IFLAG
      CHARACTER*4  RESTHR,BLANK
      CHARACTER*6  CRUN,PROJ
      CHARACTER*7  DESCR2
      CHARACTER*28 PDS
Cmp      CHARACTER*50 ENVAR
      CHARACTER*70 ENVAR
      CHARACTER*80 FNAME,OPATH
      CHARACTER*90 CMD
      INTEGER IBDSFL(9)
Cmp      INTEGER IGRD(IMOUT,JMOUT),IGDS(18),IBMASK(IMOUT,JMOUT)
      INTEGER IGRD(IMOUT,JMOUT),IBMASK(IMOUT,JMOUT)
Cmp
      REAL GRID(IMOUT,JMOUT),GRIDOT(IMOUT,JMOUT)
C     
C     THE BELOW VARIABLE ARE ONLY NEEDED FOR THE CALL TO W3FI63.
      REAL DATAFLD(IMOUT,JMOUT)
      INTEGER KGDS(20),KPTR(16)
      LOGICAL KBMS(IMOUT,JMOUT)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "OUTGRD.comm"
      INCLUDE "BITMAP.comm"
      INCLUDE "GRBDAT.comm"
Cmp
        INCLUDE "GDS.com"
Cmp
C     
C     SET DEFAULT GRIB1 PARAMETERS.  
C     PARAMETERS MNBIT, MXBIT, IBX, AND NBIT ARE USED 
C     IN THE CALL TO GET_BITS.
C        IBX    = DESIRED BINARY PRECISION.
C        NBIT   = NUMBER OF BITS TO USE IN PACKING DATA.
C     
      DATA IBX,NBIT / 0, 12 /
      DATA BLANK /'    '/
      SAVE OPATH
C
      INTEGER NMC_dec_scale(255)
      NMC_dec_scale(  1)=  0 ! PRES   ORIG VALUE = -1
      NMC_dec_scale(  2)=  0 ! PRMSL  ORIG VALUE = -1
      NMC_dec_scale(  3)=  5 ! PTEND
      NMC_dec_scale(  4)=  0 ! var4
      NMC_dec_scale(  5)=  0 ! var5
      NMC_dec_scale(  6)= -1 ! GP
      NMC_dec_scale(  7)=  0 ! HGT
      NMC_dec_scale(  8)=  0 ! DIST
      NMC_dec_scale(  9)=  0 ! HSTDV
      NMC_dec_scale( 10)=  0 ! HVAR
      NMC_dec_scale( 11)=  2 ! TMP    ORIG VALUE =  1
      NMC_dec_scale( 12)=  1 ! VTMP
      NMC_dec_scale( 13)=  2 ! POT    ORIG VALUE =  1
      NMC_dec_scale( 14)=  1 ! EPOT
      NMC_dec_scale( 15)=  1 ! TMAX
      NMC_dec_scale( 16)=  1 ! TMIN
      NMC_dec_scale( 17)=  2 ! DPT    ORIG VALUE =  1
      NMC_dec_scale( 18)=  1 ! DEPR
      NMC_dec_scale( 19)=  4 ! LAPR
      NMC_dec_scale( 20)=  0 ! VISIB
      NMC_dec_scale( 21)=  0 ! RDSP1
      NMC_dec_scale( 22)=  0 ! RDSP2
      NMC_dec_scale( 23)=  0 ! RDSP3
      NMC_dec_scale( 24)=  0 ! var24
      NMC_dec_scale( 25)=  1 ! TMPA
      NMC_dec_scale( 26)= -1 ! PRESA
      NMC_dec_scale( 27)=  0 ! GPA
      NMC_dec_scale( 28)=  0 ! WVSP1
      NMC_dec_scale( 29)=  0 ! WVSP2
      NMC_dec_scale( 30)=  0 ! WVSP3
      NMC_dec_scale( 31)=  0 ! WDIR
      NMC_dec_scale( 32)=  1 ! WIND
      NMC_dec_scale( 33)=  1 ! UGRD
      NMC_dec_scale( 34)=  1 ! VGRD
      NMC_dec_scale( 35)= -4 ! STRM
      NMC_dec_scale( 36)= -4 ! VPOT
      NMC_dec_scale( 37)= -1 ! MNTSF
      NMC_dec_scale( 38)=  8 ! SGCVV
      NMC_dec_scale( 39)=  3 ! VVEL
      NMC_dec_scale( 40)=  4 ! DZDT
      NMC_dec_scale( 41)=  6 ! ABSV
      NMC_dec_scale( 42)=  6 ! ABSD
      NMC_dec_scale( 43)=  6 ! RELV
      NMC_dec_scale( 44)=  6 ! RELD
      NMC_dec_scale( 45)=  4 ! VUCSH
      NMC_dec_scale( 46)=  4 ! VVCSH
      NMC_dec_scale( 47)=  0 ! DIRC
      NMC_dec_scale( 48)=  0 ! SPC
      NMC_dec_scale( 49)=  0 ! UOGRD
      NMC_dec_scale( 50)=  0 ! VOGRD
      NMC_dec_scale( 51)=  6 ! SPFH   ORIG VALUE =  4
      NMC_dec_scale( 52)=  1 ! RH     ORIG VALUE =  0
      NMC_dec_scale( 53)=  4 ! MIXR
      NMC_dec_scale( 54)=  1 ! PWAT
      NMC_dec_scale( 55)= -1 ! VAPP
      NMC_dec_scale( 56)= -1 ! SATD
      NMC_dec_scale( 57)=  1 ! EVP
      NMC_dec_scale( 58)=  1 ! CICE
      NMC_dec_scale( 59)=  6 ! PRATE
      NMC_dec_scale( 60)=  0 ! TSTM
      NMC_dec_scale( 61)=  2 ! APCP   ORIG VALUE =  1
      NMC_dec_scale( 62)=  2 ! NCPCP  ORIG VALUE =  1
      NMC_dec_scale( 63)=  2 ! ACPCP  ORIG VALUE =  1
      NMC_dec_scale( 64)=  6 ! SRWEQ
      NMC_dec_scale( 65)=  2 ! WEASD  ORIG VALUE =  0
      NMC_dec_scale( 66)=  2 ! SNOD
      NMC_dec_scale( 67)=  0 ! MIXHT
      NMC_dec_scale( 68)=  0 ! TTHDP
      NMC_dec_scale( 69)=  0 ! MTHD
      NMC_dec_scale( 70)=  0 ! MTHA
      NMC_dec_scale( 71)=  1 ! TCDC   ORIG VALUE =  0
      NMC_dec_scale( 72)=  1 ! CDCON  ORIG VALUE =  0
      NMC_dec_scale( 73)=  1 ! LCDC   ORIG VALUE =  0
      NMC_dec_scale( 74)=  1 ! MCDC   ORIG VALUE =  0
      NMC_dec_scale( 75)=  1 ! HCDC   ORIG VALUE =  0
      NMC_dec_scale( 76)=  1 ! CWAT
      NMC_dec_scale( 77)=  0 ! var77
      NMC_dec_scale( 78)=  1 ! SNOC
      NMC_dec_scale( 79)=  1 ! SNOL
      NMC_dec_scale( 80)=  2 ! WTMP   ORIG VALUE =  1
      NMC_dec_scale( 81)=  1 ! LAND   ORIG VALUE =  0
      NMC_dec_scale( 82)=  0 ! DSLM
      NMC_dec_scale( 83)=  5 ! SFCR
      NMC_dec_scale( 84)=  0 ! ALBDO
      NMC_dec_scale( 85)=  2 ! TSOIL  ORIG VALUE =  1
      NMC_dec_scale( 86)=  0 ! SOILM
      NMC_dec_scale( 87)=  0 ! VEG
      NMC_dec_scale( 88)=  0 ! SALTY
      NMC_dec_scale( 89)=  4 ! DEN
      NMC_dec_scale( 90)=  1 ! RUNOF
      NMC_dec_scale( 91)=  1 ! ICEC   ORIG VALUE =  0
      NMC_dec_scale( 92)=  0 ! ICETK
      NMC_dec_scale( 93)=  0 ! DICED
      NMC_dec_scale( 94)=  0 ! SICED
      NMC_dec_scale( 95)=  0 ! UICE
      NMC_dec_scale( 96)=  0 ! VICE
      NMC_dec_scale( 97)=  0 ! ICEG
      NMC_dec_scale( 98)=  0 ! ICED
      NMC_dec_scale( 99)=  2 ! SNOM   ORIG VALUE =  1
      NMC_dec_scale(100)=  0 ! HTSGW
      NMC_dec_scale(101)=  0 ! WVDIR
      NMC_dec_scale(102)=  0 ! WVHGT
      NMC_dec_scale(103)=  0 ! WVPER
      NMC_dec_scale(104)=  0 ! SWDIR
      NMC_dec_scale(105)=  0 ! SWELL
      NMC_dec_scale(106)=  0 ! SWPER
      NMC_dec_scale(107)=  0 ! DIRPW
      NMC_dec_scale(108)=  0 ! PERPW
      NMC_dec_scale(109)=  0 ! DIRSW
      NMC_dec_scale(110)=  0 ! PERSW
      NMC_dec_scale(111)=  0 ! NSWRS
      NMC_dec_scale(112)=  0 ! NLWRS
      NMC_dec_scale(113)=  0 ! NSWRT
      NMC_dec_scale(114)=  0 ! NLWRT
      NMC_dec_scale(115)=  0 ! LWAVR
      NMC_dec_scale(116)=  0 ! SWAVR
      NMC_dec_scale(117)=  0 ! GRAD
      NMC_dec_scale(118)=  0 ! var118
      NMC_dec_scale(119)=  0 ! var119
      NMC_dec_scale(120)=  0 ! var120
      NMC_dec_scale(121)=  0 ! LHTFL
      NMC_dec_scale(122)=  0 ! SHTFL
      NMC_dec_scale(123)=  0 ! BLYDP
      NMC_dec_scale(124)=  3 ! UFLX
      NMC_dec_scale(125)=  3 ! VFLX
      NMC_dec_scale(126)=  0 ! WMIXE
      NMC_dec_scale(127)=  0 ! IMGD
      NMC_dec_scale(128)= -1 ! MSLSA
      NMC_dec_scale(129)= -1 ! MSLMA
      NMC_dec_scale(130)=  0 ! MSLET  ORIG VALUE = -1
      NMC_dec_scale(131)=  1 ! LFTX
      NMC_dec_scale(132)=  1 ! 4LFTX
      NMC_dec_scale(133)=  1 ! KX
      NMC_dec_scale(134)=  1 ! SX
      NMC_dec_scale(135)= 10 ! MCONV
      NMC_dec_scale(136)=  4 ! VSSH
      NMC_dec_scale(137)=  5 ! TSLSA
      NMC_dec_scale(138)=  6 ! BVF2
      NMC_dec_scale(139)= 11 ! PVMW
      NMC_dec_scale(140)=  1 ! CRAIN  ORIG VALUE =  0
      NMC_dec_scale(141)=  1 ! CFRZR  ORIG VALUE =  0
      NMC_dec_scale(142)=  1 ! CICEP  ORIG VALUE =  0
      NMC_dec_scale(143)=  1 ! CSNOW  ORIG VALUE =  0
      NMC_dec_scale(144)=  2 ! SOILW  ORIG VALUE =  0
      NMC_dec_scale(145)=  0 ! PEVPR
      NMC_dec_scale(146)=  0 ! CWORK
      NMC_dec_scale(147)=  0 ! U-GWD
      NMC_dec_scale(148)=  0 ! V-GWD
      NMC_dec_scale(149)=  0 ! PV___
      NMC_dec_scale(150)=  0 ! var150
      NMC_dec_scale(151)=  0 ! var151
      NMC_dec_scale(152)=  0 ! var152
      NMC_dec_scale(153)=  0 ! MFXDV
      NMC_dec_scale(154)=  0 ! var154
      NMC_dec_scale(155)=  0 ! GFLUX
      NMC_dec_scale(156)=  0 ! CIN
      NMC_dec_scale(157)=  0 ! CAPE
      NMC_dec_scale(158)=  2 ! TKE    ORIG VALUE =  0
      NMC_dec_scale(159)=  0 ! CONDP
      NMC_dec_scale(160)=  0 ! CSUSF
      NMC_dec_scale(161)=  0 ! CSDSF
      NMC_dec_scale(162)=  0 ! CSULF
      NMC_dec_scale(163)=  0 ! CSDLF
      NMC_dec_scale(164)=  0 ! CFNSF
      NMC_dec_scale(165)=  0 ! CFNLF
      NMC_dec_scale(166)=  0 ! VBDSF
      NMC_dec_scale(167)=  0 ! VDDSF
      NMC_dec_scale(168)=  0 ! NBDSF
      NMC_dec_scale(169)=  0 ! NDDSF
      NMC_dec_scale(170)=  3 ! USTR
      NMC_dec_scale(171)=  3 ! VSTR
      NMC_dec_scale(172)=  3 ! MFLX
      NMC_dec_scale(173)=  0 ! LMH
      NMC_dec_scale(174)=  0 ! LMV
      NMC_dec_scale(175)=  0 ! SGLYR
      NMC_dec_scale(176)=  2 ! NLAT
      NMC_dec_scale(177)=  2 ! NLON
      NMC_dec_scale(178)=  2 ! UMAS
      NMC_dec_scale(179)=  2 ! VMAS
      NMC_dec_scale(180)=  0 ! var180
      NMC_dec_scale(181)=  9 ! LPSX
      NMC_dec_scale(182)=  9 ! LPSY
      NMC_dec_scale(183)=  5 ! HGTX
      NMC_dec_scale(184)=  5 ! HGTY
      NMC_dec_scale(185)=  0 ! STDZ
      NMC_dec_scale(186)=  0 ! STDU
      NMC_dec_scale(187)=  0 ! STDV
      NMC_dec_scale(188)=  0 ! STDQ
      NMC_dec_scale(189)=  0 ! STDT
      NMC_dec_scale(190)=  0 ! CBUW
      NMC_dec_scale(191)=  0 ! CBVW
      NMC_dec_scale(192)=  0 ! CBUQ
      NMC_dec_scale(193)=  0 ! CBVQ
      NMC_dec_scale(194)=  0 ! CBTW
      NMC_dec_scale(195)=  0 ! CBQW
      NMC_dec_scale(196)=  0 ! CBMZW
      NMC_dec_scale(197)=  0 ! CBTZW
      NMC_dec_scale(198)=  0 ! CBTMW
      NMC_dec_scale(199)=  0 ! STDRH
      NMC_dec_scale(200)=  0 ! SDTZ
      NMC_dec_scale(201)=  0 ! ICWAT
      NMC_dec_scale(202)=  0 ! SDTU
      NMC_dec_scale(203)=  1 ! SDTV   ORIG VALUE =  0
      NMC_dec_scale(204)=  0 ! DSWRF
      NMC_dec_scale(205)=  0 ! DLWRF
      NMC_dec_scale(206)=  0 ! SDTQ
      NMC_dec_scale(207)=  0 ! MSTAV
      NMC_dec_scale(208)=  0 ! SFEXC
      NMC_dec_scale(209)=  0 ! MIXLY
      NMC_dec_scale(210)=  0 ! SDTT
      NMC_dec_scale(211)=  0 ! USWRF
      NMC_dec_scale(212)=  0 ! ULWRF
      NMC_dec_scale(213)=  1 ! CDLYR  ORIG VALUE =  0
      NMC_dec_scale(214)=  6 ! CPRAT
      NMC_dec_scale(215)=  7 ! TTDIA
      NMC_dec_scale(216)=  7 ! TTRAD
      NMC_dec_scale(217)=  7 ! TTPHY
      NMC_dec_scale(218)=  2 ! PREIX
      NMC_dec_scale(219)=  1 ! TSD1D
      NMC_dec_scale(220)=  4 ! NLSGP
      NMC_dec_scale(221)=  0 ! SDTRH
      NMC_dec_scale(222)=  0 ! 5WAVH
      NMC_dec_scale(223)=  1 ! CWAT
      NMC_dec_scale(224)=  1 ! PLTRS
      NMC_dec_scale(225)=  1 ! var225
      NMC_dec_scale(226)=  0 ! BMIXL
      NMC_dec_scale(227)=  0 ! AMIXL
      NMC_dec_scale(228)=  1 ! PEVAP
      NMC_dec_scale(229)=  0 ! SNOHF
      NMC_dec_scale(230)=  0 ! var230
      NMC_dec_scale(231)=  3 ! MFLUX
      NMC_dec_scale(232)=  0 ! DTRF
      NMC_dec_scale(233)=  0 ! UTRF
      NMC_dec_scale(234)=  0 ! BGRUN
      NMC_dec_scale(235)=  0 ! SSRUN
      NMC_dec_scale(236)=  1 ! var236 ORIG VALUE =  0
      NMC_dec_scale(237)=  0 ! OZONE
      NMC_dec_scale(238)=  0 ! SNOC
      NMC_dec_scale(239)=  1 ! SNOT
      NMC_dec_scale(240)=  1 ! GLCR   ORIG VALUE =  0
      NMC_dec_scale(241)=  7 ! LRGHR
      NMC_dec_scale(242)=  7 ! CNVHR
      NMC_dec_scale(243)= 10 ! CNVMR
      NMC_dec_scale(244)=  7 ! SHAHR
      NMC_dec_scale(245)= 10 ! SHAMR
      NMC_dec_scale(246)=  7 ! VDFHR
      NMC_dec_scale(247)=  7 ! VDFUA
      NMC_dec_scale(248)=  7 ! VDFVA
      NMC_dec_scale(249)= 10 ! VDFMR
      NMC_dec_scale(250)=  7 ! SWHR
      NMC_dec_scale(251)=  7 ! LWHR
      NMC_dec_scale(252)=  0 ! CD
      NMC_dec_scale(253)=  2 ! FRICV  ORIG VALUE =  0
      NMC_dec_scale(254)=  0 ! RI
      NMC_dec_scale(255)=  0 ! var255
C
C*****************************************************************************
C     START GRIBIT HERE.
C
C     SET NUMBER OF OUTPUT GRID POINTS.
      IJOUT = IMOUT*JMOUT
C     
C     PREPARE GRIB PDS
C     
C     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
C        ID(1)  = NUMBER OF BYTES IN PRODUCT DEFINITION SECTION (PDS)
C        ID(2)  = PARAMETER TABLE VERSION NUMBER
C        ID(3)  = IDENTIFICATION OF ORIGINATING CENTER
C        ID(4)  = MODEL IDENTIFICATION (ALLOCATED BY ORIGINATING CENTER)
C        ID(5)  = GRID IDENTIFICATION
C        ID(6)  = 0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
C        ID(7)  = 0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
C        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
C        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
C        ID(10) = VALUE 1 OF LEVEL (=0 FOR 1-100,102,103,105,107,
C          109,111,113,115,117,119,125,160,200,201 LEVEL IS IN ID WORD 11)
C        ID(11) = VALUE 2 OF LEVEL
C        ID(12) = YEAR OF CENTURY
C        ID(13) = MONTH OF YEAR
C        ID(14) = DAY OF MONTH
C        ID(15) = HOUR OF DAY
C        ID(16) = MINUTE OF HOUR   (IN MOST CASES SET TO 0)
C        ID(17) = FCST TIME UNIT
C        ID(18) = P1 PERIOD OF TIME
C        ID(19) = P2 PERIOD OF TIME
C        ID(20) = TIME RANGE INDICATOR
C        ID(21) = NUMBER INCLUDED IN AVERAGE
C        ID(22) = NUMBER MISSING FROM AVERAGES
C        ID(23) = CENTURY  (20, CHANGE TO 21 ON JAN. 1, 2001)
C        ID(24) = RESERVED - SET TO 0
C        ID(25) = SCALING POWER OF 10
C
      IF (IOUTYP.EQ.3.OR.IOUTYP.EQ.5) THEN
C     
C        PREPARE DATE PART OF GRIB PDS RECORD.
         IFHR       = NTSD/TSPH+D50
         ICENT      = (IDAT(3)-1)/100 + 1
         IYY        = IDAT(3) - (ICENT-1)*100
         IMM        = IDAT(1)
         IDD        = IDAT(2)
         AYEAR0     = IYY
         AMNTH0     = IMM
         ADAY0      = IDD
         AGMT0      = IHRST
         ID(01)     = 28
         ID(02)     = 2
         ID(03)     = 7
         ID(12)     = IYY
         ID(13)     = IMM
         ID(14)     = IDD
         ID(15)     = IHRST
         ID(16)     = 0

C	ID(17)=1 (hourly time increment.  limits to 256 hours)
C	ID(17)=10 (3 hour increment.  limits to 768 hours)
C	
C	below options are listed, but currently would have problems
C	beyond 999 hours due to filename constraints.  
C
C	ID(17)=11 (6 hour increment.  limits to 1536 hours)
C	ID(17)=12 (12 hour increment.  allows 3072 hours)
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         ID(17)     = 1
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

	IF (ID(17) .eq. 1) IFACT=1
	IF (ID(17) .eq. 10) IFACT=3
	IF (ID(17) .eq. 11) IFACT=6
	IF (ID(17) .eq. 12) IFACT=12
C
C    ASSUMING ID(18-20), (P1, P2, TIME RANGE INDICATOR) 
C    ARE PASSED IN CORRECTLY IF NOT AN INSTANTANEOUS FIELD
C   
         IF (ID(20).EQ.0) THEN
          ID(18)     = IFHR/IFACT
          ID(19)     = 0
         ENDIF

!tst
	IF (ID(20).EQ.3 .OR. ID(20).EQ.4) THEN
	  ID(18)     = ID(18)/IFACT
          ID(19)     = ID(19)/IFACT
	ENDIF
!endtst

         ID(21)     = 0
         ID(22)     = 0
         ID(23)     = ICENT
         ID(24)     = 0
C
C     
C        SET OUTPUT GRID TYPE.  WE ASSUME KGYTPE HOLDS THE GRIB
C        ID FOR THE OUTPUT GRID.  
C
         KGTYP = KGTYPE
C     
C        SET GRID TYPE ID(5)
C        GENERATING PROGRAM ID(4)
C
         IJOUT      = IMOUT*JMOUT
         ID(4) = IMDLTY
         ID(5) = KGTYP
C
C        ID(6) =0 IF NO GDS SECTION, =1 IF GDS INCLUDED, 
C                 ALWAYS INCLUDE GDS
C
         ID(6) = 1
C     
C        SET DATA TYPE ID(8) AND SURFACE ID(9).
C
C     DON'T SET PARAMETER IF PRECIP TYPE, SINCE THERE ARE
C     4 PARAMETER NUMBERS FOR THE SAME IFLD
C
         IF (ID(8).LT.140.OR.ID(8).GT.143) ID(8) = IQ(IDENT(IFLD))
         IF (ID(9).EQ.0) ID(9) = IS(IDENT(IFLD))
C     
C        SET VALUE OF LEVEL IF ON PRESSURE OR ETA SURFACE.
C        OTHERWISE, WE ASSUME ID(10) AND (11) ARE SET 
C        APPROPRIATELY PRIOR TO ENTERING GRIBIT.
C     
         IF (ID(9).EQ.100)  THEN
            ISVALUE = NINT(SPL(ILVL)*D01)
            ID(10) = 0
            ID(11) = ISVALUE
         ELSEIF (ID(9).EQ.119) THEN
            ISVALUE = NINT(AETA(ILVL)*10000.)
C
C   TKE IS ON THE ETA INTERFACE AT THE BOTTOM OF THE LAYER ILVL
C
            IF (ID(8).EQ.158) ISVALUE = NINT(ETA(ILVL+1)*10000.)
            ID(10) = 0
            ID(11) = ISVALUE
         ELSEIF (ID(9) .EQ. 109) THEN
            ISVALUE = ILVL
            ID(10) = 0
            ID(11) = ISVALUE
         ENDIF
C     
C     END OF GRIB PDS LABEL PREPARATION.
C
      ENDIF

C     
C     SET DECIMAL SCALING (IDECI) FROM LIST IN INCLUDE FILE 
C     RQSTFLD.  A CALL TO GET_BITS WILL COMPUTE THE NUMBER OF
C     BITS NECESSARY TO PACK THE DATA BASED ON THE RANGE OF 
C     THE FIELD.  THE FIELD IS SCALED TO THIS PRECISION AND
C     RETURNED FOR PACKING BY THE GRIB PACKER.
C     
      DO JJ = 1,JMOUT
       DO II = 1,IMOUT
        IBMASK(II,JJ)=IBMAP(II,JJ)
       ENDDO
      ENDDO
      IBM = 0
      IBITM = 0
      SGDG  = DEC(IFLD)
!$omp  parallel do
      DO J=1,JMOUT
      DO I=1,IMOUT
        GRIDOT(I,J)=GRID(I,J)
      ENDDO
      ENDDO
C
      DO J=1,JMOUT
      DO I=1,IMOUT
        IBITM=IBITM+IBMASK(I,J)
      ENDDO
      ENDDO
C
C        ID(7) =0 IF NO BMS SECTION, =1 IF BMS INCLUDED
C
      IF (IBITM.EQ.IJOUT) THEN
        ID(7) = 0
        IBM = 0
      ELSE
        ID(7) = 1
        IBM = 1
      ENDIF
      IDECI=NMC_dec_scale(ID(8))
      CALL GETBIT(IBM,0,IDECI,IJOUT,IBMASK,GRID,
     &                GRIDOT,GMIN,GMAX,NBIT)
C
C        ID(25) = SCALING POWER OF 10
C
      ID(25) = IDECI
C     
C     GENERATE COMPLETE GRIB1 MESSAGE USING W3FI72.
C        ITYPE  = 0 SPECIFIES REAL DATA TO BE PACKED.
C        IGRD   = DUMMY ARRAY FOR INTEGER DATA.
C        IBITL  = NBIT TELLS W3FI72 TO PACK DATA USING NBIT BITS.
C        IPFLAG = 0 IS PDS INFORMATION IN USER ARRAY ID.
C                 1 IS PDS (GENERATED ABOVE BY W3FP12).
C        ID     = (DUMMY) ARRAY FOR USER DEFINED PDS.
C        IGFLAG = 0 TELLS W3FI72 TO MAKE GDS USING IGRID.
C                 1 IS GDS GENERATED BY USER IN ARRAY IGDS
C        IGRID  = GRIB1 GRID TYPE (TABLE B OF ON388).
C        IGDS   = ARRAY FOR USER DEFINED GDS.
C        ICOMP  = 0 FOR EARTH ORIENTED WINDS,
C                 1 FOR GRID ORIENTED WINDS.
C        IBFLAG = 0 TELLS W3FI72 TO MAKE BIT MAP FROM USER
C                 SUPPLIED DATA.
C        IBMASK = ARRAY CONTAINING USER DEFINED BIT MAP.
C        IBLEN  = LENGTH OF ARRAY IBMASK.
C        IBDSFL = ARRAY CONTAINING TABLE 11 (ON388) FLAG INFORMATION.
C        NPTS   = LENGTH OF ARRAY GRID OR IGRD.  MUST AGREE WITH IBLEN.
C     
C     INTIALIZE VARIABLES.
      ITYPE  = 0
!$omp  parallel do
      DO J=1,JMOUT
      DO I=1,IMOUT
        IGRD(I,J)=0
      ENDDO
      ENDDO
C
      IBITL  = MIN(NBIT,MXBIT)
C
      IPFLAG = 0
C
      IGFLAG = 0
Cwrkst      IGRID  = ID(5)
      IF (IGRID.EQ.26) IGRID=6
Cwrkst      DO 20 K = 1,18
Cwrkst        IGDS(K) = 0
Cwrkst 20   CONTINUE
      ICOMP  = 1
      IF (INDEX(PROJ,'LOLA').NE.0) ICOMP = 0
      IBFLAG = 0
      IBLEN  = IJOUT
      DO 30 K = 1,9
         IBDSFL(K) = 0
 30   CONTINUE

Cmp     this is where things need to be defined
Cmp
Cmp     want IGRID=255 (user defined type)
Cmp     IGDS needs to have the w3fi71 style GDS (18 elements)
Cmp     also NEED IGFLAG=1
Cmp
Cmp     what to do with bitmap IBFLAG/IBLEN?
        IGFLAG=1
        IGRID=255
Cmp

C
!	write(6,*) 'ID= ', ID
      CALL W3FI72(ITYPE,GRIDOT,IGRD,IBITL,
     X            IPFLAG,ID,PDS,
     X            IGFLAG,IGRID,IGDS,ICOMP,
     X            IBFLAG,IBMASK,IBLEN,
     X            IBDSFL,
     X            NPTS,KBUF,ITOT,IER)
C     
C     EXPLICITLY SET BYTE 12 OF KBUF (BYTE 4 OF THE PDS)
C     TO 2.  THIS WILL REFER ALL QUANTITIES TO PARAMETER
C     TABLE VERSION 2 OF WHICH TABLE VERSION 1 IS A SUBSET.
C     THIS IS NEEDED BECAUSE THE W3 ROUTINES HARDWIRE THIS
C     VALUE TO 1 YET SOME OF THE OUTPUT VARIABLES ARE ONLY 
C     DEFINED IN VERSION 2 OF THE PARAMETER TABLE.
C
      KBUF(12)=CHAR(2)
C
      IF (IER.NE.0) THEN
         WRITE(STDOUT,1040) IER,FIELD(IFLD)
 1040    FORMAT('GRIBIT:  ***W3FI72 ERROR IER=',I1,
     X        ' FOR ',A20)
         WRITE(STDOUT,*)'GRIBIT:  DID NOT POST THIS FIELD'
         RETURN
      ENDIF
C     
C     ON FIRST ENTRY MAKE OUTPUT DIRECTORY.  SET SWITCH (RITEHD)
C     TO FALSE FOR SUBSEQUENT ENTRIES.
      IF ( ((IOUTYP.EQ.3).AND.RITEHD) .OR.
     X     ((IOUTYP.EQ.5).AND.RITEHD) .OR.
     X     ((IOUTYP.EQ.4).AND.RITE2 ) ) THEN
C
C        PUT FORECAST HOUR INTO DIR PREFIX FOR GRIB FILE.
         IHR = NTSD/TSPH + 0.5
C     
C        GET FULL PATH FOR OUTPUT FILE FROM ENVIRONMENT VARIABLE
C        COMSP WHICH IS SET IN THE SCRIPT RUNNING THE MODEL.
C     
C        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         ENVAR = ' '
         RESTHR = ' '
         CALL GETENV('COMSP',ENVAR)
         CALL GETENV('tmmark',RESTHR)
         KDAT = INDEX(DATSET,' ') -1
         IF (KDAT.LE.0) KDAT = LEN(DATSET)
         KENV = INDEX(ENVAR,' ') -1
         IF (KENV.LE.0) KENV = LEN(ENVAR)
         KTHR = INDEX(RESTHR,' ') -1
         IF (KTHR.LE.0) KTHR = LEN(RESTHR)
       IF (IOUTYP.EQ.5) THEN
         WRITE(DESCR2,1010) IHR
 1010    FORMAT('f',I2.2)
         IF (ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).EQ.BLANK) THEN
          OPATH = DATSET(1:KDAT) // '/' // DESCR2(1:3) // '/'
         ELSEIF (ENVAR(1:4).NE.BLANK.AND.RESTHR(1:4).EQ.BLANK) THEN
          OPATH = ENVAR(1:KENV) // DATSET(1:KDAT) // '/' 
     &              // DESCR2(1:3) // '/'
         ELSEIF (ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).NE.BLANK) THEN
          OPATH = DATSET(1:KDAT) // '/' // DESCR2(1:3) // '.' //
     &              RESTHR(1:KTHR) // '/'
         ELSE
          OPATH = ENVAR(1:KENV) // DATSET(1:KDAT) // '/' 
     &              // DESCR2(1:3) // '.' // RESTHR(1:KTHR) // '/'
         ENDIF
C
         WRITE(STDOUT,*)'GRIBIT:  DIRECTORY ',OPATH,
     X        ' CREATED FOR GRIB DATA '
       ELSE
C     
C        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         IF (ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).EQ.BLANK) THEN
          WRITE(DESCR2,1011) IHR
 1011     FORMAT('.GrbF',I2.2)
          FNAME = DATSET(1:KDAT) // DESCR2
         ELSEIF(ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).NE.BLANK) THEN
          WRITE(DESCR2,1012) IHR
          FNAME = DATSET(1:KDAT) // DESCR2(1:2)  //'.'// RESTHR
         ELSE
          WRITE(DESCR2,1012) IHR
Cmp 1012     FORMAT(I2.2)
 1012     FORMAT(I3.3)
          FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:3)
     &              //'.'// RESTHR
         ENDIF
C
C        ASSIGN AND OPEN UNIT FOR GRIB DATA FILE.
         CLOSE(LUNOUT+1)
C        CALL ASNUNIT(LUNOUT+1,'-s unblocked',IER)
         CALL BAOPEN(LUNOUT+1,FNAME,IER)
         IF (IER.NE.0) WRITE(STDOUT,*)
     X        'GRIBIT:  BAOPEN ERROR FOR GRIB DATA ',
     X        'FILE.  IER=',IER
         WRITE(STDOUT,*)'GRIBIT:  OPENED ',LUNOUT+1,
     X        ' FOR GRIB DATA ',FNAME
       ENDIF
C     
C        SET OPEN-UNIT FLAGS TO FALSE.
         RITEHD = .FALSE.
         RITE2  = .FALSE.
      ENDIF
C     
C     WRITE GRIB1 MESSAGE TO OUTPUT FILE.
      CALL WRYTE(LUNOUT+1,ITOT,KBUF)
C     
C     WRITE DIAGNOSTIC MESSAGE.
C        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
C        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
C        ID(10) = VALUE 1 OF LEVEL  (0 FOR 1-100,102,103,105,107
C              111,160   LEVEL IS IN ID WORD 11)
C        ID(11) = VALUE 2 OF LEVEL
      WRITE(STDOUT,1050) ID(8),FIELD(IFLD),ID(9),ID(10),ID(11)
 1050 FORMAT('GRIBIT:  ',I3,1X,A20,1X,I3,1X,I5,1X,I5)
C     
C     END OF ROUTINE.
C     
      RETURN
      END
