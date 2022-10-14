C-----------------------------------------------------------------------
      SUBROUTINE POLATES0(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
     &                    NO,RLAT,RLON,IBO,LO,GO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLATES0   INTERPOLATE SCALAR FIELDS (BILINEAR)
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS BILINEAR INTERPOLATION
C           FROM ANY GRID TO ANY GRID FOR SCALAR FIELDS.
C           NO OPTIONS ARE ALLOWED.
C           ONLY HORIZONTAL INTERPOLATION IS PERFORMED.
C           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL (SPECTRAL NATIVE)
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL (ETA NATIVE)
C           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
C           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
C           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED.
C           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
C           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
C           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT.
C           INPUT BITMAPS WILL BE INTERPOLATED TO OUTPUT BITMAPS.
C           OUTPUT BITMAPS WILL ALSO BE CREATED WHEN THE OUTPUT GRID
C           EXTENDS OUTSIDE OF THE DOMAIN OF THE INPUT GRID.
C           THE OUTPUT FIELD IS SET TO 0 WHERE THE OUTPUT BITMAP IS OFF.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C
C USAGE:    CALL POLATES0(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
C    &                    NO,RLAT,RLON,IBO,LO,GO,IRET)
C
C   INPUT ARGUMENT LIST:
C     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS (NO OPTIONS)
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
C     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
C     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     IBI      - INTEGER (KM) INPUT BITMAP FLAGS
C     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF SOME IBI(K)=1)
C     GI       - REAL (MI,KM) INPUT FIELDS TO INTERPOLATE
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C
C   OUTPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
C     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
C     GO       - REAL (MO,KM) OUTPUT FIELDS INTERPOLATED
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C
C SUBPROGRAMS CALLED:
C   GDSWIZ       GRID DESCRIPTION SECTION WIZARD
C   (IJKGDS)     RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   POLFIXS      MAKE MULTIPLE POLE SCALAR VALUES CONSISTENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
CFPP$ EXPAND(IJKGDS)
      INTEGER IPOPT(20)
      INTEGER KGDSI(200),KGDSO(200)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(MI,KM),LO(MO,KM)
      REAL GI(MI,KM),GO(MO,KM)
      REAL RLAT(MO),RLON(MO)
      REAL XPTS(MO),YPTS(MO)
      REAL WO(MO)
      INTEGER N11(MO),N21(MO),N12(MO),N22(MO)
      REAL W11(MO),W21(MO),W12(MO),W22(MO)
      PARAMETER(FILL=-9999.)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
      IRET=0
      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,0,DUM,DUM)
        IF(NO.EQ.0) IRET=3
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS
      CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
      DO N=1,NO
        XI=XPTS(N)
        YI=YPTS(N)
        IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
          I1=XI
          I2=I1+1
          J1=YI
          J2=J1+1
          XF=XI-I1
          YF=YI-J1
          N11(N)=IJKGDS(I1,J1,KGDSI)
          N21(N)=IJKGDS(I2,J1,KGDSI)
          N12(N)=IJKGDS(I1,J2,KGDSI)
          N22(N)=IJKGDS(I2,J2,KGDSI)
          IF(MIN(N11(N),N21(N),N12(N),N22(N)).GT.0) THEN
            W11(N)=(1-XF)*(1-YF)
            W21(N)=XF*(1-YF)
            W12(N)=(1-XF)*YF
            W22(N)=XF*YF
          ELSE
            N11(N)=0
            N21(N)=0
            N12(N)=0
            N22(N)=0
          ENDIF
        ELSE
          N11(N)=0
          N21(N)=0
          N12(N)=0
          N22(N)=0
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE WITH OR WITHOUT BITMAPS
CMIC$ DO ALL AUTOSCOPE PRIVATE(WO)
      DO K=1,KM
        DO N=1,NO
          GO(N,K)=0.
          WO(N)=0.
          IF(N11(N).GT.0) THEN
            IF(IBI(K).EQ.0) THEN
              GO(N,K)=W11(N)*GI(N11(N),K)+W21(N)*GI(N21(N),K)
     &               +W12(N)*GI(N12(N),K)+W22(N)*GI(N22(N),K)
              WO(N)=1
            ELSE
              IF(LI(N11(N),K)) THEN
                GO(N,K)=GO(N,K)+W11(N)*GI(N11(N),K)
                WO(N)=WO(N)+W11(N)
              ENDIF
              IF(LI(N21(N),K)) THEN
                GO(N,K)=GO(N,K)+W21(N)*GI(N21(N),K)
                WO(N)=WO(N)+W21(N)
              ENDIF
              IF(LI(N12(N),K)) THEN
                GO(N,K)=GO(N,K)+W12(N)*GI(N12(N),K)
                WO(N)=WO(N)+W12(N)
              ENDIF
              IF(LI(N22(N),K)) THEN
                GO(N,K)=GO(N,K)+W22(N)*GI(N22(N),K)
                WO(N)=WO(N)+W22(N)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IBO(K)=IBI(K)
        DO N=1,NO
          LO(N,K)=WO(N).GE.0.5
          IF(LO(N,K)) THEN
            GO(N,K)=GO(N,K)/WO(N)
          ELSE
            IBO(K)=1
            GO(N,K)=0.
          ENDIF
        ENDDO
      ENDDO
      IF(KGDSO(1).EQ.0) CALL POLFIXS(NO,MO,KM,RLAT,RLON,IBO,LO,GO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
