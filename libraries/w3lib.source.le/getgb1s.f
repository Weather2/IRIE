C-----------------------------------------------------------------------
      SUBROUTINE GETGB1S(CBUF,NLEN,NNUM,J,JPDS,JGDS,JENS,
     &                   K,KPDS,KGDS,KENS,LSKIP,LGRIB,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB1S        FINDS A GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 95-10-31
C
C ABSTRACT: FIND A GRIB MESSAGE.
C   FIND IN THE INDEX FILE A REFERENCE TO THE GRIB MESSAGE REQUESTED.
C   THE GRIB MESSAGE REQUEST SPECIFIES THE NUMBER OF MESSAGES TO SKIP
C   AND THE UNPACKED PDS AND GDS PARAMETERS.  (A REQUESTED PARAMETER
C   OF -1 MEANS TO ALLOW ANY VALUE OF THIS PARAMETER TO BE FOUND.)
C
C PROGRAM HISTORY LOG:
C   95-10-31  IREDELL
C   98-08-28  EBISUZAKI, MINOR FIX WITH W3LIB RETURN CODES
C
C USAGE:    CALL GETGB1S(CBUF,NLEN,NNUM,J,JPDS,JGDS,JENS,
C    &                   K,KPDS,KGDS,KENS,LSKIP,LGRIB,IRET)
C   INPUT ARGUMENTS:
C     CBUF         CHARACTER*1 (NLEN*NNUM) BUFFER CONTAINING INDEX DATA
C     NLEN         INTEGER LENGTH OF EACH INDEX RECORD IN BYTES
C     NNUM         INTEGER NUMBER OF INDEX RECORDS
C     J            INTEGER NUMBER OF MESSAGES TO SKIP
C                  (=0 TO SEARCH FROM BEGINNING)
C     JPDS         INTEGER (200) PDS PARAMETERS FOR WHICH TO SEARCH
C                  (=-1 FOR WILDCARD)
C     JGDS         INTEGER (200) GDS PARAMETERS FOR WHICH TO SEARCH
C                  (ONLY SEARCHED IF JPDS(3)=255)
C                  (=-1 FOR WILDCARD)
C     JENS         INTEGER (200) ENSEMBLE PDS PARMS FOR WHICH TO SEARCH
C                  (ONLY SEARCHED IF JPDS(23)=2)
C                  (=-1 FOR WILDCARD)
C   OUTPUT ARGUMENTS:
C     K            INTEGER MESSAGE NUMBER FOUND
C                  (CAN BE SAME AS J IN CALLING PROGRAM
C                  IN ORDER TO FACILITATE MULTIPLE SEARCHES)
C     KPDS         INTEGER (200) UNPACKED PDS PARAMETERS
C     KGDS         INTEGER (200) UNPACKED GDS PARAMETERS
C     KENS         INTEGER (200) UNPACKED ENSEMBLE PDS PARMS
C     LSKIP        INTEGER NUMBER OF BYTES TO SKIP
C     LGRIB        INTEGER NUMBER OF BYTES TO READ
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    1      REQUEST NOT FOUND
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C   THIS SUBPROGRAM IS INTENDED FOR PRIVATE USE BY GETGB ROUTINES ONLY.
C
C SUBPROGRAMS CALLED:
C   GBYTE          UNPACK BYTES
C   FI632          UNPACK PDS
C   FI633          UNPACK GDS
C   PDSEUP         UNPACK PDS EXTENSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY, WORKSTATIONS
C
C$$$
      CHARACTER*1 CBUF(NLEN*NNUM)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS(200),KGDS(200),KENS(200)
      PARAMETER(LPDS=23,LGDS=22,LENS=5)     ! ACTUAL SEARCH RANGES
      CHARACTER CPDS(400)*1,CGDS(400)*1
      INTEGER KPTR(200)
      INTEGER IPDSP(LPDS),JPDSP(LPDS)
      INTEGER IGDSP(LGDS),JGDSP(LGDS)
      INTEGER IENSP(LENS),JENSP(LENS)

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPRESS REQUEST LISTS
      K=J
      LSKIP=0
      LGRIB=0
      IRET=1
C  COMPRESS PDS REQUEST
      LPDSP=0
      DO I=1,LPDS
        IF(JPDS(I).NE.-1) THEN
          LPDSP=LPDSP+1
          IPDSP(LPDSP)=I
          JPDSP(LPDSP)=JPDS(I)
        ENDIF
      ENDDO
C  COMPRESS GDS REQUEST
      LGDSP=0
      IF(JPDS(3).EQ.255) THEN
        DO I=1,LGDS
          IF(JGDS(I).NE.-1) THEN
            LGDSP=LGDSP+1
            IGDSP(LGDSP)=I
            JGDSP(LGDSP)=JGDS(I)
          ENDIF
        ENDDO
      ENDIF
C  COMPRESS ENS REQUEST
      LENSP=0
      IF(JPDS(23).EQ.2) THEN
        DO I=1,LENS
          IF(JENS(I).NE.-1) THEN
            LENSP=LENSP+1
            IENSP(LENSP)=I
            JENSP(LENSP)=JENS(I)
          ENDIF
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR REQUEST
      DOWHILE(IRET.NE.0.AND.K.LT.NNUM)
        K=K+1
        LT=0
C  SEARCH FOR PDS REQUEST
        IF(LPDSP.GT.0) THEN
c         CPDS=CHAR(0)
          do ii = 1, 400
              CPDS(ii)=CHAR(0)
          enddo
c         CPDS(1:28)=CBUF((K-1)*NLEN+26:(K-1)*NLEN+53)
          do ii = 1, 28
              CPDS(ii)=CBUF((K-1)*NLEN+25+ii)
          enddo
          NLESS=MAX(184-NLEN,0)
c         CPDS(29:40-NLESS)=CBUF((K-1)*NLEN+173:(K-1)*NLEN+184-NLESS)
          do ii = 29,40-NLESS
              CPDS(ii)=CBUF((K-1)*NLEN+173+ii-29)
          enddo
          do ii = 1, 200
              KPTR(ii)=0
          enddo
          CALL GBYTEC(CBUF,KPTR(3),(K-1)*NLEN*8+25*8,3*8)
          KPDS(18)=1
          CALL GBYTEC(CPDS,KPDS(4),7*8,8)
          CALL FI632(CPDS,KPTR,KPDS,IRET)

          DO I=1,LPDSP
            IP=IPDSP(I)
            LT=LT+ABS(JPDS(IP)-KPDS(IP))
          ENDDO
        ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  SEARCH FOR GDS REQUEST
        IF(LT.EQ.0.AND.LGDSP.GT.0) THEN
c         CGDS=CHAR(0)
          do ii = 1, 400
Cmp              CPDS(ii)=CHAR(0)
            CGDS(ii)=CHAR(0)
          enddo
c         CGDS(1:42)=CBUF((K-1)*NLEN+54:(K-1)*NLEN+95)

          do ii = 1, 42
             CGDS(ii)=CBUF((K-1)*NLEN+53+ii)
          enddo

          NLESS=MAX(320-NLEN,0)
c         CGDS(43:178-NLESS)=CBUF((K-1)*NLEN+185:(K-1)*NLEN+320-NLESS)

	  do ii = 43, 178-NLESS
             CGDS(ii)=CBUF((K-1)*NLEN+185+ii-43)
          enddo

c         KPTR=0

          do ii = 1, 200
              KPTR(ii)=0
          enddo


C ************************************************************
C
C  below line commented to make more like operational getgb1s
C
Cmp          CALL GBYTEC(CBUF,KPTR(3),(K-1)*NLEN*8+25*8,3*8)
          CALL FI633(CGDS,KPTR,KGDS,IRET)
	if (IRET .ne. 0) write(6,*) 'back from fi633 , IRET= ', IRET
          DO I=1,LGDSP
            IP=IGDSP(I)
            LT=LT+ABS(JGDS(IP)-KGDS(IP))
          ENDDO
        ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  SEARCH FOR ENS REQUEST
        IF(LT.EQ.0.AND.LENSP.GT.0) THEN
          NLESS=MAX(172-NLEN,0)
c         CPDS(41:100-NLESS)=CBUF((K-1)*NLEN+113:(K-1)*NLEN+172-NLESS)
          do ii = 41, 100-NLESS
             CPDS(ii)=CBUF((K-1)*NLEN+113+ii-41)
          enddo
          CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
          DO I=1,LENSP
            IP=IENSP(I)
            LT=LT+ABS(JENS(IP)-KENS(IP))
          ENDDO
        ENDIF
C  RETURN IF REQUEST IS FOUND
        IF(LT.EQ.0) THEN
          CALL GBYTEC(CBUF,LSKIP,(K-1)*NLEN*8,4*8)
          CALL GBYTEC(CBUF,LGRIB,(K-1)*NLEN*8+20*8,4*8)
          IF(LPDSP.EQ.0) THEN
c           CPDS=CHAR(0)
            do ii = 1, 400
                CPDS(ii)=CHAR(0)
            enddo
c           CPDS(1:28)=CBUF((K-1)*NLEN+26:(K-1)*NLEN+53)
            do ii = 1, 28
                CPDS(ii)=CBUF((K-1)*NLEN+25+ii)
            enddo
            NLESS=MAX(184-NLEN,0)
c           CPDS(29:40-NLESS)=CBUF((K-1)*NLEN+173:(K-1)*NLEN+184-NLESS)
            do ii = 29, 40-NLESS
                CPDS(ii)=CBUF((K-1)*NLEN+173+ii-29)
            enddo
c           KPTR=0
            do ii = 1, 200
                KPTR(ii)=0
            enddo
            CALL GBYTEC(CBUF,KPTR(3),(K-1)*NLEN*8+25*8,3*8)
            KPDS(18)=1
            CALL GBYTEC(CPDS,KPDS(4),7*8,8)
            CALL FI632(CPDS,KPTR,KPDS,IRET)
          ENDIF
          IF(LGDSP.EQ.0) THEN
c           CGDS=CHAR(0)
            do ii = 1, 400
Cmp                CPDS(ii)=CHAR(0)
                CGDS(ii)=CHAR(0)
            enddo
c           CGDS(1:42)=CBUF((K-1)*NLEN+54:(K-1)*NLEN+95)
            do ii = 1, 42
               CGDS(ii)=CBUF((K-1)*NLEN+53+ii)
            enddo
            NLESS=MAX(320-NLEN,0)
c           CGDS(43:178-NLESS)=CBUF((K-1)*NLEN+185:(K-1)*NLEN+320-NLESS)
            do ii = 43, 178-NLESS
                CGDS(ii)=CBUF((K-1)*NLEN+185+ii-43)
            enddo
c           KPTR=0
            do ii = 1, 200
                KPTR(ii)=0
            enddo
            CALL FI633(CGDS,KPTR,KGDS,IRET)
          ENDIF
          IF(KPDS(23).EQ.2.AND.LENSP.EQ.0) THEN
            NLESS=MAX(172-NLEN,0)
c           CPDS(41:100-NLESS)=CBUF((K-1)*NLEN+113:(K-1)*NLEN+172-NLESS)
            do ii = 41, 100-NLESS
                CPDS(ii)=CBUF((K-1)*NLEN+113+ii-41)
            enddo
            CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
          ENDIF
          IRET=0
        ELSE
          IRET=1
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
