C
C
C
      MODULE HMPLTGEN
C
C     + + + PURPOSE + + +
C     module containing utility module PLTGEN
C
C     + + + HISTORY + + +
C     2009/04/28 JLK - initial implementation of module version  
C
C     + + + MODULE VARIABLES + + +
      IMPLICIT NONE
C      
      TYPE HTPLTGEN
        SEQUENCE
        INTEGER PLOTFL,NPT,NMN,LABLFG,PYREND,NCURV,TYPEFG,MESSU,
     $          MSGFL,PLTNO,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,
     $          SPNUND,DUMA(12),UUNITS,TITLE(10),YLABL(5)
        REAL    YMIN,YMAX,IVLIN,THRESH
        INTEGER CUVDAT(9,99)
        REAL    DELT
        INTEGER NDAY(12),PIVL
        REAL    VAL(99)
        INTEGER YR,MON,DAY,HR,
     $          MIN,IVL,IVL1,NDAYS,PIVLNO,STFG,PLTFLG,PTFP(20),MNFP(99),
     $          PIVLTM,CURREC,DUMPLT
C
      END TYPE HTPLTGEN
C
      CONTAINS   
C
C
C
      SUBROUTINE   PPLTGN
     I                    (NDELT,SDATIM,EDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     process the input for the pltgen module
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C     2009/04/28 JLK - initial implementation of module version 
C
C     + + + MODULES + + +
      USE       HIOOSV 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),EDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'crin2.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTPLTGEN)  HPLTGEN
      POINTER   (P,HPLTGEN)
      POINTER   (PDATIM,DATIM),(POSV,OSV),(PCODE,CODE),(PPLTINF,PLTINF),
     $          (PGLABL,GLABL),(PSCALE,SCALE)
      INTEGER       PLTINF(7),CODE(1),GLABL(15),DATIM(5),OSV(500)
      REAL          SCALE(4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      OSVKST,OSVKND,OSVREC,I,I1,I2,I4,I6,N,
     #             TRNCOD,PLVX,SCLU,SGRP,INITFG,CONT,CLEN
      CHARACTER*4  KWDLIB(6),BLANK
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(KWDLIB,KWDLB1)
      CHARACTER*1  CHSTR1(80),KWDLB1(24)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,ITABLE,HSCKFL,RTABLE,CHKSTR
      EXTERNAL  OMSG,OMSTI,OMSTC,WMSGTT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (6(A4,1X))
 1010 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING PLTGEN OPERATION NO. ',I4)
 2030 FORMAT (/,' FINISHED PROCESSING PLTGEN OPERATION NO. ',I4,
     $        /,' ',132('+'))
 2070 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      BLANK= '    '
      SCLU = 371
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVREC= 3
      OSVKND= OSVKEY+ OSVREC
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
      POSV   = LOC(HPLTGEN)
      PPLTINF= LOC(HPLTGEN%PLOTFL)
      PGLABL = LOC(HPLTGEN%TITLE)
      PCODE  = LOC(HPLTGEN%TITLE)  
      PDATIM = LOC(HPLTGEN%YR)  
      PSCALE = LOC(HPLTGEN%YMIN)
      POSV   = LOC(HPLTGEN)
C     record the keys in opntab
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area used
        DO 10 I= 1,OSVREC
          OSV= -999
 10     CONTINUE
      END IF
C
      HPLTGEN%PLTNO = OPTNO
      HPLTGEN%MESSU = FILE(1)
      HPLTGEN%MSGFL = FILE(15)
      IF (OUTLEV.GT.0) THEN
        WRITE (HPLTGEN%MESSU,2000) OPTNO
      END IF
C
      HPLTGEN%DELT= NDELT
      DO 20 I= 1,5
        DATIM(I)= SDATIM(I)
 20   CONTINUE
      DO 30 I= 1,12
        HPLTGEN%NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      HPLTGEN%STFG  = 1
      HPLTGEN%SPIVL=  0
      HPLTGEN%SPOPNO= OPNO
      HPLTGEN%UUNITS= EMFG
      HPLTGEN%PLTFLG= 1
C     following values are assigned so that subroutine addtim will
C     work ok
      HPLTGEN%PIVL  = 1
      HPLTGEN%PIVLNO= 0
      HPLTGEN%NDAYS = DAYMNH(HPLTGEN%YR,HPLTGEN%MON,HPLTGEN%NDAY)
C
C     get info on time series to be plotted -- table-type plotinfo
      I4= 7
      CALL ITABLE
     I             (I1,I1,I4,I1,
     M              PLTINF)
C
C     check output file - if not open, then open it with a standard name
      IF (HPLTGEN%PLOTFL .GT. 0) THEN
C       check that file is open
C
        IF (HPLTGEN%TYPEFG .EQ. 1) THEN
C         indicate that this is a pltgen file for vax special case
          PLVX= HPLTGEN%PLOTFL+ 9999
        ELSE 
C         indicate that this is a dtsf or ptsf file for feq
          PLVX= HPLTGEN%PLOTFL- 9999
        END IF
        CALL HSCKFL
     I              (PLVX)
      END IF
C
C     check that total no. of curves to be plotted is within valid range
C     we need pivl for write interval,
C     but it is read in table plotinfo for user case
      HPLTGEN%PIVL= HPLTGEN%NCURV
      IF (HPLTGEN%PIVL .EQ. 0) THEN
C       zero input - default to one
        HPLTGEN%PIVL= 1
      END IF
      HPLTGEN%NCURV= HPLTGEN%NPT+ HPLTGEN%NMN
C
      IF (HPLTGEN%TYPEFG .EQ. 1) THEN
C       original format
        IF ( (HPLTGEN%NCURV .LT. 1) .OR. (HPLTGEN%NCURV .GT. 20) ) THEN
C         error - bad total number of curves
          CALL OMSTI (HPLTGEN%NCURV)
          SGRP= 31
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (HPLTGEN%TYPEFG .EQ. 2 .OR. HPLTGEN%TYPEFG .EQ. 4) THEN
C       dtsf format
        IF (HPLTGEN%NPT .GT. 0) THEN
C         error - dtsf is mean-valued only
          CALL OMSTI (HPLTGEN%NPT)
          SGRP= 34
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (HPLTGEN%TYPEFG .EQ. 3) THEN
C       ptsf format
        IF (HPLTGEN%NMN .GT. 0) THEN
C         error - ptsf is point valued only
          CALL OMSTI (HPLTGEN%NMN)
          SGRP= 35
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
        IF (HPLTGEN%NPT .GT. 1) THEN
C         error - ptsf is single valued only
          CALL OMSTI (HPLTGEN%NPT)
          SGRP= 36
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     read transformation keyword library
      SGRP  = 32
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (HPLTGEN%MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
C
      READ (CHSTR,1000) KWDLIB
C
      IF (HPLTGEN%LABLFG .NE.(-1)) THEN
C       plot label is required
C       get general labels for plot -- table-type gen-labels
        I2= 2
        I4= 15
        CALL ITABLE
     I              (I2,I1,I4,I1,
     M               GLABL)
      ELSE
C       general label fields are blank
        DO 40 I= 1,15
          READ (BLANK,2070) GLABL(I)
 40     CONTINUE
      END IF
C
C     get scale info -- table-type scaling
      I2= 3
      I4= 4
      CALL RTABLE
     I             (I2,I1,I4,I1,
     M              SCALE)
C
C     get info for each individual curve on plot -- table-type curv-data
      DO 50 N= 1, HPLTGEN%NCURV
        I2=4
        I4=8
        CALL ITABLE
     I               (I2,N,I4,I1,
     M                HPLTGEN%CUVDAT(1,N))
C       check transformation keyword
        WRITE (CHSTR(1:4),2070) HPLTGEN%CUVDAT(8,N)
        I4= 4
        I6= 6
        TRNCOD= CHKSTR (I4,I6,CHSTR1,KWDLB1)
        IF (TRNCOD.EQ.0) THEN
C         invalid keyword
          WRITE (CHSTR,2070) HPLTGEN%CUVDAT(8,N)
          I4= 4
          CALL OMSTC (I4,CHSTR1)
          SGRP= 33
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         check for default
          IF (TRNCOD.EQ.6) THEN
            TRNCOD     = 1
            READ (KWDLIB(1),1010) HPLTGEN%CUVDAT(8,N)
          END IF
          HPLTGEN%CUVDAT(9,N)= TRNCOD
        END IF
 50   CONTINUE
C
      IF (HPLTGEN%TYPEFG .EQ. 2 .OR. HPLTGEN%TYPEFG .EQ. 4) THEN
C       write header for feq dtsf format
        CALL HEDTSF (HPLTGEN%PLOTFL,NDELT,NDAMON,GLABL,
     I               HPLTGEN%NMN,HPLTGEN%CUVDAT,
     I               SDATIM,EDATIM,HPLTGEN%TYPEFG,
     O               HPLTGEN%CURREC)
      END IF
C
      IF (OUTLEV.GT.0) THEN
        WRITE (HPLTGEN%MESSU,2030) OPTNO
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE HEDTSF
     I                    (PLOTFL,NDELT,NDAMON,GLABL,NMN,CUVDAT,
     I                     SDATIM,EDATIM,TYPEFG,
     O                     CURREC)
C
C     + + + PURPOSE + + +
C     Write header for FEQ DTSF file (TYPEFG=2 or 4).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER PLOTFL,NDELT,NDAMON(12),GLABL(15),NMN,CUVDAT(9,NMN),
     $        SDATIM(5),EDATIM(5),TYPEFG,CURREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLOTFL - fortran unit number of pltgen output file
C     NDELT  - number of minutes of run interval
C     NDAMON - no. of days in each month of calendar year
C     GLABL  - character label for entire file
C     NMN    - number of (mean-valued) time series
C     CUVDAT - character label for each time series
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TYPEFG - orginal (TYPEFG=2) or revised (TYPEFG=4)
C     CURREC - current record number on DTSF file (TYPEFG=4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,N,I0,I1,I5,I12,LEN,LLEN,IREC(106),YR,MO,DY,HR,MI,SC,
     $        NHEAD,CTIME,NULSDT(5),NULEDT(5),NULLDT(5),DUM1,DUM2,DUM3,
     $        DUM4,DUM5,DUM6,DUM7,DUM8,NSTEP,XDAT(6)
      DOUBLE PRECISION SJTIME,EJTIME,DYFRAC,NJTIME
      CHARACTER*64 LFILE
      LOGICAL      LOPEN
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPI,SYDATM,EXDATE,TIMCVT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I12/0,1,5,12/
      DATA NULSDT,NULEDT/1859,1,1,0,0,1859,1,3,0,0/
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize record
      LEN= NMN+ 7
      CALL ZIPI (LEN,I0,
     O           IREC)
C
      IF (TYPEFG.EQ.4) THEN
C       revised format, use direct acess file
        INQUIRE (PLOTFL,OPENED=LOPEN,NAME=LFILE)
       write(99,*) "PLTGEN: unit # ",PLOTFL," is named ",LFILE
        IF (LOPEN .EQV. .TRUE.) THEN
C         close file before opening
          CLOSE (PLOTFL)
        END IF
C       determine record length and write it on 1st record
        LLEN= 4*LEN
       write(99,*) "PLTGEN: About to open ",LFILE,
     $             " with length ",LLEN
        OPEN (PLOTFL,FILE=LFILE,STATUS="REPLACE",
     $        ACCESS="DIRECT",RECL=LLEN)
        WRITE (PLOTFL,REC=1) LLEN,(IREC(I),I=2,LEN)
       write(99,*) "PLTGEN: wrote LLEN and Dummy 0s, like this:"
       write(99,*) "PLTGEN: ",LLEN,(IREC(I),I=1,4)
      END IF
C
C     write first record
      CALL SYDATM
     O            (YR,MO,DY,HR,MI,SC)
      IF (YR .LT. 90) THEN
C       assume file created after 1999
        YR= YR+ 2000
      ELSE
C       assume file created before 2000
        YR= YR+ 1900
      END IF
      CTIME= HR*100+ MI
      NHEAD= NMN+ 3
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) YR,MO,DY,CTIME,NHEAD,NMN,I1,(IREC(I),I=8,LEN)
      ELSE
        WRITE (PLOTFL,REC=2) YR,MO,DY,CTIME,NHEAD,NMN,I1,
     $                      (IREC(I),I=8,LEN)
      END IF
C
C     write second record
      CALL EXDATE (SDATIM,
     O             XDAT)
      XDAT(6)= 0
      CALL TIMCVT
     M            (XDAT)
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) (XDAT(I),I=1,3),(EDATIM(I),I= 1,3),
     $                 (IREC(I),I=7,LEN)
      ELSE
        WRITE (PLOTFL,REC=3) (XDAT(I),I=1,3),(EDATIM(I),I= 1,3),
     $                       (IREC(I),I=7,LEN)
      END IF
C
C     write tsf label record
      IF (LEN .LT. 10) THEN
C       only write part of label
        LLEN= LEN
      ELSE
C       write whole label
        LLEN= 10
      END IF
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) (GLABL(I),I=1,LLEN),(IREC(I),I=11,LEN)
      ELSE
        WRITE (PLOTFL,REC=4) (GLABL(I),I=1,LLEN),(IREC(I),I=11,LEN)
      END IF
C
C     write dataset header records
      LLEN= 4
      DO 10 N= 1, NMN
        IF (TYPEFG.EQ.2) THEN
          WRITE (PLOTFL) (CUVDAT(I,N),I=1,LLEN),(IREC(I),I=5,LEN)
        ELSE
          WRITE (PLOTFL,REC=4+N) (CUVDAT(I,N),I=1,LLEN),
     $                           (IREC(I),I=5,LEN)
        END IF
 10   CONTINUE
C
C     write null event
C
C     write header line for null event
      CALL MJDATE (NULEDT,
     O             EJTIME,DYFRAC)
      CALL MJDATE (NULSDT,
     O             SJTIME,DYFRAC)
      IF (TYPEFG .EQ. 2) THEN
        WRITE (PLOTFL) -SJTIME,DYFRAC,(NULSDT(I),I=1,3),EJTIME,
     $                 (IREC(I),I=10,LEN)
      ELSE
        CURREC = NMN + 5
        WRITE (PLOTFL,REC=CURREC) -SJTIME,DYFRAC,(NULSDT(I),I=1,3),
     $                            EJTIME,(IREC(I),I=10,LEN)
      END IF
C
C     write two days of zero flows for null event
      NSTEP= 2880/NDELT
      DUM1= 0
      CALL COPYI (I5,NULSDT,
     O            NULLDT)
C     convert to internal format
      CALL HDATIN
     M            (NULLDT(1),NULLDT(2),NULLDT(3),NULLDT(4),NULLDT(5))
      DO 20 N= 1, NSTEP
        CALL ADDTIM (NDELT,NDAMON,I1,I12,
     M               NULLDT,DUM1,
     O               DUM2,DUM3,DUM4,DUM5,DUM6,DUM7,DUM8)
        CALL EXDATE (NULLDT,
     O               XDAT)
        CALL MJDATE (XDAT,
     O               NJTIME,DYFRAC)
        XDAT(6)= 0
        CALL TIMCVT
     M              (XDAT)
        IF (TYPEFG .EQ. 2) THEN
          WRITE (PLOTFL) NJTIME,DYFRAC,(XDAT(I),I=1,3),(IREC(I),I=8,LEN)
        ELSE
          WRITE (PLOTFL,REC=CURREC+N) NJTIME,DYFRAC,(XDAT(I),I=1,3),
     $                                (IREC(I),I=8,LEN)
        END IF
 20   CONTINUE
C
C     write header line for main event
      CALL EXDATE (EDATIM,
     O             XDAT)
      CALL MJDATE (XDAT,
     O             EJTIME,DYFRAC)
      CALL EXDATE (SDATIM,
     O             XDAT)
      CALL MJDATE (XDAT,
     O             SJTIME,DYFRAC)
      XDAT(6)= 0
      CALL TIMCVT
     M            (XDAT)
      IF (TYPEFG .EQ. 2) THEN
        WRITE (PLOTFL) -SJTIME,DYFRAC,(XDAT(I),I=1,3),EJTIME,
     $                 (IREC(I), I= 10,LEN)
      ELSE
        CURREC = CURREC + NSTEP + 1
        WRITE (PLOTFL,REC=CURREC) -SJTIME,DYFRAC,(XDAT(I),I=1,3),EJTIME,
     $                            (IREC(I), I= 10,LEN)
        CURREC = CURREC + 1
      END IF
C
      RETURN
      END SUBROUTINE
C
C     4.2(12)
C
      SUBROUTINE   PLTGEN
     I                    (STIVL,WIDTH,LSTCAL,OSVKST)
C
C     + + + PURPOSE + + +
C     Prepare a set of point- and/or mean-valued time series for
C     display on an incremental plotter
C
C     + + + HISTORY + + +
C     2009/04/28 JLK - initial implementation of module version  
C
C     + + + MODULES + + +
      USE       HIOOSV 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,LSTCAL,OSVKST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     LSTCAL - flag indicating last interval of run
C     OSVKST - starting block in osv
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cmpad.inc'
      INCLUDE   'cmosv.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTPLTGEN)  HPLTGEN
      POINTER   (P,HPLTGEN)
      POINTER   (PDATIM,DATIM),(PCODE,CODE),(PPLTINF,PLTINF),
     $          (PGLABL,GLABL),(PSCALE,SCALE)
C     EQUIVALENCE   (ASVEC,PLOTFL),(PLTINF,PLOTFL),(CODE,TITLE(1)),
C    $              (GLABL,TITLE(1)),(SCALE,YMIN),(DATIM,YR)
      INTEGER       PLTINF(7),CODE(1),GLABL(15),DATIM(5) 
      REAL          SCALE(4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT,N,EXDAT(6),NC,DUM1,DUM2,DUM3,DUM4,EMONFG,EPYRFG,
     $            TRNCOD,PDELT,PLTNOW,I,NBLNK,ERRFLG,SCLU,SGRP,
     $            NULSDT(5),NULEDT(5)
      REAL        VALNOW
      DOUBLE PRECISION JTIME,DYFRAC
      CHARACTER*6  OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    EXDATE,PLTAGG,SPECL,ADDTIM,TRANS,UPQUAN,OMSTI,OMSG,
     $            MJDATE,ZIPR,TIMCVT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'PLTGEN'/
      DATA        NULSDT,NULEDT/1859,1,1,0,0,1859,1,3,0,0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,1X,'HSPF FILE FOR DRIVING SEPARATE PLOT PROGRAM',/,
     $        A4,1X,'Time interval:',I5,' mins',10X,
     $        'Last month in printout year:',I3,/,
     $        A4,1X,'No. of curves plotted:  Point-valued:',I3,
     $        '   Mean-valued:',I3,'   Total',I3)
 2010 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',I5,10X,'Idelt:',I5)
 2020 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',5X,'Month',5X,'Idelt:',
     $        I5)
 2030 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',5X,'Year ',5X,'Idelt:',
     $        I5)
 2040 FORMAT (A4,1X,'Plot title:   ',10A4,/,
     $        A4,1X,'Y-axis label: ',5A4,/,
     $        A4,1X,'Scale info:  Ymin: ',G12.5,10X,' Threshold:',G12.5,
     $        /,A4,1X,'             Ymax: ',G12.5,/,
     $        A4,1X,'             Time: ',G12.5,' intervals/inch')
 2050 FORMAT (A4,1X,'Data for each curve (Point-valued first,',
     $        ' then mean-valued):',/,
     $        A4,1X,'Label                   LINTYP     INTEQ',
     $        '    COLCOD      TRAN   TRANCOD')
 2060 FORMAT (A4,1X,4A4,4X,3I10,6X,A4,I10)
 2070 FORMAT (A4)
 2080 FORMAT (A4,1X,'Time series (pt-valued, then mean-valued):',/,
     $        A4,/,
     $        A4,1X,'Date/time                      Values',/,
     $        A4)
 2090 FORMAT (A4,1X,I5,4I3,20(1PG14.7))
C
C     + + + END SPECIFICATIONS + + +
C
C     Record length of plotfiles
C
C       #curves     lrecl
C          1          80 (36)
C          2          80 (50)
C          3          80 (64)
C          4          80 (78)
C          5          92
C          6         106
C          7         120
C          8         134
C          9         148
C         10         162
C         11         176
C         12         190
C         13         204
C         14         218
C         15         232
C         16         246
C         17         260
C         18         274
C         19         288
C         20         302
C
C
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
      PPLTINF= LOC(HPLTGEN%PLOTFL)
      PGLABL = LOC(HPLTGEN%TITLE)
      PCODE  = LOC(HPLTGEN%TITLE)  
      PDATIM = LOC(HPLTGEN%YR)  
      PSCALE = LOC(HPLTGEN%YMIN)
C
      HPLTGEN%IVL  =   STIVL- 1
      HPLTGEN%IVL1 =  STIVL
      IDELT= HPLTGEN%DELT
C
      IF (HPLTGEN%STFG .EQ. 1) THEN
C       start of run
        HPLTGEN%STFG  = 0
        ERRFLG= 0
        SCLU  = 370
C
C       check for missing input time series
        DO 10 N= 1, HPLTGEN%NPT
          IF (HPLTGEN%PTFP(N) .LT. 1) THEN
C           error - missing an input point time series
            CALL OMSTI (HPLTGEN%PLTNO)
            CALL OMSTI (HPLTGEN%NPT)
            CALL OMSTI (N)
            SGRP= 11
            CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M                 ERRFLG)
          END IF
 10     CONTINUE
        DO 20 N= 1, HPLTGEN%NMN
          IF (HPLTGEN%MNFP(N) .LT. 1) THEN
C           error - missing an input mean time series
            CALL OMSTI (HPLTGEN%PLTNO)
            CALL OMSTI (HPLTGEN%NMN)
            CALL OMSTI (N)
            SGRP= 12
            CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M                 ERRFLG)
          END IF
 20     CONTINUE
        IF (ERRFLG .GT. 0) THEN
C         error - at least one missing time series: stop run
          I= 0
          CALL OMSTI (HPLTGEN%PLTNO)
          SGRP= 13
          CALL OMSG (HPLTGEN%MESSU,HPLTGEN%MSGFL,SCLU,SGRP,
     M               I)
        END IF
C
C       write file header
C
        IF (HPLTGEN%TYPEFG .EQ. 1) THEN
C         formal pltgen format
C         write labeling and scaling info to the plot file plotfl
C         exact format is given on the printout design sheet
C
          PDELT= IDELT*  HPLTGEN%PIVL
          IF (HPLTGEN%PIVL .LT. 0) THEN
C           monthly or annual plotfile
            PDELT= HPLTGEN%PIVL
          END IF
          WRITE (HPLTGEN%PLOTFL,2000) 
     1           CODE,CODE,PDELT,HPLTGEN%PYREND,
     1           CODE,HPLTGEN%NPT,HPLTGEN%NMN,HPLTGEN%NCURV
C
          IF ( HPLTGEN%PIVL .GT. 0) THEN
C           normal plot file
            WRITE (HPLTGEN%PLOTFL,2010) 
     1             CODE,HPLTGEN%LABLFG,HPLTGEN%PIVL,IDELT
          ELSE IF ( HPLTGEN%PIVL .EQ. -1) THEN
C           monthly plot file
            WRITE ( HPLTGEN%PLOTFL,2020)  CODE,HPLTGEN%LABLFG,IDELT
          ELSE IF ( HPLTGEN%PIVL .EQ. -2) THEN
C           annual plot file
            WRITE ( HPLTGEN%PLOTFL,2030)  CODE,HPLTGEN%LABLFG,IDELT
          END IF
C
          WRITE (HPLTGEN%PLOTFL,2040) 
     1                        CODE,HPLTGEN%TITLE,
     1                        CODE,HPLTGEN%YLABL,
     1                        CODE,HPLTGEN%YMIN,HPLTGEN%THRESH,
     $                        CODE,HPLTGEN%YMAX,
     1                        CODE,HPLTGEN%IVLIN
C
          WRITE (HPLTGEN%PLOTFL,2050) CODE,CODE
          DO 30 N= 1,  HPLTGEN%NCURV
            WRITE (HPLTGEN%PLOTFL,2060) 
     1                        CODE,(HPLTGEN%CUVDAT(I,N),I= 1, 9)
 30       CONTINUE
C
          IF (HPLTGEN%NCURV .LT. 10) THEN
            NBLNK= 10- HPLTGEN%NCURV
            DO 40 N= 1,NBLNK
              WRITE (HPLTGEN%PLOTFL,2070) CODE
 40         CONTINUE
          END IF
C
C         write heading that goes immed above time series data
          WRITE (HPLTGEN%PLOTFL,2080) CODE,CODE,CODE,CODE
C
C         handle the initial points
C
C         get date/time to external format
          CALL EXDATE (DATIM,
     O                 EXDAT)
C
C         assemble the values to be written in the first record
          NC= 0
          IF (HPLTGEN%NPT .GT. 0) THEN
            DO 50 N= 1, HPLTGEN%NPT
              NC= NC+ 1
              HPLTGEN%VAL(NC)= PAD(HPLTGEN%PTFP(N)+ 1)
 50         CONTINUE
          END IF
C
C         initial value for a mean-valued time series is meaningless
          IF (HPLTGEN%NMN .GT. 0) THEN
            DO 60 N= 1, HPLTGEN%NMN
              NC= NC+ 1
              HPLTGEN%VAL(NC)= -1.0E30
 60         CONTINUE
          END IF
C
          IF (HPLTGEN%SPAFP .GT. 0) THEN
C           no initial values when special actions are being used
C           to turn on plotfile
            HPLTGEN%PLTFLG= 0
          END IF
          IF (HPLTGEN%PLTFLG .NE. 0) THEN
C           write initial values
            WRITE (HPLTGEN%PLOTFL,2090) 
     1                CODE,(EXDAT(N),N=1,5),(HPLTGEN%VAL(N),N=1,NC)
          END IF
        ELSE IF (HPLTGEN%TYPEFG .EQ. 2 .OR. HPLTGEN%TYPEFG .EQ. 4) THEN
C         header for feq dtsf format written by ppltgn
C
C         set number of curves
          NC= HPLTGEN%NMN
        ELSE IF (HPLTGEN%TYPEFG .EQ. 3) THEN
C         write header for feq ptsf format
C
C         write code indicating flow time series
          WRITE (HPLTGEN%PLOTFL) 4
C
C         write null event
          VALNOW= 0.0
          CALL MJDATE (NULSDT,
     O                 JTIME,DYFRAC)
          WRITE (HPLTGEN%PLOTFL) JTIME,VALNOW
          CALL MJDATE (NULEDT,
     O                 JTIME,DYFRAC)
          WRITE (HPLTGEN%PLOTFL) JTIME,VALNOW
C
C         write initial value
          HPLTGEN%VAL(1)= PAD(HPLTGEN%PTFP(1)+ 1)
          CALL EXDATE (DATIM,
     O                 EXDAT)          
          CALL MJDATE (EXDAT,
     O                 JTIME,DYFRAC)
          WRITE (HPLTGEN%PLOTFL) JTIME,HPLTGEN%VAL(1)
C
C         set number of curves
          NC= 1
        END IF
C
C       reset val
        DO 110 N= 1, NC
          TRNCOD= HPLTGEN%CUVDAT(9,N)
C         caseentry trncod
          GO TO (70,70,80,90,70),TRNCOD
C         case 1,2 and 5 sum,aver,last
 70       CONTINUE
            HPLTGEN%VAL(N)= 0.0
            GO TO 100
C         case 3 max
 80       CONTINUE
            HPLTGEN%VAL(N)= -1.0E30
            GO TO 100
C         case 4 min
 90       CONTINUE
            HPLTGEN%VAL(N)= 1.0E30
C         endcase
 100      CONTINUE
C       end loop
 110    CONTINUE
C
        IF (HPLTGEN%PIVL .LE. 0) THEN
C         determine how many intervals this month or year
          CALL PLTAGG (DATIM,HPLTGEN%NDAY,HPLTGEN%PYREND,
     I                 HPLTGEN%PIVL,IDELT,
     O                 HPLTGEN%PIVLTM)
        ELSE
C         number of intervals input by user
          HPLTGEN%PIVLTM= HPLTGEN%PIVL
        END IF
      END IF
C
C     time loop
      DO 200 I= STIVL, WIDTH+ STIVL- 1
        HPLTGEN%IVL = I
        HPLTGEN%IVL1 = HPLTGEN%IVL1+ 1
        HPLTGEN%SPIVL= HPLTGEN%SPIVL+ 1
C
        IF ( (HPLTGEN%SPAFP .GT. 0) .AND. 
     1       (HPLTGEN%SPAFP .LE. HPLTGEN%SPAKND) ) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,HPLTGEN%PLTNO,HPLTGEN%SPAKND,
     I                HPLTGEN%SPOPNO,DATIM,HPLTGEN%MESSU,HPLTGEN%SPIVL,
     I                HPLTGEN%SPOUT,HPLTGEN%SPNUND,
     M                HPLTGEN%SPAFP)
        END IF
C
C       increment date/time
        CALL ADDTIM (IDELT,HPLTGEN%NDAY,HPLTGEN%PIVLTM,HPLTGEN%PYREND,
     M               DATIM,HPLTGEN%PIVLNO,
     O               HPLTGEN%NDAYS,DUM1,DUM2,DUM3,DUM4,EMONFG,EPYRFG)
C
        IF (HPLTGEN%TYPEFG .GE. 2) THEN
C         dtsf or ptsf - need modified julian date
          CALL EXDATE (DATIM,
     O                 EXDAT)
          CALL MJDATE (EXDAT,
     O                 JTIME,DYFRAC)
        END IF
C
C       assemble the values to be written to a single record
        NC= 0
        IF (HPLTGEN%NPT .GT. 0) THEN
          DO 120 N= 1, HPLTGEN%NPT
            NC= NC+ 1
            VALNOW= PAD(HPLTGEN%PTFP(N)+ HPLTGEN%IVL1)
            CALL TRANS (HPLTGEN%CUVDAT(9,NC),HPLTGEN%PIVLTM,VALNOW,
     M                  HPLTGEN%VAL(NC))
 120      CONTINUE
        END IF
C
        IF (HPLTGEN%NMN .GT. 0) THEN
          DO 130 N= 1,HPLTGEN% NMN
            NC= NC+ 1
            VALNOW= PAD(HPLTGEN%MNFP(N)+ HPLTGEN%IVL1)
            CALL TRANS (HPLTGEN%CUVDAT(9,NC),HPLTGEN%PIVLTM,VALNOW,
     M                  HPLTGEN%VAL(NC))
 130      CONTINUE
        END IF
C
        IF (HPLTGEN%PIVLNO .EQ. HPLTGEN%PIVLTM) THEN
C         time to write a line
C
C         get date/time to external format
          CALL EXDATE (DATIM,
     O                 EXDAT)
C
          PLTNOW= 0
          IF (HPLTGEN%PLTFLG .EQ. 1) THEN
C           check values against threshold
            DO 140 N= 1, NC
              IF (HPLTGEN%VAL(N) .GT. HPLTGEN%THRESH) THEN
C               this value above threshold
                PLTNOW= 1
              END IF
 140        CONTINUE
          END IF
C
          IF (PLTNOW .EQ. 1) THEN
C           write a line this interval
            IF (HPLTGEN%TYPEFG .EQ. 1) THEN
C             formatted pltgen file
              WRITE (HPLTGEN%PLOTFL,2090) 
     1              CODE,(EXDAT(N),N=1,5),(HPLTGEN%VAL(N),N=1,NC)
            ELSE IF (HPLTGEN%TYPEFG .EQ. 2 .OR.
     1               HPLTGEN%TYPEFG .EQ. 4) THEN
C             dtsf format
              EXDAT(6)= 0
              CALL TIMCVT
     M                    (EXDAT)
              IF (HPLTGEN%TYPEFG .EQ. 2) THEN
                WRITE (HPLTGEN%PLOTFL)  JTIME,DYFRAC,(EXDAT(N),N=1,3),
     $                                 (HPLTGEN%VAL(N), N= 1, NC)
              ELSE
                WRITE (HPLTGEN%PLOTFL,REC=HPLTGEN%CURREC) 
     1                                  JTIME,DYFRAC,(EXDAT(N),N=1,3),
     $                                 (HPLTGEN%VAL(N), N= 1, NC)
            write(99,*) "PLTGEN: REC=",HPLTGEN%CURREC,
     1                  " JTIME,DYFRAC,EXDAT,VALS"
            write(99,*) JTIME,DYFRAC,(EXDAT(N),N=1,3),
     1                 (HPLTGEN%VAL(N),N=1,NC)
                HPLTGEN%CURREC= HPLTGEN%CURREC + 1
              END IF
            ELSE IF (HPLTGEN%TYPEFG .EQ. 3) THEN
C             ptsf format
              WRITE (HPLTGEN%PLOTFL)  JTIME,HPLTGEN%VAL(1)
            END IF
          END IF
C
C         reset val
          DO 190 N= 1, NC
            TRNCOD= HPLTGEN%CUVDAT(9,N)
C           caseentry trncod
            GO TO (150,150,160,170,150),TRNCOD
C           case 1,2 and 5 sum,aver,last
 150        CONTINUE
              HPLTGEN%VAL(N)= 0.0
              GO TO 180
C           case 3 max
 160        CONTINUE
              HPLTGEN%VAL(N)= -1.0E30
              GO TO 180
C           case 4 min
 170        CONTINUE
              HPLTGEN%VAL(N)= 1.0E30
C           endcase
 180        CONTINUE
C         end loop
 190      CONTINUE
C
          IF (HPLTGEN%PIVL .LE. 0) THEN
C           determine how many intervals this month or year
            CALL PLTAGG (DATIM,HPLTGEN%NDAY,HPLTGEN%PYREND,
     I                   HPLTGEN%PIVL,IDELT,
     O                   HPLTGEN%PIVLTM)
            HPLTGEN%PIVLNO= 0
          END IF
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (HPLTGEN%SPIVL,HPLTGEN%SPOPNO)
C
 200  CONTINUE
C
      IF (LSTCAL .EQ. 1) THEN
C       last interval of run
        IF (HPLTGEN%TYPEFG .EQ. 2 .OR. HPLTGEN%TYPEFG .EQ. 4) THEN
C         feq dtsf file - write last record of all zeroes
          VALNOW= 0.0
          N     = NC+ 7
          CALL ZIPR (N,VALNOW,
     O               HPLTGEN%VAL)
          IF (HPLTGEN%TYPEFG .EQ. 2) THEN
            WRITE (HPLTGEN%PLOTFL) (HPLTGEN%VAL(N),N=1,NC+7)
          ELSE
            WRITE (HPLTGEN%PLOTFL,REC=HPLTGEN%CURREC) 
     1            (HPLTGEN%VAL(N),N=1,NC+7)
          END IF
        ELSE IF (HPLTGEN%TYPEFG .EQ. 3) THEN
C         feq ptsf file - write last record of all zeroes
          VALNOW= 0.0
          WRITE (HPLTGEN%PLOTFL) VALNOW,VALNOW,VALNOW
        END IF
      END IF
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMPLTGEN
               