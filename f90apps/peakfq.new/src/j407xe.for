C
C
C
      SUBROUTINE   J407XE
     #                 ( IA1, IA3, PAUSE, EMAOPT, UPDATEFG )
C
C      + + + PURPOSE + + +
C     This routine processes users input and options and controls
C     processing for frequency analysis and output.
C     J407 -- USGS-WRC FLOOD FREQUENCY ANALYSIS PER WRC BULL 17-B, 1981.
C
C     FOR CURRENT VERSION/REV-DATE, SEE SUBRTNE PRTPHD, FMT NO. 201/202.
C     ALSO SEE SUBRTNE WCFAGB, FMT NO 1.
C
C     + + + HISTORY + + +
C     VER 76.00 BY WKIRBY, WRD-NR, MAY 1976. (BULL.17)
C     VER 2.0 BY WKIRBY, WRD-NR, APRIL 77.  (BULL.17-A)
C     VER 3.0 BY WKIRBY, WRD-NR, MAY 1979.
C     VER 3.7P - PRIME REVISIONS - K.FLYNN 12/83.
C     VER 3.8P - WK 12/86,  7/88.
C     SET ARGUMENTS = 0 FOR NON-ANNIE/NON-WDM USE
C     VER 3.9P - WK, AML 12/88
C     VER 3.9A-P - WK, AML 2/89
C     MODIFIED 8/9/89 AML (deleted BLOCKDATA)
C     Modified 6/93 AML to coding convention and add requirements
C                       for distribution by Texas, changed to an
C                       80 char/record print file, made Z,H,N,Y
C                       input records optional 
C     Updated for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IA1, IA3, PAUSE, EMAOPT
      LOGICAL   UPDATEFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IA1    - Fortran unit number for message file
C     IA3    - Fortran unit number for users WDM file
C     PAUSE  - Indicator flag for pause between stations
C              1 - yes, pause and wait for user response
C              2 - no, display summary of results and continue
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     UPDATEFG - boolean to indicate type of run
C                TRUE - run is just updating the spec file (don't do graphics)
C                FALSE - full run
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
      INCLUDE 'pmxpk.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cjobop.inc'
      INCLUDE 'clunit.inc'
      INCLUDE 'cstaid.inc'
C
      COMMON/ HEADNS / HEADNG(9)
      CHARACTER*80   HEADNG
C
      INCLUDE 'cj407.inc'
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C
C      + + + LOCAL VARIABLES + + +
      INTEGER   IPKPTR(MXPK),  IQUAL(MXPK)
      REAL      FCXPG(MXINT)
      INTEGER   MAXPKS, IER, NFCXPG, JSEQNO,NPROC, NERR, NSKIP, NSTAYR,
     &          NSKIP1, NPKS, I, NPKPLT, 
     $          ISTART, HSTFLG, XPKS    
Cprh     $      , SCLU, CNUM, CLEN, SGRP, MXLN, SCI, IWRT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IQUAL(1),IPKPTR(1))
C
C     + + + SAVES + + +
      SAVE   JSEQNO,  NFCXPG,  FCXPG
C
C     + + + FUNCTIONS + + +
      REAL   GAUSEX
C
C     + + + INTRINSICS + + +
      INTRINSIC  INT, MIN0, MAX0
C
C     + + + EXTERNALS + + +
      EXTERNAL   INPUT, PRTPHD, PRTINP, ALIGNP, PRTFIT
      EXTERNAL   OUTPUT, PLTFRQ, RUNEMA, WCFAGB
      EXTERNAL   SORTM, PRTIN2, PRTIN3, GAUSEX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  IER,  NFCXPG ,  JSEQNO
     $     /  0,   -777 ,     0    /
C
C     + + + FORMATS + + +
 1000 FORMAT(///' End PeakFQ analysis.'
     $         /'   Stations processed :',I8 
     $         /'   Number of errors   :',I8
     $         /'   Stations skipped   :',I8
     $         /'   Station years      :',I8//)
 2000 FORMAT('Data records may have been ignored ',
     $       'for the stations listed below.',
     $    /, '(Card type must be Y, Z, N, H, I, 2, 3, 4,  or *.)',
     $    /, '(2, 4, and * records are ignored.)')
C
C     + + + END SPECIFICATIONS + + +
C
C     SUBROUTINES USED --
C     INPUT   - READS INPUT DATA INTO J407.  USES INPUT2, PKFRD4.
C               ALSO - INPUT1/ANNIE,  INPUT3/PKFQH3.
C     J407VR - BLOCK DATA, VERSION NUMBER
C     PRTPHD - PRINTS PAGE-HEADINGS
C     PRTINP - PRINTS INPUT DATA LISTINGS
C     ALIGNP - ALIGNS PROB-PLOT POSITIONS WITH DISCHARGES FOR PRINTING
C     PRTFIT - PRINTS TABLE OF FITTED DISTRIBUTION
C     OUTPUT - OUTPUTS RESULTS TO FILE.  USES OUTPT1/ANNIE AND --
C     BCFPCH - PUNCHED OUTPUT IN USGS BASIN-CHAR FILE FMT.  DUMMY IN PRIME.
C     PLTFRQ - FREQUENCY CURVE PLOT.  USES  FRQPLT, FRQPL...X.
C     FRQPLT - FREQUENCY CURVE PRINTER-PLOT.  USES  J407P4, J407SC.
C     WCFAGB  - DOES THE WRC FLOOD FREQUENCY CALCULATIONS.  USES ALL
C               WCF... ROUTINES (WCFAPI ... WCFFCX) AND COMMONS
C               WCFCM0,1,2. ALSO USES GAUSEX,STUTX,OUTKGB,HARTXX....
C     WCFBDI - BLOCK DATA FOR WCFAGB.
C
C------------------------
C
C     maximum number of peaks
      MAXPKS = MXPK
C
      WRITE(*,*) "J407XE:IA1:",IA1," INFORM:",INFORM," MSG1:",MSG1
C     CHECK FOR REPEAT CALL FROM MAIN PGM
      IF( JSEQNO .LE. 0 .OR.  INFORM .GT. 0) THEN      
C
        MSG = MSG1
C
C       PRINT J407 PAGE HEADER AT START OF JOB
        CALL PRTPHD( 0 , IWXMOD, EMAOPT, IA3 )
C
C       put general message about skipping records to scratch file
        WRITE(91,2000)
C
        MSL = 2
        IF(IDEBUG.EQ.1) MSL = 4
        NOPPOS = 1 - (IPLTOP+IPPOS)
        WSKLAT=0.
        IF(ISKUDP.EQ.1)WSKLAT=0.10
        IF(ISKUDP.EQ.2)WSKLAT=0.01
        NOEPFC = NOXPA
        NOCLIM = NOCLM
C
        NPROC  = 0
        NERR   = 0
        NSKIP  = 0
        NSTAYR = 0
        JSEQNO = 0
      END IF
C
C     for ascii input need to reset start flag for 1st record read
      ISTART = 0
C
  100 CONTINUE
        JSEQNO = JSEQNO + 1
        CALL PRTPHD( 1000 , JSEQNO, EMAOPT, IA3)
C
        CALL INPUT (IA1,IA3,INFORM,MAXPKS,EMAOPT,IA3,IBCPUN,
     M              ISTART,
     O              STAID,PKS,IPKSEQ,XQUAL,IQUAL, 
     O              NHIST,NSYS,HISTPD,QHIOUT,QLWOUT,GAGEB,
     O              GENSKU,RMSEGS, IGSOPT, NSKIP1, IER )
C
           write(*,*)'After INPUT, NSYS,NHIST',NSYS,NHIST
        NPKS=NHIST+NSYS
        NSTAYR=NSTAYR+NPKS
        NSKIP=NSKIP+NSKIP1
C       count peaks to be skipped
        XPKS = 0
        DO 120 I = 1, NPKS
          IF (PKS(I) .LT. 0.0) XPKS = XPKS + 1
 120    CONTINUE
        IF(IER.GE.2) GO TO  970
        IF(NSKIP1.NE.0 )  THEN
           JSEQNO = JSEQNO + 1
cprh       this call just creates a null page, messes up the page numbering
cprh           CALL PRTPHD(  1000, JSEQNO, EMAOPT, IA3 )
        ENDIF
C
        IGSOPT=MAX0(-1,MIN0(+1,IGSOPT))
        IF(IWXMOD.NE.0  .AND. RMSEGS.LE.0.) RMSEGS = RMSDGS
        STAID(79:90) = ' '
        IF(NHIST.GT.0 .OR. HISTPD.GT.0.) STAID(79:90) =
     $                                    '* HISTORIC *'
C
C       CALL  PRTPHD(  2001 , -999 )
        CALL  PRTINP( IDEBUG, XPKS, EMAOPT, IA3 )
C
         write(*,*)'Debug Info for ',STAID
        CALL WCFAGB(PKS, PKLOG, WRCPP, SYSPP, NPKS, IER)
         write(*,*)'after WCFAGB, IER=',IER

        IF (EMAOPT.EQ.1) THEN
       write(*,*)
       write(*,*)'Running EMA for station ',STAID
       write(*,*)
       write(*,*)'calling RUNEMA: NPKS,NSYS,GENSKU,RMSEGS',
     $                            NPKS,NSYS,GENSKU,RMSEGS
          CALL RUNEMA(NPKS,PKS,IPKSEQ)
        END IF

        IF(IER .GE. 3)  THEN
          NERR=NERR+1
          IF(MSL .LT. 4) CALL PRTIN2 ( 1 ,MSG, NPKS, IPKSEQ,PKS,XQUAL,
     $                                 EMAOPT, IA3 )
        ELSE
          NPROC=NPROC+1
C
C         PRINT FITTED LOG-PEARSON TYPE III FREQUENCY CURVES PARAMETERS
C                                                   AND ORDINATES
          CALL PRTFIT ( IDEBUG, EMAOPT, IA3 )
C
Ckmf      moved call to output to after plot so that the plot will
Ckmf      be drawn before the output is displayed.  When pause
Ckmf      was added, it caused the plot to display after the
Ckmf      summary statistics were cleared from the screen, making
Ckmf      it appear as if the plot went with the next station
Ckmf      IF(IBCPUN.EQ.1) CALL OUTPUT(STAID,SYSUAV,SYSUSD,SYSSKW,
Ckmf $        WRCUAV,WRCUSD,WRCSKW,WRCFC ,  INT(HISTPN+.5), NSYS,
Ckmf $        IBCPUN, IPUNCH,  IA1,IA3, PAUSE )
C
          IF(IPLTOP.NE.0 .OR. (IPPOS*IPRTOP.NE.0)) THEN      
C
C           sort input peak logs and correlate with plotting positions
            CALL SORTM( PKLOG, IPKPTR, 1, -1, NPKS )
            IF(NHIST.GT.0) CALL ALIGNP(IPKPTR,IPKSEQ,NPKS,NHIST,SYSPP)
C
C           print input data and plotting positions
            IF(IPRTOP .NE. 0 )  THEN      
              IF(IPPOS.EQ.0) THEN
C               short output
                CALL PRTIN2(0,MSG,NPKS,IPKSEQ,PKS,XQUAL,EMAOPT,IA3)
              ELSE
C               longer output
                CALL PRTIN3 (MSG,NPKS,IPKSEQ,PKS,XQUAL,
     $                       GAGEB, IPKPTR, SYSPP, WRCPP, WEIBA,
     $                       EMAOPT, IA3)
              END IF
            END IF    
C
            IF(IPLTOP.NE.0)  THEN
C             initialize (if necessary)
              IF(NFCXPG.LE.0) THEN
Cprh                DO 170 I = 1,31
                DO 170 I = 1,MXINT
                  FCXPG(I) = GAUSEX(TXPROB(I))
  170           CONTINUE
                NFCXPG = INDX2 - INDX1 + 1
              ENDIF
              NPKPLT=NHIST+NSYS-NBGB
C             convert to std deviates
              DO 190 I=1,NPKPLT
                SYSPP(I)=GAUSEX(SYSPP(I))
                WRCPP(I)=GAUSEX(WRCPP(I))
  190         CONTINUE
C             set flag to plot historic adjusted peaks, 0-y,1-n
C             Note:  When qhiout > .01 and histpd <= .05,
C                    the historic adjusted peaks are plotted,
C                    however, they will equal the systematic
C                    peaks becaus they have not actually been
C                    adjusted.  They are plotted like this as
C                    a warning to the user, Is it appropriate
C                    to have a high-outlier discharge threshold
C                    when you don't have a lenght of historical
C                    period?
              IF (QHIOUT .LE. 0.01 .AND. HISTPD .LE. 0.5) THEN
C               don't plot historic adjusted peaks
                HSTFLG = 1
              ELSE
C               do plot historic adjusted peaks
                HSTFLG = 0
              END IF
              IF (.NOT.UPDATEFG) THEN
              CALL PLTFRQ( MSG, HEADNG, IPLTOP, GRFMT,
     $                NPKPLT, PKLOG, SYSPP, WRCPP, WEIBA,
     $                SYSRFC(INDX1),WRCFC(INDX1),FCXPG(INDX1),NFCXPG,
     $                IWXMOD,HSTFLG,
     $                NOCLIM, CLIML(INDX1), CLIMU(INDX1), JSEQNO )
              END IF
            ENDIF
          END IF
Ckmf      relocated output so in sync with graphics
          IF(IBCPUN.GT.0) THEN
C           output statistics to wdm (1 or 3) and/or watstore 2 or 3)
C           summary screen output if pause=1
            CALL OUTPUT (STAID,SYSUAV,SYSUSD,SYSSKW,
     $                   WRCUAV,WRCUSD,WRCSKW,WRCFC ,
     $                   INT(HISTPN+.5), NSYS,
     $                   IBCPUN, IPUNCH,  IA1,IA3, PAUSE, JSEQNO)
          ELSE IF (INFORM .EQ. 1  .AND.  PAUSE .EQ. 1) THEN
C           no output statistics, but summary screen for wdm input
Cprh            CALL OUTPT2 ( STAID, WRCUAV, WRCUSD, WRCSKW, WRCFC, IA1 )
          END IF
        END IF
C
        IF (INFORM .EQ. 2  .AND.  PAUSE .EQ. 1) THEN
C         ascii input and pause between statTions, stats to screen
Cprh          CALL OUTPT2 ( STAID, WRCUAV, WRCUSD, WRCSKW, WRCFC,
Cprh     I                  IA1 )
        END IF
Ckf     IF (INFORM .EQ. 2 .AND. IMODFG .EQ. 1) THEN
C         tell user completed, if ascii file
Ckf       SCLU = 121
Ckf       SGRP = 69
Ckf       MXLN = 10
Ckf       SCI = 1
Ckf       IWRT = 0
Ckf       CNUM = 1
Ckf       CLEN = 76
Ckf       CALL PMXTXA (IA1,SCLU,SGRP,MXLN,SCI,IWRT,CNUM,CLEN,
Ckf  $                 HEADNG(9)(1:76))
Ckf     END IF
C
Cprh        IF (IER .EQ. 3) THEN
CprhC         tell user aborted                    
Cprh          SCLU = 121
Cprh          SGRP = 75
Cprh          MXLN = 10
Cprh          SCI = 1
Cprh          IWRT = 0
Cprh          CNUM = 1
Cprh          CLEN = 76
Cprh          CALL PMXTXA (IA1,SCLU,SGRP,MXLN,SCI,IWRT,CNUM,CLEN,
Cprh     $                 HEADNG(9)(1:76))
Cprh        END IF
C
        IF(INFORM .LE. 0)  RETURN
      GO TO 100
C
  970 CONTINUE
      WRITE(MSG,1000) NPROC,NERR,NSKIP,NSTAYR
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTPHD
     #                  ( II , IARG2, EMAOPT, WDMSFL )
C
C     + + + PURPOSE + + +
C     PRINTS PAGE HEADINGS FOR J407 / BULLETIN 17.
C
C     + + + HISTORY + + +
C     Updated 9/03 for batch version of PEAKFQ,
C     Most common blocks now found in include files,
C     Replaced MSG with MSG1 as declared in include file clunit.inc,
C     Removed reference to JBOPT (equivalenced to IPLTOP) - not used
C     Paul Hummel, AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   II, IARG2, EMAOPT, WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     II     - <= 0 - print initial header
C              1000 - print page header
C              2001 - print station id / header
C              3000 - print header for list of peaks
C     IARG2  - sequence number of station in input
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number for input WDM file
C
C     + + + COMMON BLOCKS + + +
      COMMON / HEADNS /  HEAD1, HEAD2, HEAD3, HEAD4, HEAD5,
     $                   HEAD6, HEAD7, HEAD8, HEAD9
      CHARACTER*80     HEAD1, HEAD2, HEAD3, HEAD4, HEAD5,
     $                 HEAD6, HEAD7, HEAD8, HEAD9
      CHARACTER*80  HEADNG(9), HEAD14(6)
      EQUIVALENCE  (HEADNG(1), HEAD14(1), HEAD1)
C
      INCLUDE 'cstaid.inc'
      INCLUDE 'cjobop.inc'
Cprh  don't see anywhere JBOPT is used
Cprh      INTEGER   JBOPT(8)
Cprh      EQUIVALENCE (JBOPT(1),IPLTOP)
C
      INCLUDE 'clunit.inc'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1  BLANK
C     INTEGER   JDATE(3)
      INTEGER   I, L70,L15,L48,L80,LOC,L10
C     INTEGER   JTIME, ERRCOD, OLEN
      CHARACTER*3  CPRTOP(2), CDEBUG(2)
      CHARACTER*18 CPLTOP(4)
      CHARACTER*14 CBCPUN(6)
      CHARACTER*5  CIPPOS(2)
      CHARACTER*20 CNFORM(3)
      CHARACTER*80 FNAME
      CHARACTER*1  HEADA9(80), CSTA(10)
      CHARACTER*16 CHDTTM
C
C     + + + SAVES + + +
      SAVE  DISCLM, DT,STAIND,PAGIND
      CHARACTER*80  DISCLM(2)
      INTEGER       DT(6),STAIND,PAGIND
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR, CKNBLV
C
C     + + + EXTERNALS + + +
C     EXTERNAL    DATTIM, ZIPC
      EXTERNAL    SYDATM, DATLST, LENSTR, CARVAR, CVARAR, CHRCHR, CKNBLV
C
C      + + + DATA INITIALIZATIONS + + +
      DATA BLANK / ' ' /, L15,L48,L70,L80,L10/15,48,70,80,10/
      DATA CPLTOP/'None','Graphics device','Line printer',
     $            'Graphics & Printer'/
      DATA CBCPUN/'None','WDM','WATSTORE','Both (WAT)',
     $                    'TAB-SEPARATED','Both (TAB)'/
      DATA CPRTOP/'No','Yes'/             
      DATA CDEBUG/'No','Yes'/
      DATA CIPPOS/'Short','Long'/
      DATA CNFORM/'WDM file            ','WATSTORE peak file',
     $            'Log-Pearson records'/
      DATA CSTA  /'S','t','a','t','i','o','n',' ','-',' '/
      DATA STAIND,PAGIND/0,0/
C
C     + + + FORMATS + + +
C 101 FORMAT(///' EXECUTION BEGINNING AT DATE, TIME =',I5,2(1H/,I2),I7
C    &       //)
C 100 FORMAT(  28X,21A1)
  101 FORMAT(/,23X,' --- PROCESSING DATE/TIME ---',
     $      //,28X,21A1)
  102 FORMAT(/,24X,' --- PROCESSING OPTIONS ---  ',
     $      //,19X,'   Plot option         = ',A 
     $       /,19X,'   Basin char output   = ',A 
     $       /,19X,'   Print option        = ',A 
     $       /,19X,'   Debug print         = ',A 
     $       /,19X,'   Input peaks listing = ',A 
     $       /,19X,'   Input peaks format  = ',A,/)
C    $ ' IPLTOP   IBCPUN  IPRTOP   IDEBUG   IPPOS   ISKUDP   NOXPA',
C    $  '  NOCLM  INFORM '/ 9I8//)
C 103 FORMAT( /' Input format =',I4,'  ANNIE/WDM     FILE RETRIEVAL',
C    $  2A1,T21,66X,T21,     '  WATSTORE PEAK-FILE CARDS       ',
C    $  1A1,T21,66X,T21,    '  LOG-PEARSON CARDS              ' )
  110 FORMAT(19X,'   Input files used:')
  111 FORMAT(19X,'      peaks (WDM)    - ',A)
  112 FORMAT(19X,'      peaks (ascii)  - ',A)
  113 FORMAT(19X,'      specifications - ',A)
  114 FORMAT(19X,'   Output file(s): ',/
     $       19X,'      main - ',A)
  115 FORMAT(19X,'      bcd  - ',A)
  199 FORMAT( '1' )
  200 FORMAT('  ')
  201 FORMAT( 2X,'Program PeakFq',11X,'U. S. GEOLOGICAL SURVEY',
     $       13X,'Seq.',I3.3,'.',I3.3 )
Cprh 202 FORMAT( 21X, 'OFFICE OF SURFACE WATER, RESTON, VA' )
  202 FORMAT( 2X,'Ver. 5.1',
     $       12X,'Annual peak flow frequency analysis',
     $        6X,'Run Date / Time' )
Cprh 203 FORMAT( 21X, 'ANNUAL PEAK FLOW FREQUENCY ANALYSIS' )
  203 FORMAT( 2X,'02/01/2007',
     $       10X,'following Bulletin 17-B Guidelines',7X,A)
  213 FORMAT( 2X,'05/06/2005',
     $        9X,'using Expected Moments Algorithm (EMA)',4X,A )
Cprh 204 FORMAT( 21X, 'Following Bulletin 17-B Guidelines' )
Cprh 205 FORMAT( 21X, '          Program peakfq    ' )
  205 FORMAT(12X,'WARNING:  For experimental use only, EMA is not the')
  206 FORMAT(22X,'standard method for flood frequency analysis.')
Cprh 206 FORMAT( 21X, '     (Version 4.1, February, 2002)' )
  207 FORMAT( 20X, A40 )
  227 FORMAT(A16)
  208 FORMAT( ' ',2A1,T1,5('   *** EXPERIMENTAL ***   ')  )
C 209 FORMAT(2X, A10, A15, 2X, A48)
  301 FORMAT(  2X, '*********  NOTICE  --  Preliminary machine ',
     $             'computations.        *********' )
  302 FORMAT(  2X, '*********  User responsible for assessment ',
     $             'and interpretation.  *********' )
  401 FORMAT(  2X, '*********  WARNING  --  Experimental ',
     $             'modification of 17B calculations  *********' )
  402 FORMAT(  2X, '***************    User is responsible for ',
     $             'assessment and interpretation.    *********' )
  501 FORMAT(   '1', /, (A) )
  502 FORMAT(   80A1 )
  503 FORMAT(///, A, /, A, / )
  504 FORMAT(  /, A, /, A, / )

  600 FORMAT( A,'Bulletin 17B analysis run ',21A1)
  601 FORMAT( A,'EMA analsis run ',21A1)
  700 FORMAT(I2.2,'/',I2.2,'/',I4,I3.2,':',I2.2)
C
C     + + + END SPECIFICATIONS + + +
C
      IF( II .LE. 0 )  THEN
C       PRINT INITIAL PAGE, DETERMINE DATE AND TIME.
C       CALL DATTIM( JDATE, JTIME )
C       WRITE(MSG1, 101)JDATE,JTIME
        CALL SYDATM (DT(1),DT(2),DT(3),DT(4),DT(5),DT(6))
Ckmf    check for y2k, convert 2-digit year to 4 digit year if needed.
        IF (DT(1) .LT. 90) THEN
C         assume 2000 or later
          DT(1) = DT(1) + 2000
Cprh    fixed bug in following record that had D(I), not D(1)
        ELSE IF (DT(1) .LT. 100) THEN
C         assume before 2000
          DT(1) = DT(1) + 1900
        END IF
        WRITE(CHDTTM,700) DT(2),DT(3),DT(1),DT(4),DT(5)
        WRITE(MSG1,  199)
        WRITE(MSG1,  201) STAIND,PAGIND
        WRITE(MSG1,  202)
        IF (EMAOPT .EQ. 0) THEN
C         traditional B17 analysis
          WRITE(MSG1,  203) CHDTTM
        ELSE
C         using new EMA option
          WRITE(MSG1,  213) CHDTTM
          WRITE(MSG1, *)
          WRITE(MSG1,  205)
          WRITE(MSG1,  206)
        END IF
Cprh       WRITE(MSG1,  205)
Cprh       WRITE(MSG1,  206)
Cprh    original version never set JOBTTL, leave out for new version
Cprh        WRITE(MSG1,  207)  JOBTTL
Cprh        CALL DATLST (DT,CHDTTM,OLEN,ERRCOD)
        
Cprh        WRITE(MSG1, 101) (CHDTTM(I),I=1,OLEN)
        WRITE(MSG1, 102) CPLTOP(IPLTOP+1), CBCPUN(IBCPUN+1),
     $                   CPRTOP(IPRTOP+1), CDEBUG(IDEBUG+1),
     $                   CIPPOS(IPPOS+1),  CNFORM(INFORM)
C       WRITE(MSG1, 103) INFORM, (' ',I=1,INFORM)
        WRITE(MSG1, 110)
        IF (INFORM.EQ.1) THEN
C         WDM file
          INQUIRE(WDMSFL,NAME=FNAME)
          WRITE(MSG1,111) FNAME
        ELSE
C         Ascii file
          INQUIRE(INCRD,NAME=FNAME)
          WRITE(MSG1, 112) FNAME
        END IF
        INQUIRE(SPCFUN,NAME=FNAME)
        WRITE(MSG1,113) FNAME
        INQUIRE(MSG1,NAME=FNAME)
        WRITE(MSG1,114) FNAME
        IF (IBCPUN.GE.2) THEN
C   	    outputting additional BCD file
          INQUIRE(IPUNCH,NAME=FNAME)
          WRITE(MSG1,115) FNAME
        END IF
        WRITE(MSG1,200)
C       prepare page heading in character strings
        WRITE(HEAD1,200)
        WRITE(HEAD2,201) STAIND,PAGIND
        WRITE(HEAD3,202)
        IF (EMAOPT .EQ. 0) THEN
C         traditional B17 analysis
          WRITE(HEAD4,203) CHDTTM
          HEAD5 = ' '
          HEAD6 = ' '
        ELSE
C         using new EMA option
          WRITE(HEAD4,213) CHDTTM
C         include warning about using EMA method
          WRITE(HEAD5,205)
          WRITE(HEAD6,206)
        END IF
C       WRITE(HEAD7,207) JOBTTL
C       put date/time here for plots
Cprh    date/time now in page header
Cprh        WRITE(HEAD7,227) CHDTTM
        HEAD7 = ' '
        WRITE(HEAD8,208) (BLANK, I=1,IARG2)
C       SET UP DISCLAIMER
        IF(IARG2 .GE. 2) THEN
          WRITE(DISCLM(1),401)
          WRITE(DISCLM(2),402)
        ELSE
          WRITE(DISCLM(1),301)
          WRITE(DISCLM(2),302)
        ENDIF
C
      ELSE IF( II .EQ. 1000 )  THEN
C       PRINT PAGE HEADINGS FOR PGM OUTPUT....
C       FIRST INSERT SEQUENCE NUMBER
C       WRITE(HEADNG(6)(47:50),   '(I4)' )  IARG2
C       above sequence number deleted because didn't fit in header

C       start of new station
        STAIND = STAIND + 1
        PAGIND = 0
        WRITE(MSG1,199)
Cprh    header will be written under II=2001 conditional below
Cprh        WRITE(MSG1,501) HEAD14
C
      ELSE IF ( II .EQ. 2001 ) THEN
C       start of new page
        PAGIND = PAGIND + 1
        WRITE(MSG1,  201) STAIND,PAGIND
        WRITE(MSG1,  202)
        IF (EMAOPT .EQ. 0) THEN
C         traditional B17 analysis
          WRITE(MSG1,  203) CHDTTM
        ELSE
C         using new EMA option
          WRITE(MSG1,  213) CHDTTM
          WRITE(MSG1, *)
          WRITE(MSG1,  205)
          WRITE(MSG1,  206)
        END IF
        WRITE(MSG1,200)
C       build station id/ description
        CALL CHRCHR(L10,CSTA,HEADA9(1))
        CALL ZIPC (L70,BLANK,HEADA9(11))
        CALL CVARAR (L15,STAID(1:15),L15,HEADA9(11))
        LOC = LENSTR(L80,HEADA9)
        CALL CVARAR (L48,STAID(21:68),L48,HEADA9(LOC+3))
        LOC = LENSTR(L80,HEADA9)
        CALL CARVAR (L80,HEADA9,L80,HEAD9)
C       change null to blank
        DO 55 I = 11,80
          IF (ICHAR(HEADA9(I)) .EQ. 0) HEADA9(I) = ' '
 55     CONTINUE
        CALL CTRSTR (L80,HEADA9)
        WRITE(MSG1, 502) HEADA9
Cprh        WRITE(MSG, 100) (CHDTTM(I),I=1,OLEN)
C       WRITE(HEAD9,209)'Station - ',STAID(1:15), STAID(21:68)
C       WRITE(MSG1,'(/1X,(A))') HEAD9
Cprh    update station/page index on HEAD2 for future use
        WRITE(HEAD2,201) STAIND,PAGIND+1
      ELSE IF ( II .EQ. 2002 ) THEN
        WRITE(MSG1,503) DISCLM
      ELSE IF ( II .EQ. 3000 ) THEN
C       PRINT HEADING FOR LIST OF INPUT PEAKS
        WRITE(MSG1,501) HEADNG
        IF(IARG2 .NE. -3301) WRITE(MSG1,504) DISCLM
      ELSE
        STOP 777
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTINP
     #                  (IDEBUG ,XPKS, EMAOPT, WDMSFL)
C
C     + + + PURPOSE + + +
C     PRINTS LISTINGS OF J407/WCF INPUT DATA --  INPUT PARAMS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDEBUG, XPKS, EMAOPT, WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDEBUG -
C     XPKS   - number of peaks to be excluded from analysis (neg value)
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number for input WDM file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER * 15  DWORK(4)
      CHARACTER*12 SKUOP(3)
      INTEGER   I
      CHARACTER*8  YNHIST
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRTPHD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA         SKUOP /'STATION SKEW','  WEIGHTED  ',
     $         ' GENERALIZED'/
C
C     + + + FORMATS + + +
    4 FORMAT(// 21X,  'I N P U T   D A T A   S U M M A R Y')
    5 FORMAT(  
     $  /16X,'Number of peaks in record            = ',I8,
     $  /16X,'Peaks not used in analysis           = ',I8,
     $  /16X,'Systematic peaks in analysis         = ',I8,
     $  /16X,'Historic peaks in analysis           = ',I8,
     $  /16X,'Years of historic record             = ',I8,
     $  /16X,'Generalized skew                     = ',F8.3,
     $  /16X,'     Standard error                  = ',2X,A6,
     $  /16X,'     Mean Square error               = ',2X,A6,
     $  /16X,'Skew option                          = ',A,
     $  /16X,'Gage base discharge                  = ',F8.1,
     $  /16X,'User supplied high outlier threshold = ',A,
     $  /16X,'User supplied low outlier criterion  = ',A,
     $  /16X,'Plotting position parameter          = ',F8.2)
 6    FORMAT(/)
C    $ /'     -- YEARS OF RECORD --    HISTORIC    GENERALIZED',
C    $           'GAGE BASE'/
C    $  5X,'SYSTEMATIC   HISTORIC',6X,'PEAKS',8X,'SKEW',7X,
C    $  'GENERAL. SKEW   OPTION',
C    $  5X,      'DISCHARGE'/
C    $  /' ', 8X,I3,5X,I7,9X,I3,7X,F7.3,8X, A6,5X,  A ,2X,F8.1,//
C    $         '     USER-SET OUTLIER CRITERIA   '         /
C    $        '     HIGH OUTLIER   LOW OUTLIER  '        /
C    $   6X, 2A )
C
C     + + + END SPECIFICATIONS + + +
C
      DO 107 I = 1,3
        DWORK(I) = '  --  '
  107 CONTINUE
C
      IF (NHIST .GT. 0 .OR. HISTPN .GT. 0.0) THEN
C       historic adjustment applied
        YNHIST = '     YES'
      ELSE
        YNHIST = '      NO'
      END IF
      IF(RMSEGS .GT. 0.) THEN
        WRITE(DWORK(1),'(F6.3)') RMSEGS
        WRITE(DWORK(4),'(F6.3)') RMSEGS**2
      END IF
      IF(QHIOUT .GT. 0.) WRITE(DWORK(2),'(1X,F9.1)') QHIOUT
      IF(QLWOUT .GT. 0.) WRITE(DWORK(3),'(1X,F9.1)') QLWOUT
      WRITE(MSG,6) 
      CALL  PRTPHD(  2001 , -999, EMAOPT, WDMSFL )
      WRITE(MSG,4)
      WRITE(MSG,5) NSYS+NHIST, XPKS, NSYS-XPKS, NHIST,
C    $             INT(HISTPD+.5), YNHIST, GENSKU, DWORK(1),
     $             INT(HISTPD+.5),         GENSKU, DWORK(1),DWORK(4),
     $             SKUOP(IGSOPT+2),GAGEB, DWORK(2),DWORK(3),WEIBA
      CALL PRTPHD( 2002,   -999, EMAOPT, WDMSFL  )
      IF(IDEBUG.NE.0) THEN
        WRITE(MSG,*)'   PeakFQ-DEBUG OPTION SET =',IDEBUG
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTIN2
     #                 ( IOPT, MSG, NPKS, IPKSEQ, PKS, XQUAL,
     #                   EMAOPT, WDMSFL )
C
C     + + + PURPOSE + + +
C     PRINTS SHORT LIST OF INPUT PEAKS
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MSG, NPKS, IOPT, EMAOPT, WDMSFL
      INTEGER  IPKSEQ(NPKS)
      CHARACTER*(*)  XQUAL(NPKS)
      REAL     PKS(NPKS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPT   -
C     MSG    -
C     NPKS   -
C     IPKSEQ -
C     PKS    -
C     XQUAL  -
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number for input WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, ND2
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRTPHD
C
C     + + + FORMATS + + +
   14 FORMAT(///7X,25HLISTING  OF  INPUT  PEAKS//
     $              9X,21H YR   DISCHARGE  CODE  //
     $                  (5X,I8,F12.1,A6) )
   18 FORMAT(///' ******  AFTER  CALC  ABORT  --')
  104 FORMAT(//8X,'Explanation of peak discharge qualification codes',
     $//6X,' PeakFQ    NWIS',
     $ /6X,'  CODE     CODE   DEFINITION',
     $//6X,'    D        3    Dam failure, non-recurrent flow anomaly',
     $ /6X,'    G        8    Discharge greater than stated value',
     $ /6X,'    X       3+8   Both of the above',
     $ /6X,'    L        4    Discharge less than stated value',
     $ /6X,'    K     6 OR C  Known effect of regulation or ',
     $                        'urbanization',
     $ /6X,'    H        7    Historic peak', /
     $ /6X,'    -  Minus-flagged discharge -- Not used in computation',
     $ /6X,'          -8888.0 -- No discharge value given',
     $ /6X,'    -  Minus-flagged water year -- ',
     $            'Historic peak used in computation' ///)
 1010 FORMAT('1',//)
 1011 FORMAT(//23X,'I N P U T   D A T A   L I S T I N G')
 1012 FORMAT(//,2('     WATER YEAR    DISCHARGE   CODES ')/)
 1013 FORMAT(2(I12,F15.1, 1A10))
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(MSG,1010) 
      IF(IOPT .EQ. 1) WRITE(MSG,18)
C     IF(IOPT .NE. 1)  CALL PRTPHD( 3000, -3301 )
C     write table of observed data
      CALL PRTPHD ( 2001, -999, EMAOPT, WDMSFL )
      WRITE(MSG,1011)
      WRITE(MSG,1012)
      ND2 = (NPKS+1)/2
      DO 210 I = 1,ND2
        IF (I+ND2 .LE. NPKS) THEN
          WRITE(MSG,1013) IPKSEQ(I), PKS(I), XQUAL(I),
     $             IPKSEQ(I+ND2), PKS(I+ND2), XQUAL(I+ND2)
        ELSE
          WRITE(MSG,1013) IPKSEQ(I), PKS(I), XQUAL(I)
        END IF
  210 CONTINUE
C
C     write key to codes
      WRITE(MSG, 104 )
C
C     WRITE(MSG,14)(IPKSEQ(I),PKS(I),XQUAL(I),I=1,NPKS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTIN3
     #                 ( MSG,  NPKS, IPKSEQ,PKS, XQUAL,
     $     GAGEB,  IPKPTR, SYSPP, WRCPP , WEIBA, EMAOPT, WDMSFL )
C
C     + + + PURPOSE + + +
C     PRINTS INPUT PEAKS IN INPUT ORDER AND IN RANKED ORDER
C     WITH SYSTEMATIC AND WRC PLOTTING POSITIONS.
C
C     NOTE -- THE PEAKS AND THEIR PLOTTING POSITIONS MUST
C     BE LINED UP PROPERLY BY PREVIOUS CALLS TO SORTM
C     AND ALIGNP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MSG, NPKS, EMAOPT, WDMSFL
      REAL    PKS(NPKS),  SYSPP(NPKS), WRCPP(NPKS), WEIBA
      REAL    GAGEB
      INTEGER  IPKSEQ(NPKS), IPKPTR(NPKS)
      CHARACTER*(*)  XQUAL(NPKS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSG    -
C     NPKS   -
C     IPKSEQ -
C     PKS    -
C     XQUAL  -
C     GAGEB  -
C     IPKPTR -
C     SYSPP  -
C     WRCPP  -
C     WEIBA  -
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number for input WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   JLINE,         I, NB, J, ILINE, ND2
      REAL    EPSILN
      CHARACTER*8 ESTTYP(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
C     EXTERNAL   PRTPHD
C
C     + + + FORMATS + + +
  104 FORMAT(//8X,'Explanation of peak discharge qualification codes',
     $//6X,' PeakFQ    NWIS',
     $ /6X,'  CODE     CODE   DEFINITION',
     $//6X,'    D        3    Dam failure, non-recurrent flow anomaly',
     $ /6X,'    G        8    Discharge greater than stated value',
     $ /6X,'    X       3+8   Both of the above',
     $ /6X,'    L        4    Discharge less than stated value',
     $ /6X,'    K     6 OR C  Known effect of regulation or ',
     $                        'urbanization',
     $ /6X,'    H        7    Historic peak' /
     $ /6X,'    -  Minus-flagged discharge -- Not used in computation',
     $ /6X,'          -8888.0 -- No discharge value given',
     $ /6X,'    -  Minus-flagged water year -- ',
     $            'Historic peak used in computation' ///)
 1010 FORMAT('1',//)
 1011 FORMAT(//23X,'I N P U T   D A T A   L I S T I N G')
C1012 FORMAT(/ 23X,10HWATER YEAR,4X, 9HDISCHARGE,    
C    $       9H   CODES /)  
 1012 FORMAT(//,2('     WATER YEAR    DISCHARGE   CODES ')/)
C1013 FORMAT(20X,I10,F15.1, 1A10) 
 1013 FORMAT(2(I12,F15.1, 1A10))
C1017 FORMAT(/33X,'-- CONTINUED --')
C
 1021 FORMAT( //3X,
     $      'EMPIRICAL FREQUENCY CURVES -- ',A,' PLOTTING POSITIONS'
     $      / 73X, A, '** WEIBA =', F6.3, ' ***' )
Cprh 1022 FORMAT( 6X, 5HWATER, 9X, 6HRANKED, 7X,
Cprh     $       10HSYSTEMATIC, 6X,'BULL.17B'/ 
Cprh     $       7X,4HYEAR, 7X, 9HDISCHARGE, 8X, 6HRECORD,8X,8HESTIMATE/)
 1022 FORMAT(6X,'WATER',7X,'  RANKED ',6X,'SYSTEMATIC',6X, A, / 
     $       6X,' YEAR',7X,'DISCHARGE',6X,'  RECORD  ',6X,'ESTIMATE'/)
 1023 FORMAT( I11,F15.1,2F15.4,
     $      2A1,T27,'           --  ',  1A1, '          --  ' )
C1027 FORMAT(/33X,'-- CONTINUED --')
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   EPSILN/1.0E-6/
      DATA   ESTTYP/ 'BULL.17B' , '  EMA   ' /
C
C     + + + END SPECIFICATIONS + + +
C
C     write table of observed data
      WRITE(MSG,1010)
      CALL PRTPHD ( 2001, -999, EMAOPT, WDMSFL )
      WRITE(MSG,1011)
      WRITE(MSG,1012)
      ND2 = (NPKS+1)/2
      DO 210 I = 1,ND2
        IF (I+ND2 .LE. NPKS) THEN
          WRITE(MSG,1013) IPKSEQ(I), PKS(I), XQUAL(I),
     $             IPKSEQ(I+ND2), PKS(I+ND2), XQUAL(I+ND2)
        ELSE
          WRITE(MSG,1013) IPKSEQ(I), PKS(I), XQUAL(I)
        END IF
  210 CONTINUE
C
C     write key to codes
      WRITE(MSG, 104 )
C
C     write table of frequency curves
      WRITE(MSG,1010)
      CALL PRTPHD ( 2001, -999, EMAOPT, WDMSFL )
      JLINE = 0
  302 CONTINUE
        ILINE = JLINE+1
C       IF(ILINE.GT.1)  WRITE(MSG,1027)
C       CALL PRTPHD( 3000 , -999  )
        IF ( ABS(WEIBA).GT.EPSILN ) THEN
          WRITE(MSG,1021) 'WEIBXXX', '*', WEIBA
        ELSE
          WRITE(MSG,1021) 'WEIBULL'
        END IF
        WRITE(MSG,1022) ESTTYP(EMAOPT + 1)
C       IF(ILINE.GT.1)WRITE(MSG,1027)
        JLINE = NPKS
C       NLINES = JLINE-ILINE+1
C       IF(NLINES.GT.40)JLINE = ILINE+34
C       IF(NLINES.GT.50)JLINE = ILINE+39
        DO 310 I = ILINE,JLINE
          NB = 1
          IF(IPKSEQ(IPKPTR(I)) .LT. 0)    NB = 2
          IF(PKS(IPKPTR(I)) .LE. GAGEB )  NB = 3
          WRITE(MSG,1023) IPKSEQ(IPKPTR(I)), PKS(IPKPTR(I)), 
     *                  SYSPP(I), WRCPP(I) ,
     $                  (' ',J=1,NB)
  310   CONTINUE
      IF(JLINE.LT.NPKS) GO TO 302
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTFIT
     #                 ( IDEBUG, EMAOPT, WDMSFL )
C
C     + + + PURPOSE + + +
C     PRINTS TABLUATED FITTED LOG-PEARSON TYPE III CURVE FOR J407.
C
C     + + + HISTORY + + +
C     Updated 11/03 by PRH of AQUA TERRA Consultants for batch PEAKFQ
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDEBUG, EMAOPT, WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDEBUG -
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*13  DWORK(5)
      INTEGER   I, J, SIGDIG, DECPLA, LEN
      REAL      PEP, TMP !, XTRPK
C
C     + + + SAVES + + +
      SAVE     INITIP, IPLIST
      INTEGER  IPLIST(MXINT)
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   SORTI, MOVEI, ZIPI, DECCHX  !, QEXTRA
C
C     + + + DATA INITIALIZATIONS + + +
Cprh  use INITIP flag to force init of IPLIST so that change to
Cprh  size of MXINT doesn't require update of this DATA statement
Cprh      DATA     IPLIST / -777, 30*0 /
      DATA     INITIP / 0 /
C
C     + + + FORMATS + + +
    8 FORMAT(//1X,10X,  'ANNUAL FREQUENCY CURVE PARAMETERS -- ',
     $       21HLOG-PEARSON TYPE III    // 
     $'                        FLOOD BASE      ',
     $'             LOGARITHMIC         '/
     $'                  ----------------------',
     $'  -------------------------------'/
     $'                             EXCEEDANCE ',
     $'               STANDARD          '/
     $'                   DISCHARGE PROBABILITY',
     $'     MEAN     DEVIATION     SKEW '/18X,55('-'))      
C   9 FORMAT(     5X,32HSYSTEMATIC PEAKS ABOVE BASE  --    ,
C    $  10X,2H--,2X,2F15.4,F15.3/
C    $       6X,32HWRC-ADJUSTED PKS ABOVE BASE  --  ,
C    $       10X,2H--,2X,2F15.4,F15.3)
    9 FORMAT(  /,' SYSTEMATIC PKS',
     $         /,'   ABOVE BASE            ---     ---   ',
     $           F11.4,F12.4,F11.3,
     $         /,' BULL.17B-ADJ PKS',
     $         /,'   ABOVE BASE            ---     ---   ',
     $           F11.4,F12.4,F11.3)
   19 FORMAT(  /,' SYSTEMATIC PKS',
     $         /,'   ABOVE BASE            ---     ---   ',
     $           F11.4,F12.4,F11.3,
     $         /,' EMA-ADJ PKS',
     $         /,'   ABOVE BASE            ---     ---   ',
     $           F11.4,F12.4,F11.3)
   10 FORMAT(    ' SYSTEMATIC RECORD',F10.1,F11.4,F11.4,F12.4,F11.3
     $         /,' BULL.17B ESTIMATE',F10.1,F11.4,F11.4,F12.4,F11.3)
   11 FORMAT(    ' SYSTEMATIC RECORD',F10.1,F11.4,F11.4,F12.4,F11.3
     $         /,' EMA ESTIMATE     ',F10.1,F11.4,F11.4,F12.4,F11.3)
   15 FORMAT(///,'    ANNUAL FREQUENCY CURVE -- DISCHARGES',
     $           ' AT SELECTED EXCEEDANCE PROBABILITIES',
     $        //,'      ANNUAL                            ',
     $           '  ''EXPECTED   ',I2,'-PCT CONFIDENCE LIMITS',
     $         /,'   EXCEEDANCE     BULL.17B    SYSTEMATIC',
     $           ' PROBABILITY''  FOR BULL. 17B ESTIMATES',
     $         /,'   PROBABILITY    ESTIMATE      RECORD  ',
     $           '   ESTIMATE        LOWER        UPPER',  /  )
   16 FORMAT(///,'    ANNUAL FREQUENCY CURVE -- DISCHARGES',
     $           ' AT SELECTED EXCEEDANCE PROBABILITIES',
     $        //,'      ANNUAL                            ',
     $           '  ''EXPECTED       ',I2,'-PCT CONFIDENCE',
     $         /,'   EXCEEDANCE       EMA       SYSTEMATIC',
     $           ' PROBABILITY'' LIMITS FOR EMA ESTIMATES',
     $         /,'   PROBABILITY    ESTIMATE      RECORD  ',
     $           '   ESTIMATE        LOWER        UPPER',  /  )
   20 FORMAT(1X,F11.4,  5A   )
 1010 FORMAT('1',//)
 2011 FORMAT ( 1X, F11.4, 1X, '         -- ',
     $         2X, '(', F6.2, '-year flood below base' )
 2012 FORMAT ( 1X, F11.4, 1X, F12.1,
     $         2X, '(', F6.2, '-year flood )' )
C 203 FORMAT(1X,F12.1)
C
C     + + + END SPECIFICATIONS + + +
C
C     PRINT FITTED LOG-PEARSON TYPE III FREQUENCY CURVES PARAMETERS
C     AND ORDINATES
      WRITE(MSG,1010)
      CALL PRTPHD ( 2001, -999, EMAOPT, WDMSFL )
      WRITE(MSG,8)
      IF(IDEBUG.GT.0) THEN
        IF(EMAOPT.EQ.0) THEN
          WRITE(MSG,9) SYSAAV, SYSASD, SYSASK,
     $                 WRCAAV,WRCASD,WRCASK
        ELSE
          WRITE(MSG,19) SYSAAV, SYSASD, SYSASK,
     $                  WRCAAV,WRCASD,WRCASK
        END IF
      END IF
      SYSBAS = 10.**SYSBAS
      WRCBAS = 10.**WRCBAS
C
      IF (EMAOPT.EQ.0) THEN
C       original B-17 estimates
        WRITE(MSG,10)SYSBAS,SYSPAB,SYSUAV,SYSUSD,SYSSKW,
     $               WRCBAS,WRCPAB,WRCUAV,WRCUSD,WRCSKW
        WRITE(MSG,15) INT( CLSIZE*100. + .5)
      ELSE
C       new EMA estimates
        WRITE(MSG,11)SYSBAS,SYSPAB,SYSUAV,SYSUSD,SYSSKW,
     $               WRCBAS,WRCPAB,WRCUAV,WRCUSD,WRCSKW
        WRITE(MSG,16) INT( CLSIZE*100. + .5)
      END IF
C
      IF(INITIP .EQ. 0) THEN
        CALL MOVEI(INDXPT,IPLIST,NINDX)
        CALL SORTI(IPLIST,NINDX)
        INITIP = 1
      ENDIF
C
C     fill in table, 4 significant digits, 1 decimal place and --
C     for no entries.  DECCHX replaced intermal writes to get
C     significant digits - aml 8/93
Ckmf  list arrays
Ckmf  write (99,3001) (iplist(i),10**wrcfc(i),10**sysrfc(i),
Ckmf $                 10**climl(i),10**climu(i), i = 1, mxint)
C3001 format ( 'PRTFIT:',/,'    iplist      wrcfc       sysrfc  ',
Ckmf $         '    climl       climu   ',
Ckmf $       / ( I8, 4X, 4f12.4 ) )
      LEN = 13
      SIGDIG = 4
      DECPLA = 1
      DO 210 I = 1,NINDX
        DO 201 J = 1,5
          DWORK(J) = '          -- '
  201   CONTINUE
        J = IPLIST(I)
        PEP = TXPROB(J)
        IF(PEP.LE.SYSPAB) THEN       
C         WRITE(DWORK(2),203) 10.**SYSRFC(J)
          TMP = 10.**SYSRFC(J)
CPRH          TMP = EXP(SYSRFC(J))
          CALL DECCHX (TMP,LEN,SIGDIG,DECPLA,DWORK(2))
          IF (DWORK(2)(13:13) .EQ. ' ') DWORK(2)(13:13) = '0'
C         IF(PEP.LE.WRCPAB) THEN      
C         changed 5/94 by aml in consultation with wt and wk
          IF(PEP.LT.WRCPAB) THEN
C           WRITE(DWORK(1),203) 10.**WRCFC(J)
            TMP = 10.**WRCFC(J)
CPRH            TMP = EXP(WRCFC(J))
            CALL DECCHX (TMP,LEN,SIGDIG,DECPLA,DWORK(1))
            IF (DWORK(1)(13:13) .EQ. ' ') DWORK(1)(13:13) = '0'
            IF(NOEPFC.NE.1) THEN
C              WRITE(DWORK(3),203)  10.**EPFC(J)
               IF (EPFC(J) .LT. 11.0) THEN
C                number not to big for space
c            write(99,*) 'PRTFIT: J,EPFC ',J,EPFC(J)
                 TMP = 10.**EPFC(J)
                 CALL DECCHX (TMP,LEN,SIGDIG,DECPLA,DWORK(3))
                 IF (DWORK(3)(13:13) .EQ. ' ') DWORK(3)(13:13) = '0'
               END IF
            END IF
            IF(NOCLIM.NE.1) THEN      
C             WRITE(DWORK(4),203) 10.**CLIML(J)
              TMP = 10.**CLIML(J)
CPRH              TMP = EXP(CLIML(J))
              CALL DECCHX (TMP,LEN,SIGDIG,DECPLA,DWORK(4))
              IF (DWORK(4)(13:13) .EQ. ' ') DWORK(4)(13:13) = '0'
C             WRITE(DWORK(5),203) 10.**CLIMU(J)
              TMP = 10.**CLIMU(J)
CPRH              TMP = EXP(CLIMU(J))
              CALL DECCHX (TMP,LEN,SIGDIG,DECPLA,DWORK(5))
              IF (DWORK(5)(13:13) .EQ. ' ') DWORK(5)(13:13) = '0'
            END IF
          END IF
          WRITE(MSG,20)  PEP, DWORK
        END IF
  210 CONTINUE
C
Ckmf  Oct 3, 2000, in consultation with wrk
Ckmf  call added to compute and print extra n-year floods
Cprh  updated 11/03 for batch version of PEAKFQ that uses EMA method
Cprh  calls to QEXTRA not needed since EMA calculates these intervals
Cprh      TMP = 1.5
Cprh      CALL QEXTRA ( TMP, XTRPK )
Cprh      IF (XTRPK .GT. 0) THEN
Cprh        WRITE (MSG,2012) 1./TMP, XTRPK, TMP
Cprh      ELSE
Cprh        WRITE (MSG,2011) 1./TMP, TMP
Cprh      END IF
Cprh      TMP = 2.33
Cprh      CALL QEXTRA ( TMP, XTRPK )
Cprh      IF (XTRPK .GT. 0) THEN
Cprh        WRITE (MSG,2012) 1./TMP, XTRPK, TMP
Cprh      ELSE
Cprh        WRITE (MSG,2011) 1./TMP, TMP
Cprh      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ALIGNP
     #                 (IPKPTR, IPKSEQ, NPKS, NHIST, SYSPP)
C
C     + + + PURPOSE + + +
C     ALIGNS SYSTEMATIC-RECORD PROBABILITY-PLOTTING POSITIONS RETURNED
C     BY WCFAGB WITH CORRESPONDING SYSTEMATIC-RECORD DISCHARGES IN THE
C     SEQUENTIALLY ORDERED ARRAY OF INPUT SYSTEMATIC AND HISTORIC PEAKS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPKS, NHIST
      INTEGER   IPKPTR(NPKS), IPKSEQ(NPKS)
      REAL                                  SYSPP(NPKS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPKPTR - ARRAY OF POINTERS FROM RANKED-PEAK LIST TO
C              INPUT-SEQUENTIAL-ORDERED LIST.  IPKPTR CAN BE SET BY
C              CALLING SUBRTNE  SORTM.
C     IPKSEQ - ARRAY OF INPUT-SEQUENCE IDENTIFIERS. HISTORIC PEAKS
C              HAVE  NEGATIVE  VALUES, SYSTEMATIC ONES, POSITIVE.
C     NPKS   -
C     NHIST  -
C     SYSPP  -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IH, IS, I
C
C     + + + END SPECIFICATIONS + + +
C
      IF(NHIST.GT.0)  THEN      
        IH = 0
        IS = 0
        DO 150 I = 1,NPKS
          IF(IPKSEQ(IPKPTR(I)).GT.0) THEN       
            IS = IS+1
            SYSPP(I) = SYSPP(NHIST+IS)
          ELSE
            IH = IH+1
            SYSPP(I) = -1.
            IF(IH.GE.NHIST)  GO TO 160
          END IF
  150   CONTINUE
      END IF
C
  160 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   INPUT
     I              (IA1,IA3,INFORM,MAXPKS,EMAOPT,WDMSFL,IBCPUN,
     M               ISTART,
     O               STAID,PKSABG,IWYSN,XQUAL,IQUAL, 
     O               NHIST,NSYS,HISTPD,  QHIOUT,QLWOUT,GAGEB,
     O               GENSKU, RMSEGS,ISKUOP,  NSKIP1,  IRC )
C
C     + + + PURPOSE + + +
C     RE-WRITTEN FOR PRIME VERSION 3.8-P,  WK, 7/88.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IA1,IA3, INFORM, MAXPKS, EMAOPT, WDMSFL, NHIST, 
     &          NSYS, ISKUOP, NSKIP1, IRC, ISTART, IBCPUN
      INTEGER                     IWYSN(MAXPKS),  IQUAL(MAXPKS)
      REAL       PKSABG(MAXPKS)
      REAL       HISTPD, QHIOUT, QLWOUT, GAGEB, GENSKU, RMSEGS
      CHARACTER*(*)  STAID , XQUAL(MAXPKS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IA1   -
C     IA3   -
C     INFORM -
C     MAXPKS -
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number of input WDM file
C     IBCPUN - Additional output indicator,
C              0 - none
C              1 - WDM attributes
C              2 - Watstore BCD file
C              3 - Both WDM and BCD
C              4 - Tab-separated file
C              5 - Both WDM and Tab-separated
C     ISTART -
C     STAID  -
C     PKSABG -
C     IWYSN  -
C     XQUAL  -
C     IQUAL  -
C     NHIST  -
C     NSYS   -
C     HISTPD -
C     QHIOUT -
C     QLWOUT -
C     GAGEB  -
C     GENSKU -
C     RMSEGS  -
C     ISKUOP -
C     NSKIP1 -
C     IRC    -
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*90   IDSTA
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL   INPUT1, INPUT2, INPUT3
C
C     + + + END SPECIFICATIONS + + +
C
      IRC = 0
      NSKIP1 = 0
C
C
      IF ( INFORM .LE. 0 )  THEN   
C       do nothing
C
      ELSE IF ( INFORM .EQ. 1 )  THEN
        CALL INPUT1(IA1, IA3, IBCPUN,
     I              MAXPKS, STAID, PKSABG, IWYSN, XQUAL,
     O              NHIST, NSYS, HISTPD, QHIOUT, QLWOUT, GAGEB, GENSKU,
     O              RMSEGS,ISKUOP, NSKIP1, IRC)
C
      ELSE IF ( INFORM .EQ. 2 )  THEN
        CALL INPUT2(IA1, MAXPKS, EMAOPT, WDMSFL,
     M              ISTART,
     O              STAID, PKSABG, IWYSN, XQUAL, IQUAL, 
     O              NHIST, NSYS, HISTPD, QHIOUT, QLWOUT, GAGEB,
     O              GENSKU, RMSEGS, ISKUOP, NSKIP1, IRC)
C
      ELSE IF ( INFORM .EQ. 3 ) THEN
        CALL INPUT3(  MAXPKS, IDSTA,PKSABG, IWYSN, NHIST,NSYS,HISTPD,
     $     QHIOUT,QLWOUT,GAGEB,GENSKU,RMSEGS,ISKUOP,  NSKIP1, IRC)
        WRITE( STAID, '(7X,A12,1X,A52)') IDSTA(1:12), IDSTA(13:64)
        DO 80 I = 1,(NHIST+NSYS)
          XQUAL(I) = ' -- '
 80     CONTINUE
C
      ELSE
        STOP 233
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INPUT2
     I                 (  MESSFL, MAXPKS, EMAOPT, WDMSFL,
     M                    ISTART,
     O                    STAID, PKSABG, IWYSN, XQUAL, IQUAL, 
     O                    NHIST, NSYS, HISTPD, QHIOUT, QLWOUT, GAGEB,
     O                    GENSKU, RMSEGS, ISKUOP, NSKIP1, IRC )
C
C     + + + PURPOSE + + +
C     GETS INPUT DATA FROM WATSTORE PEAK-FILE PUNCHED-CARD RETRIEVAL
C
C     + + + HISTORY + + +
C     ORIGINALLY WRITTEN AS INPUT3 FOR VECTOR-FORMAT PEAK FILE DATA. WK 5/79.
C     REV  1/81 WK - FOR B-17-B - TO PASS RMSEGS DATA.
C     RE-WRITTEN AS INPUT2 FOR PRIME VERSION 3.8.   WK, 7/88.
C     Updated for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAXPKS, NHIST, NSYS, ISKUOP, NSKIP1, IRC, ISTART
      INTEGER   MESSFL,         IWYSN(MAXPKS) , IQUAL(MAXPKS)
      INTEGER   EMAOPT, WDMSFL
      REAL      PKSABG(MAXPKS)
      REAL      HISTPD, QHIOUT, QLWOUT, GAGEB, GENSKU, RMSEGS
      CHARACTER*(*) XQUAL(MAXPKS)
      CHARACTER*(*) STAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of AIDE message file
C     ISTART - flag 1st time = 0, else > 0.
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA
C     WDMSFL - FORTRAN unit number of input WDM file
C     MAXPKS - MAX NUMBER OF PEAKS THAT CAN BE STORED IN DATA ARRAYS
C     STAID  - CHARACTER STRING STATION ID NO AND NAME --
C              1-15 = 15-DIGIT STATION ID NO.  (8-DIGIT D.S. ORDER NO,
C              RIGHT JUST.)
C              16-20 = AGENCY CODE
C              21-78 = STATION NAME, LEFT JUSTIFIED.
C              79-90 = USED BY J407.
C     PKSABG - FLOOD PEAK DISCHARGES --
C              HISTORICAL VALUES IN FIRST NHIST (IF ANY)
C              FOLLOWED BY NSYS SYSTEMATIC PKS.
C     IWYSN  - WATER-YEARS OR SEQUENCE NUMBERS OF PKSABG PEAKS--
C              WATER-YRS OR SEQ NOS OF HISTORIC PEAKS ARE NEGATIVE-VALUED
C              VALUES FOR SYSTEMATIC PKS ARE POSITIVE.
C     XQUAL  - QUALIFICATION CODES FOR PKSABG  --   CHARACTER
C     IQUAL  - QUALIFICATION CODES FOR PKSABG  --   INTEGER
C     NHIST  - NUMBER  OF HISTORIC peaks returned
C     NSYS   - number of systematic peaks
C     HISTPD - LENGTH OF HISTORIC PERIOD
C     QHIOUT - USER-SET HIGH- OUTLIER DISCHARGE THRESHOLDS
C     QLWOUT - USER-SET low-outlier discharge threshold
C     GAGEB  - GAGE BASE DISCHARGE
C     GENSKU - GENERALIZED SKEW
C     RMSEGS - RMS ERROR OF GENERALIZED SKEW
C     ISKUOP - GEN.SKEW OPTION -- 1= GEN SKU, 0=WTD SKU, -1= STA SKU.
C     NSKIP1 - NUMBER OF STATIONS SKIPPED BECAUSE OF INPUT ERRORS
C     IRC    - RETURN CODE - 0=NO ERROR, 1=ERRORS, 2=END OF FILE, 3=BOTH
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'clunit.inc'
      INCLUDE 'cjobop.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL      AUX(13),  FLAT, FLONG, GAGEBT, XSYSPK,XHSTPK
      LOGICAL   BIT(15) ,  NOHIST,  REJECT
      CHARACTER*1   LQCODE(6)
      CHARACTER*4  LREG
Cprh      CHARACTER*15 CD
      CHARACTER*18 CURSTA
      INTEGER   MSG, NOBS, IBEGYR, IENDYR, IHOPTI, IKROPT, I, IBEGIN,
     &          IEND, IPK, LOOPBK, OKFG
Cprh                , K, L15, IRET, SCLU, SGRP,
Cprh     $          IVAL(2), CVAL(3), L3, L2, L7, L4, L1, L8,
Cprh     $          L6, L9, L10
C
C     + + + FUNCTIONS + + +
      INTEGER   IBITEX
      REAL      WCFGSM
      LOGICAL   DOSTATION
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMAX1, ABS, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   PKFRD4, WCFGSM, IBITEX, LFTSTR
      EXTERNAL   DOSTATION, PARSESTASPECS
Cprh      EXERNAL    Q1EDIT, Q1INIT, QGETR
Cprh      EXTERNAL   QSETR, QSETI, QGETI, QSETCO, QGETCO, QSTCTF
Cprh      EXTERNAL   ZSTCMA, QSETRB, QGETRB
C
C     + + + DATA INITIALIZATIONS + + +
      DATA          LQCODE   /'D','L','K','H','G','X'/
C
C     + + + FORMATS + + + 
  593 FORMAT(/' *** INPUT2 - HISTORIC PEAKS OVERFLOWED -',2I6,
     $        3X, 30A1//' *** SKIPPING FOR NEXT STATION.')
  403 FORMAT(/' *** INPUT2 - PEAK COUNT EXCEEDS STORAGE CAPACITY',
     $       I9,2X,30A1//' *** SKIPPING FOR NEXT STATION')
  486 FORMAT(/' *** INPUT2 - REQUESTED YEARS NOT IN RECORD',4I8,3X,
     $        30A1/  /' *** SKIPPING FOR NEXT STATION.')
  493 FORMAT(I4,'-',I4)
C
C     + + + END SPECIFICATIONS + + +
C
      MSG = MSG1
Cprh      L1 = 1
Cprh      L2 = 2
Cprh      L3 = 3
Cprh      L4 = 4
Cprh      L6 = 6
Cprh      L7 = 7
Cprh      L8 = 8
Cprh      L9 = 9
Cprh      L10= 10
Cprh      L15= 15
      NSKIP1 = 0
      NSYS   = 0
      NHIST  = 0
Cprh      SCLU   = 121
C
  100 CONTINUE
        LOOPBK = 0
        CALL PKFRD4( INCRD, MSG1, MAXPKS, EMAOPT, WDMSFL,
     M               ISTART, STAID, AUX, NOBS, PKSABG(21), 
     M               IQUAL(21), IWYSN(21), IRC, 
     O               XSYSPK, XHSTPK)
C       note PKSABG are offset for space for historic peaks
C
        IF (IRC.LT.2)  THEN  
C         not end of file so process
C
          IF(IRC.EQ.1)  NSKIP1 = NSKIP1 + 1
C
          CURSTA = TRIM(STAID(1:15))
          IF (ALLSOM .EQ. 2) THEN
C           check to see if this is a station user wanted
            OKFG = 0
C           call to DOSTATION will update argument CURSTA with an index 
C           if multiple instances of this station are encountered
            IF (DOSTATION(ISTART,CURSTA)) OKFG = 1
Cprh            DO 110 K = 1,20
Cprh              CD = DOSTA(K)
Cprh              CALL LFTSTR(L15,CD) 
Cprh              IF (CS .EQ. CD) OKFG = 1                     
Cprh 110        CONTINUE
          ELSE
C           do them all
            OKFG = 1
          END IF
C
          IF(NOBS+20.GT.MAXPKS .OR. OKFG .EQ. 0) THEN        
C           too many peaks or 
            IF (OKFG .EQ. 1) WRITE(MSG,403)NOBS, STAID(1:30)
            NSKIP1 = NSKIP1 + 1
            LOOPBK = 1
C
          ELSE
Cprh            IF (IMODFG .EQ. 1) THEN
CprhC             user wants to modify options
Cprh              CALL ZSTCMA (16,1)
Cprh              SGRP = 67
Cprh 120          CONTINUE
Cprh                CALL Q1INIT (MESSFL, SCLU, SGRP)
Cprh                IVAL(1) = INT(AUX(7))
Cprh                IVAL(2) = INT(AUX(8))
Cprh                CALL QSETI (L2,IVAL)       
Cprh                CVAL(2) = INT(AUX(11))+ 1
Cprh                CVAL(1) = INT(AUX(9)) + 2
Cprh                CALL QSETCO (L2,CVAL)
Cprh                CALL QSETRB (L1,L1,AUX(2))
Cprh                CALL QSETRB (L2,L2,AUX(4))
Cprh                CALL QSETRB (L2,L4,AUX(12))
Cprh                CALL QSETRB (L1,L6,AUX(3))
Cprh                CALL QSETRB (L1,L7,XHSTPK)
Cprh                CALL QSETRB (L1,L8,XSYSPK)
Cprh                IF (AUX(1).LT.-9999.0 .AND. AUX(12).GT.0.0 .AND.
Cprh     $            AUX(13).GT.0.0) THEN
Cprh                  AUX(1)  = WCFGSM(AUX(12),AUX(13))
Cprh                END IF
Cprh                IF (AUX(1).LT.-998.0) THEN
Cprh                  CALL QSETRB (L1,L9,-999.0)
Cprh                ELSE
Cprh                  CALL QSETRB (L1,L9,AUX(1))
Cprh                END IF
Cprh                CALL QSETRB (L1,L10,AUX(6))
Cprh                CALL QSTCTF (L3,L15,STAID(1:15))
Cprh                CALL Q1EDIT (IRET)
Cprh              IF (IRET .EQ. -1) GO TO 120
Cprh              CALL QGETI (L2, IVAL)
Cprh              AUX(7) = REAL(IVAL(1))
Cprh              AUX(8) = REAL(IVAL(2))
Cprh              CALL QGETCO (L2,CVAL)
Cprh              AUX(9) = REAL(CVAL(1)-2)
Cprh              AUX(11)= REAL(CVAL(2)-1)  
Cprh              CALL QGETRB (L1,L1,AUX(2))
Cprh              CALL QGETRB (L2,L2,AUX(4))
Cprh              CALL QGETRB (L2,L4,AUX(12))
Cprh              CALL QGETRB (L1,L6,AUX(3))
Cprh              CALL QGETRB (L1,L9,AUX(1))
Cprh              IF (AUX(1) .LT. -998.0) THEN
Cprh                AUX(1) = -1.01E29         
Cprh              END IF
Cprh              CALL QGETRB (L1,L10,AUX(6))
Cprh              CALL ZSTCMA (16,0)
Cprh            ELSE
Cprh              IRET = 1
Cprh            END IF
C
Cprh            IF (IRET .NE. 7) THEN
C             not too many peaks & user wants to continue
              GENSKU    =  AUX(1)
              HISTPD    =  AUX(2)
              QHIOUT    =  AUX(3)
              QLWOUT    =  AUX(4)
              GAGEB     =  AUX(5)
              RMSEGS    =  AUX(6)
              IBEGYR    =  AUX(7)            
              IENDYR    =  AUX(8)             
              IHOPTI    =  AUX(10)
              ISKUOP    =  AUX(9)      
              IKROPT    =  AUX(11)
              FLAT      =  AUX(12)
              FLONG     =  AUX(13)
C
              IF( GENSKU  .LT. -9999.9)  GENSKU  = WCFGSM(FLAT,FLONG)

C             update specs
              CALL PARSESTASPECS (CURSTA,XSYSPK,XHSTPK,
     M                            GENSKU,HISTPD,QHIOUT,QLWOUT,
     M                            GAGEB,RMSEGS,IBEGYR,IENDYR,
     M                            ISKUOP,IKROPT,FLAT,FLONG)
              
C 
              NOHIST = HISTPD.LE.0. .AND. QHIOUT.LE.0. .AND. IHOPTI.LE.0
              GAGEBT= 0.
C
C             find first and last years of record
              IF(IENDYR.LE.0) IENDYR = 9999
              IF(IWYSN(20+NOBS).LT.IBEGYR. OR. IWYSN(21).GT.IENDYR)
     $          GO TO 485
C
                DO 470 I = 1, NOBS
                  IF(IWYSN(20+I).GE.IBEGYR) GO TO 475
  470           CONTINUE
                GO TO 485
C
  475         CONTINUE
                IBEGIN = 20 + I
                DO 480 I=1,NOBS
                  IF(IWYSN(21+NOBS-I).LE.IENDYR) GO TO 490
  480           CONTINUE
C
  485         CONTINUE
                WRITE(MSG,486)IBEGYR,IENDYR,IWYSN(21),IWYSN(20+NOBS),
     $                        STAID(1:30)
                NSKIP1  = NSKIP1 + 1
                LOOPBK = 1
C
  490         CONTINUE
C 
              IF (LOOPBK .EQ. 0) THEN
C               first/last year within record continue processing
                IEND = 21 + NOBS - I
                WRITE(STAID(69:78), 493)IWYSN(IBEGIN), IWYSN(IEND)
C
C               select peaks for input
                DO 590  IPK = IBEGIN, IEND
C                 examine quality codes
                  DO 510 I = 1,15
                    BIT(  I ) =  IBITEX(IQUAL(IPK), I) .NE. 0
  510             CONTINUE
                  LREG = ' '
                  IF( BIT(3)) LREG(1:1) = LQCODE(1)
                  IF(BIT( 8)) LREG(1:1) = LQCODE(5)
                  IF(BIT( 8).AND.BIT(3)) LREG(1:1) = LQCODE(6)
                  IF(BIT(4))  LREG(2:2) = LQCODE(2)
                  IF(BIT(6).OR.BIT(12)) LREG(3:3) = LQCODE(3)
                  IF(BIT(7))  LREG(4:4) = LQCODE(4)
                  XQUAL(IPK) = LREG
C
C                 act on codes
                  REJECT = PKSABG(IPK).LT.0. .OR. BIT(3) .OR. BIT( 8)
     $                    .OR.((BIT(6).OR.BIT(12)) .AND. IKROPT.LE.0)
                  REJECT = REJECT .OR. (BIT(7).AND.NOHIST)
                  REJECT = REJECT .OR. (BIT(7).AND.
     $                     PKSABG(IPK).LT.(QHIOUT+0.5)
     $                     .AND. IHOPTI.LE.0)
                  REJECT = REJECT .OR. (BIT(7) .AND. HISTPD.LE.0)
                  IF( REJECT ) THEN
                    PKSABG(IPK) = -( ABS(PKSABG(IPK))+1E-4 )
C
                  ELSE
                    IF(BIT(4)) GAGEBT= AMAX1(GAGEBT,PKSABG(IPK))
C
C                   move historic peaks
                    IF(BIT(7)) THEN
                      NHIST = NHIST + 1
                      IWYSN (NHIST) =  -IWYSN  (IPK)
                      PKSABG(NHIST) =  PKSABG  (IPK)
                      XQUAL (NHIST) =  XQUAL   (IPK)
                      IQUAL (IPK) = -999
C                     CODE IQUAL = -999 TO DENOTE MOVED HIST PEAK
                    END IF   
                  END IF  
  590           CONTINUE
C
                IF(GAGEB.LE.0.) GAGEB = GAGEBT
C  
                IF(NHIST.GT. IBEGIN) THEN      
                  WRITE(MSG,593) NHIST, IBEGIN, STAID(1:30)
                  NSKIP1 = NSKIP1 + 1
                  LOOPBK = 1
C 
                ELSE
C                 CLOSE UP PEAKS FOR RETURN TO INPUT
                  DO 630 IPK = IBEGIN, IEND
                    IF(IQUAL(IPK).NE.-999) THEN      
                      NSYS = NSYS + 1
                      XQUAL  (NHIST+NSYS) = XQUAL  (IPK)
                      PKSABG (NHIST+NSYS) = PKSABG (IPK)
                      IWYSN  (NHIST+NSYS) = IWYSN  (IPK)
                    END IF
  630             CONTINUE
                END IF
C
              END IF
Cprh            ELSE
CprhC             user did an intrpt
Cprh              LOOPBK = 0
Cprh              IRC = 2
Cprh            END IF
          END IF 
C
        IF (LOOPBK .EQ. 1) GO TO 100
C     
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PKFRD4
     I                  ( IN,MSG, MXPKS, EMAOPT, WDMSFL,
     M                    ISTART, STAID, AUX, NPKS, PKQ,
     M                    IPKQ, IWYEAR, IER,
     O                    XSYSPK, XHSTPK)
C
C     + + + PURPOSE + + +
C     Reads annual-peak discharge CARD-format retrieval data from
C     AMDAHL/WATSTORE peak retrieval PGM.  It also will handle
C     WATSTORE/J407 I-CARDS that immediately follow the Y-CARD.
C
C     IT IS ASSUMED THAT THE RETRIEVAL WAS DONE WITH RETRIEVAL
C     OPTIONS IN THE M CARD AS FOLLOWS --
C       M-COL 47 = H -- HEADER RECORDS
C       M-COL 48 = 3 -- TYPE-3 ANNUAL-PEAK RECORDS
C
C     IF AN ERROR IS FOUND, THIS ROUTINE AUTOMATICALLY SKIPS TO
C     THE START OF THE NEXT STATION.   THUS IT ALWAYS RETURNS WITH
C     A RECORD GOOD FOR PROCESSING, EXCEPT AT THE END OF THE FILE
C     (IER .GE. 2).
C
C     Bad cards (not Y,Z,N,H,I,2,3, or 4 on column 1) are reported
C     on a temporary file (UNIT 91) and are appended to the output
C     file.
C
C     + + + HISTORY + + +
C     Updated 4/05 for batch version of PEAKFQ,
C     show station being processed and 
C     then only the cards that were ignored
C     (i.e. don't show text explaining which cards are ignored)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IN, MSG, MXPKS, NPKS, IER, EMAOPT, WDMSFL, ISTART
      INTEGER   IPKQ(MXPKS),  IWYEAR(MXPKS)
      REAL      PKQ(MXPKS),  AUX(*), XSYSPK, XHSTPK
      CHARACTER*(*)  STAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IN,MSG - LOGICAL UNIT NOS FOR INPUT AND MESSAGES
C     EMAOPT - indicator flag for performing EMA analysis
C              0 - no, just do traditional J407
C              1 - yes, run EMA 
C     WDMSFL - FORTRAN unit number for input WDM file
C     ISTART - initially 1, then incremented
C     MXPKS  - MAX ALLOWABLE NUMBER OF PEAKS TO BE RETRIEVED
C     STAID  - STATION IDENT -- 15-DIGIT STA NO, AGENCY CODE, NAME
C     AUX    - VECTOR OF AUX DATA, DIM 13 --
C     NPKS   - NUMBER OF PEAKS ACTUALLY RETRIEVED
C     PKQ    - THE PEAK DISCHARGES  ( -888 = BLANK IN FILE.)
C     IPKQ   - PEAK DISCHARGE QUALIFICATION CODES FROM WATSTORE
C              STORED AS OCTAL-CODED DECIMAL INTEGER AS FOLLOWS --
C                WATSTORE CODE    IPKQ-VALUE
C                   1, 2, 3         1, 2, 4
C                   4, 5, 6        10, 20, 40
C                   7, 8, 9       100, 200, 400
C                   A, B, C      1000, 2000, 4000
C                   D, E, F     10000, 20000, 40000
C              IPKQ = SUM OF IPKQ-VALUES.   (USE ROUTINE IBITEX TO
C              EXTRACT INDIVIDUAL BITS.)
C     IWYEAR - INTEGER ARRAY OF WATER YEARS.
C     IER    - ERROR CODE 0=NONE, 1=SKIP, 2=END OF FILE, 3=BOTH.
C     XSYSPK - highest systematic peak
C     XHSTPK - lowest historic peak
C
C     + + + SAVES + + +
      SAVE  CARD
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 CARD
      CHARACTER*48 NAME
      CHARACTER*5 AGENCY
      CHARACTER*15 STANO
      CHARACTER*9  PEAKQ
      CHARACTER*1 QCODE(14), COLD
      INTEGER   IBITS(14), I2, AGAIN
      INTEGER   I, J, MONTH, MINYR, MAXYR !, L15
      REAL    FLAT, FLON, FS, FM
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, REAL, LEN
C
C     + + + FUNCTIONS + + +
      INTEGER   IBITOX
C
C     + + + EXTERNALS + + +
      EXTERNAL   IBITOX, PRTPHD
      EXTERNAL   LFTSTR, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        QCODE    /'1','2','3','4','5','6','7','8','9',
     $      'A','B','C','D','E' /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (    ' ***PKFRD4 - PEAK OVERFLOW.  NPKS,MAX =', 2I5 )
Cprh 2001 FORMAT (    ' CARD types 4, 2, and * are ignored',
Cprh     $         /, 1X, A )
 2001 FORMAT ( A )
Cprh 2002 FORMAT (    ' Unrecognized CARD type.',
Cprh     $            '  Must be Y, Z, N, H, I, 2, 3, 4,  or *.',
Cprh     $         /, ' (2, 4, and * records are ignored.)',
Cprh     $         /, 1X, A )
 2002 FORMAT ( A )
 2003 FORMAT (    '  **PKFRD4 - Error reading input lat. or long.',
     $            ' on H card.',
     $         /, 1X, A )
 2004 FORMAT (    ' ***PKFRD4 - Error reading I-card',
     $         /, 1X, A )
 2005 FORMAT (    '  **PKFRD4 - Error reading a 3 card.',
     $         /, 1X, A )
 2010 FORMAT (//, ' ***PKFRD4 - Insufficient data to process, only',
     $            I2, ' peaks for station ', A )
 2100 FORMAT ( /  ' For the station below, the following',
     $            ' records were ignored:')
 2110 FORMAT ( /  ' FINISHED PROCESSING STATION:  ',A,/)
C
C     + + + END SPECIFICATIONS + + +
C
C     This routine requires station id on each card and at least 3
C     peak flow values.  Except for the first call, CARD is processed
C     as the last input from the previous call.  At least one N,Z or H
C     card is required to establish the station number.    
C
C     indicate which station is being processed
      WRITE(91,2100)
C
      IER = 0
      NPKS = 0
Cprh      L15  = 0
C     set Z,N,H card flags
      DO 6 I = 2,13
        AUX(I) = 0.
 6    CONTINUE
      AUX(1) = -1.01E29
      XHSTPK = 1.0E29
      XSYSPK = 0.0
      AGENCY = '     '	
      NAME = '                                                '
      I2 = LEN(STAID)
      IF (I2 .GT. 0) THEN
        DO 7 I = 1,LEN(STAID)
          STAID(I:I) = ' '
 7      CONTINUE
      END IF  
C
      IF (ISTART .EQ. 0) THEN
C       first call, no CARD value to process, so read one
 9      CONTINUE
          READ(IN, '(A)', END=998) CARD
Ckmf      left-shift station number
Ckmf      CALL LFTSTR ( L15, CARD(2:16) )
          IF (CARD(1:1).EQ.'Z'.OR.CARD(1:1).EQ.'Y'.OR.
     $        CARD(1:1).EQ.'N'.OR.CARD(1:1).EQ.'H'.OR.
     $        CARD(1:1).EQ.'I'.OR.CARD(1:1).EQ.'3')   THEN
            AGAIN = 0
            CALL ZLJUST( CARD(2:16) )
          ELSE IF (CARD(1:1).EQ.'4'.OR.CARD(1:1).EQ.'2'.OR.
     $             CARD(1:1).EQ.'*') THEN
C           skip partial duration peak, 2, and comment records
            WRITE (91,2001) CARD
            AGAIN = 1
          ELSE
C           unrecognized card type
            WRITE (91,2002) CARD
            AGAIN = 1
          END IF
        IF (AGAIN .EQ. 1) GO TO 9  
        ISTART = ISTART + 1
      END IF
      STANO = CARD(2:16)
C
      IF(CARD .NE. '$EOF') THEN       
C       last CARD not end of file
        MINYR = 2020
        MAXYR = 0
C
  10    CONTINUE
          IF(CARD(1:1) .EQ. 'Z') THEN
            AGENCY = CARD(33:37)
C
          ELSE IF(CARD(1:1) .EQ. 'H' ) THEN      
            STANO = CARD(2:16)
            READ(CARD(17:22),'(3F2.0)',ERR=20) FLAT,FM,FS
            AUX(12) = FLAT+FM/60.+FS/3600.
            READ(CARD(23:29),'(F3.0,2F2.0)',ERR=20) FLON,FM,FS
            AUX(13) = FLON +FM/60.+FS/3600.
C
            GO TO 21
 20         CONTINUE
C             Error reading input lat. or long. on H card.
              WRITE (MSG,2003) CARD
 21         CONTINUE
C
          ELSE IF(CARD(1:1).EQ.'N') THEN      
            NAME = CARD(17:64)
            STANO = CARD(2:16)
C
          ELSE IF(CARD(1:1) .EQ. 'Y') THEN     
C           do nothing
C
          ELSE IF (CARD(1:1) .EQ. 'I') THEN
C           READ OPTIONAL I-CARD
            IF(CARD(2:16).EQ.'               '
     $        .OR. CARD(2:16).EQ.STANO) THEN     
              IF(CARD(2:16).EQ.'               ') THEN
C               put stano in blank space
                CARD(2:16) = STANO
              END IF
              READ(CARD,91,ERR=98)(AUX(I),I=1,8)
   91         FORMAT(16X,6F8.0,T71,2F4.0)
              IF(CARD(17:24) .EQ. ' ')  AUX(1) = -1.01E29
              DO 95 I=65,69
                IF(CARD(I:I) .EQ. 'H' )  AUX(10) = 1.1
                IF(CARD(I:I) .EQ. 'G' )  AUX( 9) = 1.1
                IF(CARD(I:I) .EQ. 'S' )  AUX( 9) = -1.1
                IF(CARD(I:I) .EQ. 'K' )  AUX(11) = 1.1
   95         CONTINUE
C
              GO TO 99
   98         CONTINUE
C               error reading an I card
                WRITE (MSG,2004) CARD
   99         CONTINUE
C
            END IF
C  
          ELSE IF(CARD(1:1).EQ.'3') THEN               
            NPKS = NPKS + 1
            IF(NPKS.GT.MXPKS) THEN
C             peak overflow
              IER = 1
              WRITE (MSG,2000) NPKS, MXPKS
            ELSE
              READ(CARD(17:22),'(I4,I2)',ERR=140) IWYEAR(NPKS), MONTH
              IF(MONTH.GE.10) IWYEAR(NPKS) =   IWYEAR(NPKS) + 1
              PEAKQ = CARD(25:31)
              READ(PEAKQ,'(F7.0)',ERR=140)  PKQ(NPKS)
              IF(PEAKQ.EQ.' ')  PKQ(NPKS) =  -8888.
C 
C             EXTRACT PEAK--Q QUAL CODES
              DO 118 I=1,14
                IBITS(I) = 0
  118         CONTINUE
              DO 130 I = 32,43
                IF(CARD(I:I).NE.' ') THEN      
                  DO 120 J = 1,14
                    IF(CARD(I:I).EQ.QCODE(J)) THEN
                      IBITS(J) = 1
                    END IF
  120             CONTINUE
                END IF
  130         CONTINUE
C             find lowest historic and highest systematic
              IF (IBITS(7) .EQ. 1) THEN    
C               historic peak
                IF (XHSTPK .GT. PKQ(NPKS)) XHSTPK = PKQ(NPKS)
              ELSE
C               systematic peak
                IF (XSYSPK .LT. PKQ(NPKS)) XSYSPK = PKQ(NPKS)
              END IF
C             set local start/end of systematic record
              IF (IWYEAR(NPKS) .GT. MAXYR) MAXYR = IWYEAR(NPKS)
              IF (IWYEAR(NPKS) .LT. MINYR) MINYR = IWYEAR(NPKS)
              IPKQ(NPKS) = IBITOX(IBITS,14)
C
              GO TO 141
 140          CONTINUE
C               error reading 3 card
                WRITE (MSG,2005) CARD
 141          CONTINUE
            END IF
          END IF    
C
C         set old card type
          COLD = CARD(1:1)
C         read new card
 150      CONTINUE   
            READ(IN, '(A)', END=998) CARD
Ckmf        left-shift station number
Ckmf        CALL LFTSTR ( L15, CARD(2:16) )
            IF (CARD(1:1).EQ.'Z'.OR.CARD(1:1).EQ.'Y'.OR.
     $          CARD(1:1).EQ.'N'.OR.CARD(1:1).EQ.'H'.OR.
     $          CARD(1:1).EQ.'I'.OR.CARD(1:1).EQ.'3')   THEN       
              AGAIN = 0
              CALL ZLJUST( CARD(2:16) )
            ELSE IF (CARD(1:1).EQ.'4'.OR.CARD(1:1).EQ.'2'.OR.
     $               CARD(1:1).EQ.'*') THEN
C             skip partial duration peak, 2, and comment records
              WRITE (91,2001) CARD
              AGAIN = 1
            ELSE
C             unrecognized card type
              WRITE (91,2002) CARD
              AGAIN = 1
            END IF
          IF (AGAIN .EQ. 1) GO TO 150
C
C         check conditions for looping back to process the new card
        IF (CARD(2:16).EQ.STANO .AND. IER.EQ.0)          GO TO 10
        IF (CARD(1:1).EQ.'I'.AND.STANO.EQ.'               ') 
     &                                                   GO TO 10
        IF (COLD .EQ. 'Z')                               GO TO 10
C
C       must be new station or too many peaks
C
        IF (NPKS .LT. 3) THEN
C         insufficient data to process
          IF (NPKS .GT. 0) THEN
            IF(STANO.EQ.'               ') STANO='unknown        '
            WRITE (MSG,2010) NPKS, STANO
       write (*,*) '3rd call to PRTPHD 1000'
            CALL PRTPHD (1000,1,EMAOPT,WDMSFL)
          END IF
          NPKS  = 0
        ELSE
          STAID = STANO//AGENCY//NAME
        END IF 
C
        IF (NPKS .EQ. 0) GO TO 10
C
      ELSE
C       end of file
        IER = IER + 2
      END IF
C
      GO TO 999
  998 CONTINUE
C       reached end of file
        CARD = '$EOF'
        STAID = STANO//AGENCY//NAME
        IF (NPKS .LT. 5) IER = IER + 2
  999 CONTINUE
C    
C     check if start/end on I card, if not, use systematic record
      IF (ABS(AUX(7)) .LT. 1.0) AUX(7) = REAL(MINYR)
      IF (ABS(AUX(8)) .LT. 1.0) AUX(8) = REAL(MAXYR)
      IF (ABS(AUX(6)) .LT. 0.00001) AUX(6) = 0.55
Cprh      IF (XHSTPK .GT. 1.0E29) XHSTPK = 0.0
      IF (XHSTPK .GE. 1.0E29) XHSTPK = 0.0
C
      WRITE (91,2110) STAID
C
      RETURN
      END
C
C
C
      SUBROUTINE   INPUT3
     #                  ( MAXPKS, IDSTA, PKSABG, IWYSN, NHIST,NSYS,
     $     HISTPD,  QHIOUT,QLWOUT,GAGEB,GENSKU,RMSEGS,ISKUOP,
     $     NSKIP,  IRC )
C
C     + + + PURPOSE + + +
C     INPUT FROM WATSTORE LOG-PEARSON CARD FORMAT
C     -- NOTE -- THIS DOES NOT HAVE FULL ERROR-DETECTION AND OPTION-
C             SETTING CAPABILITIES OF THE WATSTORE J407 VERSION.
C     REWRITTEN FOR PRIME VERSION 3.8-P,  WK, 7/88.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAXPKS, NHIST, NSYS, NSKIP, IRC, ISKUOP
      INTEGER                               IWYSN(MAXPKS)
      CHARACTER*(*)  IDSTA
      REAL    PKSABG(MAXPKS)
      REAL     HISTPD, QHIOUT, QLWOUT, GAGEB, GENSKU, RMSEGS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAXPKS -
C     IDSTA  -
C     PKSABG -
C     IWYSN  -
C     NHIST  -
C     NSYS   -
C     HISTPD -
C     QHIOUT -
C     QLWOUT -
C     GAGEB  -
C     GENSKU -
C     RMSEGS -
C     ISKUOP -
C     NSKIP  -
C     IRC    -
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'clunit.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, NPK
C
C     + + + EXTERNALS + + +
      EXTERNAL   PKFQH3
C
C     + + + END SPECIFICATIONS + + +
C
      NSKIP = 0
  100 CONTINUE
      CALL PKFQH3(INCRDS,MSG,  MAXPKS, IDSTA,GAGEB,GENSKU,ISKUOP,NSYS,
     $       RMSEGS,QLWOUT,QHIOUT,HISTPD,NHIST,PKSABG,IRC)
      IF(IRC.GT.1)  RETURN
      NPK=NHIST+NSYS
      IF(NPK.GT.0) GO TO 120
      WRITE(MSG,111) IDSTA(1:64)
 111  FORMAT(/53H ***INPUT2 - STATION HAS NO PEAK FLOW DATA.  STA-ID = /
     $  12X,A64)
      NSKIP = NSKIP+1
      GO TO 100
  120 CONTINUE
C   ASSIGN WATER-YEAR SEQ NOS.
      IF(NHIST.LT.0)  NHIST = 0
      IF(NHIST.LE.0)  GO TO 150
      DO 130 I=1,NHIST
  130 IWYSN(I) = I-1-NHIST
  150 CONTINUE
      DO 160 I=1,NSYS
  160 IWYSN(NHIST+I) = I
C
      RETURN
      END
C
C
C
      SUBROUTINE   PKFQH3
     #                 ( IN,MSG, MAXPK,IDSTA,GAGEB,GENSKU,IGSOPT,NSYS,
     $             RMSEGS,
     $                  QLWOUT,QHIOUT,HISTPD,NHIST,PKQ,IRC)
C
C     + + + PURPOSE + + +
C     READS LOG-PEARSON-FORMAT FLOOD DATA CARDS + HISTORIC/OUTLIER DATA
C     CARDS FOR  J407 VER 2.0.  THIS VERSION HAS MINIMAL ERROR DETECTION
C     AND RECOVERY.
C     REV 1/16/81 WK. FOR J407 VER 3.5 HISTORIC/OUTLIER/GAGEB + RMSEGS.
C     REV 7/88 WK.  FOR VERS 3.8-P, MOVED 'IN,MSG' TO ARG LIST.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IN, MSG, MAXPK, IGSOPT, NSYS, NHIST, IRC
      REAL      GAGEB, GENSKU, RMSEGS, QLWOUT, QHIOUT, HISTPD
      REAL               PKQ(*)
      CHARACTER*(*)   IDSTA
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1  HH, HG, HS, HW, HN, OPT(5)
      CHARACTER*4  DEND, DEOD
      CHARACTER*8  BLANK
      CHARACTER*10 CID
      INTEGER   I, J, J1, J2, IERNO, IHIOP, NCARD
      REAL    FNHIST
C
C     + + + DATA INITIALIZATION + + +
      DATA HH,HN,HG,HW,HS/'H','N','G','W','S'/
      DATA DEND,BLANK,DEOD/'$END','        ','$EOD'/
C
C     + + + END SPECIFICATIONS + + +
C
      IRC = 0
      CID = '**********'
      IDSTA(1:1)=HH
      IDSTA(2:51)='                                                  '
      IDSTA(52:90)='                                       '
1000  CONTINUE
      GAGEB  =  0.
      QHIOUT = 0.
      QLWOUT = 0.
      HISTPD = 0.
      NHIST = 0
      READ(IN,1) IDSTA(13:64),RMSEGS,GENSKU,OPT,NSYS,IDSTA(1:10)
    1 FORMAT(A52 ,2F5.0,5A1,I3,A10)
      IF(IDSTA(1:10).EQ.CID) GO TO 1000
      IF(IDSTA(13:16).EQ.DEND.OR.IDSTA(13:16).EQ.DEOD) GO TO 970
      IF(NSYS.EQ.0)RETURN
      IERNO = 3
      IF(NSYS.LT.0.OR.NSYS.GT.MAXPK) GO TO 900
      IHIOP  = 0
      IGSOPT = 0
      DO10I = 1,5
      IF(OPT(I).EQ.HH)IHIOP = 1
      IF(OPT(I).EQ.HN)IHIOP = 0
      IF(OPT(I).EQ.HG)IGSOPT = +1
      IF(OPT(I).EQ.HS)IGSOPT = -1
      IF(OPT(I).EQ.HW)IGSOPT =  0
   10 CONTINUE
      IERNO = 5
      NCARD = (NSYS+9)/10
      DO30I = 1,NCARD
      J2  =  10*I + IHIOP*15
      J1 = J2-9
      READ(IN,2)(PKQ(J),J = J1,J2),CID
    2 FORMAT(10F7.0,A10)
      IF(CID(1:10).NE.IDSTA(1:10)) GO TO 900
   30 CONTINUE
      IF(IHIOP.NE.0) GO TO 40
      RETURN
   40 READ(IN,2)GAGEB,QLWOUT,QHIOUT,HISTPD,FNHIST,(PKQ(J),J = 1,5),CID
      NHIST = FNHIST
      IERNO = 203
      IF(NHIST.LT.0.OR.NHIST.GT.15.) GO TO 900
      IERNO = 205
      IF((CID(1:8).NE.IDSTA(1:8))
     $        .AND. (CID(1:8).NE.BLANK(1:8))) GO TO 900
      IF(NHIST.LE.5) GO TO 50
      READ(IN,2)(PKQ(J),J = 6,15),CID
      IF((CID(1:8).NE.IDSTA(1:8))
     $        .AND. (CID(1:8).NE.BLANK(1:8))) GO TO 900
   50 CONTINUE
      DO60I = 1,NSYS
   60 PKQ(NHIST+I)  =  PKQ(15+I)
      RETURN
  900 WRITE(MSG,901)IERNO,CID,IDSTA(1:10)
  901 FORMAT(/37H *** PKFQH3 ERROR AT CARD-ID, STA-ID.,I5,
     $       2(3X,1H-,A10   ,1H-),A10/10X,
     $58HERROR CODES 3,203=INVALID PEAK COUNTS.  5,205=ID MISMATCH./)
      IRC = 1
      GO TO 1000
  970 IRC = IRC+2
      RETURN
      END
C
C
C
      SUBROUTINE   OUTPUT
     #                  (STAID,SYSUAV,SYSUSD,SYSSKW,
     $      WRCUAV,WRCUSD,WRCSKW,WRCFC ,  NHSTPN, NSYS,
     $      IBCPUN, LBCPU, IA1,IA3, PAUSE, JSEQNO)
C
C     + + + PURPOSE + + +
C     WRITES OUTPUT OF J407 RESULTS TO FILE SELECTED BY IBCPUN -
C
C     + + + HISTORY + + +
C     kmf 96/12/17 - changed ibcpu to ibcpun to be consistent,
C                    simplified check for wdm and/or basin char
C     prh 10/2007  - added JSEQNO to indicate when to output 
C                    column headers on tabular output file
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*90 STAID
      REAL      WRCFC(*),SYSUSD, SYSSKW, WRCUAV, WRCUSD, WRCSKW,
     &                SYSUAV
      INTEGER   NHSTPN, NSYS, IBCPUN, LBCPU, IA1, IA3, PAUSE, JSEQNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IBCPUN - indicator flag for writing calculated statistics
C              0 - don't save
C              1 - save as attributes in wdm file
C              2 - save in WATSTORE basin characteristics format
C              3 - save in wdm file (1) and WATSTORE (2)
C              4 - Tab-separated file
C              5 - Both WDM and Tab-separated
C
C     + + + EXTERNALS + + +
      EXTERNAL   OUTPT1, BCFPCH
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IBCPUN .EQ. 1  .OR.  IBCPUN .EQ. 3 .OR. IBCPUN .EQ. 5) THEN
C       save statistics in wdm file
        CALL OUTPT1 (STAID,SYSUAV,SYSUSD,SYSSKW,
     $               WRCUAV,WRCUSD,WRCSKW,WRCFC,NHSTPN, NSYS,
     $               IA1,IA3, PAUSE )
      END IF
Cprh      IF (IBCPUN .EQ. 2  .OR.  IBCPUN .EQ. 3) THEN
      IF (IBCPUN .GT. 1) THEN
C       save statistics in watstore basin characteristics (2 or 3) or
c       in tab-separated (4 or 5) format
        CALL BCFPCH (STAID,SYSUAV,SYSUSD,SYSSKW,
     I               WRCUAV,WRCUSD,WRCSKW,WRCFC,
     I               NHSTPN,NSYS,LBCPU,IBCPUN,JSEQNO)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BCFPCH
     #                 (  STAID,SYSAV,SYSSD,SYSG, WRCAV, WRCSD,
     $                    GWRC, WRCFC, NHSTYR, NSYSYR, IPCH, 
     $                    IBCPUN, JSEQNO)
C
C     + + + PURPOSE + + +
C     PUNCHES J407 RESULTS IN BASIN-CHARACTERISTICS INPUT FORMAT
C     ON LOGICAL UNIT IPCH
C
C     + + + HISTORY + + +
C     prh 10/2007 - updated to output in tabular format (IBCPUN=4 OR 5),
C                   suitable for import to spreadsheet/stats program
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*90  STAID
      REAL         WRCFC(*), SYSAV, SYSSD, SYSG, WRCAV, WRCSD,
     &             GWRC
      INTEGER   NHSTYR, NSYSYR, IPCH, IBCPUN, JSEQNO
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*7 LCHAR(11)
      INTEGER JPUN(11)
      INTEGER VAR (9)
      INTEGER   I, IX, STAIND
      REAL      X, POWER
      CHARACTER*6 VARNAM(18)
C
C      + + + SAVES + + +
       SAVE STAIND
C
C      + + + INTRINSICS + + +
       INTRINSIC  INT, CHAR
C
C     + + + DATA INITIALIZATIONS + + +
Cprh      DATA    JPUN   /12,16,20,21,23,25,26,27,28/
Cprh  updated for inclusion of 1.5 and 2.33 intervals, 11/03
      DATA    JPUN   /12,14,17,18,21,22,24,26,27,28,29/
      DATA    VAR    /75,76,77,78,79,80,81,82,178/
      DATA    VARNAM /'STAID ',' P1.25','  p1.5','   P2.',' p2.33',
     $                '   P5.','  P10.','  P25.','  P50.',' P100.',
     $                ' P200.',' P500.','WRCSKW',' WRCMN',' WRCSD',
     $                ' YRSPK','YRSHPK','STANAM'/
C     init IREC to large to trigger output of headers
      DATA    STAIND /9999/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(A6,9X,A,16(1X,A6,A),A6)
 2010 FORMAT(2A,11(A7,A),3(F7.3,A),2(I7,A),A)
C
C     + + + END SPECIFICATIONS + + +
C
C  ROUND AND CONVERT USING FORMATS AND LCHAR ARRAY
      DO 70 I=1,11
        X=10.**WRCFC(JPUN(I))
        IF(X.LT.99.95 .OR. X.GE.9995000.) GO TO 50
        POWER=1.
   40   IX=(X/POWER)+.5
        IF(IX.LE.1000) GO TO 60
        POWER=10.*POWER
        GO TO 40
   50   WRITE(LCHAR(I) ,   51)X
   51   FORMAT(1F7.1)
        GO TO 70
   60   IX=IX*INT(POWER)
        WRITE(LCHAR(I) ,   61)IX
   61   FORMAT(1I7)
   70 CONTINUE
C
C
      IF (IBCPUN .LT. 4) THEN
C       Watstore Basin Characteristic format
Ckmf    add staion name record ("2" card) Oct 02, 2000
Ckmf    station name is defined as being 21-78, space for 21-62
        WRITE (IPCH,100) STAID(1:15), STAID(21:62)
  100   FORMAT('1',  A15, 4X, A )
C       PUNCH 3 CARDS
        WRITE(IPCH,101)STAID(1:15),(VAR(I),LCHAR(I),I=1,6)
  101   FORMAT('2',  A15,6(I3,1A7))
        WRITE(IPCH, 102)  STAID(1:15),(VAR(I),LCHAR(I),I=7,8),
     $                    SYSAV,SYSSD, SYSG, VAR(9), LCHAR(9)
  102   FORMAT('2',  A15,2(I3,1A7),' 83',F7.3,' 84',F7.3,' 85',F7.3,
     $         I3, 1A7)
        IX = 2
        IF(NHSTYR.GT.NSYSYR) IX = 1
        WRITE(IPCH,   103)STAID(1:15),GWRC,WRCAV,WRCSD ,
     $         NSYSYR, NHSTYR, (' ',I=1,IX)
  103   FORMAT('2',  A15,'179',F7.3, '180',F7.3,'181',F7.3 ,
     $         '196',I7, '197',I7, 2A1,T57, 10X)
      ELSE
C       Tab-separated format
C       only output headers when Station index (JSEQNO)
C       is smaller than previous, or initial, value of STAIND
        IF (JSEQNO .LT. STAIND) THEN
Cprh      headers for attributes to be output
          WRITE (IPCH,2000) (VARNAM(I),CHAR(9),I=1,17),VARNAM(18)
        END IF
C       remember last station index
        STAIND = JSEQNO
C
C       output tab-separated values
        WRITE(IPCH,2010) STAID(1:15),CHAR(9),(LCHAR(I),CHAR(9),I=1,11),
     $                   GWRC,CHAR(9),WRCAV,CHAR(9),WRCSD,CHAR(9),
     $                   NSYSYR,CHAR(9),NHSTYR,CHAR(9),STAID(21:62)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLTFRQ
     #                 (  MSG, HEADNG, IPLTOP, GRFMT,
     $        NPKPLT, PKLOG, SYSPP, WRCPP, WEIBA,
     $        SYSRFC,WRCFC,FCXPG,NPLOT,IWXMOD,HSTFLG,
     $        NOCLIM, CLIML, CLIMU, IPLTNO )
C
C     + + + PURPOSE + + +
C     PRODUCES FREQUENCY-CURVE PLOT
C     USES IPLTOP VALUE TO DETERMINE WHETHER PRINTER-PLOT
C     OR GRAPHICS-DEVICE PLOT.
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IPLTOP, NPKPLT, NPLOT, MSG,IWXMOD,
     &          HSTFLG, NOCLIM, IPLTNO
      CHARACTER*3    GRFMT
      CHARACTER*80   HEADNG(9)
      REAL      PKLOG(*), SYSPP(*), WRCPP(*),  SYSRFC(*), WRCFC(*),
     $     FCXPG(*), WEIBA, CLIML(*), CLIMU(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSG    - LOGICAL UNIT NUMBER FOR  PRINT-PLOT
C     HEADNG - PAGE-HEADING LINES FOR PLOT -- SAME AS ON PRINTOUT
C     IPLTOP -
C     GRFMT  - graphic format (BMP, CGM, or WMF)
C     NPKPLT - NUMBER OF OBSERVED PEAKS TO PLOT
C     PKLOG  - LOG-10 OBSERVED PEAK DISCHARGES
C     SYSPP  - SDYSTEMATIC-RECORD standard deviates  (ENTRIES FOR
C              HISTORIC PEAKS = -large)
C     WRCPP  - WRC-ESTIMATED standard deviates
C     WEIBA  -
C     SYSRFC - LOG-10 ORDINATES OF FITTED CURVE - SYSTEMATIC RECORD
C     WRCRFC - LOG-10 ORDINATES OF FITTED CURVE - WRC ESTIMATED
C     FCXPG  - TABULAR ABSCISSA PROBABILITIES FOR FITTED CURVE
C     NPLOT  - NUMBER OF PLOT POINTS IN FITTED CURVE
C     IWXMOD -
C     HSTFLG - flag to plot historic adjusted peaks, 0-yes, 1-no
C     NOCLIM - flag for confidence limits, 0-available, 1-not available
C     CLIML  - log10 ordinates of fitted curve, lower confidence limits
C     CLIMU  - log10 ordinates of fitted curve, upper confidence limits
C     IPLTNO - sequence number of this station (for identifying plots)
C
C     + + + LOCAL VARIALBES + + +
      INTEGER   IPRTPL, IGKSPL, DEVCOD
      REAL      EPSILN
      CHARACTER*3 PLTEXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   FRQPLG, FRQPLT
      EXTERNAL   GPINIT, GPDEVC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   EPSILN/1.0E-6/
C
C     + + + END SPECIFICATIONS + + +
C
      IGKSPL = MOD(IPLTOP,2)
      IPRTPL = MOD(IPLTOP/2,2)
C
      IF (IPRTPL .EQ. 1 )  THEN
        WRITE(MSG, '(''1''/(25X,A))' ) HEADNG
        IF (ABS(WEIBA) .GT. EPSILN) THEN
          WRITE(MSG,'(85X,A,F6.3,A)') '*** WEIBXXX PLOTTING, WEIBA =',
     $       WEIBA,'***'
        ELSE
          WRITE(MSG,*)' '
        END IF
        CALL FRQPLT(   MSG,
     $              NPKPLT, PKLOG, SYSPP, WRCPP,
     $              SYSRFC,WRCFC,FCXPG,NPLOT,IWXMOD)
      END IF
C
      IF (IGKSPL .EQ. 1)  THEN
Cprh    always generate BMP graphic files
C       set device type and code
        CALL GPDEVC (4,8)
        PLTEXT = 'BMP'
        CALL FRQPLG
     $            (HEADNG,NPKPLT, PKLOG, SYSPP, WRCPP, WEIBA,
     $            NPLOT,SYSRFC,WRCFC,FCXPG,HSTFLG,
     $            NOCLIM, CLIML, CLIMU, IPLTNO, PLTEXT )
        IF (IPLTOP.GT.0 .AND.
     $     (GRFMT.EQ.'CGM' .OR. GRFMT(1:2).EQ.'PS' .OR. 
     $      GRFMT.EQ.'WMF')) THEN !generate graphic metafiles also
C         set device type and code
          IF (GRFMT.EQ.'CGM') THEN
            CALL GPDEVC (4,4)
          ELSE IF (GRFMT(1:2).EQ.'PS') THEN
            CALL GPDEVC (4,6)
          ELSE IF (GRFMT.EQ.'WMF') THEN
            CALL GPDEVC (4,9)
          END IF
          CALL FRQPLG
     $              (HEADNG,NPKPLT, PKLOG, SYSPP, WRCPP, WEIBA,
     $              NPLOT,SYSRFC,WRCFC,FCXPG,HSTFLG,
     $              NOCLIM, CLIML, CLIMU, IPLTNO, GRFMT )
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FRQPLT
     #                  ( MSG , NPKS,PLGSRT,PPOS1,PPOSHW,QSTA,
     $                   QHW,XPRB,NPLT,IWXMOD)
C
C     + + + PURPOSE + + +
C     FREQUENCY PLOT OBSERVED AND COMPUTED PEAKS.
C     REV 12/83 FOR PRIME -- WK.
C     6/28 FORCE AT LEAST 1 LOG CYCLE PLOT RANGE
C     6/22 CALL J407SC NOT PPPSCL TO GET PRETTY LOG SCALE UNITS 1 OR 5
C     6/17  CALL J407P4 INSTEAD OF PLOT4 TO PRINT NATURAL UNITS PEAKS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MSG, NPKS, NPLT, IWXMOD
      REAL       PLGSRT(*),PPOS1(*),PPOSHW(*),
     #           XPRB(*),QSTA(*),QHW(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSG    -
C     NPKS   -
C     PLGSRT -
C     PPOS1  -
C     PPOSHW -
C     QSTA   -
C     QHW    -
C     XPRB   -
C     NPLT   -
C     IWXMOD -
C     + + + COMMON BLOCKS + + +
C
C     + + + LOCAL VARIABLES + + +
      INTEGER NSCLE(5)
      INTEGER  IVGRID(13)
      CHARACTER*1  PLTSYM(4)
      INTEGER   NVGRID, JJ, KK, J, I, MM
      REAL      PLTBAS, PP0, PP1, PQMAX, PQMIN, QMIN, QMNP,
     &          QMAX
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMIN1, AMAX1
C
C     + + + FUNCTIONS + + +
C
C     + + + EXTERNALS + + +
      EXTERNAL   J407P4, PLOT3, PLOT22, PLOT3N, PLOT2, PLOT1
      EXTERNAL   J407SC, OMIT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA PLTBAS/0./, PLTSYM / '*', 'O', '$', '#'/
      DATA NSCLE / 1, 0, 1, 0, 0/
      DATA  IVGRID/6,19,26,35, 41,51,61,67,76,83,91,96,101/
      DATA PP1,PP0/-2.5762,+2.8785/, NVGRID/13/
C
C     + + + FORMATS + + +
  163 FORMAT(/' ',12X,'99.5 99.0',9X,'95.0   90.0     80.0  70.0',
     #  6X,'50.0      30.0  20.0     10.0    5.0     2.0  1.0  0.5   ',
     #    '0.2'/35X,'ANNUAL EXCEEDANCE PROBABILITY, PERCENT   ',
     $         '(NORMAL SCALE)'  )
C
C     + + + END SPECIFICATIONS + + +
C
C  FIND SMALLEST NONTRIVIAL COMPUTED PCTILES
      DO70JJ=1,NPLT
      IF(QSTA(JJ).GT.-10.)GOTO71
   70 CONTINUE
      JJ=NPLT
   71 DO75KK=1,NPLT
      IF(QHW(KK).GT.-10.)GOTO76
   75 CONTINUE
      KK=NPLT
   76 CONTINUE
      QMIN=AMIN1(QSTA(JJ),QHW(KK),PLGSRT(NPKS))
      QMAX=AMAX1(QSTA(NPLT),QHW(NPLT),PLGSRT(1))
      QMNP=AMAX1(QMIN,PLTBAS)
C  FORCE PLOT RANGE GE 1 LOG CYCLE
      IF(QMAX-QMNP.GE.1.)GOTO80
      IF(QMAX.LE.8.5)QMAX=QMAX+1.
      IF(QMAX.GT.8.5)QMNP=QMNP-1.
   80 CONTINUE
      CALL J407SC(QMAX,QMNP,5,       PQMAX,PQMIN)
      IF(QMIN.LT.PQMIN)WRITE(MSG,62)
   62 FORMAT(' ***FRQPLT WILL DROP POINTS BELOW PLOT BASE.')
C
C
      CALL  PLOT1(NSCLE,5,10,1,106)
C    NOTE -- DUMMY ARG IS REQUIRED  IN ARG LIST BUT NOT USED --
      CALL PLOT2(  PP0,  PP1,  PQMAX ,   PQMIN)
      CALL PLOT22(NVGRID,IVGRID)
      J=8
      I=5
      CALL PLOT3N('*****  NOTICE  *****  NOTICE  ******',I+0,J,36)
      IF(IWXMOD.NE.2)
     $CALL PLOT3N('* PRELIMINARY MACHINE COMPUTATION. *',I+1,J,36)
      IF(IWXMOD.EQ.2)
     $CALL PLOT3N('* EXPERIMENTAL NON-17B COMPUTATION.*',I+1,J,36)
      CALL PLOT3N('*  USER IS RESPONSIBLE FOR ASSESS- *',I+2,J,36)
      CALL PLOT3N('*     MENT AND INTERPRETATION.     *',I+3,J,36)
      CALL PLOT3N('************************************',I+4,J,36)
      I=12
      J=10
      CALL PLOT3N('      PLOT SYMBOL KEY         ',I+0,J,30)
      CALL PLOT3N('_  17B FINAL FREQUENCY CURVE  ',I+1,J,30)
      CALL PLOT3N('_  OBSERVED (SYSTEMATIC) PEAKS',I+2,J,30)
      CALL PLOT3N('_  HISTORICALLY ADJUSTED PEAKS',I+3,J,30)
      CALL PLOT3N('_  SYSTEMATIC-RECORD FREQ CURVE',I+4,J,31)
      CALL PLOT3N('WHEN POINTS COINCIDE, ONLY THE',I+5,J,30)
      CALL PLOT3N('TOPMOST SYMBOL SHOWS.         ',I+6,J,30)
      DO 190 MM=1,4
  190 CALL PLOT3N(PLTSYM(MM),I+MM,J,1)
      CALL PLOT3(PLTSYM(4),XPRB(JJ),QSTA(JJ),NPLT-JJ+1)
      CALL PLOT3(PLTSYM(3),PPOSHW,PLGSRT,NPKS)
      CALL PLOT3(PLTSYM(2),PPOS1,PLGSRT,NPKS)
      CALL PLOT3(PLTSYM(1),XPRB(KK),QHW(KK),NPLT-KK+1)
      CALL OMIT(1)
      CALL J407P4(41,'     ANNUAL PEAK MAGNITUDES   /LOG SCALE/')
      CALL OMIT(0)
      WRITE(MSG,163)
      RETURN
      END
C
C
C
      SUBROUTINE   J407P4
     #                 (NL,LABEL)
C
C     + + + PURPOSE + + +
C     SPECIAL VERSION OF PRPLOT FOR PGM J407.   76-06-16 WK.
C                             REVISED FOR PRIME 12/83 WK.
C
C     PRINTS NATURAL UNITS ON VERTICAL LOG SCALE OF J407
C     LOG-PROBABILITY PRINTER PLOT.
C     ORDINATE PRINT CALCULATION IS SET UP FOR 1 DECIMAL PLACE (F12.1)
C
C     USGS PROG NO B524 - CCD USER SERVICES
C      PETE SMIDINGER   SUMMER 1966  MATH & COMP BR  GSFC  NASA
C     REVISION 730604 WKIRBY USGS-WRD ACTIVATE PLTAPE ENTRY AND
C           REPLACE DSRN = 6 (CONSTANT)  BY VARIABLE IJTAPE
C     REVISION 8/24/78 WK - PUT IJTAPE VARIABLE IN COMMON PRPCOM FOR USE
C             BY OTHER PRPLOT ROUTINES.
C     ADDED PLOT3Z ENTRY TO PLOT SYMBOL VECTOR.  WK 1/80.
C
C     REV 4/83 WK FOR      *** P R I M E  ***    *** P R I M E ***
C     REV 4/83 WK - ENTRY PLOT22 INSERTS IRREGULARLY SPACED VERTICAL GRID
C          LINES SUCH AS FOR PROBABILITY PAPER.
C     REV 8/83 WK -- RESET KPLOT1 AT 210 TO RESTORE DEFAULT GRID.
C     REV 12/83 WK -- ENTRY PLOT3N PUTS A NOTE AT SPECIFIED LINE/COLUMN OF
C          PLOT-IMAGE AREA.
C     ********************************************************************
C     ***
C     ***     NOTE THAT IMAGE AREA NOW IS A   L O C A L   VARIABLE
C     ***      NOT SUPPLIED BY USER.   BUILT-IN MAX PLOT SIZE IS
C     ***       7381 CHARACTERS, INCLUDING HORIZ AND VERT AXES.
C     ***      THIS IS ENOUGH FOR 61 MAX-LENGTH LINES OF 121 CHARS.
C     ***
C     ***        FIRST ARG OF PLOT2 ENTRY STILL MUST BE SUPPLIED
C     ***         BY USER, BUT IT MAY BE ANY DUMMY VARIABLE...
C     ***         IT'S IGNORED BY THE SUBROUTINE.
C     ***
C     ********************************************************************
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NL, NSCALE(5), NHL, NSBH, NVL, NSBV, NVGD2,
     &          IVGD2(*), LSW, JLINE, ICOL, N3, ITAPE
      REAL      XMAX, XMIN, YMAX, YMIN, X(*), Y(*)
      CHARACTER*(*) LABEL, CH
C
C     + + + ARGUMENT DEFINITION + + +
C     NL     -
C     LABEL  -
C
C     + + + COMMON BLOCKS + + +
      COMMON / PRPCOM /  IJTAPE
      INTEGER            IJTAPE
C
C     **** P R I M E **** REVISION  --  SAVE
C     + + + SAVE + + +
Ckmf  SAVE
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*1  IMAGE(7381)
      REAL                  ABNOS(26)
      CHARACTER*1 NOS(10), VC, HC,NC,BL,HF,HDOT,HCOMMA,WL
      CHARACTER*22   FOR1, FOR2, FOR3
      REAL      TENTO(10)
      INTEGER   IITAPE, NH, NSH, NV, NSV,
     &          NSCL, NVP, NDH, NDHP, NDV, NDVP, NIMG, IY, IX,
     &          NA, NS, NB, I1, I2, J, I3, I, NN, J1, J2,
     &          J3, ICHINC, ICH, NX, NY
      LOGICAL   KPLOT1, KPLOT2, KTAPEI, KABSC, KORD, KBOTGL, KPLOT,
     &          KNHOR
      REAL       SQRT10, FSY, FSX, YMX, DH, DV, XMIN1,
     &           YMIN1, DUM1, DUM2, ORDNO, ORDPRT
Ckmf  add saves for all local variables
      SAVE      IMAGE, ABNOS, NOS, VC, HC, NC, BL, HF, HDOT, HCOMMA, WL,
     $          FOR1, FOR2, FOR3,
     $          TENTO,
     $          IITAPE, NH, NSH, NV, NSV,
     $          NSCL, NVP, NDH, NDHP, NDV, NDVP, NIMG, IY, IX,
     $          NA, NS, NB, I1, I2, J, I3, I, NN, J1, J2,
     $          J3, ICHINC, ICH, NX, NY,
     $          KPLOT1, KPLOT2, KTAPEI, KABSC, KORD, KBOTGL, KPLOT,
     $          KNHOR,
     $          SQRT10, FSY, FSX, YMX, DH, DV, XMIN1,
     $          YMIN1, DUM1, DUM2, ORDNO, ORDPRT
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, MIN0, INT, IABS, FLOAT, ABS
C
C     + + + ENRTY POINTS + + +
C     PLOT1, PLOT22, PLOT3, PLOT4, FPLOT4, OMIT, PLTAPE, PLOT2, PLOT3N
C     PLOT3Z
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TENTO/1.,10.,100.,1E3,1E4,1E5,1E6,1E7,1E8,1E9/
      DATA SQRT10/3.16/
      DATA NOS   /'0','1','2','3','4','5','6','7','8','9'/
     $     ,HC/'-'/,NC/'+'/,BL/' '/
     $         , HF/'F'/,HDOT/'.'/,HCOMMA/','/
      DATA         VC/'|'/
      DATA  FOR1 / '(1X,A1,F12.?,1X,121A1)' /
      DATA  FOR2 / '(1X,A1,13X,121A1 )    ' /
      DATA  FOR3 / '(1H0,F  . ,  F   . )  ' /
C                   1...5....11...5....21..
C
      DATA IITAPE / 96 /
      DATA KPLOT1 /. FALSE./, KPLOT2/.FALSE./   , KTAPEI/.FALSE./
      DATA  KABSC,KORD,KBOTGL /3*.FALSE./
C
C     + + + END SPECIFICATIONS + + +
C
      GO TO 400
C
      ENTRY PLOT1(NSCALE,NHL,NSBH,NVL,NSBV)
      KPLOT1=.TRUE.
      KPLOT2=.FALSE.
      NH=IABS(NHL)
      NSH=IABS(NSBH)
      NV=IABS(NVL)
      NSV=IABS(NSBV)
      NSCL=NSCALE(1)
 125  CONTINUE
      IF(.NOT.KTAPEI) IJTAPE=IITAPE
      KTAPEI=.TRUE.
      IF(NH*NSH*NV*NSV.NE.0) GO TO 128
      WRITE(IJTAPE,       14 )
14    FORMAT(T5,'SOME PLOT1 ARG. ILLEGALLY 0')
      KPLOT=.FALSE.
      RETURN
128   KPLOT=.TRUE.
      IF(NV.LE.25) GO TO 126
      WRITE(IJTAPE,         12 )
      KPLOT=.FALSE.
12    FORMAT(T5,'NO. OF VERTICAL LINES >25')
      RETURN
126   CONTINUE
      NVP=NV+1
      NDH=NH*NSH
      NDHP=NDH+1
      NDV=NV*NSV
      NDVP=NDV+1
      NIMG=(NDHP*NDVP)
      IF(NDV.LE.120) GO TO 130
      KPLOT=.FALSE.
      WRITE(IJTAPE,        11 )
11    FORMAT(T5,'WIDTH OF GRAPH >121')
      RETURN
130   CONTINUE
      IF(NDHP*NDVP.LE. 7381 )   GO TO 135
      WRITE(IJTAPE,      133) NDHP,NDVP
  133 FORMAT(' *** PRPLOT -- GRAPH DIMENSIONS TOO BIG -- ',2I6)
      KPLOT = .FALSE.
      RETURN
  135 CONTINUE
      IF(NSCL.EQ.0) GO TO 70
      FSY=10.**NSCALE(2)
      FSX=10.**NSCALE(4)
      IY=MIN0(IABS(NSCALE(3)),7)+1
      IX=MIN0(IABS(NSCALE(5)),9)+1
      GO TO 75
70    FSY=1.
      FSX=1.
      IY=4
      IX=4
75    FOR1(12:12)=NOS(IY)
      NA=MIN0(IX,NSV)-1
      NS=NA-MIN0(NA,120-NDV)
      NB=11-NS+NA
      I1=NB/10
      I2=NB-I1*10
      FOR3(7:7)=NOS(I1+1)
      FOR3(8:8)=NOS(I2+1)
      FOR3(10:10)=NOS(NA+1)
      IF(NV.GT.0) GO TO 90
      DO 80 J=11,18
80    FOR3(J:J)=BL
      GO TO 100
 90    I1=NV/10
      I2=NV-I1*10
      FOR3(11:11)=HCOMMA
      FOR3(12:12)=NOS(I1+1)
      FOR3(13:13)=NOS(I2+1)
      FOR3(14:14)=HF
      I1=NSV/100
      I3=NSV-I1*100
      I2=I3/10
      I3=I3-I2*10
      FOR3(15:15)=NOS(I1+1)
      FOR3(16:16)=NOS(I2+1)
      FOR3(17:17)=NOS(I3+1)
      FOR3(18:18)=HDOT
      FOR3(19:19)=FOR3(10:10)
100   IF(KPLOT1) RETURN
      KPLOT1=.TRUE.
C
      ENTRY PLOT2(XMAX,XMIN,YMAX,YMIN)
      KPLOT2=.TRUE.
      IF(KPLOT1) GO TO 210
      NSCL=0
      NH=5
      NSH=10
      NV=10
      NSV=10
       GO TO 125
210   CONTINUE
      KPLOT1 = .FALSE.
      IF(.NOT.KPLOT)RETURN
      YMX=YMAX
      DH=(YMAX-YMIN)/FLOAT (NDH)
      DV=(XMAX-XMIN)/FLOAT(NDV)
      DO 220 I=1,NVP
220   ABNOS(I)=(XMIN+FLOAT((I-1)*NSV)*DV)*FSX
      DO 225 I=1,NIMG
225   IMAGE(I)=BL
      DO 240 I=1,NDHP
      I2=I*NDVP
      I1=I2-NDV
      KNHOR=MOD(I-1,NSH).NE.0
      IF(KNHOR) GO TO 230
      DO 228 J=I1,I2
228   IMAGE(J)=HC
230   CONTINUE
      DO 240 J=I1,I2,NSV
      IF(KNHOR) THEN       
        IMAGE(J)=VC
      ELSE
        IMAGE(J)=NC
      END IF       
240   CONTINUE
      XMIN1=XMIN-DV/2.
      YMIN1=YMIN-DH/2.
      RETURN
C
      ENTRY  PLOT22 (NVGD2, IVGD2)
C       INSERTS IRREGULARLY SPACED VERTICAL GRID LINES SUCH AS
C        PROBABILITY PAPER.
      IF(KPLOT2) GO TO 265
      IF(.NOT.KTAPEI) IJTAPE = IITAPE
        KTAPEI = .TRUE.
      WRITE(IJTAPE,13)
      KPLOT = .FALSE.
      RETURN
  265 CONTINUE
      DO 268 NN = 1,NVGD2
      J1 = IVGD2(NN)
      J2 = (NDHP-1)*NDVP + J1
      J3 = J1 - 1
      DO 268 J = J1,J2,NDVP
  268 IMAGE(J) = IMAGE(J-J3)
      RETURN
C
C
      ENTRY PLOT3N(CH,JLINE,ICOL,N3)
      ICHINC = -1
      GO TO 300
      ENTRY PLOT3(CH,X,Y,N3)
      ICHINC=0
      GO TO 300
      ENTRY  PLOT3Z(CH, X, Y, N3)
      ICHINC=1
300   IF(KPLOT2) GO TO 312
      IF(.NOT.KTAPEI) IJTAPE=IITAPE
      KTAPEI=.TRUE.
  301 WRITE(IJTAPE,       13 )
13    FORMAT(T5,'PLOT2 MUST BE CALLED')
312   CONTINUE
      IF(.NOT.KPLOT) RETURN
      IF(N3.GT.0) GO TO 314
      KPLOT=.FALSE.
      WRITE(IJTAPE,       15 )
15    FORMAT(T5,'PLOT3 ARG2 < 0')
      RETURN
 314  CONTINUE
      IF(ICHINC.LT.0) GO TO 350
      ICH = 1 - ICHINC
      DO 320 I=1,N3
      ICH = ICH + ICHINC
      DUM1=(X(I)-XMIN1)/DV
      DUM2=(Y(I)-YMIN1)/DH
      IF(DUM1.LT.0..OR.DUM2.LT.0.) GO TO 320
      IF(DUM1.GE.NDVP.OR.DUM2.GE.NDHP) GO TO 320
      NX=1+INT(DUM1)
      NY=1+INT(DUM2)
      J=(NDHP-NY)*NDVP+NX
      IMAGE(J)=CH(ICH:ICH)
320   CONTINUE
      RETURN
350   ICHINC = (JLINE-1)*NDVP + ICOL - 1
      DO 355 I = 1,N3
 355  IMAGE(ICHINC+I) = CH(I:I)
      RETURN
C
C
  400 CONTINUE
      ENTRY PLOT4(NL,LABEL)
      ENTRY FPLOT4(NL,LABEL)
      IF(.NOT.KPLOT) RETURN
      IF(.NOT.KPLOT2) GO TO 301
      DO 420 I=1,NDHP
      IF(I.EQ.NDHP.AND.KBOTGL) GO TO 420
      WL=BL
      IF(I.LE.NL) WL = LABEL(I:I)
      I2=I*NDVP
      I1=I2-NDV
      IF(MOD(I-1,NSH).EQ.0.AND..NOT.KORD) GO TO 410
      WRITE (IJTAPE,      FOR2) WL,(IMAGE(J),J=I1,I2)
      GO TO 420
410   CONTINUE
Ckmf  added .00001 to YMX because pc rounded float(i-1)*dh to slightly
Ckmf  larger than YMX (when they should have been equal), causing ordno
Ckmf  to be incorrectly negative in some cases.
Ckmf  ORDNO=(YMX-FLOAT(I-1)*DH)*FSY
      ORDNO=(YMX+0.00001 - FLOAT(I-1)*DH) * FSY
C *** J407 ***
C CVT ORDNO (LOG) TO ROUNDED NATURAL FOR PRINTING
Caml  changed fron E38 to E29 for 5/94 fortran complier
      ORDPRT = 1E29
      IF(ORDNO.GE.10. .OR. ORDNO.LT.-1.2) GO TO 419
      J = ORDNO + .001
      IF(ORDNO .LT. 0.)  J = J - 1
      ORDPRT = TENTO(IABS(J)+1)
      IF(J.LT.0) ORDPRT = 1./ORDPRT
      IF(ABS(ORDNO-J).GT.0.1) ORDPRT = ORDPRT*SQRT10
      IF(ORDNO.GE.5.3) ORDPRT = ORDPRT + 0.05
  419 ORDNO = ORDPRT
C     WRITE (*,*) "J407P4:IJTAPE:",IJTAPE," ORDNO:",ORDNO,
C    $            " I1,2:",I1,I2," WL:'",WL,"'"
C     WRITE (*,*) "       FOR1:'",FOR1,"' IMAGE:",(IMAGE(J),J=I1,I2)
      WRITE (IJTAPE,FOR1) WL,ORDNO,(IMAGE(J),J=I1,I2)
420   CONTINUE
C     jlk: Removal of the next debug write causes fatal error on lahey 
C          unless compiled with no optimization (-o0)
C     WRITE (*,*) "J407P4:KABSC:'",KABSC,"' NVP:",NVP
C     IF (KABSC) GO TO 430
      IF (KABSC .EQV. .FALSE.) THEN
        WRITE (IJTAPE,FOR3) (ABNOS(J),J=1,NVP)
      END IF
C430  CONTINUE
C     WRITE (*,*) "J407P4:PLOT4:Return"
      RETURN
C
      ENTRY OMIT(LSW)
      KABSC=MOD(LSW,2).EQ.1
      KORD=MOD(LSW,4).GE.2
      KBOTGL=LSW.GE.4
      RETURN
C
      ENTRY PLTAPE(ITAPE)
      IJTAPE = ITAPE
      KTAPEI = .TRUE.
      RETURN
      END
C
C
C
      SUBROUTINE   J407SC
     #                  (XTOP,XBOT,NGRID,GTOP,GBOT)
C
C     + + + PURPOSE + + +
C     SPECIAL VERSION OF PPPSCL FOR J407 LOG PLOT PRODUCES ONLY
C     PRETTY SCALE FACTORS OF 1. AND 5. (NOT 2.).             WK 760622
C     WK 9/23/76 ALLOW SCALE FACT 2 WHEN SCALE IS GT 1.
C     *** REVISED FOR PRIME ***  WK 12/83
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XTOP, XBOT, GTOP, GBOT
      INTEGER   NGRID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTOP   -
C     XBOT   -
C     NGRID  -
C     GTOP   -
C     GBOT   -
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL NOSWIT
      INTEGER   MSG, M
      REAL      X, XMX, XMN, U, UGLY, TOL, TEMP, TENTOM, SMALL, SCALE,
     &          GRANGE, GMX, GMN, FNGRID,EXCESS, BIG
C
C     + + + INTRINSIC + + +
      INTRINSIC   SIGN, INT, AMIN1, AMAX1,ALOG10, AINT, ABS
C
C     + + + FUNCTIONS + + +
      REAL   STEP, ZINT, FLOOR,  CEIL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TOL /.005/
      DATA MSG/6/
C
C    + + + END SPECIFICATIONS + + +
C
C***************************************
C  STATEMENT FUNCTIONS
      STEP(X) = .5 + SIGN(.5,X)
      ZINT(X) = AINT(X) - 1. + STEP(X)
      FLOOR(X) = ZINT(X+TOL)
      CEIL(X) = ZINT(X+1.-TOL)
C***************************************
      XMX=AMAX1(XTOP,XBOT)
      XMN=AMIN1(XTOP,XBOT)
      NOSWIT=XTOP.GT.XBOT
      FNGRID=NGRID
      UGLY=(XMX-XMN)/FNGRID
      U=0.
      M=0
      IF(ABS(UGLY).LE.0.) GO TO 11
      U = ALOG10(ABS(UGLY))
      M=INT(U)
      IF(U.LT.0.) M = M - 1
      U = 10**(U-M)
   11 CONTINUE
      IF(U.LT.1.+TOL)U=10.*U
      TENTOM=UGLY/U
      U=U/(1.+TOL)
      SCALE=5.
      IF(U.LT.2. .AND. UGLY.GT.1.) SCALE=2.
      IF(U.GT.5.)SCALE=10.
      SCALE=SCALE*TENTOM
C  20 CONTINUE
      GMX=CEIL(XMX/SCALE)
      GMN=FLOOR(XMN/SCALE)
      GRANGE=GMX-GMN
      IF(GRANGE.LE.FNGRID)GOTO100
      IF(ABS((SCALE/TENTOM)-5.) .LT. 1.)GOTO80
      GMX=CEIL(2.*XMX/SCALE)/2.
      GMN=FLOOR(2.*XMN/SCALE)/2.
      GRANGE=GMX-GMN
      IF(GRANGE.LE.FNGRID)GOTO100
   80 CONTINUE
      SCALE=2.*SCALE
      IF(SCALE.LT.(5.*TENTOM))SCALE=(5.*TENTOM)
      GMX=CEIL(XMX/SCALE)
      GMN=FLOOR(XMN/SCALE)
      GRANGE=GMX-GMN
      IF(GRANGE.LE.FNGRID)GOTO100
      WRITE(MSG,98)XTOP,XBOT,GMX,GMN,SCALE
   98 FORMAT(/' ***PRPSCL098 LOGIC ERROR',1P5E16.6/)
      GBOT=XBOT
      GTOP=XTOP
      RETURN
  100 CONTINUE
      EXCESS=FNGRID-GRANGE
      SMALL=AINT(EXCESS/2.)
      BIG=EXCESS-SMALL
      IF((GMX*SCALE-XMX) .GT. (XMN-GMN*SCALE))GOTO110
      GMX=GMX+BIG
      GMN=GMN-SMALL
      GOTO120
  110 GMX=GMX+SMALL
      GMN=GMN-BIG
  120 CONTINUE
      GTOP=GMX*SCALE
      GBOT=GMN*SCALE
      IF(NOSWIT)GOTO130
      TEMP=GBOT
      GBOT=GTOP
      GTOP=TEMP
  130 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   MOVEI
     #                (ISOURC, IDATA, NITEMS)
C
C     + + + PURPOSE + + +
C     MOVES INTEGER DATA FROM ISOURC TO IDATA, NITEMS ITEMS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NITEMS
      INTEGER   ISOURC(NITEMS),  IDATA(NITEMS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISOURC -
C     IDATA  -
C     NITEMS -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 30 I=1,NITEMS
   30 IDATA(I) = ISOURC(I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SORTI
     #                (IDATA, NITEMS)
C
C     + + + PURPOSE + + +
C     SORTS INTEGER ARRAY IDATA (NITEMS ITEMS).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NITEMS
      INTEGER   IDATA(NITEMS)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, N1, J, N2, ITEMP
C
C     + + + END SPECIFICATIONS + + +
C
      N1 = NITEMS-1
      DO 40 I = 1,N1
      N2  =  NITEMS-I
      DO 20 J = 1,N2
      IF(IDATA(J).LE.IDATA(J+1)) GO TO 20
      ITEMP = IDATA(J+1)
      IDATA(J+1) = IDATA(J)
      IDATA(J) = ITEMP
   20 CONTINUE
   40 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE   SORTM
     #                ( W, IW, KENTER, KOPT, NN )
C
C     + + + PURPOSE + + +
C     SORTS AN ARRAY IN ASCENDING OR DESCENDING ORDER,
C     THE ORIGINAL ORDER IS STORED IN IW
C
C     + + + DUMMY ARGUEMENTS + + +
      INTEGER   KENTER, KOPT, NN
      INTEGER   IW(NN)
      REAL      W(NN)
C
C     + + + ARGUMENT DEFINITION + + +
C     W - ARRAY OF VALUES TO BE SORTED
C     IW - ARRAY OF ORDER POINTERS FOR W
C     KENTER - FLAG FOR IW : 0 - IW ALLREADY CONTAINS POINTERS
C                            1 - IW NEEDS TO BE FILLED
C     KOPT - SORT OPTION : -1 - DECREASING ORDER
C                           1 - ASCENDING ORDER
C     NN - SIZE OF W AND IW ARRAYS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, K, M, N, IT, KASE
      REAL      T
C
C     + +  END SPECIFICATIONS + + +
C
      N = NN
      KASE = 0
      IF (KOPT .EQ. -1) KASE = 1
      IF (KENTER .EQ. 1) THEN
C       FILL IW ARRAY
        DO 5 I = 1, N
          IW(I) = I
 5      CONTINUE
      END IF
      M=N-1
      DO30I=1,M
      K=N-I
      DO20J=1,K
      IF(W(J+KASE).LE.W(J+1-KASE))GOTO20
      T=W(J)
      W(J)=W(J+1)
      W(J+1)=T
      IT=IW(J)
      IW(J)=IW(J+1)
      IW(J+1)=IT
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   IBITOX
     #                         (IBIT,N)
C
C     + + + PURPOSE + + +
C     CONVERTS BIT STRING TO OCTAL-CODED INTEGER.
C     THE LOW-INDEX (FIRST) 3 WORDS OF IBIT CORRESPOND TO THE LOW-
C     ORDER OCTAL DIGIT OF THE RESULTANT FUNCTION VALUE.  THE FUNCTION
C     VALUE IS CODED SUCH THAT IF IT IS PRINTED UNDER AN ORDINARY
C     I FORMAT, THE PRINTED DIGITS WILL BE OCTAL (I.E., 1 2 ... 7).
C     FOR EXAMPLE, BITS  1 0 1 0 1 0 1 1 1 0 0 0 0 0 1
C     WOULD YIELD PRINTED VALUE      40725.
C     WKIRBY 12/87.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N
      INTEGER   IBIT(N)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IBIT  - INTEGER VECTOR, DIMENSION N, EACH ELEMENT
C             IS 0 OR 1. (PGM CHECKS THIS, CONVERTS VALUES .LT. 0 TO 0,
C             VALUES .GT. 1 TO 1.)
C     N     - INTEGER NUMBER OF ELEMENTS IN IBIT.
C             (MAX N DEPENDS ON INTEGER WORD LENGTH.)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, MCODE, MBIT, NN, K
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IBITOX = 0
      IF(N .LE. 0) RETURN
C
      MCODE = 0
      DO 10 I = 1,N
      MBIT = 0
      IF(IBIT(I) .GT. 0) MBIT = 1
      MCODE = MCODE + MBIT*(2**(I-1))
   10 CONTINUE
C
      NN = (N+2)/3
      DO 20 I = 1,NN
      K = MOD(MCODE,8)
      IBITOX = IBITOX + K*(10**(I-1))
      MCODE = MCODE/8
   20 CONTINUE
C
      RETURN
C
      END
C
C
C
      INTEGER   FUNCTION   IBITEX
     #                          (ICTAL, N)
C
C     + + + PURPOSE + + +
C     EXTRACTS BIT NO N OUT OF OCTAL-CODED INTEGER ICTAL
C     REMARKS --
C     THIS FUNCTION IS THE INVERSE OF  IBITOX.
C
C     EXAMPLE --   ICTAL = 40725
C                  IBIT  = IBITEX(ICTAL, N)
C     THEN, FOR N = 1 - 15 --
C     IBIT = 1 0 1 0 1 0 1 1 1 0 0 0 0 0 1
C
C     WARNING -- IF ICTAL HAS DIGITS 8 OR 9 OR IF IT IS NEGATIVE,
C       RESULTS ARE UNPREDICTABLE.   IBITEX DOES NOT CHECK THIS.
C
C                            WK....  7/88.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICTAL, N
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICTAL - INTEGER SCALAR CONSISTING OF OCTAL-CODED DIGITS.
C     N     - INTEGER POSITION OF BIT TO BE EXTRACTED FROM ICTAL,
C             COUNTING FROM THE RIGHT.  (LOW-ORDER BIT = NO 1.)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IOCT, IBIT, IDIGIT
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IDIGIT = (N+2)/3
      IOCT = MOD(ICTAL/10**(IDIGIT-1),10)
      IBIT = N - 3*(IDIGIT-1)
      IBITEX = MOD(IOCT/2**(IBIT-1), 2)
      RETURN
      END
C
C
C
Ckmf  SUBROUTINE   DATTIM
Ckmf #                 ( JDATE, JTIME )
C
C     + + + PURPOSE + + +
C     This routine
C
C     + + + HISTORY + + +
C     kmf - Nov 09, 2000 - replaced by libanne routine
C
C     + + + DUMMY ARGUMENTS + + +
Ckmf  INTEGER    JDATE(3), JTIME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     JDATE  -
C     JTIME  -
C
C     + + + LOCAL VARIABLES + + +
Ckmf  INTEGER *2  STRING(28)
C     INTEGER     NUM
Ckmf  CHARACTER*6  IMAGE
Ckmf  INTEGER   J
C
C     + + + INTRINSICS + + +
Ckmf  INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
Cmyg  EXTERNAL   TIMDAT
C
C     + + + DATA INITIALIZATIONS + + +
C     DATA     NUM / 28 /
Ckmf  DATA  STRING / 28*0/
C
C     + + + END SPECIFICATIONS + + +
C
Cmyg  CALL TIMDAT( STRING, NUM )
Ckmf  WRITE(IMAGE,1) ( STRING(J), J = 1, 3 )
Ckmf  READ(IMAGE,2) ( JDATE(J), J = 1, 3 )
Ckmf  JTIME = ( STRING(4) / 60 ) * 100  +  MOD( STRING(4), 60 )
Ckmf1 FORMAT( 3A2 )
Ckmf2 FORMAT( 3I2 )
Ckmf  RETURN
Ckmf  END
C
C
C
Cprh      SUBROUTINE   OUTPT2
Cprh     I                    ( STAID, WRCUAV, WRCUSD, WRCSKW, WRCFC,
Cprh     I                      MESSFL )
CprhC
CprhC     + + + PURPOSE + + +
CprhC     Send summary of computed statistics to screen for ascii input.
Cprh
CprhC     + + + DUMMY ARGUMENTS + + +
Cprh      INTEGER   MESSFL
Cprh      REAL      WRCUAV, WRCUSD, WRCSKW, WRCFC(*)
Cprh      CHARACTER*90   STAID
Cprh
CprhC     + + + ARGUMENT DEFINITIONS + + +
CprhC     STAID  - station identification number
CprhC     WRCUAV - WRC mean of peaks
CprhC     WRCUSD - WRC standard deviation of peaks
CprhC     WRCSKW - WRC skew of peaks
CprhC     WRCFC  - array of logs of computed peaks
CprhC
CprhC     + + + LOCAL VARIABLES + + +
Cprh      INTEGER      I, J, ORD(9), TXTL(14), TXTFLG,
Cprh     $             GROUP,  SCLU, LEN, RTCMND
Cprh      REAL         PEAKST(12)
Cprh      CHARACTER*1  BLNK, FLAG
Cprh      CHARACTER*71 TXT
CprhC
CprhC     + + + INTRINSICS + + +
Cprh      INTRINSIC   ABS
CprhC
CprhC     + + + EXTERNALS + + +
Cprh      EXTERNAL    ZIPI, ZIPC, CVARAR, LFTSTR
Cprh      EXTERNAL    Q1INIT, QSETCT, QSETR, Q1EDIT
CprhC
CprhC     + + + DATA INITIALIZATIONS + + +
CprhC     return period 1.25  2   5  10  25  50 100 200 500
Cprh      DATA ORD  /    12, 16, 20, 21, 23, 25, 26, 27, 28 /
Cprh      DATA BLNK, FLAG, TXTL,       SCLU
Cprh     $    / ' ',  '*', 15,12*1,45,  121 /
CprhC
CprhC     + + + END SPECIFICATIONS + + +
CprhC
Cprh      TXTFLG = 0
Cprh      I = 71
Cprh      CALL ZIPC ( I, BLNK, TXT )
CprhC
CprhC     compute t-year peaks
Cprh      DO 100 I = 1, 9
Cprh        J = ORD(I)
Cprh        IF (ABS( WRCFC(J) ) .LT. 20.0) THEN
Cprh          PEAKST(I) = 10.0**WRCFC(J)
Cprh        ELSE
CprhC         magnitude of the exponent is too large
Cprh          PEAKST(I) = -999.
Cprh          TXT(I+15:I+15) = FLAG
Cprh          TXTFLG = 1
Cprh        END IF
Cprh  100 CONTINUE
CprhC
CprhC     bulletin 17b mean, sd, and skew of log of Q
Cprh      PEAKST(10) = WRCUAV
Cprh      PEAKST(11) = WRCUSD
Cprh      PEAKST(12) = WRCSKW
CprhC
CprhC     station number
Cprh      TXT(1:15) = STAID(1:15)
CprhC
Cprh      IF (TXTFLG .EQ. 1) THEN
CprhC       problem with one of more of the statistics, include warning
Cprh        TXT(28:71) = 'WARNING:  problem with flagged (*) attributes'
Cprh      END IF
CprhC
CprhC     bulletin 17B estimates to screen
Cprh      GROUP = 55
Cprh      CALL Q1INIT ( MESSFL, SCLU, GROUP )
Cprh      LEN = 12
Cprh      CALL QSETR ( LEN, PEAKST )
Cprh      I = 14
Cprh      LEN = 71
Cprh      CALL QSETCT ( I, TXTL, LEN, TXT )
Cprh      CALL Q1EDIT ( RTCMND )
CprhC
Cprh      RETURN
Cprh      END
C
C
C
Cprh      SUBROUTINE   QEXTRA
Cprh     I                    ( XYEAR,
Cprh     O                      PEAK )
CprhC
CprhC     + + + PURPOSE + + +
CprhC     For the specified recurrence interval, calculate the
CprhC     corresponding peak.
CprhC
CprhC     + + + DUMMY ARGUMENTS + + +
Cprh      REAL      XYEAR, PEAK
CprhC
CprhC     + + + ARGUMENT DEFINTIONS + + +
CprhC     XYEAR  - recurrence interval, in years
CprhC     PEAK   - flood peak corresponding to XYEAR recurrence
CprhC
CprhC     + + + PARAMETERS + + +
Cprh      INCLUDE 'pmxint.inc'
CprhC
CprhC     + + + COMMON BLOCKS + + +
Cprh      INCLUDE 'cwcf1.inc'
CprhC
CprhC     + + + LOCAL VARIABLES + + +
Cprh      REAL   Q, QBAS
CprhC
CprhC     + + + FUNCTIONS + + +
Cprh      REAL   HARTK
CprhC
CprhC     + + + EXTERNALS + + +
Cprh      EXTERNAL   HARTIV, HARTK
CprhC
CprhC     + + + END SPECIFICATIONS + + +
CprhC
Cprh      CALL HARTIV ( WRCSKW, WORK )
Cprh      Q    = WRCUAV + WRCUSD * HARTK ( (1.-1./XYEAR), WORK )
Cprh      QBAS = WRCUAV + WRCUSD * HARTK ( 1.-WRCPAB, WORK )
CprhC
Cprh      IF (Q .LT. QBAS ) THEN
Cprh        PEAK = -999.
Cprh      ELSE
Cprh        PEAK = 10**Q
Cprh      END IF
CprhC
Cprh      RETURN
Cprh      END
C
C
C
      SUBROUTINE   RUNEMA
     I                   (NPKS,PKS,IPKSEQ)
C
C     + + + HISTORY + + +
C     Created 11/2003 by Paul Hummel, AQUA TERRA Consultants
C     for incorporating EMA into PEAKFQ
C
C     + + + PURPOSE + + +
C     Transfers PEAKFQ input data to arguments used in EMA,
C     runs EMA, transfers EMA results to PEAKFQ output
C
C     EMAThresh contains Threshold specifications pulled from the .psf file
      USE EMAThresh
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NPKS
      INTEGER   IPKSEQ(NPKS)
      REAL      PKS(NPKS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPKS    - number of peaks
C     PKS     - array of annual peak values
C     IPKSEQ  - array of peak value water years
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXPK.INC'
      INCLUDE 'PMXINT.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwcf0.inc'
      INCLUDE 'cwcf1.inc'
      INCLUDE 'cwcf2.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,NOBS,WYMIN,WYMAX,LPKIND
      DOUBLE PRECISION WRCMOM(3,2),PR(MXINT),       !SKWWGT,
     $                 REGSKEW,REGMSE,WRCYP(MXINT),
     $                 CILOW(MXINT),CIHIGH(MXINT),GBTHRSH
      INTEGER, ALLOCATABLE :: THBY(:),THEY(:)
      REAL, ALLOCATABLE :: THLO(:),THUP(:)
      DOUBLE PRECISION, ALLOCATABLE :: QL(:),QU(:),TL(:),TU(:)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA GBTHRSH /0.0/
C
C     + + + EXTERNALS + + +
      EXTERNAL   EMADATA, EMAFIT
C
C     + + + INTRINSICS + + +
      INTRINSIC  LOG, LOG10, EXP, DBLE
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NTHRESH.EQ.0) THEN
C       determine default threshold value to use
        NTHRESH = 1
        ALLOCATE (THRESH(1))
        THRESH(1)%THRBYR = ABS(IPKSEQ(1))
        THRESH(1)%THREYR = IPKSEQ(NPKS)
C       this is what was used in the original EMA incorporation
        THRESH(1)%THRLWR = 10**WRCHHB  
        THRESH(1)%THRUPR = 1.0D99
      END IF
C
      ALLOCATE (THBY(NTHRESH), THEY(NTHRESH))
      ALLOCATE (THLO(NTHRESH), THUP(NTHRESH))
C
      write(*,*) 'RUNEMA: NTHRESH',NTHRESH
      write(*,*) 'RUNEMA: THRBYR, THREYR, THRLWR, THRUPR'
      WYMIN = 9999
      WYMAX = -9999
      DO 5 I = 1,NTHRESH
        THBY(I) = THRESH(I)%THRBYR
        THEY(I) = THRESH(I)%THREYR
        THLO(I) = THRESH(I)%THRLWR
        THUP(I) = THRESH(I)%THRUPR
        write(*,*) THBY(I),THEY(I),THLO(I),THUP(I)
        IF (THBY(I).LT.WYMIN) WYMIN = THBY(I)
        IF (THEY(I).GT.WYMAX) WYMAX = THEY(I)
 5    CONTINUE

      NOBS = WYMAX - WYMIN + 1
      ALLOCATE (QL(NOBS), QU(NOBS), TL(NOBS), TU(NOBS))
      write(*,*) 'RUNEMA: NPKS,NSYS,NHIST,NOBS ',
     $                    NPKS,NSYS,NHIST,NOBS
      write(*,*) 'RUNEMA: PKS',(PKS(I),I=1,NPKS)

      LPKIND = NPKS - (NSYS+NHIST) + 1
c      CALL EMADATA(NSYS+NHIST,PKS(LPKIND),IPKSEQ(LPKIND),WYMIN,WYMAX,
      CALL EMADATA(NPKS,PKS,IPKSEQ,WYMIN,WYMAX,
     I             NTHRESH,THBY,THEY,THLO,THUP,GAGEB,
     M             NOBS,
     O             QL,QU,TL,TU)
C
C            if(ifany('WEIGHTED',skew_option)) then
C              reg_mseV  = gen_skew_sd**2
C            else if(ifany('GENERALIZ',skew_option)) then
C              reg_mseV  = - gen_skew_sd**2   ! this includes uncty in reg_skew
Cc              reg_mseV  = 0.d0                ! this does not include uncty
C            else if(ifany('STATION',skew_option)) then
C              reg_mseV  = -99.d0
C            endif
      REGSKEW= GENSKU
      IF (IGSOPT.EQ.1) THEN
C       Generalized skew, set to very small
        REGMSE = - RMSEGS**2
      ELSE IF (IGSOPT.EQ.-1) THEN
C       Station skew, ignore regional skew
        REGMSE = -99.0
      ELSE
C       set to root mean square?
        REGMSE = RMSEGS**2
      END IF
      write(*,*) 'calling EMAFIT: NOBS,REGSKEW,REGMSE',
     $                            NOBS,REGSKEW,REGMSE
C
      CALL EMAFIT(NOBS,QL,QU,TL,TU,REGSKEW,REGMSE,GBTHRSH,
     O            WRCMOM,PR,WRCYP,CILOW,CIHIGH)
      
C     store EMA moments in WRC variables
c      WRCUAV = LOG10(EXP(WRCMOM(1)))
c      WRCUSD = LOG10(EXP(SQRT(WRCMOM(2))))
c      WRCSKW = WRCMOM(3)
      WRCUAV = WRCMOM(1,2)
      WRCUSD = SQRT(WRCMOM(2,2))
      WRCSKW = WRCMOM(3,2)
C
      write(*,*)
      write(*,*) 'RESULTS'
      write(*,*) 'Moments:',WRCUAV,WRCUSD,WRCSKW
      DO 20 I = 1,NINDX
        write(*,'(f8.4,4f12.1)')1-PR(I),10**WRCYP(I),
     $                          10**CILOW(I),10**CIHIGH(I)
 20   CONTINUE

      DEALLOCATE (QL, QU, TL, TU)
      DEALLOCATE (THBY, THEY, THLO, THUP)

C      IF (N.GT.0) THEN !perform EMA
C        DO 20 I = 1,MXINT
C          PR(I)= 1.0 - DBLE(TXPROB(I))
C          CALL EMAFIT (N,QL,QU,TL,TU,REGSKEW,REGMSE,PR(I),
C     O                 WRCMOM,WRCYP,CILOW,CIHIGH)
Cc         if (i.eq.1) then 
Cc           write(*,*) '  SYS Moments ',sysmom
Cc           write(*,*) '  WRC Moments ',wrcmom
Cc         end if
Cc         write(*,'(f8.4,4f12.3)')txprob(i),exp(SYSYP),exp(WRCYP),
Cc     $                           exp(CILOW),exp(CIHIGH)
Cc          SYSRFC(I)= LOG10(EXP(SYSYP))
C          WRCFC(I) = LOG10(EXP(WRCYP))
C          CLIML(I) = LOG10(EXP(CILOW))
C          CLIMU(I) = LOG10(EXP(CIHIGH))
C 20     CONTINUE


Cprh     This call to EMAFIT was an attempt to do all intervals
Cprh     within EMAFIT and pass whole arrays back and forth.
Cprh     This would be more efficient, but never got it quite working.
c        CALL EMAFIT (N,QL,QU,TL,TU,REGSKEW,REGMSE,MXINT,PR,
c     O               SYSMOM,WRCMOM,SYSYP,WRCYP,CILOW,CIHIGH)
C       write(*,*)'EMAFIT RESULTS'
C       write(*,*)'  SYSMOM ',SYSMOM
C       write(*,*)'  WRCMOM ',WRCMOM
c        SYSUAV = LOG10(EXP(SYSMOM(1)))
c        SYSUSD = LOG10(EXP(SQRT(SYSMOM(2))))
c        SYSSKW = SYSMOM(3)
c        WRCUAV = LOG10(EXP(WRCMOM(1)))
c        WRCUSD = LOG10(EXP(SQRT(WRCMOM(2))))
c        WRCSKW = WRCMOM(3)
c        DO 30 I = 1,MXINT
c          SYSRFC(I)= LOG10(EXP(SYSYP(I)))
c          WRCFC(I) = LOG10(EXP(WRCYP(I)))
c          CLIML(I) = LOG10(EXP(CILOW(I)))
c          CLIMU(I) = LOG10(EXP(CIHIGH(I)))
c         write(*,'(f8.4,4f12.3)')txprob(i),exp(wrcfc(i)),exp(sysrfc(i)),
c     $             exp(climl(i)),exp(climu(i))
c 30     CONTINUE
C      END IF
C
      RETURN
      END
