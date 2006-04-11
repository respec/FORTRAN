      SUBROUTINE INDAT1
C	EXTRAN BLOCK 
C	CALLED BY EXTRAN NEAR LINE 198
C=======================================================================
C     This Subroutine reads and prints control,
C     Conduit and Junction data.
C=======================================================================
C     THIS PROGRAM LAST MODIFIED BY R.E.D., 4/92.
C     MODIFICATION IS TO ALLOW ONLY ONE SET OF C3,34 LINES TO BE
C       USED FOR IRREGULAR CHANNELS IF DESIRED.
C     ADD INITIAL DATE ON LINE B1, WCH, 4/11/94.
C     MODIFY FOR CORRECT START TIME USING HOT START, WCH, 7/25/96.
C       ALSO, MINOR CORRECTION OF PRINTING 'JREDO' INSTEAD OF 'REDO'.
CIM START  <><><><><><><><><><><><>
CIM  CHANGE MADE 1/96 BY CDM TO CHANGE INTERNAL NUMBERING OF PIPE
CIM  TYPES TO ALLOW OTHER TYPES TO BE INCORPORATED
CIM END  <><><><><><><><><><><>
CIM  10/98  modifified to remove multiple reads.  Lines are
CIM  now padded with zeros in STRIP.FOR
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'FLODAT.INC'
cim TRANAID START
CIM include common blocks for tranaid interface file
      INCLUDE 'TRANAID.INC'
cim TRANAID END
CIM  NEW COMMON FOR WRITING OF RESULTS TO ASCII FILE
      INCLUDE 'CWRITE.INC'
CIM
CIM  NEW COMMON FOR SAVING AND WRITING INTERMEDIATE CONTINUITY OUTPUT
      INCLUDE 'INTCON.INC'
c common for writing detailed EXTRAN ASCII output file.
      INCLUDE 'ASCOUT1.INC'
CIM  CHANGE FOR VARIABLE BASE FLOW
      INCLUDE 'MOBFF.INC'
CIM  CHANGE FOR BE LINES FOR MULTIPLE OUTPUT PERIODS
      INCLUDE 'BE.INC'
CIM  CHANGE FOR BB AND C1 INPUT OF PIPE SEDIMENT DEPTHS
      INCLUDE 'PIPESED.INC'
CIM 1/99 CHANGE FOR BA OUTPUT CONTROLS    - OUTPUT CONTROLS
      INCLUDE 'BALINES.INC'
	INCLUDE 'WASP.INC'
cim  <><><><> increase dimensions of CTYPE from 8 to 12 and add types
cim  to data statement
cim change dimensions of kdum to NCHN  10/98
      CHARACTER GTYPE(2)*20,CTYPE(12)*10,KDUM(NCHN)*10,IREAD*4
	LOGICAL BFFACT,TABLE2
	LOGICAL GRNDERROR
      DATA GTYPE/'  POSITIVE FLAP GATE','  NEGATIVE FLAP GATE'/
      DATA CTYPE/' CIRCULAR ',' RECTANGLE',' HORSESHOE',
     +           ' EGG-SHAPE',' BASKET   ',' TRAPEZOID',
     +           ' POWER FNC',' NATURAL  '
CIM START  <><><><><><><><>
     +          ,' H ELLIPSE',' V ELLIPSE',' ARCH     ',
     +           ' BRIDGE   '/
CIM END   <><><><><><><>
      DATA IREAD/'CARD'/
CIM  START  <><><><><><>
CIM            closed is logical that flags closed conduits for equivalent
CIM            PIPE CALCS,  NEWKLASS CONTAINS THE MAPPING FROM OLD TO NEW
CIM            CLASS ID'S
      LOGICAL CLOSED(NEE)
      DIMENSION NEWKLASS(12)
CIM  THESE NEXT ARRAYS CONTAIN DATA FOR STANDARD ELLIPTICAL AND
CIM  ARCH PIPE TYPES: MAJOR AXIS LENGTH, MINOR AXIS LENGTH,
CIM  FULL FLOW AREA, FULL FLOW HYDRAULIC RADIUS, INITIALIZED IN
      INCLUDE 'SHAPE.INC'
CIM  END     <><><><><><><><><
C=======================================================================
C                  E X E C U T I O N
C=======================================================================
      NSTOP  = 0
      ISOL   = 0
      JSLOT  = 0
      KSUPER = 0
      NCSAVE = NSCRAT(4)
      IF(NCSAVE.GT.0) REWIND NCSAVE
C=======================================================================
C     READ THE TWO A1 TITLE LINES
C=======================================================================
      READ(N5,*,ERR=888) CC,ALPHA1
      READ(N5,*,ERR=888) CC,ALPHA2
      WRITE(N6,2999)
      WRITE(N6,5060) ALPHA1,ALPHA2
C=======================================================================
C     READ SOLUTION PARAMETERS ON OPTIONAL DATA GROUP B0
C=======================================================================
      ISOLSKIP = 0
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B0') THEN
                     BACKSPACE N5
cim ### RHF 8/30/96 CS           READ(N5,*,ERR=888) CC,ISOL,KSUPER
                     READ(N5,*,ERR=888) CC,ISOL,KSUPER,KREDO,
     &                                  TOLCS1,QLOWCS,TOLCS2
                     IF (ISOL.EQ.3) THEN
                     ISOL = 0
                     ISOLSKIP = 1
                     ENDIF
                     IF (ISOL.EQ.4) THEN
                     ISOL = 1
                     ISOLSKIP = 1
                     ENDIF

C ---
                     ELSE
                     BACKSPACE N5
                     ENDIF
CIM   BA THROUGH BE LINES ARE DUPLICATED BELOW.  AS DOCUMENTED IN 
CIM   EXTRAN44.DOC, B LINES SHOULD BE INPUT IN ALPHANUMERIC ORDER 
CIM   (E.G., B0, B1,...B9, BB, BC, BD.  HOWEVER NEED TO KEEP HERE 
CIM   TO MAINTAIN BACKWARD COMPTATIBILITY.
CIM END
C=======================================================================
C     READ OUTPUT CONTROL PARAMETERS ON OPTIONAL DATA GROUP BA
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BA') THEN
         BACKSPACE N5
         READ(N5,*,ERR=888) CC,JHEAD,JP10,IWLEN
         IF (JHEAD.LT.0.OR.JHEAD.GT.1) JHEAD = 0
         IF (JP10.LT.0.OR.JP10.GT.1) JP10 = 0
      ELSE
         JHEAD = 0
         JP10 = 0
	   IWLEN = 0
         BACKSPACE N5
      ENDIF
C  these lines are commented to avoid writing stuff
C  twice when program trys to read lines again below.
c         IF (JHEAD.EQ.0) WRITE(N6,8020)
c         IF (JHEAD.EQ.1) WRITE(N6,8022)
c         IF (JP10.EQ.0) WRITE(N6,8024)
c         IF (JP10.EQ.1) WRITE(N6,8026)
c	IF (IWLEN.EQ.0) WRITE(N6,7010) 
c	IF (IWLEN.EQ.1) WRITE(N6,7020) 
c	IF (IWLEN.EQ.2) WRITE(N6,7030)
C=======================================================================
C     READ SOLUTION PARAMETERS ON OPTIONAL DATA GROUP BB
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BB') THEN
                     BACKSPACE N5
cim SEDEPTH  Add IPIPESED to BB line
CIM PP 4/1/97  add IPRATE to BB lines
cim    9/22/97  add IM2 to BB line
cim
                     READ(N5,*,ERR=888) CC,JELEV,JDOWN,IPRATE,IM2,
     a                                 IPIPESED
                     IF(JELEV.LT.0) JELEV = 0
                     IF(JDOWN.LT.0.OR.JDOWN.GT.2) JDOWN = 0
                     ELSE
                     JELEV = 0
                     JDOWN = 0
                     IPRATE = 0
                     IM2 = 0
                     IPIPESED = 0
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     READ INTERMEDIATE CONTINUITY OUTPUT DATA ON OPTIONAL DATA GROUP BC
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BC') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,ICONTER
                     ELSE
                     ICONTER = 0
                     BACKSPACE N5
                     ENDIF
c      IF (ICONTER.LE.0) then
c      write(n6,8005) 
c      ICONTER = 2147483647
c      else
c      write(n6,8010) ICONTER
c      endif
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP BD <<<<<<<<<<<<
C    OPTIONAL INPUT OF BASE FLOW FACTORS
C    CIM   10/97
C=======================================================================
C  INITIALIZE STUFF
      NUMSETS = 0
	BFFACT = .FALSE.
      MONTHOLD = 0
      DO 19947 I=1,MAXSETS
      NUMBFF(I) = 0
      INBFF(I) = 0
      IBFF(I) = 0
      DO 19947 J=1,MAXBFF
19947 BFFMO(I,J) = 1.0
      do 19349 i=1,nee
19349 iwhich(i)=1
19948 READ (N5,*,ERR=888) CC
      IF (CC.EQ.'BD') THEN
	BFFACT = .TRUE.
      NUMSETS = NUMSETS+1
      IF (NUMSETS.GT.MAXSETS) THEN
      WRITE(N6,*) ' NUMBER OF BASE FLOWS SETS SPECIFIED ON BD LINES',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXSETS
      STOP 'TOO MANY BD LINES'
      ENDIF
      BACKSPACE N5
      READ (N5,*,ERR=888) CC,NUMBFF(NUMSETS)
      IF (NUMBFF(NUMSETS).GT.MAXBFF) THEN
      WRITE(N6,*) ' NUMBER OF BASE FLOWS SPECIFIED ON BD LINE',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXBFF,' FOR SET ',
     .'NUMBER ',NUMSETS
      STOP 'TOO MANY BASE BASE FLOW VALUES SPECIFED ON BD LINE'
      ENDIF
      BACKSPACE N5
      READ (N5,*,ERR=888) CC,NUMBFF(NUMSETS),
     .              (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
      WRITE(N6,*)
      WRITE(N6,*) ' MONTHLY BASE FLOW FACTOR SET NUMBER ',NUMSETS
      WRITE(N6,949) (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
C GO BACK TO TRY TO READ NEXT BD LINE
      GOTO 19948
      ELSE
C NO BD LINES READ, OR NO ADDITIONAL BD LINES FOUND
C  THEN BACKSPACE AND CONTININUE
      BACKSPACE N5
      END IF
c  initialize following below if needed
c      IF (NUMSETS.LE.0) NUMSETS = 1
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP BE <<<<<<<<<<<<
C    OPTIONAL INPUT OF DETAILED SUMMARY OUTPUT PERIODS
C    CIM   5/98
C=======================================================================
C  INITIALIZE STUFF
      DO NUMBE = 0,MAXBE
      IBESTART(NUMBE) = 0
      IBEEND(NUMBE) = 0
      ENDDO
      NUMBE = 0
      IBE = 0
19950 READ (N5,*,ERR=888) CC
      IF (CC.EQ.'BE') THEN
      NUMBE = NUMBE + 1
      IBE = 1
C  NOTE MAXBE IS RESERVED FOR PERIOD AFTER LAST PERIOD
      IF (NUMBE.GT.MAXBE-1) THEN
      WRITE(N6,*) ' NUMBER OF BE LINES',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXBE-1
      STOP 'TOO MANY BE LINES'
      ENDIF
      BACKSPACE N5
      READ(N5,*,ERR=888) CC, IBESTART(NUMBE), IBEEND(NUMBE)
      IF (NUMBE.EQ.1) WRITE(N6,951)
      WRITE(N6,952) IBESTART(NUMBE),IBEEND(NUMBE)
C  TRY TO READ NEXT BE LINE
      GO TO 19950
      ELSE
C  NO BE LINE FOUND.  BACKSPACE AND CONTINUE
CIM SET IBESTART(0) AND IBEEND(0)
CIM FOR USE IN CASE NO BE LINES ARE INPUT
c      IBESTART(0) = NSTART
c      IBEEND(0) = 999999999
      BACKSPACE N5
	ENDIF
C  Check on inputs
	IF (NUMBE.GT.0) THEN
CIM SET IBESTART(NUMBE+1) AND IBBEND(NUMBE+1) HERE
      IBESTART(NUMBE+1) = 999999999
      IBEEND(NUMBE+1) = 999999999
      DO I = 1, NUMBE
      IF (IBEEND(NUMBE).LE.IBESTART(NUMBE)) THEN
      WRITE(N6,*) ' ERROR - ENDING CYCLE IS LESS THAN STARTING CYCLE '
      WRITE(N6,*) '         BE CARD NUMBER = ',I,' START CYCLE = ',
     *IBESTART(I),' END CYCLE = ',IBEEND(I)
      STOP ' ERROR IN NE LINE INPUTS'
      ENDIF
      IF (I.GT.1.AND.IBESTART(I).LE.IBEEND(I-1)) THEN
      WRITE(N6,*) ' ERROR - STARTING CYCLE IS LESS THAN ENDING CYCLE ',
     A'OF PREVIOUS PRINTOUT PERIOD'
      WRITE(N6,*) '         BE CARD NUMBER = ',I,' START CYCLE = ',
     AIBESTART(I),' END CYCLE OF PREVIOUS NE CARD = ',IBEEND(I-1)
      STOP ' ERROR IN BE LINE INPUTS'
      ENDIF
      ENDDO
      ENDIF
C=======================================================================
C     GENERAL CONTROL PARAMETERS ON DATA GROUPS B1, B2 AND B3
C=======================================================================
C#### WCH, 4/11/94.  ADD OPTIONAL INITIAL DATE TO LINE B1 INPUT.
      IDATZ = 0
      READ(N5,*,ERR=888) CC,NTCYC,DELT,TZERO,NSTART,INTER,JNTER,JREDO,
     1 IDATZ
C
      READ(N5,*,ERR=888) CC,METRIC,NEQUAL,AMEN,ITMAX,SURTOL
      READ(N5,*,ERR=888) CC,NHPRT,NQPRT,NPLT,LPLT,NJSW
C=======================================================================
C     METRIC = 0 --> U.S. CUSTOMARY UNITS
C     METRIC = 1 --> METRIC UNITS
C                    NOTE: METRIC = METRIC + 1  SOON IN FORTRAN CODE.
C                    NOTE: EXTRAN USES CONSISTENT UNITS (U.S. OR METRIC)
C                      INTERNALLY IN CODE DURING COMPUTATIONS.
C     JSLOT  = 0 --> SUM OF JUNCTION FLOW IS ZERO
C     JSLOT  = 1 --> PREISSMAN SLOT
C=======================================================================
      IF(SURTOL.LT.0.0) THEN
                        ISOL   = 1
                        SURTOL = ABS(SURTOL)
                        ENDIF
cim rhf change .GE. to .EQ.
      IF(ISOL.EQ.2)  JSLOT = 1
      METRIC               = METRIC + 1
      RDELT                = DELT
      DELT2                = DELT/2.0
      GRVT                 = 32.2
      IF(METRIC.EQ.2) GRVT = 9.806
      IF(AMEN.EQ.0.0.AND.METRIC.EQ.1) AMEN = 12.566
      IF(AMEN.EQ.0.0.AND.METRIC.EQ.2) AMEN =  1.22
      WRITE(N6,5100)  NTCYC
      WRITE(N6,5120)  DELT,DELT*FLOAT(NTCYC)/3600.0
      IF(NEQUAL.EQ.0) WRITE(N6,5121) NEQUAL
C BAC START  !!!!!!!!
C      IF(NEQUAL.GE.1) WRITE(N6,5122) NEQUAL
      IF(NEQUAL.EQ.1) WRITE(N6,5122) NEQUAL
      IF(NEQUAL.EQ.2) WRITE(N6,5921) NEQUAL
      IF(NEQUAL.EQ.3) WRITE(N6,5922) NEQUAL
      IF(NEQUAL.EQ.4) WRITE(N6,5923) NEQUAL
      IF(NEQUAL.EQ.5) WRITE(N6,5924) NEQUAL
cim   check to eliminate possibility of using nequal 4 or 5 with isol = 0 or 3
cim
      IF(NEQUAL.GE.4) THEN
c      IF(ISOL.EQ.0.or.ISOL.EQ.3) THEN
c      WRITE(N6,7920)
c      WRITE(*,7920)
c 7920 FORMAT(' WARNING : TESTING SHOWS THAT USING NEQUAL OF 4 OR 5 ',/,
c     a' WITH ISOL EQUAL TO 0 OR 3 MAY PRODUCE ERRONEOUS RESULTS.  ',/,
c     b' WE SUGGEST USING ISOL OF 1 OR 4.  IF YOU USE ISOL OF 0 OR 3, '
c     c,/,' WE RECOMMEND THAT YOU TEST THE PROGRAM RESULTS AGAINST',/,
c     d' SIMULATIONS USING NEQUAL OF 3 OR 4.')
c      WRITE(*,*) 'PRESS ENTER TO CONTINUE WITH SIMULATION'
c      READ(*,*) KDUM(1)
c      ENDIF
      IF (ISOL.EQ.2) THEN
      WRITE(N6,7930)
      WRITE(*,7930)
 7930 FORMAT(' WARNING : USE OF NEQUAL OF 4 OR 5 HAS NOT BEEN TESTED',/
     A,' WITH ISOL OF 2.  WE SUGGEST THAT YOU COMPARE RESULTS WITH',/,
     B' NEQUAL OF 4 OR 5 WITH CORRESPONDING RESULTS FROM NEQUAL',
     C' OF 3 OR 4.')
      write(*,*) 'PRESS ENTER TO CONTINUE WITH SIMULATION'
      read(*,*) KDUM(1)
      ENDIF
      ENDIF
C BAC END  !!!!!!!
      IF(METRIC.EQ.1) WRITE(N6,5123) METRIC-1
      IF(METRIC.EQ.2) WRITE(N6,5124) METRIC-1
      IF(NSTART.LE.0) NSTART   = 1
CIM IF JNTER is ZERO SET TO BIG
      IF (JNTER.EQ.0.0) JNTER = 999999999
CIM CHANGES FOR INTER
      SELECT CASE(INTER)
      CASE (0)
CIM FIRST IF INTER IS ZERO, THEN NO OUTPUT IS WRITTEN
CIM SET INTER TO A BIG NUMBER
      INTER = 999999999
      IASCII = 0
      CASE (:-1)
cim less than zero, modify JOUT file to create
cim detailed ascii results output file
      INTER = -INTER
      WRITE(N6,5139)
      IASCII = JOUT(IOUTCT)
      NEXT = 0
      IF (IASCII.EQ.0) THEN
      WRITE(N6,5137)
      STOP
      ENDIF
      IF (FFNAME(25+IOUTCT).EQ.'JOT.UF') THEN
      WRITE(N6,5138) JOUT(IOUTCT)
      stop
      ELSE
close file and open as formatted
      CLOSE(UNIT=IASCII)
      OPEN(UNIT=IASCII,FILE=FFNAME(25+IOUTCT),STATUS='UNKNOWN',
     aFORM='FORMATTED')
      ENDIF
      CASE(1:)
      IASCII = 0
      END SELECT
      WRITE(N6,5140)  NSTART,INTER,FLOAT(INTER)*DELT/60.0,JNTER,
     +                FLOAT(JNTER)*DELT/60.0,JREDO
C#### WCH, 7/25/96.  ADD DATE/TIME EXPLANATIONS WITH HOT START.
c     IF(JREDO.EQ.0.OR.JREDO.EQ.2) THEN
c          WRITE(N6,5160)  TZERO
c          ELSEIF(NEXT.GT.0) THEN
c               WRITE(N6,5161)  TZERO
c          ENDIF
cim  changed to write always, also changed format 11/97
           WRITE(N6,5160)  TZERO
           WRITE(N6,5161)
C#### WCH, 4/11/94 AND 7/25/96 (ADD 5165).
      IF(IDATZ.EQ.0) THEN
           IDATZ = 19880101
           WRITE(N6,5163) IDATZ
           ELSE
           IYRZ = IDATZ/10000
           IF (IYRZ.LT.100) THEN
           IDATZ = IDATZ - IYRZ*10000
           IYRZ = IYRZ + 1900
           IDATZ = IDATZ + IYRZ*10000
           ENDIF
           WRITE(N6,5164) IDATZ
           ENDIF
c next line should have been last not next
c change to write always 11/97
c      IF(NEXT.GT.0) WRITE(N6,5165)
      WRITE(N6,5165)
C
      WRITE(N6,5170)  ITMAX,SURTOL
      IF(METRIC.EQ.1) WRITE(N6,5175) AMEN
      IF(METRIC.EQ.2) WRITE(N6,5176) AMEN
      IF(ISOL.EQ.0)   WRITE(N6,5177)
      IF(ISOL.EQ.1)   WRITE(N6,5178)
      IF(ISOL.EQ.2)   WRITE(N6,5179)
C ### RHF 8/30/96 CS
      if (metric.eq.1) then
      IF(ISOLSKIP.EQ.1)   WRITE(N6,9050) QLOWCS,TOLCS1,TOLCS2
      else
      IF(ISOLSKIP.EQ.1)   WRITE(N6,9051) QLOWCS,TOLCS1,TOLCS2
      endif
      IF(JSLOT.EQ.1)  WRITE(N6,9060)
C ---
      IF(KSUPER.EQ.0) WRITE(N6,5183)
      IF(KSUPER.EQ.1) WRITE(N6,5184)
      TZERO  =  3600.0*TZERO
      ITMAX  = IABS(ITMAX)
C BAC START  !!!!!!!!!!  comment out nequal
CIM NOTE THAT THIS MODIFICATION CHANGES DEFINITION OF NEQUAL FROM HERE ON.
C      IF(NEQUAL.EQ.1) NEQUAL = IFIX(DELT)
C BAC END  !!!!!!!!!!
      WRITE(N6,5180) NJSW
C=======================================================================
C     Read Junction numbers for print and plot data.
C=======================================================================
      IF(NHPRT.GT.0) THEN
         IF (JCE.EQ.0) THEN
                     READ(N5,*,ERR=888) CC,(JPRT(I),I=1,NHPRT)
                     WRITE(N6,5200)  NHPRT,(JPRT(I),I=1,NHPRT)
         ELSE
                     READ(N5,*,ERR=888) CC,(AOUT(I,1),I=1,NHPRT)
                     WRITE(N6,5201)  NHPRT,(AOUT(I,1),I=1,NHPRT)
         ENDIF
      ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR DETAILED PRINTOUT
C=======================================================================
      IF(NQPRT.GT.0) THEN
         IF (JCE.EQ.0) THEN
                     READ(N5,*,ERR=888) CC,(CPRT(I),I=1,NQPRT)
                     WRITE(N6,5220)  NQPRT,(CPRT(I),I=1,NQPRT)
         ELSE
                     READ(N5,*,ERR=888) CC,(AOUT(I,2),I=1,NQPRT)
                     WRITE(N6,5221)  NQPRT,(AOUT(I,2),I=1,NQPRT)
         ENDIF
      ENDIF
C=======================================================================
C     READ JUNCTION NUMBERS FOR PLOTTING
C=======================================================================
      IF(NPLT.GT.0) THEN
         IF(JCE.EQ.0) THEN
                    READ(N5,*,ERR=888) CC,(JPLT(N),N=1,NPLT)
                    WRITE(N6,5240)     NPLT,(JPLT(N),N=1,NPLT)
         ELSE
                    READ(N5,*,ERR=888) CC,(AOUT(N,3),N=1,NPLT)
                    WRITE(N6,5241)     NPLT,(AOUT(N,3),N=1,NPLT)
         ENDIF
      ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR PLOTTING
C=======================================================================
      IF(LPLT.GT.0) THEN
         IF(JCE.EQ.0) THEN
                    READ(N5,*,ERR=888) CC,(KPLT(N),N=1,LPLT)
                    WRITE(N6,5260)     LPLT,(KPLT(N),N=1,LPLT)
         ELSE
                    READ(N5,*,ERR=888) CC,(AOUT(N,4),N=1,LPLT)
                    WRITE(N6,5261)     LPLT,(AOUT(N,4),N=1,LPLT)
         ENDIF
      ENDIF
C=======================================================================
C     READ CONDUIT NUMBERS FOR PLOTTING WATER SURFACE SLOPE
C     DATA WILL BE ON DATA GROUP B8 IF PRESENT.
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B8') THEN
                     BACKSPACE N5
                     IF(JCE.EQ.0) THEN
                      READ(N5,*,ERR=888) CC,NSURF,(JSURF(N),N=1,NSURF)
                      WRITE(N6,5265)        NSURF,(JSURF(N),N=1,NSURF)
                     ELSE
                      READ(N5,*,ERR=888) CC,NSURF,(AOUT(N,5),N=1,NSURF)
                      WRITE(N6,5266)        NSURF,(AOUT(N,5),N=1,NSURF)
                     ENDIF
                     ELSE
                     NSURF = 0
                     BACKSPACE N5
                     ENDIF
CIM  WRITING OF RESULTS TO ASCII FILE OR BINARY FILE
C=======================================================================
C     READ CONDUIT NUMBERS FOR WRITING FLOWS TO ASCII FILE
C     DATA WILL BE ON DATA GROUP B9 IF PRESENT.
C         NOFLOW  - Number of conduits for which flows are written.
C                   Negative indicates that the flows will be written
C                   to a unformatted sequential file compatible with STATS
C                   block.
C                   IF negative then write absolute value of flows
C         NOFDUP  - Number of additional conduits to write
C         IFINTER - Number of time steps between output time steps.
C                   If negative write to a standard SWMM binary file.
C         FLOWMIN - Minimum flow.  Flows will not be written when the flow
C                   are less than this value.
C         FLOWOUT - Conduits for which flows are written.  Repeat NOFLOW
C                   times without B9 on overlap lines.
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B9') THEN
                     BACKSPACE N5
                     IF(JCE.EQ.0) THEN
                          READ(N5,*,ERR=888) CC,NOFLOW,NOFDUP,IFINTER,
     &                              FLOWMIN,(FLOWOUT(N),N=1,ABS(NOFLOW))
                     DO N = 1, NOFDUP
                        READ(N5,*,ERR=888) FLOWOUT(N+ABS(NOFLOW)),
     +				                     FLOWREF(N)
                     ENDDO
                          writing = .false.
                          dryflwstep = 0.0
                          IF(IFINTER.LT.0) THEN
                               IFLBIN = 1
                               WRITE(N6,8260)
                               IFINTER = -IFINTER
                               ELSE
                               IFLBIN = 0
                               WRITE(N6,8261)
                               ENDIF
c
                          IF(NOFLOW.LT.0) THEN
                               B9ABS = .TRUE.
                               NOFLOW = - NOFLOW
                               WRITE(N6,8263)
                          ELSE
                               B9ABS = .FALSE.
                          ENDIF
c
                          WRITE(N6,5267)    NOFLOW,IFINTER,FLOWMIN,
     &                                  (FLOWOUT(N),N=1,NOFLOW)
                          IF (NOFDUP.GT.0) THEN
                          WRITE(N6,7268)    NOFDUP
                          WRITE(N6,7269)
     &                     (FLOWOUT(N+NOFLOW),FLOWREF(N),N=1,NOFDUP)
                          ELSE
                          WRITE(N6,7270)
                          ENDIF
                          ELSE
                          READ(N5,*,ERR=888) CC,NOFLOW,NOFDUP,IFINTER,
     &                              FLOWMIN,(AFLWOUT(N),N=1,ABS(NOFLOW))
                     DO N = 1, NOFDUP
                       READ(N5,*,ERR=888) AFLWOUT(N+ABS(NOFLOW)),
     1                                    AFLOWREF(N)
                     ENDDO
                          writing = .false.
                          dryflwstep = 0.0
                          IF(IFINTER.LT.0) THEN
                               IFLBIN = 1
                               WRITE(N6,8260)
                               IFINTER = -IFINTER
                               ELSE
                               IFLBIN = 0
                               WRITE(N6,8261)
                               ENDIF
c
                          IF(NOFLOW.LT.0) THEN
                               B9ABS = .TRUE.
                               NOFLOW = - NOFLOW
                               WRITE(N6,8263)
                          ELSE
                               B9ABS = .FALSE.
                          ENDIF
C
                          WRITE(N6,5268)    NOFLOW,IFINTER,FLOWMIN,
     &                                  (AFLWOUT(N),N=1,NOFLOW)
                          IF (NOFDUP.GT.0) THEN
                          WRITE(N6,7268)    NOFDUP
                          WRITE(N6,7271)
     &                     (AFLWOUT(N+NOFLOW),AFLOWREF(N),N=1,NOFDUP)
                          ELSE
                          WRITE(N6,7270)
                          ENDIF
                          ENDIF
                     DO 25 N=1,MXFLOW
   25                SUMFLW(MXFLOW)=0.0
                     ioflow = 0
                     FLWSTEP = IFINTER * DELT
                     IF(NSCRAT(3).EQ.0) THEN
      WRITE(N6,*) 'ERROR - NSCRAT(3) MUST BE NON-ZERO TO USE OPTION',
     +' TO WRITE RESULTS FOR SELECTED CONDUITS TO ASCII FILE'
                          STOP 'NSCRAT(3) IS ZERO FOR B9-LINE OPTION'
                          ELSE
                          NFOUT = NSCRAT(3)
                          ENDIF
                     IF(FFNAME(53).EQ.'SCRT3.UF') THEN
      WRITE(N6,*) 'ERROR - YOU MUST SPECIFY THE NAME FOR NSCRAT(3)',
     +' TO WRITE RESULTS FOR SELECTED CONDUITS TO ASCII FILE'
                          STOP 'NO DOS NAME GIVEN FOR NSCRAT(3)'
                          ENDIF
                     IF(IFLBIN.EQ.0) THEN
                          WRITE(NFOUT,5269)
                          IF(JCE.EQ.0) THEN
                               WRITE(NFOUT,5270)(FLOWOUT(N),
     &                          N=1,NOFLOW+NOFDUP)
                               ELSE
                               WRITE(NFOUT,5271)(AFLWOUT(N),
     &                          N=1,NOFLOW+NOFDUP)
                               ENDIF
                          ELSE
cim here if IFLBIN = 1
                          CLOSE(NFOUT,STATUS='DELETE')
                          OPEN(NFOUT,FILE=FFNAME(53),FORM='UNFORMATTED',
     +                        STATUS='UNKNOWN')
C=======================================================================
C     Write file headers for output hydrograph.
C=======================================================================
                         REWIND NFOUT
                         NPOLL = 0
                         WRITE(NFOUT) NOFLOW+NOFDUP,NPOLL
                         IF(JCE.EQ.0) THEN
                           WRITE(NFOUT) (FLOWOUT(N),N=1,NOFLOW+NOFDUP)
                         ELSE
                           WRITE(NFOUT) (AFLWOUT(N),N=1,NOFLOW+NOFDUP)
                         ENDIF
                         SOURCE = 'EXTRAN BLOCK'
                         QQCONV = QCONV
                         QCONV  = CMET(8,METRIC)
                         TITLE(3) = ALPHA1
                         TITLE(4) = ALPHA2
                         CALL INFACE(2,NFOUT)
                         QCONV    = QQCONV
                         ENDIF
                     ELSE
                     NOFLOW = 0
                     BACKSPACE N5
                     ENDIF
CIM END
C=======================================================================
C     READ OUTPUT CONTROL PARAMETERS ON OPTIONAL DATA GROUP BA
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BA') THEN
         BACKSPACE N5
         READ(N5,*,ERR=888) CC,JHEAD,JP10,IWLEN
         IF (JHEAD.LT.0.OR.JHEAD.GT.1) JHEAD = 0
         IF (JP10.LT.0.OR.JP10.GT.1) JP10 = 0
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE
C  REMOVED.
      ELSE
c         JHEAD = 0
c         JP10 = 0
c         IWLEN = 0
         BACKSPACE N5
      ENDIF
         IF (JHEAD.EQ.0) WRITE(N6,8020)
         IF (JHEAD.EQ.1) WRITE(N6,8022)
         IF (JP10.EQ.0) WRITE(N6,8024)
         IF (JP10.EQ.1) WRITE(N6,8026)
	IF (IWLEN.EQ.0) WRITE(N6,7010) 
	IF (IWLEN.EQ.1) WRITE(N6,7020) 
	IF (IWLEN.EQ.2) WRITE(N6,7030)
C=======================================================================
C     READ SOLUTION PARAMETERS ON OPTIONAL DATA GROUP BB
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BB') THEN
                     BACKSPACE N5
cim SEDEPTH  Add IPIPESED to BB line
CIM PP 4/1/97  add IPRATE to BB lines
cim    9/22/97  add IM2 to BB line
cim
                     READ(N5,*,ERR=888) CC,JELEV,JDOWN,IPRATE,IM2,
     a                                 IPIPESED
                     IF(JELEV.LT.0) JELEV = 0
                     IF(JDOWN.LT.0.OR.JDOWN.GT.2) JDOWN = 0
                     ELSE
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE
C  REMOVED.
c                     JELEV = 0
c                     JDOWN = 0
c                     IPRATE = 0
c                     IM2 = 0
c                     IPIPESED = 0
                     BACKSPACE N5
                     ENDIF
	IF(JELEV.EQ.0) WRITE(N6,8184)
      IF(JELEV.EQ.1) WRITE(N6,8185)
	IF(JELEV.EQ.2) WRITE(N6,8186)
	IF(JELEV.EQ.3) WRITE(N6,8187)
	IF(JELEV.EQ.4) WRITE(N6,8188)
      IF(JDOWN.EQ.0) WRITE(N6,5186)
      IF(JDOWN.EQ.1) WRITE(N6,5187)
      IF(JDOWN.EQ.2) WRITE(N6,5188)
      IF(IPRATE.NE.0) WRITE(N6,5189)
change
      IF(IM2.EQ.0) THEN
      WRITE(N6,5190)
      ELSE
      IM2 = 1
      WRITE(N6,5191)
      ENDIF
      IF(IPIPESED.NE.0) THEN
      WRITE(N6,5192)
      ELSE
      WRITE(N6,5193)
      ENDIF
change
C=======================================================================
C     READ INTERMEDIATE CONTINUITY OUTPUT DATA ON OPTIONAL DATA GROUP BC
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BC') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,ICONTER
                     ELSE
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE
C  REMOVED.
c                     ICONTER = 0
                     BACKSPACE N5
                     ENDIF
      IF (ICONTER.LE.0) then
      write(n6,8005) 
      ICONTER = 2147483647
      else
      write(n6,8010) 
      endif
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP BD <<<<<<<<<<<<
C    OPTIONAL INPUT OF BASE FLOW FACTORS
C    CIM   10/97
C=======================================================================
C  INITIALIZE STUFF
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE
C  REMOVED.
c      NUMSETS = 0
c	BFFACT = .FALSE.
c      MONTHOLD = 0
c      DO 947 I=1,MAXSETS
c      NUMBFF(I) = 0
c      INBFF(I) = 0
c      IBFF(I) = 0
c      DO 947 J=1,MAXBFF
c  947 BFFMO(I,J) = 1.0
c      do 349 i=1,nee
c  349 iwhich(i)=1
  948 READ (N5,*,ERR=888) CC
      IF (CC.EQ.'BD') THEN
	BFFACT = .TRUE.
      NUMSETS = NUMSETS+1
      IF (NUMSETS.GT.MAXSETS) THEN
      WRITE(N6,*) ' NUMBER OF BASE FLOWS SETS SPECIFIED ON BD LINES',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXSETS
      STOP 'TOO MANY BD LINES'
      ENDIF
      BACKSPACE N5
      READ (N5,*,ERR=888) CC,NUMBFF(NUMSETS)
      IF (NUMBFF(NUMSETS).GT.MAXBFF) THEN
      WRITE(N6,*) ' NUMBER OF BASE FLOWS SPECIFIED ON BD LINE',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXBFF,' FOR SET ',
     .'NUMBER ',NUMSETS
      STOP 'TOO MANY BASE BASE FLOW VALUES SPECIFED ON BD LINE'
      ENDIF
      BACKSPACE N5
      READ (N5,*,ERR=888) CC,NUMBFF(NUMSETS),
     .              (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
      WRITE(N6,*)
      WRITE(N6,*) ' MONTHLY BASE FLOW FACTOR SET NUMBER ',NUMSETS
      WRITE(N6,949) (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
C GO BACK TO TRY TO READ NEXT BD LINE
      GOTO 948
      ELSE
C NO BD LINES READ, OR NO ADDITIONAL BD LINES FOUND
C  THEN BACKSPACE AND CONTININUE
      BACKSPACE N5
      END IF
      IF (NUMSETS.LE.0) NUMSETS = 1
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP BE <<<<<<<<<<<<
C    OPTIONAL INPUT OF DETAILED SUMMARY OUTPUT PERIODS
C    CIM   5/98
C=======================================================================
C  INITIALIZE STUFF
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE REMOVED
c     DO NUMBE = 0,MAXBE
c     IBESTART(NUMBE) = 0
c     IBEEND(NUMBE) = 0
c     ENDDO
c     NUMBE = 0
c     IBE = 0
  950 READ (N5,*,ERR=888) CC
      IF (CC.EQ.'BE') THEN
      NUMBE = NUMBE + 1
      IBE = 1
C  NOTE MAXBE IS RESERVED FOR PERIOD AFTER LAST PERIOD
      IF (NUMBE.GT.MAXBE-1) THEN
      WRITE(N6,*) ' NUMBER OF BE LINES',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXBE-1
      STOP 'TOO MANY BE LINES'
      ENDIF
      BACKSPACE N5
      READ(N5,*,ERR=888) CC, IBESTART(NUMBE), IBEEND(NUMBE)
      IF (NUMBE.EQ.1) WRITE(N6,951)
      WRITE(N6,952) IBESTART(NUMBE),IBEEND(NUMBE)
C  TRY TO READ NEXT BE LINE
      GO TO 950
      ELSE
C  NO BE LINE FOUND.  BACKSPACE AND CONTINUE
CIM SET IBESTART(0) AND IBEEND(0)
CIM FOR USE IN CASE NO BE LINES ARE INPUT
C  NOTE THAT THESE WERE INITIALIZED WHEN PROGRAM TRIED TO READ
C  CARD ABOVE.  IF/WHEN CODE IS CORRECTED TO HAVE B LINES IN 
C  ALPHA NUMERIC ORDER. THEN THE FOLLOWING COMMENTS SHOULD BE REMOVED
      IBESTART(0) = NSTART
      IBEEND(0) = 999999999
      BACKSPACE N5
	ENDIF
C  Check on inputs
	IF (NUMBE.GT.0) THEN
CIM SET IBESTART(NUMBE+1) AND IBBEND(NUMBE+1) HERE
      IBESTART(NUMBE+1) = 999999999
      IBEEND(NUMBE+1) = 999999999
      DO I = 1, NUMBE
      IF (IBEEND(NUMBE).LE.IBESTART(NUMBE)) THEN
      WRITE(N6,*) ' ERROR - ENDING CYCLE IS LESS THAN STARTING CYCLE '
      WRITE(N6,*) '         BE CARD NUMBER = ',I,' START CYCLE = ',
     *IBESTART(I),' END CYCLE = ',IBEEND(I)
      STOP ' ERROR IN NE LINE INPUTS'
      ENDIF
      IF (I.GT.1.AND.IBESTART(I).LE.IBEEND(I-1)) THEN
      WRITE(N6,*) ' ERROR - STARTING CYCLE IS LESS THAN ENDING CYCLE ',
     A'OF PREVIOUS PRINTOUT PERIOD'
      WRITE(N6,*) '         BE CARD NUMBER = ',I,' START CYCLE = ',
     AIBESTART(I),' END CYCLE OF PREVIOUS NE CARD = ',IBEEND(I-1)
      STOP ' ERROR IN BE LINE INPUTS'
      ENDIF
      ENDDO
      ENDIF
C=======================================================================
C     CALL TO WASPREAD TO TRY TO READ EXTRAN TO WASP LINKAGE 
C     LINES   BF, BG, BH LINES
      CALL WASPREAD
CIM TRANAID START
C=======================================================================
CIM   OPTIONAL READ OF BZ CARD FOR INFORMATION TO WRITE TRANAID
CIM   INTERFACE FILE
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'BZ') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,idump, dthyd, hydstr,IVCALC
                     else
                     idump = 0
                     dthyd = 0.0
                     hydstr = 0.0
                     BACKSPACE N5
                     end if
        if (idump .gt. 0) then
        write(n6,6666) hydstr, dthyd
	IF (IVCALC.EQ.0) THEN
	WRITE(N6,7022)
	LVCALC = .TRUE.
	ELSE
	WRITE(N6,7024)
	LVCALC = .FALSE.
	ENDIF
        end if
CIM  TRANAID END
C=======================================================================
C     Read conduit data on data group C1.
C=======================================================================
      WRITE(*,6000)
      NATUR    = 0
      DO 260 N = 1,NEE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'C1') GO TO 280
      IF(JCE.EQ.0) THEN
      IF (IPIPESED.EQ.0) THEN
         READ(N5,*,ERR=888) CC,NCOND(N),(NJUNC(N,K),K=1,2),QO(N),
     +                      NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                      ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N)
                        ENTK(N) = 0.0
                        EXITK(N) = 0.0
                        OTHERK(N) = 0.0
                        SEDEPTH(N) = 0.0
      ELSE
         READ(N5,*,ERR=888) CC,NCOND(N),(NJUNC(N,K),K=1,2),QO(N),
     +                      NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                      ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N),
     +                      SEDEPTH(N)
                        ENTK(N) = 0.0
                        EXITK(N) = 0.0
                        OTHERK(N) = 0.0
      ENDIF
CIM START  <><><><><><><>
C *** NOTE:  THE FOLLOWING IF STATEMENT WILL NEED TO BE MODIFIED IF A
C            CLOSED CONDUIT WITH NKLASS > 5 IS ADDED.
CIM  ADDED CLOSED ARRAY TO ID CLOSED CONDUITS
         CLOSED(N) = (NKLASS(N).LE.5).OR.
     .            (NKLASS(N).EQ.9).OR.
     .            (NKLASS(N).EQ.10).OR.
     .            (NKLASS(N).EQ.11).OR.
     .            (NKLASS(N).EQ.12)
CIM END     <><><><><><>
C  BAC START  !!!!!!!
C  read entrance exit and other loss coefficent if closed conduit type
        IF(NEQUAL.GE.2.AND.CLOSED(N)) THEN
          BACKSPACE N5
        IF(IPIPESED.EQ.0) THEN
          READ(N5,*,ERR=888) CC,NCOND(N),(NJUNC(N,K),K=1,2),QO(N),
     +                      NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                      ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N),
     +                      ENTK(N),EXITK(N),OTHERK(N)
          SEDEPTH(N) = 0.0
        ELSE
          READ(N5,*,ERR=888) CC,NCOND(N),(NJUNC(N,K),K=1,2),QO(N),
     +                      NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                      ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N),
     +                      ENTK(N),EXITK(N),OTHERK(N),SEDEPTH(N)
        ENDIF
        ENDIF
C BAC END  !!!!!!
       ELSE
      IF (IPIPESED.EQ.0) THEN
      READ(N5,*,ERR=888) CC,ACOND(N),(KJUNC(N,K),K=1,2),
     +                  QO(N),NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                  ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N)
                        ENTK(N) = 0.0
                        EXITK(N) = 0.0
                        OTHERK(N) = 0.0
                        SEDEPTH(N) = 0.0
      ELSE
      READ(N5,*,ERR=888) CC,ACOND(N),(KJUNC(N,K),K=1,2),
     +                  QO(N),NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),
     +                  ZU(N),ZD(N),ROUGH(N),STHETA(N),SPHI(N),
     +                  SEDEPTH(N)
                        ENTK(N) = 0.0
                        EXITK(N) = 0.0
                        OTHERK(N) = 0.0
      ENDIF
CIM START  <><><><><><><>
C *** NOTE:  THE FOLLOWING IF STATEMENT WILL NEED TO BE MODIFIED IF A
C            CLOSED CONDUIT WITH NKLASS > 5 IS ADDED.
CIM  ADDED CLOSED ARRAY TO ID CLOSED CONDUITS
         CLOSED(N) = (NKLASS(N).LE.5).OR.
     .            (NKLASS(N).EQ.9).OR.
     .            (NKLASS(N).EQ.10).OR.
     .            (NKLASS(N).EQ.11).OR.
     .            (NKLASS(N).EQ.12)
CIM END     <><><><><><>
C BAC START !!!!!!
C  read entrance exit and other loss coefficent if closed conduit type
        IF(NEQUAL.GE.2.AND.CLOSED(N)) THEN
          BACKSPACE N5
        IF (IPIPESED.EQ.0) THEN
          READ(N5,*,ERR=888) CC,ACOND(N),(KJUNC(N,K),K=1,2),QO(N),
     +         NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),ZU(N),ZD(N),
     +         ROUGH(N),STHETA(N),SPHI(N),ENTK(N),EXITK(N),OTHERK(N)
               SEDEPTH(N)=0.0
        ELSE
          READ(N5,*,ERR=888) CC,ACOND(N),(KJUNC(N,K),K=1,2),QO(N),
     +         NKLASS(N),AFULL(N),DEEP(N),WIDE(N),LEN(N),ZU(N),ZD(N),
     +         ROUGH(N),STHETA(N),SPHI(N),ENTK(N),EXITK(N),OTHERK(N),
     +         SEDEPTH(N)
        ENDIF
        ENDIF
C  BAC END !!!!!!!
      ENDIF
      IF(ROUGH(N).LE.0.0) ROUGH(N) = 0.014
C=======================================================================
C     IF LEN(N) IS NEGATIVE A FLAP GATE IS BEING MODELED
C        THIS FLAP GATE ONLY ALLOWS FLOW IN THE POSITIVE DIRECTION
C        INGATE = 1
C        FLOW IS LIMITED TO BE GREATER THAN 0.0
C=======================================================================
      INGATE(N) = 0
      IF(LEN(N).LT.0) THEN
                      INGATE(N) = 1
                      LEN(N)    = ABS(LEN(N))
                      ENDIF
C=======================================================================
C     IF NKLASS(N) IS NEGATIVE A FLAP GATE IS BEING MODELED
C        THIS FLAP GATE ONLY ALLOWS FLOW IN THE NEGATIVE DIRECTION
C        INGATE IS 2
C        FLOW IS LIMITED TO BE LESS THAN 0.0
C=======================================================================
      IF(NKLASS(N).LT.0) THEN
                         INGATE(N) = 2
                         NKLASS(N) = IABS(NKLASS(N))
                         ENDIF
C
C     IF NKLASS is not 6 7 8 or 12 then SPHI is maximum flow
C
C=======================================================================
CIM START  <><><><><><><><><>
CIM    THE FOLLOWING HOLD TRUE FOR INPUT ONLY, ARE MODIFIED
CIM    TO INTERNAL NUMBERING SCHEME AT END OF INDAT1 SUBROUTINE
CIM    Note:  In revised internal numbering scheme,
cim        o  Conduit types 1 through 20 are reserved
cim           for closed conduit types that use TWNORM, ANORM, and
cim           HRNORM ARRAYS
cim        o  Conduit types 21 through 50 are special types including
cim           rectangular pipes (22), and also includes parabolic and
cim           irregular sections (24) that use QCURVE arrays.
cimbridge     Now includes bridges type 25
cim        o  Conduit types 51 through 60 are reserved for equivalent
cim           orifice types.
CIM
CIM   NKLASS  TYPE                                NEWKLASS
C        1    CIRCULAR PIPE
                                          NEWKLASS(1)=1
C        2    RECTANGULAR PIPE
                                          NEWKLASS(2)=21
C        3    HORSESHOE PIPE
                                          NEWKLASS(3)=2
C        4    EGGSHAPED PIPE
                                          NEWKLASS(4)=3
C        5    BASKETHANDLE PIPE
                                          NEWKLASS(5)=4
C        6    TRAPEZOIDAL CHANNEL
                                          NEWKLASS(6)=22
C        7    PARABOLIC CROSS-SECTION OR POWER
C             FUNCTION CROSS SECTION
                                          NEWKLASS(7)=23
C        8    IRREGULAR CROSS-SECTION
                                          NEWKLASS(8)=24
C        9    HORIZONTAL ELLIPSE
C             (LONG AXIS IS HORIZONTAL)
                                          NEWKLASS(9)=5
C       10    VERTICAL ELLIPSE
                                          NEWKLASS(10)=6
C       11    ARCH
                                          NEWKLASS(11)=7
C       12    BRIDGE (TO BE IMPLEMENTED)
                                          NEWKLASS(12)=25
CIM    OOOOOOOOO
CIM  THESE ARE OLD ASSIGNMENTS AND NEW ASSIGNMENTS FOR ORIFICE
CIM  EQUIVALENT PIPES
C        9    SIDE OUTLET CIRCULAR ORIFICE          51
C       10    BOTTOM OUTLET CIRCULAR ORIFICE        52
C       11    SIDE OUTLET RECTANGULAR ORIFICE       53
C       12    BOTTOM OUTLET RECTANGULAR ORIFICE     54
CIM   OOOOOOOOOO
CIM END <><><><><><><><><><><><>
C=======================================================================
      KLASS = NKLASS(N)
CIM START   <><><><><><><><>
CIM       IF(KLASS.EQ.1.OR.KLASS.GE.9) THEN
CIM  9 WAS ORIFICE, DON'T KNOW DATA YET
c change this to select case
      SELECT CASE (KLASS)
      CASE (1)
CIM END   <><><><><><>
               RFULL(N)  = DEEP(N)/4.0
               AFULL(N)  = (3.1415926/4.0)*DEEP(N)**2
               WIDE(N)   = DEEP(N)
cim  CALL SEDEPTH1 to adjust pipe if IPIPESED=1
               IF (SEDEPTH(N).NE.0.0) THEN
                   CALL SEDEPTH1(N)
               ELSE
               SEDAREA(N) = 0.0
               SEDPERI(N) = 0.0
               SEDRAD(N) = 0.0
               ENDIF
      CASE (2)
               RFULL(N) = (WIDE(N)*DEEP(N))/(2.*WIDE(N)+2.0*DEEP(N))
               AFULL(N) =  WIDE(N)*DEEP(N)
      CASE (3)
               RFULL(N) = 0.25381  * DEEP(N)
      CASE (4)
               RFULL(N) = 0.19311 * DEEP(N)
      CASE (5)
               RFULL(N) = 0.28800*DEEP(N)
      CASE (6)
               AFULL(N)=DEEP(N)*(WIDE(N)+DEEP(N)/2.*(STHETA(N)+SPHI(N)))
               IF(WIDE(N).LE.0.0) WIDE(N) = 0.01
               RFULL(N)=AFULL(N)/(WIDE(N)+DEEP(N)*
     +                  (SQRT(1.0+STHETA(N)**2)+SQRT(1.0+SPHI(N)**2)))
      CASE (7)
      IF(STHETA(N).EQ.0.0) THEN
               AFULL(N) = 0.666666667*WIDE(N)*DEEP(N)
               X      = WIDE(N)/2.0
               X1     = WIDE(N)**4/(64.0*DEEP(N)**2)
               WETPER = 8.0*DEEP(N)/WIDE(N)**2*(X*SQRT(X1+X**2)
     +                             +  X1*LOG(X+SQRT(X1+X**2)) -
     +                                X1*LOG(SQRT(X1)))
               RFULL(N) = AFULL(N)/WETPER
               ELSE
               AFULL(N) = WIDE(N)*DEEP(N)*(1.0 - 1.0/(STHETA(N)+1.0))
               X      = WIDE(N)/2.0
               X1     = WIDE(N)**4/(64.0*DEEP(N)**2)
               WETPER = 8.0*DEEP(N)/WIDE(N)**2*(X*SQRT(X1+X**2)
     +                             +  X1*LOG(X+SQRT(X1+X**2)) -
     +                                X1*LOG(SQRT(X1)))
               RFULL(N) = AFULL(N)/WETPER
               ENDIF
CIM START <><><><><><><><>
CIM      HORIZONTAL ELLIPSE
      CASE (9)
      IF ((DEEP(N).LT.0.0).AND.(WIDE(N).LT.0.0)) THEN
        DEEP(N) = - DEEP(N)
        WIDE(N) = - WIDE(N)
        AFULL(N) = 1.2692 * DEEP(N) * DEEP(N)
        RFULL(N) = 0.3061 * DEEP(N)
        ELSE
        ISIZE = DEEP(N)
        IF ((ISIZE.LE.0).OR.(ISIZE.GT.NUMELL)) THEN
        IF (JCE.EQ.0)  THEN
            WRITE(N6,4070) NCOND(N)
            ELSE
            WRITE(N6,4071) ACOND(N)
        ENDIF
        ISTOP = ISTOP + 1
        ISIZE = 1
        END IF
      DEEP(N) = EMINOR(ISIZE)/12.0
      WIDE(N) = EMAJOR(ISIZE)/12.0
      AFULL(N) = EAREA(ISIZE)
      RFULL(N) = ERADIUS(ISIZE)
      END IF
CIM         VERTICAL ELLIPSE
      CASE (10)
        IF ((DEEP(N).GT.0.0).AND.(WIDE(N).LT.0.0)) THEN
        DEEP(N) = - DEEP(N)
        WIDE(N) = - WIDE(N)
        AFULL(N) = 1.2692 * WIDE(N) * WIDE(N)
        RFULL(N) = 0.3061 * WIDE(N)
        ELSE
        ISIZE = DEEP(N)
        IF ((ISIZE.LE.0).OR.(ISIZE.GT.NUMELL)) THEN
        IF (JCE.EQ.0)  THEN
            WRITE(N6,4070) NCOND(N)
        ELSE
            WRITE(N6,4071) ACOND(N)
        ENDIF
        ISTOP = ISTOP + 1
        ISIZE = 1
        END IF
      DEEP(N) = EMAJOR(ISIZE)/12.0
      WIDE(N) = EMINOR(ISIZE)/12.0
      AFULL(N) = EAREA(ISIZE)
      RFULL(N) = ERADIUS(ISIZE)
      END IF
CIM         ARCH
      CASE (11)
      IF ((DEEP(N).LT.0.0).AND.(WIDE(N).LT.0.0)) THEN
        DEEP(N) = - DEEP(N)
        WIDE(N) = - WIDE(N)
        AFULL(N) = 0.7879 * DEEP(N) * DEEP(N)
        RFULL(N) = 0.2991 * DEEP(N)
        ELSE
        ISIZE = DEEP(N)
        IF ((ISIZE.LE.0).OR.(ISIZE.GT.NUMARCH)) THEN
        IF (JCE.EQ.0) THEN
            WRITE(N6,4072) NCOND(N)
            ELSE
            WRITE(N6,4073) ACOND(N)
        ENDIF
        ISTOP = ISTOP + 1
        ISIZE = 1
        END IF
      DEEP(N) = AMINOR(ISIZE)/12.0
      WIDE(N) = AMAJOR(ISIZE)/12.0
      AFULL(N) = AAREA(ISIZE)
      RFULL(N) = ARADIUS(ISIZE)
      END IF
      END SELECT
CIM         BRIDGE  (TO BE IMPLEMENTED)
cim      IF(KLASS.EQ.12) THEN
cim      END IF
CIM END <><><><><><><><><><>
  260 CONTINUE
  280 NC  = N-1
      NTC = NC
C=======================================================================
C     PRINT OUT INFORMATION ON FLAP GATES (IF ANY EXIST)
C=======================================================================
      NFLAP     = 0
      DO 9000 N = 1,NC
      IF(INGATE(N).GT.0) THEN
                         NFLAP = NFLAP + 1
                         IF(NFLAP.EQ.1) WRITE(N6,9010)
                         ND = INGATE(N)
                         IF(JCE.EQ.0) THEN
                            WRITE(N6,9020) NCOND(N),GTYPE(ND)
                            ELSE
                            WRITE(N6,9021) ACOND(N),GTYPE(ND)
                         ENDIF
      IF (INGATE(N).EQ.1) WRITE(N6,4060)
      IF (INGATE(N).EQ.2) WRITE(N6,4061)
                         ENDIF
 9000 CONTINUE
c   printout data for pipes where maximum positive flow or the
c   minimum negative flow is limited as identified by SPHI and
c   or STHETA for all types but 6, 7, 8 and 12.
      NMXFLW = 0
      DO 9001 N=1,NC
      SELECT CASE (NKLASS(N))
      CASE (6,7,8,12)
      CASE DEFAULT
      IF(SPHI(N).NE.0.0.OR.STHETA(N).NE.0.0) THEN
      NMXFLW = NMXFLW + 1
      IF (NMXFLW.EQ.1) WRITE(N6,9011)
      IF (JCE.EQ.0) THEN
      IF (SPHI(N).NE.0.0) WRITE(N6,4064) NCOND(N),SPHI(N)
      IF (STHETA(N).NE.0.0) WRITE(N6,4065) NCOND(N),STHETA(N)
      ELSE
      IF (SPHI(N).NE.0.0) WRITE(N6,4074) ACOND(N),SPHI(N)
      IF (STHETA(N).NE.0.0) WRITE(N6,4075) ACOND(N),STHETA(N)
      END IF
      IF (SPHI(N).LT.0.0) WRITE(N6,4062)
      IF (STHETA(N).GT.0.0) WRITE(N6,4063)
      ENDIF
      END SELECT
 9001 CONTINUE
C=======================================================================
C     READ THE CS DATA GROUP TO DETERMINE IF THE NATURAL CROSS
C          SECTION INFORMATION WILL BE SAVED ON NSCRAT(4) OR
C          READ FROM NSCRAT(4) BY THE PROGRAM
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'CS') THEN
                     BACKSPACE N5
                     READ(N5,*,ERR=888) CC,IREAD
                     ELSE
                     BACKSPACE N5
                     ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS FROM THE SCRATCH FILE.
C=======================================================================
      IF(IREAD.EQ.'READ') THEN
               IF(NCSAVE.EQ.0) THEN
                               WRITE(N6,8999)
                               STOP
                               ENDIF
               READ(NCSAVE,ERR=8888) NATUR,NC
               READ(NCSAVE,ERR=8888) (NQC(N),N=1,NC)
               READ(NCSAVE,ERR=8888) (NUMQ(N),N=1,NC)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,1,J),J=1,26),N=1,NATUR)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,2,J),J=1,26),N=1,NATUR)
               READ(NCSAVE,ERR=8888) ((QCURVE(N,3,J),J=1,26),N=1,NATUR)
               DO 275 N = 1,NC
CIM START   <><><><><><><><>
CIM               IF(NKLASS(N).LT.7) GO TO 275
               IF(NKLASS(N).NE.8.OR.NKLASS(N).NE.7) GO TO 275
CIM END  <><><><><><><><>
               READ(NCSAVE,ERR=8888) SLOPE,AFULL(N),DEEP(N),WIDE(N),
     +                               LEN(N),ROUGH(N),RFULL(N),NKLASS(N)
  275          CONTINUE
               ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS, IF ANY.
C=======================================================================
      IF(IREAD.EQ.'SAVE'.OR.IREAD.EQ.'CARD') THEN
      IIPRNT = 0
      DO 285 N = 1,NC
      IF(NKLASS(N).EQ.8) THEN
            IF(IIPRNT.EQ.0) WRITE(N6,5355)
            IIPRNT = 1
            SLOPE = SPHI(N)
            NATUR = NATUR + 1
            KCOND = 0
            KSTOP = 0
            IF(STHETA(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 STHETA(N) = ABS(STHETA(N))
                                 ENDIF
            CALL GETCUR(N,STHETA(N),SLOPE,METRIC,0,KCOND,AFULL(N),
     +                  DEEP(N),WIDE(N),LEN(N),ROUGH(N),RFULL(N),
     +                  NSTOP,NCOND(N),ACOND(N),KSTOP)
            ENDIF
C=======================================================================
C     CALCULATE DATA FOR POWER FUNCTION CROSS SECTIONS
C=======================================================================
      IF(NKLASS(N).EQ.7) THEN
            KCOND    = 1
            NATUR    = NATUR + 1
            SLOPE    = 0.0
            KSTOP    = 0
            IF(STHETA(N).EQ.0.0) STHETA(N) = 2.0
            IF(STHETA(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 STHETA(N) = ABS(STHETA(N))
                                 ENDIF
            CALL GETCUR(N,STHETA(N),SLOPE,METRIC,0,KCOND,AFULL(N),
     +                  DEEP(N),WIDE(N),LEN(N),ROUGH(N),RFULL(N),
     +                  NSTOP,NCOND(N),ACOND(N),KSTOP)
            NKLASS(N) = 8
            ENDIF
  285 CONTINUE
      ENDIF
C
C=======================================================================
C   Call to read data for bridge sections if any
C=======================================================================
      CALL GETBRDGE
C=======================================================================
C     NOW SAVE THE DATA FOR IRREGULAR CHANNELS ON THE SCRATCH FILE.
C=======================================================================
      IF(IREAD.EQ.'SAVE') THEN
               IF(NCSAVE.EQ.0) THEN
                               WRITE(N6,8999)
                               STOP
                               ENDIF
               WRITE(NCSAVE,ERR=8888) NATUR,NC
               WRITE(NCSAVE,ERR=8888) (NQC(N),N=1,NC)
               WRITE(NCSAVE,ERR=8888) (NUMQ(N),N=1,NC)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,1,J),J=1,26),N=1,NATUR)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,2,J),J=1,26),N=1,NATUR)
               WRITE(NCSAVE,ERR=8888) ((QCURVE(N,3,J),J=1,26),N=1,NATUR)
               DO 295 N = 1,NC
CIM START   <><><><><><><>
CIM     note now both parabolic and irregular are nklass = 8
CIM            IF(NKLASS(N).LT.7) GO TO 295
               IF(NKLASS(N).NE.8) GO TO 295
CIM END   <><><><><><><>
               WRITE(NCSAVE,ERR=8888) SLOPE,AFULL(N),DEEP(N),WIDE(N),
     +                                LEN(N),ROUGH(N),RFULL(N),NKLASS(N)
  295          CONTINUE
               ENDIF
C=======================================================================
C     PRINT CONDUIT DATA
C=======================================================================
cim 1/99 print first header line
                             WRITE(N6,2999)
                             WRITE(N6,5060) ALPHA1,ALPHA2
                             IF(METRIC.EQ.1) WRITE(N6,5300)
                             IF(METRIC.EQ.2) WRITE(N6,5301)
      DO 300 N = 1,NC
      KLASS    = NKLASS(N)
CIM START      <><><><><><><>
      IF (KLASS.LT.1.OR.KLASS.GT.12) THEN
      IF (JCE.EQ.0) THEN
         WRITE(N6,7000) NCOND(N),NKLASS(N)
         ELSE
         WRITE(N6,7001) ACOND(N),NKLASS(N)
      END IF
       STOP 'INVALID NKLASS FOR CONDUIT'
      END IF
CIM END <><><><><><><><><>
CIM 1/99 Eliminate intermediate output headers JHEAD = 1
      IF(JHEAD.EQ.0.AND.MOD(N,50).EQ.0) THEN
                             WRITE(N6,2999)
                             WRITE(N6,5060) ALPHA1,ALPHA2
                             IF(METRIC.EQ.1) WRITE(N6,5300)
                             IF(METRIC.EQ.2) WRITE(N6,5301)
                             ENDIF
CIM 1/99
      IF(JCE.EQ.0) THEN
      IF(ZU(N).EQ.0.0.AND.ZD(N).EQ.0.0) THEN
        IF(NKLASS(N).EQ.6) WRITE(N6,5320) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5321) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2)
                 ELSE
        IF(NKLASS(N).EQ.6) WRITE(N6,5322) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),ZU(N),ZD(N),
     +                     STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5323) N,NCOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(NJUNC(N,K),K=1,2),ZU(N),ZD(N)
                ENDIF
      ELSE
      IF(ZU(N).EQ.0.0.AND.ZD(N).EQ.0.0) THEN
        IF(NKLASS(N).EQ.6) WRITE(N6,5420) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5421) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2)
                 ELSE
        IF(NKLASS(N).EQ.6) WRITE(N6,5422) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),ZU(N),ZD(N),
     +                     STHETA(N),SPHI(N)
        IF(NKLASS(N).NE.6) WRITE(N6,5423) N,ACOND(N),LEN(N),
     +                     CTYPE(KLASS),AFULL(N),ROUGH(N),WIDE(N),
     +                     DEEP(N),(KJUNC(N,K),K=1,2),ZU(N),ZD(N)
                ENDIF
      ENDIF

  300 CONTINUE
C  BAC START  !!!!!!!!
C=======================================================================
C     LENGTHEN CONDUITS AND/OR INCORPORATE LOCAL LOSSES INTO MANNING'S N
C     AS APPROPRIATE
C=======================================================================
C
C  SIMPLY REPLACE EXISTING CODE WITH THIS NEW CODE
      WRITE(N6,9030)
      VOLOLD   = 0.0
      VOLNEW   = 0.0
        DO 320 N=1,NC
            VOLOLD   = VOLOLD + AFULL(N)*LEN(N)
            IF(.NOT.CLOSED(N)) THEN
                NEWK = NEWKLASS(NKLASS(N))
                CALL HYDRAD(N,NEWK,DEEP(N),RMID,AMID,BMID)
                AB = AMID/BMID
                IF(AB.LE.0.0) AB = 0.01
            ELSE
               AB = DEEP(N)
            ENDIF
            RATIO = SQRT(AB*GRVT)*DELT/LEN(N)

        SELECT CASE (NEQUAL)
        CASE (1,5)
C
C     LENGTHEN SHORT PIPES WITHOUT INCORPORATING LOCAL LOSSES
C
            IF(RATIO.GT.1.0) THEN
               CLEN     = DELT*SQRT(AB*GRVT)
               ROUGH(N) = ROUGH(N)*SQRT(LEN(N)/CLEN)
               LEN(N)   = CLEN
               IF(JCE.EQ.0) THEN
                WRITE(N6,5335) NCOND(N),RATIO
                WRITE(N6,5337) NCOND(N),CLEN,ROUGH(N),RATIO
               ELSE
                WRITE(N6,5336) ACOND(N),RATIO
                WRITE(N6,5338) ACOND(N),CLEN,ROUGH(N),RATIO
               ENDIF
              ENDIF
         IF ((NEQUAL.EQ.5).AND.CLOSED(N)) THEN
               IF (JCE.EQ.0) THEN
                    WRITE(N6,6400) NCOND(N),ENTK(N),EXITK(N),OTHERK(N)
               ELSE
                    WRITE(N6,6401) ACOND(N),ENTK(N),EXITK(N),OTHERK(N)
               ENDIF
         ENDIF
C
C     INCORPORATE LOCAL LOSSES INTO MANNING'S N FOR CLOSED CONDUITS
C     BUT DO NOT ADJUST FOR SHORT CONDUITS
           CASE (2)
             IF(CLOSED(N)) THEN
               OLDROUGH = ROUGH(N)
               ROUGH(N) = ((ENTK(N)+EXITK(N)+OTHERK(N)+29.*ROUGH(N)*
     +                    ROUGH(N)*LEN(N)/RFULL(N)**(1.333333))*
     +                    RFULL(N)**(1.333333)/29./LEN(N))**0.5
C ****** BAC -- THIS IS A TEMPORARY WRITE TO SEE IF PROGRAM IN FUNCTIONING CORRECTLY
               IF(JCE.EQ.0) THEN
                            WRITE(N6,6335) NCOND(N),OLDROUGH,ENTK(N),
     +                                     EXITK(N),OTHERK(N),ROUGH(N)
               ELSE
                            WRITE(N6,6336) ACOND(N),OLDROUGH,ENTK(N),
     +                                     EXITK(N),OTHERK(N),ROUGH(N)
               ENDIF
             ENDIF
c  set additional losses to zero to avoid using again in _ROUTE routines
      ENTK(N) = 0.0
      EXITK(N) = 0.0
      OTHERK(N) = 0.0
      IF(RATIO.GT.1.0) THEN
        IF (JCE.EQ.0) THEN
         WRITE(N6,5335) NCOND(N),RATIO
        ELSE
         WRITE(N6,5336) ACOND(N),RATIO
        ENDIF
      ENDIF
C
C     INCORPORATE LOCAL LOSSES AND LENGTHEN SHORT CONDUITS
c
      CASE (3)
c    IF SHORT THEN DO THIS
               IF(RATIO.GT.1.0) THEN
                  CLEN     = DELT*SQRT(AB*GRVT)
C    IF NOT CLOSED AND SHORT THEN DO THIS
                 IF(.NOT.CLOSED(N)) THEN
                    ROUGH(N) = ROUGH(N)*SQRT(LEN(N)/CLEN)
               IF(JCE.EQ.0) THEN
                WRITE(N6,5335) NCOND(N),RATIO
                WRITE(N6,5337) NCOND(N),CLEN,ROUGH(N),RATIO
                ELSE
                WRITE(N6,5336) ACOND(N),RATIO
                WRITE(N6,5338) ACOND(N),CLEN,ROUGH(N),RATIO
               ENDIF
C    IF IT IS CLOSED AND SHORT THEN DO THE FOLLOWING
                  ELSE
                    OLDROUGH = ROUGH(N)
                    ROUGH(N) = ((ENTK(N)+EXITK(N)+OTHERK(N)+29.*
     +                         ROUGH(N)*ROUGH(N)*LEN(N)/RFULL(N)**
     +                         (1.333333))*RFULL(N)**(1.333333)/29./
     +                         CLEN)**0.5
               IF(JCE.EQ.0) THEN
c                WRITE(N6,5335) NCOND(N),RATIO
                WRITE(N6,6337) NCOND(N),OLDROUGH,CLEN,ENTK(N),EXITK(N),
     +                         OTHERK(N),ROUGH(N)
                ELSE
c                WRITE(N6,5336) ACOND(N),RATIO
                WRITE(N6,6338) ACOND(N),OLDROUGH,CLEN,ENTK(N),EXITK(N),
     +                         OTHERK(N),ROUGH(N)
                ENDIF
              ENDIF
                  LEN(N)   = CLEN
              ELSE
C    GO HERE IF NOT SHORT BUT CLOSED TO INCORPORATED ADDITIONAL LOSSES
C    INCORPORATE LOCAL LOSSES INTO MANNING'S N FOR CLOSED CONDUITS
C    BUT DO NOT ADJUST FOR SHORT CONDUITS
             IF(CLOSED(N)) THEN
               OLDROUGH = ROUGH(N)
               ROUGH(N) = ((ENTK(N)+EXITK(N)+OTHERK(N)+29.*ROUGH(N)*
     +                    ROUGH(N)*LEN(N)/RFULL(N)**(1.333333))*
     +                    RFULL(N)**(1.333333)/29./LEN(N))**0.5
C ****** BAC -- THIS IS A TEMPORARY WRITE TO SEE IF PROGRAM IN FUNCTIONING CORRECTLY
               IF(JCE.EQ.0) THEN
                            WRITE(N6,6335) NCOND(N),OLDROUGH,ENTK(N),
     +                                     EXITK(N),OTHERK(N),ROUGH(N)
               ELSE
                            WRITE(N6,6336) ACOND(N),OLDROUGH,ENTK(N),
     +                                     EXITK(N),OTHERK(N),ROUGH(N)
               ENDIF
             ENDIF
             ENDIF
c  set additional losses to zero to avoid using again in _ROUTE routines
      ENTK(N) = 0.0
      EXITK(N) = 0.0
      OTHERK(N) = 0.0
C
C      NEQUAL IS ZERO OR 4
C      ONLY CHECK FOR SHORT PIPES
C
      CASE (0,4)

      IF(RATIO.GT.1.0) THEN
         IF(JCE.EQ.0) THEN
           WRITE(N6,5335) NCOND(N),RATIO
         ELSE
           WRITE(N6,5336) ACOND(N),RATIO
         ENDIF
      ENDIF
         IF ((NEQUAL.EQ.4).AND.CLOSED(N)) THEN
               IF (JCE.EQ.0) THEN
                    WRITE(N6,6400) NCOND(N),ENTK(N),EXITK(N),OTHERK(N)
               ELSE
                    WRITE(N6,6401) ACOND(N),ENTK(N),EXITK(N),OTHERK(N)
               ENDIF
         ENDIF
      END SELECT
          VOLNEW   = VOLNEW + AFULL(N)*LEN(N)
  320   CONTINUE
C
      IF(NEQUAL.EQ.1.AND.METRIC.EQ.1) WRITE(N6,5339)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.1.AND.METRIC.EQ.2) WRITE(N6,5340)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.0.AND.METRIC.EQ.1) WRITE(N6,5341)  VOLOLD
      IF(NEQUAL.EQ.0.AND.METRIC.EQ.2) WRITE(N6,5342)  VOLOLD
      IF(NEQUAL.EQ.3.AND.METRIC.EQ.1) WRITE(N6,5339)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.3.AND.METRIC.EQ.2) WRITE(N6,5340)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.2.AND.METRIC.EQ.1) WRITE(N6,5341)  VOLOLD
      IF(NEQUAL.EQ.2.AND.METRIC.EQ.2) WRITE(N6,5342)  VOLOLD
      IF(NEQUAL.EQ.5.AND.METRIC.EQ.1) WRITE(N6,5339)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
      IF(NEQUAL.EQ.5.AND.METRIC.EQ.2) WRITE(N6,5340)
     +                                VOLOLD,VOLNEW,VOLNEW/VOLOLD
C  BAC END !!!!!
C=======================================================================
C     FIND JUNCTION DATA TO READ FROM GROUP D1.
C     THE FOLLOWING SEQUENCE IS REQUIRED IF NATURAL CHANNEL INFORMATION
C     WAS ENTERED IN THE DATA INPUT STREAM.  R.E.D., 4/92
C     Modified by CIM to eliminate artificial loop which could potentially
C     fail if may irregular sections.
C=======================================================================
C     DO 370 J=1,1000
 370  CONTINUE
      READ(N5,*,ERR=372) CC
C  don't know what C6 was before bridges were invoked
C     IF(CC.EQ.'D1'.OR.CC.EQ.'C6') THEN
            IF(CC.EQ.'D1') THEN
                                   BACKSPACE N5
                                   GO TO 375
                                   ENDIF
      GO TO 370
  372 WRITE(N6,*) 'D1 lines not found in input stream.'
      STOP 'D1 lines not found'
C 370 CONTINUE
  375 CONTINUE
C=======================================================================
C     READ JUNCTION DATA ON DATA GROUP D1
C=======================================================================
      WRITE(*,6050)
      DO 380 J = 1,NEE
  340 READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'D1'.AND.J.GT.1) GO TO 400
C These next lines should not be needed. cim 10/97
C      IF(J.EQ.1. AND.(CC.EQ.'C3'.OR.CC.EQ.'X1'.OR.
C     *                CC.EQ.'C4'.OR.CC.EQ.'GR')) THEN
C                                READ(N5,*,ERR=888) CC
C                                GO TO 340
C                                ENDIF
C modify to read optional xloc, yloc, and iwhich
C     
C     READ IWHICH IF BFFACT IS TRUE INDICATING THAT BD LINES WERE READ
      IF (BFFACT) THEN
         IF(JCE.EQ.0) THEN
           READ(N5,*,ERR=888) CC,JUN(J),GRELEV(J),
     +                        Z(J),QINST(J),Y(J),
     +                        XLOC(J),YLOC(J),
     +                        IWHICH(J),SURELEV(J)
         ELSE
           READ(N5,*,ERR=888) CC,AJUN(J),GRELEV(J),
     +                        Z(J),QINST(J),Y(J),
     +                        XLOC(J),YLOC(J),
     +                        IWHICH(J),SURELEV(J)
         ENDIF
	ELSE
C     DONT READ IWHICH IF BFFACT IS FALSE
         IF(JCE.EQ.0) THEN
           READ(N5,*,ERR=888) CC,JUN(J),GRELEV(J),
     +                        Z(J),QINST(J),Y(J),
     +                        XLOC(J),YLOC(J),
     +                        SURELEV(J)
         ELSE
           READ(N5,*,ERR=888) CC,AJUN(J),GRELEV(J),
     +                        Z(J),QINST(J),Y(J),
     +                        XLOC(J),YLOC(J),
     +                        SURELEV(J)
         ENDIF
	ENDIF
c  350 CONTINUE
	IF (JELEV.GE.2) Y(J) = Y(J) - Z(J)
      IF (IWHICH(J).EQ.0) IWHICH(J)=1
      IF (IWHICH(J).GT.NUMSETS) THEN
      WRITE(N6,*) 'ERROR - BASE FLOW FACTOR SET FOR JUNCTION (IWHICH)',
     .' IS GREATER THAN NUMBER OF SETS READ ON B9 LINES'
      IF (JCE.EQ.0) THEN
         WRITE(N6,*) ' JUNCTION = ',JUN(J),IWHICH(J)
      ELSE
         WRITE(N6,*) ' JUNCTION = ',AJUN(J),IWHICH(J)
      ENDIF
      STOP 'INCORRECT BASE FLOW FACTOR SET'
      ENDIF
      IF ((NUMSETS.GT.1).AND.(QINST(J).NE.0.0)) THEN
      IF (JCE.EQ.0) THEN
         WRITE(N6,*) 'JUNCTION ',JUN(J),
     .' USES BASE FLOW SET NUMBER ',IWHICH(J)
      ELSE
         WRITE(N6,*) 'JUNCTION ',AJUN(J),
     .' USES BASE FLOW SET NUMBER ',IWHICH(J)
      ENDIF
      ENDIF
      ZCROWN(J) = Z(J)
C     Set up junction connectivity array from pipe data.
C=======================================================================
      LOC      = 0
      IF(NC.GT.0) THEN
      DO 360 N = 1,NC
      DO 360 K = 1,2
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J)) THEN
                                            LOC          = LOC+1
	IF (LOC.GT.NCHN) THEN
	WRITE(N6,8040) J,JUN(J),NCHN
	NSTOP = NSTOP + 1
	LOC = NCHN
	ENDIF
                                            NCHAN(J,LOC) = N
                                            ENDIF
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J))THEN
                                            LOC          = LOC+1
	IF (LOC.GT.NCHN) THEN
	WRITE(N6,8045) J,AJUN(J),NCHN
	NSTOP = NSTOP +1
	LOC = NCHN
	ENDIF
                                            NCHAN(J,LOC) = N
                                            ENDIF
  360 CONTINUE
CIM   DO THIS CHECK AFTER PUMPS AND WEIRS... HAVE BEEN READ
CIM
CIM      IF(LOC.EQ.0) THEN
CIM                   IF(JCE.EQ.0) THEN
CIM                     WRITE(N6,5350) JUN(J)
CIM                   ELSE
CIM                     WRITE(N6,5351) AJUN(J)
CIM                   ENDIF
CIM                   JSKIP(J) = 1
CIM                   ENDIF
      ELSE
C=======================================================================
C     ZCROWN IS EQUAL TO THE GROUND ELEVATION FOR JUNCTIONS WITH
C            NO CONNECTING CONDUITS.
C=======================================================================
      ZCROWN(J) = GRELEV(J) - Z(J)
      IF(ZCROWN(J).LE.0.0) THEN
                           IF(JCE.EQ.0) THEN
                            WRITE(N6,8200)  JUN(J)
                           ELSE
                            WRITE(N6,8201) AJUN(J)
                           ENDIF
                           NSTOP = NSTOP + 1
                           ENDIF
      ENDIF
  380 CONTINUE
  400 NJ = J-1
C=======================================================================
C     CONVERT CONDUIT CONNECTIVITY NUMBERS TO INTERNAL SYSTEM
C     ASSIGN POSITIVE DOWNSTREAM FLOW CONVENTION
C=======================================================================
      IMOVE    = 0
      IF(NC.GT.0) THEN
	GRNDERROR = .FALSE.
      DO 600 N = 1,NC
      DO 540 K = 1,2
      DO 500 J = 1,NJ
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 520
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 520
  500 CONTINUE
      IF(JCE.EQ.0) THEN
       WRITE(N6,5390) NJUNC(N,K),NCOND(N)
      ELSE
       WRITE(N6,5391) KJUNC(N,K),ACOND(N)
      ENDIF
      NSTOP      = NSTOP + 1
  520 NJUNC(N,K) = J
  540 CONTINUE
      NL    = NJUNC(N,1)
      NH    = NJUNC(N,2)
      IF(JELEV.EQ.0) THEN
                     ZU(N) = Z(NL) + ZU(N)
                     ZD(N) = Z(NH) + ZD(N)
                     ELSE
                     IF(ZU(N).EQ.0.0) ZU(N) = Z(NL) + ZU(N)
                     IF(ZD(N).EQ.0.0) ZD(N) = Z(NH) + ZD(N)
                     ENDIF
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
cim bac added 11/97
C PREVENT FLAT CONDUITS
c      DO N = 1, NC
      IF(ABS(ZU(N)-ZD(N)).LE.0.0001) THEN
      ZU(N)=ZU(N)+.001
      IF (JCE.EQ.0) THEN
                        WRITE(N6,6207) NCOND(N)
                    ELSE
                        WRITE(N6,6208) ACOND(N)
      END IF
      END IF
c      END DO
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      IF(ZU(N)-ZD(N).LT.0.0) THEN
                             IMOVE      = IMOVE + 1
                             IF(JCE.EQ.1) THEN
                                          KDUM(1)    = KJUNC(N,1)
                                          KJUNC(N,1) = KJUNC(N,2)
                                          KJUNC(N,2) = KDUM(1)
                                          ENDIF
                             TEMP       = ZU(N)
                             ZU(N)      = ZD(N)
                             ZD(N)      = TEMP
                             QO(N)      = -QO(N)
                             NJUNC(N,1) = NH
                             NJUNC(N,2) = NL
                             NL         = NJUNC(N,1)
                             NH         = NJUNC(N,2)
                             IF(IMOVE.EQ.1) WRITE(N6,6200)
                             IF(JCE.EQ.0) THEN
                               WRITE(N6,6205) IMOVE,NCOND(N)
                             ELSE
                               WRITE(N6,6206) IMOVE,ACOND(N)
                             ENDIF
                             ENDIF
      IF((ZU(N) + DEEP(N)).GT.ZCROWN(NL))  ZCROWN(NL) = ZU(N)+DEEP(N)
      IF((ZD(N) + DEEP(N)).GT.ZCROWN(NH))  ZCROWN(NH) = ZD(N)+DEEP(N)
      IF(ZCROWN(NL).GT.GRELEV(NL)+0.001) THEN
	            GRNDERROR = .TRUE.
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5395) NCOND(N),JUN(NL)
                    ELSE
                      WRITE(N6,5396) ACOND(N),AJUN(NL)
                    ENDIF
c  update grelev to appropriate land elevation for printout in table below
                    GRELEV(NL) = ZCROWN(NL) + 0.01
                    NSTOP      = NSTOP + 1
                    ENDIF
      IF(ZCROWN(NH).GT.GRELEV(NH)+0.001) THEN
	            GRNDERROR = .TRUE.
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5395) NCOND(N),JUN(NH)
                    ELSE
                      WRITE(N6,5396) ACOND(N),AJUN(NH)
                    ENDIF
c  update grelev to appropriate land elevation for printout in table below
                    GRELEV(NH) = ZCROWN(NH) + 0.01
                    NSTOP      = NSTOP + 1
                    ENDIF
  600 CONTINUE
      IF (GRNDERROR) WRITE(N6,8000) 
      ENDIF
C=======================================================================
C     Print junction data.
C=======================================================================
CIM 1/99 print first header
                                   WRITE(N6,2999)
                                   WRITE(N6,5060) ALPHA1,ALPHA2
                                   IF(METRIC.EQ.1) WRITE(N6,5360)
                                   IF(METRIC.EQ.2) WRITE(N6,5361)
      DO 460 J = 1,NJ
      MPT      = 0
      NZP      = 0
      DO 420 I = 1,NCHN
      K1       = NCHAN(J,I)
      IF(K1.EQ.0)            GO TO 440
      IF(JCE.EQ.0) THEN
       IDUM(I) = NCOND(K1)
      ELSE
       KDUM(I) = ACOND(K1)
      ENDIF
      MPT                  = MPT + 1
C=======================================================================
C     Check for all conduits above the Junction invert.
C=======================================================================
                           JJ = 1
      IF(NJUNC(K1,1).EQ.J) JJ = 1
      IF(NJUNC(K1,2).EQ.J) JJ = 2
      IF(JJ.EQ.1.AND.ZU(K1).GT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.2.AND.ZD(K1).GT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.1.AND.ZU(K1).LT.Z(J)) NZP = NZP + 1
      IF(JJ.EQ.2.AND.ZD(K1).LT.Z(J)) NZP = NZP + 1
  420 CONTINUE
  440 CONTINUE
CIM 1/99 eliminate intermediate headers if JHEAD = 1
      IF (JHEAD.EQ.0.AND.MOD(J,50).EQ.0) THEN
                                   WRITE(N6,2999)
                                   WRITE(N6,5060) ALPHA1,ALPHA2
                                   IF(METRIC.EQ.1) WRITE(N6,5360)
                                   IF(METRIC.EQ.2) WRITE(N6,5361)
                                   ENDIF
      IF(MPT.GT.0) THEN
      IF(JCE.EQ.0) THEN
                   WRITE(N6,5380) J,JUN(J),GRELEV(J),ZCROWN(J),
     +                            Z(J),QINST(J),Y(J),(IDUM(K),K=1,MPT)
      ELSE
                   WRITE(N6,5381) J,AJUN(J),GRELEV(J),ZCROWN(J),
     +                            Z(J),QINST(J),Y(J),(KDUM(K),K=1,MPT)
      ENDIF
      IF(NZP.EQ.MPT) THEN
         IF(JCE.EQ.0) THEN
             WRITE(N6,5382)  JUN(J)
         ELSE
             WRITE(N6,5383) AJUN(J)
         ENDIF
      ENDIF
      ELSE
      IF(JCE.EQ.0) THEN
                   WRITE(N6,5380) J,JUN(J),GRELEV(J),
     +                            ZCROWN(J),Z(J),QINST(J),Y(J)
      ELSE
                   WRITE(N6,5381) J,AJUN(J),GRELEV(J),
     +                            ZCROWN(J),Z(J),QINST(J),Y(J)
      ENDIF
      ENDIF
	IF ((XLOC(J).NE.0.0).OR.
     1    (YLOC(J).NE.0.0).OR.
     2    (SURELEV(J).NE.0.0)) TABLE2 = .TRUE.
  460 CONTINUE
C     PRINT SECOND TABLE FOR ADDITIONAL JUNCTION DATA
C      X,Y, IWHICH, SURELEV      
      IF (TABLE2.OR.BFFACT) THEN
	IF (METRIC.EQ.1) THEN
	                   IREAD = '(FT)'
	ELSE
	                   IREAD = ' (M)'
	ENDIF
	IF (BFFACT) THEN
	    WRITE(N6,7360) IREAD
	ELSE
	    WRITE(N6,7365) IREAD
	ENDIF
      DO 480 J = 1,NJ
	IF (BFFACT) THEN
      IF(JCE.EQ.0) THEN    
                   WRITE(N6,7380) J,JUN(J),XLOC(J),YLOC(J),
     +                            IWHICH(J),SURELEV(J)
      ELSE
                   WRITE(N6,7381) J,AJUN(J),XLOC(J),YLOC(J),
     +                            IWHICH(J),SURELEV(J)
      ENDIF
	ELSE
      IF(JCE.EQ.0) THEN    
                   WRITE(N6,7385) J,JUN(J),XLOC(J),YLOC(J),
     +                            SURELEV(J)
      ELSE
                   WRITE(N6,7386) J,AJUN(J),XLOC(J),YLOC(J),
     +                            SURELEV(J)
      ENDIF
	ENDIF 
	IF ((SURELEV(J).NE.0.0).AND.(SURELEV(J).LT.GRELEV(J))) 
     1         WRITE(N6,7387)
  480 CONTINUE
      ENDIF
	DO J=1,NJ
	IF(SURELEV(J).EQ.0.0) SURELEV(J) = GRELEV(J)
	IF(SURELEV(J).LT.GRELEV(J)) SURELEV(J) = GRELEV(J)
	ENDDO
C=======================================================================
C     Check for high pipe and print a warning at the upstream end.
C=======================================================================
      DO 495 N = 1,NC
      J        = NJUNC(N,1)
      IF(ZU(N).EQ.Z(J)) GO TO 495
      ITEST     = 0
      JTEST     = 0
      DO 490 KK = 1,NCHN
      NKK       = NCHAN(J,KK)
      IF(NKK.EQ.N) GO TO 490
      IF(NKK.EQ.0.OR.NKK.GT.NC) GO TO 494
                            JJ = 1
      IF(NJUNC(NKK,1).EQ.J) JJ = 1
      IF(NJUNC(NKK,2).EQ.J) JJ = 2
      JTEST = JTEST + 1
      IF(JJ.EQ.1.AND.ZU(N).LE.ZU(NKK) + DEEP(NKK)) GO TO 490
      IF(JJ.EQ.2.AND.ZU(N).LE.ZD(NKK) + DEEP(NKK)) GO TO 490
      ITEST = ITEST + 1
  490 CONTINUE
  494 CONTINUE
      IF(ITEST.EQ.JTEST) THEN
          IF(JCE.EQ.0) THEN
             WRITE(N6,5392) NCOND(N),JUN(J)
          ELSE
             WRITE(N6,5393) ACOND(N),AJUN(J)
          ENDIF
      ENDIF
  495 CONTINUE
C=======================================================================
C     Check for high pipe and print a warning at the downstream end.
C=======================================================================
      DO 595 N = 1,NC
      J        = NJUNC(N,2)
      IF(ZD(N).EQ.Z(J)) GO TO 595
      ITEST     = 0
      JTEST     = 0
      DO 590 KK = 1,NCHN
      NKK       = NCHAN(J,KK)
      IF(NKK.EQ.N) GO TO 590
      IF(NKK.EQ.0.OR.NKK.GT.NC) GO TO 594
                            JJ = 1
      IF(NJUNC(NKK,1).EQ.J) JJ = 1
      IF(NJUNC(NKK,2).EQ.J) JJ = 2
      JTEST = JTEST + 1
      IF(JJ.EQ.1.AND.ZD(N).LE.ZU(NKK) + DEEP(NKK)) GO TO 590
      IF(JJ.EQ.2.AND.ZD(N).LE.ZD(NKK) + DEEP(NKK)) GO TO 590
      ITEST = ITEST + 1
  590 CONTINUE
  594 CONTINUE
      IF(ITEST.EQ.JTEST) THEN
        IF(JCE.EQ.0) THEN
          WRITE(N6,5392) NCOND(N),JUN(J)
        ELSE
          WRITE(N6,5393) ACOND(N),AJUN(J)
        ENDIF
      ENDIF
  595 CONTINUE
CIM START   <><><><><><><><>
CIM     CONVERT NKLASS FROM INPUT VALUES TO INTERNAL VALUES FOR LATER
CIM     COMPUTATIONS STORED IN NEWKLASS ARRAY
c
      BIG = 3.4E+38
      DO 650  N = 1,NC
      NKLASS(N) = NEWKLASS(NKLASS(N))
      IF (NKLASS(N).LE.21) THEN
      IF (STHETA(N).EQ.0.0) STHETA(N) = -BIG
      IF (SPHI(N).EQ.0.0)   SPHI(N) = BIG
      ENDIF
  650 CONTINUE
CIM END   <><><><><><>
C=======================================================================
      WRITE(*,6100)
      RETURN
 888  CALL IERROR
 8888 WRITE(N6,8887)
      STOP
C=======================================================================
  949 FORMAT(12F10.3)
  951 FORMAT(//,5X,'INTERMEDIATE OUTPUT WILL BE WRITTEN FOR THE',
     A' FOLLOWING PERIODS',/,
     B10X,' STARTING CYCLE   ENDING CYCLE')
  952 FORMAT(10X,2I15)
 2999 FORMAT(/,
     1       '1',40(2H--)/' ','ENVIRONMENTAL PROTECTION AGENCY',13X,40H*
     2***   EXTENDED TRANSPORT PROGRAM   ****,8X,'WATER RESOURCES DIVISI
     3ON',/,' ','WASHINGTON, D.C.            ',16X,4H****,32X,4H****,8X,
     4'CAMP DRESSER & MCKEE INC.',/,' ','                ',28X,4H****,
     56X,'   ANALYSIS MODULE  ',6X,4H****,8X,'ANNANDALE, VIRGINIA')
 5060 FORMAT(/,5X,A80,/,5X,A80,/)
 5100 FORMAT(/,' Control information for simulation',/,
     +       ' ----------------------------------',//,
     +       ' Integration cycles.................',I8,/)
 5120 FORMAT(' Length of integration step is......',F8.2,
     +       ' seconds',/,
     +       ' Simulation length..................',F8.2,' hours',/)
 5121 FORMAT(' Do not create equiv. pipes(NEQUAL).',I8,/)
C  BAC START
C5122 FORMAT(' Create equivalent conduits based ',/,
C    +       ' on the COURANT condition..........',I8,/)
 5122 FORMAT(' Create equivalent conduits based ',/,
     +       ' on the COURANT condition (no local',/,
     +       ' losses)............................',I8,/)
 5921 FORMAT(' Create equivalent conduits based ',/,
     +       ' on incorporating local losses (no',/,
     +       ' lengthening).......................',I8,/)
 5922 FORMAT(' Create equivalent conduits based ',/,
     +       ' on incorporating local losses and',/,
     +       ' on the COURANT condition...........',I8,/)
 5923 FORMAT(' Local losses read from C1 lines are ',/,
     +       ' factored into momentum equation (no',/,
     +       ' lengthening).......................',I8,/)
 5924 FORMAT(' Local losses read from C1 lines are ',/,
     +       ' factored into momentum equation and ',/,
     +       ' conduits are lengthened based on COURANT',/,
     +       ' condition..........................',I8,/)
C  BAC END
 5123 FORMAT(' Use U.S. customary units for I/O...',I8,/)
 5124 FORMAT(' Use metric units for I/O...........',I8,/)
C#### WCH, 7/25/96.  JREDO INSTEAD OF REDO.
 5140 FORMAT(' Printing starts in cycle...........',I8,//,
     +       ' Intermediate printout intervals of.',I8,' cycles',/,
     +       ' Intermediate printout intervals of.',F8.2,' minutes',//,
     +       ' Summary printout intervals of......',I8,' cycles',/,
     +       ' Summary printout time interval of..',F8.2,' minutes',//,
     +       ' Hot start file parameter (JREDO)...',I8,/)
 5160 FORMAT(' Initial time (TZERO)...............',F8.2,' hours')
C#### WCH, 7/25/96.
c 5161 FORMAT(' Time displacement from interface file starting date/time'
c     1,/,    ' when hot start is used (TZERO).....',F8.2,' hours')
 5161 FORMAT(' This is time displacement from JIN interface file ',
     1'starting date/time when interface is used.',/,
     1' This also describes starting hour in K3 card hydrograph',
     1' input when K3 cards are used.')
C#### WCH, 4/11/94.
 5163 FORMAT(' Initial date (default).............',I8,' (yr/mo/day)')
 5164 FORMAT(' Initial date (IDATZ)...............',I8,' (yr/mo/day)')
C#### WCH, 7/25/96.
 5165 FORMAT(' NOTE: Initial date from JIN interface file will be',
     +' used, if accessed.')
 5170 FORMAT(/,' Iteration variables: ITMAX.........',I8,/,
     1         '                      SURTOL........',F8.4,/)
 5175 FORMAT(' Default surface area of junctions....',F8.2,
     +       ' square feet.',/)
 5176 FORMAT(' Default surface area of junctions..',F6.2,
     +       ' square meters.',/)
 5177 FORMAT(' EXTRAN VERSION 3.3 SOLUTION. (ISOL = 0).',/,
     +       ' Sum of junction flow is zero during surcharge.',/)
 5178 FORMAT(' SEMI-IMPLICIT EXTRAN SOLUTION. (ISOL = 1).',/,
     +       ' NEWTON-RAPHSON SURCHARGE ITERATION.',/)
 5179 FORMAT(' ITERATIVE EXTRAN SOLUTION. (ISOL = 2).',/,
     +       ' NEWTON-RAPHSON SURCHARGE ITERATION.',/)
 5180 FORMAT(' NJSW INPUT HYDROGRAPH JUNCTIONS....',I6)
 5183 FORMAT(' NORMAL FLOW OPTION WHEN THE WATER  ',/,
     +       ' SURFACE SLOPE IS LESS THAN THE     ',/,
     +       ' GROUND SURFACE SLOPE (KSUPER=0)....',/)
 5184 FORMAT(' NORMAL FLOW OPTION WHEN THE FROUDE ',/,
     +       ' # IS GREATER THAN 1.0 (KSUPER=1)...',/)

CIM 1/99 CLEAN UP FOLLOWING TO PROVIDE BLANKS IF 10 CHARACTERS OR DIGITS
CIM ARE USED
 5200 FORMAT(/,' Printed output for the following',I3,
     +         ' Junctions',//,(5X,10(I10,1X)))
 5201 FORMAT(/,' Printed output for the following',I3,
     +         ' Junctions',//,(5X,10(A10,1X)))
 5220 FORMAT(/,' Printed output for the following',I3,
     +         ' Conduits',//,(5X,10(I10,1X)))
 5221 FORMAT(/,' Printed output for the following',I3,
     +         ' Conduits',//,(5X,10(A10,1X)))
 5240 FORMAT (/,' Water surface elevations will be plotted',
     +    ' for the following ',I3,' Junctions',//,(5X,10(I10,1X)))
 5241 FORMAT (/,' Water surface elevations will be plotted',
     +    ' for the following ',I3,' Junctions',//,(5X,10(A10,1X)))
 5260 FORMAT(/,' Flow rate will be plotted for the following ',
     +             I3,' Conduits',//,(5X,10(I10,1X)))
 5261 FORMAT(/,' Flow rate will be plotted for the following ',
     +             I3,' Conduits',//,(5X,10(A10,1X)))
 5265 FORMAT(/,' THE WATER SURFACE SLOPE WILL BE PLOTTED FOR THE ',
     +         'FOLLOWING ',I3,' CONDUITS',//,(5X,10(I10,1X)))
 5266 FORMAT(/,' THE WATER SURFACE SLOPE WILL BE PLOTTED FOR THE ',
     +         'FOLLOWING ',I3,' CONDUITS',//,(5X,10(A10,1X)))
CIM  WRITING OF RESULTS TO ASCII FILE
 5267 FORMAT(/,' FLOWS WILL BE WRITTEN FOR THE ',
     +         'FOLLOWING ',I5,' CONDUITS',/,
     +         ' FLOWS WILL BE WRITTEN EVERY ',i5,' TIME STEPS',/,
     +         ' MINIMUM FLOW EQUALS ',F10.3,' CFS',
     +          /,(5X,10(I10,1X)))
 5268 FORMAT(/,' FLOWS WILL BE WRITTEN FOR THE ',
     +         'FOLLOWING ',I5,' CONDUITS',/,
     +         ' FLOWS WILL BE WRITTEN EVERY ',I5,' TIME STEPS',/,
     +         ' MINIMUM FLOW EQUALS ',F10.3,' CFS',
     +         //,(5X,10(A10,1X)))
 8260 FORMAT(/,' FLOWS ARE WRITTEN TO STANDARD SWMM BINARY ',
     +'SEQUENTIAL UNFORMATTED FILE FOR INPUT TO STATS OR OTHER ',
     +'SWMM BLOCK')
 8261 FORMAT(/,' FLOWS ARE WRITTEN TO ASCII FORMATTED FILE')
 8263 FORMAT(/,' ABSOLUTE VALUE OF FLOWS ARE WRITTEN TO OUTPUT FILE')
 5269 FORMAT(' FLOWS IN CFS')
 7268 FORMAT(' FLOWS AT FOLLOWING ',I10,' CONDUITS WILL BE WRITTEN',
     &' WHEN FLOWS IN SECOND PIPE ARE GREATER THAN FLOWMIN',//,
     &10X,'FIRST PIPE',10X,'SECOND PIPE ')
 7269 FORMAT(10X,I10,10X,I10)
 7270 FORMAT('NOFDUP EQUALS ZERO')
 7271 FORMAT(10X,A10,10X,A10)
 5270 FORMAT(25X,100I14)
 5271 FORMAT(25X,100(4X,A10))
 5300 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                     Conduit Data                  *',/,
     +' *****************************************************',//,
     1'  INP  CONDUIT    LENGTH   CONDUIT     AREA    MANNING MAX WIDTH'
     1,'     DEPTH       JUNCTIONS          INVERT HEIGHT     TRAPEZOID'
     1,/,
     2'  NUM   NUMBER     (FT)      CLASS   (SQ FT)     COEF.   (FT)   '
     2,'      (FT)      AT THE ENDS        ABOVE JUNCTIONS   ',
     2'SIDE SLOPES',/,
     3' ----   ------   -------  --------   -------   ------- ---------'
     3,'     -----   -------   -------     ---------------   ',
     3'----- -----')
 5301 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                     Conduit Data                  *',/,
     +' *****************************************************',//,
     1'  INP  CONDUIT    LENGTH   CONDUIT     AREA    MANNING MAX WIDTH'
     1,'     DEPTH       JUNCTIONS          INVERT HEIGHT     TRAPEZOID'
     1,/,
     2'  NUM   NUMBER     (M)       CLASS    (SQ M)     COEF.   (M)    '
     2,'      (M)       AT THE ENDS        ABOVE JUNCTIONS   ',
     2'SIDE SLOPES',/,
     3' ----   ------   -------  --------   -------   ------- ---------'
     3,'     -----   -------   -------     ---------------   ',
     2'----- -----')
 5320 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2(I10,1X),
     a16X,2F7.2)
 5321 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2(I10,1X))
 5322 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2(I10,1X)
     a,2F8.2,2F7.2)
 5323 FORMAT(I4,I10,F10.0,A10,F10.2,F10.5,F10.2,F10.2,2(I10,1X)
     a,2F8.2)
 5335 FORMAT(' ===> WARNING !!! (C*DELT/LEN) IN CONDUIT',
     .       I10,' IS',F5.1,' AT FULL DEPTH.')
 5336 FORMAT(' ===> WARNING !!! (C*DELT/LEN) IN CONDUIT ',
     .       A10,' IS',F5.1,' AT FULL DEPTH.')
 5337 FORMAT(' ===> THE NEW EQUIVALENT LENGTH/ROUGHNESS/RATIO FOR COND',
     +'UIT ',I10,' IS ',F10.1,' / ',F10.7,' / ',F5.1)
 5338 FORMAT(' ===> THE NEW EQUIVALENT LENGTH/ROUGHNESS/RATIO FOR COND',
     +'UIT ',A10,' IS ',F10.1,' / ',F10.7,' / ',F5.1)
 5339 FORMAT(/,
     +' ****************************************',/,
     +' *  Equivalent Conduit Volume Analysis  *',/,
     +' ****************************************',//,
     +' Input full depth volume............',1PE14.4,' cubic feet',/,
     +' New full depth volume..............',1PE14.4,' cubic feet',/,
     +' New volume / Old volume ratio......',0PF14.4)
 5340 FORMAT(/,
     +' ****************************************',/,
     +' *  Equivalent Conduit Volume Analysis  *',/,
     +' ****************************************',//,
     +' Input full depth volume............',1PE14.4,' cubic meters',/,
     +' New full depth volume..............',1PE14.4,' cubic meters',/,
     +' New volume / Old volume ratio......',0PF14.4)
 5341 FORMAT(/,
     +' ********************',/,
     +' *  Conduit Volume  *',/,
     +' ********************',//,
     +' Input full depth volume............',1PE14.4,' cubic feet')
 5342 FORMAT(/,
     +' ********************',/,
     +' *  Conduit Volume  *',/,
     +' ********************',//,
     +' Input full depth volume............',1PE14.4,' cubic meters')
C 5350 FORMAT(/,'===> WARNING !!! JUNCTION',I10,
C     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT.')
C 5351 FORMAT(/,'===> WARNING !!! JUNCTION',A10,
C     1         ' IS NOT ASSOCIATED WITH ANY CONDUIT.')
 5355 FORMAT(//,'$$$ FOR NATURAL CHANNELS WITH MULTIPLE DEPRESSIONS OR V
     *ARIABLE MANNINGS N,',/,'    FLOW IS SUM OF FLOWS IN DEPRESSIONS AN
     *D/OR VARIABLE-N SEGMENTS.',/,'    THIS IS EQUIVALENT TO SUMMING CO
     *NVEYANCES ACROSS THE CHANNEL. $$$'//)
 5360 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                  Junction Data                    *',/,
     +' *****************************************************',//,
     1' INP  JUNCTION    GROUND    CROWN     INVERT     QINST   INITIAL'
     +,'     CONNECTING CONDUITS',/,
     3' NUM    NUMBER     ELEV.     ELEV.     ELEV.       CFS DEPTH(FT)'
     3,/,' ---    ------   -------   -------    ------   ------- ------'
     3,'---     -------------------')
 5361 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *                  Junction Data                    *',/,
     +' *****************************************************',//,
     1' INP  JUNCTION    GROUND    CROWN     INVERT     QINST   INITIAL'
     +,'     CONNECTING CONDUITS',/,
     3' NUM    NUMBER     ELEV.     ELEV.     ELEV.       CMS  DEPTH(M)'
     3,/,' ---    ------   -------   -------    ------   ------- ------'
     3,'---   -------------------')
 5380 FORMAT(I4,I10,F10.2,4F10.2,3X,8(I7,1X))
 5381 FORMAT(I4,1X,A10,F9.2,4F10.2,3X,5(A10,1X),/,67X,3(A10,1X))
 5382 FORMAT(' ===> Warning all conduits connecting to Junction ',I10,
     +       ' lie above the Junction invert.')
 5383 FORMAT(' ===> Warning all conduits connecting to Junction ',A10,
     +       ' lie above the Junction invert.')
 5390 FORMAT(/,' ===> ERROR !!! JUNCTION',I10,' ON CONDUIT',I10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5391 FORMAT(/,' ===> ERROR !!! JUNCTION',A10,' ON CONDUIT ',A10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5392 FORMAT(' ===> WARNING !!!  THE INVERT OF ',
     *'CONDUIT ',I10,' LIES ABOVE THE CROWN OF ALL CONDUITS',
     *' AT JUNCTION ',I10)
 5393 FORMAT(' ===> WARNING !!!  THE INVERT OF ',
     *'CONDUIT ',A10,' LIES ABOVE THE CROWN OF ALL CONDUITS',
     *' AT JUNCTION ',A10)
 5395 FORMAT(/,' ===> ERROR !!!  CONDUIT',I10,' HAS CAUSED ZCROWN OF',
     1       ' JUNCTION',I10,' TO LIE ABOVE THE SPECIFIED GROUND ELEV.')
 5396 FORMAT(/,' ===> ERROR !!!  CONDUIT ',A10,' HAS CAUSED ZCROWN ',
     1   'OF JUNCTION ',A10,' TO LIE ABOVE THE SPECIFIED GROUND ELEV.')
 5420 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,
     +                                 2X,2(A10,1X),16X,2F7.2)
 5421 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,2(A10,1X))
 5422 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,2(A10,1X),
     +                                           2F8.2,2F7.2)
 5423 FORMAT(I4,1X,A10,F9.0,A10,F10.2,F10.5,F10.2,F10.2,2X,
     +2(A10,1X),2F8.2)
 6000 FORMAT(/,' Reading conduit data.')
 6050 FORMAT(/,' Reading junction data.')
 6100 FORMAT(/,' Reading remaining simulation data.')
 6200 FORMAT(/,' ===> Warning !! The upstream and downstream junctions',
     +' for the following conduits',/,
     +'                 have been reversed to correspond to the',
     +' positive flow and decreasing',/
     +,'                 slope EXTRAN convention.  A negative flow in',
     +' the output thus means ',/,
     +'                 the flow was from your original upstream',
     +' junction to your original',/,
     +'                 downstream junction.  Any initial flow has',
     +' been multiplied by -1.',/)
 6205 FORMAT(13X,I5,'.  Conduit #...',I10,'  has been changed.')
 6206 FORMAT(13X,I5,'.  Conduit #...',A10,'  has been changed.')
change  11/97
 6207 FORMAT(12x,' Conduit #...',I10,' has zero slope.',
     a'  0.001 feet added to upstream invert.')
 6208 FORMAT(12x,' Conduit #...',A10,' has zero slope.',
     a'  0.001 feet added to upstream invert.')
change
C  BAC START -- TEMPORARY FORMAT STATEMENTS
 6335 FORMAT(' EQUIVALENT N COMPUTED FOR CONDUIT ',I10,/,
     a5X,'INPUT N =',F7.4,' ENTK =',F7.4,' EXITK =',F7.4,
     b' OTHERK =',F7.4,' ADJUSTED N = ',F7.4)
 6336 FORMAT(' EQUIVALENT N COMPUTED FOR CONDUIT ',A10,/,
     a5X,'INPUT N =',F7.4,' ENTK =',F7.4,' EXITK =',F7.4,
     b' OTHERK =',F7.4,' ADJUSTED N = ',F7.4)
 6337 FORMAT(' EQUIVALENT N COMPUTED FOR CONDUIT ',I10,/,
     a5X,'INPUT N =',F7.4,' NEW LENGTH =',F10.2,' ENTK =',F7.4,
     b' EXITK =',F7.4,' OTHERK =',F7.4,' ADJUSTED N = ',F7.4)
 6338 FORMAT(' EQUIVALENT N COMPUTED FOR CONDUIT ',A10,/,
     a5X,'INPUT N =',F7.4,' NEW LENGTH =',F10.2,' ENTK =',F7.4,
     b' EXITK =',F7.4,' OTHERK =',F7.4,' ADJUSTED N = ',F7.4)
 6400 FORMAT(' LOSSES INCORPORATED INTO MOMENTUM EQUATION FOR CONDUIT '
     a,I10,/,' ENTK =',F7.4,' EXITK =',F7.4,' OTHERK =',F7.4)
 6401 FORMAT(' LOSSES INCORPORATED INTO MOMENTUM EQUATION FOR CONDUIT '
     a,A10,/,' ENTK =',F7.4,' EXITK =',F7.4,' OTHERK =',F7.4)
C  BAC END
CIM CIM START  <><><><><><><><>
 7000 FORMAT(' ERROR - CONDUIT TYPE NOT VALID FOR CONDUIT ID = '
     A,  I10,' INPUT NKLASS = ',I10)
 7001 FORMAT(' ERROR - CONDUIT TYPE NOT VALID FOR CONDUIT ID = '
     A,  A10,' INPUT NKLASS = ',I10)
CIM END   <><><><><><><><><><><>
 7022 FORMAT(10X,'JUNCTION VOLUMES ARE COMPUTED USING SUBROUTINE',
     1' VOLUME')
 7024 FORMAT(10X,'JUNCTION VOLUMES ARE COMPUTED BY TRACKING TOTAL',
     1' VOLUMES DURING THE SIMULATION')
 7360 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *           Additional Junction Data                *',/,
     +' *****************************************************',//,
     147X,'BASEFLOW  SURCHARGE',/,
     2' INP  JUNCTION',34X,'FACTOR   ELEVATION',/,
     3' NUM    NUMBER',12X,'XLOC',11X,'YLOC     SET',7X,A4,/,
     4' ---    ------   -------------   ------------  --------',
     5'  --------')
 7380 FORMAT(I4,1X,I10,2F15.2,I10,F10.2)
 7381 FORMAT(I4,1X,A10,2F15.2,I10,F10.2)
 7365 FORMAT(/,1H1,/,
     +' *****************************************************',/,
     +' *           Additional Junction Data                *',/,
     +' *****************************************************',//,
     147X,'SURCHARGE',/,
     2' INP  JUNCTION',33X,'ELEVATION',/,
     3' NUM    NUMBER',12X,'XLOC',11X,'YLOC',5X,A4,/,
     4' ---    ------   -------------   ------------  --------')
 7385 FORMAT(I4,1X,I10,2F15.2,F10.2)
 7386 FORMAT(I4,1X,A10,2F15.2,F10.2)
 7387 FORMAT(15X,'WARNING : MAXIMUM SURCHARGE ELEVATION IS LESS',
     1' THAN GROUND ELEVATION.  PROGRAM WILL RESET TO EQUAL GROUND ',
     2'ELEVATION')
C
 8020 FORMAT(/,5x,'INTERMEDIATE HEADER LINES ARE PRINTED AS IN',
     a' ORIGINAL PROGRAM')
 8022 FORMAT(/,5x,'INTERMEDIATE HEADER LINES ARE EXCLUDED FROM',
     a' JUNCTION AND CONDUIT INPUT AND OUTPUT SUMMARY TABLES')
 8024 FORMAT(/,5x,'IDS ARE WRITTEN AS IN ORIGINAL PROGRAM ')
 8026 FORMAT(/,5x,'ALL 10 CHARACTERS AND DIGITS IN IDS ARE',
     a' WRITTEN THROUGHOUT PROGRAM')
 8184 FORMAT(/,5x,'JELEV = 0 (DEFAULT). STANDARD INPUTS ARE DEPTHS NOT '
     a,'ELEVATIONS')
 8185 FORMAT(/,5x,'JELEV = 1, ZP1 AND ZP2 ON CONDUIT DATA INPUT LINES',
     a' ARE READ AS ELEVATIONS IN PLACE OF DEPTHS')
 8186 FORMAT(/,5x,'JELEV = 2, FOLLOWING DATA ARE READ AS ELEVATIONS ',
     A'IN PLACE OF DEPTHS',/,
     A5x,'    -  ZP1 AND ZP2 ON CONDUIT DATA INPUT LINES',/,
     B5x,'    -  INITIAL WATER SURFACE ELEVATION (YO) ON D1 LINES')
 8187 FORMAT(/,5x,'JELEV = 3, FOLLOWING DATA ARE READ AS ',
     A'ELEVATIONS IN PLACE OF DEPTHS',/,
     A5x,'    -  ZP1 AND ZP2 ON CONDUIT DATA INPUT LINES',/,
     B5x,'    -  INITIAL WATER SURFACE ELEVATION (YO) ON D1 LINES',/,
     c5x,'    -  SURFACE AREA / ELEVATION DATA ON E2 LINES')
 8188 FORMAT(/,5x,'JELEV = 4, DEPTHS ARE READ IN PLACE OF ELEVATIONS ',
     a'THROUGHOUT PROGRAM INPUT.',/,
     a5x,'THESE ARE CONVERTED TO DEPTHS IN THE PROGRAM.')
 8200 FORMAT(' ===> ERROR !!! THE GROUND ELEVATION OF JUNCTION ',I10,
     *           ' LIES ABOVE THE JUNCTION INVERT ELEVATION.')
 8201 FORMAT(' ===> ERROR !!! THE GROUND ELEVATION OF JUNCTION ',A10,
     *           ' LIES ABOVE THE JUNCTION INVERT ELEVATION.')
 8887 FORMAT(/,' ERROR !!  READING OR WRITING NATURAL CHANNEL',
     +         ' INFORMATION ON NSCRAT(4).')
 8999 FORMAT(/,' ERROR !!  THE NATURAL CHANNEL INFORMATION WAS',
     +         ' NOT PRESENT ON THE NSCRAT(4) FILE.')
 9010 FORMAT(/,
     +' ************************************************************',/,
     +' *               Flap Gate Conduit Information              *',/,
     +' *               -----------------------------              *',/,
     +' * POSITIVE FLAP GATE - FLOW ONLY ALLOWED FROM THE UPSTREAM *',/,
     +' *                      TO THE DOWNSTREAM JUNCTION          *',/,
     +' * NEGATIVE FLAP GATE - FLOW ONLY ALLOWED FROM THE          *',/,
     +' *                      DOWNSTREAM TO THE UPSTREAM JUNCTION *',/,
     +' ************************************************************',//
     +,'    Conduit   Type of Flap Gate',/,
     + '    -------   -----------------')
 9020 FORMAT(1X,I10,A20)
 9021 FORMAT(1X,A10,A20)
 4060 FORMAT(' Flows in conduit will be limited to be greater than '
     a,'zero.')
 4061 FORMAT(' Flows in conduit will be limited to be less than '
     a,'zero.')
 9030 FORMAT(/)
 9011 FORMAT(//,5x,'WARNING OF POSSIBLE ERROR',/,
     a5X,'THE FOLLOWING CLOSED CONDUITS HAD NONZERO DATA ENTERED',
     a' FOR SPHI AND/OR STHETA',/,
     a5X,'EXTRAN NOW INTERPRETS THESE DATA AS THE MAXIMUM POSITIVE',
     a' AND/OR NEGATIVE CONDUIT FLOWS AND WILL LIMIT FLOWS IN THESE',
     a' CONDUITS ACCORDINGLY',/,
     A5X,'PLEASE MAKE SURE THAT THESE WERE NOT ENTERED BY MISTAKE',//,
     a5X'MAXIMUM POSITIVE OR NEGATIVE FLOWS IN THE FOLLOWING CONDUITS'
     b,/,5X,
     c'WILL BE LIMITED AS SPECIFIED BY SPHI AND STHETA ON C1 LINES'
     d,//,5X,
     e'   CONDUIT   FLOW LIMIT',/,1X,
     f'----------   --------------------')
 4064 FORMAT(5X,I10,
     a' Flows will be limited to be less than ',F10.3)
 4065 FORMAT(5X,I10,
     a' Flows will be limited to be greater than',F10.3)
 4074 FORMAT(5X,A10,
     a' Flows will be limited to be less than ',F10.3)
 4075 FORMAT(5X,A10,
     a' Flows will be limited to be greater than',F10.3)
 4062 FORMAT(11X,' WARNING - Entering negative SPHI to limit flow ',
     a'may produce unexpected results')
 4063 FORMAT(11X,' WARNING - Entering positive STHETA to limit flow ',
     a'may produce unexpected results')
 9050 FORMAT(/,
     +       ' With steady-state dry-weather flow computations',/
     +       '   Maximum steady-state outflow             =', F10.3,
     +       ' cfs',/,
     +       '   Maximum steady-state flow imbalance      =', F10.3/
     +       '   Maximum change in flow at any location   =', F10.3,
     +       ' cfs',/)
 9051 FORMAT(/,
     +       ' With steady-state dry-weather flow computations',/
     +       '   Maximum steady-state outflow             =', F10.3,
     +       ' cms',/,
     +       '   Maximum steady-state flow imbalance      =', F10.3/
     +       '   Maximum change in flow at any location   =', F10.3,
     +       ' cms',/)
 9060 FORMAT(' Surcharge Computations Use Preissman Slot Method'/)
 4070 FORMAT(' ERROR - ELLIPTICAL PIPE SIZE (DEEP) ENTERED FOR ',
     .'CONDUIT ',I10,' IS NOT VALID')
 4071 FORMAT(' ERROR - ELLIPTICAL PIPE SIZE (DEEP) ENTERED FOR ',
     .'CONDUIT ',A10,' IS NOT VALID')
 4072 FORMAT(' ERROR - ARCH PIPE SIZE (DEEP) ENTERED FOR ',
     .'CONDUIT ',I10,' IS NOT VALID')
 4073 FORMAT(' ERROR - ARCH PIPE SIZE (DEEP) ENTERED FOR ',
     .'CONDUIT ',A10,' IS NOT VALID')
 5137 FORMAT(' ERROR - Detailed ASCII output was requested using a ',
     a'negative INTER but JOUT is Zero',/,
     b'         Please set JOUT to nonzero and assign ',
     b'file name.')
 5138 FORMAT(' ERROR -  Detailed ASCII outut file is opened',
     a' as a scratch file',/,
     a'          Please use @ line to save ASCII results for',
     b' unit associated with JOUT = ',I10)
 5139 FORMAT(/,5x,'INTER was entered as a negative number.',/,
     a'  Intermediate output is written to JOUT in ASCII format')
 5186 FORMAT(/,5X,'JDOWN = 0 - Minimum of normal or critical depth will'
     a,' be used at free outfalls (I1).')
 5187 FORMAT(/,5X,'JDOWN = 1 - Critical depth will be used at free ',
     a'outfall conduits.')
 5188 FORMAT(/,5X,'JDOWN = 2 - Normal depth will be used at free ',
     a'outfall conduits.')
 5189 FORMAT(/,5X,' The number of RATE/VRATE pairs will be read on',
     a' H1 lines as defined by IPRATE on BB line.')
 5190 FORMAT(/,5X,'Characteristic depth for M2 and S2 water ',
     a'surface profiles will be computed as in previous versions ',
     b'of EXTRAN (IM2 = 0).')
 5191 FORMAT(/,5X,'Characteristic depth for M2 and S2 water ',
     a'surface profiles computed using tabular correction ',
     b'(IM2 = 1).')
 5192 FORMAT(/,5X,'SEDIMENT DEPTHS WILL BE READ FROM C1 LINES',/,
     a5x,'AREA, DEPTH, AND ZP ON CONDUIT DATA PRINTOUT HAVE',
     b' BEEN ADJUSTED FOR SEDIMENT DEPTH.',/,5X,
     c'NOTE THAT THIS OPTION HAS BEEN IMPLEMENTED ONLY FOR ',
     d'CIRCULAR CONDUITS')
 5193 FORMAT(/,5X,'SEDIMENT DEPTHS WILL NOT BE READ FROM C1 LINES')
 6666 FORMAT(' Start time for hydraulic output....',F6.1,' hours'/
     &       ' Time interval for hydraulic output.',F6.1,' seconds'/)
 7010 FORMAT(/,5X,'CONDUIT LENGTHS ON C1 LINE MUST EQUAL IRREGULAR',
     a' SECTION LENGTH ENTERED ON THE C3 OR X1 LINES (IWLEN = 0)')
 7020 FORMAT(/,5X,'PROGRAM USES IRREGULAR SECTION LENGTHS SPECIFIED',
     a' ON THE C3 OR X1 LINES (IWLEN = 1)')
 7030 FORMAT(/,5X,'PROGRAM USES IRREGULAR SECTION LENGTHS SPECIFIED',
     a' ON THE C1 LINES (IWLEN = 2)')
 8000 FORMAT(/,5X,'ERRORS WERE FOUND WHERE THE GROUND ELEVATION IS',
     1' LESS THAT THE PIPE CROWN ELEVATION',/,
     25X,'GROUND ELEVATIONS 0.01 FEET ABOVE MAXIMUM CROWN ELEVATIONS',
     3' ARE LISTED IN THE FOLLOWING JUNCTION DATA TABLE',/,
	45X,'CORRECT GROUND ELEVATION AND RUN AGAIN')
 8005 FORMAT(/,5X,'Intermediate continuity output will not be created')
 8010 FORMAT(/,5X,'Intermediate continuity output will be created',
     a' every ',I5,' time steps.')
C check for NCHN 9/1999  C. Moore
 8040 FORMAT(/,5X,'ERROR - TOO MANY CONDUITS ENTERING JUNCTION',I5,' - '
     a,I10,/,5X,'MAXIMUM ALLOWED CURRENTLY EQUALS ',I5,/,5X,
     b'EITHER MODIFY NETWORK OR CHANGE NCHN PARAMETER AND',
     c' RECOMPILE PROGRAM')
 8045 FORMAT(/,5X,'ERROR - TOO MANY CONDUITS ENTERING JUNCTION',I5,' - '
     a,I10,/,5X,'MAXIMUM ALLOWED CURRENTLY EQUALS ',I5,/,5X,
     b'EITHER MODIFY NETWORK OR CHANGE NCHN PARAMETER AND',
     c' RECOMPILE PROGRAM')
C ---
C=======================================================================
      END
