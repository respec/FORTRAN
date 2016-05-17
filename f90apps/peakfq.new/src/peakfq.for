C
C
C
C     PROGRAM   PEAKFQBATCH
      SUBROUTINE PEAKFQ (INPUTARG)
      !DEC$ ATTRIBUTES DLLEXPORT :: PEAKFQ
C
C     + + + PURPOSE + + +
C     Batch version of PEAKFQ program.
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C     
      USE EMAThresh
      USE CompSpecs
C
C     + + + DUMMY ARGUMENTS + + +      
      CHARACTER(LEN=*) :: INPUTARG
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxint.inc'
      INCLUDE 'pmesfl.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpkdsn.inc'
      INCLUDE 'clunit.inc'
      INCLUDE 'cjobop.inc'
      INCLUDE 'cwcf0.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,STAIND,IOSNUM,FUNIT
      INTEGER      IPEND, WDMSFL, FOUT, PAUSE
      INTEGER      LNSPECS, NSTA, ISTA, RETCOD, RDOFLG, FE
      INTEGER      DSN, DSTYP, GRCNT, LREC, ERRFLG
      CHARACTER*12 APNAME
      CHARACTER*8  LGNAME
      CHARACTER*64 FNAME, VERSN
      CHARACTER*120 S, KWD, IOSTXT
      CHARACTER*240 SPCFNM
      CHARACTER*120, ALLOCATABLE :: SPECS(:), LSPECS(:)
      LOGICAL      LFLAG, OPENED
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT, CVRINT
      CHARACTER*120 STRRETREM
      INTEGER      INFOERROR
C
C     + + + INTRINSICS + + +
      INTRINSIC    INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, CVRINT, STRRETREM, J407XE, JFLUSH, WDBOPN
C      EXTERNAL     GPOPEN, GPINIT, ANINIZ, 
      EXTERNAL     WRITESPECIO, UPDATESPECFILE
      EXTERNAL     WDDSNX, WDSCHK
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A120)
C
C     + + + END SPECIFICATIONS + + +
C
      SPCFNM= INPUTARG
C
      FE= 99     
      INQUIRE(UNIT=FE,OPENED=OPENED)
      IF (.NOT.OPENED) THEN
        OPEN(UNIT=FE,FILE='ERROR.FIL') 
      END IF
C      
C     avoid some lahey math errors
C     LFLAG = .TRUE.
C     CALL INVALOP (LFLAG)
C     CALL UNDFL (LFLAG)
C
C     Initialize user environment
C     version info for unix what
C     names of application, message file, and log file
      INCLUDE 'fpeak.inc'
c      CALL ANINIZ (MESSFL, FNAME, LGNAME)

C     open the old WDM message file (read only if possible)
      RDOFLG= 1
      CALL WDBOPN (MESSFL,FNAME,RDOFLG,
     O             ERRFLG)
      IF (ERRFLG.NE.0) THEN
C       bad wdm file
        WRITE(FE,*) 'Bad WDM file:',ERRFLG,WDNAME,FILNAM
        WRITE(FE,*)  'Bad WDM file:',ERRFLG,WDNAME,FILNAM
        STOP
      END IF
C
C     init graphics
c      CALL GPOPEN (FE)
C     always doing some graphics (BMP at a minimum)
c      CALL GPINIT
C
C     assume not going to update the specification file
      UPDATEFG = .FALSE.
C
C     init options
Ckmf? what do these next 4 variables do?
Cprh  previously set in OPNOUT, used sporatically in J407XE.FOR
Cprh  found in common JOBOPT
      IPPOS = 1
      ISKUDP= 0
      NOXPA = 0
      NOCLM = 0
C
C     init output options
      WEIBA = 0.0  !common WCFCM0
C     common JOBOPT (IPLTOP through ALLSOM)
      IPLTOP= 0
      IPRTOP= 1
      IBCPUN= 0
      IDEBUG= 0
      IMODFG= 1  
      ALLSOM= 1
      IEXTEND= 0
      PAUSE = 2    !don't pause between stations
C     assume no export or empirical output files
      EXPFUN= 0
      EMPFUN= 0
C
C     open scratch file
      OPEN (UNIT=91,FILE='RQ7J4ZV9',STATUS='UNKNOWN')
C
CC     open the old WDM message file (read only if possible)
C      MESSFL= 9
C      FILNAM= '\HASSAPPS\PEAKFQ.NEW\BIN\PKFQMS.WDM'
C      RDOFLG= 1
C      CALL WDBOPN (MESSFL,FILNAM,RDOFLG,
C     O             RETCOD)
C      IF (RETCOD.NE.0) THEN
CC       bad wdm file
C        WRITE(91,*) 'Bad Message File:',RETCOD,FILNAM
C        WRITE(FE,*)  'Bad Message File:',RETCOD,FILNAM
C        STOP
C      END IF

      SPCFUN = 11
C     get driver input file from command line arguement
C      CALL GETCL(SPCFNM)
      OPEN(SPCFUN,FILE=SPCFNM,IOSTAT=IOSNUM,ERR=5)
C     successful open of spec file
      WRITE(FE,*) "MAIN:Reading Specification file: ",SPCFNM
      GO TO 8

 5    CONTINUE !problem opening spec file
C        CALL IOSTAT_MSG(IOSNUM,IOSTXT)
        write(FE,*) "Unable to open Specification file: ",SPCFNM
        GO TO 999

 8    CONTINUE

C     scan file for stations
      write(FE,*) "Scanning Spec file for stations. Spec file contents:"
      NSTA = 0
      DO        !loop to count stations to process
        READ(SPCFUN,1000,IOSTAT=IOSNUM,END=10) S
        write(FE,*) "  " // S
        CALL UPCASE(S)
        IF (INDEX(S(1:14), 'STATION') .GT. 0) THEN
          NSTA = NSTA + 1
        END IF
      END DO
 10   CONTINUE  !get here on end of file
      write(FE,*) "Finished scan of Spec file."

      REWIND(SPCFUN,IOSTAT=IOSNUM,ERR=20)
c      write(FE,*) "Just did REWIND of spec file."
      GO TO 30

 20   CONTINUE !get here on REWIND error
c        write(FE,*) "Prblem with REWIND, IO: ",S

 30   CONTINUE

      IF (ALLOCATED(STASPECS)) THEN
        DEALLOCATE(STASPECS)
      END IF
      
      IF (NSTA .GT. 0) THEN
        WRITE (FE,*) "MAIN:Found ",NSTA," Stations"
        ALLOCATE (STASPECS(NSTA))
        ALLSOM = 2 !only doing specified stations
      ELSE  !all stations, no updates to specifications
        WRITE (FE,*) "MAIN:No Stations Found - Do All"
      END IF

      ISTA  = 0
      IPEND = 0
C     process driver input file
      DO 
        IF (IPEND .EQ. 0) THEN !read next record
          READ(SPCFUN,1000,IOSTAT=IOSNUM,END=120) S
          WRITE(FE,*) "MAIN:Process Record:'" // TRIM(S) // "'"
          CALL UPCASE(S)
          KWD = STRRETREM(S)
        ELSE !have record pending to process
          IPEND = 0 !reset
        END IF

        IF (KWD .EQ. 'I') THEN !input spec
          WRITE(FE,*) "MAIN:Got I, Remaining:'" // TRIM(S) // "'"
          CALL OPNINP
     M               (S,WDMSFL,INCRD,INFORM,RETCOD)
        ELSE IF (KWD .EQ. 'O') THEN !output spec
          WRITE(FE,*) "MAIN:Got O, Remaining:'" // TRIM(S) // "'"
          CALL OPNOUT
     M               (S, INFORM, FOUT, IPUNCH,
     M                IPLTOP, GRFMT, IPRTOP, IBCPUN, IDEBUG,
     M                CLSIZE, WEIBA, EXPFUN, EMPFUN, IEXTEND,
     O                RETCOD)
        ELSE IF (KWD .EQ. 'STATION') THEN !processing station specs
          WRITE(FE,*) "MAIN:Got STATION, Remaining:'" // TRIM(S) // "'"
          LNSPECS= 0
          ISTA   = ISTA + 1
          STASPECS(ISTA)%ID = TRIM(S)
          IF (ISTA.GT.1) THEN !look for duplicate station IDs
            I = ISTA - 1
            DO WHILE (I.GT.0)
              J = ZLNTXT(STASPECS(ISTA)%ID)
              IF (STASPECS(I)%ID(1:J).EQ.STASPECS(ISTA)%ID) THEN
C               same station ID, add an index
                K = ZLNTXT(STASPECS(I)%ID)
                IF (K.GT.J) THEN 
C                 this station already has an index, increment it
                  STAIND = CVRINT(STASPECS(I)%ID(J+2:K)) + 1
                  IF (STAIND.LT.10) THEN
                    WRITE(STASPECS(ISTA)%ID,'(I1)') 
     $                TRIM(STASPECS(ISTA)%ID) // "-",STAIND
                  ELSE
                    WRITE(STASPECS(ISTA)%ID,'(I2)') 
     $                TRIM(STASPECS(ISTA)%ID) // "-",STAIND
                  END IF
                ELSE !first duplicate of this station
                  STASPECS(ISTA)%ID = TRIM(STASPECS(ISTA)%ID) // "-1"
                END IF
                WRITE(FE,*) "Duplicate Station ID: updated original ",
     $                     TRIM(S)," to be ",STASPECS(ISTA)%ID
                I = 0  !exit loop
              END IF
              I = I - 1
            END DO
          END IF
          WRITE (FE,*) "MAIN:Assigned " // STASPECS(ISTA)%ID //
     $                " to index ", ISTA," of STASPECS"
          DO WHILE (IPEND .EQ. 0)  !loop for station specs
            READ(SPCFUN,1000,END=90) S
            CALL UPCASE(S)
            WRITE(FE,*) "MAIN:Process RecordX:'" // TRIM(S) // "'"
            KWD = STRRETREM(S)
            IF (KWD.EQ.'STATION'.OR.KWD.EQ.'I'.OR.KWD.EQ.'O'.OR.
     $          KWD.EQ.'UPDATE') THEN
C             some other spec, end specs for this station
              IPEND = 1
            ELSE
C             add record to this station's specs
              WRITE(FE,*) "MAIN:Add Record To Station Specs"
              S = TRIM(KWD) // ' ' // TRIM(S)
              IF (LNSPECS .GT. 0) THEN !make copy of existing specs
                WRITE(FE,*) "MAIN:Add to existing specs"
                ALLOCATE (LSPECS(LNSPECS))
                DO 70 I = 1, LNSPECS
                  LSPECS(I) = SPECS(I) 
 70             CONTINUE
                DEALLOCATE (SPECS)
              END IF
              LNSPECS = LNSPECS + 1
              WRITE(FE,*) "MAIN:LNSPECS:", LNSPECS
              ALLOCATE (SPECS(LNSPECS))
              IF (LNSPECS .GT. 1) THEN
                DO 80 I = 1,LNSPECS-1
                  SPECS(I) = LSPECS(I) !put copy back
 80             CONTINUE
                DEALLOCATE(LSPECS)
              END IF
              WRITE (FE,*) "MAIN:Assign spec:'" // TRIM(S) // "'"
              SPECS(LNSPECS) = S !assign new spec
            END IF
          END DO

 90       CONTINUE !get here on EOF within station specs

          IF (LNSPECS.GT.0) THEN !specs exist for this station
            ALLOCATE(STASPECS(ISTA)%SPECS(LNSPECS))
            STASPECS(ISTA)%NSPECS = LNSPECS
            WRITE(FE,*) "MAIN:", STASPECS(ISTA)%NSPECS,
     $                 " spec(s) for station ",STASPECS(ISTA)%ID
            DO 100 I = 1,LNSPECS
              STASPECS(ISTA)%SPECS(I)%STR = SPECS(I)
              WRITE(FE,*) "  Spec", I, ": '" //
     $                 TRIM(STASPECS(ISTA)%SPECS(I)%STR) // "'"
 100        CONTINUE
            DEALLOCATE(SPECS)
          END IF
        ELSE IF (KWD .EQ. 'UPDATE') THEN 
C         update spec file with verbose specifications
          UPDATEFG = .TRUE.
        END IF
      END DO
 120  CONTINUE  !get here on EOF (or other error)
C        CALL IOSTAT_MSG(IOSNUM,IOSTXT)
        WRITE(FE,*) "Done reading spec file"  !, status : ",IOSTXT

      IF (INFORM.EQ.1) THEN !populate DSN buffer
        IF (NSTA.GT.0) THEN !get from station specs
          DO 150 I = 1,NSTA
            DSNBUF(I) = CVRINT(STASPECS(I)%ID)
 150      CONTINUE
          DSNCNT = NSTA
        ELSE !no specs, find all available data sets
          DSTYP = 2
          DSNCNT = 0
          DSN = 1
          DO WHILE (DSN.GT.0)
            CALL WDDSNX (WDMSFL,
     M                   DSN)
            IF (DSN.GT.0) THEN
              CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O                     LREC,GRCNT,RETCOD)
              IF (RETCOD.EQ.0) THEN
                DSNCNT = DSNCNT + 1
                DSNBUF(DSNCNT) = DSN
              END IF
              DSN = DSN + 1
            END IF
          END DO
        END IF
        write(FE,*) 'MAIN: Analyzing',DSNCNT,' WDM data sets.'
        write(FE,*) 'MAIN: DSNs are ',(DSNBUF(I),I=1,DSNCNT)
      END IF

      IF (RETCOD .EQ. 0) THEN !run the analyses
        write(FE,*) 'MAIN:calling J407XE...'
C       do analysis, reset dataset pointer to zero
        DSNIND   = 0
C       set all other J407 common block variables
        MOROPT(1)= 0
        MOROPT(2)= 0
        JOBTTL   = ' '
        MSG1     = FOUT
        INFIL2   = 5
C       set printer plot file to FOUT
        CALL PLTAPE (FOUT)
        IF (UPDATEFG) THEN !write out verbose spec file
          CALL WRITESPECIO (WDMSFL,INCRD,INFORM,FOUT,IPUNCH,
     I                      IPLTOP,GRFMT,IPRTOP,IBCPUN,IDEBUG,
     I                      CLSIZE,WEIBA,EXPFUN,EMPFUN,IEXTEND)
        END IF
C       do the analysis
        CALL J407XE (MESSFL,WDMSFL,PAUSE,UPDATEFG,NSTA)
        IF (UPDATEFG) THEN !update spec file with verbose version
          CALL UPDATESPECFILE (SPCFUN,SPCFNM)
c        ELSE !just close spec file
c          CLOSE(SPCFUN)
        END IF
      ELSE !major problem processing input data/specs
        WRITE (FE,*) "PeakFQ NOT RUN: Problem processing ",
     $              "input data or specifications."
        WRITE (FE,*) "Review log file to determine problem."
      END IF
C
 999  CONTINUE
C       get sent here if major problem encountered
c        IF (ZLNTXT(IOSTXT).GT.0) THEN
C         write out IO Status text
c          WRITE(FE,*) "  IO Status:  ",IOSTXT
c        END IF
C
      CLOSE(MESSFL)
C
C     close GKS
Ckmf  gpclos closes unit 99 and then calls gclks.  gclks
Ckmf  (as well as later code in this program) may still
Ckmf  want to write to 99.
Ckmf  CALL GPCLOS (FE)
c      CALL GCLKS
C
C     write out any errors read on input file and close output file
      CALL JFLUSH (91,FOUT)
C
C     don't see where output file is closed, try it here
      INQUIRE(FOUT,NAME=S)
      write(FE,*) "Closing output file " // TRIM(S)
      CLOSE(FOUT)

C     always close spec file
      CLOSE(SPCFUN)
C     and error file
      CLOSE(FE)      
C
      IF (IPUNCH .EQ. 15) THEN
C       need to close BCD output file
        CLOSE(IPUNCH)
      END IF
C     close export/empirical output files
      IF (EXPFUN .GT. 0) THEN
        CLOSE(EXPFUN)
      END IF
      IF (EMPFUN .GT. 0) THEN
        CLOSE(EMPFUN)
      END IF
C
C     close and delete unused log file
c      S = TRIM(LGNAME)//'.LOG'
c      INQUIRE(FILE=S,NUMBER=FUNIT)
c      CLOSE(FUNIT,STATUS='DELETE')
C
C      CALL ANCLOS (MESSFL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNINP
     M                   ( ISTR, WDMSFL, INCRD,
     O                     INFORM, RETCOD )
C
C     + + + PURPOSE + + +
C     Get peak flow data for input
C
C     + + + HISTORY + + +
C     updated for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL, INCRD,
     $             INFORM, RETCOD
      CHARACTER*120 ISTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR   - input specification string from batch input file
C     WDMSFL - Fortran unit number of wdm file containing/for peak data
C     INCRD  - Fortran unit number for Watstore card-image input
C     INFORM - indicator flag
C              1 - input from wdm file or terminal
C              2 - input from file containing WATSTORE card-image format
C     RETCOD -
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cpkdsn.inc'
C
C     + + + LOCAL VARIABLES + + +
Cprh      INTEGER      L0, L1, IDCNT, IDLEN(20), IDLNG
      CHARACTER*64 WDMNAME
      CHARACTER*120 KWD
C
C     + + + FUNCTIONS + + +
      CHARACTER*120 STRRETREM
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRRETREM, WDBOPN
C
C     DATA INITIALIZATIONS + + +
Cprh      DATA   IDCNT, IDLEN, IDLNG, L0, L1
Cprh     $     /    20, 20*15,   300,  0,  1 /
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
C
C     input options:  WDM, ASCII FILE
      KWD = STRRETREM(ISTR)
      IF (KWD.EQ.'WDM') THEN
C       input peak data from wdm file
        WDMSFL = 12
        WDMNAME= TRIM(ISTR(1:64))
        CALL WDBOPN (WDMSFL,WDMNAME,0,
     O               IRET)
        IF (IRET.EQ.0) THEN !successful open of WDM file
          WRITE(99,*) "OPNINP:Successful Open WDM file:'"
     $               // WDMNAME // "'"
Cprh          IDATA = 1
          INFORM = 1
        ELSE !WDM file not opened
          !LOG IT
          WRITE(99,*) "OPNINP:FAILED open of WDM file'"
     $               // WDMNAME // "'"
          RETCOD = -1
        END IF
      ELSE IF (KWD.EQ.'ASCI') THEN
C       input peak data from WATSTORE formatted file
        INCRD = 13
        OPEN(INCRD,FILE=ISTR,ERR=10,STATUS='OLD')
C       successful open of Watstore file
        WRITE(99,*) "OPNINP:Successful Open Watstore file:'"
     $             // TRIM(ISTR) // "'"

Cprh        IDATA = 3
        INFORM = 2
        GO TO 20

 10     CONTINUE !get here on error opening Watstore file
          !LOG IT
          WRITE(99,*) "OPNINP:FAILED open of Watstore file'"
     $               // TRIM(ISTR) // "'"
          RETCOD = -2
Cprh          IDATA = 0

 20     CONTINUE

      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNOUT
     I                   ( ISTR, INFORM,
     M                     FOUT, IPUNCH,
     M                     IPLTOP, GRFMT, IPRTOP, IBCPUN, IDEBUG,
     M                     CLSIZE, WEIBA, EXPFUN, EMPFUN, IEXTEND,
     O                     RETCOD )
C
C     + + + PURPOSE + + +
C     Modify processing options.
C
C     + + + HISTORY + + +
C     updated for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      INFORM, FOUT, IPUNCH,
     $             IPLTOP, IPRTOP, IBCPUN, IDEBUG, 
     $             EXPFUN, EMPFUN, IEXTEND, RETCOD
      REAL         CLSIZE, WEIBA
      CHARACTER*3  GRFMT
      CHARACTER*120 ISTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR   - input specification string from batch input file
C     INFORM - ???
C     FOUT   - ???
C     IPUNCH - ???
C     IPLTOP - ???
C     GRFMT  - format of graphic file (BMP, CGM, or WMF)
C     IPRTOP - ??? 
C     IBCPUN - ???
C     IDEBUG - ???
C     CLSIZE - ??? 
C     WEIBA  - ???
C     EXPFUN - export file unit number
C     EMPFUN - empirical file unit number
C     IEXTEND- extended annual exceedence percentiles
C     RETCOD - ???
C
C     + + + LOCAL VARIABLES + + +
Cprh      INTEGER   AGAIN, RTCMND
Cprh     $          I, L0, LEN1, LEN2, LEN5
      CHARACTER*1  ISTR1(120)
      CHARACTER*120 KWD
C
C     + + + FUNCTIONS + + +
      INTEGER   IYESNO, ZLNTXT
      REAL      CHRDEC
      CHARACTER*120 STRRETREM
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   IYESNO, ZLNTXT, STRRETREM, CVARAR, CHRDEC
C
C     + + + DATA INITIALIZATIONS + + +
Cprh      DATA   SCLU, L0, LEN1, LEN2, LEN5
Cprh     $     /  121,  0,    1,    2,    5 /
C
C     + + + END SPECIFICATIONS + + +
C
C     input options:  FILE, OPTIONS
      KWD = STRRETREM(ISTR)
      IF (KWD.EQ.'FILE') THEN
        FOUT = 14
        OPEN (FOUT,FILE=ISTR,ERR=10)
C       successful open of output file
        !LOG IT
        WRITE(99,*) "OPENOUT:Opened Output File:'" 
     $               // TRIM(ISTR) // "'"
        GO TO 20

 10     CONTINUE !get here on error opening output file
          FOUT = 0
          !LOG IT
          WRITE(99,*) "OPENOUT:FAILED to Open Output File:'"
     $               // TRIM(ISTR) // "'"

 20     CONTINUE

      ELSE IF (KWD.EQ.'PLOT') THEN !get next plot keyword
c        WRITE(99,*) "OPNOUT:PLOT:'" // TRIM(ISTR) // "'"
        KWD = STRRETREM(ISTR)
        IF (KWD.EQ.'STYLE') THEN
c          WRITE(99,*) "OPNOUT:PLOT:STYLE:'" // TRIM(ISTR) // "'"
          IF (ISTR.EQ.'GRAPHICS') THEN 
c            WRITE(99,*) "OPNOPT:PLOT:STYLE:GRAPHICS"
            IPLTOP = 1
          ELSE IF (ISTR.EQ.'PRINTER') THEN 
c            WRITE(99,*) "OPNOPT:PLOT:STYLE:PRINTER"
            IPLTOP = 2
          ELSE IF (ISTR.EQ.'BOTH') THEN 
c            WRITE(99,*) "OPNOPT:PLOT:STYLE:BOTH"
            IPLTOP = 3
          ELSE 
c            WRITE(99,*) "OPNOPT:PLOT:STYLE:NONE"
            IPLTOP = 0
          END IF
        ELSE IF (KWD.EQ.'FORMAT') THEN
          GRFMT = TRIM(ISTR)
          IF (GRFMT.NE.'BMP' .AND. GRFMT.NE.'CGM' .AND. 
     $        GRFMT(1:2).NE.'PS' .AND. GRFMT.NE.'WMF') THEN
C           not a valid graphic format
c            WRITE(99,*) "OPNOUT:PLOT:FORMAT: Graphic Format '" // 
c     $                  GRFMT //"' unknown - NO GRAPHIC PLOTS"
            GRFMT = ''
          END IF
        ELSE IF (KWD.EQ.'PRINTPOS') THEN
          IPRTOP = IYESNO(ISTR,1)
        ELSE IF (KWD.EQ.'POSITION') THEN
          ILEN = ZLNTXT(ISTR)
          IF (ILEN.GT.0) THEN
            CALL CVARAR (ILEN,ISTR,ILEN,ISTR1)
            WEIBA = CHRDEC(ILEN,ISTR1)
            IF (WEIBA.LT.0.0 .OR. WEIBA.GT.1.0) THEN
              !LOG IT
            END IF
          END IF
        END IF
      ELSE IF (KWD.EQ.'ADDITIONAL') THEN
        IF (ISTR(1:3).EQ.'NON') THEN
          IBCPUN = 0
        ELSEIF (ISTR(1:3).EQ.'WDM') THEN 
          IBCPUN = 1
        ELSE IF (ISTR(1:3).EQ.'WAT') THEN 
          IBCPUN = 2
        ELSE IF (ISTR(1:3).EQ.'TAB') THEN 
          IBCPUN = 4
        ELSE IF (ISTR(1:3).EQ.'BOT') THEN  
C         determine which format for text file output
          KWD = STRRETREM(ISTR)
          IF (ISTR(1:3).EQ.'WAT') THEN
            IBCPUN = 3
          ELSE IF (ISTR(1:3).EQ.'TAB') THEN
            IBCPUN = 5
          ELSE !no format specified, assume tab-separated
            WRITE(99,*) "OPNOUT: No text file format specified ",
     $                  "on ADDITIONAL BOTH specification record"
            WRITE(99,*) "Assuming output format is Tab-separated"
            IBCPUN = 5
          END IF
        ELSE 
C         assume default format (tab-separated)
          IBCPUN = 4
        END IF
c      write(99,*) "Processing ADDITIONAL:  IBCPUN ",IBCPUN
        IF (IBCPUN.GE.2) THEN !open output file
          KWD = STRRETREM(ISTR)
c      write(99,*) "Processing ADDITIONAL:  KWD,ISTR : "
c     $            // TRIM(KWD) // ", " // TRIM(ISTR)
          IF (ZLNTXT(ISTR).EQ.0) THEN !try using contents of KWD as file name
            ISTR = KWD
          END IF
          IF (ZLNTXT(ISTR).GT.0) THEN !file name should be remaining text
            IPUNCH = 15
            OPEN (IPUNCH,FILE=ISTR,ERR=30)
C           successful open of output basin characteristics file
c            WRITE(99,*) "OPENOUT:Opened Additional Output File: '" 
c     $                   // TRIM(ISTR) // "'"
            GO TO 40

 30         CONTINUE !get here on error opening output file
              !LOG IT
              WRITE(99,*) "OPENOUT:FAILED to Open Additional Output ",
     $                    "File:'" // TRIM(ISTR) // "'"
C             dummy default (following old code, prh 8/03)
              IPUNCH = 7

 40         CONTINUE
          ELSE !no file name
            WRITE(99,*) "OPNOUT: No File Name specified for ",
     $                  "Additional output"
            IBCPUN = MOD(IBCPUN,2)
          END IF
        END IF

      ELSE IF (KWD.EQ.'EXPORT') THEN
        EXPFUN = 16
        OPEN (EXPFUN,FILE=ISTR,ERR=50)
C       successful open of Export output file
        !LOG IT
        WRITE(99,*) "OPENOUT:Opened EXPORT Output File:'" 
     $               // TRIM(ISTR) // "'"
        GO TO 60

 50     CONTINUE !get here on error opening output file
          EXPFUN = 0
          !LOG IT
          WRITE(99,*) "OPENOUT:FAILED to Open EXPORT Output File:'"
     $               // TRIM(ISTR) // "'"

 60     CONTINUE

      ELSE IF (KWD.EQ.'EMPIRICAL') THEN
        EMPFUN = 17
        OPEN (EMPFUN,FILE=ISTR,ERR=70)
C       successful open of Empirical output file
        !LOG IT
        WRITE(99,*) "OPENOUT:Opened EMPIRICAL Output File:'" 
     $               // TRIM(ISTR) // "'"
        GO TO 80

 70     CONTINUE !get here on error opening output file
          EMPFUN = 0
          !LOG IT
          WRITE(99,*) "OPENOUT:FAILED to Open EMPIRICAL Output File:'"
     $               // TRIM(ISTR) // "'"

 80     CONTINUE

      ELSE IF (KWD.EQ.'EXTENDED') THEN
        IEXTEND = IYESNO(ISTR,1)
      ELSE IF (KWD.EQ.'DEBUG') THEN
        IDEBUG = IYESNO(ISTR,1)
      ELSE IF (KWD.EQ.'CONFIDENCE') THEN
        ILEN = ZLNTXT(ISTR)
        IF (ILEN.GT.0) THEN
          CALL CVARAR (ILEN,ISTR,ILEN,ISTR1)
          CLSIZE = CHRDEC(ILEN,ISTR1)
        END IF
      END IF
C     check specs
      IF (INFORM.EQ.0) THEN !should specify input specs before output specs
         !LOG IT
      END IF
      IF (MOD(IBCPUN,2) .EQ. 1) THEN
C       WDM or BOTH for output
        IF (INFORM .NE. 1) THEN
C         but input not from WDM
          !LOG IT
          IBCPUN = IBCPUN - 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE PARSESTASPECS
     I                        (STAID,XSYSPK,XHSTPK,
     M                         GENSKU,HISTPD,QHIOUT,QLWOUT,LOTYPE,
     M                         GAGEB,RMSEGS,IBEGYR,IENDYR,
     M                         ISKUOP,IKROPT,FLAT,FLONG,EMAOPT)
C
C     + + + PURPOSE + + +
C     Parse driver input file records into station computational options
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
      USE CompSpecs
      USE EMAThresh
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IBEGYR, IENDYR, ISKUOP, IKROPT, EMAOPT
      REAL          XSYSPK, XHSTPK, GENSKU, HISTPD, QHIOUT, QLWOUT, 
     $              GAGEB, RMSEGS, FLAT, FLONG
      CHARACTER*(*) STAID
      CHARACTER*4   LOTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAID  - Station ID being processed
C     XSYSPK - highest systematic peak
C     XHSTPK - lowest historic peak
C     GENSKU - regional skew
C     HISTPD - length of historic period
C     QHIOUT - hi-outlier threshold
C     QLWOUT - lo-outlier threshold
C     LOTYPE - lo-outlier test type (NONE, GBT, MGBT, FIXE)
C     GAGEB  - gage base discharge
C     RMSEGS - standard error of regional skew
C     IBEGYR - beginning year of analysis
C     IENDYR - ending year of analysis
C     ISKUOP - skew computation option,
C              -1 - Station
C               0 - Weighted
C               1 - Regional
C     IKROPT - allow urban/regularized peaks (0 - no, 1 -yes)
C     FLAT   - station latitude, decimal
C     FLONG  - station longitude, decimal
C     EMAOPT - Analysis option,
C              0 - Bull. 17B
C              1 - EMA
C
C     + + + LOCAL VARIABLES
      INTEGER      I,J,ISTA,NSPECS,IVAL
      CHARACTER*1  S1(120)
      CHARACTER*5  LCODE
      CHARACTER*120 S,KWD
      TYPE (ThreshSpec), ALLOCATABLE :: LTHRESH(:)
      TYPE (IntervalSpec), ALLOCATABLE :: LINTERVAL(:)
      TYPE (PeakSpec), ALLOCATABLE :: LPKS(:)
C
C     + + + FUNCTIONS + + +
      INTEGER      CVRINT, IYESNO, STRFND
      REAL         CVRDEC
      CHARACTER*120 STRRETREM
C
C     + + + EXTERNALS + + +
      EXTERNAL   CVRINT, IYESNO, STRFND, CVRDEC, STRRETREM, WRITESPECSTA
C
C     + + + END SPECIFICATIONS + + +
C
      NSPECS = -1 !assume no specs for this station
C
      IF (ALLOCATED(STASPECS)) THEN !station specs exist
        ISTA = 1
        DO WHILE (ISTA .LE. UBOUND(STASPECS,1) .AND.
     $            NSPECS.LT.0)
c       write(99,*)"PARSESTASPECS: STASPECS ID:",STASPECS(ISTA)%ID,
c     $                                    TRIM(STASPECS(ISTA)%ID)
c       write(99,*)"PARSESTASPECS:       STAID:",STAID,TRIM(STAID)
          IF (TRIM(STASPECS(ISTA)%ID) .EQ. TRIM(STAID)) THEN 
C           specs exist for this station
            NSPECS = STASPECS(ISTA)%NSPECS
c           ISTA = UBOUND(STASPECS,1)
          ELSE
            ISTA = ISTA + 1
          END IF
        END DO
      ELSE !use defaults
        WRITE(99,*) "Using default Specs for Station: ",STAID
      END IF
    
C     init EMA Threshold specs
      NTHRESH = 0
      IF (ALLOCATED(THRESH)) THEN
        DEALLOCATE (THRESH)
      END IF
C     init EMA Interval specs
      NINTERVAL = 0
      IF (ALLOCATED(INTERVAL)) THEN
        DEALLOCATE (INTERVAL)
      END IF
C     init new peaks specs
      NNEWPKS = 0
      IF (ALLOCATED(NEWPKS)) THEN
        DEALLOCATE (NEWPKS)
      END IF
      IF (NSPECS .GT. 0) THEN
        DO 100 I = 1, NSPECS
          S = STASPECS(ISTA)%SPECS(I)%STR
          KWD = STRRETREM(S)
          IF (KWD .EQ. 'ANALYZE') THEN
            IF (S .EQ. 'B17B') THEN
              EMAOPT = 0
              LOTYPE = 'GBT'
            ELSEIF (S .EQ. 'EMA') THEN
              EMAOPT = 1
              LOTYPE = 'MGBT'
            END IF
          ELSEIF (KWD .EQ. 'GENSKEW') THEN
            GENSKU = CVRDEC(S)
          ELSE IF (KWD .EQ. 'SKEWSE') THEN
            RMSEGS = CVRDEC(S)
          ELSE IF (KWD .EQ. 'BEGYEAR') THEN
            IBEGYR = CVRINT(S)
          ELSE IF (KWD .EQ. 'ENDYEAR') THEN
            IENDYR = CVRINT(S)
          ELSE IF (KWD .EQ. 'HISTPERIOD') THEN
            HISTPD = CVRDEC(S)
          ELSE IF (KWD .EQ. 'SKEWOPT') THEN
            IF (S .EQ. 'STATION') THEN 
              ISKUOP = -1
            ELSE IF (S .EQ. 'WEIGHTED') THEN 
              ISKUOP = 0
            ELSE IF (S .EQ. 'REGIONAL') THEN 
              ISKUOP = 1
            END IF 
          ELSE IF (KWD .EQ. 'URB/REG') THEN
            IKROPT = IYESNO(S,0)
          ELSE IF (KWD .EQ. 'LOTHRESH') THEN
            QLWOUT = CVRDEC(S)
          ELSE IF (KWD .EQ. 'LOTYPE') THEN
            LOTYPE = S
          ELSE IF (KWD .EQ. 'HITHRESH') THEN
            QHIOUT = CVRDEC(S)
          ELSE IF (KWD .EQ. 'GAGEBASE') THEN
            GAGEB = CVRDEC(S)
          ELSE IF (KWD .EQ. 'LATITUDE') THEN
            FLAT = CVRDEC(S)
          ELSE IF (KWD .EQ. 'LONGITUDE') THEN
            FLONG = CVRDEC(S)
          ELSE IF (KWD .EQ. 'PCPT_THRESH') THEN
C           see if any threshold specs exist
            IF (NTHRESH.GT.0) THEN
C             make local copy of existing threshold specs
              ALLOCATE (LTHRESH(NTHRESH))
              DO 50 J = 1, NTHRESH
                LTHRESH(J)%THRBYR = THRESH(J)%THRBYR
                LTHRESH(J)%THREYR = THRESH(J)%THREYR
                LTHRESH(J)%THRLWR = THRESH(J)%THRLWR
                LTHRESH(J)%THRUPR = THRESH(J)%THRUPR
                LTHRESH(J)%THRCOM = THRESH(J)%THRCOM
 50           CONTINUE
              DEALLOCATE (THRESH)
            END IF
            NTHRESH = NTHRESH + 1
            ALLOCATE (THRESH(NTHRESH))
            IF (ALLOCATED(LTHRESH)) THEN
C             put local copy back in array
              DO 60 J = 1, NTHRESH - 1
                THRESH(J)%THRBYR = LTHRESH(J)%THRBYR
                THRESH(J)%THREYR = LTHRESH(J)%THREYR
                THRESH(J)%THRLWR = LTHRESH(J)%THRLWR
                THRESH(J)%THRUPR = LTHRESH(J)%THRUPR
                THRESH(J)%THRCOM = LTHRESH(J)%THRCOM
 60           CONTINUE
              DEALLOCATE (LTHRESH)
            END IF
C           read 4 EMA Threshold components
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              IVAL = CVRINT(KWD)
              THRESH(NTHRESH)%THRBYR =IVAL
            ELSE
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              THRESH(NTHRESH)%THREYR = CVRINT(KWD)
            ELSE
              WRITE(99,*) "No value found for EMA Threshold Ending Year"
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              THRESH(NTHRESH)%THRLWR = CVRDEC(KWD)
            ELSE
              WRITE(99,*) "No value found for EMA Threshold Lower Bound"
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              THRESH(NTHRESH)%THRUPR = CVRDEC(KWD)
            ELSE
              WRITE(99,*) "No value found for EMA Threshold Upper Bound"
            END IF
            KWD = STRRETREM(S)
            J = LEN_TRIM(KWD)
            IF (J.GT.0) THEN
C             include all of remaining spec string for comment
              THRESH(NTHRESH)%THRCOM = KWD(1:J+1) // S
            ELSE
              THRESH(NTHRESH)%THRCOM = ' '
              WRITE(99,*) "No value found for EMA Threshold Comment"
            END IF
          ELSE IF (KWD .EQ. 'INTERVAL') THEN
C           see if any interval specs exist
            IF (NINTERVAL.GT.0) THEN
C             make local copy of existing interval specs
              ALLOCATE (LINTERVAL(NINTERVAL))
              DO 70 J = 1, NINTERVAL
                LINTERVAL(J)%INTRVLYR = INTERVAL(J)%INTRVLYR
                LINTERVAL(J)%INTRVLLWR = INTERVAL(J)%INTRVLLWR
                LINTERVAL(J)%INTRVLUPR= INTERVAL(J)%INTRVLUPR
                LINTERVAL(J)%INTRVLCOM= INTERVAL(J)%INTRVLCOM
 70           CONTINUE
              DEALLOCATE (INTERVAL)
            END IF
            NINTERVAL = NINTERVAL + 1
            ALLOCATE (INTERVAL(NINTERVAL))
            IF (ALLOCATED(LINTERVAL)) THEN
C             put local copy back in array
              DO 80 J = 1, NINTERVAL - 1
                INTERVAL(J)%INTRVLYR = LINTERVAL(J)%INTRVLYR
                INTERVAL(J)%INTRVLLWR = LINTERVAL(J)%INTRVLLWR
                INTERVAL(J)%INTRVLUPR = LINTERVAL(J)%INTRVLUPR
                INTERVAL(J)%INTRVLCOM= LINTERVAL(J)%INTRVLCOM
 80           CONTINUE
              DEALLOCATE (LINTERVAL)
            END IF
C           read 4 EMA Interval components
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              IVAL = CVRINT(KWD)
              INTERVAL(NINTERVAL)%INTRVLYR =IVAL
            ELSE
              WRITE(99,*) "No value found for EMA Interval Year"
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              INTERVAL(NINTERVAL)%INTRVLLWR = CVRDEC(KWD)
            ELSE
              WRITE(99,*) "No value found for EMA Interval Lower Bound"
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              INTERVAL(NINTERVAL)%INTRVLUPR = CVRDEC(KWD)
            ELSE
              WRITE(99,*) "No value found for EMA Interval Upper Bound"
            END IF
            KWD = STRRETREM(S)
            J = LEN_TRIM(KWD)
            IF (J.GT.0) THEN
C             include all of remaining spec string for comment
              INTERVAL(NINTERVAL)%INTRVLCOM = KWD(1:J+1) // S
            ELSE
              INTERVAL(NINTERVAL)%INTRVLCOM = ' '
              WRITE(99,*) "No value found for EMA Interval Comment"
            END IF
          ELSE IF (KWD .EQ. 'PEAK') THEN
C           see if any new peak specs exist
            IF (NNEWPKS.GT.0) THEN
C             make local copy of existing new peak specs
              ALLOCATE (LPKS(NNEWPKS))
              DO 90 J = 1, NNEWPKS
                LPKS(J)%PKYR = NEWPKS(J)%PKYR
                LPKS(J)%PKVAL= NEWPKS(J)%PKVAL
                LPKS(J)%PKCOM= NEWPKS(J)%PKCOM
                LPKS(J)%PKCODE= NEWPKS(J)%PKCODE
 90           CONTINUE
              DEALLOCATE (NEWPKS)
            END IF
            NNEWPKS = NNEWPKS + 1
            ALLOCATE (NEWPKS(NNEWPKS))
            IF (ALLOCATED(LPKS)) THEN
C             put local copy back in array
              DO 95 J = 1, NNEWPKS - 1
                NEWPKS(J)%PKYR = LPKS(J)%PKYR
                NEWPKS(J)%PKVAL= LPKS(J)%PKVAL
                NEWPKS(J)%PKCOM= LPKS(J)%PKCOM
                NEWPKS(J)%PKCODE= LPKS(J)%PKCODE
 95           CONTINUE
              DEALLOCATE (LPKS)
            END IF
C           read 4 peak components
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              IVAL = CVRINT(KWD)
              NEWPKS(NNEWPKS)%PKYR = IVAL
            ELSE
              WRITE(99,*) "No value found for Peak Year"
            END IF
            KWD = STRRETREM(S)
            IF (LEN_TRIM(KWD).GT.0) THEN
              NEWPKS(NNEWPKS)%PKVAL = CVRDEC(KWD)
            ELSE
              WRITE(99,*) "No value found for Peak Value"
            END IF
C           assume no code or comment
            NEWPKS(NNEWPKS)%PKCOM = ''
            NEWPKS(NNEWPKS)%PKCODE = ''
            CALL CVARAR(120,S,120,S1)
            J = STRFND(120,S1,1,'''')
            IF (J.GT.0) THEN 
C             comment found
              IF (J.GT.1) THEN
C               but there is a code preceding it
                NEWPKS(NNEWPKS)%PKCODE = S(1:J-1)
              END IF  
              NEWPKS(NNEWPKS)%PKCOM = S(J+1:120)
            ELSEIF (LEN_TRIM(S).GT.0) THEN
C             assume code is only thing left
              NEWPKS(NNEWPKS)%PKCODE = S
            END IF
          END IF
 100    CONTINUE
      END IF
C
      IF (UPDATEFG) THEN
        CALL WRITESPECSTA (STAID,GENSKU,HISTPD,QHIOUT,QLWOUT,LOTYPE,
     I                     GAGEB,RMSEGS,IBEGYR,IENDYR,
     I                     ISKUOP,IKROPT,FLAT,FLONG,XSYSPK,XHSTPK,
     I                     EMAOPT)
      END IF
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION   DOSTATION
     I                            (ISTART,
     M                             STAID)
C
C     + + + PURPOSE + + +
C     Determine whether or not a station is to be processed.
C     STAID may be updated with an index if multiple
C     instances of the same station are found.
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
      USE CompSpecs
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ISTART
      CHARACTER*(*) STAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTART - flag for start of processing a data file
C              0 - start of file
C              >0 - after start of file
C     STAID  - Station ID being read
C
C     + + + SAVES + + +
      SAVE NSTAUSED,STAUSED
C
C     + + + LOCAL VARIABLES
      INTEGER      ISTA,NSTAUSED,J,K,STAIND
      LOGICAL      LDO
      CHARACTER*18 STAUSED(1000)
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT, CVRINT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NSTAUSED/0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ISTART.EQ.0 .AND. NSTAUSED.GT.0) THEN
C       starting to process a new data file, re-init station used array
        DO 10 I = 1,NSTAUSED
          STAUSED(I) = "                  "
 10     CONTINUE
        NSTAUSED = 0
      END IF
C
      LDO = .FALSE. !assume not doing this station
      IF (ALLOCATED(STASPECS)) THEN !station specs exist
        IF (NSTAUSED.GT.0) THEN !look through stations used so far
          ISTA = NSTAUSED
          DO WHILE (ISTA.GT.0)
            J = ZLNTXT(STAID)
            IF (STAUSED(ISTA)(1:J).EQ.STAID) THEN
C             same station ID, increment STAID index
              K = ZLNTXT(STAUSED(ISTA))
              IF (K.GT.J) THEN 
C               this station already has an index, increment it
                STAIND = CVRINT(STAUSED(ISTA)(J+2:K)) + 1
                IF (STAIND.LT.10) THEN
                  WRITE(STAID,'(A,I1)') TRIM(STAID) // "-",STAIND
                ELSE
                  WRITE(STAID,'(A,I2)') TRIM(STAID) // "-",STAIND
                END IF
              ELSE !first duplicate of this station
                STAID = TRIM(STAID) // "-1"
              END IF
              WRITE(99,*) "DOSTATION: Duplicate Station ID: look for ",
     $                   TRIM(STAID)
              ISTA = 0  !exit loop
            END IF
            ISTA = ISTA - 1
          END DO
        END IF

C       update station used info
        NSTAUSED = NSTAUSED + 1
        STAUSED(NSTAUSED) = STAID

C       now look for the station in the STASPECS array
        ISTA = 1
        DO WHILE (ISTA .LE. UBOUND(STASPECS,1))
          IF (STASPECS(ISTA)%ID .EQ. STAID) THEN !Yes, do this station
            LDO = .TRUE.
            ISTA = UBOUND(STASPECS,1)
          END IF
          ISTA = ISTA + 1
        END DO
      END IF
C
      DOSTATION = LDO
C
      RETURN
      END
C
C
C
      SUBROUTINE   WRITESPECIO (WDMSFL,INCRD,INFORM,FOUT,IPUNCH,
     I                          IPLTOP,GRFMT,IPRTOP,IBCPUN,IDEBUG,
     I                          CLSIZE,WEIBA,EXPFUN,EMPFUN,IEXTEND)
C
C     + + + PURPOSE + + +
C     Write out verbose version of spec file (i.e. include 
C     a record for every spec, even if defaulted).
C     This routine opens a temporary spec file and
C     writes Input and Output specs to it.
C     Station specs are written in WRITESPECSTA.
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 1/04
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,INCRD,INFORM,FOUT,IPUNCH,
     $          IPLTOP,IPRTOP,IBCPUN,IDEBUG,EXPFUN,EMPFUN
      REAL      CLSIZE,WEIBA
      CHARACTER*3 GRFMT
C
C     + + + ARGUMENT DEFINITIONS + + + 
C     WDMSFL - Fortran unit number for WDM input file
C     INCRD  - Fortran unit number for ASCI input file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*240 FNAME
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(2A)
C
C     + + + END SPECIFICATIONS + + +
C
       write(99,*) 'Updating spec file I/O options'
      OPEN (UNIT=92,FILE='TEMPSPEC',STATUS='REPLACE')
      WRITE(92,*) 'VERBOSE'
C     input file
      IF (INFORM.EQ.1) THEN !WDM file
        INQUIRE(WDMSFL,NAME=FNAME)
        WRITE(92,2000) 'I WDM ',TRIM(FNAME)
      ELSE !ASCII file
        INQUIRE(INCRD,NAME=FNAME)
        WRITE(92,2000) 'I ASCI ',TRIM(FNAME)
      END IF
C     output file
      INQUIRE(FOUT,NAME=FNAME)
      WRITE(92,2000) 'O File ',TRIM(FNAME)
C     plot style and other plot options
      IF (IPLTOP.LE.0) THEN
        WRITE(92,*) 'O Plot Style None'
      ELSE IF (IPLTOP.EQ.1) THEN
        WRITE(92,*) 'O Plot Style Graphics'
      ELSE IF (IPLTOP.EQ.2) THEN
        WRITE(92,*) 'O Plot Style Printer'
      ELSE IF (IPLTOP.EQ.3) THEN
        WRITE(92,*) 'O Plot Style Both'
      END IF
      IF ((IPLTOP.EQ.1 .OR. IPLTOP.EQ.3) .AND. GRFMT.NE.'') THEN
C       also write out graphic format
        WRITE(92,*) '0 Plot Format '//GRFMT
      END IF
      IF (IPRTOP.EQ.1) THEN
        WRITE(92,*) 'O Plot PrintPos Yes'
      ELSE
        WRITE(92,*) 'O Plot PrintPos No'
      END IF
      WRITE(92,*) 'O Plot Position ',WEIBA
C     additional output
      IF (IBCPUN.GE.2) THEN
        INQUIRE(IPUNCH,NAME=FNAME)
      END IF
      IF (IBCPUN.EQ.0) THEN
        WRITE(92,*) 'O Additional None'
      ELSE IF (IBCPUN.EQ.1) THEN
        WRITE(92,*) 'O Additional WDM'
      ELSE IF (IBCPUN.EQ.2) THEN
        WRITE(92,*) 'O Additional WAT '//TRIM(FNAME)
      ELSE IF (IBCPUN.EQ.3) THEN
        WRITE(92,*) 'O Additional Both WAT '//TRIM(FNAME)
      ELSE IF (IBCPUN.EQ.4) THEN
        WRITE(92,*) 'O Additional TAB '//TRIM(FNAME)
      ELSE IF (IBCPUN.EQ.5) THEN
        WRITE(92,*) 'O Additional Both TAB '//TRIM(FNAME)
      END IF
      IF (EXPFUN.GT.0) THEN
        INQUIRE(EXPFUN,NAME=FNAME)
        WRITE(92,*) 'O Export '//TRIM(FNAME)
      END IF
      IF (EMPFUN.GT.0) THEN
        INQUIRE(EMPFUN,NAME=FNAME)
        WRITE(92,*) 'O Empirical '//TRIM(FNAME)
      END IF
      IF (IDEBUG.EQ.1) THEN
        WRITE(92,*) 'O Debug Yes'
      ELSE
        WRITE(92,*) 'O Debug No'
      END IF
      IF (IEXTEND.EQ.1) THEN
        WRITE(92,*) 'O Extended Yes'
      ELSE
        WRITE(92,*) 'O Extended No'
      END IF
      WRITE(92,*) 'O Confidence ',CLSIZE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WRITESPECSTA
     I                        (STAID,GENSKU,HISTPD,QHIOUT,QLWOUT,LOTYPE,
     M                         GAGEB,RMSEGS,IBEGYR,IENDYR,ISKUOP,
     M                         IKROPT,FLAT,FLONG,XSYSPK,XHSTPK,EMAOPT)
C
C     + + + PURPOSE + + +
C     Write out verbose version of spec file (i.e. include 
C     a record for every spec, even if defaulted).
C     This routine writes station specs to the
C     temporary spec file opened in WRITESPECIO.
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 1/04
C     Paul Hummel of AQUA TERRA Consultants
C
      USE EMAThresh
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IBEGYR, IENDYR, ISKUOP, IKROPT, EMAOPT
      REAL          GENSKU, HISTPD, QHIOUT, QLWOUT, GAGEB, RMSEGS, 
     $              FLAT, FLONG, XSYSPK, XHSTPK
      CHARACTER*(*) STAID
      CHARACTER*4   LOTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STAID  - Station ID being processed
C     GENSKU - regional skew
C     HISTPD - length of historic period
C     QHIOUT - hi-outlier threshold
C     QLWOUT - lo-outlier threshold
C     LOTYPE - lo-outlier test type (NONE, GBT, MGBT, FIXE)
C     GAGEB  - gage base discharge
C     RMSEGS - standard error of regional skew
C     IBEGYR - beginning year of analysis
C     IENDYR - ending year of analysis
C     ISKUOP - skew computation option,
C              -1 - Station
C               0 - Weighted
C               1 - Regional
C     IKROPT - allow urban/regularized peaks (0 - no, 1 -yes)
C     FLAT   - station latitude, decimal
C     FLONG  - station longitude, decimal
C     XSYSPK - highest systematic peak
C     XHSTPK - lowest historic peak
C     EMAOPT - Analysis option,
C              0 - Bull. 17B
C              1 - EMA
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
      CHARACTER*12 LWRSTR, UPRSTR
      CHARACTER*15 OSTAID
      CHARACTER*120 LCOMMENT
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('      PCPT_Thresh ',2I6,2A12,2X,A)
C 2000 FORMAT('      PCPT_Thresh ',2I6,F10.0,G12.1,2X,A)
 2010 FORMAT('      Interval ',I6,2A12,2X,A)
 2020 FORMAT('      Peak ',I6,F10.1,2X,A,2X,A)
 2021 FORMAT('      Peak ',I6,F10.0,2X,A,2X,A)
 2030 FORMAT(F12.0)
 2040 FORMAT(G12.1)
C
C     + + + END SPECIFICATIONS + + +
C
      write(99,*) 'Updating spec file Station info'
C     save original station ID for default plot name
      OSTAID = STAID
C     start with station ID, remove any duplicate identifier at end
      J = 0
      I = ZLNTXT(STAID) - 1
      DO WHILE (I.GT.0)
        IF (STAID(I:I).EQ."-") THEN
          J = I
          I = 0
        ELSE
          I = I - 1
        END IF
      END DO
      IF (J.GT.0) THEN
        STAID = STAID(1:J-1)
      END IF
      WRITE(92,*) 'Station ',STAID
C
      IF (EMAOPT .EQ. 0) THEN
        WRITE(92,*) '     Analyze B17B'
      ELSEIF (EMAOPT .EQ. 1) THEN
        WRITE(92,*) '     Analyze EMA'
      END IF
C     thresholds and intervals
      IF (NTHRESH.GT.0) THEN
        DO 10 I=1,NTHRESH
          IF (THRESH(I)%THRLWR .LT. 1.0E9) THEN
            WRITE(LWRSTR,2030) THRESH(I)%THRLWR
          ELSE
            WRITE(LWRSTR,2040) THRESH(I)%THRLWR
          END IF
          IF (THRESH(I)%THRUPR .LT. 1.0E9) THEN
            WRITE(UPRSTR,2030) THRESH(I)%THRUPR
          ELSE
            WRITE(UPRSTR,2040) THRESH(I)%THRUPR
          END IF
          WRITE(92,2000) THRESH(I)%THRBYR,THRESH(I)%THREYR,
     $                   LWRSTR,UPRSTR,TRIM(THRESH(I)%THRCOM)
 10     CONTINUE  
      END IF
      IF (NINTERVAL.GT.0) THEN
        DO 20 I=1,NINTERVAL
          IF (INTERVAL(I)%INTRVLLWR .LT. 1.0E9) THEN
            WRITE(LWRSTR,2030) INTERVAL(I)%INTRVLLWR
          ELSE
            WRITE(LWRSTR,2040) INTERVAL(I)%INTRVLLWR
          END IF
          IF (INTERVAL(I)%INTRVLUPR .LT. 1.0E9) THEN
            WRITE(UPRSTR,2030) INTERVAL(I)%INTRVLUPR
          ELSE
            WRITE(UPRSTR,2040) INTERVAL(I)%INTRVLUPR
          END IF
          WRITE(92,2010) INTERVAL(I)%INTRVLYR,LWRSTR,UPRSTR,
     $                   TRIM(INTERVAL(I)%INTRVLCOM)
 20     CONTINUE  
      END IF
C     new/revised peaks
      IF (NNEWPKS.GT.0) THEN
        DO 30 I=1,NNEWPKS
          LCOMMENT = '''' // TRIM(NEWPKS(I)%PKCOM)
          IF (NEWPKS(I)%PKVAL .LT. 100) THEN
            WRITE(92,2020) NEWPKS(I)%PKYR,NEWPKS(I)%PKVAL,
     $                               NEWPKS(I)%PKCODE,LCOMMENT
          ELSE
            WRITE(92,2021) NEWPKS(I)%PKYR,NEWPKS(I)%PKVAL,
     $                               NEWPKS(I)%PKCODE,LCOMMENT
          END  IF
 30     CONTINUE  
      END IF
C     skew parameters
      IF (ISKUOP.EQ.-1) THEN
        WRITE(92,*) '     SkewOpt Station'
      ELSE IF (ISKUOP.EQ.0) THEN
        WRITE(92,*) '     SkewOpt Weighted'
      ELSE IF (ISKUOP.EQ.1) THEN
        WRITE(92,*) '     SkewOpt Regional'
      END IF
      WRITE(92,*) '     GenSkew ',GENSKU
      WRITE(92,*) '     SkewSE ',RMSEGS
C     historic parameters
      WRITE(92,*) '     BegYear ',IBEGYR
      WRITE(92,*) '     EndYear ',IENDYR
      WRITE(92,*) '     HistPeriod ',HISTPD
C     other flow parameters
      IF (IKROPT.EQ.1) THEN
        WRITE(92,*) '     Urb/Reg Yes'
      ELSE
        WRITE(92,*) '     Urb/Reg No'
      END IF
      WRITE(92,*) '     LOType ',LOTYPE
      WRITE(92,*) '     LoThresh ',QLWOUT
      WRITE(92,*) '     HiThresh ',QHIOUT
      WRITE(92,*) '     GageBase ',GAGEB
      WRITE(92,*) '     Latitude ',FLAT
      WRITE(92,*) '     Longitude ',FLONG
      WRITE(92,*) '     HiSys ',XSYSPK
      WRITE(92,*) '     LoHist ',XHSTPK
      WRITE(92,*) '     PlotName ',OSTAID
C
      RETURN
      END
C
C
C
      SUBROUTINE   UPDATESPECFILE
     I                           (FUNIT,SPCFIL)
C
C     + + + PURPOSE + + +
C     Replace existing spec file with updated verbose version.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FUNIT
      CHARACTER*240 SPCFIL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FUNIT  - Fortran unit number of original spec file
C     SPCFIL - name of original spec file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*120 ISTR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A)
C
C     + + + END SPECIFICATIONS + + +
C
C     delete original spec file
      CLOSE(FUNIT,STATUS='DELETE')
      OPEN(FUNIT,FILE=SPCFIL)
      REWIND 92 !back to start of temporary verbose spec file
      DO !echo temporary file to updated spec file
        READ(92,1000,END=120) ISTR
        WRITE(FUNIT,1000) TRIM(ISTR)
      END DO
C
 120  CONTINUE !get here on end of file

      CLOSE(92,STATUS='DELETE')
      CLOSE(FUNIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   JFLUSH
     I                    ( INP, OUT )
C
C     + + + PURPOSE + + +
C     Copy warning and error messages from temporary file to the
C     output file.  Close and delete temporary file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INP, OUT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       AGAIN
      CHARACTER*80  RECORD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( A )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( A )
C
C     + + + END SPECIFICATIONS + + +
C
C     end and rewind temporary file
      ENDFILE ( UNIT = INP)
      REWIND  ( UNIT = INP)
C
C     copy records from INPut file to OUTput file
      AGAIN = 1
  100 CONTINUE
        READ (INP,1000,END=108,ERR=109) RECORD
C         no problem
          WRITE (OUT,2000) RECORD
          GO TO 110
  108   CONTINUE
C         end of file, delete temporary file
          CLOSE (  UNIT =  INP, STATUS = 'DELETE' )
          AGAIN = 0
          GO TO 110
  109   CONTINUE
C         error reading, assume end of temporary file, delete
          CLOSE (  UNIT =  INP, STATUS = 'DELETE' )
          AGAIN = 0
          GO TO 110
  110   CONTINUE
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMSKU
     I                    (WDMSFL, DSN,
     O                     GENSKU, RETCOD)
C
C     + + + PURPOSE + + +
C     This routine computes regional skew from WRC guidelines using
C     the routine WCFGSM with latitude and longitude from the
C     attributes of the dataset.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, RETCOD
      REAL      GENSKU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for users WDM file
C     DSN    - dataset number in WDM file to use for lat-long
C     GENSKU - skew coefficient
C     RETCOD - return code, 0 - successful computation
C              non-zero for unsuccessful computation
C
C     + + + LOCAL VARIABLES + + +
      REAL      RLAT, RLONG
C
C     + + + FUNCTIONS + + +
      REAL   WCFGSM
C
C     + + + EXTERNALS + + +
      EXTERNAL   WCFGSM, WDBSGL
C
C     + + + END SPECIFICATIONS + + +
C
C     get latitude and longitude
      CALL WDBSGL ( WDMSFL, DSN, RLAT, RLONG, RETCOD )
      IF (RETCOD .EQ. 0) THEN
C       compute skew
        GENSKU = WCFGSM ( RLAT, RLONG )
      ELSE
C       default to 0.0
        GENSKU = 0.0
      END IF
C
      RETURN
      END
C
C
C
      CHARACTER*120 FUNCTION   STRRETREM
     M                                  (S)
C
C     + + + PURPOSE + + +
C     Returns leading portion of incoming string up to first delimeter
C     and returns incoming string without that portion.
C     Example: StrRetRem("This string") = "This", and s is reduced to "string"
C     Example: StrRetRem("This,string") = "This", and s is reduced to "string"
C
C     + + + HISTORY + + +
C     created for batch version of PEAKFQ, 9/03
C     Paul Hummel of AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*120 S
C
C     + + + ARGUMENT DEFINITIONS + + +
C     S - string to be analyzed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I, J
      CHARACTER S1(120)
      CHARACTER*120 OUTSTR
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL  STRFND, ZLJUST, CVARAR
C
C     + + + END SPECIFICATIONS + + +    
C
      CALL ZLJUST(S)
      CALL CVARAR(120,S,120,S1)
      I = STRFND(120,S1,1,'"')
      IF (I .EQ. 1) THEN !string beginning
        S = S(2:120)
        CALL CVARAR(120,S,120,S1)
        I = STRFND(120,S1,1,'"') !string end
      ELSE
        I = STRFND(120,S,1,' ')  !blank delimeter
        J = STRFND(120,S,1,',')  !comma delimeter
        IF (J .GT. 0) THEN      !comma found
          IF (I .EQ. 0 .OR. J .LT. I) THEN
            I = J
          END IF
        END IF
      END IF
       
      IF (I .GT. 0) THEN  !found delimeter
        OUTSTR = S(1:I-1) !string to return
        S = S(I+1:120)     !string remaining
        CALL ZLJUST(S)
        IF (S(1:1) .EQ. ',' .And. I .NE. J) THEN 
          S = S(2:120)
        END IF
      ELSE !take it all
        OUTSTR = S
        S = '' !nothing left
      END IF
    
      STRRETREM = OUTSTR

Cprh      WRITE (99,*) "STRRETREM:'" // TRIM(OUTSTR) //
Cprh     $                    "','" // TRIM(S) // "'"

      RETURN
      END
C
C
C
      INTEGER FUNCTION   IYESNO
     I                          (ISTR,IDEF)
C
C     + + + PURPOSE + + +
C     Return an integer value of 0 for NO or 1 for YES
C     based on the contents of a batch input record.
C     If NO or YES not found, use IDEF as default.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDEF
      CHARACTER*(*) ISTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR - input string from batch fiel
C     IDEF - default value if NO or YES not found on record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ZLNTXT(ISTR).GT.0) THEN !look for YES or NO specification
        IF (ISTR.EQ.'NO') THEN
          IVAL = 0
        ELSE IF (ISTR.EQ.'YES') THEN
          IVAL = 1
        END IF
      ELSE !assume default
        IVAL = IDEF
      END IF
C
      IYESNO = IVAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   UPCASE
     M                   (STRING)
C
C     + + + PURPOSE + + +
C     Convert a character string from lower case to upper case
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to be made upper case
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ICH,ILEN
C
C     + + + INTRINSICS + + +
      INTRINSIC  ICHAR, MOD, CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN = LEN(STRING)
      DO 10 I = 1, ILEN
        ICH= ICHAR(STRING(I:I))
        ICH= MOD(ICH,128)
        IF (ICH.GE.97 .AND. ICH.LE.122) THEN
C         character is lower case
          STRING(I:I)= CHAR(ICH-32)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      REAL FUNCTION   CVRDEC
     I                      (ISTR)
C
C     + + + PURPOSE + + +
C     Convert a character variable to a real number.
C     Returns 0.0 if string is blank.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) ISTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR - string to convert
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ILEN
      REAL      RVAL
      CHARACTER*1 ISTR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
      REAL      CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT, CHRDEC, CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN = ZLNTXT(ISTR)
      IF (ILEN.GT.0) THEN
        CALL CVARAR (ILEN,ISTR,ILEN,ISTR1)
        RVAL = CHRDEC(ILEN,ISTR1)
      ELSE
        RVAL = 0.0
      END IF
C
      CVRDEC = RVAL
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   CVRINT
     I                         (ISTR)
C
C     + + + PURPOSE + + +
C     Convert a character variable to an integer number.
C     Returns 0 if string is blank.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) ISTR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTR - string to convert
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ILEN, IVAL
      CHARACTER*1 ISTR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT, CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT, CHRINT, CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN = ZLNTXT(ISTR)
      IF (ILEN.GT.0) THEN
        CALL CVARAR (ILEN,ISTR,ILEN,ISTR1)
        IVAL = CHRINT(ILEN,ISTR1)
      ELSE
        IVAL = 0
      END IF
C
      CVRINT = IVAL
C
      RETURN
      END
