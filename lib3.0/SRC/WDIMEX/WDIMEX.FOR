C
C
C
      PROGRAM   WDIMEX
C
C     + + + PURPOSE + + +
C     Import/export data sets of all types to/from a WDM file.
C     Create a message file from scratch if one does not exist.
C
C
C     + + + PARAMETERS + + +
      INTEGER     MAXATT
      PARAMETER ( MAXATT = 9 )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      WDMSFL,ERRFLG,SEQFIL,DSN,DSTYP,RETCOD,
     1             DONFLG,IFLG,EFLG,RDOFLG,ATMSFL,AMISFG,I
      LOGICAL      LFLAG
      CHARACTER*1  CRESP
      CHARACTER*11 ATTFIL(MAXATT)
      CHARACTER*64 WDNAME,FLNAME,ATFNAM,PTHNAM,VERSN
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, WDSCHK, WDBOPN, WDMIM, WDMEX, WDFLCL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ATTFIL/'attr001.seq','attr051.seq','attr101.seq',
     1            'attr151.seq','attr201.seq','attr251.seq',
     2            'attr301.seq','attr351.seq','attr401.seq'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A64)
 1010 FORMAT (A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' Successful open of old WDM file ',A64,/)
 2005 FORMAT (/,' Successful creation of WDM file ',A64,/)
 2010 FORMAT (' Importing attribute file ',A64)
 2020 FORMAT (' Attributes from WDM file ',A64)
 2030 FORMAT (/,' Importing from file ',A64)
 2040 FORMAT (/,' Exporting to file ',A64)
C
C     + + + END SPECIFICATIONS + + +
C
C     version info for what on unix
      INCLUDE 'fversn.inc'
C
C     avoid some lahey math errors
      LFLAG= .TRUE.
      CALL INVALOP (LFLAG)
C
C     open error logging file
      OPEN(UNIT=99,FILE='error.fil')
C
      DSTYP = 8
      WDMSFL= 35
C
 10   CONTINUE
        WRITE (*,*) 'Enter name of WDM file: '
        READ (*,1000) WDNAME
C       open the old WDMS file (read only, if possible)
        RDOFLG= 0
        CALL WDBOPN (WDMSFL,WDNAME,RDOFLG,
     O               ERRFLG)
        IF (ERRFLG.EQ.0) THEN
C         open ok
          WRITE(99,2000) WDNAME
        ELSE
C         couldnt open, create, retry, or quit
 20       CONTINUE
            WRITE (*,*) 'Could not open file.  Create it, Retry,',
     1                  ' or Quit (C,R,Q)?'
            READ (*,1010) CRESP
            IF (CRESP.EQ.'C' .OR. CRESP.EQ.'c') THEN
C             create it
              RDOFLG= 2
              CALL WDBOPN (WDMSFL,WDNAME,RDOFLG,
     O                     ERRFLG)
              IF (ERRFLG.NE.0) THEN
C               failed create
                WRITE (*,*) 'Unable to create ',WDNAME
C               send 'em back to beginning
                ERRFLG= 1
              ELSE
C               add attributes?
                WRITE(99,2005) WDNAME
                WRITE (*,2005) WDNAME
                WRITE (*,*) 'Do you want to put attributes on',
     1                      ' this WDM file? (Y,N)'
                READ (*,1010) CRESP
                IF (CRESP.EQ.'Y' .OR. CRESP.EQ.'y') THEN
C                 try to add them
                  SEQFIL= 36
                  WRITE (*,*) 'Enter path name for the directory con',
     1                        'taining the attribute sequential files:'
                  WRITE (*,*)
                  READ (*,1000) PTHNAM
                  WRITE (*,*)
                  I= 1
 30               CONTINUE
                    ATFNAM= PTHNAM(1:ZLNTXT(PTHNAM))//ATTFIL(I)
                    OPEN(UNIT=SEQFIL,FILE=ATFNAM,STATUS='OLD',ERR=40)
                    WRITE (*,2010) ATFNAM
                    WRITE(99,2010) ATFNAM
                    CALL WDMIM (SEQFIL,WDMSFL,ATMSFL)
                    CLOSE (SEQFIL)
                    I= I+ 1
                    GO TO 50
 40                 CONTINUE
C                     get here on failed open of attribute seq file
                      WRITE (*,*) 'Unable to open attribute',
     1                            ' sequential file ',ATFNAM
                      WRITE (*,*)
                      WRITE (*,*) 'Would you like to try a different ',
     1                            'path name? (Y,N)'
                      READ (*,1010) CRESP
                      IF (CRESP.EQ.'Y' .OR. CRESP.EQ.'y') THEN
C                       try a different path name
                        WRITE (*,*) 'Enter path name for the directory',
     1                              ' containing the attribute ',
     2                              'sequential files:'
                        WRITE (*,*)
                        READ (*,1000) PTHNAM
                        WRITE (*,*)
                      ELSE
C                       no, abandon import of attributes
                        I= MAXATT + 1
                      END IF
 50                 CONTINUE
                  IF (I.LE.MAXATT) GO TO 30
                END IF
              END IF
              CRESP= 'C'
            ELSE IF (CRESP.EQ.'R' .OR. CRESP.EQ.'r') THEN
C             try another name
              ERRFLG= 1
            ELSE IF (CRESP.EQ.'Q' .OR. CRESP.EQ.'q') THEN
C             user wants out
              ERRFLG= -1
            END IF
          IF (CRESP.NE.'C' .AND. CRESP.NE.'c' .AND.
     1        CRESP.NE.'R' .AND. CRESP.NE.'r' .AND.
     2        CRESP.NE.'Q' .AND. CRESP.NE.'q') GO TO 20
        END IF
      IF (ERRFLG.EQ.1) GO TO 10
C
      IF (ERRFLG.EQ.0) THEN
C       see if attributes are on this WDM file
        AMISFG= 0
        DSN   = 4
 100    CONTINUE
          DSN= DSN+ 1
          CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O                 I,I,RETCOD)
          IF (RETCOD.NE.0) THEN
C           cluster does not exist or wrong type, attributes missing
            AMISFG= 1
          END IF
        IF (DSN.LT.10 .AND. AMISFG.EQ.0) GO TO 100
C
        IF (AMISFG.EQ.1) THEN
C         attributes not on this WDM file, where are they?
          WRITE (*,*) 'Attributes missing from this WDM file.'
 120      CONTINUE
C           try to find message file containing attributes
            ERRFLG= 0
            AMISFG= 0
            WRITE (*,*) 'Enter name of WDM file containing attributes',
     1                  ' (blank if none exists): '
            WRITE (*,*)
            READ (*,1000) ATFNAM
            IF (ATFNAM.NE.' ') THEN
C             try to open file entered
              ATMSFL= 36
              RDOFLG= 0
              CALL WDBOPN (ATMSFL,ATFNAM,RDOFLG,
     O                     ERRFLG)
              IF (ERRFLG.EQ.0) THEN
C               see if attributes on this WDM file
                DSN= 4
 130            CONTINUE
                  DSN= DSN+ 1
                  CALL WDSCHK (ATMSFL,DSN,DSTYP,
     O                         I,I,RETCOD)
                  IF (RETCOD.NE.0) THEN
C                   cluster does not exist, attributes missing
                    AMISFG= 1
C                   file was opened, but attributes not found, close file
                    CALL WDFLCL (ATMSFL,
     O                           RETCOD)
                    WRITE (*,*) 'Attributes missing from file ',ATFNAM
                  END IF
                IF (DSN.LT.10 .AND. AMISFG.EQ.0) GO TO 130
                IF (AMISFG .EQ. 0) THEN
C                 attributes available
                  WRITE(99,2020) ATFNAM
                END IF
              ELSE
C               couldnt open file
                WRITE (*,*) 'Unable to open ',ATFNAM
                ERRFLG= -1
              END IF
              IF (AMISFG.NE.0 .OR. ERRFLG.NE.0) THEN
C               give them another chance
 150            CONTINUE
                  WRITE (*,*) 'Try another file? (Y,N) '
                  READ (*,1010) CRESP
                  IF (CRESP.EQ.'Y' .OR. CRESP.EQ.'y') THEN
                    ERRFLG= 1
                  ELSE IF (CRESP.EQ.'N' .OR. CRESP.EQ.'n') THEN
                    ERRFLG= -1
                  END IF
                IF (CRESP.NE.'Y' .AND. CRESP.NE.'y' .AND.
     1              CRESP.NE.'N' .AND. CRESP.NE.'n') GO TO 150
              END IF
            ELSE
C             blank name, get 'em outta here
              ERRFLG= -1
              WRITE (*,*) 'Attributes are required for',
     1                    ' importing and exporting data sets.'
            END IF
          IF (ERRFLG.EQ.1) GO TO 120
        ELSE
C         attributes exist on this WDM file
          ATMSFL= WDMSFL
        END IF
      END IF
C
      IF (ERRFLG.EQ.0) THEN
C       import, export, or done
        SEQFIL= 40
 200    CONTINUE
          DONFLG= 0
          ERRFLG= 0
          IFLG  = 0
          EFLG  = 0
          WRITE(*,*)
          WRITE(*,*) 'Do you want to Import, Export or Return ',
     1               'to operating system (I,E,R)? '
          READ (*,1010) CRESP
          WRITE(*,*)
C
          IF (CRESP.EQ.'I' .OR. CRESP.EQ.'i') THEN
C           importing
            IFLG= 1
          ELSE IF (CRESP.EQ.'E' .OR. CRESP.EQ.'e') THEN
C           exporting
            EFLG= 1
          ELSE IF (CRESP.EQ.'R' .OR. CRESP.EQ.'r') THEN
C           return to operating system
            DONFLG= 2
          ELSE
C           invalid answer
            DONFLG= 1
          END IF
          IF (DONFLG.EQ.0) THEN
C           either import or export
            IF (IFLG.EQ.1) THEN
              WRITE (*,*) 'Enter name of sequential file to import: '
            ELSE IF (EFLG.EQ.1) THEN
              WRITE (*,*) 'Enter name of sequential file for output: '
            END IF
            READ (*,1000) FLNAME
            WRITE(*,*)
C           try to open file specified
            SEQFIL= 37
            IF (IFLG.EQ.1) THEN
              OPEN (UNIT=SEQFIL,FILE=FLNAME,STATUS='OLD',ERR=210)
            ELSE IF (EFLG.EQ.1) THEN
              OPEN (UNIT=SEQFIL,FILE=FLNAME,STATUS='NEW',ERR=210)
            END IF
C
            GO TO 220
C
 210        CONTINUE
C             get here on bad file specified
              IF (EFLG.EQ.1) THEN
C               can't export to existing file
                WRITE (*,*) 'Error on Export, file already exists.'
              ELSE
C               couldn't find file to import
                WRITE (*,*) 'Unable to successfully open specified file'
              END IF
              WRITE (*,*)
              ERRFLG= 1
C
 220        CONTINUE
              IF (ERRFLG.EQ.0) THEN
C               successful open of import/export file
                IF (IFLG.EQ.1) THEN
C                 do import
                  WRITE(99,2030) FLNAME
                  CALL WDMIM (SEQFIL,WDMSFL,ATMSFL)
                ELSE IF (EFLG.EQ.1) THEN
C                 do export
                  WRITE(99,2040) FLNAME
                  CALL WDMEX (SEQFIL,WDMSFL,ATMSFL)
                END IF
              END IF
          END IF
C
        IF (DONFLG.LE.1) GO TO 200
      END IF
C
      STOP
      END
C
C
C
      SUBROUTINE   WDMIM
     I                   (SUCIFL,WDMSFL,ATMSFL)
C
C     + + + PURPOSE + + +
C     Import data sets (clusters) from sequential file to WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SUCIFL,WDMSFL,ATMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUCIFL - Fortran unit number of sequential import file
C     WDMSFL - Fortran unit number of WDM file
C     ATMSFL - Fortran unit number of WDM file containing attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I3,I4,I6,I10,I80,RETCOD,DONFG,SKIPFG,
     1            CLU,CLUTYP,PSA,NDN,NUP,NSA,NSP,NDP,SAIND,SALEN,
     2            SATYP,IVAL(10),DSFREC
      REAL        RVAL(10)
      CHARACTER*1 BUFF(160),CCLU(3),CDSN(3),CEND(3),CLAB(3),CDAT(3),
     1            CRESP,CDSTYP(36)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, CHRINT, LENSTR
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, CHRINT, WDDSCK, CHRDEC, LENSTR
      EXTERNAL    WDDSDL, WDLBAX, PRWMSI, PRWMAI, PRWMDI, PRWMTI, PRWMXI
      EXTERNAL    WDBSGX, WDBSAI, WDBSAR, WDBSAC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3,I4,I6,I10,I80/3,4,6,10,80/
      DATA CCLU,CDSN,CEND/'C','L','U','D','S','N','E','N','D'/
      DATA CLAB,CDAT/'L','A','B','D','A','T'/
      DATA CDSTYP/'T','I','M','E','T','A','B','L','S','C','H','E',
     1            'P','R','O','J','V','E','C','T','R','A','S','T',
     2            'S','P','T','I','A','T','T','R','M','E','S','S'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' General information from the IMPORT file:')
 2002 FORMAT (/,' Importing DSN/CLU number ',I5,'.')
 2005 FORMAT (' This DSN/CLU already exists.')
 2010 FORMAT (' *** Error on LABEL/ADD, return code:',I5,'.')
 2015 FORMAT (' *** Error, no attrib. for new DSN on IMPORT file.')
 2020 FORMAT (1X,80A1)
 2040 FORMAT (/,' Import of DSN/CLUs complete.',/)
C
C     + + + END SPECIFICATIONS + + +
C
C     general info
      WRITE (*,2000)
      WRITE(99,2000)
C
C     loop to write out general comments
      DONFG= 0
 10   CONTINUE
        READ (SUCIFL,1000) (BUFF(I),I=1,I80)
        IF (STRFND(I4,BUFF,I3,CCLU).EQ.0 .AND.
     1      STRFND(I4,BUFF,I3,CDSN).EQ.0) THEN
C         general comment line, write it to terminal and log file
          I= LENSTR(I80,BUFF)
          WRITE (*,2020) (BUFF(J),J=1,I)
          WRITE(99,2020) (BUFF(J),J=1,I)
        ELSE
          DONFG= 1
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
 15   CONTINUE
C       process a cluster
        CLU= CHRINT(I6,BUFF(11))
        I= 36
        J= STRFND(I,CDSTYP,I4,BUFF(27))
        CLUTYP= 1+ ((J-1)/4)
        NDN= CHRINT(I3,BUFF(38))
        IF (NDN.EQ.0) NDN= 10
        NUP= CHRINT(I3,BUFF(48))
        IF (NUP.EQ.0) NUP= 20
        NSA= CHRINT(I3,BUFF(58))
        IF (NSA.EQ.0) NSA= 20
        NSP= CHRINT(I3,BUFF(68))
        IF (NSP.EQ.0) NSP= 50
        NDP= CHRINT(I3,BUFF(78))
        IF (NDP.EQ.0) NDP= 100
C
        WRITE (*,2002) CLU
        WRITE(99,2002) CLU
C
C       loop to find valid cluster to write or skip
        SKIPFG= 0
        CALL WDDSCK (WDMSFL,CLU,
     O               DSFREC,RETCOD)
        IF (DSFREC.GT.0) THEN
C         cluster exists, change, skip, add, or overwrite data?
          WRITE (*,2005)
          WRITE(99,2005)
 25       CONTINUE
            WRITE (*,*) 'Do you want to Skip data, ',
     1                  'Overwrite data or Abort import',
     2                  ' (Enter S, O, or A) '
            READ (*,1000) CRESP
          IF (CRESP.NE.'S'.AND.CRESP.NE.'s'.AND.CRESP.NE.'O'.AND.
     1      CRESP.NE.'o'.AND.CRESP.NE.'A'.AND.CRESP.NE.'a') GO TO 25
          IF (CRESP.EQ.'S' .OR. CRESP.EQ.'s') THEN
C           skip cluster
            WRITE(99,*) 'Skipping DSN/CLU'
            SKIPFG= 2
          ELSE IF (CRESP.EQ.'O' .OR. CRESP.EQ.'o') THEN
C           overwrite cluster
            CALL WDDSDL (WDMSFL,CLU,
     O                   RETCOD)
            WRITE(99,*) 'Overwrite DSN/CLU'
            SKIPFG= 0
          ELSE
C           aborting import
            WRITE(99,*) 'Abort import'
            SKIPFG= 5
          END IF
        END IF
C
        IF (SKIPFG.EQ.0) THEN
C         copy label from import file to new cluster, first add label
          CALL WDLBAX (WDMSFL,CLU,CLUTYP,NDN,NUP,NSA,NSP,NDP,
     O                 PSA)
C         next attributes
 30       CONTINUE
            READ (SUCIFL,1000) (BUFF(I),I=1,I80)
            DONFG= STRFND(I3,BUFF(3),I3,CDAT)
          IF (STRFND(I3,BUFF(3),I3,CLAB).EQ.0.AND.DONFG.EQ.0)
     1      GOTO 30
C
          IF (DONFG.EQ.0) THEN
C           'LABEL' found in 30 loop, now in attributes
            RETCOD= 0
 40         CONTINUE
              READ (SUCIFL,1000) (BUFF(I),I=1,I80)
C             are we at end?
              IF (STRFND(I3,BUFF(3),I3,CEND).GT.0) THEN
C               yes, get out of this loop
                DONFG= 2
                SAIND= 0
              ELSE
C               which attribute
                CALL WDBSGX (ATMSFL,BUFF(5),
     O                       SAIND,SATYP,SALEN)
              END IF
              IF (SAIND.GT.0) THEN
C               valid attribute type
                GOTO (41,43,45), SATYP
C
 41             CONTINUE
C                 integer attribute
                  J= 1
                  DO 42 I= 1,SALEN
                    J= J+ 10
                    IF (J.GT.I80) THEN
                      J= J+ 10
                      READ(SUCIFL,1000) (BUFF(K),K=81,160)
                    END IF
                    IVAL(I)= CHRINT(I10,BUFF(J))
 42               CONTINUE
                  CALL WDBSAI (WDMSFL,CLU,ATMSFL,SAIND,SALEN,IVAL,
     O                         RETCOD)
                  GO TO 50
C
 43             CONTINUE
C                 real attribute
                  J= 1
                  DO 44 I= 1,SALEN
                    J= J+ 10
                    IF (J.GT.I80) THEN
                      J= J+ 10
                      READ(SUCIFL,1000) (BUFF(K),K=81,160)
                    END IF
                    RVAL(I)= CHRDEC(I10,BUFF(J))
 44               CONTINUE
                  CALL WDBSAR (WDMSFL,CLU,ATMSFL,SAIND,SALEN,RVAL,
     O                         RETCOD)
                  GO TO 50
C
 45             CONTINUE
C                 character attribute
                  IF ((SALEN+12).GT.I80) THEN
                    READ (SUCIFL,1000) (BUFF(K),K=81,148)
                  END IF
                  CALL WDBSAC (WDMSFL,CLU,ATMSFL,
     I                         SAIND,SALEN,BUFF(13),
     O                         RETCOD)
                  GO TO 50
C
 50             CONTINUE
              ELSE IF (DONFG.EQ.0) THEN
C               unknown attribute
                RETCOD= 1
              END IF
C
              IF (RETCOD.NE.0) THEN
C               problem writing out attributes
                WRITE (*,2010) RETCOD
                WRITE(99,2010) RETCOD
                RETCOD= 0
              END IF
            IF (DONFG.EQ.0) GO TO 40
C
          ELSE
C           no attributes found to write on a new cluster, skip
            WRITE (*,2015)
            WRITE(99,2015)
            SKIPFG= 2
          END IF
C
C         **** ADD POINTERS HERE SOMEDAY ****
C
        END IF
C
        IF (SKIPFG.NE.2 .AND. SKIPFG.NE.5) THEN
C         skip to data
          DONFG= 0
 60       CONTINUE
            READ (SUCIFL,1000) (BUFF(I),I=1,I80)
            IF (STRFND(I3,BUFF,I3,CEND).GT.0) DONFG= 1
          IF (STRFND(I3,BUFF(3),I3,CDAT).EQ.0.AND.DONFG.EQ.0) GOTO 60
C
          IF (DONFG.EQ.0) THEN
C           message file data data exists, now input it
            IF (CLUTYP.EQ.1) THEN
C             timeseries type data
              CALL PRWMTI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.2) THEN
C             table type data
              CALL PRWMXI (ATMSFL,WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.5) THEN
C             vector (DLG) type data
              CALL PRWMDI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.8) THEN
C             attribute type data
              CALL PRWMAI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.9) THEN
C             message type data
              CALL PRWMSI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            END IF
            IF (RETCOD .NE. 0) THEN
C             problems with import, let user know
              WRITE(*,*) '*** Problems with import, return code:',
     1                    RETCOD
              WRITE(*,*) '    review file "error.fil" for details'
            ELSE
              WRITE(*,*) 'DSN/CLU data import complete - no problems.'
            END IF
          END IF
        END IF
C
        IF (SKIPFG.NE.5) THEN
C         not aborting, look for more clusters
 800      CONTINUE
            READ (SUCIFL,1000,END=900) (BUFF(I),I=1,I80)
C           WRITE (*,1000) (BUFF(I),I=1,I80)
          IF (STRFND(I4,BUFF,I3,CCLU).EQ.0 .AND.
     1        STRFND(I4,BUFF,I3,CDSN).EQ.0) GO TO 800
        END IF
      IF (SKIPFG.NE.5) GO TO 15
 900  CONTINUE
C     close import file
      CLOSE (SUCIFL)
C     end of file, all clusters copied
      WRITE (*,2040)
      WRITE(99,2040)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDMEX
     I                   (SUCIFL,WDMSFL,ATMSFL)
C
C     + + + PURPOSE + + +
C     Export data sets (clusters) from WDM file to sequential file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SUCIFL,WDMSFL,ATMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUCIFL - Fortran unit number of sequential export file
C     WDMSFL - Fortran unit number of WDM file
C     ATMSFL - Fortran unit number of WDM file containing attributes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,JW,K,L,L1,L2,I80,LEN,CLU,CLUTYP,DSFREC,
     1            JUST,ANS,DIND,CLUIND,SAMAX,PSA,PSIND,SALEN,SATYP,
     2            PSAVAL,WRTFG,DONFG,CLUNXT,DREC,PFCLU,CLMXTP,
     3            LSDAT(6),LEDAT(6),GPFLG,RETCOD,DSAIND,UPDATR,
     4            RTSBYR,RTGRP,RNDP,FTSBYR,FTGRP
      REAL        R
      CHARACTER*1 CCLU(80),CSTR(52),BUFF(80),BLNK,CRESP,
     1            CDSTYP(36),CHCLU(3),CDSN(3),CHTYP(4),CNDN(3),
     2            CNUP(3),CNSA(3),CNSP(3),CNDP(3)
      CHARACTER*4 CDUM
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, WDDSCK, WDRCGO, CHRCHR, INTCHR, ZIPC, WDSAGY
      EXTERNAL    DECCHR, WTFNDT, PRWMTE, PRWMXE, PRWMDE, PRWMME, PRWMAE
      EXTERNAL    WTGPRV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I80,JUST,GPFLG/80,0,1/
      DATA CDSTYP/'T','I','M','E','T','A','B','L','S','C','H','E',
     1            'P','R','O','J','V','E','C','T','R','A','S','T',
     2            'S','P','T','I','A','T','T','R','M','E','S','S'/
      DATA BLNK/' '/
      DATA CSTR/'W','D','M','S','F','L',' ',' ','D','A','T','E',' ',
     1          ' ',' ',' ','S','Y','S','T','E','M',' ',' ','C','O',
     2          'M','M','E','N','T',' ','L','A','B','E','L',' ',' ',
     3          ' ','P','O','I','N','T',' ',' ',' ','E','N','D',' '/
      DATA CHCLU,CDSN,CHTYP/'C','L','U','D','S','N','T','Y','P','E'/
      DATA CNDN,CNUP/'N','D','N','N','U','P'/
      DATA CNSA,CNSP,CNDP/'N','S','A','N','S','P','N','D','P'/
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (4A1)
 1020 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2001 FORMAT (4X,A6,I10)
 2005 FORMAT (' Completed export of headers and comments',/)
 2008 FORMAT (' Beginning export of DSN/CLU ',I5,' type is ',4A1)
 2010 FORMAT (A4)
 2015 FORMAT (' DSN/CLU ',I5,' does not exist')
 2020 FORMAT (' Completed export of DSN/CLU ',I5)
 2030 FORMAT (' No data to export for DSN/CLU ',I5)
 2040 FORMAT (/,' Export of specified DSN/CLUs complete.')
C
C     + + + END SPECIFICATIONS + + +
C
C     write out general headings
C     date
      WRITE (SUCIFL,2000) (CSTR(I),I=9,16)
C     wdmsfl
      WRITE (SUCIFL,2000) (CSTR(I),I=1,8)
C     system
      WRITE (SUCIFL,2000) (CSTR(I),I=17,24)
C     comment
      WRITE (SUCIFL,2000) (CSTR(I),I=25,32)
C
C     prompt for comment lines
      WRITE (*,*) 'Enter comment line(s) for export file, ',
     1            'use Enter (carriage return) to quit.'
      WRITE (*,*)
 10   CONTINUE
        CALL ZIPC (I80,BLNK,BUFF)
        READ (*,1020) BUFF
        I= LENSTR(I80,BUFF)
        IF (I.GT.0) WRITE (SUCIFL,2000) BLNK,BLNK,(BUFF(J),J=1,I)
      IF (I.GT.0) GO TO 10
C     end comments
      WRITE (SUCIFL,2000) (CSTR(J),J=49,52),(CSTR(I),I=25,32)
C
      WRITE(99,2005)
C
 15   CONTINUE
C       archive all or selected clusters?
        WRITE (*,*) 'Export All or Selected DSN/CLUs (A,S)? '
        READ (*,1020) CRESP
        ANS= 1
        IF (CRESP.EQ.'A' .OR. CRESP.EQ.'a') THEN
          CLUIND= -1
          CLUNXT= 0
          CLMXTP= 9
          CLU   = 0
        ELSE IF (CRESP.EQ.'S' .OR. CRESP.EQ.'s') THEN
          CLUIND= 0
        ELSE
          ANS= 0
        END IF
      IF (ANS.EQ.0) GO TO 15
C
      CLUTYP= 0
C
      UPDATR= 0
C     write cluster loop
 20   CONTINUE
        IF (CLUIND.EQ.-1) THEN
C         get next cluster to archive from directory
 25         CONTINUE
            IF (CLUNXT.EQ.0) THEN
C             no clusters of current type, try next one
              CLUTYP= CLUTYP+ 1
              IF (CLUTYP.LE.CLMXTP) THEN
C               get directory record
                DREC  = 1
                DIND  = WDRCGO(WDMSFL,DREC)
                PFCLU = PTSNUM+ (CLUTYP- 1)* 2+ 1
                CLUNXT= WIBUFF(PFCLU,DIND)
              END IF
            END IF
          IF (CLUNXT.EQ.0 .AND. CLUTYP.LT.CLMXTP) GO TO 25
C
          CLU= CLUNXT
C
          IF (CLU.GT.0) THEN
C           get next cluster
            CALL WDDSCK (WDMSFL,CLU,
     O                   DREC,RETCOD)
            DIND  = WDRCGO(WDMSFL,DREC)
            CLUNXT= WIBUFF(2,DIND)
          END IF
C
        ELSE
C         clusters being specified
          WRITE(*,*)
          WRITE(*,*) 'Enter DSN/CLU to export ',
     1               '(0 to indicate export complete)? '
          READ (*,*) CLU
        END IF
        IF (CLU.GT.0) THEN
C         another cluster to export
          CALL WDDSCK (WDMSFL,CLU,
     O                 DSFREC,RETCOD)
          IF (DSFREC.EQ.0) THEN
C           cluster doesnt exist
            WRITE (*,2015) CLU
            WRITE(99,2015) CLU
          ELSE
            DIND= WDRCGO(WDMSFL,DSFREC)
C           init template cluster record
            CALL ZIPC (I80,BLNK,CCLU)
C           fill in type
            CLUTYP= WIBUFF(6,DIND)
C           assume no attribute update for this one
            IF (CLUTYP.EQ.1) THEN
              IF (UPDATR.LE.1) THEN
                WRITE(*,*) 'Update ATTRs and PTRs (Yes,No,All,Xnone)?'
                READ (*,1020) CRESP
                IF  (CRESP.EQ.'N' .OR. CRESP.EQ.'n') THEN
                  UPDATR= 0
                ELSE IF (CRESP.EQ.'Y' .OR. CRESP.EQ.'y') THEN
                  UPDATR= 1 
                ELSE IF (CRESP.EQ.'A' .OR. CRESP.EQ.'a') THEN
                  UPDATR= 3
                ELSE IF (CRESP.EQ.'X' .OR. CRESP.EQ.'x') THEN
                  UPDATR= 2
                ELSE
C                 assume no
                  UPDATR= 0
                END IF
              END IF
              IF (MOD(UPDATR,2).EQ.1) THEN
                CALL WTGPRV(WDMSFL,CLU,99,
     O                      RTSBYR,RTGRP,RNDP)  
              END IF
            ELSE
C             dont know how to update non timeseries
              UPDATR= 0
            END IF
            DIND= WDRCGO(WDMSFL,DSFREC)
C           type of cluster being exported
            J= 4*(CLUTYP-1)+ 1
            WRITE(99,2008) CLU,(CDSTYP(I),I=J,J+3)
            WRITE (*,2008) CLU,(CDSTYP(I),I=J,J+3)
C
            I = 3
            IF (CLUTYP.EQ.9) THEN
C             use CLUster as header for start of data
              CALL CHRCHR (I,CHCLU,CCLU)
            ELSE
C             use DSN as header for start of data
              CALL CHRCHR (I,CDSN,CCLU)
            END IF
            LEN= 6
            CALL INTCHR (CLU,LEN,JUST,
     O                   J,CCLU(11))
            I  = 4
            CALL CHRCHR (I,CHTYP,CCLU(21))
            J= 4*(CLUTYP-1)+ 1
            CALL CHRCHR (I,CDSTYP(J),CCLU(27))
C           fill in ndn
            K= WIBUFF(9,DIND)- WIBUFF(8,DIND)- 1
            LEN= 3
            CALL CHRCHR (LEN,CNDN,CCLU(34))
            CALL INTCHR (K,LEN,JUST,
     O                   J,CCLU(38))
C           fill in nup
            K= WIBUFF(10,DIND)- WIBUFF(9,DIND)- 1
            CALL CHRCHR (LEN,CNUP,CCLU(44))
            CALL INTCHR (K,LEN,JUST,
     O                   J,CCLU(48))
C           fill in nsa
            K= (WIBUFF((WIBUFF(10,DIND)+1),DIND)-WIBUFF(10,DIND)-2)/2
            CALL CHRCHR (LEN,CNSA,CCLU(54))
            CALL INTCHR (K,LEN,JUST,
     O                   J,CCLU(58))
C           fill in nsasp (nsp)
            K= WIBUFF(11,DIND)-WIBUFF(WIBUFF(10,DIND)+1,DIND)
            CALL CHRCHR (LEN,CNSP,CCLU(64))
            CALL INTCHR (K,LEN,JUST,
     O                   J,CCLU(68))
C           fill in ndp
            IF (MOD(UPDATR,2).EQ.1) THEN
C             revised number of data pointers
              K = RNDP
            ELSE
              K= WIBUFF(12,DIND)- WIBUFF(11,DIND)- 2
            END IF
            CALL CHRCHR (LEN,CNDP,CCLU(74))
            CALL INTCHR (K,LEN,JUST,
     O                   J,CCLU(78))
C           write out label parms
            WRITE (SUCIFL,2000) (CCLU(J),J=1,80)
C
C           *** add label comments ***
C
            WRITE (SUCIFL,2000) BLNK,BLNK,(CSTR(I),I=33,40)
            PSA  = WIBUFF(10,DIND)
            SAMAX= WIBUFF(PSA,DIND)
            IF (SAMAX.GT.0) THEN
              IF (MOD(UPDATR,2).EQ.1) THEN
C               be sure to write out TGROUP and TSBYR
                FTGRP = 0
                FTSBYR= 0
              ELSE
C               no updates, dont care
                FTGRP = 1
                FTSBYR= 1
              END IF
              I= 0
 30           CONTINUE
                I     = I+ 1
                PSIND = PSA+ (I*2)
                DSAIND= WIBUFF(PSIND,DIND)
                PSAVAL= WIBUFF(PSIND+1,DIND)
                CALL ZIPC (I80,BLNK,BUFF)
                CALL WDSAGY (ATMSFL,DSAIND,
     O                       BUFF(5),L,SATYP,SALEN,L,L)
C               be sure label still in memory
                DIND = WDRCGO(WDMSFL,DSFREC)
                IF (SATYP.EQ.3) SALEN= SALEN/4
                DONFG= 0
                WRTFG= 0
                LEN  = 10
                J    = 0
                JW   = 1
C
 31             CONTINUE
                  GOTO (33,35,37), SATYP
 33               CONTINUE
C                   integer type
                    K= WIBUFF(PSAVAL+J,DIND)
                    IF (DSAIND.EQ.85 .AND. K.EQ.0) THEN
C                     fixes bad vbtime values from illinois
                      K= 2
                    END IF
                    IF (DSAIND.EQ.34 .AND. FTGRP.EQ.0) THEN
                      K    = RTGRP
                      FTGRP= 1  
                    END IF
                    IF (DSAIND.EQ.27 .AND. FTSBYR.EQ.0) THEN
                      K     = RTSBYR
                      FTSBYR= 1
                    END IF
                    CALL INTCHR (K,LEN,JUST,
     O                           L,BUFF(JW*10+1))
                    IF (J.EQ.7) WRTFG= 1
                    GO TO 39
C
 35               CONTINUE
C                   real type
                    R= WRBUFF(PSAVAL+J,DIND)
                    CALL DECCHR (R,LEN,JUST,
     O                           L,BUFF(JW*10+1))
                    IF (J.EQ.7) WRTFG= 1
                    GO TO 39
C
 37               CONTINUE
C                   character type
                    WRITE (CDUM,2010) WIBUFF(PSAVAL+J,DIND)
                    L1= JW*4+ 9
                    L2= L1+ 3
                    READ (CDUM,1010) (BUFF(L),L=L1,L2)
                    IF (L2.EQ.80) WRTFG= 1
                    GO TO 39
C
 39               CONTINUE
C
                  J = J+ 1
                  JW= JW+ 1
                  IF (J.GE.SALEN) THEN
                    WRTFG= 1
                    DONFG= 1
                  END IF
                  IF (WRTFG.EQ.1) THEN
                    WRITE (SUCIFL,2000) BUFF
                    JW   = 0
                    WRTFG= 0
                    CALL ZIPC(I80,BLNK,BUFF)
                  END IF
                IF (DONFG.EQ.0) GO TO 31
              IF (I.LT.SAMAX) GO TO 30
            END IF
            IF (FTGRP .EQ. 0) THEN
              WRITE(SUCIFL,2001) 'TGROUP',RTGRP
            END IF
            IF (FTSBYR.EQ. 0) THEN
              WRITE(SUCIFL,2001) 'TSBYR ',RTSBYR
            END IF 
C
C           write end label
            WRITE (SUCIFL,2000) BLNK,BLNK,(CSTR(J),J=49,52),
     1                                    (CSTR(I),I=33,40)
C
C           *** add pointer write here ****
C
            IF (CLUTYP.EQ.1) THEN
C             timseries data set
C             what data is avialable for this dsn?
              CALL WTFNDT (WDMSFL,CLU,GPFLG,
     O                     DSFREC,LSDAT,LEDAT,RETCOD)
              IF (RETCOD.NE.0) THEN
C               no data to export
                WRITE (*,2030) CLU
                WRITE(99,2030) CLU
              ELSE
C               export the data
                CALL PRWMTE (WDMSFL,SUCIFL,CLU,LSDAT,LEDAT)
              END IF
            ELSE IF (CLUTYP.EQ.2) THEN
C             table type data
              CALL PRWMXE (ATMSFL,WDMSFL,SUCIFL,CLU)
            ELSE IF (CLUTYP.EQ.5) THEN
C             vector (DLG) data cluster
              CALL PRWMDE (WDMSFL,SUCIFL,CLU)
            ELSE IF (CLUTYP.EQ.8) THEN
C             attribute data cluster
              CALL PRWMAE (WDMSFL,SUCIFL,CLU)
            ELSE IF (CLUTYP.EQ.9) THEN
C             message file cluster
              CALL PRWMME (WDMSFL,SUCIFL,CLU)
            END IF
C
C           write end cluster
            WRITE (SUCIFL,2000) (CSTR(J),J=49,52),(CCLU(I),I=1,4)
C
C           indicate cluster exported
            WRITE (*,2020) CLU
            WRITE(99,2020) CLU
C
          END IF
        END IF
      IF (CLU.GT.0) GO TO 20
C
C     all done, close export file
      CLOSE (SUCIFL)
C
      WRITE(99,2040)
C
      RETURN
      END
