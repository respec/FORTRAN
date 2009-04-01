C
C
C
      SUBROUTINE   PRWMSE
     I                    (MESSFL,WDMSFL,MAXDSN,PTHNAM,
     M                     DSN,DSNCNT)
C
C     + + + PURPOSE + + +
C     select for and edit the dataset buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,MAXDSN,DSNCNT
      INTEGER     DSN(MAXDSN)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     MAXDSN - maximum number of dataset numbers allowed
C     PTHNAM - character string of path of options selected to get here
C     DSN    - buffer of dataset numbers
C     DSNCNT - count of dataset numbers in buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I0,I1,SCLU,SGRP,ANS,DSNTMP(25),ITMP(1),
     $             LEN,JUST,IRET,NUMDSN,ILINE,IOFL,IHOW,I25,
     $             IVAL(2),I2,I24,DSNINP(10)
      CHARACTER*1  BLNK(1),OBUFF(80)
      LOGICAL      GOBACK
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP,  PRNTXT, ZIPC, ZIPI, QFOPEN, QFCLOS
      EXTERNAL     INTCHR, PRWMLF, ASRTIP, PMXCNW, ZMNSST, PRWMBA
      EXTERNAL     ZSTCMA, ZGTRET, PMXTXI, ZBLDWR, ZWNSOP
      EXTERNAL     PRWMBD, SCANWD, SCANBF, Q1EDIT, Q1INIT
      EXTERNAL     QGETI, QSETI, SHIFTI, QGTCOB, QSTCOB
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (10I6)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2 
      I24 = 24
      I25 = 25
      BLNK(1)= ' '
      IVAL(1) = 1
      IVAL(2) = 1
      SCLU= 24
C
 10   CONTINUE
C       set prefix to window names
        CALL ZWNSOP (I1,PTHNAM)
C       show number of datasets in buffer
        IF (DSNCNT .GT. 1) THEN
          SGRP = 6
          ITMP(1)= DSNCNT
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,ITMP)
        ELSE
C         0 or 1 data set in buffer
          IF (DSNCNT .EQ. 1) THEN
            SGRP= 8
          ELSE
            SGRP= 9
          END IF
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,-I1,J)
        END IF
C       save text for menu
        CALL ZMNSST
C
C       buffer management option: 1-Add,2-Drop,3-List,4-Clear,5-Sort,
C                                 6-Browse,7-Find,8-Input,9-Output,
C                                 10-Return
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,ANS)
        GO TO (100,120,140,160,180,200,300,400,500,900), ANS
 100    CONTINUE
C         add
C         clear temporary buffer                       
          CALL ZIPI (I25,I0,DSNTMP)
          IRET= 0
C         make prev avail
          J= 4
          CALL ZSTCMA(J,I1)
C         do screen to get data set numbers
          CALL ZWNSOP (I1,PTHNAM)
C         single dsn number
          SGRP  = 2
          CALL Q1INIT (MESSFL, SCLU, SGRP)
          DSNTMP(1) = MAXDSN - DSNCNT
          IF (DSNTMP(1) .GT. 24) DSNTMP(1) = 24
          CALL QSETI(I25,DSNTMP)
          CALL Q1EDIT (IRET)
          IF (IRET.NE.2) THEN
C           user doesnt want prev, continue
            CALL QGETI(I25,DSNTMP)
C           shift left to clear zeros
            CALL SHIFTI (I24,I0,I0,DSNTMP(2),DSNTMP(1))
C           add data sets to buffer if they exist
            IF (DSNTMP(1).GT.0) THEN
C             at least one dsn to add
              CALL PRWMBA(MESSFL,SCLU,WDMSFL,DSNTMP(1),DSNTMP(2),MAXDSN,
     M                    DSNCNT,DSN,
     O                    IRET)
            END IF
          END IF
C         turn off prev
          CALL ZSTCMA(J,I0)
          GO TO 950
C
 120    CONTINUE
C         remove dsn from buffer
C         clear temporary buffer                          
          CALL ZIPI (I25,I0,DSNTMP)
          IRET= 0
C         make prev avail
          J= 4
          CALL ZSTCMA(J,I1)
          IF (DSNCNT .GT. 0) THEN
C           data-set buffer contains at least 1 data set
 125        CONTINUE
C             do screen to get data set numbers
              CALL ZWNSOP (I1,PTHNAM)
C             single dsn number
              SGRP  = 22
              CALL Q1INIT (MESSFL, SCLU, SGRP)
              DSNTMP(1) = MAXDSN - DSNCNT
              IF (DSNTMP(1) .GT. 24) DSNTMP(1) = 24
              CALL QSETI(I25,DSNTMP)
              CALL Q1EDIT (IRET)
              IF (IRET .EQ. 1) THEN
C               drop data sets from the buffer
                CALL QGETI(I25,DSNTMP)
C               shift left to clear zeros
                CALL SHIFTI (I24,I0,I0,DSNTMP(2),DSNTMP(1))
C               delete data sets from the buffer                      
                CALL PRWMBD (MESSFL,SCLU,WDMSFL,DSNTMP(1),
     I                       DSNTMP(2),MAXDSN,
     M                       DSNCNT,DSN)
                GOBACK = .FALSE.
              ELSE IF (IRET .EQ. -1) THEN
C               oops, try again
                GOBACK = .TRUE.
              ELSE
C               assume previous
                GOBACK = .FALSE.
              END IF
            IF ( GOBACK ) GO TO 125
          ELSE
C           no data sets in buffer to remove
            SGRP= 10
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
C         turn off prev
          CALL ZSTCMA(J,I0)
C
          GO TO 950
C
 140    CONTINUE
C         list dsn in buffer, first set window name
          CALL ZWNSOP (I1,PTHNAM)
          IF (DSNCNT .GT. 1) THEN
C           lots of datasets in buffer
            SGRP = 6
            ITMP(1) = DSNCNT
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,ITMP)
          ELSE IF (DSNCNT .EQ. 1) THEN
C           only one dataset in buffer
            SGRP = 8
            CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,-I1,I)
          ELSE
C           empty buffer
            SGRP = 9
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          IF (DSNCNT .GT. 0) THEN
            I   = 0
            JUST= 0
            CALL ZBLDWR (I1,BLNK,I0,-I1,K)
C           keep track of number of lines output
            ILINE= 3
 145        CONTINUE
              LEN = 80
              CALL ZIPC (LEN,BLNK,OBUFF)
              LEN = 1
 150          CONTINUE
                I= I+ 1
                J= 7
                CALL INTCHR (DSN(I),J,JUST,
     M                       K,OBUFF(LEN))
                LEN= LEN+ 8
              IF (LEN .LT. 41 .AND. I .LT. DSNCNT) GO TO 150
C
              ILINE= ILINE+ 1
              IF (ILINE.EQ.49) THEN
C               last line allowed to put text to
                I= DSNCNT
C               show more data sets out there
                OBUFF(LEN)  = '.'
                OBUFF(LEN+1)= '.'
                OBUFF(LEN+2)= '.'
              END IF
              CALL ZBLDWR (LEN,OBUFF,I0,-I1,K)
            IF (I .LT. DSNCNT) GO TO 145
            LEN= 0
            CALL ZBLDWR (LEN,BLNK,I0,I0,K)
          END IF
          GO TO 950
C
 160    CONTINUE
C         clear out buffer
          DSNCNT = 0
          CALL ZIPI (MAXDSN,I0,DSN)
          GO TO 950
C
 180    CONTINUE
C         sort by dsn
          IF (DSNCNT.GT.0) THEN
C           only if some data sets to sort
            CALL ASRTIP (DSNCNT,
     M                   DSN)
          END IF
          GO TO 950
C
 200    CONTINUE
C         browse option
C
 205      CONTINUE
            GOBACK = .FALSE.
C           make prev avail
            J= 4
            CALL ZSTCMA(J,I1)
C
C           display Browse selection screen
            SGRP = 7
 210        CONTINUE
              CALL ZWNSOP (I1,PTHNAM)
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              CALL QSTCOB (I2,I1,IVAL)
              CALL Q1EDIT (IRET)
            IF (IRET .EQ. -1) GO TO 210
            IF (IRET .EQ. 1) THEN
C             continue with scan
              CALL QGTCOB (I2,I1,IVAL)
              IHOW = IVAL(2)
              IF (IVAL(1) .EQ. 1) THEN
C               wants to scan WDM file for data sets
                CALL SCANWD (MESSFL,SCLU,WDMSFL,IHOW,MAXDSN,PTHNAM,
     M                       DSN,DSNCNT,
     O                       GOBACK)
C
              ELSE
C               wants to scan data set buffer
                IF (DSNCNT .GT. 0) THEN
                  CALL SCANBF (MESSFL,SCLU,WDMSFL,IHOW,MAXDSN,PTHNAM,
     M                         DSN,DSNCNT,
     O                         GOBACK)
                ELSE
C                 no data sets in buffer
                  SGRP = 9
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
              END IF
            ELSE IF (IRET .EQ. 2) THEN
C             user chose Prev--go back to Select menu
            END IF
C         if user chose Prev on 1st scan screen, return to Browse selection
          IF (GOBACK) GOTO 205
C
C         turn off prev
          J= 4
          CALL ZSTCMA(J,I0)
          GO TO 950
C
 300    CONTINUE
C         find dataset numbers
          CALL PRWMLF (MESSFL,WDMSFL,SCLU,MAXDSN,
     M                 DSN,DSNCNT)
          IF (DSNCNT.GT.0) THEN
C           sort after find if data sets to sort
            CALL ASRTIP (DSNCNT,
     M                   DSN)
          END IF
          GO TO 950
C
 400    CONTINUE
C         input from file
C         make prev avail
          J= 4
          CALL ZSTCMA(J,I1)
C         open an input file
          SGRP= 3
          CALL QFOPEN (MESSFL,SCLU,SGRP,IOFL,IRET)
C         get return code
          CALL ZGTRET (I)
          IF (I .EQ. 2) THEN
C           user selected prev
            IRET= 1
          END IF
C
          IF (IRET .EQ. 0) THEN
 410        CONTINUE
              I = 10
              CALL ZIPI (I, I0, DSNINP) 
C             read a record from input file
              READ(IOFL,1000,END=430,ERR=420) DSNINP
C             how many dsns are on it
              NUMDSN= 10
 415          CONTINUE
                J= 0
                IF (DSNINP(NUMDSN) .EQ. 0) THEN
C                 not one here
                  NUMDSN= NUMDSN- 1
                  J     = 1
                END IF
              IF (J .EQ. 1 .AND. NUMDSN .GT. 0) GO TO 415
C
              IF (NUMDSN .GT. 0) THEN
C               something to add to perm buffer
                CALL PRWMBA (MESSFL,SCLU,WDMSFL,NUMDSN,DSNINP,MAXDSN,
     M                       DSNCNT,DSN,
     O                       IRET)
              ELSE
C               some problem
                IRET= 1
              END IF
            IF (IRET .EQ. 0) GO TO 410 
 420        CONTINUE
C             read error
              SGRP= 11
              CALL PRNTXT (MESSFL,SCLU,SGRP)
 430        CONTINUE
C             close input file
              CALL QFCLOS(IOFL,I0)
          END IF
C
C         turn off prev
          CALL ZSTCMA(J,I0)
          GO TO 950
C
 500    CONTINUE
C         output to file
C         make prev avail
          J= 4
          CALL ZSTCMA(J,I1)
C         open an output file
          SGRP= 4
          CALL QFOPEN (MESSFL,SCLU,SGRP,IOFL,IRET)
C         get return code
          CALL ZGTRET (I)
          IF (I .EQ. 2) THEN
C           user selected prev
            IRET= 1
          END IF
C
          IF (IRET .EQ. 0) THEN
C           ok to write buffer to file
            WRITE(IOFL,1000) (DSN(I),I=1,DSNCNT)
C           close output file
            CALL QFCLOS(IOFL,I0)
          END IF
C         turn off prev
          CALL ZSTCMA(J,I0)
          GO TO 950
C
 900    CONTINUE
C         done
          GO TO 950
C
 950    CONTINUE
C
      IF (ANS .NE. 10) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMBA
     I                   (MESSFL,SCLU,WDMSFL,NUMDSN,DSNTMP,MAXDSN,
     M                    DSNCNT,DSN,
     O                    IRET)
C
C     + + + PURPOSE + + +
C     add datasets from either add or input to buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,NUMDSN,DSNTMP(NUMDSN),MAXDSN,
     $          DSNCNT,DSN(MAXDSN),IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing information for this routine
C     WDMSFL - Fortran unit number of WDM file
C     NUMDSN - number of datasets to try to add to buffer
C     DSNTMP - array containing data-set number to try to add
C     MAXDSN - maximum number of dataset numbers allowed
C     DSNCNT - count of dataset numbers in buffer
C     DSN    - buffer of dataset numbers
C     IRET   - return code, 0 - ok, 1 - user wants to interrupt
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,I,I1,I0,FLG,SGRP,INIT,RREC,RETC
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, PMXTXI, ZBLDWR, ZSTCMA
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      BLNK(1)= ' '
C     initialize output screen
      INIT= 0
C     assume ok exit
      IRET= 0
C
      DO 150 I= 1,NUMDSN
C       try to add data-set numbers user specified
        CALL WDDSCK (WDMSFL,DSNTMP(I),RREC,RETC)
        IF (RETC .EQ. 0) THEN
C         dsn exists, be sure not in buffer
          FLG= 0
          J  = 0
 110      CONTINUE
            J= J+ 1
            IF (DSN(J) .EQ. DSNTMP(I)) THEN
C             data-set already in buffer
              FLG= 1
            END IF
          IF (J .LT. DSNCNT .AND. FLG .EQ. 0) GO TO 110
C
          IF (FLG .EQ. 1) THEN
C           data-set already in buffer
            SGRP= 61
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,I1,I1,DSNTMP(I))
            INIT= -1
          ELSE IF (DSNCNT .LT. MAXDSN) THEN
C           add data set to buffer
            DSNCNT= DSNCNT+ 1
            DSN(DSNCNT)= DSNTMP(I)
          ELSE
C           buffer full
            SGRP= 63
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,I1,I1,DSNTMP(I))
            INIT= -1
          END IF
        ELSE
C         dsn does not exist
          SGRP= 62
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,I1,I1,DSNTMP(I))
          INIT= -1
        END IF
 150  CONTINUE
C
      IF (INIT .LT. 0) THEN
C       problems with some of the data sets
C       make interrupt available
        J= 16
        CALL ZSTCMA(J,I1)
C       hold screen
        CALL ZBLDWR (I0,BLNK,I0,I0,J)
        IF (J .EQ. 7) THEN
C         user wants to interrupt
          IRET= 1
        END IF
C       turn off interrupt
        J= 16
        CALL ZSTCMA(J,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMBD
     I                   (MESSFL,SCLU,WDMSFL,NUMDSN,DSNTMP,MAXDSN,
     M                    DSNCNT,DSN)
C
C     + + + PURPOSE + + +
C     Delete datasets from Eliminate option to buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,NUMDSN,DSNTMP(NUMDSN),MAXDSN,
     $          DSNCNT,DSN(MAXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing information for this routine
C     WDMSFL - Fortran unit number of WDM file
C     NUMDSN - number of datasets to try to add to buffer
C     DSNTMP - array containing data-set number to try to add
C     MAXDSN - maximum number of dataset numbers allowed
C     DSNCNT - count of dataset numbers in buffer
C     DSN    - buffer of dataset numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,I,I1,I0,FLG,SGRP,INIT
      CHARACTER*1 BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXTXI, ZBLDWR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      BLNK(1)= ' '
C     initialize output screen
      INIT= 0
C
      DO 250 I= 1,NUMDSN
C       try to remove specified data sets from buffer
        FLG= 0
        J  = 0
 210    CONTINUE
          J= J+ 1
          IF (DSN(J) .EQ. DSNTMP(I)) THEN
C           data set is in buffer
            FLG= 1
          END IF
        IF (J .LT. DSNCNT .AND. FLG .EQ. 0) GO TO 210
        IF (FLG .EQ. 1) THEN
C         dsn in buffer, remove it
          IF (J .LT. DSNCNT) THEN
 220        CONTINUE
              DSN(J)= DSN(J+1)
              J= J+ 1
            IF (J .LT. DSNCNT) GO TO 220
          END IF
          DSN(J)= 0
          DSNCNT= DSNCNT- 1
        ELSE
C         dsn not in buffer
          SGRP= 5
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,I1,I1,DSNTMP(I))
          INIT= -1
        END IF
 250  CONTINUE
      IF (INIT .LT. 0) THEN
C       problems with some of the data sets, hold screen
        CALL ZBLDWR (I0,BLNK,I0,I0,J)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMLF
     I                    (MESSFL,WDMSFL,SCLU,MAXDSN,
     M                     DSN,DSNCNT)
C
C     + + + PURPOSE + + +
C     finds dataset numbers within the WDMS file
C     based on user supplied specifications
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,SCLU,MAXDSN,DSNCNT
      INTEGER     DSN(MAXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing information for this routine
C     MAXDSN - maximum number of dataset numbers allowed
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers in DSN
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cparam.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,TYPE,STYPE,I,K,L,M,DSNMIN(1),DSNMAX(1),
     $            ANS,NSAFLG,DSBFUL,SACNT,SAORIX,RETCOD,DSTYPS(7),
     $            I1, I2, I3, I6, AGAIN, RTCMND
      INTEGER*4   SAVAL(30)
      CHARACTER*4 CDUM
      CHARACTER*1 OBUFF(80),TYPSTR(12),BLNK(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, QRESP, QRESPS, PRNTXT
      EXTERNAL   Q1INIT, QSTCTF, Q1EDIT, QGETIB, QGETCO
      EXTERNAL   QGETRB, QGTCTF, QGTCOB
      EXTERNAL   ZIPI, WDSAGX, PRWFLS, PRWFDS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DSTYPS/0,1,2,5,7,8,9/
      DATA  I1, I2, I3, I6
     $     / 1,  2,  3,  6 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A1)
C
C     + + + END SPECIFICATIONS + + +
C
      DSBFUL = 0
      BLNK(1)= ' '
C
C     init to search for all data set types
      TYPE= 0
      I   = 12
      CALL ZIPC (I,BLNK,TYPSTR)
      TYPSTR(1)= 'A'
      TYPSTR(2)= 'l'
      TYPSTR(3)= 'l'
C
C     init to not use any attributes
      SAORIX= 1
      SACNT = 0
C     fill 'OR' index buffer with 0
      CALL ZIPI (SACNMX,SACNT,SAOR)
C
 12   CONTINUE
C       Find type: 1-Type,2-Number,3-Subset,4-Attributes,5-Execute,6-Return
        SGRP= 23
        CALL QRESP (MESSFL,SCLU,SGRP,STYPE)
C
        IF (STYPE .EQ. 1) THEN
C         determine which dataset type
          SGRP= 24
          I   = 12
          CALL QRESPS (MESSFL,SCLU,SGRP,I,I1,
     O                 TYPSTR,TYPE)
          TYPE= DSTYPS(TYPE)
        ELSE IF (STYPE .EQ. 2) THEN
C         search by dsn, get range
 14       CONTINUE
            SGRP = 40
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL Q1EDIT ( RTCMND )
            IF (RTCMND .EQ. 1) THEN
              CALL QGETIB ( I1, I1, DSNMIN )
              CALL QGETIB ( I1, I2, DSNMAX )
              AGAIN = 0
            ELSE
C             assume oops
              AGAIN = 1
            END IF
          IF (AGAIN .EQ. 1) GO TO 14
          TYPE  = -1
        ELSE IF (STYPE .EQ. 3) THEN
C         search thru buffer
          IF (DSNCNT .EQ. 0) THEN
C           buffer is empty
            SGRP= 46
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            TYPE= -3
          ELSE
            TYPE= -2
          END IF
        ELSE IF (STYPE .EQ. 4) THEN
C         which attributes
 15       CONTINUE
C           come back here if an 'OR' condition is added
 20         CONTINUE
              SACNT= SACNT+ 1
              CALL WDSAGX (MESSFL,
     O                     SAIND(SACNT),SANAM(1,SACNT),
     $                     SATYP(SACNT),SALEN(SACNT),NSAFLG)
C
              IF (NSAFLG .EQ. 0) THEN
C               not done
                IF (SAIND(SACNT) .GT. 0) THEN
C                 attribute is valid, store where string or value starts
                  IF (SACNT .EQ. 1) THEN
                    SABEG(SACNT)= 1
                  ELSE
                    IF (SATYP(SACNT-1) .EQ. 3) THEN
C                     previous type was character
                      SABEG(SACNT)= SABEG(SACNT-1)+ (SALEN(SACNT-1)/4)
                    ELSE
                      SABEG(SACNT)= SABEG(SACNT-1)+ SALEN(SACNT-1)
                    END IF
                  END IF
C                 get a value or range
                  I= SATYP(SACNT)
                  IF (I .EQ. 1) THEN
C                   integer type, get min, max, and t/f
 22                 CONTINUE
                      SGRP = 32
                      CALL Q1INIT ( MESSFL, SCLU, SGRP )
                      CALL QSTCTF ( I2, I6, SANAM(1,SACNT) )
                      CALL Q1EDIT ( RTCMND )
                      IF (RTCMND .EQ. 1) THEN
C                       get values
                        CALL QGETIB ( I1, I1, SAIMIN(SACNT) )
                        CALL QGETIB ( I1, I2, SAIMAX(SACNT) )
                        CALL QGETCO ( I1, SACOND(SACNT) )
                        AGAIN = 0
                      ELSE
C                       assume oops
                        AGAIN = 1
                      END IF
                    IF (AGAIN .EQ. 1) GO TO 22
                  ELSE IF (I .EQ. 2) THEN
C                   real type, get min, max, and t/f
 24                 CONTINUE
                      SGRP = 34
                      CALL Q1INIT ( MESSFL, SCLU, SGRP )
                      CALL QSTCTF ( I2, I6, SANAM(1,SACNT) )
                      CALL Q1EDIT ( RTCMND )
                      IF (RTCMND .EQ. 1) THEN
C                       get values
                        CALL QGETRB ( I1, I1, SARMIN(SACNT) )
                        CALL QGETRB ( I1, I2, SARMAX(SACNT) )
                        CALL QGETCO ( I1, SACOND(SACNT) )
                        AGAIN = 0
                      ELSE
C                       assume oops
                        AGAIN = 1
                      END IF
                    IF (AGAIN .EQ. 1) GO TO 24
                  ELSE
C                   better be character, get value and t/f
 26                 CONTINUE
                      SGRP = 36
                      CALL Q1INIT ( MESSFL, SCLU, SGRP )
                      CALL QSTCTF ( I3, I6, SANAM(1,SACNT) )
                      CALL Q1EDIT ( RTCMND )
                      IF (RTCMND .EQ. 1) THEN
C                       get values
                        CALL QGTCTF ( I1, SALEN(SACNT), OBUFF )
                        CALL QGTCOB ( I1, I2, SACOND(SACNT) )
                        AGAIN = 0
                      ELSE
C                       assume oops
                        AGAIN = 1
                      END IF
                    IF (AGAIN .EQ. 1) GO TO 26
C                   put string stored as 1 char/word into 4/word
                    K= SABEG(SACNT)
                    DO 27 L= 1,SALEN(SACNT),4
                      WRITE(CDUM,2000) (OBUFF(M),M=L,L+3)
                      READ (CDUM,1000) SAVAL(K)
                      K= K+ 1
 27                 CONTINUE
                  END IF
C
                  ANS= 1
                  IF (SACNT .LT. SACNMX) THEN
C                   will more be used
                    SGRP= 26
                    CALL QRESP (MESSFL,SCLU,SGRP,ANS)
                  END IF
                END IF
              ELSE
                ANS= 1
              END IF
            IF (ANS .EQ. 2) GO TO 20
            IF (SAIND(SACNT) .EQ. 0 .OR. SAIND(SACNT) .EQ. 38) THEN
C             dont count this attribute
              SACNT= SACNT- 1
            END IF
C
            SAOR(SAORIX)= SACNT
            SAORIX= SAORIX+ 1
            ANS   = 1
            IF (SACNT .LT. SACNMX .AND. NSAFLG .EQ. 0) THEN
C             should 'or' condition be used
              SGRP= 44
              CALL QRESP (MESSFL,SCLU,SGRP,ANS)
            END IF
          IF (ANS .EQ. 2) GO TO 15
        ELSE IF (STYPE .EQ. 5) THEN
C         list information about search
          CALL PRWFLS (MESSFL,SCLU,TYPE,SACNT,SANAM,SATYP,SABEG,
     I                 SAVAL,SALEN,SAIMIN,SAIMAX,SARMIN,SARMAX,
     I                 SACOND,SAOR,DSNMIN(1),DSNMAX(1),TYPSTR,
     O                 RETCOD)
C
          IF (RETCOD .EQ. 0) THEN
C           do search
            CALL PRWFDS (WDMSFL,TYPE,SACNT,SAIND,SATYP,SABEG,SAVAL,
     I                   SALEN,SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                   MESSFL,SCLU,MAXDSN,DSNMIN(1),DSNMAX(1),
     M                   DSN,DSNCNT,
     O                   DSBFUL)
          END IF
        END IF
      IF (STYPE .NE. 6) GO TO 12
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWFLS
     I                    (MESSFL,SCLU,TYPE,SACNT,SANAM,SATYP,SABEG,
     I                     SAVAL,SALEN,SAIMIN,SAIMAX,SARMIN,SARMAX,
     I                     SACOND,SAOR,DSNMIN,DSNMAX,TYPSTR,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     list information about dataset search
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,TYPE,SACNT,DSNMIN,DSNMAX,RETCOD
      INTEGER     SATYP(*),SABEG(*),SACOND(*),
     $            SALEN(*),SAIMIN(*),SAIMAX(*),SAOR(*)
      INTEGER*4   SAVAL(30)
      REAL        SARMIN(*),SARMAX(*)
      CHARACTER*1 TYPSTR(12),SANAM(6,*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing messages for this routine
C     TYPE   - type of dataset searching for, 1- timeseries
C     SACNT  - number of search attributes to be considered
C     SANAM  - array of search attribute names
C     SATYP  - array of search attribute type indicators
C              1 - integer number
C              2 - real number
C              3 - alphanumeric character
C     SABEG  - array of beginning positions in value arrays
C     SAVAL  - array of character values
C     SALEN  - array of search attribute lengths
C     SAIMIN - array of minimum acceptable values, integer attributes
C     SAIMAX - array of maximum acceptable values, integer attributes
C     SARMIN - array of minimum acceptable values, real attributes
C     SARMAX - array of maximum acceptable values, real attributes
C     SACOND - array of condition values
C              1 - true
C              2 - false
C     SAOR   - array of or condition values
C     DSNMIN - minimum dataset number to consider
C     DSNMAX - maximum dataset number to consider
C     TYPSTR - name of dataset type searching
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,LEN,SAORIN,I,J,K,K1,K2,K3,KAND,
     $            I0,I1,I2,DSNS(2)
      CHARACTER*1 OBUFF(80),BLNK(1)
      CHARACTER*4 CDUM
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETTXT, CHRCHR, ZIPC, ZBLDWR, ZSTCMA, ZGTRET
      EXTERNAL   CHRINS, INTCHR, DECCHR, PMXTXI, PMXCNW
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      LEN= 40
      CALL ZIPC (LEN,BLNK,OBUFF)
C
      I0= 0
      I1= 1
      I2= 2
C     make prev available
      J= 4
      CALL ZSTCMA(J,I1)
C
      IF (TYPE .EQ. -2) THEN
C       searching through buffer
        SGRP= 51
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,-I1,I)
      ELSE IF (TYPE .EQ. -1) THEN
C       searching by dsn
        DSNS(1)= DSNMIN
        DSNS(2)= DSNMAX
        SGRP= 52
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I2,DSNS)
      ELSE
C       searching by type
        SGRP= 53
        LEN = 80
        CALL GETTXT (MESSFL,SCLU,SGRP,LEN,OBUFF)
        LEN = LEN+ 2
        I   = 12
        CALL CHRCHR (I,TYPSTR,OBUFF(LEN))
        LEN = LEN+ I- 1
        CALL ZBLDWR (LEN,OBUFF,I1,-I1,I)
      END IF
C
      CALL ZBLDWR (I1,BLNK,I0,-I1,I)
C
      IF (SACNT .EQ. 0) THEN
C       all datasets
        SGRP= 54
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,I0,I)
      ELSE
C       search criteria
        SGRP= 55
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,-I1,I)
        SAORIN= 1
        DO 100 I= 1,SACNT
C         clear the output buffer
          LEN= 80
          CALL ZIPC(LEN,BLNK,OBUFF)
C         fill the output buffer template
          SGRP= 56
          CALL GETTXT (MESSFL,SCLU,SGRP,LEN,OBUFF)
C         starting position of AND
          KAND= 36
          J   = 6
          CALL CHRCHR(J,SANAM(1,I),OBUFF(17))
          IF (SATYP(I) .EQ. 3) THEN
C           character type
            OBUFF(14)= BLNK(1)
            OBUFF(15)= BLNK(1)
            OBUFF(24)= BLNK(1)
            IF (SALEN(I) .GT. 8) THEN
C             move AND to right
              LEN= 80
              DO 5 J= 9, SALEN(I)
                CALL CHRINS (LEN,KAND,BLNK,OBUFF)
                KAND= KAND+ 1
 5            CONTINUE
            END IF
            K=  SABEG(I)
            K1= 27
            DO 10 J= 1,SALEN(I),4
              WRITE (CDUM,2000) SAVAL(K)
              K = K+ 1
              K2= K1+ 3
              READ  (CDUM,1000) (OBUFF(K3),K3=K1,K2)
              K1= K1+ 4
 10         CONTINUE
          ELSE
C           integer or real type
            LEN      = 8
            IF (SATYP(I).EQ.1) THEN
              J= 0
              CALL INTCHR (SAIMIN(I),LEN,J,K,OBUFF(5))
              J= 1
              CALL INTCHR (SAIMAX(I),LEN,J,K,OBUFF(27))
            ELSE
              J= 0
              CALL DECCHR (SARMIN(I),LEN,J,K,OBUFF(5))
              J= 1
              CALL DECCHR (SARMAX(I),LEN,J,K,OBUFF(27))
            END IF
          END IF
          IF (I .EQ. SAOR(SAORIN) .OR. I .EQ. SACNT) THEN
C           not 'and' condition
            LEN= 4
            CALL ZIPC (LEN,BLNK,OBUFF(KAND))
          END IF
          IF (SACOND(I) .NE. 2) THEN
C           true condition
            LEN= 4
            CALL ZIPC (LEN,BLNK,OBUFF(1))
          END IF
          LEN= 78
          CALL ZBLDWR (LEN,OBUFF,I0,I1,K)
          IF (I .EQ. SAOR(SAORIN) .AND. I .NE. SACNT) THEN
C           'or' condition
            SGRP= 57
            CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,I1,K)
            SAORIN= SAORIN+1
          END IF
 100    CONTINUE
        CALL ZBLDWR (I0,BLNK,I0,I0,K)
      END IF
C     get return code
      CALL ZGTRET (K)
      IF (K .EQ. 2) THEN
C       user wants back
        RETCOD= 1
      ELSE
C       onward
        RETCOD= 0
      END IF
C     turn off avail of prev
      J= 4
      CALL ZSTCMA(J,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWFDS
     I                    (WDMSFL,TYPE,SACNT,SAIND,SATYP,SABEG,SAVAL,
     I                     SALEN,SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,
     I                     SAOR,MESSFL,SCLU,MAXDSN,DSNMIN,DSNMAX,
     M                     DSN,DSNCNT,
     O                     DSBFUL)
C
C     + + + PURPOSE + + +
C     search WDM file for datasets matching search criteria
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TYPE,SACNT,DSBFUL,MESSFL,SCLU,MAXDSN,
     $          DSNCNT,DSNMIN,DSNMAX
      INTEGER   SAIND(*),SATYP(*),SABEG(*),SACOND(*),
     $          SALEN(*),SAIMIN(*),SAIMAX(*),SAOR(*),
     $          DSN(MAXDSN)
      INTEGER*4 SAVAL(30)
      REAL      SARMIN(*),SARMAX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     TYPE   - type of dataset searching for, 1- timeseries
C     SACNT  - number of search attributes to be considered
C     SAIND  - array of search attribute index numbers
C     SATYP  - array of search attribute type indicators
C              1 - integer number
C              2 - real number
C              3 - alphanumeric character
C     SABEG  - array of beginning positions in value arrays
C     SAVAL  - array of character values
C     SALEN  - array of search attribute lengths
C     SAIMIN - array of minimum acceptable values, integer attributes
C     SAIMAX - array of maximum acceptable values, integer attributes
C     SARMIN - array of minimum acceptable values, real attributes
C     SARMAX - array of maximum acceptable values, real attributes
C     SACOND - array of condition values,
C              1 - true
C              2 - false
C     SAOR   - array of or condition values
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - Cluster containing information for this group
C     MAXDSN - maximum number of dataset numbers allowed
C     DSNMIN - minimum dataset number to consider
C     DSNMAX - maxiumu dataset number to consider
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers found
C     DSBFUL - indicator flag for full DSN array
C              0 - no
C              1 - yes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP,RREC,RIND,DSNTMP,NDSN,PFDSN,ODSNCT,
     $          I,J,I0,I1,NOSA,NOMAT,NOADD,NOCHK,RETC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDDSCK, PRWFCK, PMXTXI
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      ODSNCT= DSNCNT
      NOSA  = 0
      NOMAT = 0
      NOCHK = 0
      NOADD = 0
      IF (TYPE .GE. 0) THEN
        I   = 0
 10     CONTINUE
C         loop looking by type
          I= I+ 1
          IF (I .EQ. TYPE .OR. TYPE .EQ. 0) THEN
C           bring file definition record into memory
            RREC  = 1
            RIND  = WDRCGO(WDMSFL,RREC)
C           calculate pointers within file definition record
            PFDSN = PTSNUM+ (I-1)* 2+ 1
            DSNTMP= WIBUFF(PFDSN,RIND)
C
            IF (DSNTMP .GT. 0) THEN
 20           CONTINUE
C               loop to check datasets
                CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
                RIND= WDRCGO(WDMSFL,RREC)
                NDSN= WIBUFF(2,RIND)
                CALL PRWFCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                       SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                       SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                       MESSFL,SCLU,MAXDSN,
     M                       NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                       DSBFUL)
                DSNTMP= NDSN
              IF (DSBFUL .EQ. 0 .AND. DSNTMP .GT. 0) GO TO 20
            END IF
          END IF
        IF (DSBFUL .EQ. 0 .AND. I .LT. 9) GO TO 10
      ELSE IF (TYPE .EQ. -1) THEN
C       loop looking by dsn range
        DSNTMP= DSNMIN
 40     CONTINUE
          CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
          IF (RETC .EQ. 0) THEN
            RIND= WDRCGO(WDMSFL,RREC)
            CALL PRWFCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                   SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                   SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                   MESSFL,SCLU,MAXDSN,
     M                   NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                   DSBFUL)
          END IF
          DSNTMP= DSNTMP+ 1
        IF (DSNTMP .LE. DSNMAX .AND. DSBFUL .EQ. 0) GO TO 40
      ELSE
C       loop looking for buffer subset
        I= 1
 50     CONTINUE
          DSNTMP= DSN(I)
          CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
          RIND  = WDRCGO(WDMSFL,RREC)
          CALL PRWFCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                 SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                 SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                 MESSFL,SCLU,MAXDSN,
     M                 NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                 DSBFUL)
          IF (NOADD .GT. 0) THEN
C           dataset met the search criteria
            I    = I+ 1
            NOADD= 0
          ELSE
C           not a match, dataset in buffer, remove it
            J= I
            IF (J .LT. DSNCNT) THEN
 60           CONTINUE
                DSN(J)= DSN(J+1)
                J= J+ 1
              IF (J .LT. DSNCNT) GO TO 60
            END IF
            DSN(DSNCNT)= 0
            DSNCNT= DSNCNT- 1
          END IF
        IF (I .LE. DSNCNT) GO TO 50
      END IF
C
C     num dsn checked
      SGRP= 39
      CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,NOCHK)
      IF (NOSA .GT. 0) THEN
C       num of dsn without attributes
        SGRP= 29
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,-I1,I1,NOSA)
      END IF
C
      IF (NOMAT .GT. 0) THEN
C       num of dsn no match
        SGRP= 37
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,-I1,I1,NOMAT)
      END IF
C
      IF (NOADD .GT. 0) THEN
C       num dsn not added, already present
        SGRP= 42
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,-I1,I1,NOADD)
      END IF
C
C     num dsn added
      I   = DSNCNT- ODSNCT
      IF (I .GE. 0) THEN
        SGRP= 30
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,I0,I1,I)
      ELSE IF (I .LE. 0) THEN
C       num dsn now in buffer
        SGRP= 45
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,I0,I1,DSNCNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWFCK
     I                    (DSNTMP,TIBUFF,TRBUFF,
     I                     SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                     SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                     MESSFL,SCLU,MAXDSN,
     M                     NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                     DSBFUL)
C
C     + + + PURPOSE + + +
C     checks datasets for match of specified search attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSNTMP,SACNT,NOSA,NOMAT,NOADD,DSBFUL,NOCHK,
     $          MESSFL,SCLU,MAXDSN,DSNCNT
      INTEGER   SAIND(*),SATYP(*),SABEG(*),SACOND(*),
     $          SALEN(*),SAIMIN(*),SAIMAX(*),SAOR(*),
     $          DSN(MAXDSN)
      INTEGER*4 SAVAL(30),TIBUFF(512)
      REAL      TRBUFF(512)
      REAL      SARMIN(*),SARMAX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSNTMP - current dsn being checked
C     TIBUFF - integer version of current label
C     TRBUFF - real version of current label
C     SACNT  - number of search attributes to be considered
C     SAIND  - array of search attribute index numbers
C     SATYP  - array of search attribute type indicators
C              1 - integer number
C              2 - real number
C              3 - alphanumeric character
C     SABEG  - array of beginning positions in value arrays
C     SAVAL  - array of character values
C     SALEN  - array of search attribute lengths
C     SAIMIN - array of minimum acceptable values, integer attributes
C     SAIMAX - array of maximum acceptable values, integer attributes
C     SARMIN - array of minimum acceptable values, real attributes
C     SARMAX - array of maximum acceptable values, real attributes
C     SACOND - array of condition values,
C              1 - true
C              2 - false
C     SAOR   - array of or condition values
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - message file group number
C     MAXDSN - maximum number of dataset numbers
C     NOSA   - num dsn missing attributes
C     NOMAT  - num dsn not matching
C     NOCHK  - num dsn checked
C     NOADD  - num dsn added to buffer
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers in DSN
C     DSBFUL - indicator flag for full DSN array
C              0 - no
C              1 - yes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAMAT,I,SAORIX,SAORFG,DSADD,SAPOS,ITMP,SGRP,J,K
      REAL      RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV, PRNTXT, PMXTXI
C
C     + + + END SPECIFICATIONS + + +
C
      DSBFUL= 0
      SAORIX= 1
      IF (SACNT .NE. 0) THEN
C       check individual attributes
        SAORFG= 1
        I     = 0
 10     CONTINUE
          SAMAT= 1
 20       CONTINUE
            I= I+ 1
            IF (TIBUFF(10).NE.0) THEN
C             okay to find start of search attribute
              SAPOS= WDSASV (SAIND(I),TIBUFF)
            ELSE
              SAPOS= 0
            END IF
            IF (SAPOS .GT. 0) THEN
C             attribute present, check value
              GO TO (30,40,50), SATYP(I)
 30           CONTINUE
C               integer
                ITMP= TIBUFF(SAPOS)
                IF (ITMP.LT.SAIMIN(I).OR.ITMP.GT.SAIMAX(I)) SAMAT= 0
                GO TO 60
 40           CONTINUE
C               real
                RTMP= TRBUFF(SAPOS)
                IF (RTMP.LT.SARMIN(I).OR.RTMP.GT.SARMAX(I)) SAMAT= 0
                GO TO 60
 50           CONTINUE
C               character
                K= SABEG(I)
                J= 0
 55             CONTINUE
                  J= J+ 1
                  IF (TIBUFF(SAPOS+J-1).NE.SAVAL(K)) THEN
C                   not a match
                    SAMAT= 0
                  END IF
                  K= K+ 1
                IF (J .LT. (SALEN(I)/4) .AND. SAMAT .EQ. 1) GO TO 55
 60           CONTINUE
              IF (SACOND(I) .EQ. 2) THEN
C               use not true condition
                IF (SAMAT .EQ. 0) THEN
                  SAMAT= 1
                ELSE
                  SAMAT= 0
                END IF
              END IF
            ELSE
              SAMAT= 0
C             only count missing attribute if on last 'or'
              IF (SAOR(SAORIX) .EQ. SACNT) THEN
C               count it
                NOSA = NOSA+ 1
              END IF
            END IF
          IF (SAMAT .EQ. 1 .AND. I .LT. SAOR(SAORIX)) GO TO 20
C         maybe we will make it on an 'or' condition
          IF (SAOR(SAORIX) .GE. SACNT) THEN
C           no or
            SAORFG= 0
          ELSE
C           or case, point to correct attribute
            I= SAOR(SAORIX)
            SAORIX= SAORIX+ 1
          END IF
        IF (SAMAT .EQ. 0 .AND. SAORFG .NE. 0) GO TO 10
        IF (SAMAT.EQ.0) NOMAT= NOMAT+ 1
      ELSE
C       no search criteria, all match
        SAMAT = 1
      END IF
C
      IF (SAMAT .EQ. 1) THEN
C       add dsn to buffer if not there
        DSADD= 1
        DO 110 I= 1,DSNCNT
          IF (DSN(I).EQ.DSNTMP) THEN
C           already there
            DSADD= 0
          END IF
 110    CONTINUE
C
        IF (DSADD.EQ.1) THEN
          IF (DSNCNT .LT. MAXDSN) THEN
C           add new dataset
            DSNCNT= DSNCNT+ 1
            DSN(DSNCNT)= DSNTMP
          ELSE
C           buffer full
            DSBFUL= 1
            SGRP  = 28
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        ELSE
C         didnt add it
          NOADD= NOADD+ 1
        END IF
      END IF
C
      NOCHK= NOCHK+ 1
      IF (MOD(NOCHK,50) .EQ. 0) THEN
C       num dsn checked
        SGRP= 39
        I   = 1
        J   = 0
        CALL PMXTXI (MESSFL,SCLU,SGRP,I,J,I,I,NOCHK)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCANWD
     I                   ( MESSFL, SCLU, WDMSFL, IHOW, MAXDSN, PTHNAM, 
     M                     DSN, DSNCNT,
     O                     GOBACK )
C
C     + + + PURPOSE + + +
C     This routine lists all data sets in the user's WDM file (8 per 
C     screen w/ a 2-line header) to let the user select data sets for 
C     further processing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL, SCLU, WDMSFL, MAXDSN, IHOW, DSN(MAXDSN), 
     $             DSNCNT
      CHARACTER*8  PTHNAM(1)
      LOGICAL      GOBACK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of AIDE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing information for this routine
C     IHOW   - flag for type of information to get
C              1 - dsn, ds type, tstype, station name/ds description
C              2 - dsn, ds type, tstype, location attributes
C              3 - dsn, ds type, tstype, period of record, station ID
C              4 - station name/ds descrip. for message data sets only
C     PTHNAM - character string of path of options selected to get here
C     MAXDSN - maximum number of data-set numbers allowed
C     DSN    - array of data-set numbers
C     DSNCNT - count of data-set numbers in DSN
C     GOBACK - Boolean flag:  .TRUE. if user wants to go back to
C              Browse selection screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, I0, I1, I3, I8, J, K, L73, M, INO(1), IYES(1), 
     $             INDX, LINE, INBUFF, SGRP, DSNCK, POS, IRET, YORN(1), 
     $             MAXLIN, SCINIT, IWRT, INUM, IVAL(2), TOOMNY, DSNT(8),
     $             COUNT, CHECK, MOREFG, ICNT, WCNT, JUST, OLEN, LINCNT,
     $             INCR, PREVID, INTRID, DSTYPE
      CHARACTER*1  OBUFF(73), BLNK(1), WNAM(8)
      CHARACTER*8  CWNAM(3)
      LOGICAL      PREV, OOPS
C
C     + + + LOCAL DEFINITIONS + + +
C     COUNT  - total number of data sets on WDM file
C     DSNCK  - data set to be checked for existence (1-32000)
C     DSNCNT - counter of DSN buffer for selected sites
C     ICNT   - order number of data sets displayed so far--used to 
C              display sequence number of screen (e.g. "2 of 5")
C     LINE   - counter for stations displayed on each screen
C
C     + + + FUNCTIONS + + +
      INTEGER      WDCKDT
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + EXTERNAL + + +
      EXTERNAL     WDDSNX, WDDSNP, WDCKDT
      EXTERNAL     Q1INIT, Q1EDIT, QSTCTF, QSTCOB, QGTCOB, ZSTCMA
      EXTERNAL     ZWNSOP, PMXTXI
      EXTERNAL     INTCHR, CARVAR, ZIPC, ZIPI
      EXTERNAL     SCNTXT
C
C     + + + DATA INITIALIZATION + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      INO(1) = 1
      IYES(1) = 2
      I0  = 0
      I1  = 1
      I3  = 3
      I8  = 8
      L73 = 73
      GOBACK = .FALSE.
      PREVID = 4
      INTRID = 16
      JUST  = 1
      TOOMNY = 0
      DSNCK = 0
      LINE = 1
      INCR = 1
C
C     find total number of data sets on WDM file
      COUNT = 0
      CHECK = 0
 10   CONTINUE
        CHECK = CHECK + 1
        CALL WDDSNX (WDMSFL, CHECK)
        IF (CHECK.GT.0) THEN
C         found data set; if IHOW.EQ.4, check data-set type
          IF (IHOW .EQ. 4) DSTYPE = WDCKDT (WDMSFL, CHECK)
C
C         count ds only if IHOW.NE.4 or IHOW.EQ.4 and dstype is msg.
          IF (IHOW.NE.4 .OR. (IHOW.EQ.4 .AND. DSTYPE.EQ.9))
     $      COUNT = COUNT + 1
          MOREFG= 1
        ELSE
          MOREFG= 0
        END IF
      IF (MOREFG.EQ.1 .AND. CHECK.LT.32000) GOTO 10
C
C     enable Prev and Intrpt 
      CALL ZSTCMA (PREVID, I1)
      CALL ZSTCMA (INTRID, I1)
C
      ICNT = 0
C     begin loop to find and display all sites
 20   CONTINUE
 25     CONTINUE
C         return here if oops or prev
          PREV = .FALSE.
          OOPS = .FALSE.
C
          LINCNT = 0
          CALL ZIPI (I8, I0, DSNT)
 30       CONTINUE
C           fill DSNT with up to 8 of the next/previous data-set numbers
            DSNCK = DSNCK + INCR
            CALL WDDSNP (WDMSFL, INCR,
     M                   DSNCK)
            IF (DSNCK .GT. 0)  THEN
C             found data set; if IHOW.EQ.4, check data-set type
              IF (IHOW .EQ. 4) DSTYPE = WDCKDT (WDMSFL, DSNCK)
C
              IF (IHOW.NE.4 .OR. (IHOW.EQ.4 .AND. DSTYPE.EQ.9)) THEN
C               display ds if IHOW.NE.4 or IHOW.EQ.4 and dstype is msg.
                DSNT(LINE) = DSNCK
                LINE = LINE + INCR
                LINCNT = LINCNT + 1
                IF (INCR .EQ. 1) ICNT = ICNT + 1
              END IF
            END IF
          IF (DSNCK.GT.0 .AND. LINCNT.LT.8) GOTO 30
C
          IF (LINCNT .GT. 0) THEN
C           there are data sets to display
C           set window name
            WCNT = ICNT/8
            IF (MOD(ICNT,8) .NE. 0) WCNT = WCNT + 1
            CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
            CALL CARVAR (I8,WNAM,I8,CWNAM(1))
            WCNT = COUNT/8
            IF (MOD(COUNT,8) .NE. 0) WCNT = WCNT + 1
            CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
            CALL CARVAR (I8,WNAM,I8,CWNAM(2))
            CWNAM(3) = PTHNAM(1)
            CALL ZWNSOP (I3,CWNAM)
            SGRP = 11 + IHOW
            CALL Q1INIT (MESSFL, SCLU, SGRP)
C
C           loop through DSNT and fill in screen info. for each data set
            DO 40 I = 1, LINCNT
C             is data set in buffer?
              INBUFF = 0
              DO 35 J = 1, DSNCNT
                IF (DSNT(I) .EQ. DSN(J)) INBUFF = 1
 35           CONTINUE
              IF (INBUFF .EQ. 0) THEN
C               set select to no
                CALL QSTCOB (I1, I, INO)
              ELSE 
C               set select to yes              
                CALL QSTCOB (I1, I, IYES)
              END IF
C             fill char string with descriptive info
              CALL SCNTXT (MESSFL, WDMSFL, DSNT(I), IHOW, OBUFF)
              POS = I + 8
              CALL QSTCTF (POS, L73, OBUFF)
 40         CONTINUE
C
C           blank out any unused rows on screen
            CALL ZIPC (L73, BLNK, OBUFF)
            DO 50 M = LINCNT+1, 8
              CALL QSTCOB (I1, M, INO)
              POS = M + 8
              CALL QSTCTF (POS, L73, OBUFF)            
 50         CONTINUE
C
C           let user edit screen
            CALL Q1EDIT (IRET)
C 
            IF (IRET .EQ. 1) THEN
C             user chose 'Accept'
              DSNCK = DSNT(8)
              LINE = 1
              INCR = 1
            ELSE IF (IRET .EQ. 2) THEN
C             user chose 'Prev'
              DSNCK = DSNT(1)
              LINE = 8
              INCR = -1
              ICNT = ICNT - LINCNT
              IF (ICNT .EQ. 0) THEN
C               go back to Browse selection screen
                GOBACK = .TRUE.
              ELSE
C               not on first screen so can do previous data screen
                PREV = .TRUE.
              END IF
            ELSE IF (IRET .EQ. -1) THEN
C             user chose 'Oops'
              OOPS = .TRUE.
              DSNCK = DSNT(1) - 1
              LINE = 1
              INCR = 1
              ICNT = ICNT - LINCNT
            END IF
C         end if (lincnt .gt. 0)
          END IF
        IF (PREV .OR. OOPS) GOTO 25
C
        IF (IRET.EQ.1 .AND. LINCNT.GT.0) THEN
C         process only if user wants to continue and data sets were displayed
          DO 70 I = 1, LINCNT
            CALL QGTCOB (I1,I,YORN)
            INDX = 0
            DO 55 J = 1, DSNCNT
              IF (DSNT(I) .EQ. DSN(J)) INDX = J
 55         CONTINUE
            IF (YORN(1) .EQ. IYES(1)) THEN
              IF (DSNCNT .LT. MAXDSN) THEN
C               add site to list if not already included
                IF (INDX .EQ. 0) THEN
                  DSNCNT = DSNCNT + 1
                  DSN(DSNCNT) = DSNT(I)
                END IF
              ELSE IF (INDX .EQ. 0) THEN
C               dsn not already in buffer and max. no. already selected
                TOOMNY = TOOMNY + 1
              END IF
            ELSE
C             data set not to be added, delete if currently in buffer
              IF (INDX .NE. 0) THEN
                DO 60 K = INDX, DSNCNT-1
                  DSN(K) = DSN(K+1)
 60             CONTINUE
                DSN(DSNCNT) = 0
                DSNCNT = DSNCNT - 1
              END IF
            END IF
 70       CONTINUE
C
          IF (TOOMNY .GT. 0) THEN
C           let user know that data sets weren't added
            MAXLIN = 10
            SCINIT = 1
            IWRT = 0
            IF (TOOMNY .EQ. 1) THEN
C             just 1 site not added
              INUM = 1
              IVAL(1) = MAXDSN
              SGRP = 20
            ELSE
C             multiple sites not added
              INUM = 2
              IVAL(1) = TOOMNY
              IVAL(2) = MAXDSN
              SGRP = 21
            END IF
C           disable Prev and Intrpt
            CALL ZSTCMA (PREVID, I0)
            CALL ZSTCMA (INTRID, I0)
C           set window pathname
            CWNAM(1) = PTHNAM(1)
            CALL ZWNSOP (I1, CWNAM)
            CALL PMXTXI (MESSFL, SCLU, SGRP, MAXLIN, SCINIT, IWRT, INUM,
     I                   IVAL)
            TOOMNY = 0
C           enable Prev and Intrpt
            CALL ZSTCMA (PREVID, I1)
            CALL ZSTCMA (INTRID, I1)
          END IF
C       end if (iret.eq.1 .and. lincnt.gt.0)
        END IF
C     loop back if user wants to continue, and there might be more data sets
      IF (IRET.EQ.1 .AND. DSNCK.GT.0) GOTO 20
C
C     disable Prev and Intrpt
      CALL ZSTCMA (PREVID, I0)
      CALL ZSTCMA (INTRID, I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCANBF
     I                   ( MESSFL,SCLU,WDMSFL,IHOW,MAXDSN,PTHNAM,
     M                     DSN,DSNCNT,
     O                     GOBACK)
C
C     + + + PURPOSE + + +
C     This routine lists the data sets that have been selected
C     and added to the buffer and lets the user drop 1 or more
C     from the list.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,WDMSFL,MAXDSN, IHOW
      INTEGER      DSN(MAXDSN),DSNCNT
      CHARACTER*8  PTHNAM(1)
      LOGICAL      GOBACK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - Cluster containing information for this routine
C     IHOW   - flag for type of information to get
C              1 - dsn, ds type, tstype, station name/ds description
C              2 - dsn, ds type, tstype, location attributes
C              3 - dsn, ds type, tstype, period of record, station ID
C              4 - station name/ds descrip. for message data sets only
C     PTHNAM - character string of path of options selected to get here
C     MAXDSN - maximum number of dataset numbers allowed
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers in DSN
C     GOBACK - Boolean flag:  .TRUE. if user wants to go back to
C              Browse selection screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, I8, I3, J, K, L1, L73, SGRP, LINE, INO(1), 
     $             IYES(1), ICNT, POS, IRET, QFLG, YORN(1), ADSN, WCNT, 
     $             JUST, OLEN
      CHARACTER*1  OBUFF(73), WNAM(8)
      CHARACTER*8  CWNAM(3)
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     Q1INIT,QSTCOB,QSTCTF,Q1EDIT,QGTCOB
      EXTERNAL     SCNTXT,ZSTCMA,INTCHR,CARVAR,ZWNSOP
C
C     + + + END SPECIFICATIONS + + +
C
      INO(1)= 1
      IYES(1)= 2
      L1  = 1
      L73 = 73
      I3  = 3
      I8  = 8
      GOBACK = .FALSE.
C
C     begin loop to display stations selected
      QFLG = 0
      ICNT = 0
C     turn on Prev and Intrpt 
      CALL ZSTCMA (4,1)
      CALL ZSTCMA (16,1)
      JUST = 1
 520  CONTINUE
C       this procedure uses negative DSN numbers as flag to drop
C       stations from the list
C       set window name
        WCNT = (ICNT)/8 + 1
        CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
        CALL CARVAR (I8,WNAM,I8,CWNAM(1))
        WCNT = (DSNCNT-1)/8 + 1
        CALL INTCHR (WCNT,I8,JUST,OLEN,WNAM)
        CALL CARVAR (I8,WNAM,I8,CWNAM(2))
        CWNAM(3) = PTHNAM(1)
        CALL ZWNSOP (I3,CWNAM)
        SGRP = 15 + IHOW 
        CALL Q1INIT (MESSFL, SCLU, SGRP)
        LINE = 0
C       fill in site names
 530    CONTINUE
          ICNT = ICNT + 1
          LINE = LINE + 1
          IF (DSN(ICNT) .GT. 0) THEN
            CALL QSTCOB (L1,LINE, IYES)
          ELSE
            CALL QSTCOB (L1,LINE, INO)
          END IF
C         get and set station name
          IF (DSN(ICNT) .NE. 0) THEN
            ADSN = ABS(DSN(ICNT))
            CALL SCNTXT (MESSFL,WDMSFL,ADSN,IHOW,OBUFF)
            POS = LINE + 8
            CALL QSTCTF (POS,L73,OBUFF)         
          END IF
        IF (LINE .LT. 8 .AND. ICNT .LT. DSNCNT) GO TO 530
C       let user edit screen
        CALL Q1EDIT (IRET)
C
        IF (IRET .EQ. 1) THEN
C         set y/n for keep
          J = ICNT - LINE
          DO 540 K = 1, LINE
            J = J + 1
            CALL QGTCOB (L1,K,YORN)
            IF ((YORN(1).EQ.INO(1) .AND. DSN(J).GT.0) .OR.
     $          (YORN(1).EQ.IYES(1) .AND. DSN(J).LT.0)) THEN
C             reverse sign of dsn value: if >0 and user spec'd. NO, make
C             dsn negative; if <0 and user spec'd. YES, make positive
              DSN(J) = -(DSN(J))
            END IF
 540      CONTINUE
C
C         if no more sites, set QFLG to exit loop
          IF (ICNT .GE. DSNCNT) QFLG = 1
C
        ELSE IF (IRET .EQ. -1) THEN
C         oops, reset line pointer
          ICNT = ICNT - LINE
        ELSE IF (IRET .EQ. 2) THEN      
C         previous
          ICNT = ICNT - 8 - LINE
          IF (ICNT .LT. 0) THEN
            QFLG = 1
            GOBACK = .TRUE.
          ELSE
            QFLG = 0
          END IF
        ELSE IF (IRET .EQ. 7) THEN
C         interupt
          QFLG = 1
        END IF
      IF (QFLG .EQ. 0) GO TO 520
C
C     drop sites from buffer
      J = 0
      DO 550 I = 1,DSNCNT
        IF (DSN(I) .GT. 0) THEN
          J = J + 1
          DSN(J) = DSN(I)
        END IF
 550  CONTINUE
C     zero any left over datasets in buffer
      IF (DSNCNT .GT. J) THEN
        DO 560 I = J+1,DSNCNT
          DSN(I) = 0
 560    CONTINUE
      END IF
      DSNCNT = J
C
C     disable Prev and Intrpt
      CALL ZSTCMA (4,0)
      CALL ZSTCMA (16,0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCNTXT
     I                   (MESSFL,WDMSFL,DSN,IHOW,
     O                    OBUFF)
C
C     + + + PURPOSE + + + 
C     This routine retrieves attribute information from the specified
C     WDM file and data set.  Attributes retrieved are based on the
C     argument IHOW.  The retrieved information is placed in the
C     character string OBUFF.  If the attribute can't be retrieved
C     the characters "na" are used for not available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, IHOW, MESSFL
      CHARACTER*1  OBUFF(73)
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number for message file with awfeb clusters 
C     WDMSFL - Fortran unit number for users WDM file
C     DSN    - data det number to use to get attribute values
C     IHOW   - flag for type of information to get
C              1 - data set(station) description, station name
C              2 - data set(station) location attributes
C              3 - data set(station) period of record
C     OBUFF  - character string of the data set information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L2, L5, L8, L20, L73, SDATE(6), EDATE(6), IDUM1, 
     $            RETCOD, DSTYPE, SPACE, JUST, OLEN, ID, GPFLG, LEN,
     $            ERRCOD, POS, BLKPOS
      CHARACTER*1 BLNK, CTYPE(2,9)
C
C     + + + FUNCTIONS + + +
      INTEGER  WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCNFIT, INTCHR, CHRCHR, ZIPC, WTFNDT, WDCKDT, DATLST
C
C     + + + DATA INITIALIZATIONS + + + 
      DATA  BLNK/' '/, L2,L5,L8,L20,L73/2,5,8,20,73/ 
      DATA  CTYPE/'T','S','T','B','S','C','P','R','V','E','R','A',
     $            'S','T','A','T','M','S'/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZIPC (L73, BLNK, OBUFF)
      POS = 1
C
      IF (IHOW .NE. 4) THEN
C       fill with data set number
        JUST = 0
        CALL INTCHR (DSN,L5,JUST,OLEN,OBUFF(1))
C
C       fill with data set type
        DSTYPE = WDCKDT (WDMSFL, DSN)
        IF (DSTYPE .GT. 0 .AND. DSTYPE .LE. 9) THEN
          CALL CHRCHR (L2,CTYPE(1,DSTYPE),OBUFF(8))
        ELSE
          OBUFF(8) = 'n'
          OBUFF(9) = 'a'
        END IF
C
C       add time series type
        ID = 1 
        SPACE = 4
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(14),RETCOD)
C
        POS = 21
      END IF
C
      IF (IHOW.EQ.1 .OR. IHOW.EQ.4) THEN
C       station name
        ID = 45
        SPACE = 48
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        IF (RETCOD .NE. 0) THEN
C         try station description
          ID = 10
          SPACE = 50
          CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        END IF
C
      ELSE IF (IHOW .EQ. 2) THEN
C       location descriptors
C       latitude (real)
        ID = 8 
        SPACE = 9 
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        IF (RETCOD .NE. 0) THEN
C         try latitude (integer)  
          ID = 54
          CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        END IF
        POS = POS + SPACE + 1
C
C       longitude (real)
        ID = 9 
        SPACE = 9 
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        IF (RETCOD .NE. 0) THEN
C         try longitude (integer)
          ID = 55
          CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        END IF
        POS = POS + SPACE + 1
C
C       elevation
        ID = 7 
        SPACE = 9 
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        POS = POS + SPACE + 3
C
C       state code
        ID = 41
        SPACE = 3 
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        POS = POS + SPACE + 2
C
C       station id (integer)
        ID = 51
        SPACE = 8 
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        IF (RETCOD .NE. 0) THEN
C         try station id (character)
          ID = 2
          SPACE = 16
          CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        END IF
C
      ELSE IF (IHOW .EQ. 3) THEN
C       descriptors are period of record
C       get start and end date
        GPFLG = 0
        CALL WTFNDT (WDMSFL,DSN,GPFLG,IDUM1,SDATE,EDATE,RETCOD)
        IF (RETCOD .EQ. 0) THEN
          CALL DATLST (SDATE,OBUFF(POS),LEN,ERRCOD)
          IF (LEN .GT. 11) THEN
C           blank out hours:minutes:seconds 
            BLKPOS = POS + (LEN-8)
            CALL ZIPC (L8,BLNK,OBUFF(BLKPOS))
          END IF
C         put dash between dates
          POS = POS + 12
          OBUFF(POS) = '-'
          POS = POS + 2
          CALL DATLST (EDATE,OBUFF(POS),LEN,ERRCOD)
          IF (LEN .GT. 11) THEN
C           blank out hours:minutes:seconds 
            BLKPOS = POS + (LEN-8)
            CALL ZIPC (L8,BLNK,OBUFF(BLKPOS))
          END IF
          POS = POS + 13
        ELSE
          OBUFF(26) = 'n'
          OBUFF(27) = 'a'
          OBUFF(33) = '-'
          OBUFF(39) = 'n'
          OBUFF(40) = 'a'
          POS = POS + 27
        END IF
C
C       station id (integer)
        ID = 51
        SPACE = 8
        CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        IF (RETCOD .NE. 0) THEN
C         try station id (character)
          ID = 2
          SPACE = 16
          CALL SCNFIT (MESSFL,WDMSFL,DSN,ID,SPACE,OBUFF(POS),RETCOD)
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCNFIT 
     I                   (MESSFL,WDMSFL,DSN,ID,SPACE,
     O                    BUFF,RETCOD)
C
C     + + + PURPOSE + + + 
C     This routine retrieves the specified attribute for the
C     specified data set and wdm file and puts the retrieved
C     values in a character string.  If the value can't be 
C     retrieved, "na" is placed in the string.
C
C     + + + DUMMY ARGUMENTS + + + 
      INTEGER      WDMSFL, DSN, ID, SPACE, RETCOD, MESSFL
      CHARACTER*1  BUFF(SPACE)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of meesage file with awfeb cluster
C     WDMSFL - Fortran unit number for users WDM file
C     DSN    - data det number to use to get attribute values
C     ID     - attribute id number
C     SPACE  - character space available to place the value
C     BUFF   - character string of the data-set information
C     RETCOD - 0 - successfully filled string,
C              not 0 - used "na", could not retrieve values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SLEN,  IDUM1, IDUM2, IDUM3, TYPE, LEN, INUM, 
     $             JUST, OLEN, MIDDLE
      REAL         RNUM
      CHARACTER*1  CDUM(6), BTMP(80), BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDSAGY, WDBSGR, WDBSGC
      EXTERNAL   INTCHR, DECCHR, CHRCHR, ZIPC
C
C     + + + END SPECIFICATIONS + + + 
C
      BLNK = ' '
      JUST = 0
      CALL ZIPC (SPACE, BLNK, BUFF)
C
C     find type and length for given attribute id
      CALL WDSAGY (MESSFL,ID, CDUM,IDUM1,TYPE,LEN,IDUM2,IDUM3)
C
      IF (TYPE .EQ. 1) THEN
C       integer
        CALL WDBSGI (WDMSFL,DSN,ID,LEN,INUM,RETCOD)
        IF (RETCOD .EQ. 0) THEN
          CALL INTCHR (INUM,SPACE,JUST,OLEN,BUFF)
        ELSE
          BUFF(SPACE-1) = 'n'
          BUFF(SPACE)   = 'a'
        END IF
C
      ELSE IF (TYPE .EQ. 2) THEN
C       real
        CALL WDBSGR (WDMSFL,DSN,ID,LEN,RNUM,RETCOD)
        IF (RETCOD .EQ. 0) THEN
          CALL DECCHR (RNUM,SPACE,JUST,OLEN,BUFF)
        ELSE
          BUFF(SPACE-1) = 'n'
          BUFF(SPACE)   = 'a'
        END IF
C
      ELSE IF (TYPE .EQ. 3) THEN
C       character
        CALL WDBSGC (WDMSFL,DSN,ID,LEN,BTMP,RETCOD)
        IF (RETCOD .EQ. 0) THEN
          SLEN = SPACE
          IF (LEN .LT. SPACE) THEN
            SLEN = LEN
          ELSE
            SLEN = SPACE
          END IF
          CALL CHRCHR (SLEN,BTMP,BUFF)               
        ELSE
          MIDDLE = SPACE/2
          BUFF(MIDDLE) = 'n'
          BUFF(MIDDLE+1) = 'a'
        END IF
C
      ELSE
C       didn't recognize type
        RETCOD = -1
        BUFF(SPACE-1) = 'n'
        BUFF(SPACE)   = 'a'
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WAVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     waide library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
