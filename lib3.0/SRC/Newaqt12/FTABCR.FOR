C
C
C
      SUBROUTINE   CMFTAB
     I                   (MESSFL)
C
C     + + + PURPOSE + + +
C     This utility creates FTABLEs or modifies the
C     FTABLEs of an HSPF uci file.
C
C     To create an FTABLE you need two files, one with a set of rating
C     curves and another with specifications for each FTABLE to be
C     created.
C
C     To modify an FTABLE you may enter a multiplier for each
C     of the first four columns.  The input file is read, FTABLE
C     columns modified, and an output file is written.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP,RESP
C
C     + + + EXTERNALS + + +
      EXTERNAL   MODTAB, CRETAB, QRESP
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU  = 57
C
 10   CONTINUE
C
        SGRP= 101
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300), RESP
C
 100    CONTINUE
C         modify tables
          CALL MODTAB (MESSFL,SCLU)
          GO TO 999
C
 200    CONTINUE
C         create new ftables
          CALL CRETAB (MESSFL,SCLU)
          GO TO 999
C
 300    CONTINUE
C         return one level up
          GO TO 999
C
 999    CONTINUE
C
      IF (RESP.NE.3) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   MODTAB
     I                   (MESSFL,SCLU)
C
C     + + + PURPOSE  + + +
C     For each FTABLE to be modified you must enter a multiplier
C     for each of the columns. Input must be FTABLE format used
C     for an HSPF UCI input file. The input file is read, FTABLE
C     columns modified, and an output file is written. All non-
C     FTABLE lines and comment lines in the UCI file will be
C     copied to the output file. This program assumes that there
C     is one comment line in the FTABLE block before the record
C     with the number of rows and number of columns and two
C     comment line before the actual records for the FTABLE.
C     This program will ask you for the name of the input and
C     output files and the multipliers to be used.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FNUM,DONE,I,NUMF,NCOL,IFILE(2),I2,I1,I4,I8,
     1             IVAL(1),SGRP,I0,IRET,FOUND
      REAL         V,VM, A,AM, Q(5),QM(5), E,EM, RVAL(8)
      CHARACTER*80 CREC
C
C     + + + EXTERNALS + + +
      EXTERNAL     PRNTXT,Q1INIT,Q1EDIT,ZSTCMA,QGETF,QSETI,QSETR,QGETR
      EXTERNAL     ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I2 = 2
      I4 = 4
      I8 = 8
      EM    = 1.0
      AM    = 1.0
      VM    = 1.0
      QM(1) = 1.0
      QM(2) = 1.0
      QM(3) = 1.0
      QM(4) = 1.0
      QM(5) = 1.0
C
C     allow 'Prev' command
      CALL ZSTCMA (I4,I1)
C     give preliminary message
      SGRP = 102
      CALL PRNTXT (MESSFL,SCLU,SGRP)
      CALL ZGTRET (IRET)
C     turn off 'Prev' command
      CALL ZSTCMA (I4,I0)
      IF (IRET.EQ.1) THEN
C       user wants to continue
C       allow 'Prev' command
        CALL ZSTCMA (I4,I1)
C       enter file names
        SGRP = 103
        CALL Q1INIT (MESSFL,SCLU,SGRP)
        CALL Q1EDIT (IRET)
C       turn off 'Prev' command
        CALL ZSTCMA (I4,I0)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL QGETF (I2,IFILE)
C
          FOUND = 0
   10     CONTINUE
            READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
            IF (CREC(3:8) .EQ. 'FTABLE') THEN
C             start of ftable
              FOUND = 1
              READ(CREC,FMT='(12X,I3)',END=99,ERR=98) FNUM
C
C             Program assumes that there is one comment line
C             in the FTABLE block before the record with the
C             number of rows and number of columns and two
C             comment line before the actual records for the FTABLE.
              WRITE(IFILE(2),FMT='(A80)') CREC
              READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
              WRITE(IFILE(2),FMT='(A80)') CREC
              READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
              WRITE(IFILE(2),FMT='(A80)') CREC
              READ (CREC,FMT='(5X,I5)',ERR=98) NCOL
              READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
              WRITE(IFILE(2),FMT='(A80)') CREC
              READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
              WRITE(IFILE(2),FMT='(A80)') CREC
              READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
C
C             do screen to enter multipliers
              SGRP = 104
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              IVAL(1) = FNUM
              CALL QSETI (I1,IVAL)
              RVAL(1) = EM
              RVAL(2) = AM
              RVAL(3) = VM
              RVAL(4) = QM(1)
              RVAL(5) = QM(2)
              RVAL(6) = QM(3)
              RVAL(7) = QM(4)
              RVAL(8) = QM(5)
              CALL QSETR (I8,RVAL)
              CALL Q1EDIT (IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue
                CALL QGETR (I8,RVAL)
                EM    = RVAL(1)
                AM    = RVAL(2)
                VM    = RVAL(3)
                QM(1) = RVAL(4)
                QM(2) = RVAL(5)
                QM(3) = RVAL(6)
                QM(4) = RVAL(7)
                QM(5) = RVAL(8)
              END IF
C
 20           CONTINUE
                IF (CREC(3:5) .NE. 'END') THEN
C                 not end, so process
C                 WRITE(*,FMT='(A)') CREC
C                 determine number of discharge fields on ftable
                  NUMF = NCOL - 3
                  IF (NUMF .LT. 1) NUMF = 1
C
                  READ(CREC,FMT='(8F10.1)',END=98,ERR=98) E, A, V,
     $                (Q(I),I=1,NUMF)
                  E = E*EM
                  A = A*AM
                  V = V*VM
                  DO 24 I = 1,NUMF
                    Q(I) = Q(I)*QM(I)
 24               CONTINUE
                  WRITE(IFILE(2),FMT='(F10.3,F10.3,F10.2,5F10.2)')
     $                E,A,V,(Q(I),I=1,NUMF)
                  DONE = 0
                  READ(IFILE(1),FMT='(A80)',END=99,ERR=98) CREC
                ELSE
                  DONE = 1
                  WRITE(IFILE(2),FMT='(A80)') CREC
                END IF
              IF (DONE .EQ. 0) GO TO 20
            ELSE
              WRITE(IFILE(2),FMT='(A80)') CREC
            END IF
          GO TO 10
C
 98       CONTINUE
C           problem reading input file
            SGRP = 105
            CALL PRNTXT (MESSFL,SCLU,SGRP)
 99       CONTINUE
          IF (FOUND.EQ.0) THEN
C           no ftables found in this file
            SGRP = 113
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          CLOSE (UNIT=IFILE(1))
          CLOSE (UNIT=IFILE(2))
C
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CRETAB
     I                   (MESSFL,SCLU)
C
C     + + + PURPOSE + + +
C     To create an FTABLE you need two files, one with a set of
C     rating curves and another with specifications for each FTABLE
C     to be created.  Comment lines can be included anywhere
C     in the file by placing a 'C' or '#' in the first column.
C
C     The rating curve file must have a record with an 'R' in
C     column 1(first byte) followed by the drainage area in square
C     miles in the next 9 columns. Following the 'R' record are
C     records for the rating table with stage(elevation)(ft), top
C     width of flow(ft), cross-sectional area of flow(sqft), and
C     discharge(cuft/sec). All records must have increasing value
C     for the numbers entered. The program can accept up to 100
C     rating curves. Ratings can have up to 25 rows, but remember
C     the number of rows times number of columns in an FTABLE
C     cannot exceed 100.
C
C     The reach specifications file must contain a record for each
C     reach that needs to be computed (each FTABLE to be created).
C     Each record must contain the following fields: FTABLE
C     number(integer), length of the reach(mi), and total
C     contributing drainage area(sqmi). The program can accept
C     up to 200 reaches.
C
C     The program matches each reach in the reach specifications file
C     with a rating curve that has the closest drainage area. The
C     stage and discharge are used directly and the top width and
C     and cross-sectional area values in the rating are multiplied
C     by the reach length and converted to acres and acre-feet,
C     respectively.
C
C     Both files use a free field format so values must be separated
C     by at least one blank (use space bar).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C
C     + + + PARAMETERS + + +
      INTEGER    MAXRCH
      PARAMETER (MAXRCH=200)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  FOUT, FIN2, FIN1, TABNO(MAXRCH),
     $         I1, I0, I4, NUMRCH, N, IVAL(1),
     $         SGRP, IRET, I3, IFILE(3)
      REAL     LENGTH(MAXRCH), CUMDA(MAXRCH)
      CHARACTER*80 LINE
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRNTXT, ZGTRET, ZSTCMA, Q1INIT, Q1EDIT, QGETF
      EXTERNAL   COMPFT, PMXTXI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I3 = 3
      I4 = 4
C
C     allow 'Prev' command
      CALL ZSTCMA (I4,I1)
C     give preliminary message
      SGRP = 106
      CALL PRNTXT (MESSFL,SCLU,SGRP)
      CALL ZGTRET (IRET)
C     turn off 'Prev' command
      CALL ZSTCMA (I4,I0)
      IF (IRET.EQ.1) THEN
C       user wants to continue
C       allow 'Prev' command
        CALL ZSTCMA (I4,I1)
C       enter file names
        SGRP = 107
        CALL Q1INIT (MESSFL,SCLU,SGRP)
        CALL Q1EDIT (IRET)
C       turn off 'Prev' command
        CALL ZSTCMA (I4,I0)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL QGETF (I3,IFILE)
C         get name of files and open the files
          FIN1 = IFILE(1)
          FIN2 = IFILE(2)
          FOUT = IFILE(3)
C
C         read the file with reach numbers and characteristics
          N = 0
 10       CONTINUE
            READ(FIN2,FMT='(A)',END=17) LINE
C           write(*,*) line
            IF (LINE(1:1) .EQ. 'C' .OR. LINE(1:1) .EQ. 'c'
     $          .OR. LINE(1:1). EQ. '#') THEN
C             comment line, ignore
            ELSE
C             process record
              N = N + 1
              READ(LINE,*,ERR=16) TABNO(N),LENGTH(N),CUMDA(N)
            END IF
          IF (N .LT. 100) GO TO 10
          GO TO 17
 16       CONTINUE
C           bad record, use up to that point
            SGRP = 109
            IVAL(1) = N
            CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I0,I1,IVAL)
            N = N - 1
 17       CONTINUE
C         close file with reach specifications
          CLOSE (FIN2)
          NUMRCH = N
C
C         call routine to compute ftables
          CALL COMPFT (MESSFL,SCLU,I1,FIN1,FOUT,
     I                 MAXRCH,NUMRCH,TABNO,LENGTH,CUMDA)
C
C         close output file
          CLOSE (FOUT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMPFT
     I                   (MESSFL,SCLU,INTACT,FIN1,FOUT,
     I                    MAXRCH,NUMRCH,TABNO,LENGTH,CUMDA)
C
C     + + + PURPOSE + + +
C     do computations to produce ftables, with interaction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,INTACT,FIN1,FOUT,
     1             MAXRCH,NUMRCH,TABNO(MAXRCH)
      REAL         LENGTH(MAXRCH), CUMDA(MAXRCH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     INTACT - interactive version flag
C     FIN1   - input file unit number
C     FOUT   - output file unit number, or record number if batch version
C     MAXRCH - max number of reaches
C     NUMRCH - actual number of reaches
C     TABNO  - array of ftable numbers
C     LENGTH - array of reach lengths
C     CUMDA  - array of upstream areas for each reach
C
C     + + + PARAMETERS + + +
      INTEGER MAXRAT, MAXROW
      PARAMETER (MAXRAT=100, MAXROW=25)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  NUMROW(MAXRAT), NUMRAT, IVAL(1), I1, SGRP
      REAL     AREA(MAXRAT), RAT(MAXRAT,MAXROW,4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETRAT, PRNTXT, WRFTAB
      EXTERNAL   PMXTXI
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     read the file with the ratings
      CALL GETRAT (FIN1,MAXRAT,MAXROW,RAT,NUMRAT,AREA,NUMROW)
C     close file with ratings
      CLOSE (FIN1)
      IF (NUMRAT .GE. 0) THEN
C
C       give message with number of curves read
        IF (INTACT.EQ.1) THEN
          SGRP = 108
          IVAL(1) = NUMRAT
          CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I1,I1,IVAL)
        END IF
C
        IF (NUMRCH .GT. 0) THEN
C         give message for number of reaches read
          IF (INTACT.EQ.1) THEN
            SGRP = 110
            IVAL(1) = NUMRCH
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I1,I1,IVAL)
          END IF
C
          CALL WRFTAB (MAXRCH,MAXRAT,MAXROW,CUMDA,AREA,NUMRAT,
     I                 INTACT,FOUT,TABNO,NUMROW,NUMRCH,LENGTH,RAT)
C
        ELSE IF (INTACT.EQ.1) THEN
C         send message that reach information not read successfully
          SGRP = 111
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      ELSE IF (INTACT.EQ.1) THEN
C       send message that no rating curves read successfully
        SGRP = 112
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
