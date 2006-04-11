      SUBROUTINE PSETFN (CINKEY, CSTRNG, NSTRNG)
C
C
C        SUBROUNTINE 'PSETFN' SETS A GIVEN KEY TO A GIVEN
C        FUNCTION.  THE SUBROUNTINE HAS BEEN DESIGNED TO
C        ACCESS THE FUNCTION FILE, MAKE THE NEEDED CHANGES,
C        AND THEN CUT LOOSE FROM THE FILE.  THIS IS DONE TO
C        SAVE TIME.  TO INSURE THAT TWO USERS DON'T TRY TO
C        WRITE TO THE FILE AT THE SAME TIME, THEREBY KILLING
C        THE PROGRAM WITH AN ERROR, THE SUBROUTINE HAS A BUILT-*
C        IN WAITING MECHANISM.  IF A 'WRITE' STATEMENT TO THE
C        FILE IS UNSUCCESSFUL, THE SUBROUTINE SIMPLY WAITS AND
C        TRIES AGAIN.
C
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
C
      CHARACTER*(*) CSTRNG
      CHARACTER*1 CINKEY
      LOGICAL LEXIST
C
C
C   ***
C   ***
C   ** REESTABLISH LINK WITH FILE IFUN
C   ***
      ICOUNT = 0
      IF (IFUN.EQ.-1) RETURN
C     INQUIRE(FILE=CFUNFL,EXIST=LEXIST)                                 H
C     IF (.NOT.LEXIST) THEN                                             H
C        CALL CCREAT(CFUNFL,0,0,0,IERR)                                 H
C        CALL CRETYP(CFUNFL,'00000164,0,IERR)                           H
C        WRITE (IDSP,99) CFUNFL                                         H
C99      FORMAT (' FILE GENERATED: ',A)                                 H
C     ENDIF                                                             H
 100  OPEN(UNIT=IFUN,FILE=CFUNFL,ERR=110,IOSTAT=ISTAT)
 110  IF (ISTAT .NE. 0) THEN
C      IF (ISTAT .EQ. 77) THEN                                          H
C       CALL WAITS (2.0)                                                H
C       WRITE(IDSP,*) 'THE FILE IS IN USE => ',CFUNFL                   H
C       ICOUNT = ICOUNT + 1                                             H
C       IF (ICOUNT .GT. 10) THEN                                        H
C         WRITE(IDSP,120) CFUNFL                                        H
C         GOTO 100                                                      H
C       END IF                                                          H
C      ELSE                                                             H
        WRITE(IDSP,120) CFUNFL
 120    FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
         CLOSE (UNIT=IFUN)
         RETURN
C      END IF                                                           H
      END IF
      IF (NKEY .NE. 0) THEN
       DO 130 I=1,NKEY
        IF (CKEY(I) .EQ. CINKEY) GOTO 600
 130   CONTINUE
      END IF
C
      IF ( NSTRNG .LT. 0 ) GO TO 600
      NKEY = NKEY + 1
      CKEY(NKEY) = CINKEY
      IF ( NSTRNG .EQ. 0 ) THEN
      CFUNCT(NKEY) = ' '
      ELSE
      CFUNCT(NKEY) = CSTRNG(1:NSTRNG)
      ENDIF
      IKEY(NKEY) = NSTRNG
      IF (NKEY .EQ. 1) GOTO 700
      CALL WIND(IFUN)
      ICOUNT = 0
 200  WRITE(IFUN,400,ERR=210,IOSTAT=ISTAT1)CKEY(NKEY),IKEY(NKEY)
 210  IF (ISTAT1 .NE. 0) THEN
        IF (ISTAT1 .EQ. 77) THEN
          CALL WAITS (2.0)
          WRITE(IDSP,*) 'THE FILE IS IN USE => ',CFUNFL
          ICOUNT = ICOUNT + 1
          IF (ICOUNT .GT. 10) THEN
            WRITE(IDSP,220) CFUNFL
            GOTO 200
          END IF
        ELSE
          WRITE(IDSP,220) CFUNFL
 220      FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
          CLOSE (UNIT=IFUN)
          RETURN
        END IF
      END IF
      ICOUNT = 0
 300  WRITE(IFUN,500,ERR=310,IOSTAT=ISTAT2) CFUNCT(NKEY)
 310  IF (ISTAT2 .NE. 0) THEN
        IF (ISTAT2 .EQ. 77) THEN
          CALL WAITS (2.0)
          WRITE(IDSP,*) 'THE FILE IS IN USE => ',CFUNFL
             ICOUNT = ICOUNT + 1
          IF (ICOUNT .GT. 10) THEN
            WRITE(IDSP,320) CFUNFL
            GOTO 300
          END IF
        ELSE
          WRITE(IDSP,320) CFUNFL
 320      FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
          CLOSE (UNIT=IFUN)
          RETURN
        END IF
      END IF
 400  FORMAT(A,1X,I4)
 500  FORMAT(A)
C   ***
C   ***
C   ** SEVERE TIES WITH IFUN
C   ***
      CLOSE (UNIT=IFUN)
      RETURN
C
 600  IF ( NSTRNG .LT. 0 ) GO TO 650
      IF ( NSTRNG .EQ. 0 ) THEN
      CFUNCT(I) = ' '
      ELSE
      CFUNCT(I) = CSTRNG(1:NSTRNG)
      ENDIF
      IKEY(I) = NSTRNG
      GOTO 700
C   ***
C   ***
C   ** REMOVE FUNCTION
C   ***
 650  DO 660 J=I,NKEY - 1
       CKEY(J) = CKEY(J + 1)
       IKEY(J) = IKEY(J + 1)
       CFUNCT(J) = CFUNCT(J + 1)
 660  CONTINUE
      NKEY = NKEY - 1
C   ***
C   ***
C   ** REWIND IFUN AND WRITE OUT ALL FUNCTIONS
C   ***
 700  REWIND IFUN
      C133(1:) = CSPL(4:4)
      ICOUNT = 0
 800  WRITE(IFUN,500,ERR=810,IOSTAT=ISTAT3) C133(1:1)
 810  IF (ISTAT3 .NE. 0) THEN
        IF (ISTAT3 .EQ. 77) THEN
          CALL WAITS (2.0)
          WRITE(IDSP,*)'THE FILE IS IN USE => ',CFUNFL
          ICOUNT = ICOUNT + 1
          IF (ICOUNT .GT. 10) THEN
            WRITE(IDSP,820) CFUNFL
            GOTO 800
          END IF
        ELSE
          WRITE(IDSP,820) CFUNFL
 820      FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
          CLOSE (UNIT=IFUN)
          RETURN
        END IF
      END IF
      DO 1100 I=1,NKEY
      ICOUNT = 0
 900  WRITE(IFUN,400,ERR=910,IOSTAT=ISTAT4) CKEY(I),IKEY(I)
 910  IF (ISTAT4 .NE. 0) THEN
       IF (ISTAT4 .EQ. 77) THEN
        CALL WAITS (2.0)
        WRITE(IDSP,*)'THE FILE IS IN USE => ',CFUNFL
        ICOUNT = ICOUNT + 1
        IF (ICOUNT .GT. 10) THEN
          WRITE(IDSP,920) CFUNFL
          GOTO 900
        END IF
       ELSE
        WRITE(IDSP,920) CFUNFL
 920    FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
        CLOSE (UNIT=IFUN)
        RETURN
       END IF
      END IF
      ICOUNT=0
 1000 WRITE(IFUN,500,ERR=1010,IOSTAT=ISTAT5) CFUNCT(I)
 1010 IF (ISTAT5 .NE. 0) THEN
        IF (ISTAT5 .EQ. 77) THEN
          CALL WAITS (2.0)
          WRITE(IDSP,*)'THE FILE IS IN USE => ',CFUNFL
          ICOUNT = ICOUNT + 1
          IF (ICOUNT .GT. 10) THEN
            WRITE(IDSP,1020) CFUNFL
            GOTO 1000
          END IF
        ELSE
          WRITE(IDSP,1020) CFUNFL
 1020     FORMAT('**  WARNING:  NOT ABLE TO ACCESS FILE ',A)
          CLOSE (UNIT=IFUN)
          RETURN
        END IF
      END IF
 1100 CONTINUE
C   ***
C   ***
C   ** SEVERE TIES WITH IFUN
C   ***
      CLOSE (UNIT=IFUN)
      RETURN
      END
