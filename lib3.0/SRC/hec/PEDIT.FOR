      SUBROUTINE PEDIT
C
C        SUBROUTINE 'PEDIT' CAN BE USED TO EDIT A MACRO FILE.
C        THE COMMANDS ARE AS FOLLOWS:
C
C          <I>NSERT  -- INSERT AFTER CURRENT LINE.
C          <D>ELETE  -- DELETE CURRENT LINE (NOTE: A DELETE WHEN
C                       CURRENT LINE IS "MACRO <NAME>" WILL ALLOW
C                       YOU TO DELETE THE ENTIRE MACRO).
C          <R>EPLACE -- REPLACE CURRENT LINE.
C          <N>EXT    -- ADVANCE TO NEXT LINE (NOTE: A NEXT
C                       PERFORMED ON THE "ENDMACRO" LINE WILL
C                       BRING YOU TO THE BEGINNING OF THE MACRO).
C          <F>INISH  -- FINISHED EDITING MACRO SO FILE TO THE
C                       MACRO FILE WITH ANY CHANGES.
C
C        NOTE: WHILE IN THE EDIT MODE, DEFINED FUNCTION KEYS ARE
C        NOT EXPANDED.
C
C
C
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PNAMES                                                           H
      INCLUDE 'pnames.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
C
C     * after macro editting, the macro index is obselete - Alaric
C       NEWIND - <T> rebuild the macro index
      LOGICAL NEWIND, LEOF
      COMMON /MACCOM/ NEWIND
C
      CHARACTER*133 CLOCAL
      CHARACTER*7   CEDCOM
      CHARACTER*6   CCMNDS
      CHARACTER*3   CYORN
      CHARACTER*60  CPRMZZ
C
      LOGICAL  LNEW
C
      DATA CCMNDS /'IDRNPF'/
C
      CPRMZZ = CPROMT
      NPRMZZ = NPROMT
C
      LNEW = .TRUE.
C   ***
C   ***
C   ** SEARCH FOR MACRO
C   ***
        REWIND ISCT
        REWIND IMAC
 212    READ(IMAC,101,END=100) C133
        IF (C133(1:6) .NE. 'MACRO ') GOTO 220
        ISTART = (NINDX(C133(6:),' ')+5)
        IEND = (INDEX(C133(ISTART:),' ') + ISTART-1)
        IF (C133(ISTART:IEND) .NE. CNAMAC) GOTO 220
C   ***
C   ***
C   ** FOUND IT, SO SAVE IN MACRO BUFFER
C   ***
      LNEW = .FALSE.
      ILOOP = 1
      CLBUFM( ILOOP ) = C133
      ILOOP = ILOOP + 1
 213  READ( IMAC, 101, END=100 ) C133
      IF ( ILOOP .GT. (IMXMCP+2) ) THEN
         WRITE(IDSP,102) IMXMCP
 102     FORMAT(/' ** MACRO BUFFER OVERFLOW -- MAX LINES = ', I5/)
         RETURN
      ENDIF
      IF ( C133(1:9) .EQ. 'ENDMACRO ' ) THEN
         CLBUFM( ILOOP ) = C133
         IBUFLN = ILOOP
         GOTO 215
      ENDIF
      CLBUFM( ILOOP ) = C133
      ILOOP = ILOOP + 1
      GOTO 213
C   ***
C   ***
C   ** FREEZE IMAC AND ISCT WHERE THEY ARE AND EDIT THE EXISTING MACRO.
C   ***
 215    CONTINUE
        GOTO 100
C   ***
C   ***
C   ** WRITE LINE
C   ***
 220    CONTINUE
        CALL CHRLNB (C133,NC)
        WRITE(ISCT,101) C133(1:NC+1)
        GOTO 212
C   ***
C   ***
C   ** WRITE STATUS MESSAGE.
C   ***
 100  CONTINUE
      IF ( .NOT. LNEW ) THEN
         WRITE(IDSP,103) ' ** EDITING AN EXISTING MACRO...'
      ELSE
         WRITE(IDSP,103) ' ** EDITING A NEW MACRO...'
         CLBUFM(1) = 'MACRO '//CNAMAC
         CLBUFM(2) = 'ENDMACRO '
      ENDIF
 101  FORMAT(A)
 103  FORMAT(/,A)
C
C
      IBINDX = 1
      IF (LNEW) THEN
         ILINES = 2
      ELSE
         ILINES = IBUFLN
      ENDIF
C   ***
C   ***
C   ** PRINT CURRENT LINE
C   ***
 300  CONTINUE
      WRITE (IDSP,*)                                                    MLlg
      CALL CHRLNB(CLBUFM(IBINDX),NC)
      WRITE (IDSP,*) CLBUFM(IBINDX)(1:NC+1)                             MLlg
C     WRITE(IDSP,301) CLBUFM( IBINDX )(1:NC+1)                          H
 301  FORMAT(/,1X, A)
C   ***
C   ***
C   ** GET EDIT COMMAND AND BRANCH ACCORDINGLY
C   ***
C------
      CALL PSET ( 'PROM',
     &  'Insert, Delete, Replace, Next, Print, or Finish? ',  49 )
      CALL PGTLIN ( LEOF )
      CEDCOM = CLINE
C------
CC    CALL ANREAD( IIN,
CC   &  '<I>NSERT, <D>ELETE, <R>EPLACE, <N>EXT, <P>RINT, OR <F>INISH? ',
CC   &   61, CEDCOM, INUMCH )
      IEDCOM = INDEX( CCMNDS, CEDCOM(1:1) )
      IF ( IEDCOM .EQ. 0 ) THEN
         GOTO 310
      ELSE
         GOTO (1000, 2000, 3000, 4000, 5000, 6000), IEDCOM
      ENDIF
 310  WRITE(IDSP,101) ' ** INVALID EDIT COMMAND'
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <I>NSERT-ING.
C   ***
 1000 CONTINUE
      IF (ILINES .EQ. (IMXMCP+2) ) THEN
         WRITE(IDSP,102) IMXMCP
      ELSEIF (IBINDX .EQ. ILINES) THEN
         WRITE(IDSP,101)' ** CANNOT INSERT AT THIS LINE'
      ELSE
         DO 1010 I = ILINES, (IBINDX+1), -1
            CLBUFM( I+1 ) = CLBUFM( I )
 1010    CONTINUE
         ILINES = ILINES + 1
         IBINDX = IBINDX + 1
C------
      CALL PSET ( 'PROM',
     &  'I> ', 3 )
      CALL PGTLIN ( LEOF )
      CLOCAL = CLINE
C------
CC       READ(IIN, 101, ERR=8000) CLOCAL
         CLBUFM( IBINDX ) = CLOCAL
      ENDIF
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <D>ELETE-ING.
C   ***
 2000 CONTINUE
      IF (IBINDX .EQ. 1) THEN
C------
      CALL PSET ( 'PROM',
     &  'Do you wish to DELETE macro '//CNAMAC//'? ',  38 )
      CALL PGTLIN ( LEOF )
      CYORN = CLINE
C------
CC       CALL ANREAD( IIN, 'DO YOU WISH TO DELETE MACRO '//CNAMAC//'? ',
CC   &                38, CYORN, INUMCH )
         IF ( CYORN(1:1) .EQ. 'Y' ) GOTO 9000
      ELSEIF (IBINDX .EQ. ILINES) THEN
         WRITE(IDSP,101)' ** CANNOT DELETE THIS LINE'
      ELSE
         DO 2020 I = IBINDX, (ILINES-1)
            CLBUFM( I ) = CLBUFM( I+1 )
 2020    CONTINUE
         ILINES = ILINES - 1
      ENDIF
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <R>EPLACE-ING.
C   ***
 3000 CONTINUE
      IF ( (IBINDX .EQ. 1) .OR. (IBINDX .EQ. ILINES) ) THEN
         WRITE(IDSP,101)' ** CANNOT REPLACE THIS LINE'
      ELSE
C------
      CALL PSET ( 'PROM',
     &  'I> ',  3 )
      CALL PGTLIN ( LEOF )
      CLOCAL = CLINE
C------
         READ( IIN, 101, ERR=8000 ) CLOCAL
         CLBUFM( IBINDX ) = CLOCAL
      ENDIF
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <N>EXT-LINE.
C   ***
 4000 CONTINUE
      IF (IBINDX .EQ. ILINES) THEN
         IBINDX = 1
      ELSE
         IBINDX = IBINDX + 1
      ENDIF
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <P>RINT-ING.
C   ***
 5000 CONTINUE
      WRITE(IDSP,5050)
      DO 5010 I = 1, ILINES
         CALL CHRLNB(CLBUFM(I),NC)
         WRITE (IDSP,*) CLBUFM(I)(1:NC+1)                               MLlg
C        WRITE(IDSP,5060) CLBUFM( I )(1:NC+1)                           H
 5010 CONTINUE
 5050 FORMAT(/)
 5060 FORMAT(1X,A)
      GOTO 300
C   ***
C   ***
C   ** HERE FOR <F>INISH-ING.
C   ***
 6000 CONTINUE
      DO 6060 I = 1, ILINES
         CALL CHRLNB(CLBUFM(I),NC)
         WRITE(ISCT,101) CLBUFM( I )(1:NC+1)
 6060 CONTINUE
      GOTO 9000
C   ***
C   ***
C   ** ERROR IN READING IN A LINE.
C   ***
 8000 CONTINUE
      WRITE(IDSP,101) ' ** INVALID LINE'
      GOTO 300
C   ***
C   ***
C   ** NOW WE WILL WRITE THE REST OF IMAC INTO ISCT.
C   ***
 9000 CONTINUE
      READ(IMAC,101,END=9900) C133
      CALL CHRLNB(C133,NC)
      WRITE(ISCT,101) C133(1:NC+1)
      GOTO 9000
C   ***
C   ***
C   ** COPY BACK FROM ISCT TO IMAC.
C   ***
 9900 CONTINUE
C
C     * macro index is obselete - new index next time PLDMAC is called
      NEWIND = .TRUE.
C
      REWIND ISCT
      REWIND IMAC
 9910 READ(ISCT,101,END=9999) C133
      CALL CHRLNB(C133,NC)
      WRITE(IMAC,101) C133(1:NC+1)
      GOTO 9910
C   ***
C   ***
C   ** NOW WE CAN RETURN.
C   ***
 9999 CONTINUE
      CALL PSET ('PROM', CPRMZZ, NPRMZZ )
      RETURN
C
      END
