      SUBROUTINE PSLSCN (LRESP)
C
C        This routine write the desired screen to the display
C        and returns the translated response in CLINE.
C        If more than one line is associated with the response
C        the first will be returned in CLINE and the rest will
C        be written to the macro buffer for subsequent use.
C
C
C
C       USE OF ARRAY  IB(I,K) WHERE I=ITEM NUMBER BELOW
C     1       BEGIN BYTE POSITION OF CHAR AREA FOR ENTIRE INFO
C     2       END
C     3       BEGIN RELATIVE BYTE POS OF NAME
C     4       END
C     5       BEGIN RELATIVE BYTE POS OF SCREEN IMAGE
C     6       END
C     7       BEGIN RELATIVE BYTE POS OF TRANSLATE TABLE
C     8       END
C     9       BEGIN RELATIVE BYTE POS OF APPEND TABLE
C     10      END
C     11      # COLS IN SCREEN IMAGE
C     12      # ROWS IN SCREEN IMAGE
C     13      # COLS IN TRANSLATE/APPEND LINES ( ZERO FOR NO REPLY)
C     14      # TRANSLATE LINES
C     15      # APPEND LINES
C     16      TOTAL # BYTES REQ FOR ALL CHAR STORAGE
C     17      PROMPT CURSOR ROW
C     18      PROMPT CURSOR COL
C     19      SCREEN RETENTION PRIORITY
C     20      FULL SCREEN ERASE FLAG
C     21      BASE ROW
C     22      BASE COL
C     23      MESSAGE CURSOR ROW
C     24      MESSAGE CURSOR COL
C     25      ?
C     26      NUMBER OF ALLOWABLE CONSEQUECTIVE ERRORS
C     27      INACTIVITY TIMOUT IN SECONDS FOR THIS SCREEN
C     28      SCREEN DISPLAY LEVEL 0-9
C     29      # LINES IN ATTMAP AREA, EACH SAME WIDTH AS SCREEN
C     30      OFFSET POSITION IN ATTMAP LINE FOR ATTRIBUTES
C
C        USE OF ARRAY  IC(I)  WHERE  I=ITEM NUMBER BELOW
C
C     1       MAX # OF SCREENS SAVED (ONE LESS THAN DIMENSION  KIC)
C     2       NEXT AVAILABLE SLOT
C     3       MAX # CHAR IN CHAR AREA CSCN
C     4       NEXT AVAILABLE CHAR IN CHAR AREA
C
C
C
C
C
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PSCNCH                                                           H
      INCLUDE 'pscnch.h'                                                MLlg
C
C ------
      CHARACTER CNM*66, CLATT*80, CATT*1, CPRMTA*1, CNATT*1
C ------
C
      LOGICAL LANSI,LFRST,LCRT
      INTEGER IVUNIT
      COMMON/LANSI/LANSI,LFRST,LCRT,IVUNIT
C
      CHARACTER*132 CLINX, CTMP*8
      CHARACTER*1 CDUMMY, CPRMT*80, CSCNRG*7, CCR, CLF, CSAB*5, CREG*6
      LOGICAL LFIRST, LEND, LRESP, LBTEST, LSHOW, LABBR, LABORT, LTMP
      LOGICAL LFLUSH, LATT, LMESSY, LCASEY
C
      DATA LFIRST /.TRUE./
C ------ The following is all 2-char abreviations for control char
      DATA CNM / 'NUSHSXEXETEQAKBLBSHTLFVTFFCRSOSIDLD1D2D3D4NKSYEBCNEMSB
     *ECFSGSRSUSDT' /
C
C
C
      CCR = CHAR(13)
      CLF = CHAR(10)
C
      CDUMMY = ' '
      LSCN=.FALSE.
      LRESP = .TRUE.
      LMESSY = .FALSE.
      K=ISCRN
C
C     CHECK IF AN ABORT IS IN PROGRESS
C     CALL GSTRRG ( 'SAB', CSAB, ISAB, IS )                             H
      LABORT = .FALSE.
C     IF ( IS.EQ.0 .AND. CSAB(1:5).EQ.'ABORT' ) THEN                    H
C     LABORT = .TRUE.                                                   H
C     NERROR = 999                                                      H
C     IB(26,K) = 1                                                      H
C     CLINE = '#ABORT'                                                  H
C     JNCHR = 6                                                         H
C     ENDIF                                                             H
C ------
C ------ Check for function character occurance in screen
C ------ definition line, replace with function value
C ------
      IB21 = IB(21,K)
      CALL PSFSCN ( IB21 )
      IB22 = IB(22,K)
      CALL PSFSCN ( IB22 )
      IB19 = IB(19,K)
      CALL PSFSCN ( IB19 )
      IB20 = IB(20,K)
      CALL PSFSCN ( IB20 )
      IB26 = IB(26,K)
      CALL PSFSCN ( IB26 )
      IB27 = IB(27,K)
      CALL PSFSCN ( IB27 )
      IB28 = IB(28,K)
      CALL PSFSCN ( IB28 )
      IB30 = IB(30,K)
      CALL PSFSCN ( IB30 )
C ------
C ------ Check if translate count negative - flush typeahead buffer
      LFLUSH = .FALSE.
      IF ( IB(14,K) .LT. 0 ) THEN
      IB(14,K) = IABS( IB(14,K) )
      LFLUSH = .TRUE.
      ENDIF
C
C ------ Check if ATTRIBUTES available
      LATT = .FALSE.
      IF ( IB(29,K) .GT. 0 ) THEN
      LATT = .TRUE.
      CATT = CHAR(255)
      JATT = IB(11,K) * IB(12,K)
      KATT = IB(1,K) + IB(4,K)
      IF ( IB(30,K) .EQ. 0 ) LATT = .FALSE.
      ENDIF
C
C     CHECK WHAT LEVEL OF SCREENS TO DISPLAY
C     CALL GSTRRG ( 'SCN', CSCNRG, NSCNRG, IS )                         H
C     IF (IS.EQ.0 .AND. CSCNRG(1:6).EQ.'NOSCNS' .AND. NSCNRG.EQ.7) THEN H
C     LSHOW = .FALSE.                                                   H
C     READ ( CSCNRG, '(6X,I1)' ,IOSTAT=IERR) INOSCN                     H
C     IF ( IERR.NE.0 ) LSHOW = .TRUE.                                   H
C     IF ( IB28 .GE. INOSCN ) LSHOW = .TRUE.                            H
C     ELSE                                                              H
      LSHOW = .TRUE.
C     ENDIF                                                             H
C                                                                         PEP
C     CHECK IF ONETIME FORCE SCREEN VISABLE OR INVISABLE                   |
C                                                                          |
      IF(LVSCN) THEN                                                       |
         LSHOW=.TRUE.                                                      |
         LVSCN=.FALSE.                                                     |
      ENDIF                                                                |
C                                                                          |
      IF(LISCN) THEN                                                       |
         LSHOW=.FALSE.                                                     |
         LISCN=.FALSE.                                                     |
      ENDIF                                                                |
C                                                                         PEP
C
C     ERASE GRAPHICS AREA AND DIALOG IF BIT 2 SET (VALUE=4)
      IF(LBTEST(IB20,2)) THEN
      CALL VERASG
      CALL WAITS (0.1)
      CALL VSET ( 'ANSI', '----' )
      CALL WAITS (0.1)
      CALL VERASE
      ENDIF
C
C     ERASE SCREEN IF FLAG SET TO ODD VALUE  1,3,7,F
      IF(LBTEST(IB20,0)) THEN
      CALL WAITS (0.1)
      CALL VSET ( 'ANSI', '----' )
      CALL WAITS (0.1)
      CALL VERASE
      ENDIF
C
      IF(LBTEST(IB20,3)) THEN
      CALL WAITS (0.1)
      CALL VSET ( 'ANSI', 'ANSI' )
      ENDIF
C
      IF ( LABORT ) GO TO 23
C
C     WRITE SCREEN IMAGE TO DISPLAY
C
      JC=IB22
      IS=IB(1,K) + IB(5,K) - 2 - IB(11,K)
      ISATT = IB(1,K) + IB(5,K) + IB(11,K)*IB(12,K)*2 - 1
C ------ Check if null defined attribute exists
      CALL PSCATT ( CNATT, CSCN, ISATT, IB(11,K),IB(29,K),-IB30,INSTAT)
      ITMP = IB(11,K)
      DO 10 I=1,IB(12,K)
         JR=IB21+I-1
         IS=IS + IB(11,K)
         IF ( INSTAT .EQ. 4 .AND. CSCN(IS+JATT+1) .EQ. CNATT )GO TO 904
         CALL CHRBLK ( CLINX )
         DO 9 J=1,ITMP
         IF ( LATT ) CLATT(J:J) = CSCN(IS+J+JATT)
    9    CLINX(J:J) = CSCN(IS+J)
C ------- Set up for expanding forced function references
C ------- within the screen text itself
         LTMP = LFUN
         LFUN = .FALSE.
         CLINE = CLINX(1:ITMP)
         INCHR = ITMP
         CALL PEXLIN
         LFUN = LTMP
         CLINX = CLINE
         CALL CHRLNB ( CLINX, NCH)
      IF ( LSHOW ) THEN
         CALL VMOVCR (JR,JC)
       IF ( LATT ) THEN
         I901S = 1
         I901E = IB(11,K)
  903    DO 901 I901 = I901S, I901E
         IF(CLATT(I901:I901) .NE. CATT ) THEN
         CATT = CLATT(I901:I901)
         GO TO 902
         ENDIF
  901    CONTINUE
         I901 = I901E + 1
  902    N901 = I901 - I901S
         CALL CHRWT ( IKB, CLINX(I901S:I901), N901 )
         IF ( I901 .LE. I901E ) THEN
         I901S = I901
         CALL PSCATT ( CATT, CSCN, ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
         IF ( IPSTAT .EQ. 2 ) GO TO 904
         GO TO 903
         ENDIF
       ELSE
         CALL CHRWT(IKB,CLINX,NCH)
C        CALL CHRWT(IKB,CSCN(IS),IB(11,K))
       ENDIF
      ENDIF
  904 CONTINUE
C     REMEMBER THE PROMPT LINE
      IF ( I .EQ. IB(17,K) ) THEN
      NPRMT = NCH
      CPRMT = CLINX(1:NCH)
      CPRMTA = CLATT(IB(18,K):IB(18,K))
      ENDIF
C
   10 CONTINUE
C
C        Strip trailing <CR> from any previous WRITE statement
C     This is done in anticapation of a FORTRAN READ that will follow
C     soon in ANREAD.  The WRITE below is only needed for the PC
C     implementation of ANREAD and may change in the future!
      WRITE (IKB,13)                                                    MLlg
C13   FORMAT(\)                                                         M
 13   FORMAT('+',A)                                                     Llg
C
C     MOVE CURSOR TO PROMPT LOCATION
C
      IF ( LCRT ) THEN
      IPR = 0
      IPC = 1
      ELSE
      IPR = IB(17,K) + IB21 - 1
      IPC = IB(18,K) + IB22 - 1
      ENDIF
C
      IF ( LSHOW ) CALL VMOVCR (IPR, IPC)
C
C     NOW GET REPLY
C     CHECK IF NO REPLIES POSSIBLE (IE IB(13,K).LE.0)
      IF(IB(13,K).LE.0) THEN
      LRESP=.FALSE.
      RETURN
      ENDIF
C
      NERROR = 0
   22 CONTINUE
C
C     SET UP INACTIVITY TIME OUT IF NECESSARY
C     IF ( IB27 .GT. 0 ) THEN                                           H
C     WRITE(CREG,123) IB27                                              H
C123  FORMAT(I6)                                                        H
C     CALL SSTRRG ( 'SIA', CREG, 6, IST )                               H
C     ELSE                                                              H
C     Check if old reg setting, and reset to zero
C     CALL GSTRRG ( 'SIA', CREG, NREG, IST)                             H
C     IF ( IST .EQ. 0 ) THEN                                            H
C     CREG = '0'                                                        H
C     CALL SSTRRG ( 'SIA', CREG, 6, IST)                                H
C     ENDIF                                                             H
C     ENDIF                                                             H
C ------ Check for flush typeahead request
      IF ( LFLUSH ) THEN
      CALL CHRFL ( IKB )
      ENDIF
C ------ Check if AUTO Mode for screens is on
      IF ( LAUTO ) THEN
C ------ Check if there is an #AUTO response defined for this screen
C
      CLINE = '#AUTO'
      INCHR = 5
C     SEARCH TRANSLATE TABLE FOR MATCH
      IS=IB(1,K) + IB(7,K) - IB(13,K) - 1 + 2
      DO 1030 I=1,IB(14,K)
      IS=IS + IB(13,K)
      DO 1025 II=1,INCHR
      IF(CSCN(IS+II-1) .NE. CLINE(II:II)) GO TO 1030
 1025 CONTINUE
C ------ Found #AUTO go to response processing area
C ------ I know we re-search table there, but don't worry about it!
      GO TO 1111
 1030 CONTINUE
C ------ No match - continue
      ENDIF
C ------ AUTO mode not on, go directly to keyboard for response
      IF ( LATT ) THEN
      CALL PSCATT ( CSCN(KATT),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      IF ( .NOT. LSHOW ) THEN
      CALL CHRWT(IKB,CCR//CLF, 2)
      CALL ANREAD ( IKB, CPRMT, NPRMT, CLINE, JNCHR )
      ELSE
      CALL VMOVCR (IPR, IPC)
      CALL ANREAD(IKB,CDUMMY,-99,CLINE,JNCHR)
      ENDIF
C ------
C ------
   23 CALL CHRBLK(CKBLIN)
      IF ( LATT ) THEN
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      IF(JNCHR .GT. 0 ) CKBLIN(1:JNCHR) = CLINE(1:JNCHR)
      INCHR = JNCHR
      IF(INCHR.GT.8) INCHR=8
      IF(INCHR.EQ.0 .OR. INCHR.EQ.-1) THEN
      INCHR = 3
      CLINE = '^CR'
      ELSE IF ( INCHR .EQ. -3) THEN
      CLINE = '#INACT'
      INCHR = 6
      ELSE IF ( INCHR .EQ. -4) THEN
      CLINE = '#TIMEOUT'
      INCHR = 8
C ------ CHECK FOR CONTROL CHARACTERS AND SET TO ^nm
      ELSE IF ( INCHR .EQ. 1 ) THEN
      ICC = ICHAR ( CKBLIN(1:1) )
      IF ( ICC .LT. 32 ) THEN
      ICC = ICC * 2 + 1
      CLINE = '^'//CNM(ICC:ICC+1)
      INCHR = 3
      ELSE IF ( ICC .EQ. 127 ) THEN
      CLINE = '^'//CNM(65:66)
      INCHR = 3
      ENDIF
      ENDIF
C     SEARCH TRANSLATE TABLE FOR MATCH
 1111 IS=IB(1,K) + IB(7,K) - IB(13,K) - 1 + 2
      DO 30 I=1,IB(14,K)
      IS=IS + IB(13,K)
C     CHECK IF FULL RESPONSE OR ABBREVIATIONS ARE ALLOWED
      LABBR = .TRUE.
      IF ( CSCN(IS-1) .EQ. ':' ) LABBR = .FALSE.
      IF ( CSCN(IS-1) .EQ. ';' ) LABBR = .FALSE.
C------ Check if Case sensitive response
      LCASEY = .TRUE.
      IF ( CSCN(IS-1) .EQ. ',' ) LCASEY = .FALSE.
      IF ( CSCN(IS-1) .EQ. ';' ) LCASEY = .FALSE.
      CTMP = CLINE
      IF ( .NOT. LCASEY ) CALL UPCASE ( CTMP )
C
C     FIRST CHECK FOR TABLE ENTRY OF #ANY
      IF(CSCN(IS) .EQ. '#') THEN
      IF(CSCN(IS+1).EQ.'A'.AND.CSCN(IS+2).EQ.'N'.AND.CSCN(IS+3).EQ.'Y')
     1  GO TO 27
      ENDIF
      DO 25 II=1,INCHR
      IF(CSCN(IS+II-1) .NE. CTMP (II:II)) GO TO 30
   25 CONTINUE
C     CHECK FOR ABBREVIATION
      IF ( .NOT. LABBR ) THEN
C     NEXT POSITION MUST BE BLANK TO BE OK
      IF( CSCN(IS+INCHR) .EQ. ' ' ) GO TO 27
      GO TO 30
      ENDIF
C     FOUND MATCH
C
C     STUFF ONLY ONE LINE IN CLINE, REST IN MACRO BUFFER
   27 CALL CHRBLK (CLINE)
C ------
C ------ CLEAN UP IF THERE IS AN OLD ERROR MESSAGE THERE
C ------
      IF ( LMESSY ) THEN
      IF ( LATT ) THEN
      CALL PSCATT(CSCN(KATT+2),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      IF ( LSHOW ) THEN
      CALL VMOVCR (IMR, IMC)
      CALL CHRWT(IKB,'                                                 '
     +  ,NMESSY)
      ENDIF
      IF ( LATT ) THEN
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      LMESSY = .FALSE.
      ENDIF
      ENDIF
C ------
C ------ ALL CLEANED UP - CONTINUE
      IS=IS+9
      DO 28 II = 1,IB(13,K)-11
   28 CLINE(II:II) = CSCN(IS+II-1)
      IF(CSCN(IS-11).NE.'1'.OR.IB(15,K).GT.0) THEN
C     NEED TO USE MACRO BUFFER FOR MULT-LINES
      NL=ICHAR(CSCN(IS-11)) - 48   - 1
      IS=IS+IB(13,K)
      NLX=NL
      ML=IB(15,K)
      MLX=ML
C--------------------------------------------------------------------
C   ** MOVE TO TOP OF BUFFER
C   ***
      DO 930 J=1,IMACP+1
      CALL CHRBLK(CLINX)
      CALL PNXTLN(K,IS,NL,NLX,ML,MLX,CLINX,LEND)
      IF(LEND) GO TO 900
C   ***
C   ***
C   ***
        IF ( J .EQ. IMACP+1 ) GOTO 1100
        CLBUFM( J ) = CLINX
 930  CONTINUE
 1100 WRITE (IDSP,1110) IMXMCP
 1110 FORMAT(/' ** MACRO BUFFER OVERFLOW -- MAX LINES = ',I5/)
      IMACP = IMXMCP
      LMACRO = .FALSE.
      RETURN
C   ***
C   ***
C   ** HIT END OF MACRO, MOVE IT DOWN
C   ***
 900  CONTINUE
      IF ( J .EQ. (IMACP+1) ) THEN
         IMACP = 0
         GOTO 920
      ENDIF
      DO 910 IM = (J-1), 1, -1
         CLBUFM( IMACP ) = CLBUFM( IM )
         IMACP = IMACP - 1
 910  CONTINUE
 920  LMACRO = .TRUE.
C   ***
      ENDIF
C
      IF(LBTEST(IB20,1)) THEN
      CALL VERASE
      CALL WAITS (0.1)
      CALL VSET ( 'ANSI', '----' )
      CALL WAITS (0.1)
      CALL VERASE
      ELSE  IF (LSHOW) THEN
      ITR = IB(12,K) + IB21
      CALL VMOVCR ( ITR, 1 )
      ENDIF
      IF(LBTEST(IB20,3)) THEN
      CALL WAITS (0.1)
      CALL VSET ( 'TEK', '----' )
      ENDIF
C
      RETURN
C   ***
C     NO MATCH TRY NEXT LINE
   30 CONTINUE
      NERROR = NERROR + 1
C     CHECK FOR TOO MANY ERRORS IN A SEQUENCE
      IF ( IB26 .GT. 0 ) THEN
      IF ( NERROR .GT. IB26 +1 ) THEN
      CLINE = '#ABORT'
      LRESP = .TRUE.
      RETURN
      ELSE IF ( NERROR .EQ. IB26 +1 ) THEN
      CLINE = '#ABORT'
      JNCHR = 6
      GO TO 23
      ELSE IF ( NERROR .EQ. IB26    ) THEN
      CLINE = '#SEQERR'
      JNCHR = 7
      GO TO 23
      ENDIF
      ENDIF
C     NO MATCH IN ENTIRE TABLE, WRITE MESSAGE AND REQUEST AGAIN
   35 IMR = IB(23,K) + IB21 - 1
      IMC = IB(24,K) + IB22 - 1
      IF ( LATT ) THEN
      CALL PSCATT(CSCN(KATT+2),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      IF (.NOT. LSHOW ) THEN
      NMESSY = INCHR + 27
      CALL CHRWT(IKB,CCR//CLF//'Invalid --"'//CLINE(1:INCHR)
     + //'"-- Re-enter'//CCR//CLF,NMESSY)
      ELSE
      CALL VMOVCR (IMR, IMC)
      CALL CHRWT(IKB,'                                                 '
     +  ,NMESSY)
      CALL VMOVCR (IMR, IMC)
      NMESSY = INCHR + 23
      CALL CHRWT(IKB,'Invalid --"'//CLINE(1:INCHR)
     + //'"-- Re-enter',NMESSY)
      LMESSY = .TRUE.
      CALL VMOVCR (IPR, IPC)
      IF ( LATT ) THEN
      CALL PSCATT ( CPRMTA, CSCN, ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      CALL CHRWT(IKB,'                                     ',JNCHR)
      ENDIF
      IF ( LATT ) THEN
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)
      ENDIF
      GO TO 22
      END
