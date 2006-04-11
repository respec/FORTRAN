*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/ofiles.f,v 1.1 1998/07/07 20:20:46 grogers Exp $
*  ofiles.f
*
      SUBROUTINE OFILES (KIN, KOUT, FOUTA, FOUTO, BASEIN, RIDENT, PGM,
     +                   IMETR)

* Purpose: Interactively get output file basenames.
***************************************************
*
*  $Log: ofiles.f,v $
*  Revision 1.1  1998/07/07 20:20:46  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      CHARACTER*28 B28
      PARAMETER (B28 = '                            ')
      CHARACTER*32 B32
      PARAMETER (B32 = '                                ')

      INTEGER KIN, KOUT, IMETR
      INTEGER IFLAG2, N2
      CHARACTER*56 RIDENT
      CHARACTER*32 FOUTA, FOUTO
      CHARACTER*28 BASEIN, BASOUT, DBNAME
      CHARACTER*8 PGM
      CHARACTER*1 ANS

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      REAL XMIN0, XMAX0, YMIN0, YMAX0, DX, DY
      INTEGER NC, NR
      COMMON /GRID0/ XMIN0, XMAX0, YMIN0, YMAX0, DX, DY, NR, NC

      DATA IFLAG2 /2/

* Find out about the output file(s)

  220 WRITE (LUOUT,115)
  115 FORMAT (   ' For the output files enter:',
     +        /, '     ''A'' for ASCII transfer format.',
     +        /, '     ''B'' for binary - NADCON data file format.',
     +        /, '     ''G'' for ASCII graphics format. ',
     +           ' There are five header information records,',
     +        /, '         and the record lengths',
     +           ' can be VERY large.'
     +        /, '     ''I'' for input file information written to a',
     +           ' single output file.',
     +        /, ' (Default is B)')
      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. ' '  .OR.  ANS .EQ. 'B'  .OR.  ANS .EQ. 'b') THEN
        KOUT = 1
      ELSEIF (ANS .EQ. 'A'  .OR.  ANS .EQ. 'a') THEN
        KOUT = -1
      ELSEIF (ANS .EQ. 'G'  .OR.  ANS .EQ. 'g') THEN
        KOUT = -2
      ELSEIF (ANS .EQ. 'I'  .OR.  ANS .EQ. 'i') THEN
        KOUT = 0
      ELSE
        WRITE (LUOUT, 210) ANS
  210   FORMAT (/, ' ERROR - ''', A1, ''' is not a legal answer.')
        GOTO 220
      ENDIF

* default basename is the input basename unless the same format

      IF (KOUT .NE. KIN) THEN
        DBNAME = BASEIN
        CALL NBLANK (DBNAME, IFLAG2, N2)
      ELSE
        DBNAME = 'nadgrd'
        N2 = 6
      ENDIF

  180 IF (KOUT .EQ. 1) THEN

* The extensions for the binary output files are .las and .los

        WRITE (LUOUT,160) DBNAME(1:N2)
  160   FORMAT (/, ' Enter the file name for the new pair of',
     +             ' NADCON grids.  Enter only the',
     +          /, ' basename.  The ''.las'' and ''.los'' extensions',
     +             ' will be added to the',
     +          /, ' basename by NADGRD.  The default base file name',
     +             ' is ''', A, '''.')
        READ (LUIN,'(A28)') BASOUT
        IF (BASOUT .EQ. B28) BASOUT = DBNAME
        IF (KIN .EQ. 1  .AND.  BASOUT .EQ. BASEIN) THEN
          WRITE (LUOUT,165) BASOUT
  165     FORMAT (/, ' ***** ERROR ****',
     +            /, ' File ''', A, ''' is the name of the input file!',
     +            /, ' You must choose another name.')
          GOTO 180
        ENDIF

        CALL NBLANK (BASOUT, IFLAG2, N2)
        FOUTA = B32
        FOUTA(1:N2) = BASOUT(1:N2)
        FOUTO = B32
        FOUTO(1:N2) = BASOUT(1:N2)
        FOUTA(N2+1:N2+4) = '.las'
        FOUTO(N2+1:N2+4) = '.los'

      ELSEIF (KOUT .EQ. -1) THEN

* The extensions for the ASCII output files are .laa and .loa

        WRITE (LUOUT,190) DBNAME(1:N2)
  190   FORMAT (/, ' Enter the file name for the new pair of',
     +             ' NADCON grids.  Enter only the',
     +          /, ' basename.  The ''.laa'' and ''.loa'' extensions',
     +             ' will be added to the',
     +          /, ' basename by NADGRD.  The default base file name',
     +             ' is ''', A, '''.')
        READ (LUIN,'(A28)') BASOUT
        IF (BASOUT .EQ. B28) BASOUT = DBNAME
        IF (KIN .EQ. -1  .AND.  BASOUT .EQ. BASEIN) THEN
          WRITE (LUOUT,185) BASOUT
  185     FORMAT (/, ' ***** ERROR ****',
     +            /, ' File ''', A, ''' is the name of the input file!',
     +            /, ' You must choose another name.')
          GOTO 180
        ENDIF

        CALL NBLANK (BASOUT, IFLAG2, N2)
        FOUTA = B32
        FOUTA(1:N2) = BASOUT(1:N2)
        FOUTO = B32
        FOUTO(1:N2) = BASOUT(1:N2)
        FOUTA(N2+1:N2+4) = '.laa'
        FOUTO(N2+1:N2+4) = '.loa'

      ELSEIF (KOUT .EQ. -2) THEN

* The extensions for the graphics output files are .lag and .log for
* seconds of arc or .lam and .lom for meters

        WRITE (LUOUT,310)
  310   FORMAT (/, ' Do you wish the ouput grids to have the shifts',
     +             ' in seconds of arc or in meters?',
     +          /, ' Enter ''S'' for seconds of arc or ''M'' for',
     +             ' meters.  The default is seconds of arc.')
        READ (LUIN,'(A1)') ANS
        IF (ANS .EQ. 'M'  .OR.  ANS .EQ. 'm') THEN
          IMETR = -1
        ELSE
          IMETR = 1
        ENDIF

        IF (IMETR .EQ. 1) THEN
          WRITE (LUOUT,170) DBNAME(1:N2)
  170     FORMAT (/, ' Enter the file name for the new pair of',
     +               ' NADCON grids.  Enter only the',
     +            /, ' basename.  The ''.lag'' and ''.log'' extensions',
     +               ' will be added to the',
     +            /, ' basename by NADGRD.  The default base file name',
     +               ' is ''', A, '''.')
        ELSE
          WRITE (LUOUT,175) DBNAME(1:N2)
  175     FORMAT (/, ' Enter the file name for the new pair of',
     +               ' NADCON grids.  Enter only the',
     +            /, ' basename.  The ''.lam'' and ''.lom'' extensions',
     +               ' will be added to the',
     +            /, ' basename by NADGRD.  The default base file name',
     +               ' is ''', A, '''.')
        ENDIF
        READ (LUIN,'(A28)') BASOUT
        IF (BASOUT .EQ. B28) BASOUT = DBNAME

        CALL NBLANK (BASOUT, IFLAG2, N2)
        FOUTA = B32
        FOUTA(1:N2) = BASOUT(1:N2)
        FOUTO = B32
        FOUTO(1:N2) = BASOUT(1:N2)
        IF (IMETR .EQ. 1) THEN
          FOUTA(N2+1:N2+4) = '.lag'
          FOUTO(N2+1:N2+4) = '.log'
        ELSE
          FOUTA(N2+1:N2+4) = '.lam'
          FOUTO(N2+1:N2+4) = '.lom'
        ENDIF

      ELSEIF (KOUT .EQ. 0) THEN

* The extension for the information file is .inf

        WRITE (LUOUT,195) DBNAME(1:N2)
  195   FORMAT (/, ' Enter the file name for the grid information',
     +             ' file.  Enter only the basename.',
     +          /, ' The ''.inf'' extension will be added to the',
     +             ' basename by NADGRD.  The default',
     +          /, ' base file name is ''', A, '''.')
        READ (LUIN,'(A28)') BASOUT
        IF (BASOUT .EQ. B28) BASOUT = DBNAME
        CALL NBLANK (BASOUT, IFLAG2, N2)
        FOUTA = B32
        FOUTA(1:N2) = BASOUT(1:N2)
        FOUTA(N2+1:N2+4) = '.inf'

* Write input header information to the information file

C
cBriel: Did not change default ACTION for output file:
C
        OPEN (NOUTA,FILE=FOUTA,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        STATUS='UNKNOWN')
C
        IF (KIN .EQ. 1) THEN
          CALL NBLANK (BASEIN, IFLAG2, N2)
          WRITE (NOUTA,230) BASEIN(1:N2)//'.las', BASEIN(1:N2)//'.los'
  230     FORMAT (/, ' Files ''', A, ''' and ''', A,
     +               ''' are binary.')
        ELSEIF (KIN .EQ. -1) THEN
          CALL NBLANK (BASEIN, IFLAG2, N2)
          WRITE (NOUTA,235) BASEIN(1:N2)//'.laa', BASEIN(1:N2)//'.loa'
  235     FORMAT (/, ' Files ''', A, ''' and ''', A, ''' are',
     +               ' ASCII.')
        ENDIF
        WRITE (NOUTA,250) RIDENT, PGM
  250   FORMAT (/, ' From the header record(s):',
     +          /, ' Data Identification =''', A56, '''',
     +          /, ' Originating software =''', A8, '''')
        WRITE (NOUTA,260) NC, NR
  260   FORMAT (/, ' Number of columns =', I5,
     +             '    Number of rows =', I5)
        WRITE (NOUTA,270) YMIN0, YMAX0, DY
  270   FORMAT (/, ' Latitude:   minimum =', F8.0,
     +             '   maximum =', F8.0,
     +             '  increment =', F9.5)
        WRITE (NOUTA,280) -XMAX0, -XMIN0, DX
  280   FORMAT (   ' Longitude:  minimum =', F8.0,
     +             '   maximum =', F8.0,
     +             '  increment =', F9.5)
      ENDIF

      RETURN
      END
