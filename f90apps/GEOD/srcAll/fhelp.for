*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/fhelp.f,v 1.1 1998/07/07 19:32:09 grogers Exp $
*  fhelp.f
*
      SUBROUTINE FHELP

*  Purpose: Print information about the formats of the input data
*** file types used by NADCON.
*****************************************************************
*
*  $Log: fhelp.f,v $
*  Revision 1.1  1998/07/07 19:32:09  grogers
*  PR#0, initial load of nadcon_lib
*
*

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      INTEGER ITYPE, IOS
      CHARACTER*1 ANS

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

*****************************
* CHOSE THE INPUT FILE FORMAT
*****************************

 9001 WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' What format do you want information about?'
      WRITE (LUOUT,*) '  1) Free Format Type 1'
      WRITE (LUOUT,*) '  2) Free Format Type 2'
      WRITE (LUOUT,*) '  3) NGS Blue Book format (default)'

      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. ' ') THEN

        ITYPE = 3

      ELSE
        READ (ANS,347,ERR=9940,IOSTAT=IOS) ITYPE
  347   FORMAT (I1)

        IF (ITYPE .GT. 3  .OR.  ITYPE .LT. 1) THEN

* Not a good answer - Try again.

          WRITE (LUOUT,*) ' Gotta pick one of these -',
     +                    ' sorry try again.'
          GOTO 9001
        ENDIF
      ENDIF

* Print information

      IF (ITYPE .EQ. 1) THEN

*******************************
* FOR FILE FORMAT ITYPE = 1
* FREE FORMAT TYPE1 INPUT FILE
* PRETTY OUTPUT FORMAT
*******************************

        WRITE (LUOUT,2)
*   2   FORMAT ('1')
    2   FORMAT ('')
        WRITE (LUOUT, 110)
  110   FORMAT (' Free Format Type 1 - The first 40 characters of',
     +                                  ' the input data record may', /,
     +          ' contain the station name or be blank.  The rest of',
     +                                 ' the record (columns 41-80)', /,
     +          ' must contain the latitude and longitude.  They may',
     +                                    ' be given in (1) decimal', /,
     +          ' degrees; (2) integer degrees and decimal minutes,',
     +                                    ' or (3) integer degrees,', /,
     +          ' integer minutes, and decimal seconds.  The decimal',
     +                                    ' portion of the latitude', /,
     +          ' MUST contain a decimal point as it is used to',
     +                                ' determine which is the last', /,
     +          ' number forming part of the latitude.  The output',
     +                                ' will be in "pretty" format.', /)

        WRITE (LUOUT, 120)
  120   FORMAT (' The following three records are examples of valid',
     +                                            ' input records:', //,
     +          ' <------------ Columns 1-40 ------------>',
     +                     '<------------ Columns 41-80----------->', /,
     +          ' AAA                                     34.',
     +                                     '4444444      98.8888888', /,
     +          ' BBB                                     25',
     +                                   ' 55.55555     76 56.66666', /,
     +          ' CCC                                     45 45',
     +                                     ' 45.555   111 11 11.111', /)

        WRITE (LUOUT,931)
  931   FORMAT (12X,  '             (Hit RETURN to continue.)')
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)

        WRITE (LUOUT, 130)
  130   FORMAT (' The following is an example of the output.  Note',
     +                               ' that with Free Format Type 1', /,
     +          ' data, both the input and transformed latitude and',
     +                                 ' longitude are expressed in', /,
     +          ' degrees, minutes, and seconds regardless of the',
     +                                           ' method of input.', /)
        WRITE (LUOUT, 140)
  140   FORMAT ('                           Transformation #:',
     +                                 '    1        Region: Conus', //,
     +                                        ' Station name:  AAA', //,
     +          '                                    Latitude',
     +                                  '                 Longitude', /,
     +          '  NAD 27 datum values:           34 26 39.99984',
     +                                   '           98 53 19.99968', /,
     +          '  NAD 83 datum values:           34 26 40.28857',
     +                                   '           98 53 21.25886', /,
     +          '  NAD 83 - NAD 27 shift values:         0.28873',
     +                            '                  1.25918(secs.)', /,
     +          '                                        8.897',
     +                         '                   32.144  (meters)', /,
     +          '  Magnitude of total shift:',
     +                        '                      33.353(meters)', /)

      ELSEIF (ITYPE .EQ. 2) THEN

*******************************
* FOR FILE FORMAT ITYPE = 2
* FREE FORMAT TYPE2 INPUT FILE
* FREE FORMAT OUTPUT
*******************************

        WRITE (LUOUT,2)
        WRITE (LUOUT, 210)
  210   FORMAT (' Free Format Type 2 - The first 40 characters of',
     +                                 ' the input data record must', /,
     +          ' contain the latitude and longitude.  They may be',
     +                              ' given in (1) decimal degrees;', /,
     +          ' (2) integer degrees and decimal minutes, or (3)',
     +                                   ' integer degrees, integer', /,
     +          ' minutes, and decimal seconds.  The decimal portion',
     +                               ' of the latitude MUST contain', /,
     +          ' a decimal point as it is used to determine which',
     +                                 ' is the last number forming', /,
     +          ' part of the latitude.  The rest of the input record',
     +                                ' (columns 41-80) may contain', /,
     +          ' the station name or be blank.  The output will be',
     +                                  ' in the same format as the', /,
     +          ' input but will contain the transformed latitude and',
     +                                                 ' longitude.', /)

        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)

        WRITE (LUOUT, 220)
  220   FORMAT (' The following three records are examples of',
     +                                      ' valid input records:', //,
     +          ' <------------ Columns 1-40 ------------>',
     +                     '<------------ Columns 41-80----------->', /,
     +          ' 45 45 45.55555 111 11 11.11111          ',
     +                                                         'one', /,
     +          ' 25 55.5555555   76 56.6666666           ',
     +                                                         'two', /,
     +          ' 34.444444444    98.888888888            ',
     +                                                          'three')

        WRITE (LUOUT, 230)
  230   FORMAT (/, ' The following is an example of the output.', //,
     +          ' NADCON Version 1.02 - NAD 83 datum values converted',
     +                                   ' from NAD 27 datum values', /,
     +          '   45 45 45.30043 111 11 13.94256        ',
     +                                                         'one', /,
     +          '   25 55.5778817   76 56.6404343         ',
     +                                                         'two', /,
     +          '   34.444524645    98.889238661          ',
     +                                                       'three', /)

      ELSEIF (ITYPE .EQ. 3) THEN

****************************************
* FOR INPUT FILE ITYPE = 3
* THE HORIZONTAL BLUE BOOK SPECIFICATION
****************************************

        WRITE (LUOUT,2)
        WRITE (LUOUT, 310)
  310   FORMAT (' NGS Horizontal Blue Book format - *80* (Control',
     +                              ' Point) Record.  Only the *80*', /,
     +          ' records in a Blue Book file are used by NADCON, the',
     +                                   ' other records are passed', /,
     +          ' through without change to the output.  On the *80*',
     +                                 ' records, only the latitude', /,
     +          ' and longitude are modified - the rest of the record',
     +                                  ' is unchanged.  Thus, this', /,
     +          ' format can be used with either ''old'' Blue Book',
     +                          ' files or ''new'' Blue Book files.', /,
     +          ' On the *80* records, the direction of the latitude',
     +                             ' must be north positive (''N'')', /,
     +          ' and the direction of the longitude must be west',
     +                           ' positive (''W'').  The precision', /,
     +          ' of the output will be the same as the precision of',
     +                                   ' the input latitude.')

        WRITE (LUOUT, 320)
  320   FORMAT (/, ' For more information on this format,',
     +                                           ' please refer to:', /,
     +          '   ''Input Formats and Specifications of the',
     +                       ' National Geodetic Survey Data Base''', /,
     +          '   ''Volume 1. Horizontal Control Data''.', /,
     +          ' Published by the Federal Geodetic Control Committee',
     +                                       ' in January, 1989 and', /,
     +          ' available from: the National Geodetic Survey, NOAA,',
     +                                       ' Rockville, MD 20852.', /)

        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)

        WRITE (LUOUT, 330)
  330   FORMAT (' The following input example is a *80* record from a',
     +                                 ' Blue Book file with NAD 27', /,
     +          ' coordinates:', //,

     +          ' 004560*80*096 KNOXVILLE CT HSE',
     +                    '              411906578  N0930548534  W 277')

        WRITE (LUOUT, 340)
  340   FORMAT (/, ' The following example is of the output *80*',
     +                                ' record with the transformed', /,
     +          ' NAD 83 latitude and longitude.', //,
     +          ' 004560*80*096 KNOXVILLE CT HSE',
     +                 '              411906566  N0930549268  W 277', /)

      ENDIF

      WRITE (LUOUT,*) ' Do you want more information (Y/N)?'
      WRITE (LUOUT,*) ' (Default is Y)'
      READ (LUIN,'(A1)') ANS
      IF (ANS .NE. 'N'  .AND.  ANS .NE. 'n') GOTO 9001

      RETURN

* Error message

 9940 WRITE (LUOUT,*) ' Gotta pick ''1'' or ''2'' or ''3'' -',
     +                ' sorry try again.'
      GOTO 9001
      END
