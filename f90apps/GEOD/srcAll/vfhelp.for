*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vfhelp.f,v 1.1 1998/07/07 20:10:33 grogers Exp $
*  vfhelp.f
*
      SUBROUTINE vFHELP(VRSION)
*
*  Purpose: Print information about the formats of the input data
*** file types used by VERTCON.
*****************************************************************
*
*  $Log: vfhelp.f,v $
*  Revision 1.1  1998/07/07 20:10:33  grogers
*  PR#0, initial add of vertcon_lib
*
*
      CHARACTER*1 ANS
      CHARACTER*6 COR0,COR1,COR2,COR3
      CHARACTER*15 VERSIO
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      DATA COR0,COR1,COR2,COR3/'      ',' -0.05','  0.10',' -0.03'/,
     +     VERSIO/'VERTCON Version'/
* CHOSE THE INPUT FILE FORMAT
 9001 WRITE (LUOUT,*) ' '
      WRITE (LUOUT,*) ' What format do you want information about?'
      WRITE (LUOUT,*) '  1) Free Format Type 1'
      WRITE (LUOUT,*) '  2) Free Format Type 2'
      WRITE (LUOUT,*) '  3) NGS Blue Book Format Type 3'
      WRITE (LUOUT,*) '  4) NGS Internal Bench Mark Format Type 4'
      READ (LUIN,'(A1)') ANS
      IF (ANS .EQ. ' ') THEN
         ITYPE = 5
       ELSE
        READ (ANS,90,ERR=9940,IOSTAT=IOS) ITYPE
   90   FORMAT (I1)
        ENDIF
        IF (ITYPE .GT. 4  .OR.  ITYPE .LT. 1)  GO TO 9940
* Print information
      IF (ITYPE .EQ. 1) THEN
* FOR FILE FORMAT ITYPE = 1
* FREE FORMAT TYPE1 INPUT FILE
* AND    OUTPUT FORMAT
        WRITE (LUOUT,2)
*   2   FORMAT ('1')
    2   FORMAT ('')
        WRITE (LUOUT, 110)
  110   FORMAT (' Free Format Type 1 - ',//,' The first 40 characters',
     +          ' of the input data record may contain the',/,
     +          ' station name or be blank.  The rest of the record',
     +          ' (columns 41-80)',/,' must contain'
     +          ' the latitude and longitude;',/,'  they may be',
     +          ' given either in',//,'  (1) decimal degrees; -OR-',/,
     +          '  (2) integer degrees and decimal minutes; -OR-',/,
     +          '  (3) integer degrees, minutes, and decimal seconds'/
     +          '      [decimal points must be entered !]'/)
        WRITE (LUOUT,3)
    3   FORMAT (' The following is an example of the input.'/)
        WRITE (LUOUT, 120)
  120   FORMAT (' <- - - - - -  Columns 1-40  - - - - - ->',
     +                     '<------------ Columns 41-80----------->',
     +          ' AAA                                     34.',
     +                                     '4444444      98.8888888'/
     +          ' BBB                                     25',
     +                                   ' 55.55555     76 56.66666'/
     +          ' CCC                                     45 45',
     +                                     ' 45.555   111 11 11.111'/)
        WRITE (LUOUT,931)
  931   FORMAT (15X,    '     (Hit RETURN to continue.)')
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,4)
    4   FORMAT (' The following are TWO EXAMPES of the output.'/)
        WRITE (LUOUT, 140)
  140   FORMAT (' Station Name:  AAA')
        WRITE (LUOUT, 142)
        WRITE (LUOUT, 143)
        WRITE (LUOUT, 141)
  141   FORMAT (' Station sequence #:   1')
        WRITE (LUOUT, 142)
  142   FORMAT ('     Latitude         Longitude',
     +          '    NAVD 88 - NGVD 29 (meters)',/
     +          '  34 26 39.99984   98 53',
     +          ' 19.99968        -0.16', /)
  143   FORMAT(20x,'- OR -'/)
        WRITE (LUOUT, 130)
  130   FORMAT (' NOTE -  that with Free Format Type 1 data -'//7x,
     +          ' Station Name is printed if not',
     +          ' blank in the input',/7x,
     +          '  else (generated) Station sequence #',
     +          ' is printed;'/7x,' Output latitude and longitude',
     +          ' are expressed',/7x,'  in degrees, minutes, and', 
     +          ' seconds regardless'/7x,'  of the method of input.'/)
      ELSEIF (ITYPE .EQ. 2) THEN
*
* FOR FILE FORMAT ITYPE = 2
* FREE FORMAT TYPE2 INPUT FILE
* FREE FORMAT OUTPUT
        WRITE (LUOUT,2)
        WRITE (LUOUT, 210)
  210   FORMAT (' Free Format Type 2 - '//' The first 32 characters',
     +          ' of the input data record must contain the',/,
     +          ' latitude and longitude;  they may be given either in',
     +       //,'  (1) decimal degrees; -OR-',/,
     +          '  (2) integer degrees and decimal minutes; -OR-',/,
     +          '  (3) integer degrees, minutes, and decimal seconds',/
     +          '      [decimal points must be entered !]'//
     +          ' The rest of the input record ',
     +          ' (col.41-80) may contain the station name',/,
     +          '  or be blank')
        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,3)
        WRITE (LUOUT, 220)
  220   FORMAT (' <- - - - - -  Columns 1-40 - - - - - ->',
     +          '<- - - - - -  Columns 41-80 - - - - ->')
        WRITE (LUOUT, 230) COR0,COR0,COR0
        WRITE (LUOUT,9)
    9   FORMAT (' The following is an example of the output.'/)
        WRITE (LUOUT, 220)
        WRITE (LUOUT, 221) VERSIO,VRSION
  221   FORMAT(1x,A15,F4.1)
        WRITE (LUOUT, 230) COR1,COR2,COR3
  230   FORMAT (' 45 45 45.55555 111 11 11.11111  ',A6,' one', /,
     +          ' 25 55.5555555   76 56.6666666   ',A6,' two', /,
     +          ' 34.444444444    98.888888888    ',A6,' three', /)
        WRITE (LUOUT,5) VRSION
    5   FORMAT (' NOTE : The output record format is the same as the ',
     +    'input format -'/7x,' except : VERTCON Version',f4.1,
     +    ' (i.e., first line) was added, and'/,16x,
     +    ' It contains the NAVD 88 - NGVD 29 values (col. 33-39)'/)
      ELSEIF (ITYPE .EQ. 3) THEN
* FOR INPUT FILE FORMAT ITYPE = 3
* SAME OUTPUT
        WRITE (LUOUT,2)
        WRITE (LUOUT, 310)
  310   FORMAT (' NGS Blue Book format-'//
     +   ' Columns   Type',12x,'Contents'/25(' -')/
     +   ' 1 -   6   999999          sequence no.'/
     +   ' 7 -  10   char(4)         *30* code'/
     +   ' 11 - 14   9999            SPSN no.'/
     +   ' 15 - 39   char(25)        designation'/
     +   ' 40 - 41   char(2)         unit (KM=kilometers)'/
     +   ' 42 - 49   99999999        accumulated distance'/
     +   ' 50 - 51   char(2)         unit (MT=meters or FT=feet)'/
     +   ' 52 - 61   9999.99999      field elevation'/
     +   ' 62 - 67   char(6)         ACRN number'/
     +   ' 68 - 73   999999          latitude  (ddmmss)'/
     +   ' 74 - 80   999999          longitude (ddmmss)'/)
        WRITE (LUOUT,6)
    6   FORMAT (' NOTE : Blue Book record *15* is required in the',
     +   ' input file !'/7x,' It must specify the input Datum in',
     +   ' columns 11 -16'/7x,' (e.g., NGVD29 or NAVD88) -'/
     +   ' VERTCON - converts NGVD29 field elevations to NAVD88 when',
     +   ' former is input',/35x,'- or -'/10x,' converts NAVD88',
     +   ' field elevations to NGVD29 when former is input;')
        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,9)
        WRITE (LUOUT, 320) VERSIO,VRSION
  320   FORMAT (' <',15('- '),'Columns 1 - 80 ',16(' -'),'>'/
     +          1x,A15,F4.1/ ' 000010*ZZ*VERTOBS NGS   NAT'/ 
     +          ' 000020*10*LXXX         1993020219930202MM4    12MD',
     +          '    EIBNGS           19930202'/
     +          ' 000030*11* TEST OF VERTCON WITH 30 RECORDS'/
     +          ' 000040*15*NAVD88'/
     +          ' 000050*30*0001 BOSSLER RM 1',11x,'KM12.34   MT',
     +          '12.3456   AB123 401020 803040'/)
        WRITE (LUOUT,61) VRSION
   61 FORMAT(' The output records are the same as input records - '/
     +   ' except : 1) VERTCON Version',f4.1,' (i.e., first line) was',
     +   ' added'/10x,'2) datum code in *15* record is reset to new',
     +   ' datum'/10x,'3) field elevation is converted to the new',
     +   ' datum'/10x,'  [unit of field elevation (meter/feet) is',
     +   ' retained;'/10x,'   other than *15* and *30* records are',
     +   ' copied unaltered]'/)
      ELSEIF (ITYPE .EQ. 4) THEN
* FOR INPUT FILE FORMAT ITYPE = 4
* SAME OUTPUT
        WRITE (LUOUT,2)
        WRITE (LUOUT, 311)
  311   FORMAT (' NGS Internal Bench Mark format-'//
     +   ' Columns   Type',12x,'Contents'/25(' -')/
     +   ' 1 -  5    99999           mark number'/
     +   ' 6 - 13    99999999        data base id. (UID)'/
     +   ' 14 - 19   char(6)         archive ref. no. (ACRN)'/
     +   ' 20 - 49   char(30)        designation'/
     +   ' 50 - 55   999999          latitude  (ddmmss)'/
     +   '      56   char(1)         latitude (N=north, S=south)'/
     +   ' 57 - 63   9999999         longitude (dddmmss)'/
     +   '      64   char(1)         longitude (E=east,  W=west)'/
     +   ' 65 - 74   9999.99999      approximate elevation (meter)'/
     +   '      75   char(1)         elev. code (A,B,C,G,F,P,S,X)'/
     +   ' 76 - 83   999.9999        surface gravity (gal.)'/
     +   ' 84 - 88   999.9           sigma gravity (mgal.)'/
     +   '      89   char(1)         status code (D,F,P)'/
     +   ' 90 - 94   99999           original mark no.'/
     +   ' 95 - 96   char(2)         datum code (88 = NAVD88;'/
     +   '                              blank or 29 = NGVD29)'/)
        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,7)
    7   FORMAT (' NOTE :  VERTCON prompts the user ',
     +   ' for the the desired datum conversion; e.g.:'//2x,
     +   ' '' Do you want to convert NGVD29 heights ? Enter y/n :''',
     +   //2x,' When the response is ''y'' or ''Y'' VERTCON will :',
     +    /4x,' convert NGVD29 approx. elevations to NAVD88 for',
     +        ' records'/5x,' having blanks or ''29'' in cols 95-96',
     +   //2x,' When the response is ''n'' or ''N'' VERTCON will :',
     +    /4x,' convert NAVD88 approx. elevations to NGVD29 for',
     +        ' records'/5x,' having  ''88'' in cols 95-96'/)
        WRITE (LUOUT,931)
        READ (LUIN,'(A1)') ANS
        WRITE (LUOUT,2)
        WRITE (LUOUT,9)
        WRITE (LUOUT, 321) VERSIO,VRSION
  321   FORMAT (' <',16('- '),'Columns 1 - 75 ',13(' -'),'>'/
     +       1x,A15,F4.1/ '  1234 1234567EV1234 BOSSLER RM 1',17x,
     +       '334455N1334455W 123.12345F'//55x,'<- - - 76 - 96 - - ->'/
     +       55x,'980.5555  1.0P 123429'/)
        WRITE (LUOUT,71) VRSION
   71 FORMAT(' The output records are the same as input records - '/
     +   ' except : 1) VERTCON Version',f4.1,' (i.e., first line) was',
     +   ' added'/10x,'2) datum code in cols. 95-96 is reset to new',
     +   ' datum'/10x,'3) approximate elevation is converted to new',
     +   ' datum'/)
        ENDIF
      WRITE (LUOUT,*) ' Do you want more information (Y/N)?'
      WRITE (LUOUT,*) ' (Default is Y)'
      READ (LUIN,'(A1)') ANS
      IF (ANS .NE. 'N'  .AND.  ANS .NE. 'n') GOTO 9001
      RETURN
* Error message
 9940 WRITE (LUOUT,*) ' Gotta pick ''1'', ''2'' , ''3'' or ''4'' -',
     +                ' sorry try again.'
      GOTO 9001
      END
