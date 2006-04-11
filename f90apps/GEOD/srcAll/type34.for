*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/type34.f,v 1.1 1998/07/07 20:10:28 grogers Exp $
*  type34.f
*
      SUBROUTINE TYPE34 (NCONV,VRSION,ITYPE,NAME,IDLA, IMLA, SLA, 
     .    IDLO, IMLO, SLO, XPT, YPT, EOF, NOPT)
*
* Purpose: Read a record from a file of type 3 or 4.
****************************************************
*
*  $Log: type34.f,v $
*  Revision 1.1  1998/07/07 20:10:28  grogers
*  PR#0, initial add of vertcon_lib
*
*
* Read a record from a file of type 3 or 4. In this type only the
* latitude, longitude and orthometric height are extracted;
* the record MUST BE either in NGS Blue Book or in 
*       NGS Internal Bench Mark File format
*  NOTE - blank records or non- *30* blue book records are copied
*
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      DOUBLE PRECISION XPT, YPT, RDLA, RDLO
      REAL SLA, SLO, RMLA, RMLO
      INTEGER IDLA, IMLA, IDLO, IMLO
      LOGICAL EOF, NOPT, TONGVD, CODE15
      CHARACTER*6  NGVD,NAVD
      CHARACTER*80 CARD
      CHARACTER*96 B96 
      CHARACTER*40 NAME
      COMMON /vCURNT/ B96
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, TONGVD, CODE15
      EQUIVALENCE(CARD,B96)
      DATA NGVD/'NGVD29'/,NAVD/'NAVD88'/
* FOR INPUT FILE OF ITYPE = 3 or 4
   10 IF(ITYPE.EQ.3) THEN
        READ (NIN,'(A80)',IOSTAT=IOS,ERR=9991,END=9999) CARD
       ELSE
        READ (NIN,'(A96)',IOSTAT=IOS,ERR=9991,END=9999) B96 
      ENDIF
* Check for blank line
      IF (CARD .EQ. B80) THEN
         NCONV=NCONV+1
       IF (NCONV .EQ. 1) THEN
        WRITE (NOUT, 1) VRSION
    1   FORMAT ('VERTCON  Version', F4.1)
       ENDIF
         WRITE(NOUT,2,IOSTAT=IOS,ERR=9992) CARD
    2 FORMAT(A80)
        GOTO 10
       ENDIF
* Check for non-*30* blue book records
      IF(ITYPE.EQ.3) THEN
        IF(CARD(7:10).NE.'*30*') THEN
          NCONV=NCONV+1
          IF (NCONV .EQ. 1) WRITE (NOUT, 1) VRSION
         IF(CARD(7:10).EQ.'*15*') THEN
          IF(CARD(11:16).EQ.NGVD) THEN
            CODE15=.TRUE.
            TONGVD=.TRUE.
            CARD(11:16)=NAVD(1:6)
          ELSEIF (CARD(11:16).EQ.NAVD) THEN
            CODE15=.TRUE.
            TONGVD=.FALSE.
            CARD(11:16)=NGVD(1:6)
          ENDIF
        ENDIF
          WRITE(NOUT,2,IOSTAT=IOS,ERR=9992) CARD
         GO TO 10
       ELSE
        IF(.NOT.CODE15) THEN
         WRITE(LUOUT,*) ' Blue Book record *15* with Datum Code',
     +           ' was not found before *30* record -'
         NOPT=.TRUE.
         RETURN
        ENDIF
       ENDIF
        READ(CARD(15:39),'(A25)') NAME
        READ(B96,3,IOSTAT=IOS,ERR=9970) 
     .        RDLA,RMLA,SLA,RDLO,RMLO,SLO
    3 FORMAT(67x,3f2.0,f3.0,2f2.0,16x)
       ELSE
        READ(CARD(20:49),'(A30)') NAME
        READ(B96,4,IOSTAT=IOS,ERR=9970) 
     .        RDLA,RMLA,SLA,RDLO,RMLO,SLO
    4 FORMAT(49x,3f2.0,1x,f3.0,2f2.0,33x)
      ENDIF
* Check for illogical values
      IF (RDLA .LT.   0.D0) GOTO 9950
      IF (RDLA .GT.  90.D0) GOTO 9950
      IF (RMLA .LT. 0.E0  .OR.  RMLA .GT. 60.E0) GOTO 9950
      IF ( SLA .LT. 0.E0  .OR.   SLA .GT. 60.E0) GOTO 9950
* LONGITUDE
* Check for illogical values
      IF (RDLO .LT.   0.D0) GOTO 9960
      IF (RDLO .GT. 360.D0) GOTO 9960
      IF (RMLO .LT. 0.E0  .OR.  RMLO .GT. 60.E0) GOTO 9960
      IF ( SLO .LT. 0.E0  .OR.   SLO .GT. 60.E0) GOTO 9960
* Calculate decimal degrees
      YPT = RDLA + DBLE(RMLA)/60.D0 + DBLE(SLA)/3600.D0
      XPT = RDLO + DBLE(RMLO)/60.D0 + DBLE(SLO)/3600.D0
* Get degrees, minutes, seconds
      CALL ANGLE (YPT, IDLA, IMLA, SLA)
      CALL ANGLE (XPT, IDLO, IMLO, SLO)
      RETURN
* Error messages
 9950 WRITE (LUOUT,*) ' ERROR - Illogical values for latitude'
      GO TO 9980
 9960 WRITE (LUOUT,*) ' ERROR - Illogical values for longitude'
      GO TO 9980
 9970 WRITE (LUOUT,*) ' ERROR - NGS File geod. position format error'
*
 9980 NOPT = .TRUE.  
      WRITE(LUOUT,'(a80)') CARD
      RETURN
 9991 WRITE(LUOUT,5) IOS
    5 FORMAT(' INPUT file i/o error -',i5)
      GO TO 9980
 9992 WRITE(LUOUT,6) IOS
    6 FORMAT(' OUTPUT file i/o error -',i5)
      GO TO 9980
*
 9999 EOF = .TRUE.
      RETURN
      END
