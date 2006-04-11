*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/gr_vertcon.f,v 1.1 1998/07/07 20:10:23 grogers Exp $
*  gr_vertcon.f
*
      subroutine gr_vertcon(
     i  option,         !*  (0=open files, 9=close files,
                        !*   1=calc correction, NGVD29 to NAVD88,
     i  ilat,           !*  input latitude, Sddmmss.sss, char*11
     i  ilon,           !*  input longitude, Sdddmmss.sss, char*12
     o  ht_corr,        !*  height correction, NAVD88-NGVD29, meters
     o  rtncde)         !*  return code (0=no error)
*
**    program vertcon
***********************************************************************
*                            Release 2.0                              *
* PURPOSE:    CALCULATE THE DATUM CONVERSION CORRECTION TO NGVD29     *
*             ORTHOMETRIC HEIGHT IN ORDER TO OBTAIN NAVD88 HEIGHT.    *
*             THE INPUT REQUIRES LATITUDE AND LONGITUDE VALUES;       *
***********************************************************************
*                                                                     *
*  This program adapted from NOAA VERTCON.F program
*  See /watstore/geod/src/vertcon/vertcon.f for more information
*
*  $Log: gr_vertcon.f,v $
*  Revision 1.1  1998/07/07 20:10:23  grogers
*  PR#0, initial add of vertcon_lib
*
*
c
      real vrsion
      parameter (vrsion = 2.00E0)
      logical page, screen,nogo
      character*60 name
**    SAVE NCONV,ITYPE,PAGE,SCREEN
*
      include 'nadcomm.i'
      save
      integer*4 ideg, imin, option, rtncde
      real*4 ht_corr
      real*8 deg, min, sec
      character ilat*11, ilon*12

c
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)

      rtncde = 0

*-----------------------------------------------------------------------
*     open files (option = 0)
*-----------------------------------------------------------------------

      if (option .eq. 0)  then
*
* INITIALIZE VARIABLES
      CALL vINITL (SCREEN, PAGE, NAME, IPAGE, ITYPE)
* DISCLAIMER
*GR   CALL vHEADR(VRSION)

* LOAD 'VERTCON' GRID
       call loadgrd(nogo)
*GR   if(.not.nogo) then
      if (nogo)  goto 9900

*-----------------------------------------------------------------------
*     process one point (option =1/2)
*-----------------------------------------------------------------------

      else if (option.eq.1 .or. option.eq.2)  then

* REQUEST FOR THE NEEDED VARIABLE VALUES FROM THE USER
*GR   CALL vIPARMS (ITYPE, SCREEN,VRSION)

*
*     save input variables in common block
*
      read (ilat,'(1x,f2.0,f2.0,f6.0)',err=9900) deg, min, sec
      yvalue = deg + min/60. + sec/3600.
      if (ilat(1:1) .eq. '-')  yvalue = -yvalue
      read (ilon,'(1x,f3.0,f2.0,f6.0)',err=9900) deg, min, sec
      xvalue = deg + min/60. + sec/3600.
      if (ilon(1:1) .eq. '-')  xvalue = -xvalue

* LOOP ONCE FOR EACH COMPUTATION
      CALL gr_vMLOOP (NCONV, IPAGE, ITYPE, VRSION,
     +            PAGE, SCREEN, NAME)
      if (errcode .ne. 0)  goto 9900
      ht_corr = ght_diff
*-----------------------------------------------------------------------
*     close files (option=9)
*-----------------------------------------------------------------------

      else         !*  option must be 9

        call closegrd

*GR   IF(ITYPE.GT.0) THEN 
*GR     CLOSE(NIN,IOSTAT=IOS,ERR=1001,STATUS='KEEP')
*GR     CLOSE(NOUT,IOSTAT=IOS,ERR=1002,STATUS='KEEP')
*GR     call closegrd
*GR     IF(LDUMP.NE.LUOUT)
*GR  +   CLOSE(LDUMP,IOSTAT=IOS,ERR=1003,STATUS='KEEP')
*GR   ENDIF
*GR   endif

      endif
      goto 9999

*GR   write(LUOUT,*) 'Normal End'
*GR   stop
*GR 1001 write(LUOUT,1104) ios
*GR 1104 format(' Input File i/o err. =',i5)
*GR      stop
*GR 1002 write(LUOUT,1105) ios
*GR 1105 format(' Output File i/o err. =',i5)
*GR      stop
*GR 1003 write(LUOUT,1106) ios
*GR 1106 format(' Dump  File i/o err. =',i5)
*GR      stop

 9900 rtncde = 1

 9999 return

      end                                     
