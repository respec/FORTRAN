*  $Header: /nwiscvs/watstore/geod/src/inserts/nadcomm.i,v 1.1 1998/07/07 20:22:08 grogers Exp $
*  nadcomm.i
*
*  Purpose: define common block for geodetic transformation software
********************************************************************
*
*  $Log: nadcomm.i,v $
*  Revision 1.1  1998/07/07 20:22:08  grogers
*  PR#0, initial add of geod inserts
*
*
      common /nadcomm/
     -  xvalue, yvalue, xvalue2, yvalue2,
     -  errcode,
     -  ght_diff
      real*8
     -  xvalue,yvalue,         !*  input longitude, latitude, degs
     -  xvalue2,yvalue2        !*  output longitude, latitude, degs
      integer*4
     -  errcode                !*  error code
      real*4
     -  ght_diff               !*  height diff, NAVD88 - NGVD29, meters
