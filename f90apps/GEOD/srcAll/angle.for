*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/angle.f,v 1.1 1998/07/07 20:10:22 grogers Exp $
*  angle.f
*
      subroutine angle(degrees,deg,min,sec)
*
*  Purpose: Convert angle from degrees (real*8) to deg, min, sec
****************************************************************
*
*  $Log: angle.f,v $
*  Revision 1.1  1998/07/07 20:10:22  grogers
*  PR#0, initial add of vertcon_lib
*
*
       real*8 degrees
       integer*4 deg
       deg=degrees
       min=(degrees-deg)*60.d0
       sec=(degrees-deg-min/60.d0)*3600.d0
       if((sec+0.000001).ge.60.0) then
         min=min+1
         sec=sec-60.0
       endif
       if(min.ge.60) then
         deg=deg+1
         min=min-1
       endif
       return
       end
