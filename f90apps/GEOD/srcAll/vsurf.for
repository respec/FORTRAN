*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vsurf.f,v 1.1 1998/07/07 20:10:42 grogers Exp $
*  vsurf.f
*
      subroutine vsurf (xgrid,ygrid,zee,ay,bee,cee,dee,irow,jcol)
c
c Purpose: calculates the value of the grid at the point xpt, ypt. the interp.
c is done in the index coordinate system for convenience. 
******************************************************************************
*
*  $Log: vsurf.f,v $
*  Revision 1.1  1998/07/07 20:10:42  grogers
*  PR#0, initial add of vertcon_lib
*
*
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c
      zee1 = ay
      zee2 = bee*(xgrid - float(jcol))
      zee3 = cee*(ygrid - float(irow))
      zee4 = dee*(xgrid - float(jcol))*(ygrid - float(irow))
      zee  = zee1+zee2+zee3+zee4 
      return
      end
