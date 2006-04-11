*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vfgrid.f,v 1.1 1998/07/07 20:10:32 grogers Exp $
*  vfgrid.f
*
      subroutine vfgrid(lin,glo,gla,glomn,glamn,dglo,dgla,nlo,
     .   xgrid,ygrid,irow,jcol,tee1,tee2,tee3,tee4,ios)
*
*  Purpose: Calculates coordinates for VERTCON
**********************************************
*
*  $LOG$
*
      implicit double precision (a-h,o-z)
      parameter(len=461)
      integer dummy
      real*4 z(len)
      real*4 glamn,dgla,glomn,dglo
c  calculate the coordinates for the point in terms of grid
c  indices
      xgrid=(glo-glomn)/dglo+1.d0
      ygrid=(gla-glamn)/dgla+1.d0
c   find the i,j, values for the SW corner of local square
       irow=int(ygrid)
       jcol=int(xgrid)
c
       read(lin,rec=irow+1) dummy,(z(i),i=1,nlo)
        if((z(jcol).eq.9999.0).or.(z(jcol+1).eq.9999.0)) go to 10
c
       tee1=z(jcol)
       tee3=z(jcol+1)
c
       read(lin,rec=irow+2) dummy,(z(i),i=1,nlo)
        if((z(jcol).eq.9999.0).or.(z(jcol+1).eq.9999.0)) go to 10
       tee2=z(jcol)
       tee4=z(jcol+1)
       return
c
   10  ios=999
       return
       end
