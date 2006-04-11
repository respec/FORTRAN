*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/interp.f,v 1.4 1998/07/14 20:37:03 grogers Exp $
*  interp.f
*
       subroutine interp(gla,glo,val,ios)

*  Purpose: bilinear interpolation from a standard grid file
************************************************************
*
*  $Log: interp.f,v $
*  Revision 1.4  1998/07/14 20:37:03  grogers
*  PR#0, added declaration of iop
*
c Revision 1.3  1998/07/14  17:38:31  grogers
c PR#0, added code to do dynamic unit allocation for opening grid files.
c Files opened with hard-coded file unit numbers  were being deleted by
c GW tabling software.
c
c Revision 1.2  1998/07/08  22:13:57  grogers
c PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
c
c Revision 1.1  1998/07/07  20:10:25  grogers
c PR#0, initial add of vertcon_lib
c
*

      implicit double precision(a-h,o-z)
      parameter(narea=10,len=461)
      
cpd   external ints, nlen$a, nwf_add_nwishome
cpd   integer*2 ints, nlen$a
      integer i,funits(narea) 
      logical iop
      save funits

      character grid_path*128

      logical nogo
      integer nz(narea)
      real*4 glamn,dgla,glamx,glomn,dglo,glomx,margin,skip
      character*8  pgm
      character*12 files
      character*56 ident
      common/vgrids/ files(narea)
      common/gstuff/glamn(narea),dgla(narea),glamx(narea),glomn(narea),
     .    dglo(narea),glomx(narea),margin(narea),nla(narea),nlo(narea)


*** do the interpolation, check for clipped points
        ios=0
*GR   lin =90

      do n=1,narea

*GR     lin=lin+1
        lin = funits(n)
c
      if(gla.ge.glamn(n).and.gla.le.glamx(n).and.
     *   glo.ge.glomn(n).and.glo.le.(glomx(n)-margin(n))) then
c
        call vfgrid(lin,glo,gla,glomn(n),glamn(n),
     .   dglo(n),dgla(n),nlo(n),xgrid,ygrid,irow,jcol,
     .   tee1,tee2,tee3,tee4,ios)
         if(ios.ne.0) return
c
        call vcoeff(tee1,tee2,tee3,tee4,ay,bee,cee,dee)
        call vsurf(xgrid,ygrid,val,ay,bee,cee,dee,irow,jcol)
c
       return
       endif
      enddo
c
      ios=999
      return
*GR---------------------------------------------------------------------
      entry loadgrd(nogo)
*GR   lin =90
      nogo=.true.
c  open available grid files 
      do n=1,narea
       if(files(n).ne.'           ') then
*GR       added following code to do dynamic unit allocation
        do 20 i=10,99
        inquire (unit=i, opened=iop, err=10)
        if (iop) goto 20
        funits(n) = i
        goto 30
 10     write (*,'(a)') 'ERROR opening grid files from vertcon/interp'
        stop
 20     continue
 30     lin = funits(n)
*GR       end added code

*GR       lin=lin+1
          nogo=.false.

         grid_path = files(n)
cpd      call nwf_add_nwishome(grid_path)

*GR      open(lin,file=files(n),status='old',form='unformatted',
cBriel:
c       +--------------------------------------------------------+
c       | NOTE: To allow for simultaneous access by more than    |
c       | one user, input files can be opened in READ-only mode. |
c       | Output files (if any) can be opened with a unique name |
c       | tag, such as:  userdatetime_outfilename.               |
c       +--------------------------------------------------------+
c
         open(lin,file=grid_path,status='old',form='unformatted',
     .       access='direct',recl=1848,
     +       action='READ')
C
         read(lin,rec=1) ident,pgm,nlo(n),nla(n),nz(n),
     .   glomn(n),dglo(n),glamn(n),dgla(n),skip

c        write(6,'(i4,4x,a12/1x,a56,a8/3i5,5f10.5)')
c    .     n,files(n),ident,pgm,nlo(n),nla(n),nz(n),
c    .   glomn(n),dglo(n),glamn(n),dgla(n),margin(n)
c     WRITE (6,933)
c 933 FORMAT(20x,'(Hit RETURN to continue)')
c     READ (5,'(A1)') ANS
c
         glamx(n)=glamn(n)+dgla(n)*(nla(n)-1)
         glomn(n)=360.0+glomn(n)
         glomx(n)=glomn(n)+dglo(n)*(nlo(n)-1)
         if(nlo(n).gt.len) stop 12345
         if(nla(n).lt.3.or.nlo(n).lt.3) stop 54321
        else
         margin(n)=0.0
         glamn(n)=0.0
         glomn(n)=0.0
         glamx(n)=0.0
         glomx(n)=0.0
        endif
       enddo
       return
*GR---------------------------------------------------------------------
      entry closegrd
*GR      lin=90
      do n=1,narea
       if(files(n).ne.'           ') then
*GR     lin=lin+1
        lin = funits(n)
        close(lin,status='keep')
       endif
      enddo
       return
      end
