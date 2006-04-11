*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vcoeff.f,v 1.1 1998/07/07 20:10:30 grogers Exp $
*  vcoeff.f
*
      subroutine vcoeff (tee1,tee2,tee3,tee4,ay,bee,cee,dee)
*
*  Purpose: Calc coefficient for VERTCON
****************************************
*
*  $Log: vcoeff.f,v $
*  Revision 1.1  1998/07/07 20:10:30  grogers
*  PR#0, initial add of vertcon_lib
*
*
      implicit double precision (a-h,o-z)
      ay=tee1                          
      bee=tee3-tee1
      cee=tee2-tee1
      dee=tee4-tee3-tee2+tee1
      return
      end
