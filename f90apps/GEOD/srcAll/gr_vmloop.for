*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/gr_vmloop.f,v 1.3 1998/08/14 19:29:14 grogers Exp $
*  gr_vmloop.f
*
      SUBROUTINE gr_vMLOOP (NCONV, IPAGE, ITYPE, VRSION,
     +                  PAGE, SCREEN, NAME)
* Purpose:  LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT DATA *
* FILE OR INTERACTIVELY), CALCULATES THE CORRECTION VALUES,          *
* PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN.           *
**********************************************************************
*
*  $Log: gr_vmloop.f,v $
*  Revision 1.3  1998/08/14 19:29:14  grogers
*  PR#0, corrected error return on out-of-bounds
*
c Revision 1.2  1998/07/08  22:13:56  grogers
c PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
c
c Revision 1.1  1998/07/07  20:10:24  grogers
c PR#0, initial add of vertcon_lib
c
*
      include 'nadcomm.i'

      INTEGER NCONV, IPAGE, ITYPE
      INTEGER IDLA, IMLA, IDLO, IMLO
      REAL GHT
      real*8 dhpred,xeast
      DOUBLEPRECISION XPT,YPT
      CHARACTER*40 NAME
      LOGICAL PAGE, SCREEN, NOPT, EOF
       save ios
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
c
* set defaults for those variables not used by every format type
* BEGIN THE COMPUTATION LOOP FOR EACH INTERPOLATION
* DO UNTIL END OF FILE OR NO MORE COMPUTATIONS REQUESTED
      NCONV = 0
      NPOINT = 0
  160 CONTINUE
        PAGE = .FALSE.
* GET THE NAME AND LOCATION OF ANOTHER POINT
*GR     CALL vGETPT (NCONV, VRSION, ITYPE, NAME, IDLA, IMLA, SLA, IDLO,
*GR  +              IMLO, SLO, XPT, YPT, EOF, NOPT)
*GR     IF (NOPT) GOTO 160
*GR     IF (EOF) GOTO 9999
      xpt = xvalue
      ypt = yvalue
      errcode = 0

* DO THE INTERPOLATION
* LOOP ONCE FOR EACH COMPUTATION
        xeast=360.0-xpt
        call interp(ypt,xeast,dhpred,ios)
          NCONV = NCONV + 1
        if(ios.ne.0) then
*GR      write(LUOUT,161) ypt,xpt
*GR161    format(' Position out of bounds :',2f10.5)
         errcode = 1
         go to 9999
        endif
          GHT=dhpred*0.001d0
          GHT=vROUND(GHT)
          ght_diff = ght
        NPOINT = NPOINT + 1
c
** WRITE TO OUTPUT FILE AND SCREEN
*GR     CALL vWRTPT (ITYPE, NCONV, NPOINT, VRSION, NAME,
*GR  +              IDLA, IMLA, SLA, IDLO, IMLO, SLO,
*GR  +              GHT,IPAGE, PAGE, SCREEN)
* START THE LOOP AGAIN
*GR   GOTO 160
 9999 RETURN
      END
