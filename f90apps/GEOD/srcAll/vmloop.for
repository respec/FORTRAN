*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vmloop.f,v 1.2 1998/07/08 22:14:00 grogers Exp $
*  vmloop.f
*
      SUBROUTINE vMLOOP (NCONV, IPAGE, ITYPE, VRSION,
     +                  PAGE, SCREEN, NAME)
*
* Purpose: LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT DATA *
* FILE OR INTERACTIVELY), CALCULATES THE CORRECTION VALUES,          *
* PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN.           *
**********************************************************************
*
*  $Log: vmloop.f,v $
*  Revision 1.2  1998/07/08 22:14:00  grogers
*  PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
*
c Revision 1.1  1998/07/07  20:10:37  grogers
c PR#0, initial add of vertcon_lib
c
*
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
        CALL vGETPT (NCONV, VRSION, ITYPE, NAME, IDLA, IMLA, SLA, IDLO,
     +              IMLO, SLO, XPT, YPT, EOF, NOPT)
        IF (NOPT) GOTO 160
        IF (EOF) GOTO 9999
* DO THE INTERPOLATION
* LOOP ONCE FOR EACH COMPUTATION
        xeast=360.0-xpt
        call interp(ypt,xeast,dhpred,ios)
          NCONV = NCONV + 1
        if(ios.ne.0) then
         write(LUOUT,161) ypt,xpt
  161    format(' Position out of bounds :',2f10.5)
         go to 160
        endif
          GHT=dhpred*0.001d0
          GHT=vROUND(GHT)
        NPOINT = NPOINT + 1
c
** WRITE TO OUTPUT FILE AND SCREEN
        CALL vWRTPT (ITYPE, NCONV, NPOINT, VRSION, NAME,
     +              IDLA, IMLA, SLA, IDLO, IMLO, SLO,
     +              GHT,IPAGE, PAGE, SCREEN)
* START THE LOOP AGAIN
      GOTO 160
 9999 RETURN
      END
