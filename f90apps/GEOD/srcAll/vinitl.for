*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vinitl.f,v 1.2 1998/07/08 22:13:59 grogers Exp $
*  vinitl.f
*
      SUBROUTINE vINITL (SCREEN, PAGE, NAME, IPAGE, ITYPE)
*
*  Purpose:  Initialize for VERTCON
***********************************
*
*  $Log: vinitl.f,v $
*  Revision 1.2  1998/07/08 22:13:59  grogers
*  PR#0, chgd GRIDS to vGRIDS, ROUND to vROUND
*
c Revision 1.1  1998/07/07  20:10:36  grogers
c PR#0, initial add of vertcon_lib
c
*
      parameter (MAXAREA=10,NSPACE1=6*MAXAREA,NSPACE2=2*MAXAREA)
      CHARACTER*12 FILES
      CHARACTER*20 B20
      CHARACTER*80 B80
      CHARACTER*80 CARD
      CHARACTER*40 NAME
      CHARACTER*96 B96
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
c
      INTEGER IPAGE, ITYPE
      LOGICAL PAGE, SCREEN
      REAL*4  SPACE1,SPACE2,MARGIN
      COMMON /vCURNT/ B96 
      COMMON /vGRIDS/ FILES(MAXAREA)
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
      common/GSTUFF/ SPACE1(NSPACE1),MARGIN(MAXAREA),SPACE2(NSPACE2)
      EQUIVALENCE(CARD,B96)
* Initialize card variable in common CURNT to blank
      CARD = B80
* Set the logical units for input/output common INOUT
      LUIN  = 5
      LUOUT = 6
*GR   NOUT  = 101
*GR   NIN   = 102
      NOUT  = 1
      NIN   = 2
      LDUMP = LUOUT
*                             INITIALIZE
* Defaults: SCREEN = .TRUE. => send results to screen
*           PAGE = .FALSE.  => don't start a new page in the output file
*           NAME = '     '  => no station name
*           IPAGE = 0       => current output file page number is 0
*           ITYPE = 0       => interactive input of points
      SCREEN = .TRUE.
      PAGE = .FALSE.
      NAME = '        '
      IPAGE = 0
      ITYPE = 0
        FILES(1)='vertconw.94 '
        MARGIN(1)=5.0
        FILES(2)='vertconc.94 '
        MARGIN(2)=5.0
        FILES(3)='vertcone.94 '
        MARGIN(3)=0.0
       DO N=4,MAXAREA
        FILES(N)='            '
       ENDDO
      RETURN
      END
