C     These are all parameters for various values in GKS.  They are
C     are used to represent the various numerical valued flags. 
C     The first set is claimed to be part of the FORTRAN binding 
C     by my source of information for GKS.  The second set is 
C     unique to the current GKS interface.  None of these 
C     parameters relates to the size of arrays or the limits of 
C     numbers.  
C
      INTEGER GBUNDL, GINDIV, GCONDI, GALWAY, GNCLIP, GCLIP,
     A        GHOLLO, GSOLID, GPATTR, GHATCH, GAHNOR, GALEFT,
     B        GACENT, GARITE, GAVNOR, GATOP, GACAP, GAHALF,
     C        GABASE, GABOTT, GRIGHT, GLEFT, GUP, GDOWN,
     D        GSTRP, GCHARP, GSTRKP
C
      PARAMETER(GBUNDL=0,GINDIV=1,GCONDI=0,GALWAY=1,GNCLIP=0,GCLIP=1,
     A          GHOLLO=0,GSOLID=1,GPATTR=2,GHATCH=3,GAHNOR=0,GALEFT=1,
     B          GACENT=2,GARITE=3,GAVNOR=0,GATOP=1, GACAP=2, GAHALF=3,
     C          GABASE=4,GABOTT=5,GRIGHT=0,GLEFT=1, GUP=2,   GDOWN=3,
     D          GSTRP=0, GCHARP=1,GSTRKP=2)
C
C     These are unique to the current interface.  I have violated the 
C     6 character limit for names because this system only makes sense
C     with the SVS compiler and that compiler allows long names. 
C
      INTEGER FULL,
     A   BLACK, BLUE, GREEN, CYAN, RED, MAGENTA, YELLOW, WHITE, 
     B   DARKGRAY, LIGHTBLUE, LIGHTGREEN, LIGHTCYAN, LIGHTRED,
     C   LIGHTMAGENTA, LIGHTYELLOW, BRIGHTWHITE, DOT, PLUS,
     D   ASTERISK, BIGOH, BIGX, CIRCLE, TRIANGLE, SQUARE, 
     E   DIAMOND, HOURGLASS, OCTAGON, INVERTTRIANGLE, 
     F   PENTAGON, HOLLOW, SOLID, XHATCH, HATCHED, YES, NO,
     G   HAT00, HAT45, HAT90, HAT135, HATSQR, HATSQR45,
     H   HATSQR45CUTV,HATSQR45CUTH
C
      PARAMETER(FULL=0,
     A   BLACK=0,     BLUE=168,  GREEN=104, CYAN=136,     RED=40,
     A   MAGENTA=200, YELLOW=56, WHITE=216, DARKGRAY=248, LIGHTBLUE=120,
     B   LIGHTGREEN=88, LIGHTCYAN=112, LIGHTRED=16, LIGHTMAGENTA=176,
     C   LIGHTYELLOW=48, BRIGHTWHITE=208, DOT=1, PLUS=2,
     D   ASTERISK=3, BIGOH=4, BIGX=5, CIRCLE=0, TRIANGLE=-1, 
     E   SQUARE=-2, DIAMOND=-3, HOURGLASS=-4, OCTAGON=-5,
     F   INVERTTRIANGLE=-6, PENTAGON=-7,
     G   HOLLOW=0, SOLID=1, XHATCH=2, HATCHED=3, YES=1, NO=0,
     H   HAT00=-1, HAT45=-2, HAT90=-3, HAT135=-4, HATSQR=-5, 
     I   HATSQR45=-6, HATSQR45CUTV=-7, HATSQR45CUTH=-8)


