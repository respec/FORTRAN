C
C     mrktbl.f
C
      SUBROUTINE MRKTBL
     I                 (ISTART,IEND)
C
C     + + + PURPOSE + + +
C     This routine will plot the range of symbols specified in columns
C     of twenty.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS, markersubs.f
C     LANGUAGE: Fortran 77
C      VERSION: June 7, 1993
C
C     + + + KEYWORDS + + +
C     marker, GKS, symbol
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER ISTART, IEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISTART - Integer index of first marker symbol to plot
C     IEND   - Integer index of last consecutive marker symbol to plot
C
C     + + + LOCAL VARIABLES + + +
      INTEGER i, j, k, ibeg, ilst, nlst
      REAL yy, xx, aa, xinc, chsiz, chsizh, yinc, chsiz2, y2
      CHARACTER cnum*3
C
C     + + + EXTERNALS + + +
      EXTERNAL GSCHH, GMKSTR, GMARK1
C
C     + + + EXTERNAL DEFINITIONS + + +
C     GSCHH  - GKS routine to set character size
C     GMKSTR - markersubs routine to plot a text string at a location
C     GMARK1 - markersubs routine to plot a symbol at a location
C
C     + + + END SPECIFICATIONS + + +
C
      ibeg = ISTART
      ilst = IEND
      nlst = ibeg + 19
      chsiz = 0.3
      chsiz2 = 0.2
      chsizh = chsiz*0.5
      xinc = chsiz2*3.1 + chsiz
      yinc = chsiz*1.8
      CALL GSCHH(chsiz)
      yy = 11.5
      aa = 0.0
      DO 100 i = ibeg, nlst
        xx = 0.1
        y2 = yy - chsizh
        k = i
        DO 50 j = k, ilst, 20
          WRITE (cnum,'(I3)') j
          CALL GMKSTR(cnum,3,chsiz2,xx,y2,aa)
          xx = xx + xinc
          CALL GMARK1(j,chsiz,xx,yy)
          xx = xx + chsiz
 50     CONTINUE
        yy = yy - yinc
 100  CONTINUE
C
      RETURN
      END
C
C     gmark1.f
C
      SUBROUTINE GMARK1
     I                 (MARK,HGT,PX,PY)
C
C     + + + PURPOSE + + +
C     This routine is used to place the specified marker of specified
C     height at the specified location.  Valid marker types are referenced
C     by an integer number from 1-31 (for centered marker) or 32-133 (for
C     centered character, 32-126=ASCII character, space=32,0=48,A=65,a=97).
C     Valid marker types are defined in routines GPUTMK.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS
C     LANGUAGE: Fortran 77
C      VERSION: May 26, 1993
C
C     + + + KEYWORDS + + +
C     marker, GKS, symbol
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MARK
      REAL HGT, PX, PY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MARK   - Integer index of marker symbol.  Markers 1-5 are the
C              standard GKS markers.
C     HGT    - Height of marker in world coordinates, the width of
C              marker is assumed to be the same.
C     PX     - X location in world coordinates of center of marker.
C     PY     - Y location in world coordinates of center of marker.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER isymb(1), nn
      REAL pxa(1), pya(1), ang, height
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL GPUTMK
C
C     + + + END SPECIFICATIONS + + +
C
      IF ( MARK.NE.0 ) THEN
        isymb(1) = MARK
        height = ABS(HGT)
        pxa(1) = PX
        pya(1) = PY
        nn = 1
        ang = 0.0
        CALL GPUTMK(isymb,height,pxa,pya,nn,ang)
      ENDIF
C
      RETURN
      END
C
C     gmarkn.f
C
      SUBROUTINE GMARKN
     I                 (MARK,HGT,PX,PY,N,ANGLE)
C
C     + + + PURPOSE + + +
C     This routine is used to place the specified marker(s) of specified
C     height at the specified location(s) at the specified angle of rotation.
C     GMARKN can place a "curve" of markers at the specified locations in
C     arrays PX and PY.  Valid marker types are referenced by an integer
C     number from 1-31 (for centered marker) or 32-133 (for centered
C     character, 32-126=ASCII character, space=32,0=48,A=65,a=97).  Valid
C     marker types are defined in routines GPUTMK.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS
C     LANGUAGE: Fortran 77
C      VERSION: May 26, 1993
C
C     + + + KEYWORDS + + +
C     marker, curve, rotate, GKS, symbol
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MARK, N
      REAL HGT, PX(N), PY(N), ANGLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MARK   - Integer index of marker symbol.  Markers 1-5 are the
C              standard GKS markers.
C     HGT    - Height of marker in world coordinates, the width of
C              marker is assumed to be the same.
C     PX(N)  - Array of X location(s) in world coordinates of center of
C              marker (REAL).
C     PY(N)  - Array of Y location(s) in world coordinates of center of
C              marker (REAL).
C     N      - Number of markers to produce.
C     ANGLE  - Angle of rotation of marker in clockwise direction (0.0
C              is normal orientation).
C
C     + + + LOCAL VARIABLES + + +
      INTEGER isymb(1), nn
      REAL height, ang
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL GPUTMK
C
C     + + + END SPECIFICATIONS + + +
C
      IF ( MARK.NE.0 ) THEN
        nn = N
        height = ABS(HGT)
        ang = ANGLE
        isymb(1) = MARK
        CALL GPUTMK(isymb,height,PX,PY,nn,ang)
      ENDIF
C
      RETURN
      END
C
C     gmarks.f
C
      SUBROUTINE GMARKS
     I                 (MARK,HGT,PX,PY,N,ANGLE)
C
C     + + + PURPOSE + + +
C     This routine is used to place the specified marker(s) of specified
C     height at the specified location(s) at the specified angle of rotation.
C     GMARKS can place a "curve" of markers, as specified in the array MARK,
C     at the specified locations in arrays PX and PY.  For example, a different
C     marker type can be placed at each location.  Valid marker types are
C     referenced by an integer number from 1-31 (for centered marker) or 32-128
C     (for centered character, 32-126=ASCII character, space=32,0=48,A=65,a=97).
C     Valid marker types are defined in routine GPUTMK.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS
C     LANGUAGE: Fortran 77
C      VERSION: May 26, 1993
C
C     + + + KEYWORDS + + +
C     marker, curve, rotate, GKS, symbol
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER N
      INTEGER MARK(N)
      REAL HGT, PX(N), PY(N), ANGLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MARK(N)- Array of integer indices of the marker symbol to be placed
C              at each location.  Markers 1-5 are the standard GKS markers.
C     HGT    - Height of marker in world coordinates, the width of
C              marker is assumed to be the same.
C     PX(N)  - Array of X location(s) in world coordinates of center of
C              marker (REAL).
C     PY(N)  - Array of Y location(s) in world coordinates of center of
C              marker (REAL).
C     N      - Number of markers to produce.
C     ANGLE  - Angle of rotation of marker in clockwise direction (0.0
C              is normal orientation).
C
C     + + + LOCAL VARIABLES + + +
      INTEGER nn
      REAL height, ang
C
C     + + + INTRINSICS + + +
      INTRINSIC SIGN
C
C     + + + EXTERNALS + + +
      EXTERNAL GPUTMK
C
C     + + + END SPECIFICATIONS + + +
C
      nn = N
      height = SIGN(HGT,-1.0)
      ang = ANGLE
C
      CALL GPUTMK(MARK,height,PX,PY,nn,ang)
C
      RETURN
      END
C
C     gmkstr.f
C
      SUBROUTINE GMKSTR
     I                 (STRING,NCHAR,HGT,PX,PY,ANGLE)
C
C     + + + PURPOSE + + +
C     This routine is used to place the specified string of specified
C     height at the specified location using the software generated
C     characters.  Valid characters are defined in routines GPUTMK.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS
C     LANGUAGE: Fortran 77
C      VERSION: May 25, 1993
C
C     + + + KEYWORDS + + +
C     string, GKS, character
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*(*) STRING
      INTEGER NCHAR
      REAL HGT, PX, PY, ANGLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - Character string to plot.
C     NCHAR  - Number of character in string to plot.
C     HGT    - Height of character(s) in world coordinates, the width is
C              assumed to be the same.
C     PX     - X location in world coordinates of lower-left corner of marker
C     PY     - Y location in world coordinates of lower-left corner of marker
C     ANGLE  - Angle of rotation of marker in clockwise direction (0.0
C              is normal orientation).
C
C     + + + LOCAL VARIABLES + + +
      INTEGER isymb(1), nn, i
      REAL pxa(1), pya(1), ang, height, c, s, xm, ym
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS, COS, SIN, ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL GPUTMK
C
C     + + + END SPECIFICATIONS + + +
C
      nn = NCHAR
      height = ABS(HGT)
      ang = ANGLE*0.0174533
      c = COS(ang)
      s = SIN(ang)
      xm = height*c
      ym = height*s
      pxa(1) = PX + height*0.5
      pya(1) = PY + height*0.5
      ang = ANGLE
      DO 100 i = 1, nn
        isymb(1) = ICHAR(STRING(i:i))
        IF ( isymb(1).NE.0 ) CALL GPUTMK(isymb,height,pxa,pya,1,ang)
        pxa(1) = pxa(1) + xm
        pya(1) = pya(1) + ym
 100  CONTINUE
C
C
      RETURN
      END
C
C     gputmk.f
C
      SUBROUTINE GPUTMK
     I                 (ISYM,HGT,PX,PY,NN,ANGLE)
C
C     + + + PURPOSE + + +
C     Software character generator used to produce centered coordinate
C     marker(s) or character(s) at the specified height, location(s),
C     and angle.  Valid types are referenced by the index numbers 1-31 and
C     32-128 (32-126 refer to the ASCII characater set).  This routine is
C     called by subroutine GMARKN, GMARK1, and GMARKS.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM: requires GKS
C     LANGUAGE: Fortran 77
C      VERSION: May 26, 1993
C     REVISION: ACW - July 7,1993
C
C     + + + KEYWORDS + + +
C     marker, character, GKS, symbol, angle, curve
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NN, ISYM(*)
      REAL HGT, PX(NN), PY(NN), ANGLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISYM(N)- Array of integer indices of marker symbol(s).
C              Markers 1-5 are the standard GKS markers.
C              ISYM must be dimensioned at least N, if HGT
C              is input as <0.0 (marker specified for each
C              coordinate location).  Otherwise, it can be
C              a single element array.
C              <0 = -ER symbol generated
C              0 = no character generated
C              1 = dot (.)
C              2 = plus (+)
C              3 = asterisk (*)
C              4 = circle
C              5 = cross (X)
C              6 = box
C              7 = triangle
C              8 = diamond
C              9 = octagon
C              10 = triangle pointed down
C              11 = water-surface symbol
C              12 = star
C              13 = open up-arrow
C              14 = spider
C              15 = octagon window (octagon + plus)
C              16 = crossed box (box + X)
C              17 = window (box + plus)
C              18 = crossed diamond (diamond + X)
C              19 = diamond window (diamond + plus)
C              20 = vertical line
C              21 = filled dot
C              22 = filled circle
C              23 = filled box
C              24 = filled triangle
C              25 = filled diamond
C              26 = filled octagon
C              27 = filled triangle pointed down
C              28 = filled water-surface symbol
C              29 = filled star
C              30 = filled up arrow
C              31 = filled spider
C              32 = space
C              33-126 = ASCII character
C              127    = less than or equal to sign
C              128    = greater than or equal to sign
C              129    = not equal sign
C              130    = division sign
C              131    = multiplication sign
C              132    = plus or minus sign
C              133    = relational sign
C              134    = approximately equal to sign
C              135    = open arrow to the right
C              136    = double sided arrow
C              137    = double sided open arrow
C              138    = three dots, triangle
C              139    = side-ways '8'
C              140    = fish
C              141    = square root
C              142    = North arrow
C              143    = three dots
C              144    = cents sign
C              145    = right arrow
C              146    = alpha
C              147    = Beta
C              148    = chi
C              149    = delta
C              150    = Epsilon
C              151    = phi
C              152    = gamma
C              153    = eta
C              154    = kappa
C              155    = lamda
C              156    = mu
C              157    = pi
C              158    = theta
C              159    = rho
C              160    = sigma
C              161    = Tau
C              162    = nu
C              163    = omega
C              164    = zi
C              165    = Psi
C              166    = zeta
C              167    = Delta
C              168    = Sigma
C              169    = Phi
C              170    = Gamma
C              171    = Pi
C              172    = Omega
C              173    = long curved scroll
C              174    = angled phi
C              175    = one-sixth
C              176    = one-forth
C              177    = one-third
C              178    = one-half
C              179    = two-thirds
C              180    = three-forths
C              181    = flagged building
C              182    = church
C              183    = USGS symbol
C              184    = up arrow with center line
C              185    = pod
C              186    = down, then left arrow
C              187    = down, then right arrow
C              188    = up, then right arrow
C              189    = up, then left arrow
C              190    = Y
C              191    = hour glass
C              192    = up arrow
C              193-199 = blank for now
C              200    = crossed diamond
C              201    = crossed diamond with top square filled
C              202    = crossed diamond with right square filled
C              203    = crossed diamond with bottom square filled
C              204    = crossed diamond with left square filled
C              205    = crossed diamond with top & right squares filled
C              206    = crossed diamond with right & bottom squares filled
C              207    = crossed diamond with left & bottom squares filled
C              208    = crossed diamond with left & top squares filled
C              209    = crossed diamond with top & bottom squares filled
C              210    = crossed diamond with left & right squares filled
C              211    = crossed diamond filled with top square open
C              212    = crossed diamond filled with right square open
C              213    = crossed diamond filled with bottom square open
C              214    = crossed diamond filled with left square open
C              >214  = +ER character generated
C
C     HGT    - Height of marker in world coordinates, the width of
C              marker is assumed to be the same.  If HGT<0.0, then the
C              ISYM array must be dimensioned at least N and contain a
C              marker index number for each coordinate pair (PX and PY).
C     PX(N)  - Array of X location(s) in world coordinates of center of
C              marker (REAL).
C     PY(N)  - Array of Y location(s) in world coordinates of center of
C              marker (REAL).
C     N      - Number of markers to produce.
C     ANGLE  - Angle of rotation of marker in clockwise direction (0.0
C              is normal orientation).
C
C     + + + PARAMETERS + + +
      INTEGER NUMARK, NUCHAR, CHRDEF, NSEG, ICHDIM
      PARAMETER (NUMARK=20,NUCHAR=181,CHRDEF=61,NSEG=8,ICHDIM=27)
C
C     + + + PARAMETER DEFINITIONS + + +
C     NUMARK - Number of defined markers
C     NUCHAR - Number of defined character.
C     CHRDEF - Maximum number of line segments defining a character.
C     NSEG   - Maximum number of connected lines defining each character.
C     ICHDIM - Dimension of arrays defining each character.
C
      INTEGER icsym(ICHDIM,NUCHAR)
      INTEGER icmrk(15,NUMARK), circle(26), errneg(18), errpos(20), 
     $        usgs(62)
      SAVE icsym, icmrk, circle, errneg, errpos, usgs
C
C     + + + LOCAL VARIABLES + + +
      INTEGER i, j, k, num, ipt, ix, is(NSEG), ierr, ltype, np, ns, 
     $        ifill, mark, ii, ifaclr, ipmclr, iplclr
      REAL x(CHRDEF), y(CHRDEF), cx(CHRDEF,NSEG), cy(CHRDEF,NSEG), ang, 
     $     height, width, angrad, cosang, sinang, xm, ym, xx, yy, hgthlf
      LOGICAL lfill
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS, IABS, FLOAT, IFIX, COS, SIN
C
C     + + + EXTERNALS + + +
      EXTERNAL GDGROT, GQLN, GQLWSC, GSLN, GSLWSC, GQFAIS, GSFAIS, GPL, 
     $         GFA, GQFACI, GSFACI, GQPMCI, GSPLCI, GQPLCI
C     GKS routines:
C         GQLN   = inquire line type
C         GSLN   = set line type
C         GQLWSC = inquire line width scale factor
C         GSLWSC = set line width scale factor
C         GQFAIS = inquire fill area interior style
C         GSFAIS = set fill area interior style
C         GQFACI = inquire fill area colour index
C         GSFACI = set fill area color index
C         GQPMCI = inquire polymarker color index
C         GQPMCI = inquire polyline color index
C         GSPLCI = set polyline color index
C         GPL    = draw a sequence of connected straight lines
C         GFA    = fill a sequence of connected straight lines
C
C     + + + DATA INITIALIZATIONS + + +
      DATA (icsym(i,1),i=1,ICHDIM)/10, 275, 325, 327, 277, 275, 327, 
     $      277, 325, 931, 324, 16*0/
      DATA (icsym(i,2),i=1,ICHDIM)/4, 249, 268, 999, 393, 22*0/
      DATA (icsym(i,3),i=1,ICHDIM)/8, 299, 175, 950, 449, 791, 491, 759, 
     $      459, 18*0/
      DATA (icsym(i,4),i=1,ICHDIM)/24, 519, 471, 372, 272, 171, 119, 92, 
     $      90, 113, 162, 462, 511, 534, 532, 505, 453, 352, 252, 153, 
     $      105, 874, 225, 1000, 399, 0, 0/
      DATA (icsym(i,5),i=1,ICHDIM)/20, 499, 50, 920, 272, 223, 172, 145, 
     $      168, 217, 268, 295, 904, 302, 351, 402, 429, 406, 357, 306, 
     $      279, 6*0/
      DATA (icsym(i,6),i=1,ICHDIM)/22, 525, 264, 218, 220, 248, 274, 
     $      324, 348, 370, 368, 341, 264, 212, 158, 130, 128, 151, 250, 
     $      300, 351, 378, 486, 4*0/
      DATA (icsym(i,7),i=1,ICHDIM)/4, 374, 267, 349, 374, 22*0/
      DATA (icsym(i,8),i=1,ICHDIM)/8, 374, 321, 293, 264, 260, 281, 303, 
     $      350, 18*0/
      DATA (icsym(i,9),i=1,ICHDIM)/8, 274, 321, 343, 364, 360, 331, 303, 
     $      275, 18*0/
      DATA (icsym(i,10),i=1,ICHDIM)/6, 322, 302, 707, 542, 717, 532, 
     $      20*0/
      DATA (icsym(i,11),i=1,ICHDIM)/4, 87, 537, 928, 321, 22*0/
      DATA (icsym(i,12),i=1,ICHDIM)/10, 329, 279, 281, 331, 329, 281, 
     $      331, 279, 329, 275, 16*0/
      DATA (icsym(i,13),i=1,ICHDIM)/2, 162, 462, 24*0/
      DATA (icsym(i,14),i=1,ICHDIM)/10, 325, 275, 277, 327, 325, 277, 
     $      327, 325, 300, 302, 16*0/
      DATA (icsym(i,15),i=1,ICHDIM)/2, 75, 549, 24*0/
      DATA (icsym(i,16),i=1,ICHDIM)/23, 75, 549, 1170, 497, 448, 374, 
     $      274, 198, 147, 95, 42, 32, 79, 127, 176, 250, 350, 426, 477, 
     $      529, 582, 592, 545, 3*0/
      DATA (icsym(i,17),i=1,ICHDIM)/5, 220, 324, 300, 400, 200, 21*0/
      DATA (icsym(i,18),i=1,ICHDIM)/12, 94, 147, 198, 274, 374, 448, 
     $      497, 544, 542, 514, 75, 525, 14*0/
      DATA (icsym(i,19),i=1,ICHDIM)/20, 94, 147, 198, 274, 374, 497, 
     $      544, 542, 514, 387, 212, 387, 510, 532, 530, 477, 350, 250, 
     $      127, 80, 6*0/
      DATA (icsym(i,20),i=1,ICHDIM)/4, 350, 374, 85, 535, 22*0/
      DATA (icsym(i,21),i=1,ICHDIM)/16, 549, 99, 88, 215, 389, 463, 511, 
     $      533, 530, 477, 426, 350, 250, 176, 127, 80, 10*0/
      DATA (icsym(i,22),i=1,ICHDIM)/20, 83, 161, 263, 387, 510, 532, 
     $      530, 452, 350, 250, 151, 80, 94, 147, 198, 274, 374, 448, 
     $      497, 544, 6*0/
      DATA (icsym(i,23),i=1,ICHDIM)/4, 96, 99, 549, 125, 22*0/
      DATA (icsym(i,24),i=1,ICHDIM)/23, 94, 147, 198, 274, 374, 448, 
     $      497, 544, 542, 489, 135, 82, 80, 127, 250, 350, 477, 530, 
     $      532, 485, 139, 92, 94, 3*0/
      DATA (icsym(i,25),i=1,ICHDIM)/22, 541, 514, 437, 336, 286, 187, 
     $      114, 92, 94, 147, 198, 274, 374, 448, 497, 544, 530, 477, 
     $      350, 250, 127, 80, 4*0/
      DATA (icsym(i,26),i=1,ICHDIM)/16, 279, 281, 331, 329, 279, 331, 
     $      281, 329, 918, 295, 345, 343, 293, 345, 295, 343, 10*0/
      DATA (icsym(i,27),i=1,ICHDIM)/17, 279, 281, 331, 329, 279, 331, 
     $      281, 329, 275, 918, 295, 345, 343, 293, 345, 295, 343, 9*0/
      DATA (icsym(i,28),i=1,ICHDIM)/3, 528, 87, 546, 23*0/
      DATA (icsym(i,29),i=1,ICHDIM)/4, 83, 533, 716, 541, 22*0/
      DATA (icsym(i,30),i=1,ICHDIM)/3, 96, 537, 78, 23*0/
      DATA (icsym(i,31),i=1,ICHDIM)/18, 275, 325, 327, 277, 275, 719, 
     $      147, 198, 274, 374, 448, 497, 544, 542, 514, 387, 312, 305, 
     $      8*0/
      DATA (icsym(i,32),i=1,ICHDIM)/26, 409, 417, 415, 367, 267, 215, 
     $      209, 257, 357, 409, 407, 506, 560, 564, 518, 421, 372, 272, 
     $      221, 118, 64, 60, 106, 203, 252, 352/
      DATA (icsym(i,33),i=1,ICHDIM)/8, 75, 94, 199, 449, 544, 525, 712, 
     $      537, 18*0/
      DATA (icsym(i,34),i=1,ICHDIM)/16, 75, 99, 399, 474, 547, 542, 465, 
     $      389, 89, 389, 463, 536, 528, 451, 375, 75, 10*0/
      DATA (icsym(i,35),i=1,ICHDIM)/16, 544, 497, 448, 374, 274, 198, 
     $      147, 94, 80, 127, 176, 250, 350, 426, 477, 530, 10*0/
      DATA (icsym(i,36),i=1,ICHDIM)/11, 75, 99, 374, 448, 497, 544, 530, 
     $      477, 426, 350, 75, 15*0/
      DATA (icsym(i,37),i=1,ICHDIM)/6, 525, 75, 99, 549, 712, 412, 20*0/
      DATA (icsym(i,38),i=1,ICHDIM)/5, 75, 99, 549, 712, 412, 21*0/
      DATA (icsym(i,39),i=1,ICHDIM)/18, 544, 497, 448, 374, 274, 198, 
     $      147, 94, 80, 127, 176, 250, 350, 426, 477, 530, 535, 385, 
     $      8*0/
      DATA (icsym(i,40),i=1,ICHDIM)/6, 549, 525, 700, 99, 712, 537, 
     $      20*0/
      DATA (icsym(i,41),i=1,ICHDIM)/6, 300, 324, 799, 474, 775, 450, 
     $      20*0/
      DATA (icsym(i,42),i=1,ICHDIM)/9, 549, 530, 477, 426, 350, 250, 
     $      176, 127, 80, 17*0/
      DATA (icsym(i,43),i=1,ICHDIM)/6, 75, 99, 710, 549, 839, 525, 20*0/
      DATA (icsym(i,44),i=1,ICHDIM)/3, 525, 75, 99, 23*0/
      DATA (icsym(i,45),i=1,ICHDIM)/5, 75, 99, 314, 549, 525, 21*0/
      DATA (icsym(i,46),i=1,ICHDIM)/4, 75, 99, 525, 549, 22*0/
      DATA (icsym(i,47),i=1,ICHDIM)/17, 544, 497, 448, 374, 274, 198, 
     $      147, 94, 80, 127, 176, 250, 350, 426, 477, 530, 544, 9*0/
      DATA (icsym(i,48),i=1,ICHDIM)/9, 75, 99, 374, 473, 546, 540, 463, 
     $      387, 87, 17*0/
      DATA (icsym(i,49),i=1,ICHDIM)/19, 544, 497, 448, 374, 274, 198, 
     $      147, 94, 80, 127, 176, 250, 350, 426, 477, 530, 544, 1006, 
     $      525, 7*0/
      DATA (icsym(i,50),i=1,ICHDIM)/11, 75, 99, 374, 473, 546, 540, 463, 
     $      387, 87, 937, 525, 15*0/
      DATA (icsym(i,51),i=1,ICHDIM)/20, 544, 497, 448, 374, 274, 198, 
     $      147, 94, 90, 138, 486, 534, 530, 477, 426, 350, 250, 176, 
     $      127, 80, 6*0/
      DATA (icsym(i,52),i=1,ICHDIM)/4, 549, 99, 925, 324, 22*0/
      DATA (icsym(i,53),i=1,ICHDIM)/10, 549, 530, 477, 426, 350, 250, 
     $      176, 127, 80, 99, 16*0/
      DATA (icsym(i,54),i=1,ICHDIM)/3, 549, 300, 99, 23*0/
      DATA (icsym(i,55),i=1,ICHDIM)/5, 549, 425, 310, 175, 99, 21*0/
      DATA (icsym(i,56),i=1,ICHDIM)/4, 99, 525, 700, 549, 22*0/
      DATA (icsym(i,57),i=1,ICHDIM)/5, 99, 314, 549, 925, 314, 21*0/
      DATA (icsym(i,58),i=1,ICHDIM)/4, 99, 549, 75, 525, 22*0/
      DATA (icsym(i,59),i=1,ICHDIM)/4, 374, 274, 250, 350, 22*0/
      DATA (icsym(i,60),i=1,ICHDIM)/2, 99, 525, 24*0/
      DATA (icsym(i,61),i=1,ICHDIM)/4, 250, 350, 374, 274, 22*0/
      DATA (icsym(i,62),i=1,ICHDIM)/5, 300, 324, 715, 324, 540, 21*0/
      DATA (icsym(i,63),i=1,ICHDIM)/2, 0, 600, 24*0/
      DATA (icsym(i,64),i=1,ICHDIM)/4, 274, 367, 299, 274, 22*0/
      DATA (icsym(i,65),i=1,ICHDIM)/18, 500, 516, 504, 452, 401, 325, 
     $      275, 201, 129, 107, 109, 137, 215, 291, 341, 415, 464, 512, 
     $      8*0/
      DATA (icsym(i,66),i=1,ICHDIM)/17, 124, 100, 103, 176, 275, 325, 
     $      401, 479, 507, 509, 487, 415, 341, 291, 215, 137, 109, 9*0/
      DATA (icsym(i,67),i=1,ICHDIM)/16, 504, 452, 426, 325, 275, 201, 
     $      129, 107, 109, 137, 215, 291, 341, 440, 464, 512, 10*0/
      DATA (icsym(i,68),i=1,ICHDIM)/18, 524, 500, 504, 452, 426, 325, 
     $      275, 201, 129, 107, 109, 137, 215, 291, 341, 415, 487, 512, 
     $      8*0/
      DATA (icsym(i,69),i=1,ICHDIM)/17, 504, 452, 426, 325, 275, 201, 
     $      129, 107, 109, 137, 215, 291, 341, 415, 487, 509, 109, 9*0/
      DATA (icsym(i,70),i=1,ICHDIM)/9, 300, 320, 347, 373, 424, 474, 
     $      522, 840, 415, 17*0/
      DATA (icsym(i,71),i=1,ICHDIM)/21, -242, -288, -286, -257, -204, 
     $      404, 329, 381, 411, 413, 391, 319, 245, 195, 119, 41, 13, 
     $      11, 33, 105, 129, 5*0/
      DATA (icsym(i,72),i=1,ICHDIM)/11, 124, 100, 109, 163, 215, 291, 
     $      341, 415, 463, 509, 500, 15*0/
      DATA (icsym(i,73),i=1,ICHDIM)/10, 300, 317, 924, 349, 347, 297, 
     $      299, 347, 349, 297, 16*0/
      DATA (icsym(i,74),i=1,ICHDIM)/14, 511, 513, 463, 461, 511, 463, 
     $      461, 513, 962, -212, -263, -290, -291, -269, 12*0/
      DATA (icsym(i,75),i=1,ICHDIM)/6, 124, 100, 107, 518, 886, 500, 
     $      20*0/
      DATA (icsym(i,76),i=1,ICHDIM)/2, 324, 300, 24*0/
      DATA (icsym(i,77),i=1,ICHDIM)/16, 75, 91, 87, 140, 191, 241, 290, 
     $      313, 300, 313, 365, 391, 441, 490, 537, 525, 10*0/
      DATA (icsym(i,78),i=1,ICHDIM)/10, 125, 141, 137, 190, 241, 391, 
     $      440, 488, 511, 500, 16*0/
      DATA (icsym(i,79),i=1,ICHDIM)/17, 341, 291, 215, 137, 109, 107, 
     $      129, 201, 275, 325, 401, 479, 507, 509, 487, 415, 341, 9*0/
      DATA (icsym(i,80),i=1,ICHDIM)/17, -295, 420, 345, 369, 392, 413, 
     $      411, 383, 305, 229, 179, 105, 33, 11, 13, 42, 120, 9*0/
      DATA (icsym(i,81),i=1,ICHDIM)/17, -279, 404, 329, 381, 411, 413, 
     $      391, 319, 245, 195, 119, 41, 13, 11, 33, 105, 129, 9*0/
      DATA (icsym(i,82),i=1,ICHDIM)/8, 200, 216, 213, 265, 341, 391, 
     $      465, 513, 18*0/
      DATA (icsym(i,83),i=1,ICHDIM)/16, 513, 440, 341, 291, 190, 113, 
     $      111, 159, 457, 505, 503, 426, 325, 275, 176, 103, 10*0/
      DATA (icsym(i,84),i=1,ICHDIM)/8, 324, 303, 326, 375, 400, 477, 
     $      790, 465, 18*0/
      DATA (icsym(i,85),i=1,ICHDIM)/11, 516, 500, 504, 452, 401, 325, 
     $      275, 201, 152, 104, 116, 15*0/
      DATA (icsym(i,86),i=1,ICHDIM)/3, 116, 300, 516, 23*0/
      DATA (icsym(i,87),i=1,ICHDIM)/5, 66, 175, 316, 425, 566, 21*0/
      DATA (icsym(i,88),i=1,ICHDIM)/4, 100, 516, 741, 500, 22*0/
      DATA (icsym(i,89),i=1,ICHDIM)/9, 420, 12, 404, 12, -216, -268, 
     $      -295, -297, -274, 17*0/
      DATA (icsym(i,90),i=1,ICHDIM)/4, 116, 516, 100, 500, 22*0/
      DATA (icsym(i,91),i=1,ICHDIM)/19, 399, 374, 323, 297, 270, 268, 
     $      291, 339, 338, 262, 336, 335, 283, 256, 254, 277, 301, 350, 
     $      375, 7*0/
      DATA (icsym(i,92),i=1,ICHDIM)/4, 412, 112, 1237, -212, 22*0/
      DATA (icsym(i,93),i=1,ICHDIM)/19, 225, 250, 301, 327, 354, 356, 
     $      333, 285, 286, 362, 288, 289, 341, 367, 369, 347, 323, 274, 
     $      249, 7*0/
      DATA (icsym(i,94),i=1,ICHDIM)/12, 107, 109, 135, 186, 236, 284, 
     $      332, 380, 430, 481, 507, 509, 14*0/
      DATA (icsym(i,95),i=1,ICHDIM)/5, 530, 89, 548, 701, 526, 21*0/
      DATA (icsym(i,96),i=1,ICHDIM)/5, 80, 539, 98, 701, 526, 21*0/
      DATA (icsym(i,97),i=1,ICHDIM)/6, 83, 533, 716, 541, 851, 423, 
     $      20*0/
      DATA (icsym(i,98),i=1,ICHDIM)/18, 278, 280, 330, 328, 278, 330, 
     $      280, 328, 919, 296, 346, 344, 294, 346, 296, 344, 737, 512, 
     $      8*0/
      DATA (icsym(i,99),i=1,ICHDIM)/4, 104, 520, 745, 504, 22*0/
      DATA (icsym(i,100),i=1,ICHDIM)/6, 324, 306, 715, 540, 701, 526, 
     $      20*0/
      DATA (icsym(i,101),i=1,ICHDIM)/6, 22, 622, 637, 612, 627, 602, 
     $      20*0/
      DATA (icsym(i,102),i=1,ICHDIM)/16, 117, 119, 145, 196, 246, 294, 
     $      342, 390, 440, 491, 517, 519, 736, 511, 730, 505, 10*0/
      DATA (icsym(i,103),i=1,ICHDIM)/9, 6, 481, 350, 481, 612, 493, 374, 
     $      493, 18, 17*0/
      DATA (icsym(i,104),i=1,ICHDIM)/8, 106, 12, 118, 12, 612, 518, 612, 
     $      506, 18*0/
      DATA (icsym(i,105),i=1,ICHDIM)/10, 153, 12, 171, 65, 565, 471, 
     $      612, 453, 559, 59, 16*0/
      DATA (icsym(i,106),i=1,ICHDIM)/24, 154, 156, 206, 204, 154, 206, 
     $      156, 204, 1029, 406, 456, 454, 404, 456, 406, 454, 918, 295, 
     $      345, 343, 293, 345, 295, 343, 2*0/
      DATA (icsym(i,107),i=1,ICHDIM)/23, 175, 125, 76, 31, 8, 10, 37, 
     $      92, 143, 193, 309, 425, 475, 526, 581, 608, 610, 587, 542, 
     $      493, 443, 309, 175, 3*0/
      DATA (icsym(i,108),i=1,ICHDIM)/22, 600, 450, 402, 359, 337, 292, 
     $      218, 168, 92, 38, 10, 8, 30, 76, 150, 200, 276, 331, 359, 
     $      416, 468, 618, 4*0/
      DATA (icsym(i,109),i=1,ICHDIM)/5, 31, 59, 100, 299, 624, 21*0/
      DATA (icsym(i,110),i=1,ICHDIM)/14, 474, 456, 81, 84, 6, 78, 84, 
     $      54, 78, 58, 31, 78, 30, 84, 12*0/
      DATA (icsym(i,111),i=1,ICHDIM)/24, 130, 132, 182, 180, 130, 182, 
     $      132, 180, 905, 282, 332, 330, 280, 332, 282, 330, 1055, 432, 
     $      482, 480, 430, 482, 432, 480, 2*0/
      DATA (icsym(i,112),i=1,ICHDIM)/18, 508, 456, 430, 329, 279, 205, 
     $      133, 111, 113, 141, 219, 295, 345, 444, 468, 516, 925, 324, 
     $      8*0/
      DATA (icsym(i,113),i=1,ICHDIM)/6, 12, 387, 399, 612, 375, 387, 
     $      20*0/
      DATA (icsym(i,114),i=1,ICHDIM)/23, 601, 550, 526, 459, 341, 292, 
     $      243, 193, 142, 91, 38, 10, 8, 31, 77, 126, 175, 225, 276, 
     $      327, 459, 592, 618, 3*0/
      DATA (icsym(i,115),i=1,ICHDIM)/21, 150, 180, 206, 406, 432, 460, 
     $      462, 440, 416, 442, 469, 471, 448, 424, 249, 223, 196, 191, 
     $      416, 191, 180, 5*0/
      DATA (icsym(i,116),i=1,ICHDIM)/13, 20, 48, 99, 148, 312, 624, 312, 
     $      0, 312, 476, 525, 576, 604, 13*0/
      DATA (icsym(i,117),i=1,ICHDIM)/23, 545, 497, 448, 349, 299, 198, 
     $      147, 120, 118, 141, 190, 314, 509, 506, 478, 401, 325, 275, 
     $      201, 128, 106, 109, 314, 3*0/
      DATA (icsym(i,118),i=1,ICHDIM)/26, 477, 451, 400, 125, 76, 28, 5, 
     $      7, 34, 86, 137, 362, 137, 88, 40, 17, 19, 46, 98, 149, 424, 
     $      473, 497, 496, 471, 497/
      DATA (icsym(i,119),i=1,ICHDIM)/19, 345, 295, 219, 141, 113, 111, 
     $      133, 205, 279, 329, 405, 483, 511, 513, 491, 419, 345, 925, 
     $      324, 7*0/
      DATA (icsym(i,120),i=1,ICHDIM)/14, 404, 12, -159, -234, -285, 
     $      -311, -313, -289, -240, -165, 12, 420, 422, 349, 12*0/
      DATA (icsym(i,121),i=1,ICHDIM)/15, 324, 348, 346, 320, 20, 295, 
     $      318, 340, 334, 306, 255, -80, -129, -152, -150, 11*0/
      DATA (icsym(i,122),i=1,ICHDIM)/10, 75, 97, 74, 23, 1148, 548, 549, 
     $      85, 214, 525, 16*0/
      DATA (icsym(i,123),i=1,ICHDIM)/13, 98, 149, 199, 224, 246, 378, 
     $      401, 425, 475, 526, 552, 775, 312, 13*0/
      DATA (icsym(i,124),i=1,ICHDIM)/14, 344, -169, 119, 68, 16, 8, 56, 
     $      130, 330, 80, 29, 3, 2, 25, 12*0/
      DATA (icsym(i,125),i=1,ICHDIM)/21, 44, 95, 145, 194, 268, 368, 
     $      442, 491, 541, 592, 651, 75, 125, 152, 194, 1067, 428, 451, 
     $      500, 550, 577, 5*0/
      DATA (icsym(i,126),i=1,ICHDIM)/19, 341, 291, 215, 137, 109, 107, 
     $      129, 201, 275, 325, 401, 479, 507, 509, 487, 415, 341, 733, 
     $      508, 7*0/
      DATA (icsym(i,127),i=1,ICHDIM)/19, 241, 191, 115, 37, 9, 7, 29, 
     $      101, 175, 225, 301, 379, 407, 409, 387, 315, 241, 841, -391, 
     $      7*0/
      DATA (icsym(i,128),i=1,ICHDIM)/20, 595, 567, 540, 489, 289, 213, 
     $      136, 108, 106, 128, 201, 275, 325, 401, 478, 506, 508, 486, 
     $      413, 339, 6*0/
      DATA (icsym(i,129),i=1,ICHDIM)/18, 48, 74, 124, 148, 197, 271, 
     $      371, 445, 494, 544, 570, 946, 278, 301, 350, 425, 476, 503, 
     $      8*0/
      DATA (icsym(i,130),i=1,ICHDIM)/5, 141, 300, 434, 462, 466, 21*0/
      DATA (icsym(i,131),i=1,ICHDIM)/21, 192, 142, 91, 62, 53, 76, 125, 
     $      225, 276, 303, 310, 303, 326, 375, 475, 526, 553, 562, 541, 
     $      492, 442, 5*0/
      DATA (icsym(i,132),i=1,ICHDIM)/26, 519, 495, 470, 416, 439, 464, 
     $      490, 491, 371, 321, 269, 187, 210, 235, 262, 194, 146, 96, 
     $      17, 9, -31, -81, -106, -132, -159, -163/
      DATA (icsym(i,133),i=1,ICHDIM)/16, 24, 99, 147, 170, 160, 185, 
     $      231, 381, 433, 460, 470, 522, 549, 624, 925, 324, 10*0/
      DATA (icsym(i,134),i=1,ICHDIM)/22, 590, 515, 489, 436, 435, 459, 
     $      484, 510, 512, 489, 443, 296, 96, 44, 15, 9, -31, -81, -106, 
     $      -132, -159, -163, 4*0/
      DATA (icsym(i,135),i=1,ICHDIM)/4, 0, 324, 600, 0, 22*0/
      DATA (icsym(i,136),i=1,ICHDIM)/7, 620, 624, 24, 312, 0, 600, 604, 
     $      19*0/
      DATA (icsym(i,137),i=1,ICHDIM)/23, 345, 295, 219, 141, 113, 111, 
     $      133, 205, 279, 329, 405, 483, 511, 513, 491, 419, 345, 875, 
     $      375, 300, 324, 249, 399, 3*0/
      DATA (icsym(i,138),i=1,ICHDIM)/7, 544, 549, 124, 174, 150, 75, 
     $      225, 19*0/
      DATA (icsym(i,139),i=1,ICHDIM)/12, 599, 49, 99, 75, 25, 125, 75, 
     $      99, 549, 525, 475, 575, 14*0/
      DATA (icsym(i,140),i=1,ICHDIM)/20, 52, 50, 200, 152, 82, 61, 63, 
     $      92, 223, 299, 349, 423, 542, 563, 561, 532, 452, 400, 550, 
     $      552, 6*0/
      DATA (icsym(i,141),i=1,ICHDIM)/12, 202, 201, 225, 250, 276, 303, 
     $      321, 348, 374, 399, 423, 422, 14*0/
      DATA (icsym(i,142),i=1,ICHDIM)/19, 345, 295, 219, 141, 113, 111, 
     $      133, 205, 279, 329, 405, 483, 511, 513, 491, 419, 345, 625, 
     $      624, 7*0/
      DATA (icsym(i,143),i=1,ICHDIM)/20, 137, 237, 187, 199, 147, 1134, 
     $      511, 462, 412, 361, 352, 400, 450, 502, 505, 482, 382, 355, 
     $      625, 624, 6*0/
      DATA (icsym(i,144),i=1,ICHDIM)/11, 137, 237, 187, 199, 147, 1075, 
     $      463, 357, 532, 625, 624, 15*0/
      DATA (icsym(i,145),i=1,ICHDIM)/26, 137, 237, 187, 199, 147, 977, 
     $      376, 425, 500, 551, 577, 579, 555, 506, 431, 506, 557, 583, 
     $      585, 561, 512, 437, 386, 360, 625, 624/
      DATA (icsym(i,146),i=1,ICHDIM)/17, 137, 237, 187, 199, 147, 985, 
     $      386, 437, 512, 561, 585, 583, 557, 350, 550, 625, 624, 9*0/
      DATA (icsym(i,147),i=1,ICHDIM)/25, 48, 99, 174, 248, 245, 219, 37, 
     $      237, 1001, 425, 500, 551, 577, 579, 506, 431, 506, 583, 585, 
     $      561, 512, 437, 386, 625, 624, 0/
      DATA (icsym(i,148),i=1,ICHDIM)/19, 88, 137, 212, 263, 289, 291, 
     $      218, 295, 297, 273, 224, 149, 99, 1075, 463, 357, 532, 625, 
     $      624, 7*0/
      DATA (icsym(i,149),i=1,ICHDIM)/9, 312, 162, 150, 450, 462, 312, 
     $      324, 418, 318, 17*0/
      DATA (icsym(i,150),i=1,ICHDIM)/9, 312, 162, 150, 450, 462, 312, 
     $      324, 1045, 220, 17*0/
      DATA (icsym(i,151),i=1,ICHDIM)/5, 300, 324, 12, 612, 324, 21*0/
      DATA (icsym(i,152),i=1,ICHDIM)/6, 24, 624, 00, 312, 600, 24, 20*0/
      DATA (icsym(i,153),i=1,ICHDIM)/14, 474, 456, 81, 84, 6, 78, 84, 
     $      54, 78, 58, 32, 78, 30, 84, 12*0/
      DATA (icsym(i,154),i=1,ICHDIM)/14, 174, 156, 531, 534, 606, 528, 
     $      534, 554, 528, 558, 582, 528, 580, 534, 12*0/
      DATA (icsym(i,155),i=1,ICHDIM)/14, 150, 168, 543, 546, 618, 540, 
     $      546, 566, 540, 570, 594, 540, 592, 546, 12*0/
      DATA (icsym(i,156),i=1,ICHDIM)/14, 456, 468, 93, 96, 18, 90, 96, 
     $      66, 90, 70, 44, 90, 42, 96, 12*0/
      DATA (icsym(i,157),i=1,ICHDIM)/5, 24, 312, 300, 312, 624, 21*0/
      DATA (icsym(i,158),i=1,ICHDIM)/5, 24, 624, 00, 600, 24, 21*0/
      DATA (icsym(i,159),i=1,ICHDIM)/9, 300, 324, 347, 444, 395, 949, 
     $      297, 245, 194, 17*0/
      DATA (icsym(i,160),i=1,ICHDIM)/27*0/
      DATA (icsym(i,161),i=1,ICHDIM)/27*0/
      DATA (icsym(i,162),i=1,ICHDIM)/27*0/
      DATA (icsym(i,163),i=1,ICHDIM)/27*0/
      DATA (icsym(i,164),i=1,ICHDIM)/27*0/
      DATA (icsym(i,165),i=1,ICHDIM)/27*0/
      DATA (icsym(i,166),i=1,ICHDIM)/27*0/
      DATA (icsym(i,167),i=1,ICHDIM)/10, 168, 456, 312, 156, 468, 324, 
     $      12, 300, 612, 324, 16*0/
      DATA (icsym(i,168),i=1,ICHDIM)/14, 168, 312, 468, 324, 168, 793, 
     $      12, 300, 1093, 612, 300, 781, 312, 456, 12*0/
      DATA (icsym(i,169),i=1,ICHDIM)/14, 312, 468, 612, 456, 312, 1093, 
     $      324, 12, 637, 300, 456, 781, 312, 168, 12*0/
      DATA (icsym(i,170),i=1,ICHDIM)/14, 300, 456, 312, 156, 300, 781, 
     $      12, 324, 949, 612, 456, 793, 312, 468, 12*0/
      DATA (icsym(i,171),i=1,ICHDIM)/14, 156, 12, 168, 312, 156, 781, 
     $      300, 612, 1237, 324, 168, 1081, 312, 468, 12*0/
      DATA (icsym(i,172),i=1,ICHDIM)/12, 456, 612, 324, 168, 456, 1081, 
     $      300, 12, 781, 312, 637, 168, 14*0/
      DATA (icsym(i,173),i=1,ICHDIM)/12, 156, 300, 612, 468, 156, 781, 
     $      12, 324, 937, 168, 949, 468, 14*0/
      DATA (icsym(i,174),i=1,ICHDIM)/12, 456, 300, 12, 168, 456, 1081, 
     $      612, 324, 937, 468, 949, 168, 14*0/
      DATA (icsym(i,175),i=1,ICHDIM)/12, 156, 12, 324, 468, 156, 781, 
     $      300, 612, 937, 456, 1237, 468, 14*0/
      DATA (icsym(i,176),i=1,ICHDIM)/13, 156, 300, 456, 168, 324, 468, 
     $      156, 1081, 612, 468, 781, 12, 168, 13*0/
      DATA (icsym(i,177),i=1,ICHDIM)/13, 12, 156, 468, 612, 456, 168, 
     $      12, 793, 324, 468, 781, 300, 456, 13*0/
      DATA (icsym(i,178),i=1,ICHDIM)/10, 168, 12, 300, 612, 468, 312, 
     $      168, 793, 324, 468, 16*0/
      DATA (icsym(i,179),i=1,ICHDIM)/10, 468, 312, 456, 300, 12, 324, 
     $      468, 1093, 612, 456, 16*0/
      DATA (icsym(i,180),i=1,ICHDIM)/10, 456, 312, 156, 12, 324, 612, 
     $      456, 1081, 300, 156, 16*0/
      DATA (icsym(i,181),i=1,ICHDIM)/10, 156, 312, 168, 324, 612, 300, 
     $      156, 781, 12, 168, 16*0/
      DATA (icmrk(i,1),i=1,15)/9, 339, 289, 263, 261, 285, 335, 361, 
     $      363, 339, 5*0/
      DATA (icmrk(i,2),i=1,15)/5, 324, 300, 312, 12, 612, 9*0/
      DATA (icmrk(i,3),i=1,15)/11, 324, 300, 312, 24, 600, 312, 12, 612, 
     $      312, 00, 624, 3*0/
      DATA (icmrk(i,4),i=1,15)/10, 324, 174, 18, 06, 150, 450, 606, 618, 
     $      474, 324, 4*0/
      DATA (icmrk(i,5),i=1,15)/5, 24, 600, 312, 00, 624, 9*0/
      DATA (icmrk(i,6),i=1,15)/5, 24, 00, 600, 624, 24, 9*0/
      DATA (icmrk(i,7),i=1,15)/4, 324, 00, 600, 324, 10*0/
      DATA (icmrk(i,8),i=1,15)/5, 324, 12, 300, 612, 324, 9*0/
      DATA (icmrk(i,9),i=1,15)/9, 474, 174, 18, 06, 150, 450, 606, 618, 
     $      474, 5*0/
      DATA (icmrk(i,10),i=1,15)/4, 24, 300, 624, 24, 10*0/
      DATA (icmrk(i,11),i=1,15)/10, 24, 624, 312, 24, 825, 400, 728, 
     $      503, 631, 606, 4*0/
      DATA (icmrk(i,12),i=1,15)/11, 324, 240, 15, 184, 100, 306, 500, 
     $      434, 615, 390, 324, 3*0/
      DATA (icmrk(i,13),i=1,15)/6, 300, 312, 612, 324, 12, 312, 8*0/
      DATA (icmrk(i,14),i=1,15)/12, 468, 168, 24, 168, 156, 00, 156, 
     $      456, 600, 456, 468, 624, 2*0/
      DATA (icmrk(i,15),i=1,15)/14, 324, 174, 18, 06, 150, 450, 606, 
     $      618, 474, 324, 300, 312, 12, 612/
      DATA (icmrk(i,16),i=1,15)/8, 24, 00, 600, 624, 24, 600, 00, 624, 
     $      6*0/
      DATA (icmrk(i,17),i=1,15)/10, 324, 300, 312, 12, 612, 624, 24, 00, 
     $      600, 612, 4*0/
      DATA (icmrk(i,18),i=1,15)/10, 168, 456, 312, 156, 468, 324, 12, 
     $      300, 612, 468, 4*0/
      DATA (icmrk(i,19),i=1,15)/9, 324, 300, 312, 12, 612, 324, 12, 300, 
     $      612, 5*0/
      DATA (icmrk(i,20),i=1,15)/2, 324, 300, 12*0/
      DATA circle/25, 613, 592, 545, 497, 448, 349, 299, 198, 147, 95, 
     $     42, 13, 11, 32, 79, 127, 176, 275, 325, 426, 477, 529, 582, 
     $     611, 613/
      DATA usgs/61, 2, 26, 75, 125, 176, 202, 226, 275, 325, 376, 402, 
     $     426, 475, 525, 576, 602, 578, 552, 501, 476, 427, 403, 377, 
     $     326, 276, 227, 203, 177, 126, 101, 52, 28, 2, 630, 56, 556, 
     $     322, 56, 5, 324, 605, 5, 704, 103, 469, 541, 521, 424, 397, 
     $     445, 79, 1128, 529, 195, 247, 224, 172, 119, 91, 169, 503/
      DATA errneg/17, 12, 112, 1015, 140, 133, 308, 133, 125, 375, 390, 
     $     565, 613, 610, 558, 383, 508, 600/
      DATA errpos/19, 12, 112, 1015, 140, 133, 308, 133, 125, 375, 390, 
     $     565, 613, 610, 558, 383, 508, 600, 685, 64/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GQLN(ierr,ltype)
      CALL GQLWSC(ierr,width)
      CALL GQPMCI(ierr,ipmclr)
      CALL GQFACI(ierr,ifaclr)
      CALL GQPLCI(ierr,iplclr)
      IF ( ltype.NE.1 ) CALL GSLN(1)
      IF ( ABS(width-1.0).GT.1.0E-6 ) CALL GSLWSC(1.0)
      IF ( ipmclr.NE.iplclr ) CALL GSPLCI(ipmclr)
      IF ( ipmclr.NE.ifaclr ) CALL GSFACI(ipmclr)
      CALL GQFAIS(ierr,ifill)
      IF ( ifill.NE.1 ) CALL GSFAIS(1)
C
C     Size the marker:
      height = ABS(HGT)/25.0
      angrad = ANGLE - IFIX(ANGLE/360.0)*360
      angrad = ANGLE*0.0174533
      cosang = COS(angrad)
      sinang = SIN(angrad)
      hgthlf = HGT*0.5
      xm = hgthlf*cosang - hgthlf*sinang - hgthlf
      ym = hgthlf*cosang + hgthlf*sinang - hgthlf
C
      DO 100 ii = 1, NN
        lfill = .FALSE.
        IF ( ii.EQ.1 .OR. HGT.LT.0.0 ) THEN
          mark = ISYM(ii)
          ang = ANGLE
          IF ( mark.GT.0 .AND. mark.LT.32 ) THEN
C           Centered marker
C           Check if marker is to be filled
            IF ( mark.GT.20 ) THEN
              lfill = .TRUE.
              IF ( mark.EQ.21 ) k = 1
              IF ( mark.EQ.22 ) k = 4
              IF ( mark.EQ.23 ) k = 6
              IF ( mark.EQ.24 ) k = 7
              IF ( mark.EQ.25 ) k = 8
              IF ( mark.EQ.26 ) k = 9
              IF ( mark.EQ.27 ) k = 10
              IF ( mark.EQ.28 ) k = 11
              IF ( mark.EQ.29 ) k = 12
              IF ( mark.EQ.30 ) k = 13
              IF ( mark.EQ.31 ) k = 14
            ELSE
              k = mark
            ENDIF
            num = icmrk(1,k)
            IF ( k.EQ.4 ) num = circle(1)
          ELSEIF ( mark.GT.32 .AND. mark.LT.183 ) THEN
C         Centered character
            IF ( mark.EQ.103 .OR. mark.EQ.106 .OR. mark.EQ.112 .OR. 
     $           mark.EQ.113 .OR. mark.EQ.121 .OR. mark.EQ.124 .OR. 
     $           mark.EQ.152 .OR. mark.EQ.153 .OR. mark.EQ.156 .OR. 
     $           mark.EQ.159 .OR. mark.EQ.164 .OR. mark.EQ.166 )
     $           ang = ang + 90.0
            k = mark - 32
            num = icsym(1,k)
          ELSEIF ( mark.GT.183 .AND. mark.LT.215 ) THEN
            k = mark - 33
            num = icsym(1,k)
            IF ( mark.GT.200 ) lfill = .TRUE.
          ELSEIF ( mark.EQ.183 ) THEN
            lfill = .TRUE.
            num = usgs(1)
          ELSEIF ( mark.LT.1 ) THEN
            num = errneg(1)
          ELSEIF ( mark.GT.214 ) THEN
            num = errpos(1)
          ELSE
C         mark=0 or 32
            GOTO 100
          ENDIF
C         mark not currently defined
          IF ( mark.GT.192 .AND. mark.LT.200 ) GOTO 100
          num = num + 1
C
C       Load arrays of coordinates of the line segments of the marker
          ns = 1
          np = 0
          DO 20 i = 2, num
            IF ( mark.GT.0 .AND. mark.LT.32 ) THEN
C           Centered marker
              IF ( k.NE.4 ) THEN
                ipt = icmrk(i,k)
              ELSE
                ipt = circle(i)
              ENDIF
            ELSEIF ( mark.EQ.183 ) THEN
              ipt = usgs(i)
            ELSEIF ( mark.LT.0 ) THEN
              ipt = errneg(i)
            ELSEIF ( mark.GT.214 ) THEN
              ipt = errpos(i)
            ELSE
C             character symbol
              ipt = icsym(i,k)
            ENDIF
            IF ( ipt.GT.624 ) THEN
              ipt = ipt - 625
              is(ns) = np
              ns = ns + 1
CC            IF ( ns.GT.NSEG ) STOP 'exceeded NSEG'
              np = 0
            ENDIF
            np = np + 1
            ix = ipt/25
            cy(np,ns) = (FLOAT(IABS(ipt-ix*25))-12.5)*height
            cx(np,ns) = (FLOAT(ix)-12.5)*height
            IF ( ABS(ang).GT.0.0005 )
     $           CALL GDGROT(ang,cx(np,ns),cy(np,ns))
 20       CONTINUE
          is(ns) = np
        ENDIF
C
C       Plot each point of centered symbol:
        DO 50 i = 1, ns
          np = is(i)
          DO 40 j = 1, np
            xx = PX(ii)
            yy = PY(ii)
            IF ( ABS(ANGLE).GT.0.0005 ) THEN
              xx = xx + xm
              yy = yy + ym
            ENDIF
            x(j) = xx + cx(j,i)
            y(j) = yy + cy(j,i)
 40       CONTINUE
          CALL GPL(np,x,y)
          IF ( lfill .AND. np.GT.3 ) CALL GFA(np,x,y)
 50     CONTINUE
 100  CONTINUE
C
C     Reset polyline and fill area characteristics
      IF ( ltype.NE.1 ) CALL GSLN(ltype)
      IF ( ABS(width-1.0).GT.1.0E-6 ) CALL GSLWSC(width)
      IF ( lfill .AND. ifill.NE.1 ) CALL GSFAIS(ifill)
      IF ( ipmclr.NE.ifaclr ) CALL GSFACI(ifaclr)
      IF ( ipmclr.NE.iplclr ) CALL GSPLCI(iplclr)
C
      RETURN
      END
C
C     gdgrot.f
C
      SUBROUTINE GDGROT
     I                 (ANGLE,
     M                  X,Y)
C
C     + + + PURPOSE + + +
C     Rotates the X and Y coordinates.  This routine is called by GPUTMK.
C
C     + + + HISTORY + + +
C       AUTHOR: R. Steven Regan  rsregan@usgs.gov
C       SYSTEM:
C     LANGUAGE: Fortran 77
C      VERSION: May 21, 1993
C
C     + + + KEYWORDS + + +
C     rotate
C
C     + + + DUMMY ARGUMENTS + + +
      REAL ANGLE, X, Y
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ANGLE  - The angle of rotation.
C     X      - The X coordinate.
C     Y      - The Y coordinate.
C
C     + + + LOCAL VARIABLES + + +
      REAL t, cosang, sinang
C
C     + + + INTRINSICS + + +
      INTRINSIC COS, SIN
C
C     + + + END SPECIFICATIONS + + +
C
C      Rotate X and Y:
C
      t = 0.0174533*ANGLE
      cosang = COS(t)
      sinang = SIN(t)
 
      t = X*cosang - Y*sinang
      Y = Y*cosang + X*sinang
      X = t
C
      RETURN
      END
