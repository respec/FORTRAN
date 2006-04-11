C
C     include file CMAP.INC
C     common block for storage of map data structure
      INCLUDE 'pmxmap.inc'
C
      COMMON/CMAP/MAPDEV,MAPBRD,INTACT,MPNMFG,MPNMCL,
     $            SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL,
     $            MPNMSZ,RLAT,RLNG,LSIZE,LEGFLG,BASEY,
     $            BASEX,LGSIZE,LEGCOL,NMAPS,CURMAP
      INTEGER       MAPDEV,MAPBRD,INTACT,MPNMFG(MXMAP),MPNMCL(MXMAP),
     $              SFCOLR,UFCOLR,SBCOLR,UBCOLR,BACKCL(2),
     $              LEGFLG(MXMAP),LEGCOL(MXMAP),NMAPS,CURMAP
      REAL          MPNMSZ(MXMAP),LSIZE,
     $              RLAT(2,MXMAP),RLNG(2,MXMAP),
     $              BASEY(MXMAP),BASEX(MXMAP),
     $              LGSIZE(MXMAP)
C
      SAVE /CMAP/
C
      COMMON/CMAPC/BASNAM,CMSID
      CHARACTER*64  BASNAM(MXMAP)
      CHARACTER*8   CMSID(MXMAP)
C
