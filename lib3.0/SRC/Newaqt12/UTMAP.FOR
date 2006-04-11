C
C
C
      SUBROUTINE   MAPINI
     I                   (CNMAPS,CMAPDE,CMAPBR,CINTAC,CMPNMF,CMPNMC,
     I                    CSFCOL,CUFCOL,CSBCOL,CUBCOL,CBACKC,
     I                    CMPNMS,CRLAT,CRLNG,CLSIZE,CCURMA,
     I                    CBASNA,CLEGFL,CBASEY,CBASEX,CLGSIZ,CLEGCO,
     I                    CCMSID)
C
C     + + + PURPOSE + + +
C     initialize map info data structure
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CNMAPS,CMAPDE,CMAPBR,CINTAC,CMPNMF(CNMAPS),
     $              CMPNMC(CNMAPS),CSFCOL,CUFCOL,CSBCOL,CUBCOL,
     $              CBACKC(2),CLEGFL(CNMAPS),CLEGCO(CNMAPS),CCURMA
      REAL          CMPNMS(CNMAPS),CRLAT(2,CNMAPS),CRLNG(2,CNMAPS),
     $              CLSIZE,CBASEY(CNMAPS),CBASEX(CNMAPS),
     $              CLGSIZ(CNMAPS)
      CHARACTER*8   CCMSID(CNMAPS)
      CHARACTER*64  CBASNA(CNMAPS)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CMAPDE - map device
C     CMAPBR - map border
C     CINTAC - interact flag
C     CMPNMF - include map name flag
C     CMPNMC - map name color
C     CSFCOL - select foreground color
C     CUFCOL - unselect foreground color
C     CSBCOL - select background color
C     CUBCOL - unselect background color
C     CBACKC - map background color
C     CMPNMS - map name size
C     CRLAT  - base map latitude min/max
C     CRLNG  - base map longitude min/max
C     CLSIZE - lettering size for commands
C     CCURMA - current map number
C     CBASNA - basin name for map title
C     CLEGFL - legend flag
C     CBASEY - base position of legend(y)
C     CBASEX - base position of legend(x)
C     CLGSIZ - legend text size
C     CLEGCO - legend text color
C     CNMAPS - number of maps available
C     CCMSID - character string id for each map
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      MAPDEV   = CMAPDE
      MAPBRD   = CMAPBR
      INTACT   = CINTAC
      SFCOLR   = CSFCOL
      UFCOLR   = CUFCOL
      SBCOLR   = CSBCOL
      UBCOLR   = CUBCOL
      BACKCL(1)= CBACKC(1)
      BACKCL(2)= CBACKC(2)
      LSIZE    = CLSIZE
      NMAPS    = CNMAPS
      CURMAP   = CCURMA
C
      DO 10 I= 1,CNMAPS
        RLAT(1,I)= CRLAT(1,I)
        RLAT(2,I)= CRLAT(2,I)
        RLNG(1,I)= CRLNG(1,I)
        RLNG(2,I)= CRLNG(2,I)
        BASNAM(I)= CBASNA(I)
        LEGFLG(I)= CLEGFL(I)
        BASEY(I) = CBASEY(I)
        BASEX(I) = CBASEX(I)
        CMSID(I) = CCMSID(I)
        MPNMFG(I)= CMPNMF(I)
        MPNMCL(I)= CMPNMC(I)
        MPNMSZ(I)= CMPNMS(I)
        LGSIZE(I)= CLGSIZ(I)
        LEGCOL(I)= CLEGCO(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPGET
     I                   (PMXMAP,
     O                    CNMAPS,CMAPDE,CMAPBR,CINTAC,CMPNMF,CMPNMC,
     O                    CSFCOL,CUFCOL,CSBCOL,CUBCOL,CBACKC,
     O                    CMPNMS,CRLAT,CRLNG,CLSIZE,CCURMA,
     O                    CBASNA,CLEGFL,CBASEY,CBASEX,CLGSIZ,CLEGCO,
     O                    CCMSID)
C
C     + + + PURPOSE + + +
C     get map info to write to sta file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       PMXMAP,CNMAPS,CMAPDE,CMAPBR,CINTAC,CMPNMF(PMXMAP),
     $              CMPNMC(PMXMAP),CSFCOL,CUFCOL,CSBCOL,CUBCOL,
     $              CBACKC(2),CLEGFL(PMXMAP),CLEGCO(PMXMAP),CCURMA
      REAL          CMPNMS(PMXMAP),CRLAT(2,PMXMAP),CRLNG(2,PMXMAP),
     $              CLSIZE,CBASEY(PMXMAP),CBASEX(PMXMAP),
     $              CLGSIZ(PMXMAP)
      CHARACTER*8   CCMSID(PMXMAP)
      CHARACTER*64  CBASNA(PMXMAP)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     PMXMAP - max number of maps
C     CMAPDE - map device
C     CMAPBR - map border
C     CINTAC - interact flag
C     CMPNMF - include map name flag
C     CMPNMC - map name color
C     CSFCOL - select foreground color
C     CUFCOL - unselect foreground color
C     CSBCOL - select background color
C     CUBCOL - unselect background color
C     CBACKC - map background color
C     CMPNMS - map name size
C     CRLAT  - base map latitude min/max
C     CRLNG  - base map longitude min/max
C     CLSIZE - lettering size for commands
C     CCURMA - current map number
C     CBASNA - basin name for map title
C     CLEGFL - legend flag
C     CBASEY - base position of legend(y)
C     CBASEX - base position of legend(x)
C     CLGSIZ - legend text size
C     CLEGCO - legend text color
C     CNMAPS - number of maps available
C     CCMSID - character string identifier for this map
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      CMAPDE   = MAPDEV
      CMAPBR   = MAPBRD
      CINTAC   = INTACT
      CSFCOL   = SFCOLR
      CUFCOL   = UFCOLR
      CSBCOL   = SBCOLR
      CUBCOL   = UBCOLR
      CBACKC(1)= BACKCL(1)
      CBACKC(2)= BACKCL(2)
      CLSIZE   = LSIZE
      CNMAPS   = NMAPS
      CCURMA   = CURMAP
C
      DO 10 I= 1,CNMAPS
        CRLAT(1,I) = RLAT(1,I)
        CRLAT(2,I) = RLAT(2,I)
        CRLNG(1,I) = RLNG(1,I)
        CRLNG(2,I) = RLNG(2,I)
        CBASNA(I)  = BASNAM(I)
        CLEGFL(I)  = LEGFLG(I)
        CBASEY(I)  = BASEY(I)
        CBASEX(I)  = BASEX(I)
        CMPNMF(I)  = MPNMFG(I)
        CMPNMC(I)  = MPNMCL(I)
        CMPNMS(I)  = MPNMSZ(I)
        CLGSIZ(I)  = LGSIZE(I)
        CLEGCO(I)  = LEGCOL(I)
        CCMSID(I)  = CMSID(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPPUT
     I                   (IMAP,CMPNMF,CMPNMC,CMPNMS,CRLAT1,CRLAT2,
     I                    CRLNG1,CRLNG2,
     I                    CBASNA,CLEGFL,CBASEY,CBASEX,CLGSIZ,CLEGCO,
     I                    CCMSID)
C
C     + + + PURPOSE + + +
C     put a map info data structure to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IMAP,CMPNMF,CMPNMC,CLEGFL,CLEGCO
      REAL          CMPNMS,CRLAT1,CRLAT2,CRLNG1,CRLNG2,CBASEY,
     $              CBASEX,CLGSIZ
      CHARACTER*8   CCMSID
      CHARACTER*64  CBASNA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CMPNMF - include map name flag
C     CMPNMC - map name color
C     CMPNMS - map name size
C     CRLAT  - base map latitude min/max
C     CRLNG  - base map longitude min/max
C     CBASNA - basin name for map title
C     CLEGFL - legend flag
C     CBASEY - base position of legend(y)
C     CBASEX - base position of legend(x)
C     CLGSIZ - legend text size
C     CLEGCO - legend text color
C     CCMSID - character string id for each map
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      RLAT(1,IMAP)= CRLAT1
      RLAT(2,IMAP)= CRLAT2
      RLNG(1,IMAP)= CRLNG1
      RLNG(2,IMAP)= CRLNG2
      BASNAM(IMAP)= CBASNA
      LEGFLG(IMAP)= CLEGFL
      BASEY(IMAP) = CBASEY
      BASEX(IMAP) = CBASEX
      CMSID(IMAP) = CCMSID
      MPNMFG(IMAP)= CMPNMF
      MPNMCL(IMAP)= CMPNMC
      MPNMSZ(IMAP)= CMPNMS
      LGSIZE(IMAP)= CLGSIZ
      LEGCOL(IMAP)= CLEGCO
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPCUR
     O                   (CCURMA)
C
C     + + + PURPOSE + + +
C     get current map number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CCURMA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CCURMA - current map number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CCURMA = CURMAP
C
      RETURN
      END
C
C
C
      SUBROUTINE   CURPUT
     I                   (CCURMA)
C
C     + + + PURPOSE + + +
C     put current map number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CCURMA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CCURMA - current map number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CURMAP = CCURMA
C
      RETURN
      END
C
C
C
      SUBROUTINE   MBRGET
     O                   (CMAPBR)
C
C     + + + PURPOSE + + +
C     get map boundary flag from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CMAPBR
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CMAPBR - map border
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CMAPBR = MAPBRD
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLLGET
     I                   (IMAP,
     O                    CRLAT,CRLNG)
C
C     + + + PURPOSE + + +
C     get map boundary min/max from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IMAP
      REAL   CRLAT(2),CRLNG(2)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP  - which map to get info about
C     CRLAT - map boundary lat min/max
C     CRLNG - map boundary lng min/max
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CRLAT(1) = RLAT(1,IMAP)
      CRLAT(2) = RLAT(2,IMAP)
      CRLNG(1) = RLNG(1,IMAP)
      CRLNG(2) = RLNG(2,IMAP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLLPUT
     I                   (IMAP,CRLAT,CRLNG)
C
C     + + + PURPOSE + + +
C     put map boundary min/max to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IMAP
      REAL    CRLAT(2),CRLNG(2)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP  - which map to put info for
C     CRLAT - map boundary lat min/max
C     CRLNG - map boundary lng min/max
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      RLAT(1,IMAP) = CRLAT(1)
      RLAT(2,IMAP) = CRLAT(2)
      RLNG(1,IMAP) = CRLNG(1)
      RLNG(2,IMAP) = CRLNG(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MDVGET
     O                   (CMAPDE)
C
C     + + + PURPOSE + + +
C     get map device from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CMAPDE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CMAPDE - map device
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CMAPDE = MAPDEV
C
      RETURN
      END
C
C
C
      SUBROUTINE   MDVPUT
     I                   (CMAPDE)
C
C     + + + PURPOSE + + +
C     put map device to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CMAPDE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CMAPDE - map device
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      MAPDEV = CMAPDE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MINGET
     O                   (CINTAC)
C
C     + + + PURPOSE + + +
C     get map device from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CINTAC
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CINTAC - map interact flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CINTAC = INTACT
C
      RETURN
      END
C
C
C
      SUBROUTINE   MNMGET
     I                   (IMAP,
     O                    CMPNMF,CBASNA,CMPNMC,CMPNMS)
C
C     + + + PURPOSE + + +
C     get map title info from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IMAP,CMPNMF,CMPNMC
      REAL          CMPNMS
      CHARACTER*64  CBASNA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP   - which map to get info about
C     CMPNMF - map name flag
C     CMPNMC - map name color
C     CMPNMS - map name size
C     CBASNA - map name for title
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CMPNMF = MPNMFG(IMAP)
      CMPNMC = MPNMCL(IMAP)
      CMPNMS = MPNMSZ(IMAP)
      CBASNA = BASNAM(IMAP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MNMPUT
     I                   (IMAP,CMPNMF,CBASNA,CMPNMC,CMPNMS)
C
C     + + + PURPOSE + + +
C     put map title info to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IMAP,CMPNMF,CMPNMC
      REAL          CMPNMS
      CHARACTER*64  CBASNA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP   - which map to put info for
C     CMPNMF - map name flag
C     CMPNMC - map name color
C     CMPNMS - map name size
C     CBASNA - map name for title
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      MPNMFG(IMAP) = CMPNMF
      MPNMCL(IMAP) = CMPNMC
      MPNMSZ(IMAP) = CMPNMS
      BASNAM(IMAP) = CBASNA
C
      RETURN
      END
C
C
C
      SUBROUTINE   MBKGET
     O                   (CBACKC)
C
C     + + + PURPOSE + + +
C     get map background fill colors from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CBACKC(2)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CBACKC - map background fill layer and color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CBACKC(1) = BACKCL(1)
      CBACKC(2) = BACKCL(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MBKPUT
     I                   (CBACKC)
C
C     + + + PURPOSE + + +
C     put map background fill colors to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CBACKC(2)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CBACKC - map background fill layer and color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      BACKCL(1) = CBACKC(1)
      BACKCL(2) = CBACKC(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MOPGET
     O                   (CLSIZE,CSBCOL,CUBCOL,CSFCOL,CUFCOL)
C
C     + + + PURPOSE + + +
C     get map background fill colors from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CSBCOL,CUBCOL,CSFCOL,CUFCOL
      REAL          CLSIZE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLSIZE - lettering size for commands
C     CSFCOL - select foreground color
C     CUFCOL - unselect foreground color
C     CSBCOL - select background color
C     CUBCOL - unselect background color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CLSIZE = LSIZE
      CSFCOL = SFCOLR
      CUFCOL = UFCOLR
      CSBCOL = SBCOLR
      CUBCOL = UBCOLR
C
      RETURN
      END
C
C
C
      SUBROUTINE   MOPPUT
     I                   (CLSIZE,CSBCOL,CUBCOL,CSFCOL,CUFCOL)
C
C     + + + PURPOSE + + +
C     put map background fill colors to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       CSBCOL,CUBCOL,CSFCOL,CUFCOL
      REAL          CLSIZE
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CLSIZE - lettering size for commands
C     CSFCOL - select foreground color
C     CUFCOL - unselect foreground color
C     CSBCOL - select background color
C     CUBCOL - unselect background color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LSIZE  = CLSIZE
      SFCOLR = CSFCOL
      UFCOLR = CUFCOL
      SBCOLR = CSBCOL
      UBCOLR = CUBCOL
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLGGET
     I                    (IMAP,
     O                     CLEGFL,CBASEY,CBASEX,CLGSIZ,CICOL)
C
C     + + + PURPOSE + + +
C     get map legend details from common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IMAP,CLEGFL,CICOL
      REAL          CBASEY,CBASEX,CLGSIZ
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP   - which map to get info about
C     CLEGFL - legend flag
C     CBASEY - base position of legend(y)
C     CBASEX - base position of legend(x)
C     CLGSIZ - legend text size
C     CICOL  - legend text color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CLGSIZ = LGSIZE(IMAP)
      CICOL  = LEGCOL(IMAP)
      CLEGFL = LEGFLG(IMAP)
      CBASEY = BASEY(IMAP)
      CBASEX = BASEX(IMAP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLGPUT
     I                   (IMAP,CLEGFL,CBASEY,CBASEX,CLGSIZ,CICOL)
C
C     + + + PURPOSE + + +
C     put map legend details to common
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IMAP,CLEGFL,CICOL
      REAL          CBASEY,CBASEX,CLGSIZ
C
C     + + + ARGUMENT DEFINTIONS + + +
C     IMAP   - which map to put info for
C     CLEGFL - legend flag
C     CBASEY - base position of legend(y)
C     CBASEX - base position of legend(x)
C     CLGSIZ - legend text size
C     CICOL  - legend text color
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LGSIZE(IMAP) = CLGSIZ
      LEGCOL(IMAP) = CICOL
      LEGFLG(IMAP) = CLEGFL
      BASEY(IMAP)  = CBASEY
      BASEX(IMAP)  = CBASEX
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPADD
     I                   (MAPBID,COPZOM,
     O                    MAPID)
C
C     + + + PURPOSE + + +
C     add a new map set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAPBID,MAPID,COPZOM
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MAPBID - map set base id - default values for new map set
C     MAPID  - identifier of new map set
C     COPZOM - copy or zoom flag for new map set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      MAPID = NMAPS+ 1
      IF (MAPID .LE. MXMAP) THEN
C       room for a new map set
        IF (MAPBID.GT.0 .AND. MAPBID.LE.NMAPS) THEN
C         use base for map set defaults
          RLAT(1,MAPID)= RLAT(1,MAPBID)
          RLAT(2,MAPID)= RLAT(2,MAPBID)
          RLNG(1,MAPID)= RLNG(1,MAPBID)
          RLNG(2,MAPID)= RLNG(2,MAPBID)
          BASNAM(MAPID)= BASNAM(MAPBID)
          LEGFLG(MAPID)= LEGFLG(MAPBID)
          BASEY(MAPID) = BASEY(MAPBID)
          BASEX(MAPID) = BASEX(MAPBID)
          MPNMFG(MAPID)= MPNMFG(MAPBID)
          MPNMCL(MAPID)= MPNMCL(MAPBID)
          MPNMSZ(MAPID)= MPNMSZ(MAPBID)
          LGSIZE(MAPID)= LGSIZE(MAPBID)
          LEGCOL(MAPID)= LEGCOL(MAPBID)
          IF (COPZOM.EQ.1) THEN
C           copying this map set
            CMSID(MAPID) = '<COPY>'
          ELSE
C           zoomed to get this map set
            CMSID(MAPID) = '<ZOOM>'
          END IF
        ELSE
C         general defaults
          RLAT(1,MAPID)= 0.0
          RLAT(2,MAPID)= 0.0
          RLNG(1,MAPID)= 0.0
          RLNG(2,MAPID)= 0.0
          BASNAM(MAPID)= ' '
          LEGFLG(MAPID)= 0
          BASEY(MAPID) = 0.0
          BASEX(MAPID) = 0.0
          MPNMFG(MAPID)= 0
          MPNMCL(MAPID)= 0
          MPNMSZ(MAPID)= 0.0
          LGSIZE(MAPID)= 0.0
          LEGCOL(MAPID)= 0
          IF (COPZOM.EQ.1) THEN
C           copying this map set
            CMSID(MAPID) = '<COPY>'
          ELSE
C           zoomed to get this map set
            CMSID(MAPID) = '<ZOOM>'
          END IF
        END IF
C       update number of classes
        NMAPS= MAPID
      ELSE
C       no room
        MAPID= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MAPDEL
     I                   (MAPDID)
C
C     + + + PURPOSE + + +
C     delete a set of map specs
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAPDID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MAPDID - map set id to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cmap.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MAPDID.GT.0 .AND. MAPDID.LE.NMAPS) THEN
C       the map set exists to delete
        IF (MAPDID.LT.NMAPS) THEN
C         move later map sets
          DO 10 ID= MAPDID,NMAPS-1
            RLAT(1,ID)= RLAT(1,ID+1)
            RLAT(2,ID)= RLAT(2,ID+1)
            RLNG(1,ID)= RLNG(1,ID+1)
            RLNG(2,ID)= RLNG(2,ID+1)
            BASNAM(ID)= BASNAM(ID+1)
            LEGFLG(ID)= LEGFLG(ID+1)
            BASEY(ID) = BASEY(ID+1)
            BASEX(ID) = BASEX(ID+1)
            MPNMFG(ID)= MPNMFG(ID+1)
            MPNMCL(ID)= MPNMCL(ID+1)
            MPNMSZ(ID)= MPNMSZ(ID+1)
            LGSIZE(ID)= LGSIZE(ID+1)
            LEGCOL(ID)= LEGCOL(ID+1)
            CMSID(ID) = CMSID(ID+1)
 10       CONTINUE
        END IF
C       update number of map sets
        NMAPS= NMAPS- 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLLABX
     I                   (RLAT,RLNG,
     O                    AXMIN,AXMAX,AYMIN,AYMAX)
C
C     calcs albers box which includes all of a lat-lng box
C
C     + + + DUMMY ARGUMENTS + + +
      REAL     RLAT(2),RLNG(2),AXMIN,AXMAX,AYMIN,AYMAX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RLAT  - min and max latitude
C     RLNG  - min and max longitude
C     AXMIN - albers x min
C     AXMAX - albers x max
C     AYMIN - albers y min
C     AYMAX - albers y max
C
C     + + + LOCAL VARIABLES + + +
      REAL*8      DLAT,DLNG,ALBX,ALBY
C
C     + + + EXTERNALS + + +
      EXTERNAL    CLLAL1
C
C     + + + END SPECIFICATIONS + + +
C
      DLAT= RLAT(1)
      DLNG= -RLNG(1)
      CALL CLLAL1(DLAT,DLNG,
     O            ALBX,ALBY)
      AXMIN= ALBX
      AXMAX= ALBX
      AYMIN= ALBY
      AYMAX= ALBY
C     mn,mx
      DLNG= -RLNG(2)
      CALL CLLAL1(DLAT,DLNG,
     O            ALBX,ALBY)
      IF (ALBX.LT.AXMIN) AXMIN= ALBX
      IF (ALBX.GT.AXMAX) AXMAX= ALBX
      IF (ALBY.LT.AYMIN) AYMIN= ALBY
      IF (ALBY.GT.AYMAX) AYMAX= ALBY
C     mx,mx
      DLAT= RLAT(2)
      CALL CLLAL1(DLAT,DLNG,
     O            ALBX,ALBY)
      IF (ALBX.LT.AXMIN) AXMIN= ALBX
      IF (ALBX.GT.AXMAX) AXMAX= ALBX
      IF (ALBY.LT.AYMIN) AYMIN= ALBY
      IF (ALBY.GT.AYMAX) AYMAX= ALBY
C     mx,mn
      DLNG= -RLNG(1)
      CALL CLLAL1(DLAT,DLNG,
     O            ALBX,ALBY)
      IF (ALBX.LT.AXMIN) AXMIN= ALBX
      IF (ALBX.GT.AXMAX) AXMAX= ALBX
      IF (ALBY.LT.AYMIN) AYMIN= ALBY
      IF (ALBY.GT.AYMAX) AYMAX= ALBY
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLLAL1
     I                   (DLAT,DLON,
     O                    X,Y)
C
C     Converts latitude and longitude (degrees) to the Albers
C        equal area projection for the United States.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8  DLAT,DLON,X,Y
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DLAT   - Latitude in degrees.
C     DLON   - Longitude in degrees.
C     X      - X Albers coordinates
C     Y      - Y Albers coordinates
C
C     + + + COMMON BLOCKS + + +
      INCLUDE  'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL*8      RHO,RHO1SQ,FR2B,SPSI1,RHO0,BD,DEGRAD,DIF
C
C     + + + INTRINSICS + + +
      INTRINSIC   DSQRT,DSIN,DCOS
C
C     + + + DATA INITIALZATIONS + + +
      DATA RHO1SQ /8.4607444756165D13/
      DATA FR2B   /1.3466195843067D14/
      DATA SPSI1  /0.4924235601035D00/
C     DATA RHO0   /9914713.649668/
      DATA RHO0   /9909400./
      DATA BD     /1.0521490583632D-02/
      DATA DEGRAD /1.7453292519943E-02/
C     noew comes from calling program thru common
C     DATA LAM0   /-119.0/
C
C     + + + END SPECIFICATIONS + + +
C
      RHO = DSQRT (RHO1SQ + FR2B * (SPSI1 - DSIN(DEGRAD*DLAT)) )
      DIF = BD * (DLON - LAM0)
      X   = RHO * DSIN (DIF)
      Y   = RHO0 - RHO * DCOS (DIF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CALDIS
     I                   (X1,Y1,HANCNT,HANX,HANY,
     O                    HANDIS,HANCLO)
C
C     calculates distances between point and handles
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  HANCNT,HANCLO
      REAL     X1,Y1,HANX(HANCNT),HANY(HANCNT),HANDIS(HANCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X1     - x coordinate to calculate distances from
C     Y1     - y coordinate to calculate distances from
C     HANCNT - count of handles
C     HANX   - array of handle x coordinates
C     HANY   - array of handle y coordinates
C     HANDIS - array of distances to each handle point
C     HANCLO - index of closest point
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       MDIS,XDIS,YDIS
C
C     + + + INTRINSICS + + +
      INTRINSIC  SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      MDIS  = 1.0E30
      HANCLO= 0
C
      DO 10 I= 1,HANCNT
        XDIS= X1- HANX(I)
        YDIS= Y1- HANY(I)
        HANDIS(I)= SQRT((XDIS*XDIS)+(YDIS*YDIS))
        IF (HANDIS(I).LT.MDIS) THEN
C         closest so far
          HANCLO= I
          MDIS  = HANDIS(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CALLL1
     I                   (X,Y,
     O                    DLAT,DLON)
C
C     + + + PURPOSE + + +
C     transform from X/Y Albers conic equal-area projection to Long/Lat
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8   X,Y,DLAT,DLON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      - X Albers coordinates
C     Y      - Y Albers coordinates
C     DLAT   - Latitude in degrees.
C     DLON   - Longitude in degrees.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE  'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    INIT,I
      REAL       SUP,SLP,OLON,OLAT
      REAL*8     A,B
      REAL*8     RADIAN,PHI0,SLP1,SUP1,E2,ECCEN
      REAL*8     TEMP0,TEMP1,TEMP2,XM1,XM2,Q0,Q1,Q2,XN,C,P0
      REAL*8     TEMP11,TEMP22
      REAL*8     PHI,TEMP,Q,P,THETA,T,RPHI
      REAL*8     CLON,AA
      SAVE       INIT,CLON,AA,RADIAN,E2,ECCEN,XN,C,P0
C
C     + + + INTRINSICS + + +
      INTRINSIC        DSQRT,DSIN,DCOS,DLOG,DATAN,DABS,DASIN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INIT  /0/
      DATA RADIAN/57.2957795D0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INIT .EQ. 0) THEN
C       compute transformation constatns
        OLAT  = 23.0
        OLON  = LAM0
        A     = 6378206.4
        B     = 6356583.8
        SLP   = 29.5
        SUP   = 45.5
        PHI0  = OLAT/RADIAN
        SLP1  = SLP/RADIAN
        SUP1  = SUP/RADIAN
        E2    = (A*A-B*B)/(A*A)
        ECCEN = DSQRT(E2)
        TEMP0 = 1.D0-E2*DSIN(PHI0)*DSIN(PHI0)
        TEMP1 = 1.D0-E2*DSIN(SLP1)*DSIN(SLP1)
        TEMP11= DSQRT(TEMP1)
        TEMP2 = 1.D0-E2*DSIN(SUP1)*DSIN(SUP1)
        TEMP22= DSQRT(TEMP2)
        XM1   = DCOS(SLP1)/TEMP11
        XM2   = DCOS(SUP1)/TEMP22
        Q1    = DLOG((1.D0-ECCEN*DSIN(SLP1))/(1.D0+ECCEN*DSIN(SLP1)))
        Q1    = Q1*(1.D0/(2.D0*ECCEN))
        Q1    = (DSIN(SLP1)/TEMP1)-Q1
        Q1    = (1.D0-E2)*Q1
        Q2    = DLOG((1.D0-ECCEN*DSIN(SUP1))/(1.D0+ECCEN*DSIN(SUP1)))
        Q2    = Q2*(1.D0/(2.D0*ECCEN))
        Q2    = (DSIN(SUP1)/TEMP2)-Q2
        Q2    = (1.D0-E2)*Q2
        Q0    = DLOG((1.D0-ECCEN*DSIN(PHI0))/(1.D0+ECCEN*DSIN(PHI0)))
        Q0    = Q0*(1.D0/(2.D0*ECCEN))
        Q0    = (DSIN(PHI0)/TEMP0)-Q0
        Q0    = (1.D0-E2)*Q0
        XN    = (XM1*XM1-XM2*XM2)/(Q2-Q1)
        C     = XM1*XM1+XN*Q1
        P0    = A*(DSQRT(C-XN*Q0))/XN
        CLON  = OLON
        AA    = A
        INIT  = 1
      END IF
C
C     do transformation
      P    = DSQRT(X*X+(P0-Y)*(P0-Y))
      THETA= DATAN(X/(P0-Y))
      THETA= THETA*RADIAN
      Q    = (C-(P*XN/AA)*(P*XN/AA))/XN
      T    = Q/(1.D0-E2)
      PHI  = DASIN(Q/2.D0)*RADIAN
      I    = 0
10    CONTINUE
        I    = I+ 1
        RPHI = PHI/RADIAN
        TEMP = (1.D0-E2*DSIN(RPHI)*DSIN(RPHI))
        TEMP1= T-(DSIN(RPHI)/TEMP)
        TEMP2= DLOG((1.D0-ECCEN*DSIN(RPHI))/(1.D0+ECCEN*DSIN(RPHI)))
        TEMP2= TEMP2*(1.D0/(2.D0*ECCEN))
        TEMP1= TEMP1+TEMP2
        TEMP1= TEMP1*RADIAN
        TEMP1= TEMP1*(TEMP*TEMP)/(2.D0*DCOS(RPHI))
        PHI  = PHI+TEMP1
      IF (DABS(TEMP1).GT. 0.0000001D0 .AND. I.LT.100) GO TO 10
C
      DLAT= PHI
      DLON= CLON+(THETA/XN)
C
      RETURN
      END
