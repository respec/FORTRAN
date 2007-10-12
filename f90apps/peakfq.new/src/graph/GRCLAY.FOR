C
C
C
      SUBROUTINE   GPERFL
     I                   (FEE)
C
C     + + + PURPOSE + + +
C     This routine adds the Fortran unit number for the GKS error
C     file to the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FEE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FEE    - Fortran unit number for GKS error file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      FE = FEE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGERFL
     O                   (FEE)
C
C     + + + PURPOSE + + +
C     This routine gets the Fortran unit number for the GKS error
C     file from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FEE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FEE    - Fortran unit number for GKS error file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      FEE = FE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDVTY
     I                   (DVTYPE)
C
C     + + + PURPOSE + + +
C     This routine puts the output device type in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DVTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVTYPE - output device category
C              1 - screen
C              2 - printer (impact and laser)
C              3 - plotter
C              4 - meta file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DEVTYP = DVTYPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGDVTY
     O                   (DVTYPE)
C
C     + + + PURPOSE + + +
C     This routine gets the current output device type
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DVTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVTYPE - output device category
C              1 - screen
C              2 - printer (impact and laser)
C              3 - plotter
C              4 - meta file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DVTYPE = DEVTYP
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPDVCD
     I                   (DVCODE)
C
C     + + + PURPOSE + + +
C     This routine puts the output device code in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DVCODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVCODE - device code, GKS system dependent number for the
C              output device, DEVTYP, from the TERM.DAT file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DEVCOD = DVCODE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGDVCD
     O                   (DVCODE)
C
C     + + + PURPOSE + + +
C     This routine gets the current output device code
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DVCODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVCODE - device code, GKS system dependent number for the
C              output device, DEVTYP, from the TERM.DAT file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      DVCODE = DEVCOD
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPTITL
     I                   (TITLE)
C
C     + + + PURPOSE + + +
C     This routine puts the plot title in the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*240  TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TITLE  - title for the plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L240
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L240 = 240
      CALL CVARAR (L240, TITLE, L240, TITL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTITL
     O                   (TITLE)
C
C     + + + PURPOSE + + +
C     This routine gets the current plot title from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*240  TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TITLE  - title for the plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L240
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L240 = 240
      CALL CARVAR (L240, TITL, L240, TITLE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPMTFL
     I                    (GDMTPL)
C
C     + + + PURPOSE + + +
C     This routine adds the meta file Fortran unit number to the
C     plotting COMMON block, CPLTV.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GDMTPL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GDMTPL - Fortran unit number for the graphics output file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      MTPLUT = GDMTPL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGMTFL
     O                    (GDMTPL)
C
C     + + + PURPOSE + + +
C     This routine gets the meta file Fortran unit number from the
C     plotting COMMON block, CPLTV.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GDMTPL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GDMTPL - Fortran unit number for the graphics output file
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      GDMTPL = MTPLUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPPAGE
     I                   (XPG, YPG)
C
C     + + + PURPOSE + + +
C     This routine puts the horizontal and vertical page size in the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XPG, YPG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPG    - horizontal page size in inches
C     YPG    - vertical page size in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XPAGE = XPG
      YPAGE = YPG
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGPAGE
     O                   (XPG, YPG)
C
C     + + + PURPOSE + + +
C     This routine gets the current horizontal and vertical page size
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XPG, YPG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPG    - horizontal page size in inches
C     YPG    - vertical page size in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XPG = XPAGE
      YPG = YPAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPORIG
     I                   (XORIG, YORIG)
C
C     + + + PURPOSE + + +
C     This routine puts the horizontal and vertical position of the
C     origin in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XORIG, YORIG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XORIG - horizontal position of origin in inches
C     YORIG - vertical position of origin in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XPHYS = XORIG
      YPHYS = YORIG
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGORIG
     O                   (XORIG, YORIG)
C
C     + + + PURPOSE + + +
C     This routine gets the current horizontal and vertical position
C     of the origin from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XORIG, YORIG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XORIG - horizontal position of origin in inches
C     YORIG - vertical position of origin in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      XORIG = XPHYS
      YORIG = YPHYS
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLSIZ
     I                   (LTRSIZ)
C
C     + + + PURPOSE + + +
C     This routine puts the lettering height in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   LTRSIZ
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LTRSIZ - height of lettering in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SIZEL = LTRSIZ
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLSIZ
     O                   (LTRSIZ)
C
C     + + + PURPOSE + + +
C     This routine gets the current lettering height from the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   LTRSIZ
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LTRSIZ - height of lettering in inches
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LTRSIZ = SIZEL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLEDG
     I                    (GLOCLG)
C
C     + + + PURPOSE + + +
C     This routine puts the location of the legend into the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   GLOCLG(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GLOCLG - location of legend as fraction of XLEN and YLEN
C              from origin (values between 0.0 and 1.0)
C              -2.0 - no legend
C              -1.0 - legend in upper left corner (default location)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LOCLGD(1) = GLOCLG(1)
      LOCLGD(2) = GLOCLG(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLEDG
     O                    (GLOCLG)
C
C     + + + PURPOSE + + +
C     This routine gets the location of the legend from the
C     CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   GLOCLG(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GLOCLG - location of legend as fraction of XLEN and YLEN
C              from origin (values between 0.0 and 1.0)
C              -2.0 - no legend
C              -1.0 - legend in upper left corner (default location)
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      GLOCLG(1) = LOCLGD(1)
      GLOCLG(2) = LOCLGD(2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPBVFG
     I                   (BADFLG)
C
C     + + + PURPOSE + + +
C     This routine puts the bad value flag for the bottom, top, left,
C     and right of the plot in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BADFLG(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BADFLG - bad value flag for bottom, top, left, and right of plot
C              1 - clip, plot at point going off scale
C              2 - skip, leave blank
C              3 - plot arrow pointing off scale, don't connect lines
C              4 - ignore, connect good points
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYI (L4, BADFLG, BVALFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGBVFG
     O                   (BADFLG)
C
C     + + + PURPOSE + + +
C     This routine gets the current bad value flag for the bottom, top,
C     left, and right of the plot from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BADFLG(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BADFLG - bad value flag for bottom, top, left, and right of plot
C              1 - clip, plot at point going off scale
C              2 - skip, leave blank
C              3 - plot arrow pointing off scale, don't connect lines
C              4 - ignore, connect good points
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYI (L4, BVALFG, BADFLG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPBLNK
     I                    (GBLNKI)
C
C     + + + PURPOSE + + +
C     This routine puts into the CPLTV common block the corners of a
C     rectangle within which nothing is to be plotted.  This is useful
C     to keep pen plotters from putting holes in paper when too many
C     zeros are in the data or when a text block is not to have any
C     data in it.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   GBLNKI(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GBLNKI - min-max on y-axis and min-max on x-axis for box for
C              no plotting
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYR (L4, GBLNKI, BLNKIT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGBLNK
     O                    (GBLNKI)
C
C     + + + PURPOSE + + +
C     This routine gets from the CPLTV common block the corners of a
C     rectangle within which nothing is to be plotted.  This is useful
C     to keep pen plotters from putting holes in paper when too many
C     zeros are in the data or when a text block is not to have any
C     data in it.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   GBLNKI(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GBLNKI - min-max on y-axis and min-max on x-axis for box for
C              no plotting
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYR (L4, BLNKIT, GBLNKI)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPCTXT
     I                   (CTEXT)
C
C     + + + PURPOSE + + +
C     This routine puts the extra text to be placed on the plot in the
C     CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*120  CTEXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CTEXT  - text to be placed on plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L120
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      L120 = 120
      CALL CVARAR (L120, CTEXT, L120, CTXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGCTXT
     O                   (CTEXT)
C
C     + + + PURPOSE + + +
C     This routine gets the extra text to be placed on the plot
C     from the CPLTC common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*120  CTEXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CTEXT  - text to be placed on plot
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L120
C
C     + + + EXTERNALS + + +
      EXTERNAL  CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      L120 = 120
      CALL CARVAR (L120, CTXT, L120, CTEXT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPNTXT
     I                   (NUMCHR, CHRLIN)
C
C     + + + PURPOSE + + +
C     This routine puts the number of extra characters to plot and the
C     number of characters per line in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMCHR, CHRLIN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMCHR - number of characters to use (up to 120)
C     CHRLIN - characters per line
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NCHR = NUMCHR
      CPR = CHRLIN
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGNTXT
     O                   (NUMCHR, CHRLIN)
C
C     + + + PURPOSE + + +
C     This routine gets the number of extra characters to plot and the
C     number of characters per line from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMCHR, CHRLIN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMCHR - number of characters to use (up to 120)
C     CHRLIN - characters per line
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NUMCHR = NCHR
      CHRLIN = CPR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPFTXT
     I                   (FRXTXT, FRYTXT)
C
C     + + + PURPOSE + + +
C     This routine puts the horizontal and vertical position of the
C     extra text to be placed on the plot in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   FRXTXT, FRYTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRXTXT - fraction (0.0-1.0) of YLEN for upper left corner of text
C     FRYTXT - fraction (0.0-1.0) of XLEN for upper left corner of text
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      FXT = FRXTXT
      FYT = FRYTXT
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGFTXT
     O                   (FRXTXT, FRYTXT)
C
C     + + + PURPOSE + + +
C     This routine gets the current horizontal and vertical position of
C     the extra text to be placed on the plot from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   FRXTXT, FRYTXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRXTXT - fraction (0.0-1.0) of YLEN for upper left corner of text
C     FRYTXT - fraction (0.0-1.0) of XLEN for upper left corner of text
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      FRXTXT = FXT
      FRYTXT = FYT
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPXLOC
     I                   (XWLOC)
C
C     + + + PURPOSE + + +
C     This routine puts the location of the upper left and lower right
C     corners of the X-window for the plot in the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XWLOC(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XWLOC  - location of upper left and lower right corners of X-window
C              for plot as fractions of the monitor size (needed only for
C              screen output on systems running X11)
C              XWLOC(1) - fraction of horizontal distance for upper left corner
C              XWLOC(2) - fraction of vertical distance for upper left corner
C              XWLOC(3) - fraction of horizontal distance for lower left corner
C              XWLOC(4) - fraction of vertical distance for lower left corner
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYR (L4, XWLOC, XWINLC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGXLOC
     O                   (XWLOC)
C
C     + + + PURPOSE + + +
C     This routine gets the current location of the upper left and lower
C     right corners of the X-window for the plot from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XWLOC(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XWLOC  - location of upper left and lower right corners of X-window
C              for plot as fractions of the monitor size (needed only for
C              screen output on systems running X11)
C              XWLOC(1) - fraction of horizontal distance for upper left corner
C              XWLOC(2) - fraction of vertical distance for upper left corner
C              XWLOC(3) - fraction of horizontal distance for lower left corner
C              XWLOC(4) - fraction of vertical distance for lower left corner
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L4
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYR
C
C     + + + END SPECIFICATIONS + + +
C
      L4 = 4
      CALL COPYR (L4, XWINLC, XWLOC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPLBCL
     I                   (LBCLR)
C
C     + + + PURPOSE + + +
C     This routine adds the color for the axes, labels, and title
C     to the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LBCLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LBCLR    - Code for color for axes, labels and title
C                0-black, 1-white
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LBCOLR = LBCLR
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGLBCL
     O                   (LBCLR)
C
C     + + + PURPOSE + + +
C     This routine gets the color for the axes, labels, and title
C     from the CPLTV common block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LBCLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LBCLR    - Code for color for axes, labels and title
C                0-black, 1-white
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplot.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LBCLR = LBCOLR
C
      RETURN
      END
