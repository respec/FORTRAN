      subroutine yldopt(ityp,nowcrp,iplane,imngmt)
c
c     + + + PURPOSE + + +
c     Estimate growing degree days during the growing season (gddmax).
c     Compute optimum biomass production (dm), and use it to adjust
c     the biomass conversion factor (be) if the user inputs an optimum
c     yield.
c
c     Called from TILAGE
c     Author(s): Arnold
c     Reference in User Guide:
c
c     NOTE: REMOVE REFERENCE TO "YOPTP" FROM MAKEFILE.  THIS SAVED
c           8k in the UNIX VERSION.
c
c     Changes:
c              1) Converted XMONTH from a subroutine to a function.
c                 Calls to it from this routine were modified
c                 accordingly.
c              2) Program structure used to look like this:
c
c                                      CONTIN
c                                         |
c                                      TILAGE
c                                      /    \
c                                 YOPTP      YLDOPT
c                                   |          |
c                   -----------------          -----------------
c                   |        |      |          |      |        |
c                GROWOPT  XMONTH  GDMAX      GDMAX  XMONTH  GROWOPT
c                                   |          |
c                                XMONTH      XMONTH
c
c                 Eliminated call to XMONTH from GDMAX, and combined
c                 YOPTP & YLDOPT, giving the following structure:
c
c                                      CONTIN
c                                         |
c                                      TILAGE
c                                         |
c                                      YLDOPT
c                                         |
c                                  ----------------
c                                  |      |       |
c                                GDMAX  XMONTH  GROWOP
c
c              3) Since XMONTH is now a function, the following line
c                     call growop(imngmt,xmonth(ida),ityp,sumgd,alai,dm)
c                 was substituted for:
c                     call xmonth(ida,month)
c                     call growop(dm,month,ityp,sumgd,alai,imngmt)
c              4) Eliminated local variable YIELD.
c              5) Embedded RETURN's eliminated.
c              6) GOTO's to outside of do-loops eliminated.
c              7) CALLS to subroutine GROWOPT were changed to send the
c                 extinction coefficient to it.   dcf  2/5/93
c              8) CALLS to subroutine GROWOPT changed to GROWOP.
c                 dcf  6/2/94
c
c
c     Version: This module recoded from Arnold's WEPP Post-version 92.25.
c     Date recoded: 07/14/92 - 07/16/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer ityp, nowcrp, iplane, imngmt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ityp   - Index of plant, from list of those used in the simulation.
c     nowcrp - Index of plant (1st, 2nd, 3rd....) of the sequence grown
c              on this OFE, this year.
c     iplane - Index of the current OFE (land use area).
c     imngmt - Cropping System (annual, perennial, or fallow).
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpprm.inc'
c        read: jdharv(mxcrop,mxplan), jdplt(mxcrop,mxplan),
c              yld(mxcrop,mxplan)
c
      include 'ccrpvr3.inc'
c        read: dlai(ntype),itype(mxcrop,mxplan)
c      modify: gddmax(ntype)
c
      include 'ccrpgro.inc'
c        read: beinp(ntype), y4(ntype), hi(ntype), extnct(ntype)
c       write: be(ntype)
c
c     + + + LOCAL VARIABLES + + +
      real dm, alai, sumgd, yield
      integer ida
c
c     + + + LOCAL DEFINITIONS + + +
c     dm     - Biomass; ie, dry matter
c     alai   - Leaf area index
c     sumgd  - Accumulated growing degree days (gdd).
c
c     + + + SUBROUTINES CALLED + + +
c     GDMAX
c     GROWOP
c
c     + + + FUNCTION DECLARATIONS + + +
      integer xmonth
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c      *** L0 IF ***
c      ANNUALS
      if (imngmt.eq.1) then
c
c       *** L1 IF ***
c       SUMMER CROPS
        if (jdharv(nowcrp,iplane).gt.jdplt(nowcrp,iplane)) then
c
c         -------- If user did not input the number of degrees in the growing
c         season (gddmax), estimate it.
          if (gddmax(ityp).le.0.0) then
c
c           ---------- Estimate maximum growing degree days (gddmax) from planting
c           date (jdplt) to harvest date (jdharv).
            call gdmax(jdplt(nowcrp,iplane),jdharv(nowcrp,iplane),sumgd,
     1          ityp)
            gddmax(ityp) = sumgd
c
          end if
c
c
c         *** L2 IF ***
c         -------- If the user did not input an optimum yield (yld), then
c         do not adjust the biomass conversion factor (be).
c
          if (yld(ityp).le.0) then
c
            be(ityp) = beinp(ityp)
c
c
c         *** L2 ELSE ***
c         -------- Estimate maximum biomass production (dm) from planting
c         date (jdplt) to harvest date (jdharv).
c
          else
c
            dm = 0.
            alai = 0.
            sumgd = 0.0
c
            ida = jdplt(nowcrp,iplane) - 1
   10       continue
            ida = ida + 1
c           Original Code:
c           call xmonth(ida,month)
c           call growop(dm,month,ityp,sumgd,alai,imngmt)
c
c           call growop(imngmt,xmonth(ida),ityp,sumgd,alai,dm)
            call growop(imngmt,xmonth(ida),ityp,
     1          extnct(itype(nowcrp,iplane)),sumgd,alai,dm)
            if ((ida.lt.jdharv(nowcrp,iplane)).and.(sumgd.le.
     1          gddmax(ityp)*dlai(ityp))) go to 10
c
c           Adjust the biomass conversion factor (be)
c           yldbsh = hi(ityp) * dm * 8907.7 / y4(ityp)
            yield = hi(ityp) * dm
            be(ityp) = beinp(ityp) * yld(ityp) / yield
c
c           write (6,2000) iplane,yld(ityp),yldbsh
            write (6,1000) iplane, yld(ityp), yield
c
c         *** L2 ENDIF ***
          end if
c
c
c       *** L1 ELSE ***
c       WINTER CROPS
        else
c
c         -------- If user did not input the number of degrees in the growing
c         season (gddmax), estimate it.
          if (gddmax(ityp).le.0.0) then
c
c           ---------- Estimate maximum growing degree days (gddmax) from planting
c           date (jdplt) to end of year.
            call gdmax(jdplt(nowcrp,iplane),365,sumgd,ityp)
            gddmax(ityp) = sumgd
c
c           ---------- Estimate maximum growing degree days (gddmax) from beginning
c           of year to harvest date (jdharv).
            call gdmax(1,jdharv(nowcrp,iplane),sumgd,ityp)
            gddmax(ityp) = gddmax(ityp) + sumgd
c
          end if
c
c
c         *** M2 IF ***
c         If the user did not input an optimum yield (yld), then
c         do not adjust the biomass conversion factor (be).
c
          if (yld(ityp).le.0) then
            be(ityp) = beinp(ityp)
c
c
c         *** M2 ELSE ***
          else
c
            dm = 0.
            alai = 0.
            sumgd = 0.0
c
c           ---------- Estimate maximum biomass production (dm) from planting
c           date (jdplt) to end of calendar year.
            ida = jdplt(nowcrp,iplane) - 1
   20       continue
            ida = ida + 1
c           Original Code:
c           call xmonth(ida,month)
c           call growop(dm,month,ityp,sumgd,alai,imngmt)
c
c           call growop(imngmt,xmonth(ida),ityp,sumgd,alai,dm)
            call growop(imngmt,xmonth(ida),ityp,
     1          extnct(itype(nowcrp,iplane)),sumgd,alai,dm)
            if ((ida.lt.365).and.(sumgd.le.gddmax(ityp)*dlai(ityp)))
     1          go to 20
c
            if (sumgd.le.gddmax(ityp)*dlai(ityp)) then
c
c             ------------ Estimate maximum biomass production (dm) from beginning
c             of calendar year to harvest date (jdharv).
              ida = 0
   30         continue
              ida = ida + 1
c             Original Code:
c             call xmonth(ida,month)
c             call growop(dm,month,ityp,sumgd,alai,imngmt)
c
c             call growop(imngmt,xmonth(ida),ityp,sumgd,alai,dm)
              call growop(imngmt,xmonth(ida),ityp,
     1            extnct(itype(nowcrp,iplane)),sumgd,alai,dm)
              if ((ida.lt.jdharv(nowcrp,iplane)).and.(sumgd.le.
     1            gddmax(ityp)*dlai(ityp))) go to 30
c
            end if
c
c           Adjust the biomass conversion factor (be)
c           yldbsh = hi(ityp) * dm * 8907.7 / y4(ityp)
            yield = hi(ityp) * dm
c
            be(ityp) = beinp(ityp) * yld(ityp) / yield
c
            write (6,1000) iplane, yld(ityp), yield
c
c         *** M2 ENDIF ***
          end if
c
c       *** L1 ENDIF ***
        end if
c
c
c     *** L0 ELSEIF ***
c     PERENNIALS
      else if (imngmt.eq.2) then
c
c       ------ If user did not input the number of degrees in the growing
c       season (gddmax), estimate it.
        if (gddmax(ityp).le.0.0) then
          call gdmax(1,365,sumgd,ityp)
          gddmax(ityp) = sumgd
        end if
c
c       *** N1 IF ***
c       If the user did not input an optimum yield (yld), then
c       do not adjust the biomass conversion factor (be).
c
        if (yld(ityp).le.0) then
          be(ityp) = beinp(ityp)
c
c       *** N1 ELSE ***
        else
c
c         Estimate maximum biomass production (dm).
          dm = 0.
          alai = 0.
          sumgd = 0.0
c
          ida = 0
c         *** BEGIN N2 LOOP ***
   40     continue
          ida = ida + 1
c         Original Code:
c         call xmonth(ida,month)
c         call growop(dm,month,ityp,sumgd,alai,imngmt)
c
c         call growop(imngmt,xmonth(ida),ityp,sumgd,alai,dm)
          call growop(imngmt,xmonth(ida),ityp,
     1        extnct(itype(nowcrp,iplane)),sumgd,alai,dm)
c         *** END N2 LOOP ***
          if ((ida.lt.365).and.(sumgd.le.gddmax(ityp)*dlai(ityp))) go to
     1        40
c
c         -------- Compute YLDTON in tons
c         yldton = hi(ityp) * dm * 4.0469
c         yldton = hi(ityp) * dm * 4.4609
          yield = hi(ityp) * dm
c
c
c         -------- Adjust the biomass conversion factor (be)
          be(ityp) = beinp(ityp) * yld(ityp) / yield
c
          write (6,1000) iplane, yld(ityp), yield
c
c       *** N1 ENDIF ***
        end if
c
c     *** L0 ENDIF ***
      end if
c
      return
 1000 format (/,' For OFE #',i2,', User Input Peak Crop Yield =',f7.2,
     1    '; EPIC Peak Crop Yield =',f7.2)
      end
