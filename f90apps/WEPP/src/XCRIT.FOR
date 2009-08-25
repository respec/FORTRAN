      subroutine xcrit(a,b,c,tauc,xb,xe,qostar,xc1,xc2,mshear)
c
c     + + + PURPOSE + + +
c     Determines whether shear stress exceeds critical shear stress
c     for a certain segment.  Returns a flag.
c
c     Called from subroutine ROUTE.
c     Author(s): G. Foster, M. Nearing, D. Flanagan
c     Reference in User Guide:
c
c     Changes: 1) All the GOTO logic was changed to nested IF's.  This
c                 permitted a single RETURN at the end of the module.
c              2) In the statement under the label "added 8/23/90 by dcf",
c                 "...x2.gt.xb)mshear=1" was changed to "...x2.gt.xE)
c                 mshear=1".  (Dennis's suggestion.)
c              3) Re: the values of MSHEAR, since 2 & 3 were lumped
c                 together, and a computed GOTO can't handle a zero,
c                 the original "3" was eliminated, and 0-2 were "pro-
c                 moted" to 1-3. CRM - 1/24/91.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/09/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real a, b, c, tauc, xb, xe, qostar, xc1, xc2
      integer mshear
c
c     + + + ARGUMENT DEFINITIONS + + +
c     a     - shear stress equation coefficient
c     b     - shear stress equation coefficient
c     c     - shear stress equation coefficient
c     tauc  - n.d. critical shear for the Overland Flow Element
c     xb    - beginning distance for segment of OFE
c     xe    - ending distance for segment of OFE
c     qostar- n.d. water inflow to the current OFE
c     xc1   - 1st point where shear = critical shear (if one exists)
c     xc2   - 2nd point where shear = critical shear (if one exists)
c     mshear- flag indicating what shear conditions exist on segment
c
c     mshear |      Meaning
c     ----------------------------------------------------------------
c       1    |  shear less than critical throughout segment
c       2    |  shear greater than critical throughout segment
c       3    |  shear equal critical shear within a segment
c            |      upslope  shear < critical shear
c            |      downslope shear > critical shear
c       4    |  shear equal critical shear within a segment
c            |      shear decreasing within segment
c            |      upslope  shear > critical shear
c            |      downslope shear < critical shear
c       5    |  shear equal critical shear two places in a segment
c            |      shear increases then decreases
c     ----------------------------------------------------------------
c
c
c
c     + + + LOCAL VARIABLES + + +
      double precision tauchk, x1, x2
      real taub,taue,part
c
c     tauchk - partial solution to quadratic equation used here
c     taub   - n.d. shear stress calculated at the beg. of segment
c     taue   - n.d. shear stress calculated at the end of the segment
c     part   - partial solution to quadratic equation used here
c
c     + + + SUBROUTINES CALLED + + +
c     root
c
c     + + + FUNCTION DECLARATIONS + + +
      real shear
c
c     + + + END SPECIFICATIONS + + +
c
c
      tauchk = tauc ** 1.5 - c
      taub = shear(a,b,c,xb)
      taue = shear(a,b,c,xe)

c     if(tauchk.lt.0.0) tauchk=0.0
c
c     ********************
c     ***  The BIG If  ***
c     ********************
      if (a.eq.0.0) then
c
c       UNIFORM-SLOPE SEGMENT.
c       Determine location where critical shear stress is exceeded.
c
        if (b.ne.0.0) then
          xc1 = tauchk / b
        else
          xc1 = 1000.
        end if
c
c
cc      if (qostar.ge.0.0) then
        if (taue.gt.taub) then
          mshear = 3
          if (xc1.le.xb) mshear = 2
          if (xc1.ge.xe) mshear = 1
        else
          mshear = 4
          if (xc1.ge.xe) mshear = 2
          if (xc1.le.xb) mshear = 1
        end if
c
c     *************************
c     ***  The BIG Else-If  ***
c     *************************
c     else if (a.gt.0.0.and.qostar.ge.0.0) then
      else if (a.gt.0.0 .and. taue.gt.taub) then
c
c     CONVEX SEGMENT on an OFE on which shear increases downslope
c
cc      taub = shear(a,b,c,xb)
c
c       If shear stress at the beginning of the convex segment exceeds
c       critical, then the entire segment exceeds critical shear stress.
c
        if (taub.ge.tauc) then
          mshear = 2
c
        else
cc        taue = shear(a,b,c,xe)
c
c         Else, if shear stress at the end of the convex segment is less
c         than critical, then the entire segment is below critical shear
c         stress.
c
          if (taue.le.tauc) then
            mshear = 1
c
          else
            call root(a,b,tauchk,x1,x2)
c
            mshear = 3
c
c           Else, determine the point where shear stress exceeds critical
c           shear stress.
c
            if (x1.ge.xb.and.x1.le.xe) then
              xc1 = x1
            else
              if (x2.ge.xb.and.x2.le.xe) xc1 = x2
            end if
          end if
        end if
c
c     **********************
c     ***  The BIG Else  ***
c     **********************
      else
c
c       any other type of segment:
c       CONVEX with shear decreasing down segment
c       CONCAVE with shear increasing or decreasing down segment
c       UNIFORM with shear increasing or decreasing down segment
c
cc      taub = shear(a,b,c,xb)
cc      taue = shear(a,b,c,xe)
c
        if (taue.ge.tauc.and.taub.ge.tauc) then
c
c         If shear stress exceeds critical at both ends of the segment,
c         it exceeds critical all along the segment.
c
          mshear = 2
        else
c
c         Determine if shear exceeds critical shear somewhere
c         along the segment.
c
          part = b ** 2 + 4.0 * a * tauchk
c
c         If solution of quadratic equation has no real roots, then
c         critical shear stress is not exceeded along entire segment.
c
          if (part.le.0.0) then
            mshear = 1
          else
c
c           Else, determine WHERE shear stress equals critical shear
c           stress, by solving for roots of quadratic equation.
c
            call root(a,b,tauchk,x1,x2)
c
c           If shear stress INCREASES on segment, set MSHEAR flag = 3
c           and return location where shear = critical.
c
            if (taub.le.tauc.and.taue.ge.tauc) then
              mshear = 3
              if (x1.le.xb.or.x1.ge.xe) then
                xc1 = x2
              else
                xc1 = x1
              end if
            else
c
c             If shear stress DECREASES on segment, set MSHEAR flag = 4
c             and return location where shear = critical.
c
              if (taub.ge.tauc.and.taue.le.tauc) then
                mshear = 4
                if (x1.le.xb.or.x1.ge.xe) then
                  xc1 = x2
                else
                  xc1 = x1
                end if
              else
c
c               If shear at both top and bottom of segment is below
c               critical, return two locations where it equals critical.
c
                if (taub.le.tauc.and.taue.le.tauc) then
                  mshear = 5
c                 note:  root always computes x1 < x2
                  xc1 = x1
                  xc2 = x2
cWarnings from ftnchek
c                 89           xc1 = tauchk / b
c                 ^
cWarning near line 89 col 15: dble truncated to real
c                 139               xc1 = x1
c                 ^
cWarning near line 139 col 19: dble truncated to real
c                 141               if (x2.ge.xb.and.x2.le.xe) xc1 = x2
c                 ^
cWarning near line 141 col 46: dble truncated to real
c                 168           part = b ** 2 + 4.0 * a * tauchk
c                 ^
cWarning near line 168 col 16: dble truncated to real
c                 188                 xc1 = x2
c                 ^
cWarning near line 188 col 21: dble truncated to real
c                 190                 xc1 = x1
c                 ^
cWarning near line 190 col 21: dble truncated to real
c                 200                   xc1 = x2
c                 ^
cWarning near line 200 col 23: dble truncated to real
c                 202                   xc1 = x1
c                 ^
cWarning near line 202 col 23: dble truncated to real
c                 212                   xc1 = x1
c                 ^
cWarning near line 212 col 23: dble truncated to real
c                 213                   xc2 = x2
c                 ^
cWarning near line 213 col 23: dble truncated to real
c
c                 Check to make sure that for a Case 4, both points fall
c                 between xb and xe, and that they are not the SAME point.
c
                  if (x1.lt.xb.or.x1.gt.xe.or.x2.lt.xb.or.x2.gt.xe.or.x1
     1                .eq.x2) mshear = 1
                end if
              end if
            end if
          end if
        end if
c
c     ************************
c     ***  The BIG End-If  ***
c     ************************
      end if
      return
      end
