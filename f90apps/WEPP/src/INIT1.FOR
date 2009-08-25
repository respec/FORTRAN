      subroutine init1(nowcrp)
c
c*******************************************************************
c     *
c     Initialize variables used in decomposition subroutine.       *
c     Calculates initial surface residue, submerged residue        *
c     and root masses on the first day of simulation.              *
c     *
c*******************************************************************
c
c*******************************************************************
c     *
c     Arguments                                                      *
      integer nowcrp
c     nowcrp :                                                     *
c     *
c*******************************************************************
c
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxres.inc'
      include 'pmxhil.inc'
      include 'pmxcut.inc'
c
c********************************************************************
c     *
c     Common Blocks                                                   *
c     *
c********************************************************************
c
c
c
      include 'cclim.inc'
c
      include 'ccover.inc'
c
c******************************************************************
c     *
c     cover variables updated                                         *
c     rilcov(mxplan)                                                *
c     *
c******************************************************************
c
      include 'ccrpgro.inc'
      include 'ccrpvr1.inc'
c
c*******************************************************************
c     *
c     crpvr1 variables updated                                         *
c     rmogt(mxres,mxplan),  rmagt(mxplan)
c     rtm(mxres,mxplan),  smrm(mxres,mxplan)                        *
c     *
c*******************************************************************
c
      include 'ccrpvr2.inc'
c
c********************************************************************
c     *
c     crpvr2 variables updated                                          *
c     vdmt(mxplan)                            *
c     *
c********************************************************************
c
      include 'ccrpvr3.inc'
c
c*********************************************************************
c     *
c     crpvr3 variables updated                                           *
c     sumgdd(mxplan), bb(ntype), vdmmax(mxplan)                        *
c     *
c*********************************************************************
c
c      include 'ccrpvr4.inc'
c
      include 'ccrpvr5.inc'
c
      include 'ccrpout.inc'
c
      include 'ccrpprm.inc'
c
      include 'cends4.inc'
c
      include 'cgcovr.inc'
c     modify: gcvplt(mxplan),mantyp(mxplan)
c
      include 'cperen.inc'
c
c*********************************************************************
c     *
c     peren variables updated                                            *
c     srmhav                                                           *
c     *
c*********************************************************************
c
      include 'cridge.inc'
c
c**********************************************************************
c     *
c     ridge variables updated                                             *
c     rilrm(mxres,mxplan), rigrm(mxres,mxplan),  rigcov(mxplan)      *
c     *
c**********************************************************************
c
      include 'cstruc.inc'
      include 'cwater.inc'
      include 'cwint.inc'
c     *
c     Local Variables                                                  *
c     stemar :                                                       *
c     areacv :                                                       *
c     *
c*********************************************************************
c-----------------------------------------------------------------------*
      real areacv, stemar, stuff, ytntop, ytnbot, wght1, ch, h,
     1    vdmxx,biomas
      integer nowres
c
      nowres = 1
      iroot(1,iplane) = iresd(nowres,iplane)
      areacv = pltsp(iresd(nowres,iplane)) * rw(nowcrp,iplane)
c
      popmat(iplane) = (1.0/areacv)
      stemar = 3.14 * (diam(iresd(nowres,iplane))**2) / 4.0
      basmat(iplane) = popmat(iplane) * stemar
c
      wght1 = (rspace(iplane)-width(iplane)) / rspace(iplane)
      rescov(iplane) = wght1 * inrcov(iplane) + (1.0-wght1) *
     1    rilcov(iplane)
      rigcov(iplane) = inrcov(iplane)
c
c     Following 2 variables are used in Ksat macroporosity
c     adjustments (currently neutered  -  12/1/93)  dcf
      mantyp(iplane) = imngmt(nowcrp,iplane)
      gcvplt(iplane) = 0.5 * rescov(iplane)
c
c     Assume that no standing residue exists, since input information
c     on type of residue management and type of tillage after harvest
c     is not available.  RMAGT = 0      dcf   11/30/93
c
      rmagt(iplane) = 0.0
c
c     Use input initial conditions and other information about
c     crops and temperatures to determine the correct canopy
c     cover for day 1 of a continuous simulation
c
      if (imodel.eq.1) then
c
c       ----- ANNUAL CROP
        if (imngmt(nowcrp,iplane).eq.1) then
c
c         ------- SUMMER ANNUAL - not planted yet so no canopy possible
          if (jdplt(nowcrp,iplane).lt.jdharv(nowcrp,iplane)) then
            cancov(iplane) = 0.0
          end if
c
c       ----- PERENNIAL CROP
        else if (imngmt(nowcrp,iplane).eq.2) then
c
c         ------- IF a planting date exists - no living crop should be
c         present - thus set canopy cover to zero
c
          if (jdplt(nowcrp,iplane).gt.0) then
            cancov(iplane) = 0.0
c
c         ELSE, a planting date of zero was input, indicating that
c         a living perennial is already present
c
          else
c
c           ESTABLISHED PERENNIALS - initialize root depth/mass
            if (rdmax(itype(nowcrp,iplane)).lt.
     1          solthk(nsl(iplane),iplane)) then
              rtd(iplane) = rdmax(itype(nowcrp,iplane))
            else
              rtd(iplane) = solthk(nsl(iplane),iplane)
            end if
c
            rtmass(iplane) = rtmmax(itype(nowcrp,iplane))
c
            if (rtd(iplane).gt.0.60) then
              rtm15(iplane) = 0.42 * rtmass(iplane)
              rtm30(iplane) = 0.28 * rtmass(iplane)
              rtm60(iplane) = 0.20 * rtmass(iplane)
            else if (rtd(iplane).gt.0.30) then
              rtm15(iplane) = 0.45 * rtmass(iplane)
              rtm30(iplane) = 0.30 * rtmass(iplane)
              rtm60(iplane) = 0.25 * rtmass(iplane)
            else if (rtd(iplane).gt.0.15) then
              rtm15(iplane) = 0.60 * rtmass(iplane)
              rtm30(iplane) = 0.40 * rtmass(iplane)
              rtm60(iplane) = 0.0
            else
              rtm15(iplane) = rtmass(iplane)
              rtm30(iplane) = 0.0
              rtm60(iplane) = 0.0
            end if
c
c
c   FOLLOWING SECTION OF CODE commented out 4/19/94 because at
c   the point where INIT1 is called the first day's value for
c   variable "tmnavg" has not been determined yet.  By commenting
c   this section out in the initialization, the initial canopy
c   cover input may have an impact on day 1 of the simulation -
c   but will be converted to dead residue on day 2 if in too
c   cold of a climate.     dcf   4/19/94
c
c           If temperature is too cold - we need to freeze above
c           ground live canopy and convert it to standing dead
c           residue
c
c           if (tmnavg.le.tmpmin(itype(nowcrp,iplane))) then
c
c             Compute vegetative biomass from canopy cover (CANCOV).
c
c             biomas = log(1.0-cancov(iplane)) / (-
c    1            bb(itype(nowcrp,iplane)))
c             if (biomas.lt.0.) biomas = 0.0
c             rmagt(iplane) = rmagt(iplane) + biomas
c
c             Reset live canopy cover to zero for perennial present
c
c             cancov(iplane) = 0.0
c           end if
cd    Added by S.Dun, March 17, 2006
             if (cancov(iplane).GT.1.0) then
                  cancov(iplane) = 0.999999
             endif
             biomas = log(1.0-cancov(iplane)) / (-
     1            bb(itype(nowcrp,iplane)))
             if (biomas.lt.0.) biomas = 0.0
             vdmt(iplane) = vdmt(iplane) + biomas
cd    End adding
          end if
c
c       ELSE for FALLOW (imngmt = 3), no live canopy possible
        else
          cancov(iplane) = 0.0
        end if
      end if
c
c     CODE to calculate minimum day length for input latitude moved
c     before the first return statement to prevent model bombing
c     if input value for inrcov is 0.0. (MAKES ytn undefined) dcf
c
c     calculate minimum day length for the input latitude
c
c     Code changed due to problem with tangent function on
c     AT&T 6300 machine using Lahey compiler  - dcf 10/2/91
c
c     original code
c     ytn = tan(deglat / 58.09)
c
      stuff = deglat / 58.09
      ytntop = sin(stuff)
      ytnbot = cos(stuff)
      ytn = ytntop / ytnbot
c
      ch = .4349 * abs(ytn)
      if (ch.ge.1.) then
        h = 0.
      else
        h = acos(ch)
      end if
      daymin = 7.72 * h
c
c     --- IF FLAT OR STANDING RESIDUE COVER OR LIVE CANOPY COVER
c     EXISTS - then initialize the residue and plant mass variables
c
      if (rescov(iplane).gt.0.0.or.cancov(iplane).gt.0.0.or.
     1    rmagt(iplane).gt.0.0) then
c
c       calculate the maximum vegetative dry matter for the
c       previous crop (current residue), the standing residue
c       mass after harvest, and the current standing residue cover
c
        vdmxx = log(1.-0.95) / (-bb(iresd(nowres,iplane)))
        srmhav(iplane) = vdmxx * partcf(iresd(nowres,iplane))
c       strcov = rmagt(iplane) / srmhav(iplane) * basmat(iplane)
c
c       calculate initial residue mass
c
        rilrm(nowres,iplane) = log(1.-rilcov(iplane)) / (-
     1      cf(iresd(nowres,iplane)))
        rigrm(nowres,iplane) = log(1.-rigcov(iplane)) / (-
     1      cf(iresd(nowres,iplane)))
        rmogt(nowres,iplane) = wght1 * rigrm(nowres,iplane) + (1.0-wght1
     1      ) * rilrm(nowres,iplane)
c
c       pop - current dead stalk population ( used in sndrft )
c
c       added trap to prevent divide by zero if user has input a
c       value of zero for cutting height (making partcf = 0.0 )
c       dcf   3/8/94
c
        if(srmhav(iplane) .gt. 0.0)then
          pop(iplane) = (rmagt(iplane)/srmhav(iplane)) * popmat(iplane)
        else
          pop(iplane) = 0.0
        endif
c
        if (pop(iplane).gt.popmat(iplane)) pop(iplane) =
     1      popmat(iplane)
c
c       Initialize submerged residue and dead root mass
c       For submerged residue - assume initial value is 5% of the
c       flat residue amount.  For dead roots, assume that the initial
c       value is 25% of the sum of the flat and standing residues
c
c       Comment out the following 6 lines - change to a user input
c       the values for initial submerged residue mass and dead roots.
c       dcf  5/3/94
c       smrm(nowres,iplane) = rmogt(nowres,iplane) * 0.05
c       rtm(nowres,iplane) = (rmogt(nowres,iplane)+rmagt(iplane)) * 0.25
c
c       if (imngmt(nowres,iplane).eq.2) then
c         if (rtm(nowres,iplane).gt.rtmmax(itype(nowres,iplane)))
c    1        rtm(nowres,iplane) = rtmmax(itype(nowres,iplane))
c       end if
c
c       ... Initial live biomass and cumulated growing degree days are
c       computed based on the initial canopy cover given by the
c       user. If initial canopy cover is 0., then crop variables
c       are set to 0.
c
        if (imngmt(nowcrp,iplane).ne.3) then
c
          if (cancov(iplane).gt.0.0) then
            call initgr(nowcrp)
c
c           Initialize Live root mass and depth variables for Annuals
c           (Perennials are initialized at top - where check for
c           freezing of live canopy is made.)
c
c           ANNUALS - assume root depth at RSR*CANHGT
            if (imngmt(nowcrp,iplane).eq.1) then
              rtd(iplane) = rsr(itype(nowcrp,iplane)) * canhgt(iplane)
              if (rtd(iplane).gt.solthk(nsl(iplane),iplane))
     1            rtd(iplane) = solthk(nsl(iplane),iplane)
              rtmass(iplane) = rsr(itype(nowcrp,iplane)) *
     1            vdmt(iplane)
              if (rtd(iplane).gt.0.60) then
                rtm15(iplane) = 0.42 * rtmass(iplane)
                rtm30(iplane) = 0.28 * rtmass(iplane)
                rtm60(iplane) = 0.20 * rtmass(iplane)
              else if (rtd(iplane).gt.0.30) then
                rtm15(iplane) = 0.45 * rtmass(iplane)
                rtm30(iplane) = 0.30 * rtmass(iplane)
                rtm60(iplane) = 0.25 * rtmass(iplane)
              else if (rtd(iplane).gt.0.15) then
                rtm15(iplane) = 0.60 * rtmass(iplane)
                rtm30(iplane) = 0.40 * rtmass(iplane)
                rtm60(iplane) = 0.0
              else
                rtm15(iplane) = rtmass(iplane)
                rtm30(iplane) = 0.0
                rtm60(iplane) = 0.0
              end if
            end if
          end if
        end if
      end if
c
      return
      end
