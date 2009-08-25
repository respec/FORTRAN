      subroutine res_dp(lanuse,numres,iplane,resdep)
c
c     +++ PURPOSE +++
c     This function computes total residue depth (for all residue 
c     types) on a single Overland Flow Element (OFE).
c     It was originally incorporated into WINTER, but was split out
c     by Charles R. Meyer 6/7/96 to reduce code complexity.
c
c     +++ ARGUMENT DECLARATIONS +++
      integer lanuse,numres,iplane
      real resdep
c
c     +++ ARGUMENT DEFINITIONS +++
c     lanuse -
c     numres -
c     iplane -
c     resdep -
c
c     +++ PARAMETERS +++
      include 'pmxcrp.inc'
      include 'pmxpln.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxres.inc'
c
c
c     +++ COMMON BLOCKS +++
      include  'ccrpprm.inc'
c       read:  iresd
      include  'ccrpvr5.inc'
c       read:  diam
      include  'ccrpvr1.inc'
c       read:  rmogt
c
c     +++END SPECIFICATIONS+++
c
      integer nowres
      real efsg,pithyFac,woodyFac,hollowFac
c
      resdep = 0.0
c
c XXX WHY ARE WE SETTING NUMRES EQUAL TO 1?  We are limiting
c     our residue depth calculations to only the residue that
c     is currently in register 1 (most recently harvested)?
c     We could easily use all 3 residue registers.  dcf  2/6/97
c
c
c     Determine number of residue groups currently being tracked
c-----
c    7/23/2008 jrf - factors changed to convert residue amount to depth.
c    The factors used to convert the biomass to depth are based on no-till management
c    with 90+% cover. For corn in IA 8700 kg/ha -> 5cm depth, for wheat in WA 
c    10000 kg/ha -> 3cm depth. These values were found in:
c     "Effects of crop residue cover and architecture on heat and 
c            water transfer at the soil surface"
c G. N. Flerchinger, T. J. Sauer, R. A. Aiken,  GeodermaVolume 116, Issues 1-2, 
c Quantifying agricultural management effects on soil properties and processes, 
c September 2003, Pages 217-233.
c (http://www.sciencedirect.com/science/article/B6V67-487N1SV-2/1/f0b7e8958c09af96e9d914cfecbe3c91)
c
c  There was not a factor for woody residues in the above paper. The old factor was greater 
c  than corn or wheat residue by 2-3 times so this relationship was kept. 
c
c  
c------
c
      pithyFac = 0.174
      woodyFac = 0.6
      hollowFac = 0.3
      
      if (iresd(3,iplane).ne.0) then
        numres = 3
      else if (iresd(2,iplane).ne.0) then
        numres = 2
      else
        numres = 1
      end if
c     numres = 1
c
      do 10 nowres = 1,numres
c
c ----- Corn and sorghum (pithy residue).
      if (lanuse .eq. 1) then
        if (iresd(nowres,iplane) .ne. 0) then
          if (diam(iresd(nowres,iplane)) .le. 0.06) then
             if (diam(iresd(nowres,iplane)) .ge. 0.03) then
c                efsg = 0.042   
                 efsg = pithyFac
c
c ------------ Soybeans, cotton, etc.. (woody residue).
             else if (diam(iresd(nowres,iplane)) .ge. 0.007) then
c                efsg = 0.078   
                 efsg = woodyFac
c
c ------------ Winter wheat, spring wheat and alfalfa, etc... (hollow residue).
             else if (diam(iresd(nowres,iplane)) .ge. 0.001) then
c                efsg = 0.027   
                 efsg = hollowFac 
c
c -- XXX - Problem with undefined variable here - for case of Forest
c          Service inputting a tree diameter of 0.5 meters - variable
c          efsg is undefined.   SO  -   set efsg to the value for
c          woody residue (as above  -  0.078)   dcf  9/9/94
c
             else
c                efsg = 0.078
               efsg = woodyFac
             endif
          else
c            efsg = 0.078
            efsg = woodyFac
          endif
        else
          efsg = 0.0
        endif
      else
c        efsg = 0.078
        efsg = woodyFac
      endif
c
      if (efsg .le. 0.0) then
        resdep = 0.0
      else
        resdep = resdep + rmogt(nowres,iplane) / (efsg * 100.0)
      endif
c
 10   continue
c
      return
      end
