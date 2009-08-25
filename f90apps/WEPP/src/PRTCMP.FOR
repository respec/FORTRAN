c
      subroutine prtcmp(ielmt)
c
c**********************************************************************
c                                                                     *
c  Called from subroutine CONTIN                                      *
c  Generates a default set of particle for the program using the soil *
c  characteristics read in with the initial parameters.               *
c  Calls functions SEDIA and FALVEL                                   *
c                                                                     *
c  Authors: Foster, Flanagan                                          *
c                                                                     *
c  Reference:                                                         *
c                                                                     *
c    Foster, Young and Neibling. 1985. Sediment composition for       *
c    nonpoint source pollution analyses. Trans. ASAE 28(1):133-139.   *
c                                                                     *
c  In subroutine PRTCMP as well as through the rest of the WEPP       *
c  model, the 5 particle size classes used are:                       *
c                                                                     *
c               1   primary clay                                      *
c               2   primary silt                                      *
c               3   small aggregate                                   *
c               4   large aggregate                                   *
c               5   primary sand                                      *
c                                                                     *
c  Note:                                                              *
c                                                                     *
c    The prediction of particle sizes and detached distributions      *
c    needs to be improved since cover conditions on the plots as well *
c    as whether the sediment is coming from rill or interrill areas   *
c    may greatly affect the sizes and fractions.  The following code  *
c    is the best available predictive science as of now.  dcf 3/5/91  *
c                                                                     *
c**********************************************************************
c
c                                                                     *
      real falvel, sedia
      integer ielmt
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxelm.inc'
      include 'pmxprt.inc'
c
c**********************************************************************
c                                                                     *
c  Common Blocks                                                      *
c                                                                     *
c**********************************************************************
c
      include 'cpart.inc'
c
c**********************************************************************
c                                                                     *
c  part variables updated                                             *
c     dia(10,mxplan),spg(10),eqsand(10,mxplan)                        *
c     frac(10,mxplan),fall(10,mxplan)                                 *
c     frcly(10,mxplan),frslt(10,mxplan)                               *
c     frsnd(10,mxplan),frorg(10,mxplan)                               *
c                                                                     *
c**********************************************************************
c
      include 'csolvar.inc'
c
      include 'cstruc.inc'
c
c******************************************************************
c                                                                 *
c   Local Variables                                               *
c     ratiom : ratio of organic matter to clay in the insitu soil *
c     crct   : correction factor to adjust size fractions when    *
c              fraction of large aggregate is too low             *
c     frclyt : used to calculate fraction of clay in large        *
c              aggregates                                         *
c     frcly1 : used to calculate fraction of clay in large        *
c              aggregates                                         *
c     frcly5 : used to calculate fraction of clay in large        *
c              aggregates                                         *
c                                                                 *
c******************************************************************
c
      save
      real ratiom, crct, frclyt, frcly1, f1f2f5
      integer k, i, jflag
c
c     all the arrays within this subroutine have been redimensionalized
c     from mxplan to mxelem.  this is required for the channel erosion
c     component for the watershed version.  since all the planar
c     computations are completed prior to the other elements during
c     the watershed simulation, the index iplane is identical to IELMT
c     up until IELMT exceeds iplane.  therefore, the extension of these
c     arrays should not affect hillslope computations. the common blocks
c     /part/ and /solvar/ have been redimensionalized.  **rv** 25-aug-89
c
      jflag = 0
c
c
c     Set the value to proportion the organic matter -
c     this should be done using the clay of the in-situ soil
c     however, if there is no clay, then use the silt.  If
c     there is no silt, then use the sand  - dcf 3/8/91
c
      if (clay(1,ielmt).gt.0.0) then
        ratiom = orgmat(1,ielmt) / clay(1,ielmt)
      else if (silt(1,ielmt).gt.0.0) then
        ratiom = orgmat(1,ielmt) / silt(1,ielmt)
      else
        ratiom = orgmat(1,ielmt) / sand(1,ielmt)
      end if
c
c     Set or calculate the particle diameters
c
      dia(1,ielmt) = 0.002
      dia(2,ielmt) = 0.010
      dia(4,ielmt) = 0.300
      if (clay(1,ielmt).gt.0.15) dia(4,ielmt) = 2.0 * clay(1,ielmt)
      dia(5,ielmt) = 0.200
c
c     Set the particle specific gravities
c
      spg(1) = 2.60
      spg(2) = 2.65
      spg(3) = 1.80
      spg(4) = 1.60
      spg(5) = 2.65
c
c     Determine the fraction of each detached particle type based
c     upon the original soil texture (improved CREAMS approach)
c
c     Calculate the fraction of particle type 1 - primary clay
c
      if (clay(1,ielmt).gt.0.0.and.clay(1,ielmt).lt.1.0) then
        frac(1,ielmt) = 0.26 * clay(1,ielmt)
      else if (clay(1,ielmt).le.0.0) then
        frac(1,ielmt) = 0.0001
      else
        frac(1,ielmt) = 0.9996
      end if
c
c     Calculate the fraction of particle type 5 - primary sand
c
      frac(5,ielmt) = sand(1,ielmt) * (1.0-clay(1,ielmt)) ** 5.0
      if (frac(5,ielmt).le.0.0) frac(5,ielmt) = 0.0001
c
c     Calculate the diameter and fraction of class 3 - small agg.
c     and the fraction of class 2 - primary silt.
c
c     If the surface soil is 100% clay - no small aggregates can
c     exist - set a mimimal fraction and a diameter.
c
      if (clay(1,ielmt).ge.1.0) then
        dia(3,ielmt) = 0.180
        frac(3,ielmt) = 0.0001
        go to 20
      end if
c
      if (clay(1,ielmt).le.0.25) then
        dia(3,ielmt) = 0.030
        frac(3,ielmt) = 1.8 * clay(1,ielmt)
c
c       If the surface soil has no clay in it, then no small aggregates
c       can exist - set a mimimal fraction
c
        if (frac(3,ielmt).le.0.0) frac(3,ielmt) = 0.0001
        go to 20
      end if
c
      if (clay(1,ielmt).lt.0.60) then
        dia(3,ielmt) = 0.20 * (clay(1,ielmt)-0.25) + 0.030
        if (clay(1,ielmt).ge.0.50) go to 10
        frac(3,ielmt) = 0.45 - 0.6 * (clay(1,ielmt)-0.25)
        go to 20
      end if
c
      dia(3,ielmt) = 0.1
   10 frac(3,ielmt) = 0.6 * clay(1,ielmt)
   20 frac(2,ielmt) = silt(1,ielmt) - frac(3,ielmt)
c
      if (frac(2,ielmt).le.0.0) then
        frac(2,ielmt) = 0.0001
        frac(3,ielmt) = silt(1,ielmt) - frac(2,ielmt)
        if (frac(3,ielmt).le.0.0) frac(3,ielmt) = 0.0001
      end if
c
c     Calculate the diameter and fraction of particle class 4
c     large aggregates and make sure that all size classes have
c     at least a small amount of sediment and that no size
c     classes have negative fractions.
c
      frac(4,ielmt) = 1.0 - frac(1,ielmt) - frac(2,ielmt) -
     1    frac(3,ielmt) - frac(5,ielmt)
c
      if (frac(4,ielmt).le.0.0) then
        crct = 1.0 / (1.0+abs(frac(4,ielmt))+0.0001)
        frac(4,ielmt) = 0.0001
        do 30 k = 1, npart
          frac(k,ielmt) = frac(k,ielmt) * crct
   30   continue
      end if
c
c
c     Determine the composition of each of the particle classes
c     in terms of primary clay, silt, sand, and organic matter.
c
c     Particle class 1 - primary clay
c
      frcly(1,ielmt) = 1.0
      frslt(1,ielmt) = 0.0
      frsnd(1,ielmt) = 0.0
c
c     If there is no clay in the soil surface layer,
c     then there can be no organic matter associated with the
c     primary clay fraction since it does not exist.
c
      if (clay(1,ielmt).gt.0.0) then
        frorg(1,ielmt) = frcly(1,ielmt) * ratiom
      else
        frorg(1,ielmt) = 0.0
      end if
c
c     Particle class 2 - primary silt
c
      frcly(2,ielmt) = 0.0
      frslt(2,ielmt) = 1.0
      frsnd(2,ielmt) = 0.0
c
c     If there is no clay in the soil surface layer, but there
c     is silt, use the silt to proportion the organic matter to
c     the primary silt class.  If no clay or silt, then no organic
c     matter can be assigned to this class
c
      if (clay(1,ielmt).gt.0.0) then
        frorg(2,ielmt) = 0.0
      else if (silt(1,ielmt).gt.0.0) then
        frorg(2,ielmt) = ratiom
      else
        frorg(2,ielmt) = 0.0
      end if
c
c     Particle class 3 - small aggregate
c
c
c     If no clay or silt in the surface profile then there can
c     be no clay or silt or associated organic matter in the
c     small aggregate class.
c
      if (clay(1,ielmt).gt.0.0.and.silt(1,ielmt).gt.0.0) then
c
        frcly(3,ielmt) = clay(1,ielmt) / (clay(1,ielmt)+silt(1,ielmt))
        frslt(3,ielmt) = silt(1,ielmt) / (clay(1,ielmt)+silt(1,ielmt))
        frorg(3,ielmt) = frcly(3,ielmt) * ratiom
      else
        frcly(3,ielmt) = 0.0
        frslt(3,ielmt) = 0.0
        frorg(3,ielmt) = 0.0
      end if
c
      frsnd(3,ielmt) = 0.0
c
c     Particle class 4 - large aggregate
c
      if (frac(4,ielmt).gt.0.0001) then
c
        frcly(4,ielmt) = (clay(1,ielmt)-frac(1,ielmt)-(frcly(3,ielmt)*
     1      frac(3,ielmt))) / frac(4,ielmt)
        if (frcly(4,ielmt).lt.0.0.or.frcly(4,ielmt).gt.1.0)
     1      frcly(4,ielmt) = 0.0
c
        frslt(4,ielmt) = (silt(1,ielmt)-frac(2,ielmt)-(frslt(3,ielmt)*
     1      frac(3,ielmt))) / frac(4,ielmt)
        if (frslt(4,ielmt).lt.0.0.or.frslt(4,ielmt).gt.1.0)
     1      frslt(4,ielmt) = 0.0
c
        frsnd(4,ielmt) = (sand(1,ielmt)-frac(5,ielmt)) / frac(4,ielmt)
c
        if (frsnd(4,ielmt).lt.0.0.or.frsnd(4,ielmt).gt.1.0)
     1      frsnd(4,ielmt) = 0.0
c
        frorg(4,ielmt) = frcly(4,ielmt) * ratiom
c
      else
        frcly(4,ielmt) = 0.0
        frslt(4,ielmt) = 0.0
        frsnd(4,ielmt) = 0.0
        frorg(4,ielmt) = 0.0
      end if
c
c     Particle class 5 - primary sand
c
      frcly(5,ielmt) = 0.0
      frslt(5,ielmt) = 0.0
      frsnd(5,ielmt) = 1.0
c
c     Check to be sure that there is some clay in soil surface
c     layer, or some silt in this layer.  If not, all organic matter
c     must then be associated with the primary sand
c
      if (clay(1,ielmt).gt.0.0.or.silt(1,ielmt).gt.0.0) then
        frorg(5,ielmt) = 0.0
      else
        frorg(5,ielmt) = ratiom
      end if
c
c
      frclyt = 0.5 * clay(1,ielmt)
      frcly1 = 0.95 * frclyt
c
c     Check to make sure that large aggregate class does
c     not have too little of a fraction of primary clay within
c     it.  If it does - proportion more of the clay into size
c     class 4 (large aggregate) then go back to statement 40
c     and reproportion all of the size classes.
c
      if (clay(1,ielmt).lt.1.0.and.jflag.eq.0) then
c
        if (frcly(4,ielmt).lt.frcly1) then
          f1f2f5 = frac(1,ielmt) + frac(2,ielmt) + frac(5,ielmt)
          frcly(4,ielmt) = frclyt
c
c
          frac(3,ielmt) = (clay(1,ielmt)-frcly(4,ielmt)-frac(1,ielmt)+
     1        frcly(4,ielmt)*f1f2f5) / (frcly(3,ielmt)-frcly(4,ielmt))
          if (frac(3,ielmt).le.0.0) frac(3,ielmt) = 0.0001
          jflag = 1
          go to 20
        end if
      end if
c
c     convert diameter from millimeters to meters
c
      do 40 k = 1, npart
        dia(k,ielmt) = dia(k,ielmt) / 1000.0
   40 continue
c
c     Calculate the fall velocities and equivalent sand diameters
c     for each particle class.
c
      do 50 i = 1, npart
        fall(i,ielmt) = falvel(spg(i),dia(i,ielmt))
        eqsand(i,ielmt) = sedia(2.65,fall(i,ielmt))
   50 continue
      return
      end
