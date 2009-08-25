
c     begin include file cstruc.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /struc/    iplane,nplane,ihill,nhill,ivers,imodel,iflget

c     + + + VARIABLE DECLARATIONS + + +

      integer           iplane,nplane,ihill,nhill,ivers,imodel,iflget

c     + + + VARIABLE DEFINITIONS + + +

c     iplane: current overland flow element
c     nplane: number of overland flow elements for a
c             hillslope profile
c     ihill : current hillslope element if multiple
c             hillslopes selected (hillslope/watershed version)
c     nhill : total number of hillslopes if multiple
c             hillslopes selected (hillslope/watershed version)
c     ivers : model version (hillslope,hillslope/watershed,
c             or watershed)
c     imodel: type of simulation (1 - continuous, 2 - single storm)
c     iflget: type of ET method (1 - WEPP original Penman,
c                                2 - Penman_Monteith)    
c     end include file cstruc.inc

