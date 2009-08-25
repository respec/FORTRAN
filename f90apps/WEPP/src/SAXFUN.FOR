      subroutine Saxfun(lysoil,varsm,varwtp,varkus)
c
c     + + + PURPOSE + + +
c
c     Estimate soil water potential and unsaturated hydraulic conductivity usingSaxton&Rawl equation
c
c     Called from: 
c     Author(s): Shuhui Dun, WSU
c     Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
c     Soil water characteristics estimates by texture and organic matter for hydraologic solution.
c     Soil SCI. SOC. AM. J., 70, 1569--1578
c
c     Version: 2008.
c     Date recoded: Febuary 26, 2008
c     Verified by: Joan Wu, WSU
c

c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer lysoil
      real varsm,varwtp,varkus
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c      varsm - soil water content
c      varwtp - soil water potital in meter 
c      varkus - unsaturated hydraulic conductivity (m/s)
c      lysoil - soil layer number
c
c     + + + COMMON BLOCKS + + +
      include 'cstruc.inc'
c     Read: iplane
c
      include 'csaxp.inc'
c     Modify: parameters for van Genuchten equation
c
c
c     + + + LOCAL VARIABLES + + +
      real wtpkpa
c
c     + + + LOCAL DEFINITIONS + + +
c
c     sw1500: first solution 1500 kpa soil moisture
c     sw33: first solution 1500 kpa soil moisture
c     sws33:first solution SAT-33 kpa soil moisture
c     s33:  moisture SAT-33 kpa, normal density
c     spaen: first solution air entry tension, kpa
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c
c     + + + DATA INITIALIZATIONS + + +
c      
c     + + + END SPECIFICATIONS + + +
c
c
c
c    Estimate the water potential of the soil layer below the frozen front
c    using Saxton and Rawls, 2006
c
c
      if (varsm.lt.saxfc(lysoil,iplane)) then
c     water potential between 1500kpa and 33kpa
          if (varsm.lt.saxwp(lysoil,iplane)) then
              wtpkpa = 1500.
          else
          wtPkpa = saxA(lysoil,iplane) * varsm**(-saxB(lysoil,iplane))
          endif
c
          if (wtpkpa .gt. 1500) wtpkpa = 1500.
c
      elseif (varsm.ge.saxpor(lysoil,iplane)) then
c     Saturation
          wtpkpa = 0
      else
c     water potential between 33kpa and 0 kpa
          wtpkpa = 33.0 - (33.0 - saxenp(lysoil,iplane))*
     1           (varsm - saxfc(lysoil,iplane))/
     1           (saxpor(lysoil,iplane) - saxfc(lysoil,iplane))
          if (wtpkpa .lt. saxenp(lysoil,iplane)) wtpkpa = 0
      endif
c
c     Convert Kpa to meter of water
      varwtp = -wtpkpa / 10.
c
      if(varsm.lt.saxpor(lysoil,iplane)) then
          varkus = saxks(lysoil,iplane) * (varsm/saxpor(lysoil,iplane))
     1             **(3. + 2.0*saxB(lysoil,iplane))
      else
          varkus = saxks(lysoil,iplane)
      endif
c
c
      return
      end