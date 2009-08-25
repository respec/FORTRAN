      subroutine Saxpar
c
c     + + + PURPOSE + + +
c
c     Estimate Saxton&Rawl equation parameters for a soil
c
c     Called from: SR WINIT
c     Author(s): Shuhui Dun, WSU
c     Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
c     Soil water characteristics estimates by texture and organic matter for hydraologic solution.
c     Soil SCI. SOC. AM. J., 70, 1569--1578
c
c     Version: 2008.
c     Date recoded: Febuary 19, 2008
c     Verified by: Joan Wu, WSU 
c

c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c
c     + + + COMMON BLOCKS + + +
c
      include 'cstruc.inc'
c     Read: iplane
      include 'cwater.inc'
c      read: nsl(iplane)
      include 'csolva1.inc'
c
      include 'csaxp.inc'
c     Modify: parameters for van Genuchten equation
c
c
c     + + + LOCAL VARIABLES + + +
c
      integer i
      real sw1500, sw33, sws33, s33, spaen
c
c     + + + LOCAL DEFINITIONS + + +
c
c     sw1500: first solution 1500 kpa soil moisture
c     sw33: first solution 1500 kpa soil moisture
c     sws33:first solution SAT-33 kpa soil moisture
c     s33:  moisture SAT-33 kpa, normal density
c      spaen: first solution air entry tension, kpa
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
      do 10 i = 1, nsl(iplane)
c      eqation 1 
         sw1500 = - 0.024*sand(i,iplane) 
     1            + 0.487*clay(i,iplane)
     1            + 0.006*orgmat(i,iplane) 
     1            + 0.005*sand(i,iplane)*orgmat(i,iplane)
     1            - 0.013*clay(i,iplane)*orgmat(i,iplane)
     1            + 0.068*sand(i,iplane)*clay(i,iplane)
     1            + 0.031
c
          saxwp(i,iplane) = sw1500 + 0.14*sw1500 - 0.02
c
c      equation 2
         sw33 =   - 0.251*sand(i,iplane) 
     1            + 0.195*clay(i,iplane)
     1            + 0.011*orgmat(i,iplane) 
     1            + 0.006*sand(i,iplane)*orgmat(i,iplane)
     1            - 0.027*clay(i,iplane)*orgmat(i,iplane)
     1            + 0.452*sand(i,iplane)*clay(i,iplane)
     1            + 0.299
c
          saxfc(i,iplane) = sw33 + 1.283*sw33**2 - 0.374*sw33 - 0.015
c
c      equation 3
         sws33 =  + 0.278*sand(i,iplane) 
     1            + 0.034*clay(i,iplane)
     1            + 0.022*orgmat(i,iplane) 
     1            - 0.018*sand(i,iplane)*orgmat(i,iplane)
     1            - 0.027*clay(i,iplane)*orgmat(i,iplane)
     1            - 0.584*sand(i,iplane)*clay(i,iplane)
     1            + 0.078
c
          s33 = sws33 + 0.636*sws33 - 0.107
c
c      eqation 4
          spaen =  - 21.67*sand(i,iplane) 
     1            - 27.93*clay(i,iplane)
     1            - 81.97*s33
     1            + 71.12*sand(i,iplane)*s33
     1            +  8.29*clay(i,iplane)*s33
     1            + 14.05*sand(i,iplane)*clay(i,iplane)
     1            + 27.16
c
          saxenp(i,iplane) = spaen + 0.02*spaen**2 - 0.113*spaen - 0.70
c
c     equation 5
          saxpor(i,iplane) = saxfc(i,iplane) + s33 
     1                       - 0.097*sand(i,iplane) + 0.043
c
c     eqation 14 and 15
            saxB(i,iplane) = (log(1500.) - log(33.))/
     1                     (log(saxfc(i,iplane)) - log(saxwp(i,iplane)))
            saxA(i,iplane) = exp (log(33.) + 
     1                          saxB(i,iplane)*log(saxfc(i,iplane)))
c
c     equation 16
c     The unit of the original saxton ans Rawls is mm/hr.
c     The factor 1./3.6e+6 converts mm/hr to m/s
          saxks(i,iplane) = 1930.*(saxpor(i,iplane) - saxfc(i,iplane))
     1                      **(3. - 1./saxB(i,iplane))*1.0/3.6E+6
10    continue
c
      return
      end