      subroutine frznw(lyn,flyn,engtm,frztm,hour)
c
c     +++PURPOSE+++
c     This function is responsible for extending the frost depth
c
c     The purpose of this program is to extend the frost depth with
c     excess heat flow when the heat flow through the frozen layer
c     system is greater than heat flow from the thermal conductivity
c     of the unfrozen soil.  The excess heat flow is balanced by the
c     heat of fusion released by freezing water.
c
c     Author(s):  Shuhui Dun, WSU
c     Date: 04/29/2008
c     Verified by: Joan Wu, WSU
c
c
c     +++ARGUMENT DECLARATIONS+++
      integer  lyn, flyn, hour
      real engtm, frztm
c
c     +++ARGUMENT DEFINITIONS+++
c     lyn   - large layer number
c     flyn  - fine layer number
c     engtm - time for the energy to apply
c     frztm - time to freeze the water of the frozen zone in a fine layer.
c
c     +++PARAMETERS+++
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
c
c     +++COMMON BLOCKS+++
c
      include  'cstruc.inc'
c       read:  iplane
      include 'cupdate.inc'
c       read:  sdate
      include  'cwint.inc'
c       read:  snodpt(iplane),tfrdp(mxplan),tthawd(mxplan),frdp(mxplan),
c              thdp(mxplan),densg(mxplan)
c
      include 'cflgfs.inc'
c     fine layer for frost simulation
c
      include 'cpfrst.inc'
c
      include 'cwater.inc'
c     read: dg(i,iplane)
c
c
c     +++LOCAL VARIABLES+++
c
      integer  i,j
      real     htreq,ceh2o,lhfh2o,qoutnw,frzwat,vardp,vardm,spcav,pfrzw,
     1         varthk
c
c     +++LOCAL DEFINITIONS+++
c     htreq  - Heat (energy), required to freeze soil of current finer layer (J/m^2).
c     ceh2o  - Coefficient of expansion for water (unitless).
c     lhfh2o - Latent heat of fusion of water (J/m^3).
c     qoutnw - energy flux from top for freezing water in frozen zone (W/m^2)
c     frzwat - the ammount of liquid water in frozen of a fine layer (m)
c     spcav  - space available for holding liquid water in the frozen zone
c
c     vardp  - depth variable
c     varsm  - soil moisture variable
c     varthk - thickmess variable
c     varsmc - variable for maximum water flow rate an adjecent layer can supply
c
c     +++DATA INITIALIZATIONS+++
c     Phase change expention coefficient, water to ice
      data   ceh2o/1.1/
c     Latent heat of fusion of ice in J/m3
      data   lhfh2o/3.35e08/

c     +++END SPECIFICATIONS+++
c
      frztm = 0.
      vardm = 0.0
      vardp = 0.0
      varthk = 0.0
c
      if (lyn .gt. 1) vardp = solthk(lyn-1 , iplane)
c
      vardp = vardp + dg(lyn,iplane)/nfine(lyn) * flyn
c
      if (vardp .gt. tilld(iplane)) then
         vardm = tilld(iplane)/kftill + (vardp - tilld(iplane))/kfutil
      else
         vardm = vardp/kftill
      endif
c
c      Heat flux from above
       qoutnw = surtmp(hour) / (dmfrsn + vardm)
c
       if ((qoutnw + qdry).gt.0.) return
c
c      Prevent div by 0 5-7-2008 jrf
c      This needs to be checked.
       if (frozen(lyn,iplane).gt.0.0) then
          frzwat = nwfrzz(lyn,iplane)/frozen(lyn,iplane)
     1               * slfsd(flyn, lyn, iplane)
       else
          frzwat = nwfrzz(lyn,iplane)
       endif
      spcav = ul(lyn,iplane)/dg(lyn,iplane)*slfsd(flyn, lyn, iplane)
     1      - slsic(flyn,lyn,iplane)
      if(spcav .lt. 0.0) spcav = 0.0
c
      if (frzwat.gt.spcav) then
        frzwat = spcav
      endif  
      htreq = lhfh2o * frzwat
c      
      if (htreq .gt. qoutnw*engtm) then
c        water could be frozen by the energy
         pfrzw = qoutnw*engtm / lhfh2o
         frztm = engtm
         varthk = pfrzw/frzwat* slfsd(flyn, lyn, iplane)
      else
         frztm = htreq/qoutnw
         varthk = slfsd(flyn, lyn, iplane) 
      endif
c
      nwfrzz(lyn,iplane) = nwfrzz(lyn,iplane) - frzwat
      frozen(lyn,iplane) = frozen(lyn,iplane) - varthk
c
      if ((frozen(lyn,iplane).lt.0.001) .or.
     1                       (nwfrzz(lyn,iplane).lt. 0.00001)) then
           slsic(flyn,lyn,iplane) = slsic(flyn,lyn,iplane) + frzwat
     1                            + nwfrzz(lyn,iplane)
           nwfrzz(lyn,iplane) = 0.0
           frozen(lyn,iplane) = 0.0
      else
           slsic(flyn,lyn,iplane) = slsic(flyn,lyn,iplane) + frzwat
      endif
c
      return
      end
