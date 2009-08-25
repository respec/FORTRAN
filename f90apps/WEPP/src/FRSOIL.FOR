      subroutine frsoil(sscunf)
c
c     +++PURPOSE+++
c
c     The purpose of this program is to estimate saturated hydraulic conductivity
c     when frost exists.
c     We treat ice as air for saturated hydralic conductivity calculation.
c     Then the saturated K with ice would be as if the unsaturated K with 
c     liqiud water content = porosity - ice water content 
c     
c     Author(s):  Shuhui Dun, WSU
c     Date: 02/28/2008
c     Verified by: Joan Wu, WSU
c
c     +++PARAMETERS+++
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
c
c     +++ARGUMENT DECLARATIONS+++
      real  sscunf(mxnsl,mxplan)
c
c     +++ARGUMENT DEFINITIONS+++
c    
c     sscunf - unfrozen saturated hydraulic conductivity (SSC)
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
      include 'csaxp.inc'
c     Saxton and Ralwls model coefficients
c
      include 'cwater.inc'
c     read: dg(i,iplane)
c
c
c     +++LOCAL VARIABLES+++
c
      integer  frstn,i,j,jend
      real     slks(10,mxnsl,mxplan)
      real     varsm,varwtp,varkus,vardp,tmpvr1,tmpvr2
c
c     +++LOCAL DEFINITIONS+++
c
c     frstn - frost existing fine layer number in a soil layer
c     slks -  satuareated hydraulic conductivity of a fine layer m/s.
c
c     varsm  - soil moisture variable
c     varwtp - water potential variable
c     varkus - unsaturated K varible
c
c     tmpvr1 - variable for mathmatic mean
c     tmpvr2 - variable for harmonic mean
c
c     +++DATA INITIALIZATIONS+++
c
c     +++END SPECIFICATIONS+++
c
      Do 10 i = 1, nsl(iplane)
c
      if (i.gt.LNfrst) then
c     deeper than the frost bottom
         ssc(i,iplane) = sscunf(i, iplane)
         sscv(i,iplane) = sscunf(i, iplane)
      else
c     in frost zone
         tmpvr1 = 0.
         tmpvr2 = 0.
         vardp = dg(i,iplane)/nfine(i)
         jend = nfine(i)
c
         do 20 j = 1, jend
c
c             Estimate unsaturated hydraulic conductivity of a soil
c             using Saxton and Rawls, 2006
c
             if( slsic(j,i,iplane) .gt. 0.001) then
c            frost exists
c
c                as if soil water content at     
                 varsm = saxpor(i,iplane) 
     1                 - slsic(j,i,iplane)/vardp
c
c                kfactor = 1E-5
c                 kfactor = 0.5
c
                 if (varsm .le. 0.01) then
c                forst heave
                      slks(j,i,iplane) = kfactor*sscunf(i,iplane)
                 else
                      call saxfun(i,varsm,varwtp, varkus)
                      if ((varkus/sscunf(i,iplane)).lt.kfactor) then
                         slks(j,i,iplane) = kfactor*sscunf(i,iplane)
                      else
                         slks(j,i,iplane) = varkus
                      endif
                 endif
c
              else
c             no frost
                 slks(j,i,iplane) = sscunf(i,iplane)
              endif
c              
              tmpvr1 = tmpvr1 + vardp*slks(j,i,iplane)
              tmpvr2 = tmpvr2 + vardp/slks(j,i,iplane)
20       continue
c
         ssc(i,iplane) = tmpvr1/dg(i,iplane)
         sscv(i,iplane) = dg(i,iplane)/tmpvr2
c
      endif
10    continue
c
      return
      end
