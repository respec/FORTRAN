      subroutine sheart(q,sslope,shear)
c
c     + + + PURPOSE + + +
c     Compute top rill width adjustments, Chezy's coefficient for rill
c     flow, flow depth, wetted area, wetted perimeter, hydraulic
c     radius, and shear stress at the end of the slope
c
c     Called from PARAM
c     Author(s): Flanagan
c     Reference in User Guide:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real q, sslope, shear
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     q      : flow discharge (m**3/s)
c     sslope : channel slope (m/m)
c     shear  : flow shear stress (Pa)
c
c     + + + COMMON BLOCKS + + +
c
      include 'cclim.inc'
      include 'cconsta.inc'
c
      include 'cends.inc'
c      modify: wdhtop(mxplan)
c
      include 'cffact.inc'
c
      include 'cslpopt.inc'
c     modify: dpthch(mxplan)
c
      include 'csolvar.inc'
      include 'cstruc.inc'
      include 'cupdate.inc'
c
c     + + + LOCAL VARIABLES + + +
      real dz, tol, u, chezch, sinang, wdthck, wp,
     1    xsarea,dpthch
c
c     + + + LOCAL DEFINITIONS + + +
c
c      u      : portion of uniform flow equation
c      wdthck : test width to check against to see if wider than
c               current rill width
c      chezch : Chezy C - roughness factor
c      dz     : trial valve for channel depth (m)
c      xsarea : cross sectional area of the flow (m**2)
c      wp     : wetted perimeter of the flow (m)
c      hydrad : hydraulic radius (m)
c      sinang : sine of slope angle
c      tol    : tolerance value
c
      save
c
c********************************************************************
c
c     Tolerance value changed by Baffaut, 1996.  dcf 3/97
c     tol = 5.0e-05
      tol = 5.0e-06
      q = abs(q)
c
c     SSLOPE value changed by Baffaut, 1996.  dcf 3/97
c     if (sslope.le.0.0) sslope = 0.00001
      if (sslope.le.0.0) sslope = 0.000001
c
c     compute top rill width (wdhtop). Note that when tillage occurs
c     top rill wdhtop is set to zero in SR CONTIN
c
c     Using Gilley's relationship
c
      if (rwflag(iplane).eq.1) then
        wdthck = 1.13 * q ** 0.303
        if (wdhtop(iplane).lt.wdthck) wdhtop(iplane) = wdthck
      end if
c
      if (wdhtop(iplane).gt.rspace(iplane)) wdhtop(iplane) =
     1    rspace(iplane)
c
c     compute Chezy's coefficient:
c
      chezch = sqrt(8.0*accgav/frctrl(iplane))
c
c     compute rill flow depth (dpthch(iplane)). This is an iterative
c     process to solve the uniform flow equation:
c
c     dpthch(iplane)=((Q/chezch/sqrt(sslope)**
c                    (2/3)/width)*(width+2*dpthch(iplane))**(1/3)
c
      if (q.le.0.) then
        dpthch = 0.0
      else
        u = (q/chezch/sqrt(sslope)) ** (2./3.) / wdhtop(iplane)
        dpthch = 0.2 * q ** .36
   10   dz = dpthch
        dpthch = u * (wdhtop(iplane)+dz+dz) ** (1./3.)
        if (abs(dz/dpthch-1.).gt.tol) go to 10
      end if
c
c     compute wetted area (xsarea), wetted perimeter (wp), and
c     hydraulic radius (hydrad):
c
      xsarea = dpthch * wdhtop(iplane)
      wp = wdhtop(iplane) + 2.0 * dpthch
c
c     check for rill width of 0 causing 0 wetted perimeter
c     4-24-2008 jrf     
c      if (wp.ne.0.0) then
         hydrad(iplane) = xsarea / wp
c      else
c         hydrad(iplane) = 0.0
c      endif
c
c     compute shear stress:
c
c     Correction made to compute shear stress using the SINE of the
c     slope angle, not the tangent of the slope angle as has previously
c     been computed.  This should only impact results greatly on steeper
c     hillslopes.   dcf   1/21/93
c     shears=wtdens*sslope*hydrad*frcsol(iplane)/frctrl(iplane)
c
      sinang = sin(atan(sslope))
      shear = wtdens * sinang * hydrad(iplane) * frcsol(iplane) /
     1    frctrl(iplane)
      return
      end
