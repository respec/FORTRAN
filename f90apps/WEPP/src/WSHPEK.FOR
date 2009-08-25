      subroutine wshpek
c
c     + + + PURPOSE + + +
c
c     SR WSHPEK calculates peak runoff at the channel outlet
c     using modified methods from either the CREAMS computer
c     model or the Rational equation.
c
c     Called from: SR WSHRUN
c
c     Author(s): Ascough II, C. Baffaut
c
c     Reference in User Guide:
c
c     Version:
c
c     Date recoded:
c
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxtim.inc'
c
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpek.inc'
      include 'cchtmp.inc'
      include 'cdata1.inc'
      include 'cdata3.inc'
      include 'cenrpa1.inc'
      include 'cflags.inc'
      include 'chydrol.inc'
      include 'cimpnd.inc'
      include 'coutchn.inc'
      include 'cpart1.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
      include 'cupdate.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real alpha, peaksu(3), qpmax, rtc, tc, tctmp, volume
      integer i, icnt, ieltmp(3)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     alpha  -
c     peaksu(3) -
c     qpmax -
c     rtc -
c     tc -
c     tctmp -
c     volume
c     i -
c     icnt -
c     ieltmp(3) -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     peak
c     table
c     wshscs
c     wshtc
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     peak runoff into a channel depends on the contributing
c     hillslope, channel, and impoundment elements (one
c     hillslope, one impoundment, or up to three channels
c     may contribute to a channel)
c
c     initialize local variables
c
      do 10 i = 1, 3
        peaksu(i) = 0.0
        ieltmp(i) = 0
   10 continue
c     
      icnt = 0
c     
      call peak(peaksu,tmppkr(nhtop(ielmt)),nhtop(ielmt),ieltmp,icnt)
      call peak(peaksu,peakot(ncleft(ielmt)),ncleft(ielmt),ieltmp,icnt)
      call peak(peaksu,peakot(ncrght(ielmt)),ncrght(ielmt),ieltmp,icnt)
      call peak(peaksu,peakot(nctop(ielmt)),nctop(ielmt),ieltmp,icnt)
      call peak(peaksu,peakot(nitop(ielmt)),nitop(ielmt),ieltmp,icnt)
c     
      if (icnt.eq.1) peakin(ielmt) = peaksu(1)
c     
      if (icnt.gt.1) then
c       
c       more than one element is contributing runoff
c       to the channel - calculate SCS Triangular Hydrographs
c       and superimpose to find a new peak runoff rate
c       
        call wshscs(peaksu,ieltmp,icnt,qpmax)
c       
        peakin(ielmt) = qpmax
c     
      end if
c     
c     do not go through peak runoff discharge calculations for
c     channel element if there is no runoff volume
c     
      if (runvol(ielmt).lt.0.001) then
        peakot(ielmt) = 0.0
        rundur(ielmt) = 0.0
        return
      end if
c     
c     if the final watershed element is a channel then sum
c     the number of runoff events for each month, year, and
c     total at the outlet
c     
      if (ielmt.eq.nelmt) then
        nrunm = nrunm + 1
        nruny = nruny + 1
        nrunt = nrunt + 1
      end if
c     
      call wshtc
c     
      if (ipeak.eq.1) then
c       
c       modified Rational method for peak runoff calculation
c       
c       calculate time of concentration at the outlet of the channel
c       element
c       
        tc = htcc(ielmt)
c       
c       calculation of channel alpha parameter
c       
        if ((idflag.eq.2).or.(idflag.eq.4)) then
c         
c         Case 2 - channel runoff only
c         
          tctmp = tc * 3600.0
c         
c         check for maximum bounds on alpha
c         
          if (tctmp.gt.tr(nf)) then
            walpha(ielmt) = 1.0
          else
c           
c           interpolate value of rainfall amount (rtc) falling
c           within the time of concentration (tctmp)
c           
            call table(2,nf,tr,rr,tctmp,rtc)
c           
            walpha(ielmt) = rtc / rr(nf)
            if (walpha(ielmt).lt.tc/24.0) walpha(ielmt) = tc / 24.0
          end if
c       
        end if
c       
        if ((idflag.eq.3).or.(idflag.eq.4)) then
c         
c         Case 3 - hillslope/channel/impoundment runon only
c         
          if ((tmpvol(nhleft(ielmt)).gt.0.0).or.(tmpvol(nhrght(ielmt))
     1        .gt.0.0).or.(tmpvol(nhtop(ielmt)).gt.0.0)) then
c           
c           check the hillslope runon alphas
c           
            if (tmpvol(nhleft(ielmt)).gt.0.0) then
              alpha = halpha(idelmt(nhleft(ielmt)))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(nhrght(ielmt)).gt.0.0) then
              alpha = halpha(idelmt(nhrght(ielmt)))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(nhtop(ielmt)).gt.0.0) then
              alpha = halpha(idelmt(nhtop(ielmt)))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c         
          end if
c         
          if ((tmpvol(ncleft(ielmt)).gt.0.0).or.(tmpvol(ncrght(ielmt))
     1        .gt.0.0).or.(tmpvol(nctop(ielmt)).gt.0.0)) then
c           
c           check the channel runon alphas
c           
            if (tmpvol(ncleft(ielmt)).gt.0.0) then
              alpha = walpha(ncleft(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(ncrght(ielmt)).gt.0.0) then
              alpha = walpha(ncrght(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(nctop(ielmt)).gt.0.0) then
              alpha = walpha(nctop(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c         
          end if
c         
          if ((tmpvol(nileft(ielmt)).gt.0.0).or.(tmpvol(nirght(ielmt))
     1        .gt.0.0).or.(tmpvol(nitop(ielmt)).gt.0.0)) then
c           
c           check the impoundment runon alphas
c           
            if (tmpvol(nileft(ielmt)).gt.0.0) then
              walpha(nileft(ielmt)) = 1
              alpha = walpha(nileft(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(nirght(ielmt)).gt.0.0) then
              walpha(nirght(ielmt)) = 1
              alpha = walpha(nirght(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c           
            if (tmpvol(nitop(ielmt)).gt.0.0) then
              walpha(nitop(ielmt)) = 1
              alpha = walpha(nitop(ielmt))
              walpha(ielmt) = amax1(walpha(ielmt),alpha)
            end if
c         
          end if
c       
        end if
c       
c       calculate peak runoff for the channel
cd    added by S. Dun 2002/04/10 for incorporating Arthur's work
        if(chkflg.eq.2) then     
          rundur(ielmt) = 24*60*60
          peakot(ielmt) = runvol(ielmt) / rundur(ielmt)
        else
cd    end Adding!  
c        
          peakot(ielmt) = walpha(ielmt) * runvol(ielmt) / (3600.0*tc)
c
cd    Added by S. Dun 2002/04/10
        endif
cd    End adding     
      else
c       
c       modified CREAMS method for peak runoff calculation
c       
c       original CREAMS equation for peak runoff rate calculation 
c       in ft^3/s
c       
c       peakot(ielmt) = 200.0 * (area**0.7) * (slope**0.159) * (vol**
c       1     (0.917*(area**0.0166))) * lw ** (-0.187)
c       
c       area  - contributing watershed drainage area (mile^2)
c       slope - mainstem channel slope (ft/mile )
c       vol   - runoff volume (inches)
c       
c       The CREAMS equation used in WEPP is the metric conversion 
c       to m^3/s from the original CREAMS English equation 
c       
        volume = rofave(ielmt) * 39.37
c       
        peakot(ielmt) = 7.172e-04 * (wsarea(ielmt)**0.7) * (
     1      chslop(ichan)**0.159) * (volume**(0.71764*(wsarea(ielmt)**
     1      0.0166))) * lw ** (-0.187)
c     
      end if
c     
c     compute the effective runoff duration of the event
c     
      if (peakot(ielmt).eq.0.0) write (*,'("wshpek div 0")')
      if (peakot(ielmt).gt.99999999) write(*,'("wshpek inf")')
      
      rundur(ielmt) = runvol(ielmt) / peakot(ielmt)
c     
      return
      end
