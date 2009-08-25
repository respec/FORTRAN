      subroutine cutgrz(nowcrp,sdate,iplane)
c
c     ... update perennial crops after cutting and grazing ...
c
c
      integer nowcrp, sdate, iplane
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxgrz.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pntype.inc'
c
      include 'cperen.inc'
      include 'ccrpprm.inc'
      include 'ccrpvr3.inc'
      include 'crinpt1.inc'
c
      if (imngmt(nowcrp,iplane).eq.2.and.mgtopt(nowcrp,iplane).ne.3)
     1    then
        if (sdate.eq.jdharv(nowcrp,iplane)) then
          nnc(iplane) = nnc(iplane) + 1
          if (mgtopt(nowcrp,iplane).eq.1) then
c
c           set harvest date to next cut date if there is one
c
            if (nnc(iplane).le.ncut(nowcrp,iplane)) then
              jdharv(nowcrp,iplane) =
     1            cutday(nnc(iplane),nowcrp,iplane)
            end if
c
          else if (mgtopt(nowcrp,iplane).eq.2) then
c
c           set harvest date to next grazing date if there is one
c
            if (nnc(iplane).le.ncycle(nowcrp,iplane)) then
              jdharv(nowcrp,iplane) = gend(nnc(iplane),nowcrp,iplane)
            end if
          end if
        end if
      end if
      return
      end
