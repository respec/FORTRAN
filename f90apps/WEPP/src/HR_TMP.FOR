      subroutine hr_tmp(itflag,jyear,sdate,hour,temp24,radmj,halfdy,
     1                     rcalsl,cratio,rpoth,tmin,tmax,hradmj,hrtemp)
c
c     +++ PURPOSE +++
c     This subroutine computes the hourly temperature.
c     It was originally incorporated into WINTER, but was split out
c     by Charles R. Meyer 6/6/96 to reduce code complexity.
c
c     +++ ARGUMENT DECLARATIONS +++
      integer itflag,jyear,sdate,hour
      real    temp24,radmj,halfdy,rcalsl,cratio,rpoth,tmin,tmax,hradmj,
     1        hrtemp
c
c     +++ARGUMENT DEFINITIONS+++
c     itflag -
c     jyear  -
c     sdate  -
c     hour   -
c     temp24 -
c     radmj  -
c     halfdy -
c     rcalsl -
c     cratio -
c     rpoth  -
c     tmin   -
c     tmax   -
c     hradmj -
c     hrtemp -

      integer tmpflg
      real temp1,temp2,temp3,diff

      save tmpflg,temp1,temp2,temp3
c
c     +++END SPECIFICATIONS+++
c
c
c      Note -- Tmpadj calculates surface temperature on an hourly basis.
c
      if(itflag.eq.1) then
cd        hradmj=radmj/12
        hradmj=radmj/24
        call tmpadj(hour,halfdy)
        hrtemp=(tmax+tmin)/2
c
      else
        hradmj = rcalsl * cratio/rpoth
        call tmpadj(hour,halfdy)

        if(sdate.gt.1 .or. jyear.gt.1)then
          if(hour.eq.1)then
            diff=abs(hrtemp-temp24)
            if(diff.ge.4)then
              hrtemp=(hrtemp+temp24)/2
              tmpflg=0
            else
              tmpflg=1
            endif
            temp1=hrtemp
          endif

          if(tmpflg.eq.0) then
            if(hour.eq.2)then
              diff=abs(hrtemp-temp1)
              if(diff.ge.4.) hrtemp=(hrtemp+temp1)/2
              temp2=hrtemp
            else if(hour.eq.3) then
              diff=abs(hrtemp-temp2)
              if(diff.ge.4) hrtemp=(hrtemp+temp2)/2
              temp3=hrtemp
            else if(hour.eq.4)then
              diff=abs(hrtemp-temp3)
              if(diff.ge.4) hrtemp=(hrtemp+temp3)/2
            endif
          endif
        endif
      endif
c
      return
      end
