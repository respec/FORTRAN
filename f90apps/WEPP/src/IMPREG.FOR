       subroutine impreg
c
c     + + + PURPOSE + + +
c
c     SR IMPREG is to modify the arrangment of flow regimes when
c     soil deposition occurs in an impoundment. This subroutine
c     was added as a new subroutine to the original WEPP code.
c
c     Called from: SR IMPMAI
c     Author(s):  S. Dun
c     Reference in User Guide:
c
c     Version:
c     Date recoded: 01/12/99
c     Recoded by: S.Dun
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmximp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
        include 'cimflg.inc'
c        flags for if a structure exists in an impondment
      include 'cimitf.inc'
c     write: a,b,c,d,e,ha,ht,hlm,a0,a1,a2,l0,l1,l2,qinf,isize
c
      include 'cimpnd.inc'
c        generic variables needed by the impoundment component
c
      include 'cimsed.inc'
c     write: d0,di,d100,sg,ndiv,ctd,cdp,hmin,hset,co,vs,cot
        include 'cimqot.inc'
c        here h is called
c
        include 'cimreg.inc'
c        coefficients used for rearranging the flow regimes.
        include 'cimds.inc'
c        Variables describing the feature of drop spillway.
        include 'cimcv1.inc'
c        Variables describing culvert 1's feature.
           include 'cimcv2.inc'
c        Variables describing culvert 2's feature.
        include 'cimrf.inc'
c        Variables describing rock-fill check dam's feature.
        include 'cimff.inc'
c        Variables describing filterfence,straw bales or trash barriers.
        include 'cimpr.inc'
c        Variables describing perforated riser.
c
        include 'cimcln.inc'
c        Flag for automatically cleanning.
        include 'cimsre.inc'
c        Sediment stage at last rearrangement time.
c                
c     + + + LOCAL VARIABLES + + +
        integer regflag
c
      real c1wd,c1hitn,c1arn,c1hcv
        real c2wd,c2hitn,c2arn,c2hcv
        real rfh,ffh,hd
      real qpr(100), hpr(100), qb, hp, hpdel, qs, apr(5), hs,
     1     y, ko, ab, qw, qoo        ,hb,as
c        
        real hl(15,30)
      integer  is, itf, ir(15), itr,i,j
c
c     + + + LOCAL DEFINITIONS + + +
c
c        regflag     - flag for determining whether or not to rearrange the flow 
c                                  regimes of the impondment. 0 for not, 1 for yes.
c
c        c1arn                 - adjusted area of culvert inlet after sediment stage heigter
c                                  than inlet stage
c        c1hitn                - adjusted height of culvert inlet
c        c1wd                - culvert inlet's width
c        c1hcv                - stage of the bottom of culvert 1's inlet
c
c        c2arn                 - adjusted area of culvert inlet after sediment stage heigter
c                                  than inlet stage
c        c2hitn                - adjusted height of culvert inlet
c        c2wd                - culvert inlet's width
c        c2hcv                - stage of the bottom of culvert 1's inlet
c
c        rfh         - Stage of the bottom of rock-fill check dam's inlet
c
c        ffh         - Inlet bottom stage of filterfence,straw bales or trash barriers
c
c
c     hl(i,j)     - limiting lowest stage for outflow function i, flow regime j
c     i, j        - both are dummy do loop variables  
c     ir          - initial flow regime dummy variable
c     is          - dummy do loop variable
c     itr         - flow regime indicator
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     If sediment stage haven't changed since last rearrangement,
c         this SR will not be used.
        if (abs(hmin(ipond)-sstage).lt.0.001) return
c
         regflag=0
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the original inlet's
c     bottom stage of Drop Spillway, then reassign the coefficients 
c     for corresponding structures.                
c
        if(fids(ipond).ne.0) then
        if(hmin(ipond).gt.dhrs(ipond).or.atcln.eq.1) then
        regflag=1
        do 50 i=1,3
         dnfr(i,ipond) = 1
c        
          do 40 j = 1, 10
            daf(i,j,ipond) = 0.0
            dbf(i,j,ipond) = 0.0
            dcf(i,j,ipond) = 0.0
            ddf(i,j,ipond) = 0.0
            def(i,j,ipond) = 0.0
            dhaf(i,j,ipond) = 0.0
            dhlf(i,j,ipond) = 0.0
            dhtf(i,j,ipond) = 0.0
   40     continue
   50        continue
c
        dbf(3,1,ipond)=10.0**8
c
        dcf(1,1,ipond) = 1.0
        dcf(2,1,ipond) = 1.0
        dcf(3,1,ipond) = 1.0
c
        dhaf(3,1,ipond) = -200.0
c
c     If the sediment stage is higher than the inlet's top stage of
c     the riser, then drop spillway cease to be effective. 
c
       if (atcln.eq.1) then
c        
            dcf(1,2,ipond) = 1.5
            dcf(2,2,ipond) = 0.5
            dcf(3,2,ipond) = 0.5
c
            dhaf(1,2,ipond) = dhrs(ipond)
            dhaf(2,2,ipond) = dhrs(ipond)           
c           
           if(fids(ipond).eq.1) then
            daf(3,2,ipond) = dhblot(ipond) + 0.6 * ddiabl(ipond)
c
            dbf(1,2,ipond) = dcoefw(ipond) * 22.0 / 7.0 * ddiars(ipond)
            dbf(2,2,ipond) = dcoefo(ipond) * 22.0 / 7.0 * ddiars(ipond)
     1           ** 2.0 / 4.0 * (2*32.2)** 0.5
            dbf(3,2,ipond) = 22.0 / 7.0 * ddiabl(ipond) ** 2.0 / 4.0 * 
     1          (2*32.2) ** 0.5/ (1.0+dke(ipond)+dkb(ipond)+dkc(ipond)
     1                  *(dlbl(ipond)+dhrh(ipond))) ** 0.5
c
            dhaf(3,2,ipond) = dhrs(ipond) - (dhrh(ipond)+dsbl(ipond)*
     1                  dlbl(ipond)-0.6*ddiabl(ipond))
c
          else if (fids(ipond).eq.2) then
            daf(3,2,ipond) =dhblot(ipond) + 0.6 * ddiabl(ipond)
c
            dbf(1,2,ipond) = dcoefw(ipond) * 2.0 * (dlenrs(ipond)+
     1                 dwidrs(ipond))
            dbf(2,2,ipond) = dcoefo(ipond)*dlenrs(ipond)*dwidrs(ipond)* 
     1                 (2*32.2) ** 0.5
            dbf(3,2,ipond) = 22.0/7.0*ddiabl(ipond)** 2.0/4.0 *(2*32.2) 
     1          ** 0.5/ (1.0 +dke(ipond) +dkb(ipond) + dkc(ipond)*
     1                  (dlbl(ipond)+dhrh(ipond))) ** 0.5
c
            dhaf(3,2,ipond) = dhrs(ipond)-(dhrh(ipond)+dsbl(ipond)*
     1                  dlbl(ipond)-0.6*ddiabl(ipond))
c
          else if (fids(ipond).eq.3) then
            daf(3,2,ipond) = dhblot(ipond) + 0.6 * dhitbl(ipond)
c
            dbf(1,2,ipond) = dcoefw(ipond) * 2.0 * (dlenrs(ipond)+
     1                 dwidrs(ipond))
            dbf(2,2,ipond) = dcoefo(ipond)*dlenrs(ipond) *dwidrs(ipond)
     1                 * (2*32.2) ** 0.5
            dbf(3,2,ipond) = dhitbl(ipond) * dwdbl(ipond) * (2*32.2) ** 
     1         0.5 / (1.0+dke(ipond)+dkb(ipond)+dkc(ipond)*(dlbl(ipond)
     1                 +dhrh(ipond))) ** 0.5
c
            dhaf(3,2,ipond) = dhrs(ipond) - (dhrh(ipond)+dsbl(ipond)*
     1                  dlbl(ipond)-0.6*dhitbl(ipond))
c
          end if
c
          do 30 i = 1, 3
            dhlf(i,2,ipond) = dhaf(i,2,ipond)
            dhtf(i,2,ipond) = dhrs(ipond)
                  if(dhlf(i,2,ipond).gt.dhtf(i,2,ipond)) 
     1                dhtf(i,2,ipond)=dhlf(i,2,ipond)
            dnfr(i,ipond) = 2
   30                continue        
          endif
        endif
        endif
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the original inlet's
c     bottom stage of culvert 1, then reassign the coefficients 
c     for corresponding structures.                
c
c         
        if(fcv1(ipond).ne.0) then
        if(hmin(ipond).gt.c1h(ipond).or.atcln.eq.1) then
        regflag=1
        do 60 i=4,6
         dnfr(i,ipond) = 1
c        
          do 70 j = 1, 10
            daf(i,j,ipond) = 0.0
            dbf(i,j,ipond) = 0.0
            dcf(i,j,ipond) = 0.0
            ddf(i,j,ipond) = 0.0
            def(i,j,ipond) = 0.0
            dhaf(i,j,ipond) = 0.0
            dhlf(i,j,ipond) = 0.0
            dhtf(i,j,ipond) = 0.0
   70     continue
   60        continue
c
        daf(5,1,ipond) = 10.0**8
c
        dbf(4,1,ipond) = 1.0
        dbf(5,1,ipond) = 1.0
        dbf(6,1,ipond)=10.0**8
c
        dcf(4,1,ipond) = 1.0
        dcf(5,1,ipond) = 10.0**8
        dcf(6,1,ipond) = 1.0
c
        ddf(5,1,ipond) = 1.0
c
        dhaf(6,1,ipond) = -200.0
c
c     If the sediment stage is higher than the inlet's top stage of
c     culvert 1, then culvert 1 cease to be effective. 
c
       if (hmin(ipond).lt.(c1h(ipond)+c1hit(ipond)).or.
     1         atcln.eq.1) then
c
          dnfr(4,ipond) = 2
          dnfr(5,ipond) = 2
          dnfr(6,ipond) = 2
c
c             calculate the changed height and area of culvert's inlet.
c             the inlet shape we consider as following is rectangular.
                   c1wd=c1ar(ipond)/c1hit(ipond)
                   c1hitn=(c1hit(ipond)-(hmin(ipond)-c1h(ipond)))
                   c1arn=c1hitn*c1wd
                   c1hcv=hmin(ipond)
c
                   if(atcln.eq.1) then
                        c1arn=c1ar(ipond)
                        c1hitn=c1hit(ipond)
                        c1hcv=c1h(ipond)
                   endif
c                
          daf(4,2,ipond) = c1arn * c1hitn ** 0.5 * float(c1ncv(ipond))
          daf(5,2,ipond) = c1arn * c1hitn ** 0.5 * float(c1ncv(ipond))
          daf(6,2,ipond) = c1hot(ipond) + 0.6 * c1hit(ipond)
c
          dbf(4,2,ipond) = c1hitn * c1kus(ipond)
          dbf(5,2,ipond) = c1hitn
          dbf(6,2,ipond) = c1ar(ipond) * (2.*32.2) ** 0.5  
     1    / (1.0+c1ke(ipond)+c1kb(ipond)+c1kc(ipond)*c1l(ipond)) ** 0.5 
     1        * float(c1ncv(ipond))
c
          dcf(4,2,ipond) = 1 / c1mus(ipond)
          dcf(5,2,ipond) = 0.5 * c1s(ipond) - c1ys(ipond)
          dcf(6,2,ipond) = 0.5
c
          ddf(5,2,ipond) = c1cs(ipond)
c
          dhaf(4,2,ipond) = c1hcv
          dhaf(5,2,ipond) = c1hcv
          dhaf(6,2,ipond) = dhaf(6,2,ipond)
c
          dhlf(4,2,ipond) = c1hcv
          dhlf(5,2,ipond) = (dhaf(5,2,ipond)-dcf(5,2,ipond)
     1                                    *dbf(5,2,ipond)) + 0.0001
          dhlf(6,2,ipond) = dhaf(6,2,ipond)
c
          dhtf(4,2,ipond) = c1hcv
          dhtf(5,2,ipond) = c1h(ipond) + c1hit(ipond) - 0.0001
                 if(dhlf(5,2,ipond).gt.dhtf(5,2,ipond)) 
     1           dhtf(5,2,ipond)=dhlf(5,2,ipond)
          dhtf(6,2,ipond) = c1hcv
                      if(dhlf(6,2,ipond).gt.dhtf(6,2,ipond))
     1           dhtf(6,2,ipond)=dhlf(6,2,ipond)
       
          endif
        endif
        endif
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the original inlet's 
c     bottom stage of culvert 2, then reassign the coefficients 
c     for corresponding structures.                
c        
        if(fcv2(ipond).ne.0) then
        if(hmin(ipond).gt.c2h(ipond).or.atcln.eq.1) then
        regflag=1
        do 80 i=7,9
         dnfr(i,ipond) = 1
c        
          do 90 j = 1, 10
            daf(i,j,ipond) = 0.0
            dbf(i,j,ipond) = 0.0
            dcf(i,j,ipond) = 0.0
            ddf(i,j,ipond) = 0.0
            def(i,j,ipond) = 0.0
            dhaf(i,j,ipond) = 0.0
            dhlf(i,j,ipond) = 0.0
            dhtf(i,j,ipond) = 0.0
   90     continue
   80        continue
c
        daf(8,1,ipond) = 10.0**8
c
        dbf(7,1,ipond) = 1.0
        dbf(8,1,ipond) = 1.0
        dbf(9,1,ipond)=10.0**8
c
        dcf(7,1,ipond) = 1.0
        dcf(8,1,ipond) = 10.0**8
        dcf(9,1,ipond) = 1.0
c
        ddf(8,1,ipond) = 1.0
c
        dhaf(9,1,ipond) = -200.0
c
c     If the sediment stage is higher than the inlet's top stage of
c     culvert 2, then culvert 2 cease to be effective. 
c
       if (hmin(ipond).lt.(c2h(ipond)+c2hit(ipond)).or.
     1         atcln.eq.1) then
c
          dnfr(7,ipond) = 2
          dnfr(8,ipond) = 2
          dnfr(9,ipond) = 2
c
c          calculate the changed height and area of culvert's inlet.
c          the inlet shape we consider as following is rectangular.
                   c2wd=c2ar(ipond)/c2hit(ipond)
                   c2hitn=(c2hit(ipond)-(hmin(ipond)-c2h(ipond)))
                   c2arn=c2hitn*c2wd
                   c2hcv=hmin(ipond)
c
                   if(atcln.eq.1) then
                        c2arn=c2ar(ipond)
                        c2hitn=c2hit(ipond)
                        c2hcv=c2h(ipond)
                   endif
c                
          daf(7,2,ipond) = c2arn * c2hitn ** 0.5 * float(c2ncv(ipond))
          daf(8,2,ipond) = c2arn * c2hitn ** 0.5 * float(c2ncv(ipond))
          daf(9,2,ipond) = c2hot(ipond) + 0.6 * c2hit(ipond)
c
          dbf(7,2,ipond) = c2hitn * c2kus(ipond)
          dbf(8,2,ipond) = c2hitn
          dbf(9,2,ipond) = c2ar(ipond) * (2.*32.2) ** 0.5  
     1    / (1.0+c2ke(ipond)+c2kb(ipond)+c2kc(ipond)*c2l(ipond)) ** 0.5 
     1        * float(c2ncv(ipond))
c
          dcf(7,2,ipond) = 1 / c2mus(ipond)
          dcf(8,2,ipond) = 0.5 * c2s(ipond) - c2ys(ipond)
          dcf(9,2,ipond) = 0.5
c
          ddf(8,2,ipond) = c2cs(ipond)
c
          dhaf(7,2,ipond) = c2hcv
          dhaf(8,2,ipond) = c2hcv
          dhaf(9,2,ipond) = dhaf(9,2,ipond)
c
          dhlf(7,2,ipond) = c2hcv
          dhlf(8,2,ipond) = (dhaf(8,2,ipond)-dcf(8,2,ipond)
     1                                    *dbf(8,2,ipond)) + 0.0001
          dhlf(9,2,ipond) = dhaf(9,2,ipond)
c
          dhtf(7,2,ipond) = c2hcv
          dhtf(8,2,ipond) = c2h(ipond) + c2hit(ipond) - 0.0001
                 if(dhlf(8,2,ipond).gt.dhtf(8,2,ipond)) 
     1           dhtf(8,2,ipond)=dhlf(8,2,ipond)
          dhtf(9,2,ipond) = c2hcv
                      if(dhlf(9,2,ipond).gt.dhtf(9,2,ipond))
     1           dhtf(9,2,ipond)=dhlf(9,2,ipond)
       
          endif
        endif
        endif
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the original inlet's 
c     bottom stage of the Rock-fill check dam, then reassign the  
c     coefficients for corresponding structure.                
c        
        if(firf(ipond).ne.0) then
        if(hmin(ipond).gt.rhrf(ipond)*3.281.or.atcln.eq.1) then
        regflag=1
c
c        set coefficients for no flow regime
         dnfr(10,ipond) = 1
c        
          do 100 j = 1, 10
            daf(10,j,ipond) = 0.0
            dbf(10,j,ipond) = 0.0
            dcf(10,j,ipond) = 0.0
            ddf(10,j,ipond) = 0.0
            def(10,j,ipond) = 0.0
            dhaf(10,j,ipond) = 0.0
            dhlf(10,j,ipond) = 0.0
            dhtf(10,j,ipond) = 0.0
  100     continue
c
        dbf(10,1,ipond) = 1.0
        dcf(10,1,ipond) = 1.0
c
c     If the sediment stage is higher than the inlet's top stage of
c     the rock-fill checkdam, only 2 flow regimes for this structure. 
c        no flow and overtopping flow.
c
         if(hmin(ipond).ge.rhotrf(ipond)*3.281) then
                dnfr(10,ipond)=2
c
                dhaf(10,2,ipond)=hmin(ipond)
                dhtf(10,2,ipond)=dhaf(10,2,ipond)+0.001
                dhlf(10,2,ipond)=dhtf(10,2,ipond)
                dbf(10,2,ipond) = 1.0
                dcf(10,2,ipond) = 1.0
                ddf(10,2,ipond)=3.087*rwdrf(ipond)*3.281
                def(10,2,ipond)=dhaf(10,2,ipond)
          endif

c
       if (hmin(ipond).lt.(rhotrf(ipond))*3.281.or.atcln.eq.1) then
c
          dnfr(10,ipond) = 3
c
                   if(atcln.eq.1) then
                        rfh=rhrf(ipond)*3.281
                   else
                        rfh=hmin(ipond)
                   endif
c
                dhaf(10,2,ipond)=rfh
                dhtf(10,2,ipond)=dhaf(10,2,ipond)+0.001
                dhlf(10,2,ipond)=dhtf(10,2,ipond)
            daf(10,2,ipond) =rwdrf(ipond)*3.281**3.0
          dbf(10,2,ipond) = rlnrf(ipond)*rarf(ipond)*3.281
          dcf(10,2,ipond) = 1 / rbrf(ipond)
c       
                dhaf(10,3,ipond)=rfh
                dhtf(10,3,ipond)=rhotrf(ipond)*3.281
                dhlf(10,3,ipond)=dhtf(10,3,ipond)
            daf(10,3,ipond) =daf(10,2,ipond)
          dbf(10,3,ipond) =dbf(10,2,ipond)
          dcf(10,3,ipond) =dcf(10,2,ipond)
                ddf(10,3,ipond)=3.087*rwdrf(ipond)*3.281
                def(10,3,ipond)=dhtf(10,3,ipond)
c
          endif
        endif
        endif
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the original inlet's 
c     bottom stage of filterfence or straw bales, then reassign   
c     the coefficients for corresponding structure.                
c        
        if(fiff(ipond).ne.0) then
        if(hmin(ipond).gt.fhff(ipond).or.atcln.eq.1) then
        regflag=1
c
c        set coefficients for no flow regime
         dnfr(12,ipond) = 1
c        
          do 110 j = 1, 10
            daf(12,j,ipond) = 0.0
            dbf(12,j,ipond) = 0.0
            dcf(12,j,ipond) = 0.0
            ddf(12,j,ipond) = 0.0
            def(12,j,ipond) = 0.0
            dhaf(12,j,ipond) = 0.0
            dhlf(12,j,ipond) = 0.0
            dhtf(12,j,ipond) = 0.0
  110     continue
c
c     If the sediment stage is higher than the inlet's top stage of
c     the filterfence or straw bales, only 2 flow regimes for this  
c        structure.no flow and overtopping flow.
c
         if(hmin(ipond).ge.fhotff(ipond)) then
                dnfr(12,ipond)=2
c
                dhaf(12,2,ipond)=hmin(ipond)
                dhtf(12,2,ipond)=dhaf(12,2,ipond)+0.001
                dhlf(12,2,ipond)=dhtf(12,2,ipond)
                dbf(12,2,ipond)=3.087*fwdff(ipond)
          endif

c
       if (hmin(ipond).lt.(fhotff(ipond)).or.atcln.eq.1) then
c
          dnfr(12,ipond) = 3
c
                   if(atcln.eq.1) then
                        ffh=fhff(ipond)
                   else
                        ffh=hmin(ipond)
                   endif
c
                dhaf(12,2,ipond)=ffh
                dhtf(12,2,ipond)=dhaf(12,2,ipond)+0.001
                dhlf(12,2,ipond)=dhtf(12,2,ipond)
            daf(12,2,ipond) =fwdff(ipond)*fvsl(ipond)
c       
                dhaf(12,3,ipond)=ffh
                dhtf(12,3,ipond)=fhotff(ipond)
                dhlf(12,3,ipond)=dhtf(12,3,ipond)
            daf(12,3,ipond) =daf(12,2,ipond)
                if(fiff(ipond).eq.1) then
c                choosing sharp crested weir for filterfence
                        dbf(12,3,ipond) =3.27*fwdff(ipond)
                dcf(12,3,ipond) =0.4/(fhotff(ipond)-fhff(ipond))
     1                                                *fwdff(ipond)
                else
c                choosing broad crested weir for straw bales
                        dbf(12,3,ipond)=3.087*fwdff(ipond)
                endif
                ddf(12,3,ipond)= fhotff(ipond)
c
          endif
        endif
        endif
c
c     IF the impoundment is cleaning automatically at this day 
c        or the sediment stage is higher than the bottom stage of  
c     the slots on a perforated riser, then reassign the  
c     coefficients for corresponding structure.                
c        
        if(fipr(ipond).ne.0) then
        if(hmin(ipond).gt.phd(ipond).or.atcln.eq.1) then
        regflag=1
c
c        set coefficients for no flow regime
        do 120 i= 13,15
         dnfr(i,ipond) = 1
c        
          do 125 j = 1, 10
            daf(i,j,ipond) = 0.0
            dbf(i,j,ipond) = 0.0
            dcf(i,j,ipond) = 0.0
            ddf(i,j,ipond) = 0.0
            def(i,j,ipond) = 0.0
            dhaf(i,j,ipond) = 0.0
            dhlf(i,j,ipond) = 0.0
            dhtf(i,j,ipond) = 0.0
  125      continue            
  120   continue
c
        dbf(13,1,ipond) = 1.0
        dhaf(13,1,ipond) = -0.01
c
          dcf(15,1,ipond)= 1.0
c
c     If the sediment stage is higher than the inlet's top stage of
c     the perforated riser. This structure cease to be effective. 
c
         if(hmin(ipond).le.phr(ipond).or.atcln.eq.1) then
                if(atcln.eq.1) then
                   hb=phb(ipond)
                   hs=phs(ipond)
                   hd=phd(ipond)
                   as=pas(ipond)
                else
                   hb=phb(ipond)+(hmin(ipond)-phd(ipond))
                   hd=hmin(ipond)
                   if(hmin(ipond).ge.(phd(ipond)+phs(ipond)-0.001)) then
                                hs=0.001
                                as=0.0
                        else
                                hs=phs(ipond)-(hmin(ipond)-phd(ipond))
                                as=pas(ipond)
                        endif
                endif
c
c         determining stage discharge relationship for unsubmerged
c         flow in riser
c
          ko = exp(-0.60721+0.329229*pdiab(ipond)/pdiar(ipond))
          ab = 22.0 / 7.0 * pdiab(ipond) ** 2 / 4.0
c
          i = 1
          hpdel = 0.05
          hp = hpdel
          y = -hb
c
  240     qb = pcb(ipond) * ab * (64.4*(hb+y)) ** 0.5
c
          if (i.ge.100) then
            i = 1
            hpdel = hpdel * 2.0
            hp = hpdel
            y = -hb
            go to 240
          end if
c
          if (hp.lt.hs) then
c
            if (y.le.0.0) then
              qs = 2.0 / 3.0 * (pcs(ipond)*pas(ipond)/hs) * 64.4
     1                         ** 0.5 * hp ** 1.5
            else
              qs = (pcs(ipond)*pas(ipond)/hs) * 64.4 ** 0.5 * 
     1             (y*(hp-y)**0.5+(2./3.)*(hp-y)**1.5)
            end if
c
          else
c
            if (hp.le.(phr(ipond)-hd)) then
c
              if (y.le.0.0) then
                qs = (2./3.) * (pcs(ipond)*pas(ipond)/hs) * 64.4 ** 
     1               0.5 * (hp**1.5-(hp-hs)**1.5)
c
              else if (y.le.hs) then
c
                qs = (pcs(ipond)*pas(ipond)/hs) * 64.4 ** 0.5 * 
     1              (y*(hp-y)**0.5+(2./3.)*((hp-y)**1.5-(hp-hs)**1.5))
              else
                qs = (pcs(ipond)*pas(ipond)) * (64.4*(hp-y)) ** 0.5
              end if
c
            else
              qw = pcoefw(ipond) * 22.0 / 7.0 * pdiar(ipond)
     1                         * (hp-(phr(ipond)-hd)) ** 1.5
              qoo = pcoefo(ipond) * 22.0 / 7.0 * pdiar(ipond)
     1             ** 2 / 4.0 * (hp-(phr(ipond)-hd))** 0.5
c
              if (y.le.0.0) then
                qs = (2./3.) * (pcs(ipond)*pas(ipond)/hs) * 64.4 ** 
     1              0.5 * (hp**1.5-(hp-hs)**1.5) + min(qw,qoo)
c
              else if (y.le.hs) then
c
                qs = (pcs(ipond)*pas(ipond)/hs) * 64.4 ** 0.5 * 
     1                          (y*(hp-y)**0.5+(2./3.)*(
     1              (hp-y)**1.5-(hp-hs)**1.5)) + min(qw,qoo)
              else
                qs = (pcs(ipond)*pas(ipond)) * (64.4*(hp-y)) ** 0.5 +
     1              min(qw,qoo)
              end if
c
            end if
c
          end if
c
          if (qb.lt.qs) then
            y = y + 0.0001
c
            if (y.ge.hp) then
              qpr(i) = qb
              hpr(i) = hp
              i = i + 1
              hp = hp + hpdel
              go to 240
c
            else if (y.gt.(phr(ipond)-hd)) then
c
              i = i - 1
              go to 250
            end if
c
            go to 240
c
          else
c
            qpr(i) = qs
            hpr(i) = hp
            i = i + 1
            hp = hp + hpdel
            go to 240
          end if
c
c         run regression routine on hpr and qpr to determine function
c         coefficients
c
  250     call impris(i,hpr,qpr,apr)
c
          daf(13,2,ipond) = 1.0
          daf(14,2,ipond) = pcb(ipond) * ab * 64.4 ** 0.5
c
          dbf(13,2,ipond) = apr(1)
          dbf(15,2,ipond) = 22.0 / 7.0 * pdiabl(ipond) ** 2.0 / 4.0 * 
     1        (2*32.2) ** 0.5 /(1.0+pke(ipond)+pkb(ipond)+pkc(ipond)
     1                *plbl(ipond)+ko)
c
          dcf(13,2,ipond) = apr(2)
          dcf(15,2,ipond) = 0.5
c
          dhaf(13,2,ipond) = hd
          dhaf(14,2,ipond) = phd(ipond)-phb(ipond)
          dhaf(15,2,ipond) = phr(ipond) - (phrh(ipond)+psbl(ipond)*
     1                plbl(ipond)-0.6*pdiabl(ipond))
c
          dhlf(13,2,ipond) = hd
          dhlf(14,2,ipond) = phd(ipond)-phb(ipond)
          dhlf(15,2,ipond) = dhaf(15,2,ipond)
c
          dhtf(13,2,ipond) = hd
          dhtf(14,2,ipond) = hd
          dhtf(15,2,ipond) = hd
c
c         setting outflow functions for flow through a submerged riser
c         (flow regime #3)
c
          daf(13,3,ipond) = 10000000.0
          dbf(13,3,ipond) = 1.0
          dcf(13,3,ipond) = 1.0
c
          dhtf(13,3,ipond) = hpr(i)+hd
c
          dnfr(13,ipond) = 3
          dnfr(14,ipond) = 2
          dnfr(15,ipond) = 2
c 
          endif
        endif
        endif 

        if(regflag.eq.0) return
c
        sstage=hmin(ipond)
c
c     Rearrange the flow regimes.
        itr = 1
c
          do 10 j=1,mxrgm
              ht(j,ipond)=0.0
              hlm(j,ipond)=0.0
                do 20 i=1,mxstc
              a(i,j,ipond)=0.0
              b(i,j,ipond)=0.0
              c(i,j,ipond)=0.0
              d(i,j,ipond)=0.0
              e(i,j,ipond)=0.0
              ha(i,j,ipond)=0.0
20            continue
10            continue
c
        do 260 i = 1, 15
          ir(i) = 1
  260   continue
c
        ht(itr,ipond) = 0.0
c
  280   itf = 0
c
        do 290 is = 1, 15
c
          if (ht(itr,ipond).ge.dhtf(is,ir(is),ipond)) then
              a(is,itr,ipond) = daf(is,ir(is),ipond)
            b(is,itr,ipond) = dbf(is,ir(is),ipond)
            c(is,itr,ipond) = dcf(is,ir(is),ipond)
            d(is,itr,ipond) = ddf(is,ir(is),ipond)
            e(is,itr,ipond) = def(is,ir(is),ipond)
            ha(is,itr,ipond) = dhaf(is,ir(is),ipond)
            hl(is,itr) = dhlf(is,ir(is),ipond)
c
            if (ir(is).eq.(dnfr(is,ipond)+1)) then
              a(is,itr,ipond) = daf(is,(ir(is)-1),ipond)
              b(is,itr,ipond) = dbf(is,(ir(is)-1),ipond)
              c(is,itr,ipond) = dcf(is,(ir(is)-1),ipond)
              d(is,itr,ipond) = ddf(is,(ir(is)-1),ipond)
              e(is,itr,ipond) = def(is,(ir(is)-1),ipond)
              ha(is,itr,ipond) = dhaf(is,(ir(is)-1),ipond)
              hl(is,itr) = dhlf(is,(ir(is)-1),ipond)
            else
              ir(is) = ir(is) + 1
              itf = 1
            end if
c
          else
c
c           continue
c
            a(is,itr,ipond) = daf(is,(ir(is)-1),ipond)
            b(is,itr,ipond) = dbf(is,(ir(is)-1),ipond)
            c(is,itr,ipond) = dcf(is,(ir(is)-1),ipond)
            d(is,itr,ipond) = ddf(is,(ir(is)-1),ipond)
            e(is,itr,ipond) = def(is,(ir(is)-1),ipond)
            ha(is,itr,ipond) = dhaf(is,(ir(is)-1),ipond)
            hl(is,itr) = dhlf(is,(ir(is)-1),ipond)
          end if
c
  290   continue
c
        if (itf.eq.1) then
          itr = itr + 1
          ht(itr,ipond) = ht(itr-1,ipond) + 0.005
        else
          ht(itr,ipond) = ht(itr,ipond) + 0.005
        end if
c
        if (ht(itr,ipond).ge.(hot(ipond)+5.0)) then
          ht(itr,ipond) = hot(ipond) + 200.0
          nt(ipond) = itr
          go to 300
        else
          go to 280
        end if
c
c     determining the stage below which one of the outflow functions
c     blows up
  300   do 320 itr = 1, nt(ipond)
          hlm(itr,ipond) = 0.0
c
          do 310 is = 1, 15
            if (hl(is,itr).ge.hlm(itr,ipond)) then
              hlm(itr,ipond) = hl(is,itr)
            end if
  310     continue
c
  320   continue
c
        atcln=0
c
        return
        end
