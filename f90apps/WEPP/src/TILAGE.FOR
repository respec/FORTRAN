      subroutine tilage(nowcrp)
c
c     + + + purpose + + +
c     Reads in the crop, tillage, and management options and variables
c     for each year of the simulation.  Computes the initial average
c     sand fraction (avsand) and clay fraction (avclay) of the primary
c     and secondary tillage layers, which is needed by infpar.  Also,
c     prorates the physical properties of the soil layers within the
c     tillage layers; changes the secondary & primary tillage layers
c     to layers 1 & 2, respectively; and re-assigns the subsequent
c     soil layers as layer 3 and following.
c
c     Called from contin
c     Cuthor(s): alberts, ghiddey, ferris, arnold, meyer
c     Reference in user guide:
c
c     Changes:
c              1) Removed references to the following common blocks,
c                 since none of their variables were used:
c                     ccliyr
c                     ccrpvr1
c                     ccrpvr3
c                     crinpt2
c                     crinpt6
c                     cupdate
c                     cnew1.
c              2) Removed reference to include file ptilty.inc
c                 since it's parameter is not used.
c              3) Nowcrp, the variable passed to tilage, was declared
c                 to be an integer, and dimensioned to 10 in a second
c                 integer declaration.  this was corrected.
c              4) Converted "do 40" loop to an implied-do.
c              5) Converted several one-line block-if's to simple if's.
c              6) Converted "do-50" loop with an embedded-goto to the
c                 format prescribed by the wepp coding convention.
c              7) Local variable watcon is computed and never used.
c                 it was removed.
c              8) In the calculations to set up the secondary & primary
c                 tillage layers as layer numbers 1 & 2, the partial
c                 layer (from the original layers input by the user)
c                 below them was ignored.  this was corrected.
c              9) tilflg is now in common block ccontcv.inc jca2 8/31/93
c
c        note: Avsand, sand(1, iplane), and sand(2, iplane) all contain
c              the same value.  They are accessed by different routines.
c              This needs to be cleaned up!  (the same holds true for
c              avclay, clay(1, iplane), and clay(2, iplane).)
c              crm -- 8/21/92.
c
c     Version: this module recoded from wepp version 92.25.
c     Date recoded: 08/18/92 - 08/25/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxres.inc'
      include 'pmxhil.inc'
      include 'pntype.inc'
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxpnd.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c      integer nowcrp(mxplan)
c
c     + + + argument definitions + + +
c     nowcrp - current crop
c
      include 'ccdrain.inc'
c     modify:drseq(ntype,ntype)
c     + + + common blocks + + +
      include 'ccntour.inc'
c     modify: conseq(mxcrop,mxplan)
c
      include 'ccontcv.inc'
c      write: tilseq(mxcrop,mxplan)
c      modify: tilflg
c
      include 'ccover.inc'
c       read: lanuse(mxplan)
c      write: ntill(mxplan)
c
      include 'ccrpout.inc'
c     modify: bd(mxnsl,mxplan)
c
      include 'ccrpprm.inc'
c       read: itype(mxcrop,mxplan), grazig(mxplan),
c             resmgt(mxcrop,mxplan)
c     modify: rw(mxcrop,mxplan), nycrop(mxplan)
c      write: jdplt(mxcrop,mxplan), jdharv(mxcrop,mxplan)
c
      include 'ccrpvr5.inc'
c       read: pltsp(ntype)
c
      include 'cinpman1.inc'
c       read: transfer of management input from new variables to
c             original variable names
c
      include 'cinpop.inc'
c       read: frmov1, jdmov1
c
      include 'cparame.inc'
c     modify: por(mxnsl,mxplan), sat(mxplan)
      include 'cnew.inc'
c       read: manver
c
      include 'cperen.inc'
c     modify: imngmt(mxcrop,mxplan), cutday(mxcut,mxplan),
c             mgtopt(mxplan), ncut(mxplan), ncycle(mxplan),
c      write: jdherb(mxcrop,mxplan), jdburn(mxcrop,mxplan),
c             fbrnag(mxcrop,mxplan), fbrnog(mxcrop,mxplan),
c             frmove(mxcrop,mxplan), frcut(mxcrop,mxplan),
c             jdcut(mxcrop,mxplan), jdmove(mxcrop,mxplan),
c             digest(mxplan), jdstop(mxcrop,mxplan), nnc(mxplan)
c
      include 'crinpt1.inc'
c     modify: gend(mxgraz,mxplan),
c      write: animal(mxplan), bodywt(mxplan), suppmt(mxplan),
c             digmin(mxplan), gday(mxgraz,mxplan),
c             ssday(mxgraz,mxplan), send(mxgraz,mxplan), area(mxplan),
c             jgraz(mxplan), access(mxplan), woody(mxplan), ianflg
c
      include 'crinpt3.inc'
c     modify: jfdate(mxplan)
c      write: alter(mxplan), burned(mxplan), change(mxplan),
c             hurt(mxplan), reduce(mxplan)
c
      include 'crinpt4.inc'
c     modify: ihdate(mxplan)
c      write: active(mxplan), herb(mxplan),
c             update(mxplan), regrow(mxplan), dleaf(mxplan)
c
      include 'csolvar.inc'
c      write: sand(mxnsl,mxplan),orgmat(mxnsl,mxplan),
c             silt(mxnsl,mxplan),clay(mxnsl,mxplan),
c             rfg(mxnsl,mxplan), cec(mxnsl,mxplan)
c
c
      include 'cstmflg.inc'
c
c******************************************************************
c     stmflg variable read
c     year
c******************************************************************
c
      include 'cstruc.inc'
c       read: nplane
c     modify: iplane
c
      include 'ctemp.inc'
c       read: sand1(mxnsl,mxplan), clay1(mxnsl,mxplan),
c             orgma1(mxnsl,mxplan), rfg1(mxnsl,mxplan),
c             cec1(mxnsl,mxplan), nslorg(mxnsl,mxplan),
c             ssc1(mxnsl,mxplan), bd1(mxnsl,mxplan),
c             thetf1(mxnsl,mxplan), solth1(mxnsl,mxplan),
c             avke(mxplan)
c     modify: thetd1(mxnsl,mxplan),
c             avsand(mxplan), avclay(mxplan)
c
      include 'ctillge.inc'
c     modify: tillay(2,mxplan)
c
      include 'cwater.inc'
c     modify: solthk(mxnsl,mxplan), dg(mxnsl,mxplan), st(mxnsl,mxplan)
c             thetdr(mxnsl,mxplan)
c      write: thetfc(mxnsl,mxpan), ssc(mxnsl,mxplan), nsl
c
c     + + + LOCAL VARIABLES + + +
c      integer conflg
c
c     + + + LOCAL DEFINITIONS + + +
c     ii     -
c     yy     - depth to bottom of previous soil layer
c     icount - soil layers found within the primary tillage layer
c     xx     - fraction of primary tillage layer represented by the
c              the current soil layer.
c     conflg - flag.  used to determine whether a contour sequence
c              occurs on an ofe below an ofe *without* a contour
c              sequence (a "no-no").
c
c     + + + SAVES + + +
c
c     + + + DECLARATIONS
      integer nowcrp(mxplan), conflg, manndx, i, icount, ii, kk
      real tiltmp, xx, yy
c
c     + + + SUBROUTINES CALLED + + +
c     yldopt
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + OUTPUT FORMATS + + +
c
c     + + + INPUT FORMATS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     read in number of crops per year and convert tillage information
c     to the new management format.
c
      conflg = 0
      do 90 iplane = 1, nplane
c       *** L0 IF ***
c       cropland
        if (lanuse(iplane).eq.1) then
          nowcrp(iplane) = 1
c
c         number of crops /year for each year
c
          call eatcom(12)
          read (12,*) nycrop(iplane)
c
c
c         -------- for each crop, determine plant type, cropping system, tillage
c         sequence, contour set, and primary & secondary tillage
c         depth.
c
          do 30 i = 1, nycrop(iplane)
            call eatcom(12)
            read (12,*) manndx
            itype(i,iplane) = ityp1(manndx)
            imngmt(i,iplane) = imngm1(manndx)
            tilseq(i,iplane) = tilse1(manndx)
            conseq(i,iplane) = conse1(manndx)
            drseq(i,iplane) = drseq1(manndx)
            if (drseq(i,iplane).gt.0) idrain(iplane) = 1
c
c           ------------ prevent secondary tillage depth from being
c           greater than primary.
c
            if (tillay(1,iplane).gt.tillay(2,iplane)) then
              tiltmp = tillay(2,iplane)
              tillay(2,iplane) = tillay(1,iplane)
              tillay(1,iplane) = tiltmp
            end if
c
c           ------------ if entered as zero, use default values.
c
            if (tillay(2,iplane).le.0.0) tillay(2,iplane) = 0.2
            if (tillay(1,iplane).le.0.0) tillay(1,iplane) = 0.1
c
c           ------------ prevent tillage depths from being greater
c           than the depths of the soil profile.
c
            if (tillay(2,iplane).gt.solth1(nslorg(iplane),iplane))
     1          tillay(2,iplane) = solth1(nslorg(iplane),iplane)
            if (tillay(1,iplane).gt.solth1(nslorg(iplane),iplane))
     1          tillay(1,iplane) = solth1(nslorg(iplane),iplane)
c
c           ------------ if contouring is used for *any* crop on this ofe,
c           this year, set conflg.
            if (conseq(i,iplane).eq.0) conflg = 1
c
c
c           *** L1 IF ***
c           read management options for annual crops
c
            if (imngmt(i,iplane).eq.1.or.imngmt(i,iplane).eq.3) then
c
              jdplt(i,iplane) = jdpl1(manndx)
              jdharv(i,iplane) = jdhar1(manndx)
              jdstop(i,iplane) = 0
              rw(i,iplane) = r1(manndx)
              resmgt(i,iplane) = resmg1(manndx)
c
c             following line moved here 3/26/93 by sjl
c
              if (rw(i,iplane).le.0) rw(i,iplane) =
     1            pltsp(itype(i,iplane))
c
c             ...set herbicide date if resmgt(i,iplane) = 1
c
              if (resmgt(i,iplane).eq.1) then
c
                jdherb(i,iplane) = jdher1(manndx)
                jdslge(i,iplane) = 0
                jdburn(i,iplane) = 0
                if(manver.lt.98.3)jdmove(i,iplane) = 0
                jdcut(i,iplane) = 0
                fbrnog(i,iplane) = 0.0
                fbrnag(i,iplane) = 0.0
                frcut(i,iplane) = 0.0
                if(manver.lt.98.3)frmove(i,iplane) = 0.0
c
c             ...set burn info if resmgt(i,iplane) = 2
c
              else if (resmgt(i,iplane).eq.2) then
c
                jdburn(i,iplane) = jdbur1(manndx)
                fbrnag(i,iplane) = fbrna1(manndx)
                fbrnog(i,iplane) = fbrno1(manndx)
                jdherb(i,iplane) = 0
                jdslge(i,iplane) = 0
                if(manver.lt.98.3)jdmove(i,iplane) = 0
                jdcut(i,iplane) = 0
                frcut(i,iplane) = 0.0
                if(manver.lt.98.3)frmove(i,iplane) = 0.0
c
c             ...set silage harvest date if resmgt(i,iplane)=3
c
              else if (resmgt(i,iplane).eq.3) then
c
                jdslge(i,iplane) = jdslg1(manndx)
                jdherb(i,iplane) = 0
                jdburn(i,iplane) = 0
                if(manver.lt.98.3)jdmove(i,iplane) = 0
                jdcut(i,iplane) = 0
                fbrnog(i,iplane) = 0.0
                fbrnag(i,iplane) = 0.0
                frcut(i,iplane) = 0.0
                if(manver.lt.98.3)frmove(i,iplane) = 0.0
c
c             ...set cutting info if resmgt(i,iplane) = 4
c
              else if (resmgt(i,iplane).eq.4) then
c
                jdcut(i,iplane) = jdcu1(manndx)
                frcut(i,iplane) = frcu1(manndx)
                jdherb(i,iplane) = 0
                jdslge(i,iplane) = 0
                jdburn(i,iplane) = 0
                if(manver.lt.98.3)jdmove(i,iplane) = 0
                fbrnog(i,iplane) = 0.0
                fbrnag(i,iplane) = 0.0
                if(manver.lt.98.3)frmove(i,iplane) = 0.0
c
c             ...set residue removal info if resmgt(i,iplane) =5 and
c                management file version LT98.3
c
              else if (manver.lt.98.3.and.resmgt(i,iplane).eq.5) then

                jdmove(i,iplane) = jdmov1(manndx)
                frmove(i,iplane) = frmov1(manndx)
                jdherb(i,iplane) = 0
                jdslge(i,iplane) = 0
                jdburn(i,iplane) = 0
                jdcut(i,iplane) = 0
                fbrnog(i,iplane) = 0.0
                fbrnag(i,iplane) = 0.0
                frcut(i,iplane) = 0.0
c
c             ...no residue management selected if resmgt(i,iplane)=6
c
              else
                jdherb(i,iplane) = 0
                jdslge(i,iplane) = 0
                jdburn(i,iplane) = 0
                jdcut(i,iplane) = 0
                if(manver.lt.98.3)jdmove(i,iplane) = 0
                fbrnog(i,iplane) = 0.0
                fbrnag(i,iplane) = 0.0
                frcut(i,iplane) = 0.0
                if(manver.lt.98.3)frmove(i,iplane) = 0.0
              end if
c
c
c             estimate potential yield.
c             original code:
c             call yldopt(itype(i,iplane),i,iplane)
              call yldopt(itype(i,iplane),i,iplane,imngmt(i,iplane))
c
c
c           *** L1 ELSE-IF ***
c           read management options for perennial crops
c
            else if (imngmt(i,iplane).eq.2) then
c
              jdplt(i,iplane) = jdpl1(manndx)
              rw(i,iplane) = r1(manndx)
              mgtopt(i,iplane) = mgtop1(manndx)
              jdharv(i,iplane) = jdhar1(manndx)
              jdsene(i,iplane) = jdhar1(manndx)
              jdstop(i,iplane) = jdsto1(manndx)
              if (rw(i,iplane).le.0) rw(i,iplane) =
     1            pltsp(itype(i,iplane))
              call yldopt(itype(i,iplane),i,iplane,imngmt(i,iplane))
c
c             ...set cutting info if mgtopt(i,iplane)=1
c
c
              if (mgtopt(i,iplane).eq.1) then
c
                ncut(i,iplane) = ncu1(manndx)
c
c               ...set each cutting day for perennial crops
c
                do 10, kk = 1, ncut(i,iplane)
                  cutday(kk,i,iplane) = cutda1(manndx,kk)
   10           continue
c
c               estimate potential yield.
c               original code:
c               call yoptp(itype(i,iplane))
c
                nnc(iplane) = 1
c               yld(i,iplane) = yild(1,i,iplane)
                jdharv(i,iplane) = cutday(1,i,iplane)
c
              else if (mgtopt(i,iplane).eq.2) then
c               read(12,*) (gday(k,i,iplane), gend(k,i,iplane),
c               1                        animal(k,i,iplane), bodywt(k,i,iplane),
c               2                        area(iplane), digest(itype(i,iplane)),
c               3                        k = 1,ncycle(i,iplane))
                ncycle(i,iplane) = ncycl1(manndx)
                do 20 kk = 1, ncycle(i,iplane)
                  gday(kk,i,iplane) = gda1(manndx,kk)
                  gend(kk,i,iplane) = gen1(manndx,kk)
                  animal(kk,i,iplane) = anima1(manndx,kk)
                  bodywt(kk,i,iplane) = bodyw1(manndx,kk)
                  area(iplane) = are1(manndx,kk)
                  digest(itype(i,iplane)) = diges1(manndx,kk)
   20           continue
c
                jdharv(i,iplane) = gend(1,i,iplane)
                nnc(iplane) = 1
              end if
c
c           *** L1 ENDIF ***
            end if
c
   30     continue
c
c         ---------- if this is not the top ofe, *and* contouring is used
c         for *any* crop on this ofe this year, or on an ofe above
c         this one this year, print an error message and abort
c         execution.
c
          if (conflg.eq.1.and.iplane.gt.1) then
            do 40 i = 1, nycrop(iplane)
              if (conseq(i,iplane).ne.0) then
                write (6,1000) iplane
c                stop
                 conseq(i,iplane)=0
              end if
   40       continue
          end if
c
c       *** L0 ELSE ***
c       rangeland
        else
c
          nowcrp(iplane) = 1
c         number of crops /year for each year
c
          call eatcom(12)
          read (12,*) nycrop(iplane)
          nycrop(iplane) = 1
          read (12,*) manndx
          itype(1,iplane) = ityp1(manndx)
          jfdate(iplane) = jfdat1(manndx)
          ihdate(iplane) = ihdat1(manndx)
          grazig(iplane) = grazi1(manndx)
          ntill(iplane) = 1
          if (grazig(iplane).gt.0) ianflg = 1
c
c         ...set if herbicide is applied.
c
          if (ihdate(iplane).gt.0) then
            active(iplane) = activ1(manndx)
            woody(iplane) = wood1(manndx)
            herb(iplane) = her1(manndx)
            update(iplane) = updat1(manndx)
            regrow(iplane) = regro1(manndx)
            dleaf(iplane) = dlea1(manndx)
          end if
c
c         ...set if burning occurrs.
c
          if (jfdate(iplane).gt.0) then
            alter(iplane) = alte1(manndx)
            burned(iplane) = burne1(manndx)
            change(iplane) = chang1(manndx)
            hurt(iplane) = hur1(manndx)
            reduce(iplane) = reduc1(manndx)
          end if
c
c         ...set grazing info if grazig flag =1
c
          if (grazig(iplane).eq.1) then
            area(iplane) = are2(manndx)
            access(iplane) = acces1(manndx)
            suppmt(iplane) = suppm1(manndx)
            jgraz(iplane) = jgra1(manndx)
            digmin(iplane) = digmi1(manndx)
            digmax(iplane) = digma1(manndx)
c
            do 50 i = 1, jgraz(iplane)
              gday(i,1,iplane) = gda1(manndx,i)
              gend(i,1,iplane) = gen1(manndx,i)
              ssday(i,iplane) = ssda1(manndx,i)
              send(i,iplane) = sen1(manndx,i)
              animal(i,1,iplane) = anima1(manndx,i)
              bodywt(i,1,iplane) = bodyw1(manndx,i)
   50       continue
c
c         ...no other plant types supported at this time
c
          end if
c**************         end new input        ****************
c
c       *** L0 ENDIF ***
        end if
c
c       *** M0 IF ***
c       get average soil properties based on primary and secondary tillage
c       layers
c
c       if(tillay(1,iplane).ne..0.and.tillay(2,iplane).ne..0) then
        if (tilflg.eq.0) then
c
c         compute avgs. of soil characteristics for primary &
c         secondary tillage layers
c
c         -------- initialize
          avsand(iplane) = 0.0
          avclay(iplane) = 0.0
          sand(2,iplane) = 0.0
          clay(2,iplane) = 0.0
          orgmat(2,iplane) = 0.0
          thetdr(2,iplane) = 0.0
          rfg(2,iplane) = 0.0
          thetfc(2,iplane) = 0.0
          cec(2,iplane) = 0.0
          bd(2,iplane) = 0.0
          ssc(2,iplane) = 0.0
          yy = 0.0
          icount = 0
c
c
          i = 0
   60     continue
          i = i + 1
c
c         for each soil layer (i), determine fraction of each layer
c         (xx) within the primary tillage layer (tillay(2,iplane)) .
c
c         ---------- layer is entirely within the primary tillage layer.
          if ((solth1(i,iplane)-tillay(2,iplane)).lt.0.0001) then
            xx = (solth1(i,iplane)-yy) / tillay(2,iplane)
            icount = icount + 1
          else
c           ------------ this is the top layer.
            if (i.eq.1) then
              xx = 1.
            else
              xx = (tillay(2,iplane)-yy) / tillay(2,iplane)
            end if
          end if
c
c         iteratively compute avgs. of soil physical properties for the
c         soil layers within the primary tillage layer (soil layer 2).
c
          sand(2,iplane) = sand(2,iplane) + sand1(i,iplane) * xx
          clay(2,iplane) = clay(2,iplane) + clay1(i,iplane) * xx
          orgmat(2,iplane) = orgmat(2,iplane) + orgma1(i,iplane) * xx
          thetdr(2,iplane) = thetdr(2,iplane) + thetd1(i,iplane) * xx
          thetfc(2,iplane) = thetfc(2,iplane) + thetf1(i,iplane) * xx
          rfg(2,iplane) = rfg(2,iplane) + rfg1(i,iplane) * xx
          cec(2,iplane) = cec(2,iplane) + cec1(i,iplane) * xx
          ssc(2,iplane) = ssc(2,iplane) + ssc1(i,iplane) * xx
c
c         Following code allows user input value for effective
c         hydraulic conductivity to be used.
c
          if (avke(iplane).gt.0.0) ssc(2,iplane) = avke(iplane)
c
          bd(2,iplane) = bd(2,iplane) + bd1(i,iplane) * xx
          yy = solth1(i,iplane)
          if ((i.lt.nslorg(iplane)).and.(solth1(i,iplane).lt.
     1        tillay(2,iplane))) go to 60
c
c
c         assume the same averages for soil physical properties in
c         in the secondary tilage layer (soil layer 1), as in the
c         primary tilage layer.
c
          sand(1,iplane) = sand(2,iplane)
          clay(1,iplane) = clay(2,iplane)
          orgmat(1,iplane) = orgmat(2,iplane)
          thetdr(1,iplane) = thetdr(2,iplane)
          thetfc(1,iplane) = thetfc(2,iplane)
          rfg(1,iplane) = rfg(2,iplane)
          cec(1,iplane) = cec(2,iplane)
          ssc(1,iplane) = ssc(2,iplane)
          bd(1,iplane) = bd(2,iplane)
c
c         -- xxx -- this is redundant... crm -- 8/21/92.
          avsand(iplane) = sand(2,iplane)
          avclay(iplane) = clay(2,iplane)
c
c         compute remaining avgs. for the primary and secondary
c         tillage layers.
c
          yy = 0.0
          do 70 i = 1, 2
            solthk(i,iplane) = tillay(i,iplane)
            dg(i,iplane) = tillay(i,iplane) - yy
            silt(i,iplane) = 1. - sand(i,iplane) - clay(i,iplane)
            por(i,iplane) = 1. - bd(i,iplane) / 2650.
            if (sat(iplane).lt.(thetdr(i,iplane)/por(i,iplane)))
     1          sat(iplane) = thetdr(i,iplane) / por(i,iplane)
            st(i,iplane) = ((sat(iplane)*por(i,iplane))-
     1          thetdr(i,iplane)) * dg(i,iplane)
            if (st(i,iplane).lt.1e-10) st(i,iplane) = 0.0
            yy = tillay(i,iplane)
   70     continue
c
c
c         calculate the revised number of soil layers (nsl(iplane)).
c         determine whether a partial layer remains beneath the primary
c         & secondary tillage layers.  if so, add one to nsl and set
c         ii to begin in layer icount.
c
c         -------- plow layer is within 1st soil layer....
          if (icount.le.0) then
            nsl(iplane) = nslorg(iplane) + 2
            ii = 1
c         -------- plow layer extends only part way through a soil layer...
          else if (yy.lt.solth1(icount,iplane)) then
            nsl(iplane) = nslorg(iplane) - icount + 3
            ii = icount
c         -------- plow layer extends exactly to bottom of a soil layer...
          else
            nsl(iplane) = nslorg(iplane) - icount + 2
            ii = icount + 1
          end if
c
c         -------- add soil layers below plow layer, to data arrays.
          do 80 i = 3, nsl(iplane)
            solthk(i,iplane) = solth1(ii,iplane)
            dg(i,iplane) = solth1(ii,iplane) - yy
            sand(i,iplane) = sand1(ii,iplane)
            clay(i,iplane) = clay1(ii,iplane)
            silt(i,iplane) = 1. - sand1(ii,iplane) - clay1(ii,iplane)
            orgmat(i,iplane) = orgma1(ii,iplane)
            rfg(i,iplane) = rfg1(ii,iplane)
            cec(i,iplane) = cec1(ii,iplane)
            ssc(i,iplane) = ssc1(ii,iplane)
            bd(i,iplane) = bd1(ii,iplane)
            por(i,iplane) = 1. - bd1(ii,iplane) / 2650.
            thetdr(i,iplane) = thetd1(ii,iplane)
            thetfc(i,iplane) = thetf1(ii,iplane)
            st(i,iplane) = ((sat(iplane)*por(i,iplane))-
     1          thetdr(i,iplane)) * dg(i,iplane)
            if (st(i,iplane).lt.1e-10) st(i,iplane) = 0.00
            yy = solth1(ii,iplane)
            ii = ii + 1
   80     continue
c
c       *** M0 ENDIF ***
        end if
   90 continue
c
      tilflg = 1
c
      return
c 1000 format ('***ERROR***',/,
c     1    ' contours must occur at the upslope portion of',
c     1    ' of the hillslope.',/,'(a contoured ofe follows',
c     1    ' a noncontoured ofe)',/,10x,'program terminated')
 1000 format(/,' *** WARNING ***',/,
     1    ' contours must occur at the upslope portion',
     1    ' of the hillslope.',/,' (a contoured ofe follows',
     1    ' a noncontoured ofe)',/,' OFE ',i2,' will be run without',
     1    ' contours.',/,'*** WARNING ***',/)
           end
