      subroutine wshtc
c
c     + + + PURPOSE + + +
c
c     Calculates the time of concentration at the outlet of a channel
c     element.
c
c     Called from:  SR WSHPEK
c     Author(s):  C. Baffaut, Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
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
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchtmp.inc'
      include 'cchvar.inc'
      include 'cflags.inc'
      include 'chydrol.inc'
      include 'cimpnd.inc'
      include 'cslope2.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cstruct.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real qcstar, cltmp(mxplan), cstmp(mxplan), cmtmp(mxplan),
     1    tcc(mxplan), tcil, tcir, tcit, tcimax, tcmax, tcsmx
      integer idrout(mxplan), idl, idr, idt, impr, impl, impt
c
c     + + + LOCAL DEFINITIONS + + +
c
c     qcstar         -  average channel flowrate
c     cltmp(mxplan)  -  temporary length of channel path that
c                       contributes to largest tc
c     cstmp(mxplan)  -  temporary slope of channel path that
c                       contributes to largest tc
c     cmtmp(mxplan)  -  temporary manning's n of channel path that
c                       contributes to largest tc
c     tcc(mxplan)    -  travel time for channel path that
c                       contributes to largest tc
c     tcil           -  tc from impoundment contributing to channel
c                       from the left
c     tcir           -  tc from impoundment contributing to channel
c                       from the right
c     tcit           -  tc from impoundment contributing to channel
c                       from the top
c     tcimax         -  maximum tc from a contributing impoundment
c     tcmax          -  maximum channel tc depending on
c                       contributing elements
c     tcsmx          -  maximum hillslope or impoundment tc for a first
c                       order channel.
c     idrout(mxplan) -  local variable to track upstream channel
c                       that contributes to largest tc
c     idl            -  tracking variable for elements feeding channel
c                       from the left
c     idr            -  tracking variable for elements feeding channel
c                       from the right
c     idt            -  tracking variable for elements feeding channel
c                       from the top
c     impr           -  tracking variable for impoundment element feeding
c                       channel from the right
c     impl           -  tracking variable for impoundment element feeding
c                       channel from the left
c     impt           -  tracking variable for impoundment element feeding
c                       channel from the top
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (runvol(ielmt).lt.0.0001) then
        htcc(ielmt) = 0
        return
      end if
c
c     calculate the average flowrate in the channel
c
      qcstar = runvol(ielmt) / watdur(ielmt)
c
c     first order channels (no channels feeding)
c
      if ((ncltmp(ielmt).eq.0).and.(ncrtmp(ielmt).eq.0).and.(
     1    ncttmp(ielmt).eq.0)) then
c
c       calculate the slope length, manning's n, and slope
c       of individual first order channels that make up the
c       longest flow path towards the subwatershed
c
        chleng(ichan) = chnlen(ichan)
        chslop(ichan) = avgslp(ichan)
        chmann(ichan) = chnn(ichan)
c
c       set channel time of concentration for this channel
c
        tcc(ichan) = 0.4 * chleng(ichan) * 1.0e-3 * (chmann(ichan)**0.75
     1      ) / ((qcstar**0.25)*(chslop(ichan)**0.375))
c
c       hillslope(s) contributing directly to the channel
c
        hill(ichan) = nhleft(ielmt)
c
        if (htcs(nhtop(ielmt)).gt.htcs(hill(ichan))) hill(ichan) =
     1      nhtop(ielmt)
        if (htcs(nhrght(ielmt)).gt.htcs(hill(ichan))) hill(ichan) =
     1      nhrght(ielmt)
c
c       set time of concentration for the subwatershed at the outlet
c       of this channel
c
        htcc(ielmt) = tcc(ichan) + htcs(hill(ichan))
        tcsmx = htcs(hill(ichan))
c
c       set routing flag to track longest flow path through watershed
c
        idrout(ichan) = ich(ichan)
c
c       impoundment feeds the channel and contributes runoff
c
        if (runvol(nileft(ielmt)).gt.0.001) then
          impl = nileft(ielmt)
          tcil = amax1(htcs(nhleft(impl)),htcs(nhrght(impl)),
     1        htcs(nhtop(impl)),tcf(idelmt(impl)))
          htcc(nileft(ielmt)) = tcil
c
          if (tcil.gt.tcsmx) then
            htcc(ielmt) = tcc(ichan) + tcil
            hill(ichan) = 0
            tcsmx = tcil
          end if
c
        end if
c
        if (runvol(nirght(ielmt)).gt.0.001) then
          impr = nirght(ielmt)
          tcir = amax1(htcs(nhleft(impr)),htcs(nhrght(impr)),
     1        htcs(nhtop(impr)),tcf(idelmt(impr)))
          htcc(nirght(ielmt)) = tcir
c
          if (tcir.gt.tcsmx) then
            htcc(ielmt) = tcc(ichan) + tcir
            hill(ichan) = 0
            tcsmx = tcir
          end if
c
        end if
c
        if (runvol(nitop(ielmt)).gt.0.001) then
          impt = nitop(ielmt)
          tcit = amax1(htcs(nhleft(impt)),htcs(nhrght(impt)),
     1        htcs(nhtop(impt)),tcf(idelmt(impt)))
          htcc(nitop(ielmt)) = tcit
c
          if (tcit.gt.tcsmx) then
            htcc(ielmt) = tcc(ichan) + tcit
            hill(ichan) = 0
          end if
c
        end if
c
      else
c
c       higher order channel (at least one channel or one
c       impoundment feeding)
c
c       calculate the slope length, manning's n, and slope
c       of the higher order channels that make up the longest
c       time of concentration through the subwatersheds towards
c       the watershed outlet
c
        idl = idelmt(ncleft(ielmt))
        idr = idelmt(ncrght(ielmt))
        idt = idelmt(nctop(ielmt))
c
c       find upstream channel element that has the largest tc
c       and compare with possible hillslopes and impoundments
c
        tcmax = 0.0
        idrout(ichan) = ich(ichan)
c
c       channel tc
c
        if (htcc(ncleft(ielmt)).gt.0.0) then
          idrout(ichan) = ich(idl)
          hill(ichan) = hill(idl)
          tcmax = htcc(ncleft(ielmt))
        end if
c
        if (htcc(nctop(ielmt)).gt.tcmax) then
          idrout(ichan) = ich(idt)
          hill(ichan) = hill(idt)
          tcmax = htcc(nctop(ielmt))
        end if
c
        if (htcc(ncrght(ielmt)).gt.tcmax) then
          idrout(ichan) = ich(idr)
          hill(ichan) = hill(idr)
          tcmax = htcc(ncrght(ielmt))
        end if
c
c       hillslope tc
c
        if (htcs(nhleft(ielmt)).gt.tcmax) then
          idrout(ichan) = ich(ichan)
          hill(ichan) = nhleft(ielmt)
          tcmax = htcs(nhleft(ielmt))
        end if
c
        if (htcs(nhrght(ielmt)).gt.tcmax) then
          idrout(ichan) = ich(ichan)
          hill(ichan) = nhrght(ielmt)
          tcmax = htcs(nhrght(ielmt))
        end if
c
c       impoundment tc
c
        tcimax = 0.0
c
        if (runvol(nileft(ielmt)).gt.0.001) then
          impl = nileft(ielmt)
          tcil = amax1(htcs(nhleft(impl)),htcs(nhrght(impl)),
     1           htcs(nhtop(impl)),tcf(idelmt(impl)))
          htcc(nileft(ielmt)) = tcil
c
          if (tcil.gt.tcmax) then
            idrout(ichan) = ich(ichan)
            hill(ichan) = 0
            tcmax = tcil
            tcimax = tcil
          end if
c
        end if
c
        if (runvol(nirght(ielmt)).gt.0.001) then
          impr = nirght(ielmt)
          tcir = amax1(htcs(nhleft(impr)),htcs(nhrght(impr)),
     1           htcs(nhtop(impr)),tcf(idelmt(impr)))
          htcc(nirght(ielmt)) = tcir
c
          if (tcir.gt.tcmax) then
            idrout(ichan) = ich(ichan)
            hill(ichan) = 0
            tcmax = tcir
            tcimax = tcir
          end if
c
        end if
c
        if (runvol(nitop(ielmt)).gt.0.001) then
          impt = nitop(ielmt)
          tcit = amax1(htcs(nhleft(impt)),htcs(nhrght(impt)),
     1        htcs(nhtop(impt)),htcc(ncleft(impt)),
     1        htcc(ncrght(impt)),htcc(nctop(impt)),tcf(idelmt(impt)))
          htcc(nitop(ielmt)) = tcit
c
          if (tcit.gt.tcmax) then
            idrout(ichan) = ich(ichan)
            hill(ichan) = 0
            tcmax = tcit
            tcimax = tcit
          end if
c
        end if
c
        if (idrout(ichan).eq.ich(ichan)) then
          chleng(ichan) = chnlen(ichan)
          chslop(ichan) = avgslp(ichan)
          chmann(ichan) = chnn(ichan)
        else
          chleng(ichan) = chnlen(ichan) +
     1        chleng(idelmt(idrout(ichan)))
          cltmp(ichan) = chleng(idelmt(idrout(ichan)))
          cstmp(ichan) = chslop(idelmt(idrout(ichan)))
          cmtmp(ichan) = chmann(idelmt(idrout(ichan)))
          chslop(ichan) = ((cltmp(ichan)*cstmp(ichan))+(chnlen(ichan)*
     1        avgslp(ichan))) / chleng(ichan)
          chmann(ichan) = ((cltmp(ichan)*cmtmp(ichan))+(chnlen(ichan)*
     1        chnn(ichan))) / chleng(ichan)
        end if
c
c       set time of concentration for channel
c
        tcc(ichan) = 0.4 * chleng(ichan) * 1.0e-3 * (chmann(ichan)**0.75
     1      ) / ((qcstar**0.25)*(chslop(ichan)**0.375))
c
c       set time of concentration for the subwatershed at the outlet
c       of the channel
c
        htcc(ielmt) = tcc(ichan) + htcs(hill(ichan)) + tcimax
c
      end if
c
      return
      end
