      subroutine wshini
c
c     + + + PURPOSE + + +
c
c     SR WSHINI initializes hillslope, channel, and impoundment
c     variables for hydrologic and erosion routing through
c     the watershed.
c
c     Called from: SR WSHDRV
c     Author(s): Ascough II, C. Baffaut
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
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpar.inc'
c     read: chnlen(mxplan)
c
      include 'cchpek.inc'
c     modify: uparea(0:mxplan),loarea(0:mxplan),
c             charea(0:mxplan),toarea(0:mxplan),
c             chleng(0:mxplan),chmann(mxplan),
c             chslop(mxplan)
c
      include 'cchtmp.inc'
c     modify: ncltmp(mxelem),ncrtmp(mxelem),ncttmp(mxelem),
c             nhltmp(mxelem),nhrtmp(mxelem),nhttmp(mxelem)
c
      include 'cchvar.inc'
c     read: chnn(mxplan)
c
      include 'cimpnd.inc'
c     read: npond
c
      include 'cpart.inc'
c     modify: dia(mxpart,mxelem)
c     read: npart
c
      include 'cslope2.inc'
c     read: avgslp(mxplan)
c
      include 'cslpopt.inc'
c     read: fwidth(mxplan)
c
      include 'csolva1.inc'
c     modify: sand(mxnsl,mxelem), silt(mxnsl,mxelem),
c             clay(mxnsl,mxelem), orgmat(mxnsl,mxelem)
c
      include 'cstore.inc'
c     modify: wsarea(0:mxelem)
c     read: hlarea(0:mxelem)
c
      include 'cstruc.inc'
c     read: nhill
c
      include 'cstruct.inc'
c     read: elmt(mxelem),ichan,nelmt,ncleft(mxelem),
c           ncrght(mxelem),nctop(mxelem)
c
      include 'cwshed.inc'
c     modify: iwsbyr,maxyrs
c
c     + + + LOCAL VARIABLES + + +
c
      integer i, j, jpond, l
c
c     + + + LOCAL DEFINITIONS + + +
c
c     i -
c     j -
c     jpond  -
c     l -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     chncon
c     eatcom
c     impint
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     Section I. Calculation/manipulation of watershed routing
c                variables
c
      ichan = 0
c
      do 20 i = nhill + 1, nelmt
c
c       set channel index arrays
c
        ncltmp(i) = ncleft(i)
        ncrtmp(i) = ncrght(i)
        ncttmp(i) = nctop(i)
c
c       set hillslope index arrays
c
        nhltmp(i) = nhleft(i)
        nhrtmp(i) = nhrght(i)
        nhttmp(i) = nhtop(i)
c
        if (elmt(i).eq.2) then
c
          ichan = ichan + 1
c
c         use watershed structure information to determine channel
c         relationships (ignore impoundments on watershed and
c         assume channels only flow into channels, i.e., if
c         impoundment is feeding a channel, is it masking feeds
c         from other channels?)
c
c          correction claire, april 96
c          if ((i.gt.nhill+1).and.(elmt(i-1).eq.3)) then
          if ((i.gt.nhill+1).and.(nitop(i).gt.0)) then
c
c           impoundment feeding channel - write the channel
c           feed element information into an index array as
c           if no impoundments were present on the watershed
c
c            ncltmp(i) = ncleft(i-1)
c            ncrtmp(i) = ncrght(i-1)
c            ncttmp(i) = nctop(i-1)
             ncltmp(i) = ncleft(nitop(i))
             ncrtmp(i) = ncrght(nitop(i))
             ncttmp(i) = nctop(nitop(i))
c
          end if
c
c         calculate channel areas
c
          charea(ichan) = chnlen(idelmt(i)) * fwidth(idelmt(i))
c
c         put channel texture and particle variables into correct
c         element slots
c
          sand(1,i) = sand(1,ichan)
          silt(1,i) = silt(1,ichan)
          clay(1,i) = clay(1,ichan)
          orgmat(1,i) = orgmat(1,ichan)
c
          do 10 j = 1, npart
            dia(j,i) = dia(j,ichan)
   10     continue
c
        end if
c
   20 continue
c
c     read in remainder of non-temporal variables from
c     hillslope/watershed pass file (maximum watershed
c     simulation years and beginning year of hillslope
c     climate files)
c
      read (49,*) maxyrs
      read (49,*) iwsbyr
c
c     continue reading pass file
c
      do 30 i = 1, 5
        read (49,*)
   30 continue
c
c     read in particle size diameters (m), overland flow lengths (m),
c     overland flow areas (m^2), overland flow average slopes (m/m),
c     and overland flow manning's n from hillslope to watershed pass file
c
      do 40 i = 1, nhill
        read (49,1000) (dia(l,i),l = 1,npart), hlarea(i)
        wsarea(i) = hlarea(i)
   40 continue
c
      do 50 i = 1, 3
        read (49,*)
   50 continue
c
c     determine the contributing area at each of the subwatersheds
c
      ichan = 0
c
      do 60 i = nhill + 1, nelmt
c
        if (elmt(i).eq.2) then
c
          ichan = ichan + 1
c
c         hillslope feeding channel - write the hillslope
c         feed element information into an index array as
c         if no impoundments were present on the watershed
c
          if (nhltmp(i).eq.0) then
c
c           if no hillslope feeding channel from the left
c
            if (nileft(i).gt.0) then
c
c             check and see if impoundment is feeding channel
c
c             if a hillslope fed the impoundment set the index
c             array nhltmp to the hillslope element number (repeated
c             for next two sections for right and top feeding)
c
              if (nhleft(nileft(i)).gt.0) nhltmp(i) =
     1            nhleft(nileft(i))
              if (nhrght(nileft(i)).gt.0) nhltmp(i) =
     1            nhrght(nileft(i))
              if (nhtop(nileft(i)).gt.0) nhltmp(i) =
     1            nhtop(nileft(i))
            end if
          end if
c
          if (nhrtmp(i).eq.0) then
            if (nirght(i).gt.0) then
              if (nhleft(nirght(i)).gt.0) nhrtmp(i) =
     1            nhleft(nirght(i))
              if (nhrght(nirght(i)).gt.0) nhrtmp(i) =
     1            nhrght(nirght(i))
              if (nhtop(nirght(i)).gt.0) nhrtmp(i) =
     1            nhtop(nirght(i))
            end if
          end if
c
          if (nhttmp(i).eq.0) then
            if (nitop(i).gt.0) then
              if (nhleft(nitop(i)).gt.0) nhttmp(i) =
     1            nhleft(nitop(i))
              if (nhrght(nitop(i)).gt.0) nhttmp(i) =
     1            nhrght(nitop(i))
              if (nhtop(nitop(i)).gt.0) nhttmp(i) = nhtop(nitop(i))
            end if
          end if
c
c         use nc*tmp variables so that impoundments are ignored
c
          if ((ncltmp(i).eq.0).and.(ncrtmp(i).eq.0).and.(ncttmp(i).eq.0)
     1        ) then
c
c           upper area of the first order channels (no channel is
c           feeding current channel) - area is contributing
c           top hillslope
c
            uparea(ichan) = hlarea(idelmt(nhttmp(i)))
c
          else
c
c           higher order channels - upper area is total area of
c           contributing channels to current channel
c
            uparea(ichan) = toarea(idelmt(ncltmp(i))) +
     1          toarea(idelmt(ncrtmp(i))) + toarea(idelmt(ncttmp(i)))
          end if
c
c         lower area is area of contributing lateral hillslopes +
c         area of current channel
c
          loarea(ichan) = hlarea(idelmt(nhltmp(i))) +
     1        hlarea(idelmt(nhrtmp(i))) + charea(ichan)
c
c         total area is the sum of the upper and lower areas
c
          toarea(ichan) = uparea(ichan) + loarea(ichan)
          wsarea(i) = toarea(ichan)
c
        end if
c
c       element is an impoundment
c
        if (elmt(i).eq.3) then
c
c         calculate area feeding into the impoundment
c
          wsarea(i) = wsarea(nhleft(i)) + wsarea(nhrght(i)) +
     1        wsarea(nhtop(i)) + wsarea(ncleft(i)) + wsarea(ncrght(i)) +
     1        wsarea(nctop(i))
c
        end if
c
   60 continue
c
c     Section II.  Initialize channel variables
c
      call chncon
c
c     Section III. Initialize the impoundment routines
c
      if (npond.gt.0) then
c
c       check #1 --> number of impoundment elements read in the
c       structure file must be less than or equal to number of
c       elements read in the impoundment file
c
        call eatcom(20)
        read (20,*) jpond
c
        if (npond.gt.jpond) then
          write (6,1100) npond, jpond
          stop
        end if
c
c       check #2 --> number of impoundments read in from the
c       structure or impoundment files must be less than or
c       equal to mximp
c
        if (npond.gt.mximp) then
          write (6,1200) npond, mximp
          stop
        else if (jpond.gt.mximp) then
          write (6,1300) jpond, mximp
          stop
        end if
c
        write (6,1400)
c
        call impint
      end if
c
      return
c
 1000 format (68x,5(e11.5,1x),5x,e10.5)
 1100 format (//' Impoundments read in structure file   : ',i2,/,
     1    ' Impoundments read in impoundment file : ',i2,//,
     1    ' Program stop - number of impoundments read in',/,
     1    ' structure file must be less than or equal to number',/,
     1    ' of impoundments read in impoundment file')
 1200 format (//' Impoundments read in structure file : ',i2,/,
     1    ' Maximum allowable impoundments      : ',i2,//,
     1    ' Program stop - maximum number of impoundments exceeded')
 1300 format (//' Impoundments read in impoundment file : ',i2,/,
     1    ' Maximum allowable impoundments        : ',i2,//,
     1    ' Program stop - maximum number of impoundments exceeded')
 1400 format (/'INITIALIZING IMPOUNDMENTS on watershed '//)
      end
