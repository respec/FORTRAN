      subroutine nowup(tlseq2,ntill2,jdplt2,jdhrv2,jdhrv1,jdstp1,switch)
c
c      find the date to update nowcrp to the next crop
c      (the minimum of the planting date and the minimum
c      tillage date of the next crop)
c      if the above is zero then default to the harvest of the
c      current crop (next crop is fallow with no tillage)
c      note NOWUP is only called if nycrop > 1
c
c
      integer tlseq2, ntill2, jdplt2, jdhrv2, jdhrv1, switch, jdstp1,
     1        indtil
c
c    ARGUMENT DEFINITIONS
c
c     tlseq2 - flag for tillage sequence for next crop
c     ntill2 - number of tillages in tlseq2
c     jdplt2 - julian date of planting for next crop
c     jdhrv2 - julian date of harvesting for next crop
c     jdhrv1 - julian date of harvesting for current crop
c     jdstp1 - julian stop date for perennial growth for current crop
c     switch - day of simulation year on which switch to new crop made
c
c
      include 'pmxpln.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
      include 'cupdate.inc'
c
c    set the initial switch date to 1 day previous to the planting date
c    of the next crop
c
      switch = jdplt2 - 1
c
c     if the next crop has a tillage sequence, check to see if there is
c     a tillage between the harvest (or growth stop date) of the current
c     crop and this tillage date.
c
      if (tlseq2.gt.0) then
c
c       for the case of a fallow crop - with an input of 0 planting date -
c       set the switch date to the last tillage date in the sequence
c       NOTE - this assumes that dates are in increasing order - this may
c       cause problems if they are not.  dcf  2/24/93
c
        if (switch.le.0) then
          switch = mdate(ntill2,tlseq2) - 1
        end if
c
c       go through the next crop's tillage sequence - check each tillage
c       date and see if it is a lower date than the current value for the
c       switch date AND that it is a greater date than the harvest date
c       of the current crop OR a greater date than the kill date of the
c       current perennial crop if one exists - if it is - set the switch
c       date to this earlier tillage date minus 1 day.
c
c       NOTE - must subtract 1 day because the updating to the next crop
c       takes place at the bottom of the loop in subroutine CONTIN -
c       thus to have the correct tillage sequence and tillage to be done
c       on the desired management day the "switch" has to be done at the
c       end of the previous day.   dcf  2/24/93
c
        do 10 indtil = 1, ntill2
          if (mdate(indtil,tlseq2)-1.lt.switch) then
            if (jdstp1.gt.0.and.mdate(indtil,tlseq2)-1.gt.jdstp1) then
              switch = mdate(indtil,tlseq2) - 1
            else if (mdate(indtil,tlseq2)-1.gt.jdhrv1) then
              switch = mdate(indtil,tlseq2) - 1
            end if
          end if
   10   continue
c
c       Added a check to make sure that the switch date does not
c       occur before the kill date of a perennial.  A perennial's
c       kill date must be reached before a switch is made so that
c       all root masses and indices will be changed properly.
c       A problem can arise when the same tillage sequence is used
c       for 2 different crops in a rotation (a perennial and an annual)
c       dcf   12/20/93
c
        if (switch.lt.jdstp1) switch = jdstp1
      end if
c
c     following check made because of problems with computed
c     switch date for winter annual crops grown in conjunction
c     with summer annual crops.  This will check to see if the switch
c     date that has been computed falls after the current crop's
c     harvest or kill date BUT before the next crop's harvest date
c     if it is before the next crop's harvest date - set the switch
c     date to that harvest date - which will prevent harvesting of
c     of crop which has never been planted by the plant growth routines
c     dcf   2/24/93
c
      if (jdhrv2.lt.jdplt2) then
        if (switch.le.jdhrv2) switch = jdhrv2
      end if
c
c     fallow crop with no tillage
c
      if (switch.le.0) switch = jdhrv1
c
      return
      end
