      subroutine depsto(i,recum,f,dtime,avedep,idep,dinf,retemp,re,xmul,
     1    relast)
c
c
c     + + + PURPOSE + + +
c
c     Computes the decrease in rainfall excess due to depression storage.
c
c     Called from GRNA
c     Author(s): Jeffrey J. Stone
c     Reference in User Guide:
c
c     Changes:
c           1)  Order of input parameters must be changed to conform
c               to WEPP Coding Convention.  Original parameter numbers
c               1,5,7,9 & 10 moved to front of list.
c           2)  Include file substituted for internal references to
c               parameter MXTIME.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 08/08/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxtim.inc'
c     read: mxtime
c
c     + + + ARGUMENT DECLARATIONS + + +
      real recum(mxtime), f(mxtime), dtime, avedep
      real dinf, retemp(mxtime), re(mxtime), xmul, relast
      integer i, idep
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c  variable      description                                  type   units
c ------------------------------------------------------------------------
c     i      - current time for infiltration computations      int   (nod)
c
c     recum  - cumulative rainfall excess volume               real  (m)
c
c     f      - infiltration rate                               real  (m/s)
c
c     dtime  - time step for infiltration computations         real  (s)
c
c     avedep - average potential depressional storage          real  (m)
c
c     idep   - 0 : depression storage not satisfied.           int   (nod)
c              1 : depression storage satisfied.
c
c     dinf   - depression storage depth left after an          real  (m)
c              increment of the storage infiltrates at the
c              infiltration rate when rainfall excess goes
c              to zero.
c
c     retemp - rainfall excess minus depression storage.       real  (m)
c
c     re     - rainfall excess rate                            real  (m/s)
c
c     xmul   - multiplier for the depression storage depth.    real  (nod)
c
c     relast - cumulative rainfall excess depth at the time    real  (m)
c              rainfall excess rate goes to zero.
c
c     + + + LOCAL VARIABLES + + +
      real frate, redep
      integer ifst
c
c     + + + LOCAL DEFINITIONS + + +
c
c  variable      description                                  type   units
c ------------------------------------------------------------------------
c     frate  - infiltration rate when rainfall excess goes     real  (m/s)
c              to zero (i.e. frate = f(i-1) when re(i) = 0).
c
c     ifst   - 0 : depression storage not satisfied            int   (nod)
c                   when rainfall excess goes to zero.
c              1 : depression storage is satisfied
c                   when rainfall excess goes to zero.
c
c     + + + SAVES + + +
      save ifst, frate
c
c     + + + END SPECIFICATIONS + + +
c
c
c ------------------------------------------------------------------------
c
c     Check whether there has been sufficient rainfall excess
c     to satisfy the depression storage.
c
      if (idep.eq.0) then
c
c       depression storage has not yet been satisfied
c
        redep = recum(i+1) - relast + dinf
        if (redep.le.avedep) then
c
          if (re(i).gt.0.) then
c
c           set rainfall excess equal to previous value until
c           depression storage is satisfied
c
            retemp(i+1) = retemp(i)
            re(i) = 0.
            ifst = 0
c
          else
c
c           rainfall excess goes to zero.
c
            if (ifst.eq.0) then
c
c             rainfall excess goes to zero before depression
c             storage is satisfied.
c             FIND NUMBER OF CUMULATIVE VOLUMES OF DEPRESSION STORAGE
c             WHICH ARE DETAINED AND INFILTATRATE INTO SOIL, xmul.
c             SET THE DEPTH OF WATER DETAINED WITHIN THE
c             DEPRESSIONS, DINF, EQUAL TO THE CURRENT DEPTH OF
c             DEPRESSION STORAGE.  SET THE INFILTRATION RATE OF THE
c             DEPRESSION STORAGE EQUAL TO THE INFILTRATION RATE OF THE
c             THE BEGINNING OF THE PERIOD OF ZERO RAINFALL EXCESS.
              xmul = xmul + (redep-dinf) / avedep
c
              dinf = redep
c             xmul  = xmul + redep/(avedep*xmul)
              frate = f(i-1)
              ifst = 1
c
            end if
c
c           DECREASE DEPTH OF DEPRESSION STORAGE BY THE DEPTH
c           OF WATER WHICH INFILTRATED INTO THE SOIL AFTER A STOP
c           IN RAINFALL EXCESS
            dinf = dinf - frate * dtime
            if (dinf.lt.0.) dinf = 0.
c
            idep = 0
            retemp(i+1) = retemp(i)
            relast = recum(i+1)
          end if
c
        else
c
c         special case, 1st time after time to ponding
c         that depression storage is satisfied.
c         FIND NUMBER OF CUMULATIVE VOLUMES OF DEPRESSION STORAGE
c         WHICH ARE DETAINED AND INFILTATRATE INTO SOIL, xmul.
c         retemp(i+1) = retemp(i+1) - avedep*xmul + dinf
          xmul = xmul + (avedep-dinf) / avedep
          retemp(i+1) = retemp(i+1) - avedep * xmul
c         IF(RETEMP(I+1) .LT. RETEMP(I))RETEMP(I+1) = RETEMP(I)
          idep = 1
          ifst = 1
c
        end if
c
      else
c
c       depression storage has been satisfied (idep=1).
c       now check if rainfall excess is greater than zero
c
        if (re(i).gt.0.) then
c
c         continue to subtract depression storage
c
c         retemp(i+1) = retemp(i+1) - avedep*xmul + dinf
          retemp(i+1) = retemp(i+1) - avedep * xmul
c       IF(RETEMP(I+1) .LT. RETEMP(I))RETEMP(I+1) = RETEMP(I)
c
        else
c
c         rainfall excess is zero. get infiltration rate
c         and begin to infiltrate depth of depression storage.
c         increment depression storage multiplier
c         and reset idep = 0
c
c         xmul        = xmul + 1
c         XMUL        = XMUL + (AVEDEP-DINF)/AVEDEP
c
          frate = f(i-1)
          dinf = avedep - frate * dtime
          if (dinf.lt.0.) dinf = 0.
c
          idep = 0
          retemp(i+1) = retemp(i)
          relast = recum(i+1)
c
        end if
      end if
c
      return
      end
