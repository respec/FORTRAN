      subroutine close(lunp,luns,lunw,lun1)
c
c     + + + PURPOSE + + +
c
c     Controls the closing of all hillslope and watershed
c     input and output files.
c
c     Called from: SR CONTIN
c     Author(s): Ascough II
c     Reference in User Guide:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer lunp, luns, lunw, lun1
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     lunp,luns,lunw - plant, and soil, water balance, output flags
c              respectively (0 - no; 1 - yes)
c     lun1   - distance and sediment loss output flag
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccrpprm.inc'
c     read: rngplt, rnganm
c
      include 'cerrid.inc'
c     read: ifile
c
      include 'ciplot.inc'
c     read: ifofe, ievt, isum
c
      include 'cflags.inc'
c     read: snoflg,yldflg
c
      include 'cirriga.inc'
c     read: irschd(1)
c
      include 'cstruc.inc'
c     read: ivers
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + LOCAL DEFINITIONS + + +
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
c     ...close input files
c
c     ...close slope file
c
      close (unit=10)
c
c     ...close soil file
c
      close (unit=11)
c
c     ...close management file
c
      close (unit=12)
c
c     ...close storm file
c
      close (unit=13)
c
c     ...close fixed date irrigation file
c
      if (irsyst.gt.0) then
        if (irschd(1).ne.1) close (unit=14)
      end if
c
c     ...close depletion level irrigation file
c
      if (irsyst.gt.0) then
        if (irschd(1).ne.2) close (unit=15)
      end if
c
c     ...close output files
c
c     ...event-by-event summary
c
      if (ievt.eq.1) close (unit=30)
c
c     ...continous version ofe soil loss
c
      if (imodel.eq.1) then
c
        if (ivers.eq.3) then
          close (unit=38)
        else
          close (unit=31)
        end if
c
      end if
c
c     ...single storm version ofe soil loss
c
      if (imodel.ne.1) close (unit=32)
c
c     ...event-by-event summary for each ofe
c
      if (ifofe.eq.1) close (unit=33)
c
c     ...ofe abbreviate raw summary (abbrev.raw)
c
      if (imodel.eq.1) close (unit=34)
c
c     ...water balance daily output
c
      if (lunw.eq.1) close (unit=35)
c
c     ...plant and residue daily output
c
      if (lunp.eq.1) close (unit=36)
c
c     ...soil properties daily output
c
      if (luns.eq.1) close (unit=39)
c
c     ...large graphics output file for hillslopes
c
      if (lun1.gt.1.and.ivers.ne.3) close (unit=40)
c
c     ...large graphics output file for watershed
c
      if (lun1.gt.1.and.ivers.eq.3) close (unit=41)
c
c     ...winter component output file
c
      if (snoflg.eq.1) close (unit=42)
c
c     ...distance and sediment loss output file
c
      if (lun1.eq.1.or.lun1.eq.2) then
c
        if (ivers.ne.3) then
          close (unit=43)
        else
          close (unit=37)
        end if
c
      end if
c
c     ...rangeland range output file
c
      if (rngplt.eq.2) close (unit=44)
c
c     ...rangeland animal output file
c
      if (rnganm.eq.2) close (unit=45)
c
c     ...crop yield output file
c
      if (yldflg.eq.1) close (unit=46)
c
c     ...initial condition output file
c
      if (ifile.eq.2) close (unit=47)
c
c     ...hillslope pass file
c
      close (unit=48)
c
c     ...watershed pass file and detailed runoff file
c
      if (ivers.eq.3) then
        close (unit=49)
        close (unit=59)
      end if
c
c     ...temporary and final summary files
c
      if (isum.eq.1) then
        close (unit=52)
        close (unit=53)
      end if
c
      return
      end
