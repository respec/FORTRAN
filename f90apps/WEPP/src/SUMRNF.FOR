      subroutine sumrnf
c*******************************************************************
c                                                                  *
c   This subroutine is called from SR STMGET to sum up the number  *
c   of rainfall producing events and rainfall amounts during the   *
c   simulation period.                                             *
c                                                                  *
c*******************************************************************
c
      include 'pmxelm.inc'
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c*******************************************************************
c                                                                  *
c   Common Blocks                                                  *
c                                                                  *
c*******************************************************************
c
      include 'cavloss.inc'
c
      include 'chydrol.inc'
c
      include 'cstmflg.inc'
c
      include 'cstruc.inc'
      include 'csumout.inc'
c
c*******************************************************************
c                                                                  *
c   sumout variables updated                                       *
c       nraint,nrainy,nrainm(13),traint,trainy,trainm(13)          *
c                                                                  *
c*******************************************************************
c
      nraint = nraint + 1
      nrainy = nrainy + 1
      nrainm = nrainm + 1
c
      traint = traint + (prcp*1000.0)
      trainy = trainy + (prcp*1000.0)
      trainm = trainm + (prcp*1000.0)
c
      return
      end
