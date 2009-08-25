
c     begin include file cefflen.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Stores the effective length

      common /effln/ efflen(0:mxplan)
c
c     + + + VARIABLE DECLARATIONS + + +
c
      real efflen

c     + + + VARIABLE DEFINITIONS + + +

c      efflen : effective flow length of an overland flow element
c               for continuous flow planes - efflen is the sum of
c               the plane lengths having flow - for a Case 4 plane
c               on which runoff ends - efflen is the length of the
c               portion of the Case 4 OFE on which runoff is present

c     end include file cefflen.inc

