
c     begin include file crinpt3.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt3/   alter(mxplan),burned(mxplan),change(mxplan),
     1                  hurt(mxplan),jfdate(mxplan),reduce(mxplan),
     1                  basden,spai(ntype),tpai(ntype)
c -- XXX -- Added SPAI, & TPAI to permit single calculation of them in RNGINT,
c           rather than daily calculation in PATRIB.

c     + + + VARIABLE DECLARATIONS + + +

      integer           jfdate
      real              alter,burned,change,hurt,reduce,basden,spai,tpai

c     + + + VARIABLE DEFINITIONS + + +

c     alter(mxplan)  : coefficient of increase in accessibility (0-1)
c     burned(mxplan) : fraction of reduction or increase in standing
c                      dead wood after burning
c     change(mxplan) : fraction increase or decrease in potential
c                      above and below ground biomass
c     hurt(mxplan)   : fraction increase or decrease of evergreen
c                      biomass
c     jfdate(mxplan) : julian day of burning
c     reduce(mxplan) : fraction reduction in standing herbaceous and
c                      organic residue as a function of burning
c     basden         : effective plant basal density

      include 'crinpt3a.inc'

c     end include file crinpt3.inc

