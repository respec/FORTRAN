
c     begin include file crinpt6.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt6/   totsup(mxplan),rtotsu(mxplan),rsuppm(mxplan),
     1                  dinner(mxplan),food(mxplan),growth(mxplan),
     1                  ideath(mxplan),sdead(mxplan),rrough(mxplan),
     1                  bcover(mxplan),rufcov(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              totsup,rtotsu,rsuppm,dinner,food,growth,
     1                  sdead,rrough,bcover,rufcov
      integer           ideath

c     + + + VARIABLE DEFINITIONS + + +

c     totsup(mxplan) : running total of supplemental feed consumed
c     rtotsu(mxplan) : running total of emergency food supplied by model
c     rsuppm(mxplan) : emergency food supplied by model to prevent
c                      livestock from straving and dieing.  If this is
c                      activated the user needs to be aware that senerio
c                      has been altered and the stocking rate should be
c                      altered to reflect realistic grazing practices.
c     dinner(mxplan) :
c     food(mxplan)   : daily mass of forage per animal when supplemental
c                      feeding is being utilized
c     growth(mxplan) :
c     ideath(mxplan) : flag. 1=live leaves present.  No live leaves ==> no
c                      herbicide application
c     sdead(mxplan)  : new standing dead mass killed from herbicide appli-
c                      cation.  Typically this is dead woody plants that
c                      have not fallen yet.
c     rrough(mxplan) :
c     bcover(mxplan) :
c     rufcov(mxplan) :

c     end include file crinpt6.inc

