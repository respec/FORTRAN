c     begin include file crinpt4.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt4/   ihdate(mxplan),active(mxplan),herb(mxplan),
     1                  update(mxplan),regrow(mxplan),dleaf(mxplan),
     1                  ptlive(mxplan),first(ntype),dwood(mxplan),
     1                  xlive(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           ihdate,active
      real              herb,update,regrow,dleaf,ptlive,first,
     1                  dwood,xlive

c     + + + VARIABLE DEFINITIONS + + +

c     ihdate(mxplan) : Julian day of herbicide application
c     active(mxplan) : flag for type of herbicide used (0=instant
c                      death; 1=activated once 12.5 mm rain occurs)
c     herb(mxplan)   : fraction evergreen leaves remaining alive
c                      after herbicide application
c     update(mxplan) : fraction of biomass available for consumption
c                      by livestock after burning (replaces ACCESS)
c     regrow(mxplan) : fraction applied to total plant production
c                      (PTLIVE and PROOT that is expected after
c                      herbicide application
c     dleaf(mxplan)  : fraction of standing live biomass remaining
c                      after herbicide application (non-evergreen)
c     ptlive(mxplan) : previous total live leaf biomass (kg/m^2)
c     first(ntype)   : previous years initiation of growth date
c                      (julian)
c     dwood(mxplan)  : standing dead biomass left after
c                      burning or herbicide (kg/m^2)
c     xlive(mxplan)  : evergreen leafy component (kg/m^2)

c     end include file crinpt4.inc
