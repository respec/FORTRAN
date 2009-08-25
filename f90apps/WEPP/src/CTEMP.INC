c     begin include file ctemp.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /temp/     sand1(mxnsl,mxplan),clay1(mxnsl,mxplan),
     1                  orgma1(mxnsl,mxplan),rfg1(mxnsl,mxplan),
     1                  cec1(mxnsl,mxplan),nslorg(mxplan),
     1                  ssc1(mxnsl,mxplan),bd1(mxnsl,mxplan),
     1                  por1(mxnsl,mxplan),thetd1(mxnsl,mxplan),
     1                  thetf1(mxnsl,mxplan),solth1(mxnsl,mxplan),
     1                  avclay(mxplan),avsand(mxplan),solwpv,
     1                  avke(mxplan),slflag(mxplan),
     1                  kslast(mxplan),anisrt(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real              sand1,clay1,orgma1,rfg1,cec1,ssc1,bd1,por1,
     1                  thetd1,thetf1,solth1,avclay,avsand,avke,
     1                  kslast,anisrt
      integer           nslorg, solwpv, slflag

c     + + + VARIABLE DEFINITIONS + + +

c     sand1(mxnsl,mxplan) : % sand
c     clay1(mxnsl,mxplan) : % clay
c     orgma1(mxnsl,mxplan): % organic matter
c     rfg1(mxnsl,mxplan)  : % rock fragments
c     cec1(mxnsl,mxplan)  : cation exchange capacity
c     nslorg(mxplan)      : number of soil layers
c     ssc1(mxnsl,mxplan)  : initial saturated hydraulic conductivity
c                           (mm/h)
c     bd1(mxnsl,mxplan)   : initial bulk density (g/cc)
c     thetd1(mxnsl,mxplan): inital 15-bar soil water content (mm/mm)
c     thetf1(mxnsl,mxplan): initial 1/3 bar soil water content(mm/mm)
c     solth1(mxnsl,mxplan): cumulative thickness of soil layer (mm)
c     avclay(mxplan)      : average % clay based on primary and i
c                           secondary tillage layers
c     avsand(mxplan)      : average % sand based on primary and i
c                           secondary tillage layers
c     por1(mxnsl,mxplan)  :
c     slflag(mxplan)      : flag to determine either to use calculated 
c                           value or user choice of hydraulic
c                           conductivity
c     kslast(mxplan)      : hydraulic conductivity of last rock layer.
c     solwpv              : soil file WEPP version flag to determine
c                           which format the soil input file is
c                           (version number from line 1 of soil file
c                           times 10, for values less than 1000,
c                           then converted to integer. For values larger
c                           than 1000, actual value in input file used).
c
c                           less than 941 - OLD FORMAT SOIL FILES
c                                           and model to compute
c                                           effective conductivity,
c                                           Ks for sublayers,
c                                           BD, field capacity H20,
c                                           and wilting point H20
c                           941 and greater - NEW FORMAT SOIL FILES
c                                             model will read input
c                                             value for effective
c                                             conductivity.  Will
c                                             compute Ks sublayers, BD
c                                             fld cap H20, wlt pt H20
c                           7777 = superuser - old format soil files
c                                  allow option of entering and using
c                                  values of conductivity, BD, wlt pt
c                                  H20 and fld cap H20 for all layers
c                           2006 - Has soil restricting layer info,
c                                  including ansisotrophy and ksat
c                                  after layer data.
c
c     avke(mxplan)        : Effective hydraulic conductivity value for
c                           the soil surface layers, input by the user
c                           If user inputs a value of 0.0, WEPP will
c                           calculate a value for AVKE
c
c     anisrt(mxplan)      : Anisotropy ratio for the whole soil profile
c

c     end include file ctemp.inc
