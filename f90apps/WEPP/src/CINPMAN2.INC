
c     begin include file cinpman2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpman2/ activ1(ntype),wood1(ntype),her1(ntype),
     1                 updat1(ntype),regro1(ntype),dlea1(ntype),
     1                 alte1(ntype),burne1(ntype),chang1(ntype),
     1                 hur1(ntype),reduc1(ntype),acces1(ntype),
     1                 suppm1(ntype),digmi1(ntype),digma1(ntype),
     1                 jgra1(ntype),ssda1(ntype,ntype2),are2(ntype),
     1                 sen1(ntype,ntype2),idrai1(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real             her1,updat1,regro1,dlea1,alte1,burne1,chang1,
     1                 hur1,reduc1,acces1,suppm1,digmi1,digma1,are2
      integer          activ1,wood1,ssda1,sen1,idrai1,jgra1

c     + + + VARIABLE DEFINITIONS + + +

c     activ1(ntype)       : flag for activated herbicides for rangeland
c     wood1(ntype)        : flag for decomp. of standing biomass due to herbicide
c     her1(ntype)         : fraction change in evergreen biomass
c     updat1(ntype)       : fraction increase of foliage
c     regro1(ntype)       : frac. change in above and below ground biomass
c     dlea1(ntype)        : frac. reduction in live biomass (0-1)
c     alte1(ntype)        : frac. increase in forage
c     burne1(ntype)       : frac. change in standing dead biomass
c     chang1(ntype)       : frac. change in potential above ground biomass
c     hur1(ntype)         : frac. change in evergreen biomass
c     reduc1(ntype)       : frac. reduction in residue
c     acces1(ntype)       : frac. of forage available for construction
c     suppm1(ntype)       : avg. amt. of supp. feed per day (kg/day)
c     digmi1(ntype)       : min. digestibility of forage
c     digma1(ntype)       : max. digestibility of forage
c     jgra1(ntype)        : number of grazing cycles per year on rangeland
c     ssda1(ntype,ntype2) : julian start of supp. feeding
c     are2(ntype)         : grazing area (m^2)
c     sen1(ntype,ntype2)  : julian end of supp. feeding
c     idrai1(ntype)       :

c     end include file cinpman2.inc
