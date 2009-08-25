
c     begin include file cinpofe.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpofe/ bdtil1(ntype),cancv1(ntype),crypt1(ntype),
     1                daydi1(ntype),ddrai1(ntype),drain1(ntype),
     1                drdia1(ntype),sdrai1(ntype),dshar1(ntype),
     1                frdp1(ntype),imngt1(ntype),inrcv1(ntype),
     1                iresd1(ntype),lanus1(ntype),pptg1(ntype),
     1                rfcum1(ntype),rhini1(ntype),rilcv1(ntype),
     1                rmagt1(ntype),rmogt1(ntype),rrini1(ntype),
     1                rroug1(ntype),rspc1(ntype),snodp1(ntype),
     1                thdp1(ntype),wcf1(ntype),width1(ntype),
     1                rtype(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real            bdtil1,cancv1,crypt1,daydi1,ddrai1,drain1,drdia1,
     1                sdrai1,dshar1,frdp1,inrcv1,pptg1,rfcum1,rhini1,
     1                rilcv1,rmagt1,rmogt1,rrini1,rroug1,rspc1,snodp1,
     1                thdp1,wcf1,width1
      integer         imngt1,iresd1,lanus1,rtype

c     + + + VARIABLE DEFINITIONS + + +

c     bdtil1(ntype) : bulk density after last tillage (g/cc)
c     cancv1(ntype) : initial canopy cover (0-1)
c     crypt1(ntype) : cryptogram surface cover (0-1)
c     daydi1(ntype) : days since last tillage (days)
c     ddrail(ntype) :
c     drain1(ntype) :
c     drdia1(ntype) :
c     sdrai1(ntype) :
c     dshar1(ntype) : days since last harvest (days)
c     frdp1 (ntype) : initial frost depth (m)
c     imngt1(ntype) : initial residue cropping system (1=annual, 2=perennial,
c                     3=fallow
c     inrcv1(ntype) : initial interril cover (0-1)
c     iresd1(ntype) : initial residue type (index based on plant inputs)
c     lanus1(ntype) : 1=agricultural, 2=rangeland, 3=forest, 4=roads
c     pptg1 (ntype) : average rainfall during growing season (m)
c     rfcum1(ntype) : cumulative rainfall since last tillage (mm)
c     rhini1(ntype) : initial ridge height (m)
c     rilcv1(ntype) : initial rill cover (0-1)
c     rmagt1(ntype) : initial residue mass above the ground (kg/m**2)
c     rmogt1(ntype) : initial residue mass on the ground (kg/m**2)
c     rrini1(ntype) : initial ridge roughness (m)
c     rroug1(ntype) : initial random roughness for rangeland (m)
c     rspc1 (ntype) : rill spacing (m)
c     snodp1(ntype) : initial snow depth (m)
c     thdp1 (ntype) : initial thaw depth (m)
c     wcf1  (ntype) : rock and gravel surface cover (0-1)
c     width1(ntype) : initial rill width (m)
c     rtype (ntype) : 1=temporary, 2=permanent

c     end include file cinpofe.inc

