c     begin include file cinpman1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /inpman1/ iscen(500),tilse1(ntype),
     1       conse1(ntype),irrset(ntype),imngm1(ntype),tilla1(ntype),
     1       tilla2(ntype),jdpl1(ntype),jdhar1(ntype),r1(ntype),
     1       resmg1(ntype),jdher1(ntype),jdbur1(ntype),
     1       fbrno1(ntype),fbrna1(ntype),
     1       jdslg1(ntype),jdcu1(ntype),frcu1(ntype),
     1       jdsto1(ntype),mgtop1(ntype),ncu1(ntype),
     1       ncycl1(ntype),gda1(ntype,ntype2),gen1(ntype,ntype2),anima1
     1       (ntype,ntype2),bodyw1(ntype,ntype2),are1(ntype,ntype2),
     1       diges1(ntype,ntype2),jfdat1(ntype),ihdat1(ntype),grazi1
     1       (ntype),cutda1(ntype,ntype2),ityp1(ntype),drseq1(ntype)

c     + + + VARIABLE DECLARATIONS + + +

      real    tilla1,r1,fbrno1,fbrna1,frcu1,bodyw1,are1,diges1,
     1        anima1,tilla2
      integer iscen,tilse1,conse1,irrset,imngm1,resmg1,
     1        jdpl1,jdhar1,jdher1,jdbur1,jdslg1,jdcu1,jdsto1,
     1        mgtop1,ncu1,gda1,gen1,jfdat1,ihdat1,
     1        grazi1,cutda1,ityp1,ncycl1,drseq1

c     + + + VARIABLE DEFINITIONS + + +

c     iscen(ntype)         : 1=cropland, 2=rangeland, 3=forest, 4=roads
c     tilse1(ntype)        : surface effects scenario index
c     conse1(ntype)        : contour scenario index
c     irrset(ntype)        : irrigation scenario index
c     imngm1(ntype)        : 1=annual, 2=perennial, 3= fallow
c     tilla1(ntype)        : depth of secondary tillage layer
c     tilla2(ntype)        : depth of primary tillage layer
c     jdpl1(ntype)         : julian planting date
c     jdhar1(ntype)        : julian harvest date
c     r1(ntype)            : row width (m)
c     resmg1(ntype)        : residue managment option
c                            1) herbicide
c                            2) burning
c                            3) silage
c                            4) shredding or cutting
c                            5) residue removal
c                            6) none
c     jdher1(ntype)        : julian herbicide application date
c     jdbur1(ntype)        : julian burning date
c     fbrno1(ntype)        : fraction of flat residue after burning (0-1)
c     fbrna1(ntype)        : fraction of standing residue after burning (0-1)
c     jdslg1(ntype)        : julian silage harvest date
c     jdcu1(ntype)         : julian shredding or cutting date
c     frcu1(ntype)         : fraction of standing residue cut (0-1)
c     jdmov1(ntype)        : julian removal date
c     frmov1(ntype)        : fraction of flat residue removed (0-1)
c     jdsto1(ntype)        : perennial crop growth stop julian date
c     mgtop1(ntype)        : crop management option 1=cut, 2=graze, 3=neither
c     ncu1(ntype)          : number of cuttings
c     cutda1(ntype,ntype2) : julian cutting date
c     ncycl1(ntype)        : number of grazing cycles
c     gda1(ntype)          : cropland julian date grazing begins for
c                            current cycle
c     gen1(ntype)          : cropland julian date grazing ends for
c                            current cycle
c     anima1(ntype,ntype2) : number of animal units
c     bodyw1(ntype,ntype2) : unit animal body weight (kg)
c     are1(ntype,ntype2)   : cropland field size (m^2)
c     diges1(ntype,ntype2) : digestibility
c     jfdat(ntype)         : julian burning date for rangeland
c     ihdat1(ntype)        : julian herbicide app. date for rangeland
c     grazi1(ntype)        : grazing flag
c     ityp1(ntype)         :
c     drseq1(ntype)        : drainage scenario index

      include 'cinpman2.inc'

c     end include file cinpman1.inc
