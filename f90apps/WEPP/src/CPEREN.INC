
c     begin include file cperen.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /peren/  cutday(mxcut,mxcrop,mxplan),mgtopt(mxcrop,mxplan),
     1                ncut(mxcrop,mxplan),popmat(mxplan),
     1                jdherb(mxcrop,mxplan),jdburn(mxcrop,mxplan),
     1                jdslge(mxcrop,mxplan),fbrnag(mxcrop,mxplan),
     1                fbrnog(mxcrop,mxplan),pop(mxplan),srmhav(mxplan),
     1                ncycle(mxcrop,mxplan),jdcut(mxcrop,mxplan),
     1                digest(mxplan),jdstop(mxcrop,mxplan),
     1                tothav(mxcrop,mxplan),nnc(mxplan),
     1                frcut(mxcrop,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer         cutday,mgtopt,ncut,jdherb,jdburn,
     1                jdslge,ncycle,jdcut,jdstop,nnc
      real            popmat,fbrnag,fbrnog,pop,
     1                srmhav,frcut,digest,tothav

c     + + + VARIABLE DEFINITIONS + + +

c     cutday(mxcut,mxcrop,mxplan) : cutting date (Julian)
c     mgtopt(mxcrop,mxplan)       : crop management option
c                                   1) cutting
c                                   2) grazing
c                                   3) not harvested or grazed
c     ncut(mxcrop,mxplan)         : number of cuttings
c     popmat(mxplan)              : plant population at maturity
c     jdherb(mxcrop,mxplan)       : herbicide application date (Julian)
c     jdburn(mxcrop,mxplan)       : residue burning date (Julian)
c     jdslge(mxcrop,mxplan)       : silage date (Julian)
c     fbrnag(mxcrop,mxplan)       : fraction of flat residue burned(0-1)
c     fbrnog(mxcrop,mxplan)       : fraction of standing residue burned
c                                   (0-1)
c     pop(mxplan)                 : plant population on day of simulation
c     srmhav(mxplan)              : standing residue mass at harvest (kg/m^2)
c     ncycle(mxcrop,mxplan)       : number of grazing cycles
c     jdcut(mxcrop,mxplan)        : standing residue shredding or
c                                   cutting date(Julian)
c     digest(mxplan)              : digestibility
c     jdstop(mxcrop,mxplan)       : perennial crop growth stop date
c                                   (Julian)
c     tothav(mxcrop,mxplan)       : maximum above ground biomass produced
c
c     nnc(mxplan)                 : perennial crop index for the number
c                                   of cuttings and grazings
c     frcut(mxcrop,mxplan)        : fraction of flat residue shredded
c                                   or cut (0-1)

      include 'cperen1.inc'

c     end include file cperen.inc
