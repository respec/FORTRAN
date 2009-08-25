
c     begin include file cchprt.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /chprt/ cnpart,crdia(mxpart,mxelem),crspg(mxpart),
     1               crfrac(mxpart,mxelem),crfall(mxpart,mxelem),
     1               solcly(mxelem),solsnd(mxelem),solslt(mxelem),
     1               solorg(mxelem),sssoil(mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real           crdia,crspg,crfrac,crfall,solcly,solsnd,solslt,
     1               solorg,sssoil
      integer        cnpart

c     + + + VARIABLE DEFINITIONS + + +

c     cnpart                 : number of particle size classes
c     crdia(mxpart,mxelem)   : diameter of particle for each class and element
c     crspg(mxpart)          : specific gravity of particle in each class
c     crfrac(mxpart,mxelem)  : fraction of load for each class and element
c     crfall(mxpart,mxelem)  : fall velocity for each class and element
c     solcly(mxelem)         : amount of clay in sediment for each element
c     solsnd(mxelem)         : amount of sand in sediment for each element
c     solslt(mxelem)         : amount of silt in sediment for each element
c     solorg(mxelem)         : amount of organic matter for each element
c     sssoil(mxelem)         : index of specific surface area of sediment
c                            : for each element

c     end include file cchprt.inc

