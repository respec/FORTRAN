
c     begin include file cdiss3.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /diss3/    p,fq,deltfq,timep,ip

c     + + + VARIABLE DECLARATIONS + + +

      real              p,fq,deltfq,timep,ip

c     + + + VARIABLE DEFINITIONS + + +

c     p      : total rainfall depth (m)
c     fq     : cumulative normalized rainfall depth (m)
c     ip     : relative peak intensity:  ratio maximum rainfall
c              intensity to average rainfall intensity
c     deltfq : incremental normalized rainfall depth (m)
c     timep  : ratio time to rainfall peak to rainfall duration

c     end include file cdiss3.inc

