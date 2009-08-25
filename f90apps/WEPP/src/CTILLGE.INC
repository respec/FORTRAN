
c     begin include file ctillge.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /tillge/   tildep(mxtill,mxtlsq),nrplt,nrcul,
     1                  tillay(2,mxplan),typtil(mxtill,mxtlsq),
     1                  rro(mxtill,mxtlsq),rho(mxtill,mxtlsq),
     1                  tdmean(mxtill,mxtlsq),nrdril,cltpos,
     1                  surdis(mxtill,mxtlsq),resman(mxtill,mxtlsq)

c     + + + VARIABLE DECLARATIONS + + +

      real              tildep,tillay,rro,rho,tdmean,surdis
      integer           nrplt,nrcul,nrdril,cltpos,typtil,resman

c     + + + VARIABLE DEFINITIONS + + +

c     tildep(mxtill,mxtlsq) : implement tillage depth (m)
c     nrplt                 : planter row number
c     nrcul                 : cultivator row number
c     tillay(2,mxplan)      : depth of secondary (tillay (1)) and
c                             primary (tillay (2)) tillage for the
c                             current crop on the OFE
c     typtil(mxtill,mxtlsq) : tillage type
c                             1)  primary
c                             2)  secondary
c     rro(mxtill,mxtlsq)    : random roughness value after tillage (m)
c     rho(mxtill,mxtlsq)    : ridge height value after tillage (m)
c     tdmean(mxtill,mxtlsq) : mean tillage depth (m)
c     nrdril                : drill row number
c     cltpos                : cultivator position
c     surdis(mxtill,mxtlsq) : fraction of soil surface disturbed by
c                             a pass of a tillage implement (0-1)
c     resman(mxtill,mxtilsq): residue management option for this operation
c                             10 = residue addition with no disturbance
c                             11 = residue removal with no disturbance
c                             12 = residue addition with disturbance
c                             13 = residue removal with disturbance
c
c     end include file ctillge.inc
