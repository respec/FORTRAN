c     begin include file cimflg.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imflg/ are common to impoundment
c     element  impint and impmai subroutines to modify the 
c     overtop flow when neither Rock-fill check dam, Emergency
c     spillway nor Filter fence don't exist.

      common /imflg/ fids(mximp),fcv1(mximp),fcv2(mximp),
     1   firf(mximp),fies(mximp),fiff(mximp),fipr(mximp)

c     + + + VARIABLE DECLARATIONS + + +

      integer fids,fcv1,fcv2,firf,fies,fiff,fipr

c     + + + VARIABLE DEFINITIONS + + +

c     fcv1(mximp)       - flag for culvert #1
c     fcv2(mximp)       - flag for  culvert #2
c     fids(mximp)       - flag for drop spillway
c     fies(mximp)       - emergency spillway indicator
c     fiff(mximp)       - filter fence or straw bale indicator
c     fipr(mximp)       - perforated riser indicator
c     firf(mximp)       - porous rock fill indicator
     

c     end include file cimflg.inc
