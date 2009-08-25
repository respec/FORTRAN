c     begin include file cimacl.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imacl/ are common to impoundment
c     element  impint,impmai and endchn subroutines to collect 
c     and write out message about ipoundment automatically clean.

      common /imacl/ alnum(mximp),aldate(mximp,100),
     1               alyear(mximp,100),alvol(mximp,100),alhs(mximp,100)

c     + + + VARIABLE DECLARATIONS + + +

      real alvol,alhs
      integer alnum,aldate,alyear

c     + + + VARIABLE DEFINITIONS + + +

c     alnum(mximp)     : Automatic clean sediment times in a impoundment
c         
c     aldate(mximp,100): used for recording cleanning event date(Julian)
c                        
c     alyear(mximp,100): used for recording cleanning event year.
c  
c     alvol(mximp,1000): used for recording cleanning sediment volume
c                        in a event.
c     alhs(mximp,1000):  used for recording cleanning sediment at which
c                        sediment stage in a event.
c
c     end include file cimacl.inc
