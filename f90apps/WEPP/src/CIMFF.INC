c     begin include file cimff.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imff/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables are used
c     for describing the feature of straw bales, filter fence,
c     or trash barriers

      common /imff/ ffiff(mximp),
     1    fvsl(mximp),fwdff(mximp),fhff(mximp),fhotff(mximp)

c     + + + VARIABLE DECLARATIONS + + +
  
      integer ffiff
      real fvsl,fwdff,fhff,fhotff
    
c     + + + VARIABLE DEFINITIONS + + +


c 
c       ffiff(mximp)    - flag for judging whether straw bales,
c                         filterfence,or   
c                         trash barriers exists in an impoundment.
c                         0 for no this kind of structure,
c                         1 for choosing sharp crested weir for
c                         filterfence,
c                         2 for choosing broad crested weir for
c                         straw bales or trash barriers.
c       fvsl(mximp)     - Slurry flow rate
c       fwdff(mximp)    - Cross-sectional width of filter fence,
c                         straw bales or
c                         trash barriers
c       fhff(mximp)     - stage at which flow begins
c       fhotff(mximp)   - stage at which overtopping flow occurs

c     end include file cimff.inc
