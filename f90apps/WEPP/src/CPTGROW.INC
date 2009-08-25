
c     begin include file cptgrow.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /ptgrow/   ngraz(mxplan),idecom(mxplan),ifreez(mxplan),
     1                  icount(mxplan),iburn(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           ngraz,idecom,ifreez,icount,iburn

c     + + + VARIABLE DEFINITIONS + + +

c     ngraz  : index of the current grazing cycle
c     ifreez : set to 1 the first day the ambient
c              temperature falls below that required for [this type of]
c              plant growth.  At this time, live biomass is converted
c              to dead biomass.  IFREEZ is reset to zero on the first
c              "warm" day, and at planting.
c     idecom : when IDECOM is set (idecom=1) RESUP is called (residue
c              parameters are updated), and IDECOM is reset (idecom=0)
c     iburn  : tells whether ambient temperature is above upper
c              limit for plant growth.  0=OK; 1=too hot.  Stops growth
c              for 5 days.
c     icount : number of consecutive days below upper temperature limit,
c              once limit is exceeded.  (Five days required to resume
c              growth.)
c
c     end include file cptgrow.inc

