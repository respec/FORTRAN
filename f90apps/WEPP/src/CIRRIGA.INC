
c     begin include file cirriga.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irriga/ are common to all types of irrigation
c     systems and irrigation scheduling schemes.

      common /irriga/   irdept(mxplan),irfrac,irofe,irschd(mxplan),
     1                  irsyst,noirr,irabrv

c     + + + VARIABLE DECLARATIONS + + +

      integer           irofe,irschd,irsyst,noirr,irabrv
      real              irdept,irfrac

c     + + + VARIABLE DEFINITIONS + + +

c     irabrv         : irrigation flag to be used in abbrev.raw,
c                      same as irsyst except it is not reset to 0 at end
c                      of simulation
c     irdept(mxplan) : depth of water applied by irrigation (m)
c     irfrac         : ratio of irrigation amount to total water applied
c                      (rainfall plus irrigation) for the day (m/m)
c     irofe          : flag used to determine the overland flow element
c                      dependent parameters to be used for the
c                      irrigation
c     irschd(mxplan) : irrigation scheduling option for each overland
c                      flow element
c     irsyst         : flag indicating irrigation system type (0-no
c                      irrigation, 1-sprinkler irrigation, 2-furrow
c                      irrigation)
c     noirr          : flag indicating whether irrigation occurs on the
c                      current simulation day and the type of irrigation
c                      scheduling used if irrigation does occur (0-no
c                      irrigation, 1-depletion level scheduling, 2-fixed
c                      date scheduling)
c

c     end include file cirriga.inc

