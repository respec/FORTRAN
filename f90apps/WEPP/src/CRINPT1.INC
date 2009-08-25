c     begin include file crinpt1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /rinpt1/   animal(mxgraz,mxcrop,mxplan),
     1                  bodywt(mxgraz,mxcrop,mxplan),
     1                  suppmt(mxplan),digmin(mxplan),digmax(mxplan),
     1                  gday(mxgraz,mxcrop,mxplan),
     1                  gend(mxgraz,mxcrop,mxplan),it(mxplan),
     1                  ssday(mxgraz,mxplan),send(mxgraz,mxplan),
     1                  area(mxplan),jgraz(mxplan),access(mxplan),
     1                  woody(mxplan),yield(mxplan),pyield(mxplan),
     1                  ianflg

c     + + + VARIABLE DECLARATIONS + + +

      integer           gday,gend,ssday,send,jgraz,it,ianflg,woody
      real              animal,bodywt,suppmt,digmin,digmax,access,
     1                  area,yield,pyield

c     + + + VARIABLE DEFINITIONS + + +

c     animal(mxgraz,mxcrop,mxplan) : animal units grazing each npast
c                                    pastures (animal units per year)
c     digmin(mxplan)               : minimum digestibility of forage index (0-1)
c     digmax(mxplan)               : maximum digestibility of forage index (0-1)
c     bodywt(mxgraz,mxcrop,mxplan) : average body weight of an animal (kg)
c     suppmt(mxplan)               : average amount of supplement feed per day
c                                    (kg/day)
c     gday(mxgraz,mxcrop,mxplan)   : julian day grazing starts
c     gend(mxgraz,mxcrop,mxplan)   : julian day grazing stops
c     it(mxplan)                   : counter used to compute 4-day average water
c                                    stress in subroutine range
c     ssday(mxgraz,mxplan)         : julian day supplementary feeding begins
c     send(mxgraz,mxplan)          : julian day supplementary feeding ends
c     area(mxplan)                 : pasture area
c     jgraz(mxplan)                : grazing cycle per pasture per simulation
c     access(mxplan)               : fraction of forage available for
c                                    consumption (0-1)
c     woody(mxplan)                : a flag for user to specify whether
c                                    defoliation is instantaneous (woody=0) or
c                                    occurs over several months (woody = 1)
c     yield(mxplan)                : total above ground plant production for a
c                                    simulation year (kg/m^2)
c     pyield(mxplan)               : daily net primary plant production (kg/m^2)

      include 'crinpt1a.inc'

c     end include file crinpt1.inc

