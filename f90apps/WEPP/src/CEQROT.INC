
c     begin include file ceqrot.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /eqrot/  ax

c     + + + VARIABLE DECLARATIONS + + +

      real            ax

c     + + + VARIABLE DEFINITIONS + + +

c     ax     :  the saved value of constant A in the equation:
c               1 - exp(-u) = a*u.  (In this routine, A = 1/ip.)
c               Used in subroutine eqroot.
c
c               (Note that since A cannot be zero, setting AX to zero forces
c               evaluation of EQRT the first time though subroutine eqroot)

c     end include file ceqrot.inc

