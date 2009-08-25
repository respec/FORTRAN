      subroutine chnvar(ichan,length)
c
c     + + + PURPOSE + + +
c
c     SR CHNVAR outputs values of updateable channel parameters.
c
c     Called from: SR CHNERO
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:  Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real length
      integer ichan
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     ichan   -
c     length  -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchvar.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real toplen
c
c     + + + LOCAL DEFINITIONS + + +
c
c     toplen -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      toplen = chnlef - length
c
      write (38,1000) ichan, toplen * 3.281, chnlef * 3.281,
     1    chnks(ichan) * 141723.0, chnn(ichan), chntcr(ichan) * .021,
     1    chnk(ichan), chneds(ichan) * 3.281, chnedm(ichan) * 3.281
c
      return
 1000 format (1x,///,26x,'channel',i2,' updateable parameters',/,26x,31(
     1    '-'),/,23x,'effective top length       ',f8.2,' feet',/,23x,
     1    'effective channel length   ',f8.2,' feet',/,23x,
     1    'hydraulic conductivity     ',f8.3,' in/hr',/,23x,
     1    'mannings roughness         ',f8.3,/,23x,
     1    'critical shear stress      ',f8.3,' lb/ft**2',/,23x,
     1    'channel erodibility factor ',f8.3,/,23x,
     1    'erodible depth to side     ',f8.3,' feet'/,23x,
     1    'erodible depth to middle   ',f8.3,' feet')
      end
