      subroutine chnpar(nptsc)
c
c     + + + PURPOSE + + +
c
c     Outputs the values of channel parameters remaining
c     constant during the simulation.
c
c     Called from: SR CHNERO
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nptsc
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchflo.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchtrl.inc'
      include 'cchvar.inc'
      include 'cpart.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real diamm, seddia, sscly, ssorg, ssslt, sssnd, total, xstr, eqsmm
      integer k, iseg
      character text1(4)*16, text2(2)*23, text3(4)*33
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     diamm  -
c     seddia -
c     sscly  -
c     ssorg  -
c     ssslt  -
c     sssnd  -
c     total  -
c     xstr   -
c     eqsmm  -
c
c     Integer Variables
c
c     k    -
c     iseg -
c
c     Character Variables
c
c     text1(4)*16 -
c     text2(2)*23 -
c     text3(4)*33 -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     seddia
c
c     + + + DATA INITIALIZATIONS + + +
c
      data text1 /'triangular      ', 'rectangular    ',
     1    'naturally eroded', 'trapezoidal     '/
c
      data text2 /'slope of channel       ', 'energy gradeline curves'/
c
      data text3 /'critical flow at outlet          ',
     1    'uniform flow at outlet           ',
     1    'uniform flow at outlet, diff. "n"',
     1    'rating curve at outlet           '/
c
c     + + + END SPECIFICATIONS + + +
c
c
      write (38,1000) ichan
c
      write (38,1100) wtdsoi * ((3.281**3)*0.4536), wtdsoi, wtdh2o * ((
     1    3.281**3)*0.4536), wtdh2o, msdh2o * ((3.281**3)*0.4536),
     1    msdh2o, agrav / 3.281, agrav, knvis / (3.281**2), knvis,
     1    yalcon, beta
c
      ssorg = 1000.0
      sscly = 200.0
      ssslt = 4.0
      sssnd = 0.05
c
      total = solorg(ich(ichan)) + 1.0
c
      sssoil(ich(ichan)) = ssorg * solorg(ich(ichan)) / 1.73 + (sscly*
     1    solcly(ich(ichan))+ssslt*solslt(ich(ichan))+sssnd*
     1    solsnd(ich(ichan))) / total
c
      write (38,1200) solcly(ich(ichan)), sscly, sscly * ((3.281**2)*
     1    453.6), solslt(ich(ichan)), ssslt, ssslt * ((3.281**2)*453.6),
     1    solsnd(ich(ichan)), sssnd, sssnd * ((3.281**2)*453.6),
     1    solorg(ich(ichan)), ssorg, ssorg * ((3.281**2)*453.6),
     1    sssoil(ich(ichan)), sssoil(ich(ichan)) * ((3.281**2)*453.6)
c
      write (38,1300)
c
      do 10 k = 1, cnpart
c
        diamm = 304.8 * crdia(k,ich(ichan))
        eqsand(k,ich(ichan)) =
     1      seddia(2.65,crfall(k,ich(ichan)),knvis,agrav)
        eqsmm = 304.8 * eqsand(k,ich(ichan))
c
        write (38,1400) k, diamm, diamm / 25.4, eqsmm, eqsmm / 25.4,
     1      crfall(k,ich(ichan)), crfall(k,ich(ichan)) / 3.281,
     1      crspg(k), crspg(k) * 62.427, crfrac(k,ich(ichan))
c
   10 continue
c
      write (38,1500)
c
      do 20 k = 1, cnpart
        write (38,1600) k, frcly(k,ich(ichan)), frslt(k,ich(ichan)),
     1      frsnd(k,ich(ichan)), frorg(k,ich(ichan))
   20 continue
c
      write (38,1700) ichan, chnlen(ichan), chnlen(ichan) * 3.281,
     1    uparea(ichan) / 1.0e4, (uparea(ichan)*2.471) / 1.0e4,
     1    loarea(ichan) / 1.0e4, (loarea(ichan)*2.471) / 1.0e4,
     1    toarea(ichan) / 1.0e4, (toarea(ichan)*2.471) / 1.0e4,
     1    chnnbr(ichan), chnz(ichan), text1(ishape(ichan)),
     1    text2(ienslp(ichan))
c
      write (38,1800)
c
      do 30 iseg = 1, nptsc
        xstr = chnx(ichan,iseg) / chnlen(ichan)
        write (38,1900) chnx(ichan,iseg), chnx(ichan,iseg) * 3.281,
     1      xstr, chnslp(ichan,iseg)
   30 continue
c
      write (38,2000) text3(icntrl(ichan)), ctlslp(ichan), ctln(ichan),
     1    ctlz(ichan)
c
      if (icntrl(ichan).eq.4) then
        write (38,2100) rccoef(ichan), rcexp(ichan), rcoset(ichan)
      end if
c
      return
c
 1000 format (//11x,60('p'),//,27x,'channel number ',i2,' parameters'/)
 1100 format (/32x,'initial constants',/32x,17('-'),//7x,
     1    'wt. density soil (in place) ',f7.2,' kgs/m**3',' (',f9.2,
     1    ' lbs/ft**3)',/7x,'wt. density water           ',f7.2,
     1    ' kgs/m**3',' (',f9.2,' lbs/ft**3)',/7x,
     1    'mass density water          ',f7.2,' kgs/m**3',' (',f9.2,
     1    ' lbs/ft**3)',/7x,'acc. due to gravity         ',f7.2,
     1    ' m/sec**2',' (',f9.2,' ft/sec**2)',/7x,
     1    'kinematic viscosity      ',e10.3,' m**2/sec',' (',e9.3,
     1    ' ft**2/sec)',/7x,'yalin constant (all particles)       ',f7
     1    .3,' (unitless)',//,7x,'momentum coeff. for nonuniform',
     1    ' velocity in x-section',f7.2,' (unitless)'//)
 1200 format (1x,/,24x,'distribution of primary particles',/,19x,
     1    'and organic matter in the original soil mass',/,19x,44('-'),
     1    /,19x,'   type        fraction     specific surface',/,22x,4(
     1    '-'),8x,8('-'),5x,16('-'),/,48x,'m**2/g of soil   ft**2/lb',/,
     1    48x,14('-'),3x,8('-'),/,22x,'clay',9x,f5.3,11x,f8.3,5x,e9.3,/,
     1    22x,'silt',9x,f5.3,11x,f8.3,5x,f9.3,/,22x,'sand',9x,f5.3,11x,
     1    f8.3,5x,f9.3,//,43x,'m**2/g of org. car.   ft**2/lb',/,43x,19(
     1    '-'),3x,8('-'),/,12x,'organic matter',9x,f5.3,11x,f8.3,5x,e9
     1    .3,//,22x,'(organic carbon = organic matter/1.73)',//,20x,
     1    'index of specific surface ',f7.2,' m**2/g',/,23x,'(',f9.2,
     1    ' ft**2/lb) of total soil'/)
 1300 format (1x,/,29x,'particle specifications',/,29x,23('-'),//'type',
     1    3x,'diameter',6x,'eqsand dia.',3x,'fall velocity',6x,
     1    'specific grav.',2x,'frac. in',/,1x,'no.',2x,'mm.',4x,'in.',6
     1    x,'mm.',4x,'in.',3x,'ft/sec',3x,'m/sec',3x,'g/cm**3',1x,
     1    'lbs/ft**3',1x,'det. sed.',/,4('-'),2x,3('-'),4x,3('-'),6x,3(
     1    '-'),4x,3('-'),3x,6('-'),3x,5('-'),3x,7('-'),1x,9('-'),1x,9(
     1    '-'))
 1400 format (i2,3x,f5.3,2x,f6.4,2x,f5.3,2x,f6.4,1x,e8.3,1x,e8.3,3x,f4
     1    .2,4x,f6.2,4x,f4.2)
 1500 format (1x,/,31x,'particle composition',/,31x,20('-'),//,12x,
     1    'type',15x,'primary particle fractions',/,13x,'no.',6x,'clay',
     1    7x,'silt',7x,'sand',5x,'organic matter',/,12x,4('-'),6x,4('-'
     1    ),7x,4('-'),7x,4('-'),5x,14('-'))
 1600 format (13x,i2,1x,3(5x,f6.3),8x,f6.3)
 1700 format (///,28x,'channel ',i2,' fixed parameters',/,28x,27('-'),
     1    //,16x,'channel length         ',f8.2,'  m',' (',f9.2,
     1    ' feet)',/,16x,'drainage area upper end',f8.2,' ha',' (',f8.2,
     1    ' acres)',/,16x,'drainage area lower end',f8.2,' ha',' (',f8
     1    .2,' acres)',/,16x,'drainage area at outlet',f8.2,' ha',' (',
     1    f8.2,' acres)',/,16x,'bare channel manning n ',f8.3,/,16x,
     1    '"best-fit" side slope  ',f8.3,/,16x,'channel x-section = ',
     1    a16,/,16x,'energy slope      = ',a23,//)
 1800 format (/29x,'slope steepness along channel',/,29x,29('-'),//,23x,
     1    'distance    distance    distance',/,23x,
     1    '(meters)     (feet)     (nondim)     slope',/,23x,3(8('-'),4x
     1    ),1x,5('-'))
 1900 format (22x,f7.2,6x,f7.2,5x,f7.4,5x,f7.4)
 2000 format (//,26x,'control section parameters',/,26x,26('-'),//,26x,
     1    a33,//,26x,'bed slope          ',f7.4,/,26x,
     1    'mannings roughness ',f7.3,/,26x,'side slope         ',f7.2)
 2100 format (/,26x,'rating curve parameters',//,26x,
     1    'q = ra*(y-ybase)**rn',/,26x,'ra    = ',f8.3,/,26x,'rn    = ',
     1    f8.3,/,26x,'ybase = ',f7.2)
      end
