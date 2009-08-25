      subroutine convrt
c
c     + + + PURPOSE + + +
c
c     SR CONVRT converts WEPP variable names and units into
c     CREAMS variable names and respective units.
c
c     Called from: SR CHNCON
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
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchflo.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchsed.inc'
      include 'cchtrl.inc'
      include 'cchvar.inc'
      include 'cgully.inc'
      include 'cpart.inc'
      include 'csolva1.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      integer k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     k -
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
c     convert WEPP variable names into CREAMS names and convert
c     variable units from metric to english
c
c     convert soil texture variables
c
      solcly(ich(ichan)) = clay(1,ich(ichan))
      solsnd(ich(ichan)) = sand(1,ich(ichan))
      solslt(ich(ichan)) = silt(1,ich(ichan))
      solorg(ich(ichan)) = orgmat(1,ich(ichan))
c
      do 10 k = 1, cnpart
c
c       use dia(k,ich(ichan)) here because we have already
c       read in dia() for hillslopes and assigned dia()
c       for channels (i.e., dia() is indexed by element)
c
        crdia(k,ich(ichan)) = dia(k,ich(ichan)) * 3.281
        crspg(k) = spg(k)
c
c       water balance for channels is indexed from 1 to
c       nplane (which is equivalent to nchan) so frac and
c       fall are indexed here from 1 to nchan; crfrac and
c       crfall are indexed by element (i.e., ich(ichan)
c       corresponds to proper element in structure file)
c
        crfrac(k,ich(ichan)) = frac(k,ichan)
        crfall(k,ich(ichan)) = fall(k,ichan) * 3.281
c
        frcly(k,ich(ichan)) = frcly(k,ichan)
        frslt(k,ich(ichan)) = frslt(k,ichan)
        frsnd(k,ich(ichan)) = frsnd(k,ichan)
        frorg(k,ich(ichan)) = frorg(k,ichan)
c
   10 continue
c
      return
      end
