      subroutine enrcmp(flag,ielmt,nelmt,tgs,elem)
c
c     + + + PURPOSE + + +
c
c     SR ENRCMP computes the textural composition of deposited
c     sediment and the specific surface area index.
c
c     Called from: SRS ANNCHN, ENDCHN, MONCHN, STROUT
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
      include 'pmxprt.inc'
      include 'pmxelm.inc'
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real tgs(mxelem)
      integer flag, ielmt, nelmt, elem
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     tgs   -
c     flag  -
c     ielmt -
c     nelmt - total number of elements
c     elem  - flag : 2 for a channel, 3 for an impoundment.
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpar.inc'
      include 'cchprt.inc'
      include 'cchsed.inc'
      include 'cpart.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real sedcly, sedorg, sedslt, sedsnd, sscly, ssorg, sssed, ssslt,
     1    sssnd, sumcly, sumorg, sumsed, sumslt, sumsnd, total
      integer flgprt, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     sedcly -
c     sedorg -
c     sedslt -
c     sedsnd -
c     sscly  -
c     ssorg  -
c     sssed  -
c     ssslt  -
c     sssnd  -
c     sumcly -
c     sumorg -
c     sumsed -
c     sumslt -
c     sumsnd -
c     total  -
c
c     Integer Variables
c
c     flgprt -
c     k      -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      flgprt = 0
c
      sumsed = 0.0
      sumcly = 0.0
      sumslt = 0.0
      sumsnd = 0.0
      sumorg = 0.0
c
      ssorg = 1000.0
      sscly = 200.0
      ssslt = 4.0
      sssnd = 0.05
c
      total = solorg(ielmt) + 1.0
c
      sssoil(ielmt) = ssorg * solorg(ielmt) / 1.73 + (sscly*
     1    solcly(ielmt)+ssslt*solslt(ielmt)+sssnd*solsnd(ielmt)) / total
c
c     sstot=sstot+sssoil(ielmt)
c
c     if(ielmt.eq.nelmt)then
c     do 10 i = nhill + 1, nelmt
c     enrch2=enrch2 + (tgs(i)*(sssoil(i)/sstot))
c     10   continue
c     end if
c
      do 10 k = 1, cnpart
        sumsed = sumsed + conc(k,ielmt)
        sumcly = sumcly + conc(k,ielmt) * frcly(k,ielmt)
        sumslt = sumslt + conc(k,ielmt) * frslt(k,ielmt)
        sumsnd = sumsnd + conc(k,ielmt) * frsnd(k,ielmt)
c
c
c       XXX flgprt set to 0 at beginning of this routine
c       sjl 06-26-95 03:45pm
c
        if (flgprt.ne.0) then
          sumorg = sumorg + conc(k,ielmt) * frorg(k,ielmt)
        end if
c
   10 continue
c
c
      if (sumsed.gt.0.0.and.elem.eq.2) then
c
        sedcly = sumcly / sumsed
        sedslt = sumslt / sumsed
        sedsnd = sumsnd / sumsed
c
c
c       XXX flgprt set to 0 at beginning of this routine
c       sjl 06-26-95 03:45pm
c
        if (flgprt.ne.0) then
          sedorg = sumorg / sumsed
        else
          sedorg = solorg(ielmt) * sedcly / solcly(ielmt)
        end if
c
        total = 1.0 + sedorg
        sssed = ssorg * sedorg / 1.73 + (sscly*sedcly+ssslt*sedslt+
     1      sssnd * sedsnd) / total
        enrich = sssed / sssoil(ielmt)
c
      else
c
c       if the last element is an impoundment or if the runoff is clear
c       of sediment, the enrichment ratio is not calculated.
c
        enrich = 1001
c
      end if
c
      if(enrich.lt.1000) then
        if (watsum.gt.0.) then
c
          if (flag.gt.1) write (38,1000) sedcly, sedslt, sedsnd, sedorg,
     1        sssed, enrich
c
        else if (watsum.eq.0.and.ielmt.eq.nelmt) then
c
          if (flag.gt.1) write (38,1000) sedcly, sedslt, sedsnd, sedorg,
     1        sssed, enrich
c
        end if
c
      else
        if (watsum.gt.0.or.ielmt.eq.nelmt) then
c
          if (flag.gt.1) write (38,1100)
c
        end if
c
      end if
c
      return
 1000 format (//'Distribution of Primary Particles and',/,
     1    'Organic Matter in the Eroded Sediment:',//,41('-'),/,7x,
     1    'type',12x,'fraction',/,7x,4('-'),12x,8('-'),/,7x,'clay',13x,
     1    f6.3,/,7x,'silt',13x,f6.3,/,7x,'sand',13x,f6.3,/,3x,
     1    'organic matter',7x,f6.3,/,41('-'),//,
     1    'Index of specific surface            =   ',f7.2,' m**2/g of',
     1    ' total sediment',/,
     1    'Enrichment ratio of specific surface =   ',f7.2/)
 1100 format (//
     1    'The Enrichment Ratio is not calculated in this case.'/)
      end
