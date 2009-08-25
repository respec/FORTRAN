      subroutine chncon
c
c     + + + PURPOSE + + +
c
c     Called from: SR WSHINI
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
      include 'pmxtim.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchtrl.inc'
      include 'cchvar.inc'
      include 'cgully.inc'
      include 'cpart1.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
c
c
c     + + + LOCAL VARIABLES + + +
c
      real deltlm, sslp(mxcseg), xslp(mxcseg)
      integer i, idwn, j, nptsc
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     deltlm       -
c     sslp(mxcseg) -
c     xslp(mxcseg) -
c
c     Integer Variables
c
c     i      -
c     idwn   -
c     j      -
c     nptsc  -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     chnpar
c     convrt
c     table
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      agrav = 32.2
      msdh2o = 1.94
      wtdh2o = 62.4
      wtdsoi = 96.0
      knvis = 1.05e-05
      yalcon = 0.635
      beta = 1.56
      cnpart = npart
      nptsc = 11
c
      do 40 ichan = 1, nchan
c
c       compute computational channel segment length
c
        deltlm = chnlen(ichan) / (nptsc - 1)
c
c       insert slope point measurement at channel inlet and
c       and insert into local array
c
        idwn = ncsseg(ichan) + 1
c
        do 10 i = 1, ncsseg(ichan)
          xslp(idwn) = chnx(ichan,idwn-1)
          sslp(idwn) = chnslp(ichan,idwn-1)
          idwn = idwn - 1
   10   continue
c
        xslp(1) = 0.0
        sslp(1) = sslp(2)
c
c       divide the channel into nptsc-1 homogeneous slope segments
c
        chnx(ichan,nptsc) = chnlen(ichan)
        chnslp(ichan,nptsc) = chnslp(ichan,ncsseg(ichan))
c
        chnx(ichan,1) = xslp(1)
        chnslp(ichan,1) = sslp(1)
c
        do 20 i = 2, nptsc - 1
c
c         compute the slope for each computational segment
c         by interpolating the slope-distance input pairs
c
          chnx(ichan,i) = chnx(ichan,i-1) + deltlm
          call table(2,ncsseg(ichan)+1,xslp,sslp,chnx(ichan,i),
     1        chnslp(ichan,i))
c
   20   continue
c
c       convert WEPP metric variables into CREAMS english variables
c
        call convrt
c
c       write initial channel parameters
c
c       call chnpar(nptsc)
c
c       set initial depth and width of non-erodible layer
c
        do 30 j = 1, nptsc
          depa(ichan,j) = chnedm(ichan) * 3.281
          depb(ichan,j) = chnedm(ichan) * 3.281
          wida(ichan,j) = chnwid(ichan) * 3.281
          widb(ichan,j) = chnwid(ichan) * 3.281
c          depa(ichan,j) = chnedm(ichan)
c          depb(ichan,j) = chnedm(ichan)
c          wida(ichan,j) = chnwid(ichan)
c          widb(ichan,j) = chnwid(ichan)
   30   continue
c
   40 continue
c
      return
      end
