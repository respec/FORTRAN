      subroutine infile(ncrop,jstruc,nsurf,nrots,nyears,
     1    iofe,iwpass)
c
c     + + + PURPOSE + + +
c
c     Opens input files for the reading in of the slope,
c     cropping practice, soil, and management files.
c
c     Called from: SR CONTIN
c     Author(s): Livingston, Flanagan, Ascough II
c     Reference in User Guide:
c
c     Version: This module not yet recoded.
c     Date recoded:
c     Recoded by:
c
c     Input files are read in the following subroutines:
c
c     Input File                         Unit #       Subroutine
c     ----------                         ------       ----------
c     Slope file                           10         PROFIL
c     Soil file                            11         INPUT
c     Management file                      12         INFILE, TILAGE
c     Storm file                           13         STMGET
c     Fixed date irrigation file           14         IRINPT
c     Depletion level irrigation file      15         IRINPT
c     Watershed structure file             17         WSHINP
c     Watershed channel data file          18         WSHINP
c     Hillslope\watershed pass file        19         WSHINP, WSHDRV
c     Watershed impoundment file           20         IMPINT
c     P-M ET threshold for crop dev.stg    21         PMETCOEF
c     P-M ET crop basal coefficient        22         PMETCOEF
c     P-M ET coefcient for RAW formula     23         PMETCOEF
c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer  ncrop, jstruc, nsurf, nrots, nyears, iofe,
     1    iwpass
c     real irdchk,irfchk
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     ncrop  -
c     jstruc -
c     nsurf  -
c     nrots  - number of times rotation is repeated
c     nyears - number of years in a single rotation
c     iofe   -
c     iwpass - hillslope pass file creation flag
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccdrain.inc'
c     modify: idrain, ddrain, drainc, sdrain, drainq, satdep,
c             drdiam, drseq
c
      include 'cclig.inc'
c     read:   iclig
c
      include 'cclim.inc'
c
      include 'ccliyr.inc'
c     modify: ibyear, numyr
c
      include 'ccntour.inc'
c     modify: cntslp, rowspc, rowlen, rdghgt
c
      include 'ccover.inc'
c     modify: cancov, inrcov, rilcov, lanuse, daydis,
c
      include 'ccrpout.inc'
c     modify: rescov(mxplan)
c
      include 'ccrpprm.inc'
c     read:   yld, betemp
c
      include 'ccrpgro.inc'
c     read:   be,otemp,hi,hia,vdmx,beinp,daymin,daylen,ytn,y4
c
      include 'ccrpvr1.inc'
c     modify: pltol(ntype)
c
      include 'ccrpvr2.inc'
c     modify: resamt, cn(ntype), aca(ntype), as(ntype), cf(ntype),
c             ar(ntype), sminit, rminit, fct1(mxplan), fct2(mxplan),
c             vdmt(mxplan)
c
      include 'ccrpvr3a.inc'
c     modify: hmax(ntype), crit(ntype), bb(ntype), bbb(ntype),
c             rsr(ntype), spriod(ntype), dlai(ntype), gssen(ntype),
c             xmxlai(ntype), rdmax(ntype), gddmax(ntype), decfct(ntype),
c             dropfc(ntype), mfocod(ntype)
c
      include 'ccrpvr5a.inc'
c     modify: pltsp(ntype), diam(ntype)
c
      include 'cdecvar1.inc'
c     modify: cuthgt(ntype), oratea(ntype), orater(ntype)
c
      include 'cends4.inc'
      include 'cerrid.inc'
c
      include 'cflags.inc'
c     read: yldflg
c
      include 'cirriga.inc'
c     modify: irsyst,irschd(1)
c
      include 'cinpop.inc'
c     read:   rro1(ntype*2), rho1(ntype*2), rint1(ntype*2), tdmea1(ntype*2),
c             mfo11(ntype*2), mfo21(ntype*2), code1, numof1, cltps1
c
      include 'cinpsur.inc'
c     modify: lantyp, mdat1, tilde1, op
c
      include 'cinpman1.inc'
c     modify: iscen, itype, tilse1, conse1, irrset, imngm1,
c             tilla1(1,i), tilla1(2,i), jdpl1, jdhar1, r1, resmg1,
c             jdher1, jdbur1, fbrno1, fbrna1, jdslg1, jdcu1, frcu1,
c             jdmov1, frmov1, jdsto1, mgtop1, ncu1, cutda1, ncycl1,
c             gda1, gen1, anima1, bodyw1, are1, diges1, jfdat, ihdat1,
c             grazi1, ntill, ityp1
c
      include 'cimpnd.inc'
c     read:   impond
c
      include 'cke.inc'
c     read:   ksflag
      include 'cnew.inc'
c     read:   plunit,numof,rmfo1,rmfo2,deglon,elev,obsyrs
c             (variables read but not used at this time)
c     read:   manver
      include 'cnew1.inc'
c
      include 'cobclim.inc'
c     modify: obmaxt,obmint,radave,obrain
c
      include 'cparame.inc'
      include 'cperen1.inc'
c
      include 'crinpt2.inc'
c     read:   pptg(ntype), rootf(ntype), rdf(ntype), pscday(ntype),
c             strrgc(ntype), cshape(ntype), dshape(ntype), scday2(ntype),
c             strgc2(ntype), eshape(ntype), fshape(ntype), rgcmin(ntype),
c             cf1(ntype), cf2(ntype), gtemp(ntype),tempmn(ntype),
c             root10(ntype), ffp(ntype)
c
      include 'crinpt1a.inc'
      include 'crinpt3a.inc'
      include 'crinpt5.inc'
      include 'crinpt6.inc'
      include 'cslinit.inc'
c
      include 'cstruc.inc'
c     modify: iplane, nplane
c
      include 'ctemp.inc'
c
      include 'ctillge.inc'
c     modify: tillay(1,iplane), tillay(2,iplane)
c
      include 'cwint.inc'
c     modify: deglat
c
      include 'cwshed.inc'
c     modify: wshcli(mxhill)
c     read:   watfil
c
      include 'cupdate.inc'
c     modify: mdate
c
c     internal version control
c
      include 'cver.inc'
      include 'cdat.inc'
c
c     winter flags
c
      include 'CFlgFS.inc'      
c
c
c     + + + LOCAL VARIABLES + + +
c
      real canco1(ntype), inrco1(ntype), rilco1(ntype), widt1(ntype),
     1    rspac1(ntype), rrini1(ntype), rhini1(ntype), bdtil1(ntype),
     1    rfcu1(ntype), snodp1(ntype), frd1(ntype), thd1(ntype),
     1    rmog1(ntype), rmag1(ntype), wc1(ntype), crypt1(ntype),
     1    ppt1(ntype), rroug1(ntype), daydi1(ntype), datver, tiltmp,
     1    rtm1(ntype), smrm1(ntype), litcv1(ntype), rokcv1(ntype),
     1    bascv1(ntype), crycv1(ntype), fresr1(ntype), fresi1(ntype),
     1    frokr1(ntype), froki1(ntype), fbasr1(ntype), fbasi1(ntype),
     1    fcryr1(ntype), fcryi1(ntype)
c
c*      **Added by Kidwell on 5/25/95
c
      real rescof1(ntype), cancof1(ntype)
c
c*      **Added by Kidwell on 6/6/95
c
      real resr1(ntype), resi1(ntype), rokr1(ntype), roki1(ntype),
     1    basr1(ntype), basi1(ntype), cryr1(ntype), cryi1(ntype)
c
c
      integer i, j, itemp, jtemp, ktemp, nop, nini, ncnt, ndrain,
     1    nmscen, lanus1(ntype), icont(ntype), rtyp1(ntype),
     1    ires1(ntype), imngm2(ntype), dshar1(ntype), flag, inindx,
     1    inyr, iout, irrig, nowres, nwsofe, ijunk
c
      character*1 ans
      character*51 scenam
      character*51 filen, manfil, solfil, clifil, slpfil, strfil,
     1    chnfil, impfil
      character*65 istrng
      character*75 stmid
      character*21 mesg, solcom
c
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables:
c
c     canco1(ntype) -
c     inrco1(ntype) -
c     rilco1(ntype) -
c     widt1(ntype)  -
c     rspac1(ntype) -
c     rrini1(ntype) -
c     rhini1(ntype) -
c     bdtil1(ntype) -
c     rfcu1(ntype)  -
c     snodp1(ntype) -
c     frd1(ntype)   -
c     thd1(ntype)   -
c     rmog1(ntype)  -
c     rmag1(ntype)  -
c     wc1(ntype)    -
c     crypt1(ntype) -
c     ppt1(ntype)   -
c     rroug1(ntype) -
c     daydi1(ntype) -
c     datver        - data file compatibility number
c     tiltmp        -
c
c     Integer Variables:
c
c     i             -
c     j             -
c     itemp         -
c     jtemp         -
c     ktemp         -
c     nop           - number of different operations used by tillage sequences
c     nini          -
c     ncnt          -
c     ndrain        -
c     nmscen        -
c     lanus1(ntype) -
c     icont(ntype)  -
c     rtyp1(ntype)  -
c     ires1(ntype)  -
c     imngm2(ntype) -
c     dshar1(ntype) -
c     flag          -
c     inindx        -
c     inyr          -
c     iout          -
c     irrig         -
c     nowres        -
c     nwsofe        -
c
c     Character Variables:
c
c     ans       -
c     scenam    -
c     filen     -
c     manfil    -
c     solfil    -
c     clifil    -
c     slpfil    -
c     strfil    -
c     chnfil    -
c     impfil    -
c     mancom(3) -
c     istrng    -
c     stmid     -
c     mesg      -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     eatcom
c     getdat
c     open
c     readin
c     scenhd
c     verchk
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      if (imodel.eq.2) iout = 32
      if (imodel.ne.2) iout = 31
c
      if (ivers.eq.3) iout = 38
c
      write (iout,1300) ver, vermon, veryr
c
      if (ivers.eq.1) write (iout,1400) ver, vermon, veryr
      if (ivers.eq.2) write (iout,1500) ver, vermon, veryr
      if (ivers.eq.3) write (iout,1600) ver, vermon, veryr
c
c     if watershed version then open watershed pass, structure
c     and channel files and perform version checks
c
      if (ivers.eq.3) then
c
c*************************************************
c
c       WATERSHED VERSION PASS FILE SECTION
c       (UNIT=49, STATUS='OLD')
c
c*************************************************
c
c       watershed master pass file is opened in MAIN
c
        read (49,*)
        read (49,*)
c
        read (49,5800) datver
c
        if (datver.gt.10.0) then
          backspace (49)
c
c         version control check - will exit with message if not correct
c
          mesg = 'WATERSHED MASTER PASS'
          call verchk(49,datver,hilchk,mesg,ver)
c
c       no further checks because no ofe information in
c       hillslope/watershed file
c
        end if
c
        write (iout,1700) 'WATERSHED PASS:', watfil
c
c*************************************************
c
c       WATERSHED STRUCTURE FILE SECTION
c       (UNIT=17, STATUS=2)
c
c*************************************************
c
        istrng = 'Enter name of file containing watershed structure data
     1 -->'
        call open(17,2,65,istrng,strfil)
c
        call eatcom(17)
        read (17,*) datver
c
        if (datver.gt.10.0) then
          backspace (17)
c
c         version control check - will exit with message if not correct
c
          mesg = 'WATERSHED STRUCTURE'
          call verchk(17,datver,strchk,mesg,ver)
c
c       no further checks because no ofe information in
c       watershed structure file
c
        end if
c
        write (iout,1700) 'WAT. STRUCTURE:', strfil
c
c*************************************************
c
c       WATERSHED CHANNEL FILE SECTION
c       (UNIT=18, STATUS=2)
c
c*************************************************
c
        istrng =
     1      'Enter name of file containing watershed channel data -->'
        call open(18,2,65,istrng,chnfil)
c
        call eatcom(18)
        read (18,*) datver
c
        if (datver.gt.10.0) then
          backspace (18)
c
c         version control check - will exit with message if not correct
c
          mesg = 'WATERSHED CHANNEL'
          call verchk(18,datver,chnchk,mesg,ver)
c
c       checks on number of channels in the channel file versus
c       number of channels in the structure file made in SR WSHINP
c
        end if
c
        write (iout,1700) 'WAT. CHANNEL:', chnfil
c
c*************************************************
c
c       WATERSHED IMPOUNDMENT FILE SECTION (ONLY
c       IMPOUNDMENTS ARE MODELED)
c
c       (UNIT=20, STATUS=2)
c
c*************************************************
c
        if (impond.gt.0) then
c
          istrng = 'Enter name of file containing impoundment data -->'
          call open(20,2,65,istrng,impfil)
c
          call eatcom(20)
          read (20,*) datver
c
          if (datver.gt.10.0) then
            backspace (20)
c
c           version control check - will exit with message if not correct
c
            mesg = 'WATERSHED IMPOUNDMENT'
            call verchk(20,datver,impchk,mesg,ver)
c
c         checks on number of impoundments in the impoundment
c         file versus number of impoundments in the structure
c         file made in SR IMPINT
c
          end if
c
          write (iout,1700) 'IMPOUNDMENT:', impfil
c
        end if
c
      end if
c
      irsyst = 0
      irabrv = 0
      rngout = 0
c
c*******************************
c
c     MANAGEMENT FILE SECTION
c     (UNIT=12, STATUS=2)
c
c*******************************
c
      istrng = 'Enter name of file containing management data -->'
      flag = 1
c
      call open(12,2,flag,istrng,manfil)
c
c     version control check - will exit with message if not correct.
c
      mesg = 'MANAGEMENT'
c     if the management file is older than manchk exit
c     with an error
      call verchk(12,datver,manchk,mesg,95.7)
      manver=datver
c
c     following values required for the file builder but are not
c     used elsewhere in the code
c
      call readin(12,iofe,1,ntype,'iofe        ')
      call readin(12,inyr,1,10000,'inyr        ')
c
c
c*******************************
c
c     PLANT GROWTH PARAMETERS SECTION
c
c*******************************
c
c
c     ncrop = NUMBER OF PLANT SCENARIOS
c
      call readin(12,ncrop,1,ntype,'ncrop       ')
c
c
c     LOOP NCROP TIMES
c
      do 10, i = 1, ncrop
c
c       read the scenario name, comments, and landuse.
c
        call scenhd(12,crpnam(i),iplant(i))
c
c
c       if yield output requested (yldflg=1) then write crop index(i)
c       and name associated with that number
c
        if (yldflg.eq.1) write (46,3500) i, crpnam(i)
c
        if (iplant(i).eq.1) then
c
c         CROPLAND PLANT
c
          read (12,2100) plunit
c
          call eatcom(12)
          read (12,*) bb(i), bbb(i), beinp(i), btemp(i), cf(i),
     1        crit(i), critvm(i), cuthgt(i), decfct(i), diam(i)
c
          call eatcom(12)
          read (12,*) dlai(i), dropfc(i), extnct(i), fact(i),
     1        flivmx(i), gddmax(i), hi(i), hmax(i)
c
          gddmip(i) = gddmax(i)
c
          if (extnct(i).le.0.0) extnct(i) = 0.65
c
          call eatcom(12)
          read (12,*) mfocod(i)
c
          call eatcom(12)
          read (12,*) oratea(i), orater(i), otemp(i), pltol(i),
     1        pltsp(i), rdmax(i), rsr(i), rtmmax(i), spriod(i),
     1        tmpmax(i)
c
          if (spriod(i).eq.0) spriod(i) = 14
          call eatcom(12)
          read (12,*) tmpmin(i), xmxlai(i), yld(i)
c
          partcf(i) = cuthgt(i) / hmax(i)
c
        else if (iplant(i).eq.2) then
c
c         RANGELAND PLANT
c
          call eatcom(12)
          read (12,*) aca(i), aleaf(i), ar(i), bbb(i), bugs(i), cf1(i),
     1        cf2(i), cn(i), cold(i), ffp(i)
          call eatcom(12)
          read (12,*) gcoeff(i), gdiam(i), ghgt(i), gpop(i), gtemp(i),
     1        hmax(i), plive(i,1), pltol(i), pscday(i), rgcmin(i)
c
          call eatcom(12)
          read (12,*) root10(i), rootf(i), scday2(i), scoeff(i),
     1        sdiam(i), shgt(i), spop(i), tcoeff(i), tdiam(i),
     1        tempmn(i)
c
          call eatcom(12)
          read (12,*) thgt(i), tpop(i), wood(i)
c
        else if (iplant(i).eq.3) then
c
c         FOREST PLANT
c
          write (6,5100) 'FOREST', 'PLANTS'
          stop
c
        else
c
c         ROAD PLANT
c
          write (6,5100) 'ROAD', 'PLANTS'
          stop
c
        end if
c
c       detailed rangeland outputs
c
        if (iplant(i).eq.2) rngout = 1
c
   10 continue
c
      if (rngout.eq.1) then
        ans = 'N'
        write (6,3600)
        read (5,3800,err=20) ans
c
c       print*,'range?',ans
c
   20   if (ans.eq.'Y'.or.ans.eq.'y') then
          ans = 'Y'
        end if
c
        if (ans.eq.'Y') then
c
c         open file for rangeland plant output
c
          istrng = 'Enter name of range plant output file -->'
          call open(44,iost,65,istrng,filen)
          write (44,3900)
          rngplt = 2
        end if
c
        write (6,3700)
        ans = 'N'
        read (5,3800,err=30) ans
c
c       print*,'range?',ans
c
   30   if (ans.eq.'Y'.or.ans.eq.'y') then
          ans = 'Y'
        end if
c
        if (ans.eq.'Y') then
c
c         open file for rangeland animal output
c
          istrng = 'Enter name of animal output file -->'
          call open(45,iost,1,istrng,filen)
          write (45,4000)
          rnganm = 2
        end if
c
        rngout = 2
c
      end if
c
c*******************************
c
c     OPERATIONS SECTION
c
c*******************************
c
c     nop = number of unique surface operation types
c
      call readin(12,nop,0,ntype*2,'nop         ')
c
c
c     LOOP NOP TIMES
c
      do 40, i = 1, nop
c
c       read the scenario name, comments, and landuse
c
        call scenhd(12,scenam,iop(i))
c
c       if cropland operation
        if (iop(i).eq.1) then
c
c         CROPLAND OPERATION
c
          call eatcom(12)
          read (12,*) mfo11(i), mfo21(i), numof
c
c         flag for cultivator type if less than 5
c         flag for residue management if greater than 9
c
          call readin(12,code1,1,13,'code        ')
          resma1(i)=code1
          if (code1.le.4)then
c
            if (code1.eq.3) then
              call readin(12,cltpos,1,2,'cltpos      ')
            end if
c
          end if
c
          call eatcom(12)
          read (12,*) rho1(i), rint1(i), rmfo1(i), rmfo2(i), rro1(i),
     1      surdi1(i), tdmea1(i)
c
          if(datver.ge.98.3)then
c
            if (resma1(i).eq.10.or.resma1(i).eq.12)then
c
c           RESIDUE ADDITION, additional data line for new files
c             resma1(i) = residue management flag
c                         10 = residue addition without surface disturbance
c                         12 = residue addition with disturbance
c             iresa1(i) = index of residue pointer
c
c
                call readin(12,iresa1(i),1,ncrop,'iresad      ')
c
c             resad1(i)  = amount of residue added (kg/m^2)
c
c
              call eatcom(12)
c
              read(12,*)resad1(i)
            end if
c
            if (resma1(i).eq.11.or.resma1(i).eq.13)then
c             RESIDUE REMOVAL additional data line for new files
c             resma1(i) = residue management flag
c                         11 = residue removal without surface disturbance
c                         13 = residue removal with disturbance
c             frmov1(i)=fraction of residue removed (0-1)
c
              call eatcom(12)
c
              read (12,*) frmov1(i)
            end if
          end if
c
        else if (iop(i).eq.2) then
c
c         RANGELAND OPERATION
c
          write (6,5100) 'RANGELAND', 'OPERATIONS'
          stop
c
        else if (iop(i).eq.3) then
c
c         FOREST OPERATION
c
          write (6,5100) 'FOREST', 'OPERATIONS'
          stop
c
        else
c
c         ROAD OPERATION
c
          write (6,5100) 'ROAD', 'OPERATIONS'
          stop
c
        end if
   40 continue
c
c*******************************
c
c     OVERLAND FLOW ELEMENT INITIAL CONDITIONS SECTION
c
c*******************************
c
c     nini = NUMBER OF INITIAL CONDITION SCENARIOS
c
      call readin(12,nini,1,ntype,'nini        ')
c
c
c     LOOP NINI TIMES
c
      do 50, i = 1, nini
c
c       read the scenario name, comments, and landuse
c
        call scenhd(12,scenam,lanus1(i))
c
        if (lanus1(i).eq.1) then
c
c         CROPLAND INITIAL CONDITION
c
          call eatcom(12)
          read (12,*) bdtil1(i), canco1(i), daydi1(i), dshar1(i),
     1        frd1(i), inrco1(i)
c
          call readin(12,ires1(i),1,ncrop,'iresd       ')
          call readin(12,imngm2(i),1,3,'imngm2      ')
c
          call eatcom(12)
          read (12,*) rfcu1(i), rhini1(i), rilco1(i), rrini1(i),
     1        rspac1(i)
c
          call readin(12,rtyp1(i),0,2,'rtype       ')
          call eatcom(12)
          read (12,*) snodp1(i), thd1(i), tilla1(i), tilla2(i),
     1        widt1(i)
c
c         ADD read of initial conditions for submerged residue mass
c         and dead roots - use version control to determine if file
c         will contain these values.    dcf  5/3/94
c
          if (datver.ge.94.303) then
            call eatcom(12)
            read (12,*) rtm1(i), smrm1(i)
          else
            rtm1(i) = 0.0
            smrm1(i) = 0.0
          end if
c
        else if (lanus1(i).eq.2) then
c
c         RANGELAND INITIAL CONDITION
c
          call eatcom(12)
c
          if (datver.lt.95.102) then
            read (12,*) crypt1(i), frd1(i), ppt1(i), rmag1(i),
     1          rmog1(i), rroug1(i), snodp1(i), thd1(i), tilla1(i),
     1          tilla2(i)
            call eatcom(12)
            read (12,*) wc1(i)
            write (6,5900)
            write (iout,5900)
            stop
          else
            call eatcom(12)
            read (12,*) frd1(i), ppt1(i), rmag1(i), rmog1(i),
     1        rroug1(i), snodp1(i), thd1(i), tilla1(i), tilla2(i)
            call eatcom(12)
c*      **Modified by Kidwell on 6/6/95
            read (12,*) resi1(i), roki1(i), basi1(i), cryi1(i),
     1        resr1(i), rokr1(i), basr1(i), cryr1(i), canco1(i)
c*      **Added by Kidwell on 5/25/95
            call eatcom(12)
c            read (12,*) rescof1(i), cancof1(i)
c
c           anything other than 0 for rescof1 and cancof1
c           causes WEPP to BOMB under Rangeland 06-20-95 02:33pm  sjl
c
            rescof1(i)=0.0
            cancof1(i)=0.0
c
c*      **Added by Kidwell on 6/6/95
c           Calculate totals of all cover components
c
            litcv1(i) = resi1(i) + resr1(i)
            rokcv1(i) = roki1(i) + rokr1(i)
            bascv1(i) = basi1(i) + basr1(i)
            crycv1(i) = cryi1(i) + cryr1(i)
c
c*      **Added by Kidwell on 5/25/95
c           Limit input value of litter cover to between 0 and 0.9999
c
            if(litcv1(i) .lt. 0.0) litcv1(i)=0.0
            if(litcv1(i) .gt. 0.9999) litcv1(i)=0.9999
            if(rokcv1(i) .lt. 0.0) rokcv1(i)=0.0
            if(rokcv1(i) .gt. 0.9999) rokcv1(i)=0.9999
            if(bascv1(i) .lt. 0.0) bascv1(i)=0.0
            if(bascv1(i) .gt. 0.9999) bascv1(i)=0.9999
            if(crycv1(i) .lt. 0.0) crycv1(i)=0.0
            if(crycv1(i) .gt. 0.9999) crycv1(i)=0.9999
            if(canco1(i) .lt. 0.0) canco1(i)=0.0
            if(canco1(i) .gt. 0.9999) canco1(i)=0.9999
c
c*      **Commented out by Kidwell on 5/25/95
c           Compute the initial residue mass on the soil surface.
c
c*            rmog1(i) = (log(1.-litcv1(i))/(-13.5))
c
c
c*      **Modified/Added by Kidwell on 6/6/95
c
c           Calculate values for the fraction of litter, rocks,
c           basal vegetation and cryptogams for rill and interrill
c           areas.
c
            if (litcv1(i) .eq. 0.0) then
                fresr1(i) = 0.0
            else
                fresr1(i) = resr1(i)/litcv1(i)
            end if
            if (rokcv1(i) .eq. 0.0) then
                frokr1(i) = 0.0
            else
                frokr1(i) = rokr1(i)/rokcv1(i)
            end if
            if (bascv1(i) .eq. 0.0) then
                fbasr1(i) = 0.0
            else
                fbasr1(i) = basr1(i)/bascv1(i)
            end if
            if (crycv1(i) .eq. 0.0) then
                fcryr1(i) = 0.0
            else
                fcryr1(i) = cryr1(1)/crycv1(i)
            end if
c
            fresi1(i) = 1.0 - fresr1(i)
            froki1(i) = 1.0 - frokr1(i)
            fbasi1(i) = 1.0 - fbasr1(i)
            fcryi1(i) = 1.0 - fcryr1(i)
c
          end if
c
        else if (lanus1(i).eq.3) then
c
c         FOREST INITIAL CONDITION
c
          write (6,5100) 'FOREST', 'INITIAL CONDITIONS'
          stop
c
        else
c
c         ROAD INITIAL CONDITION
c
          write (6,5100) 'ROAD', 'INITIAL CONDITIONS'
          stop
c
        end if
   50 continue
c
c*******************************
c
c     SURFACE EFFECTS SECTION
c
c*******************************
c
c     nsurf = NUMBER OF SURFACE EFFECT SCENARIOS
c
      call readin(12,nsurf,0,ntype,'nsurf       ')
c
c     LOOP NSURF TIMES
c
      do 70, i = 1, nsurf
c
c       read the scenario name, comments, and landuse.
c
        call scenhd(12,scenam,lantyp(i))
c
c
c       GET NUMBER OF OPERATIONS FOR THIS SURFACE EFFECT SCENARIO
c
        call readin(12,ntill(i),1,ntype,'ntill       ')
c       print*,ntill(i)
c
c       LOOP NTILL TIMES
c
        do 60, j = 1, ntill(i)
c
          if (lantyp(i).eq.1) then
c
c           CROPLAND SURFACE EFFECT
c
            call getdat(12,mdate(j,i),1,'mdate       ')
c
c           print*, mdate(j,i)
c
            call readin(12,op(j,i),1,nop,'op          ')
c
c           print*,op(j,i), nop
c
            call eatcom(12)
            read (12,*) tildep(j,i)
c
            call readin(12,typtil(j,i),1,2,'typtil      ')
c
c         print*,typtil(j,i)
c
          else if (lantyp(i).eq.2) then
c
c           RANGELAND SURFACE EFFECT
c
            write (6,5100) 'RANGELAND', 'SURFACE EFFECTS'
            stop
c
          else if (lantyp(i).eq.3) then
c
c           FOREST SURFACE EFFECT
c
            write (6,5100) 'FOREST', 'SURFACE EFFECTS'
            stop
c
          else
c
c           ROAD SURFACE EFFECT
c
            write (6,5100) 'ROAD', 'SURFACE EFFECTS'
            stop
c
          end if
c
   60   continue
   70 continue
c
c*******************************
c
c     CONTOUR SCENARIO SECTION
c
c*******************************
c
c     ncnt = NUMBER OF CONTOUR SCENARIOS
c
      call readin(12,ncnt,0,ntype,'ncnt        ')
c
c     LOOP NCNT TIMES
c
      do 80, i = 1, ncnt
c
c       read the scenario name, comments, and landuse.
c
        call scenhd(12,scenam,icont(i))
        if (icont(i).eq.1) then
c
c         CROPLAND CONTOUR
c
          call eatcom(12)
          if(datver.gt.98.4)
     1    read (12,*) cntslp(i), rdghgt(i), rowlen(i), rowspc(i),
     1                cntday(i), cntend(i)
c
          if(datver.le.98.4)
     1    read (12,*) cntslp(i), rdghgt(i), rowlen(i), rowspc(i)
c
c
c         correction by dcf to prevent model bombing - do not
c         allow cntslp=0    5/4/94
c
c         Change from Baffaut - 1996  -  dcf  3/14/97
c         if (cntslp(i).le.0.0) cntslp(i) = 0.00001
          if (cntslp(i).le.0.0) cntslp(i) = 0.000001
c
        else if (icont(i).eq.2) then
c
c         RANGELAND CONTOUR
c
          write (6,5100) 'RANGELAND', 'CONTOUR'
          stop
c
        else if (icont(i).eq.3) then
c
c         FOREST CONTOUR
c
          write (6,5100) 'FOREST', 'CONTOUR'
          stop
c
        else
c
c         ROAD CONTOUR
c
          write (6,5100) 'ROAD', 'CONTOUR'
          stop
c
        end if
   80 continue
c
c*******************************
c
c     DRAINAGE SCENARIO SECTION
c
c*******************************
c
c     ndrain = NUMBER OF DRAINAGE SCENARIOS
c
      call readin(12,ndrain,0,ntype,'ndrain      ')
c
c     LOOP NDRAIN TIMES
c
      do 90, i = 1, ndrain
c
c       read the scenario name, comments, and landuse.
c
        call scenhd(12,scenam,idrai1(i))
        if (idrai1(i).eq.1) then
c
c         CROPLAND DRAINAGE
c
          call eatcom(12)
          read (12,*) ddrain(i), drainc(i), drdiam(i), sdrain(i)
c
        else if (idrai1(i).eq.2) then
c
c         RANGELAND DRAINAGE
c
          write (6,5100) 'RANGELAND', 'DRAINAGE'
          stop
c
        else if (idrai1(i).eq.3) then
c
c         FOREST DRAINAGE
c
          write (6,5100) 'FOREST', 'DRAINAGE'
          stop
c
        else
c
c         ROAD DRAINAGE
c
          write (6,5100) 'ROAD', 'DRAINAGE'
          stop
c
        end if
   90 continue
c
c*******************************
c
c     YEARLY SCENARIO SECTION
c
c*******************************
c
c     nmscen = NUMBER OF YEARLY SCENARIOS
c
      call readin(12,nmscen,1,ntype,'nmscen      ')
c
c     LOOP NMSCEN TIMES
c
      do 130, i = 1, nmscen
c
c       read the scenario name, comments, and landuse.
c
        call scenhd(12,scenam,iscen(i))
        if (iscen(i).eq.1) then
c
c         CROPLAND YEARLY
c
c         read plant growth scenario pointer
          call readin(12,ityp1(i),1,ncrop,'ntype       ')
c
c         read operations sequence scenario pointer
          call readin(12,tilse1(i),0,nsurf,'tilseq      ')
c
c         read contours scenario pointer
          call readin(12,conse1(i),0,ncnt,'conseq      ')
c
c         read drainage scenario pointer
          call readin(12,drseq1(i),0,ndrain,'drseq       ')
c
c         read in management option flag
          call readin(12,imngm1(i),1,3,'imngmt      ')
c
c         if option is cropland annual or cropland fallow then
c         proceed
          if (imngm1(i).eq.1.or.imngm1(i).eq.3) then
c
c           ANNUAL/FALLOW CROPPING SYSTEM
c
            call getdat(12,jdhar1(i),0,'jdharv      ')
            call getdat(12,jdpl1(i),0,'jdplt       ')
c
            call eatcom(12)
            read (12,*) r1(i)
            call readin(12,resmg1(i),1,6,'resmgt      ')
c
            if (resmg1(i).eq.1) then
c
c             HERBICIDE
c
              call getdat(12,jdher1(i),1,'jdherb      ')
c
c           print*, jdher1(i)
c
            else if (resmg1(i).eq.2) then
c
c             BURNING
c
              call getdat(12,jdbur1(i),1,'jdburn      ')
c
              call eatcom(12)
              read (12,*) fbrna1(i), fbrno1(i)
c
            else if (resmg1(i).eq.3) then
c
c             SILAGE HARVEST
c
              call getdat(12,jdslg1(i),1,'jdslge      ')
c
            else if (resmg1(i).eq.4) then
c
c             CUTTING
c
              call getdat(12,jdcu1(i),1,'jdcut       ')
c
              call eatcom(12)
              read (12,*) frcu1(i)
            else if (manver.lt.98.3.and.resmg1(i).eq.5)then
c
c             RESIDUE REMOVAL
c
              call getdat(12,jdmov1(i),1,'jdmove      ')
c
              call eatcom(12)
              read (12,*) frmov1(i)
            else
c              write(6,6100)
c
            end if
c
          else
c
c           PERRENIAL CROPLAND
c
            call getdat(12,jdhar1(i),0,'jdharv      ')
            call getdat(12,jdpl1(i),0,'jdplt       ')
            call getdat(12,jdsto1(i),0,'jdstop      ')
c
            call eatcom(12)
            read (12,*) r1(i)
c
            call readin(12,mgtop1(i),1,3,'mgtopt      ')
c
            if (mgtop1(i).eq.1) then
c
c             CUTTING
c
              call readin(12,ncu1(i),1,ntype2,'ncut        ')
c
c             LOOP NCUT TIMES
c
              do 100, j = 1, ncu1(i)
                call getdat(12,cutda1(i,j),1,'cutday      ')
  100         continue
c
            else if (mgtop1(i).eq.2) then
c
c             GRAZING
c
              call readin(12,ncycl1(i),1,ntype2,'ncycle      ')
c
c             LOOP NCYCLE TIMES
c
              do 110, j = 1, ncycl1(i)
c
                call eatcom(12)
                read (12,*) anima1(i,j), are1(i,j), bodyw1(i,j),
     1              diges1(i,j)
c
                call getdat(12,gda1(i,j),1,'gday        ')
                call getdat(12,gen1(i,j),1,'gend        ')
c
  110         continue
c
            end if
c
          end if
c
        else if (iscen(i).eq.2) then
c
c         RANGELAND YEARLY
c
          call readin(12,ityp1(i),1,ncrop,'itype       ')
          call readin(12,tilse1(i),0,nsurf,'tilseq      ')
          call readin(12,drseq1(i),0,ndrain,'drseq       ')
          call readin(12,grazi1(i),0,1,'grazig      ')
c
          if (grazi1(i).eq.1) then
c
c           GRAZING
c
            call eatcom(12)
            read (12,*) are2(i), acces1(i), digma1(i), digmi1(i),
     1          suppm1(i)
c
            call readin(12,jgra1(i),1,ntype2,'jgraz       ')
c
c
c           LOOP JGRAZ1 TIMES
c
            do 120, j = 1, jgra1(i)
c
              call eatcom(12)
              read (12,*) anima1(i,j), bodyw1(i,j)
c
              call getdat(12,gda1(i,j),1,'gday        ')
              call getdat(12,gen1(i,j),1,'gend        ')
              call getdat(12,sen1(i,j),0,'send        ')
              call getdat(12,ssda1(i,j),0,'ssday       ')
c
  120       continue
c
          end if
c
          call getdat(12,ihdat1(i),0,'ihdate      ')
c
          if (ihdat1(i).gt.0) then
c
c           HERBICIDE
c
            call readin(12,activ1(i),0,1,'active      ')
c
            call eatcom(12)
            read (12,*) dlea1(i), her1(i), regro1(i), updat1(i)
c
            call readin(12,wood1(i),0,1,'woody       ')
c
          end if
c
          call getdat(12,jfdat1(i),0,'jfdate      ')
c
          if (jfdat1(i).gt.0) then
c
c           BURNING
c
            call eatcom(12)
            read (12,*) alte1(i), burne1(i), chang1(i), hur1(i),
     1          reduc1(i)
c
          end if
c
        else if (iscen(i).eq.3) then
c
c         FOREST YEARLY
c
          write (6,5100) 'FOREST', 'MANAGEMENT'
          stop
c
        else
c
c         ROAD YEARLY
c
          write (6,5100) 'ROAD', 'MANAGEMENT'
          stop
c
        end if
  130 continue
c
c*******************************
c
c     MASTER SCENARIO SECTION
c
c*******************************
c
c     MASTER SCENARIO NAME (8 CHAR)
c
      call eatcom(12)
      read (12,2000) scenam
c
c     COMMENTS TO BE PRINTED ON OUTPUT (60 CHAR)
c     AND INITIAL CONDITION SCENARIO
c
      do 140, i = 1, 3
        read (12,2200) mancom(i)
  140 continue
c
c
c     number of ofes on hillslope
c
      call readin(12,nwsofe,1,ntype,'nwsofe      ')
      jstruc = nwsofe
c
c*******************************
c
c     OFE SCENARIO INDEX AND CONVERSION TO WEPP VALUES
c
c*******************************
c
      do 150, iplane = 1, nwsofe
c
        call readin(12,inindx,1,nini,'inindx      ')
c
        if (lanus1(inindx).eq.1) then
          nowres = 1
          lanuse(iplane) = lanus1(inindx)
          cancov(iplane) = canco1(inindx)
          inrcov(iplane) = inrco1(inindx)
          rilcov(iplane) = rilco1(inindx)
          rtm(nowres,iplane) = rtm1(inindx)
          smrm(nowres,iplane) = smrm1(inindx)
          width(iplane) = widt1(inindx)
          wdhtop(iplane) = widt1(inindx)
          rspace(iplane) = rspac1(inindx)
          iresd(nowres,iplane) = ires1(inindx)
          imngmt(nowres+3,iplane) = imngm2(inindx)
          rrinit(iplane) = rrini1(inindx)
          rhinit(iplane) = rhini1(inindx)
          bdtill(iplane) = bdtil1(inindx)
          rfcum(iplane) = rfcu1(inindx)
          daydis(iplane) = daydi1(inindx)
          dsharv(iplane) = dshar1(inindx)
          snodpy(iplane) = snodp1(inindx)
          frdp(iplane) = frd1(inindx)
          thdp(iplane) = thd1(inindx)
          tillay(1,iplane) = tilla1(inindx)
          tillay(2,iplane) = tilla2(inindx)
c
c         ------------ prevent secondary tillage depth from being
c         greater than primary.
c
          if (tillay(1,iplane).gt.tillay(2,iplane)) then
            tiltmp = tillay(2,iplane)
            tillay(2,iplane) = tillay(1,iplane)
            tillay(1,iplane) = tiltmp
          end if
c
c         ------------ if entered as zero, use default values.
c
          if (tillay(2,iplane).le.0.0) tillay(2,iplane) = 0.2
          if (tillay(1,iplane).le.0.0) tillay(1,iplane) = 0.1
c
c         Check to prevent negative values for input rill width.
c         For negative or zero values for rill spacing - set to
c         default spacing of 1 rill per meter.
c
          if (width(iplane).lt.0.0) width(iplane) = 0.0
          if (wdhtop(iplane).lt.0.0) wdhtop(iplane) = 0.0
          if (rspace(iplane).le.0.0) rspace(iplane) = 1.
c
c         Use input flag for type of rills (temporary or permanent)
c         to set the rill width flag used in subroutines SOIL and SHEARS
c         For temporary situations and continuous simulations - set
c         width to a default 0.15 meters if user has entered a 0 value.
c         For temporary situations and single storm simulations use
c         whatever value user has provided for rill width.
c
          if (rtyp1(inindx).eq.1) then
            rwflag(iplane) = 1
            if (imodel.eq.1.and.width(iplane).le.0.0)then
              width(iplane) = 0.15
              wdhtop(iplane) = 0.15
          endif
c
c         ELSE for PERMANENT type rills - set rwflag to 2 and use the
c         input value for rill width always.  IF the user has input
c         a value of zero rill width - assume that he/she actually
c         wants flow across the entire area and set rill width equal
c         to rill spacing. (since the program will bomb if a constant
c         value of rill width = 0 meters is used).
c
          else
            rwflag(iplane) = 2
            if (width(iplane).le.0.0) then
              width(iplane) = rspace(iplane)
              wdhtop(iplane) = rspace(iplane)
            end if
          end if
c
c         Check to make sure that rill width is not greater than
c         rill spacing.  If it is, set width to rill spacing.
c
          if (width(iplane).gt.rspace(iplane)) then
             width(iplane) = rspace(iplane)
             wdhtop(iplane) = rspace(iplane)
          endif
c
c
c         CONVERT BDTILL FROM GM/CM3 TO KG/M3
c
          bdtill(iplane) = bdtill(iplane) * 1000.0
c
c
c         CONVERT RFCUM FROM MM TO M
c
          rfcum(iplane) = rfcum(iplane) / 1000.0
c
c         Calculate initial kecum based on approximate average of 15 J/m2 per
c         mm of rainfall. Risse 11/4/93
c
          rkecum(iplane) = rfcum(iplane) * 1000.0 * 15.0
c
c
          if (cancov(iplane).ge..999) cancov(iplane) = 0.999
          if (inrcov(iplane).ge..999) inrcov(iplane) = 0.999
          if (rilcov(iplane).ge..999) rilcov(iplane) = 0.999
c
c         Changed minimum value on RRINIT and RHINIT both to 0.006 meters
c         based on conversation with John Gilley 7/16/93 and based on
c         minimum roughness for a smooth surface reported by
c         (Zobeck and Onstad, 1987)   dcf  7/16/93
c         if (rrinit(iplane).lt.0.01) rrinit(iplane) = 0.01
c         if (rhinit(iplane).lt.0.02) rhinit(iplane) = 0.02
c
          if (rrinit(iplane).lt.0.006) rrinit(iplane) = 0.006
          if (rhinit(iplane).lt.0.006) rhinit(iplane) = 0.006
c
c       ADDED BY DCF - 7/6/90 DUE TO ERRORS CAUSED WHEN RILL COVER
c       OF 0.0 IS ENTERED.
c
c       THIS ERROR CORRECTED
c       IF (RILCOV(IPLANE) .LE. 0.001) RILCOV(IPLANE) = 0.001
c       IF (INRCOV(IPLANE) .LE. 0.001) INRCOV(IPLANE) = 0.001
c
        else if (lanus1(inindx).eq.2) then
          nowres = 1
          lanuse(iplane) = lanus1(inindx)
          snodpy(iplane) = snodp1(inindx)
          frdp(iplane) = frd1(inindx)
          thdp(iplane) = thd1(inindx)
          rmogt(nowres,iplane) = rmog1(inindx)
          rmagt(iplane) = rmag1(inindx)
c         wcf(iplane) = wc1(inindx)
c         crypto(iplane) = crypt1(inindx)
          pptg(iplane) = ppt1(inindx)
          rrough(iplane) = rroug1(inindx)
          tillay(1,iplane) = tilla1(inindx)
          tillay(2,iplane) = tilla2(inindx)
          rescov(iplane) = litcv1(inindx)
          bascov(iplane) = bascv1(inindx)
          rokcov(iplane) = rokcv1(inindx)
          crycov(iplane) = crycv1(inindx)
          fresr(iplane) = fresr1(inindx)
          fresi(iplane) = fresi1(inindx)
          frokr(iplane) = frokr1(inindx)
          froki(iplane) = froki1(inindx)
          fbasr(iplane) = fbasr1(inindx)
          fbasi(iplane) = fbasi1(inindx)
          fcryr(iplane) = fcryr1(inindx)
          fcryi(iplane) = fcryi1(inindx)
          cancov(iplane) = canco1(inindx)
          rkecum(iplane) = rfcum(iplane) * 1000.0 * 15.0
c*      **Added by Kidwell on 5/25/95
          rescof(iplane) = rescof1(inindx)
          cancof(iplane) = cancof1(inindx)
c*      **Added by Kidwell on 6/6/95
          resr(iplane) = resr1(inindx)
          resi(iplane) = resi1(inindx)
          rokr(iplane) = rokr1(inindx)
          roki(iplane) = roki1(inindx)
          basr(iplane) = basr1(inindx)
          basi(iplane) = basi1(inindx)
          cryr(iplane) = cryr1(inindx)
          cryi(iplane) = cryi1(inindx)
c
        else if (lanus1(inindx).eq.3) then
          write (6,5100) 'FOREST', 'INITIAL CONDITIONS'
        else
          write (6,5100) 'ROAD', 'INITIAL CONDITIONS'
        end if
c
c
c     Commented out following line 6/8/94  - Now set densg to 100 in
c     INIDAT.   dcf
c     if (snodpy(iplane).gt.0) densg(iplane) = 0.225
c
  150 continue
c
c     NUMBER OF ROTATION REPETITIONS
c
      call readin(12,nrots,1,10000,'nrots       ')
c
c
c     NUMBER OF YEARS IN EACH ROTATION
c
      call readin(12,nyears,1,10000,'nyears      ')
c
c*******************************
c
c     PENMAN_MONTEITH ET COEFFICIENTS
c
c*******************************
c
c     Check to see if special ET coefficients
c     input file exists - if it does then assume
c     that user wants to use the Penman-Montieth
c     equation and set IFLGET=2.  Otherwise, set
c     IFLGET=1 and use the original Penman equation.
c     dcf  -  5/25/2004

      open(unit=22,file='pmetpara.txt',status='old',err=157)
      iflget=2
cd    We adpted Claudio Stockle's recommendation. We use an advanced 
cd    method instead of the method in FAO 50 documentation to deal 
cd    with the ET deffience on crop growth stage. please 
cd    check the related documents for detail.
      do 155, i = 1, ncrop    
        call pmetcoef(crpnam(i),i)
 155  continue
      close(22)
      go to 159
 157  iflget=1
 159  continue
 
c
c*********************************************
c winter processes control
c   8/1/2008 - jrf
c
c*********************************************
c
c Check if a file with settings for controlling the execution
c speed of the winter freeze/thaw processes exists.
c This file has one line with 3 integer variables:
c   redist - 1 = run water residtribution as part of frost simulation
c            0 = bypass water redistribution as part of frost
c            default is 1, to run redistribution.
c   fine layers top - number of fine soil layers used in winter freeze/thaw
c            in each of 2 top 10cm sections of soil profile. Valid values are
c            1-10, default is 10.
c   fine layers bottom - number of fine soil layers used in winter freeze/thaw
c            in each of soil layers below top layers. Valid values are
c            1-10, default is 10.
      open(unit=22,file='frost.txt',status='old',err=301)
      read (22,*) wintRed,fineTop,fineBot
      read (22,*) ksnowf, kresf, kfactor
      if ((fineTop.gt.10).or.(fineTop.lt.1)) fineTop=10
      if ((fineBot.gt.10).or.(fineBot.lt.1)) fineBot=10
      if ((wintRed.gt.1).or.(wintRed.lt.0)) wintRed=1
      if ((ksnowf.lt.0.1).or.(ksnowf.gt.10.0)) ksnowf = 1.0
      if ((kresf.lt.0.1) .or. (kresf.gt.10.0)) kresf = 1.0 
      if ((kfactor.le.0.0).or.(kfactor.gt.1.0)) kfactor = 1e-5
      close(22)
      go to 302
 301  fineTop = 10
      fineBot = 10
      wintRed = 1
      kresf = 1.0
      ksnowf = 1.0
      kfactor = 1e-5
 302  continue
 
c 
c*******************************
c
c     SLOPE FILE SECTION
c     (UNIT=10, STATUS=2)
c
c*******************************
c
      istrng = 'Enter name of file containing slope data -->'
  160 call open(10,2,65,istrng,slpfil)
c
      call eatcom(10)
      read (10,*) datver
c
      if (datver.gt.10.0) then
        backspace (10)
c
c       version control check - will exit with message if not correct.
c
        mesg = 'SLOPE'
        call verchk(10,datver,slpchk,mesg,ver)
      else
c
c       assuming that the 1st line is nwsofe
c       reset pointer to beginning of line and assume pre 93.005
c
        backspace (10)
c
      end if
c
c     version control check - will exit with message if not correct.
c
      call readin(10,nwsofe,1,ntype,'nwsofe      ')
c
      if (nwsofe.ne.jstruc) then
c
        if (nwsofe.eq.1) then
          write (6,2500)
          write (6,2600)
        else
          write (6,2500)
          write (6,2700) nwsofe
        end if
c
        if (ibomb.eq.1) stop
        close (unit=10)
c
        go to 160
      end if
c
      write (iout,1700) 'MANAGEMENT:', manfil
      write (iout,1800) 'MAN. PRACTICE:', (mancom(i),i = 1,3)
      write (iout,1700) 'SLOPE:', slpfil
c
c*******************************
c
c     CLIMATE FILE SECTION
c     (UNIT=13, STATUS=2)
c
c*******************************
c
      istrng = 'Enter name of file containing storm data -->'
c
  170 call open(13,2,65,istrng,clifil)
c
c     Set initial value for station elevation (needed in EVAP
c     and IMPEO) to 1000.0 meters as a default).  If climate
c     file contains data - will use that value instead.
c
      elev = 1000.0
c
      read (13,*) datver
c      
      vercli = int(datver)
c           
c     condition L1
      if (datver.le.2.0.and.datver.gt.0.0) then
        write (6,1000)
c
c       assuming that the 1st line is itemp and ibrkpt
c       reset pointer to beginning of line and assume CLIGEN 2.3
c
        backspace (13)
c
        read (13,*) itemp, ibrkpt
        iwind = 0
c
        if (itemp.ne.imodel) then
c
          if (itemp.eq.1) then
            write (6,2800)
          else
            write (6,2900)
          end if
c
          close (unit=13)
          if (ibomb.eq.1) stop
          go to 170
        end if
c
c
c       CHECK IF OLD CLIMATE FILE IS REALLY FROM VER 2.3
c
        read (13,2300) stmid, datver
c
        if (datver.eq.0.0) then
c
c         if user sets CLIGEN version number to 0.0, then use the
c         actual values in the CLIGEN file for Ip and duration
          iclig = 0
c
        else if (datver.ge.4.0) then
c
c         if the user is running the new CLIGEN version (4.0+)
c         which has the corrected durations, then use the value
c         for storm duration with no correction.  Make an adjustment
c         to the Ip to account for the steady-state erosion assump.
c
          iclig = 1
c
        else
c
c         if the user is running a CLIGEN version between 2.3 and
c         3.1, make the old adjustments to duration and Ip, and
c         warn the user that the CLIGEN file being used is out of
c         date.
c
          iclig = 2
c
        end if
c
        if (datver.gt.0.0.and.datver.lt.2.3) then
          write (6,1100)
          stop
        end if
c
        if (iclig.eq.2) write (6,1200)
        read (13,*)
c
c       LATITUDE
c
        read (13,*) deglat
c
c       BEGINNING YEAR (IBYEAR) AND NUMBER OF YEARS (NUMYR)
c
        read (13,2400) ibyear, numyr
c
c     condition L1
      else if (datver.ge.3.0.or.datver.eq.0.0) then
c
c       CLIMATE FILE FROM VER 3.0 (format) OR GREATER  a value of DATVER
c       equal to 0.0 indicates that observed precipitation duration
c       data has been input and should be used - variable ICLIG
c       needs to be passed to subroutine STMGET    dcf  11/15/93
c
        if (datver.eq.0.0.or.datver.ge.4.0) then
          iclig = 0
        else
          iclig = 1
        end if
c
        read (13,*) itemp, ibrkpt, iwind
c
c       print*,'iwind= ',iwind
c
        if (itemp.ne.imodel) then
c
          if (itemp.eq.1) then
            write (6,2800)
          else
            write (6,2900)
          end if
c
          close (unit=13)
          if (ibomb.eq.1) stop
          go to 170
        end if
c
        read (13,2300) stmid
        read (13,*)
c
c       LATITUDE LONGITUDE ELEVATION, OBSERVED YEARS, BEGINNING, NUMBER
c
        read (13,*) deglat, deglon, elev, obsyrs, ibyear, numyr
c
c       limit elevation to reasonable values (-200 < elev < 10000)
        if(elev.lt.-200) elev = -200.0
        if(elev.gt.10000) elev = 10000.0
c
c     condition L1
      else
        write (6,2500)
        write (6,5200)
        if (ibomb.eq.1) stop
        go to 170
      end if
c     condition L1      
c
      read (13,*)
      read (13,*) (obmaxt(i),i = 1,12)
      read (13,*)
      read (13,*) (obmint(i),i = 1,12)
      read (13,*)
      read (13,*) (radave(i),i = 1,12)
      read (13,*)
      read (13,*) (obrain(i),i = 1,12)
c
c     SKIP TWO LINES IN THE CLIMATE INPUT FILE (HEADINGS)
c     CHANGED FROM SLASHES AT END OF 701 WHICH WILL NOT WORK WITH
c     LPI FORTRAN (UNIX COMPILER)
c
      read (13,*)
      read (13,*)
c
c     hillslope/watershed version stores climate file name
c     for consistency checking between hillslopes
c
      if ((iwpass.eq.1).or.(ivers.eq.2)) wshcli(ihill) = clifil
c
      write (iout,1700) 'CLIMATE:', clifil
      write (iout,1900) stmid, datver
c
c*******************************
c
c     SOIL FILE SECTION
c     (UNIT=11, STATUS=2)
c
c*******************************
c
      istrng = 'Enter name of file containing soil data -->'
  180 call open(11,2,65,istrng,solfil)
c
      call eatcom(11)
      read (11,*) datver
c
      if (datver.gt.90.0) then
c
        backspace (11)
c       version control check - will exit with message if not correct.
c
        mesg = 'SOIL'
c
        call verchk(11,datver,solchk,mesg,ver)
c
        call eatcom(11)
        if (datver.ge.95.3) read (11,6000) solcom
c
        call readin(11,itemp,1,ntype,'itemp       ')
c
c       reset the pointer to the beginning of the line and now check
c       to see what preference the user has for the internal Ksat
c       adjustments.  If  ksflag=0  use no adjustments
c       ksflag=1  use all internal Ks adjustments
c
        backspace (11)
c
c       use the version control value to determine whether or
c       not an ksflag value should be expected in the SOIL file
c
        if (datver.gt.93.621) then
          read (11,*) ijunk, ksflag
          if (ksflag.lt.0.or.ksflag.gt.1) ksflag = 1
c
c         Commented out the following line.  KSFLAG should not be
c         set to 0 regardless of the user's wishes.  If anything,
c         it should be set to 1, so that values for conduct. are
c         recalculated on a daily basis.  dcf  11/29/95
c         if (lanuse(1).eq.2) ksflag = 0
c
        else
          read (11,*) ijunk
          ksflag = 1
c
c         see above  -  dcf  11/29/95
c         if (lanuse(1).eq.2) ksflag = 0
c
        end if
c
      else
c
c       assuming that the 1st line is itemp
c       reset pointer to beginning of line and assume pre 93.005
c
        backspace (11)
        call readin(11,itemp,1,ntype,'itemp       ')
c
c       If the data file is old format with no version control,
c       assume that it will not have the flag to use or not
c       use the internal Ksat adjustments.  Set ksflag to 1,
c       which means USE all internal Ksat adjustments.  dcf  1/11/94
c
        ksflag = 1
c
      end if
c
c     Calculate the soil input file WEPP version variable to be
c     used in subroutine INPUT for determining how to read the
c     soil file
c
c     Only multiply version values that are less than 100 by ten.
c     This prevents a problem (bug?) with Salford compiler for a
c     real number having a value of 77770, which is too large for
c     it to convert to an integer.  (Super user)
c
      if (datver.lt.100) datver = datver * 10.0
      solwpv = nint(datver)
c
c
      if (itemp.ne.jstruc) then
c
        if (itemp.eq.1) then
          write (6,3000)
        else
          write (6,3100) itemp
        end if
c
        close (unit=11)
        if (ibomb.eq.1) stop
        go to 180
      end if
c
      write (iout,1700) 'SOIL:', solfil
c
c     create header for initial condition scenario header
c
      if (ifile.eq.2) then
        write (47,*) ver
        write (47,5300) ver, manfil, clifil, solfil, slpfil
      end if
c
c*******************************
c
c     IRRIGATION FILE SECTION
c
c     (DEPLETION  -> UNIT=15, STATUS=2)
c     (FIXED-DATE -> UNIT=14, STATUS=2)
c
c*******************************
c
c
c     IRRIGATION OUTPUT OPTIONS
c
      irrig = 0
c
      write (6,4100)
      read (5,5700,err=190) irrig
c
  190 if ((irrig.lt.0).or.(irrig.gt.6)) then
        write (6,5600)
        irrig = 0
      end if
c
      if (irrig.eq.0) then
c
c       NO IRRIGATION
c
        irsyst = 0
        write (6,4200)
c
      else if (irrig.eq.1) then
c
c       SPRINKLER FIXED DATE
c
        irsyst = 1
        irschd(1) = 2
      else if (irrig.eq.2) then
c
c       SPRINKLER DEPLETION
c
        irsyst = 1
        irschd(1) = 1
      else if (irrig.eq.3) then
c
c       SPRINKLER COMBINATION
c
        irsyst = 1
        irschd(1) = 3
      else if (irrig.eq.4) then
c
c       FURROW FIXED DATE
c
        irsyst = 2
        irschd(1) = 2
      else if (irrig.eq.5) then
c
c       FURROW DEPLETION
c
        irsyst = 2
        irschd(1) = 1
      else if (irrig.eq.6) then
c
c       FURROW COMBINATION
c
        irsyst = 2
        irschd(1) = 3
      end if
c
      if (irsyst.ne.0) irabrv = irsyst
c
      if (irsyst.ne.0) then
c
c       sprinkler irrigation
c
        if (irsyst.eq.1) then
          write (iout,4500)
          write (6,4300)
        else
          write (iout,4600)
          write (6,4400)
        end if
c
c       sprinkler depletion
c
        if (irschd(1).ne.2) then
          istrng = 'Enter name of file containing depletion level irriga
     1tion data-->'
  200     call open(15,2,65,istrng,filen)
          write (iout,4700) filen
c
          if (ifile.eq.2) then
            write (47,4800) filen
          end if
c
c         version control check - will exit with message if not correct.
c
          read (15,*) datver
          if (datver.gt.2.0) then
c
c           version control check - will exit with message if not correct.
c
            backspace (15)
c
c           mesg = 'DEPLETION IRRIGATION'
c           if(irschd(1).eq.1)irdchk=irdsch
c           if(irschd(1).eq.2)irdchk=irdfch
c           call verchk(15,datver,irdchk,mesg,ver)
c
            read (15,*) datver
c
            if (irsyst.eq.1) then
              idsver = datver
            else
              idfver = datver
            end if
c
            if (irsyst.eq.1) then
c
              if (idsver.lt.94.21) then
                write (6,5400) irdsch
                write (iout,5400) irdsch
              else
              end if
c
            else
c
            end if
c
          else
c
c           assuming that the 1st line is itemp
c           reset pointer to beginning of line and assume pre 93.005
c
            backspace (15)
c
          end if
c
          read (15,*) itemp, jtemp, ktemp
c
          if (jtemp.ne.irsyst) then
            write (6,2500)
            write (6,3300)
            close (unit=15)
            if (ibomb.eq.1) stop
            go to 200
          else if (ktemp.ne.1) then
            write (6,2500)
            write (6,3400)
            close (unit=15)
            if (ibomb.eq.1) stop
            go to 200
          else if (itemp.ne.jstruc) then
            write (6,2500)
            write (6,3200)
            close (unit=15)
            if (ibomb.eq.1) stop
            go to 200
          end if
        end if
c
        if (irschd(1).ne.1) then
          istrng = 'Enter name of file containing fixed date irrigation
     1data -->'
  210     call open(14,2,61,istrng,filen)
          write (iout,4900) filen
c
          if (ifile.eq.2) then
            write (47,5000) filen
          end if
c
          read (14,*) datver
          if (datver.gt.2.0) then
c
c           version control check - will exit with message if not correct.
c
            backspace (14)
c
c           mesg = 'FIXED-DATE IRRIGATION'
c           if(irschd(1).eq.1)irdchk=irfsch
c           if(irschd(1).eq.2)irdchk=irffch
c
            read (14,*) datver
c
            if (datver.lt.94.21) then
c
              if (irsyst.eq.1) then
                write (6,5500) irfsch
                write (iout,5500) irfsch
              end if
c
            else
c
            end if
c
c           call verchk(14,datver,irfchk,mesg,ver)
c
            if (irsyst.eq.1) then
              ifsver = datver
            else
              iffver = datver
            end if
c
          else
c
c           assume that the 1st line is itemp
c           reset pointer to beginning of line and assume pre 93.005
c
            backspace (14)
          end if
c
          read (14,*) itemp, jtemp, ktemp
c
          if (jtemp.ne.irsyst) then
            write (6,2500)
            write (6,3300)
            close (unit=14)
            if (ibomb.eq.1) stop
            go to 210
          else if (ktemp.ne.2) then
            write (6,2500)
            write (6,3400)
            close (unit=14)
            if (ibomb.eq.1) stop
            go to 210
          else if (itemp.ne.jstruc) then
            write (6,2500)
            write (6,3200)
            close (unit=14)
            if (ibomb.eq.1) stop
            go to 210
          end if
        end if
      end if
c
c     Do not arbitrarily set ksflag to 0 for the case of rangeland
c     dcf 11/29/95    After repeated attempts to obtain information
c     from Tucson ARS group (Weltz, Kidwell) on whether or not to
c     keep these changes, changed back 3/5/97 - dcf
      if (lanuse(1).ne.1) ksflag = 0
c
      return
c
 1000 format (' ***NOTE***',/,
     1    ' PRE CLIGEN 3.0 CLIMATE FILE, ASSUMING CLIGEN 2.3',/,
     1    ' ASSUMING WIND DATA , DEW POINT TEMP. AVAILABLE')
 1100 format ('***ERROR*** CLIMATE FILE OLDER THAN CLIGEN 2.3 ',/,
     1    ' USE CLIGEN 2.3 OR GREATER')
 1200 format (' *** WARNING *** ',/,
     1    ' CLIMATE FILE OLDER THAN CLIGEN 4.0 ',/,
     1    ' UPDATE YOUR CLIMATE FILES USING CLIGEN 4.0 OR GREATER',/,
     1    ' *** WARNING ***',/)
 1300 format (//10x,'USDA WATER EROSION PREDICTION PROJECT',/,10x,
     1    37('-'),//,10x,'HILLSLOPE PROFILE AND WATERSHED MODEL',/,20x,
     1    ' VERSION',1x,f9.3,/,10x,a17,1x,i4///,15x,
     1    'TO REPORT PROBLEMS OR TO BE PUT ON THE MAILING',/,15x,
     1    'LIST FOR FUTURE WEPP MODEL RELEASES, PLEASE CONTACT:',//,20x,
     1    'WEPP TECHNICAL SUPPORT',/,20x,
     1    'USDA-AGRICULTURAL RESEARCH SERVICE',/,20x,
     1    'NATIONAL SOIL EROSION RESEARCH LABORATORY',/,20x,
     1    '275 SOUTH RUSSELL STREET',/,20x,
     1    'WEST LAFAYETTE, IN 47907-2077  USA',//,20x,
     1    'PHONE: (765) 494-8673',/,20x,
     1    '  FAX: (765) 494-5948',/,20x,
     1    'email:  wepp@ecn.purdue.edu',/,20x,
     1    '  URL:  http://topsoil.nserl.purdue.edu'
     1    ,//)
 1400 format (5x,'HILLSLOPE INPUT DATA FILES - VERSION ',f9.3,/,5x,a17,1
     1    x,i4//)
 1500 format (5x,'HILLSLOPE/WATERSHED INPUT DATA FILES - VERSION ',f9.3,
     1    /,5x,a17,1x,i4//)
 1600 format (5x,'WATERSHED INPUT DATA FILES - VERSION ',f9.3,/,5x,a17,1
     1    x,i4//)
 1700 format (a15,1x,a50)
 1800 format (a15,1x,a60,2(/16x,a60))
 1900 format (4x,a75,f5.2)
 2000 format (a8)
 2100 format (a16)
 2200 format (a60)
c 2000 format (a51)
 2300 format (a75,f5.2)
 2400 format (20x,i3,30x,i3)
 2500 format (' ***ERROR***')
 2600 format (/' *** MULTIPLE ELEMENTS WERE CHOSEN ***'/
     1    ' *** SLOPE FILE IS FOR A SINGLE ELEMENT ***'/)
 2700 format (/' *** INCORRECT NUMBER OF ELEMENTS WERE CHOSEN ***'/
     1    ' *** SLOPE FILE IS FOR ',i2,' ELEMENTS ***'/)
 2800 format (/' *** SINGLE STORM SIMULATION WAS CHOSEN ***'/
     1    ' *** CLIMATE FILE IS FOR CONTINUOUS SIMULATION'/)
 2900 format (/' *** CONTINUOUS SIMULATION WAS CHOSEN ***'/
     1    ' *** CLIMATE FILE IS FOR SINGLE STORM SIMULATION'/)
 3000 format (/' *** MULTIPLE ELEMENTS WERE CHOSEN ***'/
     1    ' *** SOIL FILE IS FOR A SINGLE ELEMENT ***'/)
 3100 format (//' *** INCORRECT NUMBER OF ELEMENTS WERE CHOSEN ***'/
     1    ' *** SOIL FILE IS FOR ',i2,' ELEMENTS ***'/)
 3200 format (/
     1    ' *** IRRIGATION FILE IS FOR INCORRECT NUMBER OF ELEMENTS',
     1    ' ***'/)
 3300 format (/' *** IRRIGATION FILE IS FOR THE WRONG SYSTEM TYPE ***'/)
 3400 format (/
     1    ' *** IRRIGATION FILE IS FOR THE WRONG SCHEDULING SCHEME',
     1    ' ***'/)
 3500 format (' Crop Type # ',i2,' is ',a50)
 3600 format (/,'  Do you want rangeland plant outputs (y/n)? [N] -->')
 3700 format (/,'  Do you want rangeland animal outputs (y/n)? [N] -->')
 3800 format (a1)
 3900 format (' sdate    rgc   growth     vdmt     tlive    ',
     1    'slive    rmagy    sdead    rmogt')
 4000 format (' date   yield   utiliz    tfood',
     1    '     food   suppmt   totsup    rsupp    rtotsupp  ')
 4100 format (' Please enter the irrigation option',/,
     1    ' ----------------------------------',/,' [0] no irrigation',
     1    /,'  1  sprinkler, fixed date',/,
     1    '  2  sprinkler, depletion level',/,
     1    '  3  sprinkler, combination',/,'  4  furrow,    fixed date',
     1    /,'  5  furrow,    depletion',/,'  6  furrow,    combination'
     1    /,' -----------------------------------',/,
     1    ' Enter irrigation option [0]',/)
 4200 format (/,' NO IRRIGATION SELECTED.',/)
 4300 format (/2x,'SOLID-SET, SIDE-ROLL OR HAND-MOVE IRRIGATION SYSTEM '
     1    /2x,'HAS BEEN INDICATED.')
 4400 format (/2x,'FURROW IRRIGATION SYSTEM HAS BEEN INDICATED.')
 4500 format (' SPRINKLER IRRIGATION OPTION')
 4600 format (' FURROW IRRIGATION OPTION')
 4700 format (' DEPLETION IRRIGATION:  ',a50)
 4800 format ('#DEPLETION IRRIGATION:  ',a50)
 4900 format (' FIXED DATE IRRIGATION: ',a50)
 5000 format ('#FIXED DATE IRRIGATION: ',a50)
c     4600 format (a)
c     4700 format (/' *** THE NUMBER OF CHANNELS IN THE CHANNEL FILE ***'/
c     1    ' *** DIFFERS FROM THE WATERSHED STRUCTURE FILE ***'/)
 5100 format (/' *** ERROR ***',/,a10,a20,' CURRENTLY NOT SUPPORTED ***'
     1    /)
 5200 format (' Invalid climate file, please check versions, must be',
     1    ' version 2.3 or greater')
 5300 format ('#',/,
     1    '###########################################################',
     1    /,'# This scenario was created using WEPP Version ',f7.3,/,
     1    '# Contains Initial Condition and related Scenarios',/,
     1    '# Management: ',a51,/,'# Climate:    ',a51,/,
     1    '# Soil:       ',a51,/,'# Slope:      ',a51,/,
     1    '###########################################################')
c4900 format (//'*** BEGINNING YEARS OF HILLSLOPE CLIMATE FILES ',
c     1    ' ARE NOT THE SAME ***',//,'** SIMULATION STOPPED ***'//)
 5400 format (/,' *** WARNING ***',/,
     1    ' DEPLETION LEVEL SPRINKLER IRRIGATION FILE IS PRE ',f7.3,
     1    ', nozzle factor set to 1.0',/,
     1    ' *** WARNING ***',/)
 5500 format (/,' *** WARNING ***',/,
     1    ' FIXED DATE SPRINKLER IRRIGATION FILE PRE ',f7.3,', nozzle',/
     1    ,' factor set to 1.0',/,' *** WARNING ***',/)
 5600 format (' *** WARNING ***',/,'IRRIGATION OPTION SET TO 0',/,
     1    ' *** WARNING ***',/)
 5700 format (i1)
 5800 format (f6.3)
 5900 format (' *** WARNING ***',/,' Rangeland inputs are pre-95.102',
     1    ' format.',/,' PROGRAM STOPPED ',/,' *** WARNING ***',/)
 6000 format (a80)
c6100 format (' *** WARNING ***',/,'Error in residue management input ',
c    1    /,' *** WARNING ***',/)
      end
