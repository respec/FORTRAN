      subroutine input(ncrop,jstruc,nsurf,iwpass)
c
c     + + + PURPOSE + + +
c
c     Reads in the management and soil data
c     from input data files.
c
c     Called from subroutine CONTIN.
c     Author(s): Livingston, Ferris, Flanagan
c     Reference in User Guide:
c
c     Version: This module not yet recoded.
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxcsg.inc'
      include 'pmxcut.inc'
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxelm.inc'
      include 'pmxres.inc'
      include 'pmxslp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     real sdist(mxplan)
      integer  ncrop, jstruc, nsurf, iwpass
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     ncrop  -
c     sdist  -
c     jstruc -
c     nsurf  -
c     iwpass -
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccdrain.inc'
c
      include 'cchpar.inc'
c     modify: chnx(mxplan,mxcseg),chnslp(mxplan,mxcseg)
c
      include 'cchtrl.inc'
c     modify: ctlslp(mxplan)
      include 'ccntour.inc'
c     modify: cntslp(mxplan),rowspc(mxplan),rowlen(mxplan),
c             rdghgt(mxplan)
c
      include 'ccover.inc'
c     modify: cancov(mxplan),inrcov(mxplan),rilcov(mxplan),
c             ntill(mxtlsq),lanuse(mxplan),daydis(mxplan)
c
      include 'ccrpgro.inc'
      include 'ccrpout.inc'
      include 'ccrpprm.inc'
c
      include 'ccrpvr1.inc'
c     modify: mfo(tiltyp,ntype),pltol(ntype),rmogt(mxres,mxplan),
c             rmagt(mxplan)
c
      include 'ccrpvr2.inc'
c     modify: cn(ntype),aca(ntype),cf(ntype),as(ntype),ar(ntype)
c
      include 'ccrpvr3.inc'
c     modify: gddmax(ntype),bbb(ntype),rdmax(ntype),rsr(ntype),
c             hmax(ntype)
c
      include 'ccrpvr5.inc'
c     modify: pltsp(ntype), diam(ntype)
c
      include 'cdecvar.inc'
c
      include 'cdist.inc'
c     modify: slplen(mxplan),xinput(101,mxplan)
c
      include 'cends4.inc'
      include 'cerrid.inc'
c
      include 'cflags.inc'
c     read: yldflg
c
      include 'cinpsur.inc'
      include 'cinpop.inc'
      include 'cirriga.inc'
      include 'cnew.inc'
      include 'cnew1.inc'
c
      include 'cparame.inc'
c     modify: sat(mxplan)
c
      include 'cparval.inc'
c
      include 'cperen.inc'
c     modify: imngmt(ntype),tmpmin(ntype),tmpmax(ntype),rtmmax(ntype),
c             fact(ntype)
c
      include 'cridge.inc'
c     modify: iridge(mxtlsq)
c
      include 'crinpt1.inc'
c     modify: bugs(ntype),cold(ntype)
c
      include 'crinpt2.inc'
c     modify: getmp(ntype),tempmn(ntype),pscday(ntype),ffp(ntype),
c             cf1(ntype),cf2(ntype),root10(ntype),rootf(ntype)
c
      include 'crinpt3.inc'
c     modify: shgt(ntype),spop(ntype),sdiam(ntype),scoeff(ntype)
c
      include 'crinpt5.inc'
c     modify: thgt(ntype),tpop(ntype),tdiam(ntype),tcoeff(ntype)
c
      include 'crinpt6.inc'
c
      include 'cslinit.inc'
c     modify: rrinit(mxplan),rhinit(mxplan),bdtill(mxplan),
c             rfcum(mxplan)
c
      include 'cslope.inc'
c     modify: nslpts(mxplan),slpinp(mxslp,mxplan)
c
      include 'cslpopt.inc'
c     modify: xdel(100),xslp(100),fwidth(mxplan),itop,ninpts,
c             harea(mxhill),hslop,hleng
c
      include 'csolvar.inc'
c
      include 'cstruc.inc'
c     modify: iplane
c
      include 'cstruct.inc'
c
      include 'ctemp.inc'
c     modify: orgma1(mxnsl,mxplan), bd1(mxnsl,mxplan), rfg1(mxnsl,mxplan),
c             solth1(mxnsl,mxplan), ssc1(mxnsl,mxplan), sand1(mxnsl,mxplan),
c             clay1(mxnsl,mxplan), nslorg(mxplan)
c
      include 'ctillge.inc'
c     modify: rro(tiltyp), rho(tiltyp), tdmean(tiltyp),
c             tildep(10,mxplan), typtil(10,mxplan), nrplt, nrdril, nrcul,
c             cltpos
c
      include 'cupdate.inc'
c     modify: mdate(mxtill,tlsq)
c
      include 'cwater.inc'
c     modify: ssc(mxnsl,mxplan), salb(mxplan)
c
      include 'cwint.inc'
c     modify: azm(mxplan)
c
c     + + + LOCAL VARIABLES + + +
c
      real rint, ddg(mxnsl), bd2(mxnsl), ssc2(mxnsl), thetf2(mxnsl),
     1    thetd2(mxnsl), sand2(mxnsl), clay2(mxnsl), orgma2(mxnsl),
     1    cec2(mxnsl), rfg2(mxnsl), del, dep, ksinv(mxnsl), slayth,
     1    hslope,sleng,yy(mxslp),avslp,totthk,thkadd,solbtm
      integer i, j, k, l, iout, ibdf(mxnsl), ithf(mxnsl), ithd(mxnsl),
     1    issc(mxnsl), iii, iiii, n , km
      character*20 slid, texid
c
c     + + + LOCAL DEFINITIONS + + +
c
c     i      -
c     j      -
c     k      -
c     l      -
c     iplant - id for plant type (1 - crop, 2 - range vegetation)
c     iout   -
c     slid   -
c     texid  -
c     rint   -
c     totthk - total thickness of soil profile
c     thkadd - amount to add to lowest soil layer if totthk is less
c              than 0.2m
c     solbtm -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     irinpt
c     profil
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (imodel.eq.2) iout = 32
      if (imodel.ne.2) iout = 31
      if (ivers.eq.3) iout = 38
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     CROP GROWTH PARAMETERS READ IN INFILE SJL 12/15/92
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if (yldflg.eq.1) write (46,1000)
c for the number(nsurf) of surface effect scenarios
      do 30, i = 1, nsurf
        if (lantyp(i).eq.1) then
c
c
c         ASSIGN CROPLAND TILLAGE values based on operation pointer(op(j,i))
c         for the number of tillage operations(j) in each surface
c         effect scenario(i)
c
          do 20 j = 1, ntill(i)
            rro(j,i) = rro1(op(j,i))
            rho(j,i) = rho1(op(j,i))
            rint = rint1(op(j,i))
            tdmean(j,i) = tdmea1(op(j,i))
            surdis(j,i) = surdi1(op(j,i))
            resman(j,i)=resma1(op(j,i))
c
            if(manver.lt.98.3.or.resman(j,i).le.4)then
c           pre 98.3 management code
              do 10, k = 1, ncrop
                if (mfocod(k).eq.1) then
                  mfo(j,i,k) = mfo11(op(j,i))
                  rmfo(j,i,k) = rmfo1(op(j,i))
                else
                  mfo(j,i,k) = mfo21(op(j,i))
                  rmfo(j,i,k) = rmfo2(op(j,i))
                end if
   10         continue
            else
c             New residue management code for management files >= 98.3
c             residue addition operation performed with no disturbance
c             iresad=index of residue type
c             resad=amount of residue added (kg/m^2)
c
              if(resman(j,i).eq.10.or.resman(j,i).eq.12)then
                iresad(j,i)=iresa1(op(j,i))
                resad(j,i)=resad1(op(j,i))
              end if
c
c             residue removal operation performed
c
              if(resman(j,i).eq.11.or.resman(j,i).eq.13)
     1            frmove(j,i)=frmov1(op(j,i))
c

              do 15, k = 1, ncrop
                if (mfocod(k).eq.1) then
c                 tillage intensity for non-fragile residue on interrill areas
c
c                   no surface disturbance
                  if(resman(j,i).eq.10.or.resman(j,i).eq.11)then
                    mfo(j,i,k)=0.0
                    rmfo(j,i,k) = 0.0
c
c                   surface disturbance
c
                  else if(resman(j,i).le.4.or.resman(j,i).ge.12)then
                    mfo(j,i,k) = mfo11(op(j,i))
                    rmfo(j,i,k) = rmfo1(op(j,i))
                  end if

                else
c                 tillage intensity for fragile residue on interrill areas
c
c                 no surface disturbance
                  if(resman(j,i).eq.10.or.resman(j,i).eq.11)then
                    mfo(j,i,k)=0.0
                    rmfo(j,i,k) = 0.0
c                 surface disturbance
                  else if(resman(j,i).le.4.or.resman(j,i).ge.12)then
                    mfo(j,i,k) = mfo21(op(j,i))
                    rmfo(j,i,k) = rmfo2(op(j,i))
                  end if

                end if

   15         continue
            end if
c
c           TDMEAN NOT YET USED REPLACED WITH TMP TO SAVE SPACE
c           RINT ONLY USED HERE SO REDUCED (ARRAY NOT NEEDED)
c           1         RINT(J, I), TDMEAN(J, I), (MFO(J, I, K), K = 1, NCROP)
c           IF(J.GT.1) THEN
c           IF(MDATE(J,I).LE.MDATE(J-1,I)) MDATE(J,I)=MDATE(J-1,I)+1
c           ENDIF
c
c           TO IDENTIFY IF THE TILLAGE SYSTEM IS A RIDGE FALLOW SYSTEM
c
c           print,' INPUT     rro= ',rro(j,i)
c           print,' INPUT     rho= ',rho(j,i)
c           print,' INPUT       j= ',j
c           print,' INPUT       i= ',i
c
c           PROBLEM WITH RINT - REAL NUMBER BUT CHECKING FOR EQUALITY TO
c           INTEGER 1    CHANGED CODE 9/20/91   DCF
c
c           1          RINT .EQ. 1) THEN
c           1          RINT(J, I) .EQ. 1) THEN
c
c           MARK RISSE IDENTIFIED A PROBLEM WITH THESE NEW LIMITS - IF THE USER
c           DID NOT REALLY WANT TO DENOTE A SYSTEM AS A RIDGE TILLAGE ONE -
c           IT MIGHT GET SET TO ONE ANYWAY IF ALL CONDITIONS MET FOR ONE OF
c           THE TILLAGE IMPLEMENTS IN A SEQUENCE  -  DCF   2/10/92
c
            if (iridge(i).eq.0.and.rho(j,i).ge.0.10.and.(rint.gt.0.6
     1          .and.rint.lt.1.4)) then
              iridge(i) = 1
            end if
c         print,' INPUT  iridge= ',iridge(i)
   20     continue
        end if
c
c     ...NO OTHER LAND TYPES HAVE SURFACE DISTURBANCES YET
c
   30 continue
c
c     NUMOF AND CLTPOS ARE READ IN INFILE, NOT CURRENTLY USED IN
c     MODEL, THEREFORE NOT DIMENSIONED.
c
c     READ IN CONTOUR SETS
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     CONTOUR VALUES READ IN INFILE (SJL)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     THE CALL TO IRINPT MUST BE OUT OF THE 500 LOOP JEF 4/12/91
c
c     IMODEL REMOVED FROM IRINPT CALL
c     OLD
c
c     IF (IRSYST .NE. 0) CALL IRINPT( IMODEL, IPLANT)
c     NEW
c
      nplane = jstruc
c
      if (irsyst.ne.0) call irinpt(iplant(1))
c
      hslope = 0.0
c
      do 110 iplane = 1, nplane
c
c*********************  SLOPE INPUT  *************************
c
c       azm and fwidth dimensioned and read for hillslope
c       profile (hillslope and hillslope/watershed versions)
c       and for each plane or channel (watershed version)
c
        if (ivers.eq.3) then
          call eatcom(10)
          read (10,*) azm(iplane), fwidth(iplane)
        else
          if (iplane.eq.1) then
            call eatcom(10)
            read (10,*) azm(1), fwidth(1)
          else
            azm(iplane) = azm(1)
            fwidth(iplane) = fwidth(1)
          end if
        end if
c
        call eatcom(10)
        read (10,*) nslpts(iplane), slplen(iplane)
c
        call eatcom(10)
        read (10,*) (xinput(l,iplane),slpinp(l,iplane),l = 1,
     1      nslpts(iplane))
c
        if (ivers.eq.3) then
          sleng = xinput(nslpts(iplane),iplane)
          yy(nslpts(iplane)) = 0.0
          do 11 k = 1, nslpts(iplane) - 1
            km = nslpts(iplane) - k
            yy(km) = yy(km+1) + (xinput(km+1,iplane)-
     1        xinput(km,iplane)) * (slpinp(km,iplane)+
     1        slpinp(km+1,iplane)) / 2.0
  11      continue
          avslp = yy(1) / sleng
c          write (38,*) 'avslp = ',avslp
c
c         save last slope segment if icntrl = 0
c
          if(icntrl(iplane).eq.0) then
            slplst = slpinp(nslpts(iplane),iplane)
          end if
c
c         set the slope of each segment to the average slope of the
c         channel
          do 35 l = 1,nslpts(iplane)
            chnx(iplane,l) = xinput(l,iplane) * slplen(iplane)
            chnslp(iplane,l) = avslp
c
c           watershed bombs when slope=0.0
c
            if(chnslp(iplane,l).le.0)
     1           chnslp(iplane,l)=0.0001
   35     continue
c

        end if
c
c       sdist(iplane) = xinput(nslpts(iplane),iplane)
c
        call profil(iplane)
c
        if (iplane.gt.1) then
          totlen(iplane) = totlen(iplane-1) + slplen(iplane)
        else
          totlen(iplane) = slplen(iplane)
        end if
c
        if ((iwpass.eq.1).or.(ivers.eq.2)) then
          hslope = hslope + (avgslp(iplane) * slplen(iplane))
          if (iplane.eq.nplane) then
            harea(ihill) = fwidth(1) * totlen(iplane)
            hleng = totlen(iplane)
            hslop = hslope / totlen(iplane)
          end if
        end if
c
c**********************  SOIL INPUT  *************************
c
c       LINE 1:  SOIL NAME, SOIL TEXTURE, NO. SOIL LAYERS, SOIL
c       ALBEDO, EFFECTIVE RELATIVE SATURATION, KI, KR
c
c       LINE 2 TO NO. SOIL LAYERS:  THICKNESS OF SOIL LAYER, BULK
c       DENSITY, SATURATED COND., AVAILABLE WATER, WILTING PT,
c       % SAND, SILT, CLAY, ORG MATTER, SOIL CONSTANT, ROCK FRAGMENTS,
c       UPPER LIMIT FOR CROP ROUTINES (UL4)
c
c       ... SOIL DATA
c
        call eatcom(11)
c
c       Don't read in AVKE (effective hydraulic conductivity of
c       surface soil) if pre V94.1 or superuser
c
        if (solwpv.lt.941.or.solwpv.eq.7777) then
          read (11,*) slid, texid, nslorg(iplane), salb(iplane),
     1        sat(iplane), ki(iplane), kr(iplane), shcrit(iplane)
        else
c
c         Read in AVKE if v94.1 or greater.  This value will be
c         passed to subroutine TILAGE to set the ssc values for
c         soil layers 1 and 2.
c
          read (11,*) slid, texid, nslorg(iplane), salb(iplane),
     1        sat(iplane), ki(iplane), kr(iplane), shcrit(iplane),
     1        avke(iplane)
c
c         Convert input value of effective conductivity from mm/h to m/s
c
          avke(iplane) = avke(iplane) / 3.6e6
c
        end if
c
c
c       Initialize ddg, ssc2, ..., error reported in SalfordFTN95, sjl
        do 39 i=1,mxnsl
          ibdf(i) = 0
          issc(i) = 0
          ithf(i) = 0
          ithd(i) = 0
          thetd2(i) = 0.0
          orgma2(i) = 0.0
          cec2(i) = 0.0
          clay2(i) = 0.0
          sand2(i) = 0.0
          thetf2(i) = 0.0
          bd2(i) = 0.0
          ssc2(i) = 0.0
          rfg2(i) = 0.0
          ddg(i) = 0.0
          ksinv(i) = 0.0
   39   continue
c       Write soil information to the top of the erosion summary
c       output file.
c
        if (solwpv.lt.941.and.iplane.eq.1) write (iout,1200)
        if (solwpv.eq.7777.and.iplane.eq.1) write (iout,1300)
c
        if (ivers.ne.3) write (iout,1100) iplane, slid, texid
        if (ivers.eq.3) write (iout,1150) iplane, slid, texid
c
        if (nslorg(iplane).gt.mxnsl-1) nslorg(iplane) = mxnsl - 1
        if (sat(iplane).gt.0.95) sat(iplane) = 0.95
c
        totthk=0.0
        do 40 i = 1, nslorg(iplane)
          call eatcom(11)
          if (solwpv.lt.941.or.solwpv.eq.7777) then
            read (11,*) solth1(i,iplane), bd2(i), ssc2(i), thetf2(i),
     1          thetd2(i), sand2(i), clay2(i), orgma2(i), cec2(i),
     1          rfg2(i)
          else
            read (11,*) solth1(i,iplane), sand2(i), clay2(i),
     1          orgma2(i), cec2(i), rfg2(i)
          end if

c
            totthk=totthk+solth1(i,iplane)
            if(i.eq.nslorg(iplane))then
               if(totthk.lt.200.0)then
                thkadd=200.0 - totthk
                write(6,1400)totthk,iplane,i,solth1(i,iplane)
                solth1(i,iplane) = solth1(i,iplane) + thkadd
              end if
            end if
 1400 format(/,' *** WARNING ***',/,' INCORRECT SOIL THICKNESS',/,
     1    ' TOTAL SOIL THICKNESS WAS ',f5.1,'mm',
     1    ' ON OFE ',i2,'-- THICKNESS MUST BE >= 200.0mm',/,
     1    ' THE BOTTOM LAYER (LAYER ',i2,') HAS BEEN CHANGED from ',
     1    f5.1,'mm to 200 mm',/,' *** WARNING ***',/)
c
c         Reset pre-94.1 values to 0.0, set 94.1+ values, and
c         leave 77770 (superuser) values alone.
c
          if (solwpv.ne.7777) then
            bd2(i) = 0.0
            ssc2(i) = 0.0
            thetf2(i) = 0.0
            thetd2(i) = 0.0
          end if
c
          if (solth1(i,iplane).gt.1800.0) solth1(i,iplane) = 1800.0
c
c         ... INITIALIZE AVAILABLE WATER CONTENT PER SOIL LAYER AT THE
c         BEGINNING OF SIMULATIONS ...
c
c         SET THE UPPER AND LOWER LIMITS TO INPUT
c
cd    modified by S. Dun 06/20/2002
c
c         NEUTERING OF KSAT CHANGES - prevent input Ks value from being
c         too small (LESS THAN 0.07 mm/h)  dcf 6/3/93
          if (solwpv.ne.2006) then
             if (ssc2(i).lt.0.07) ssc2(i) = 0.07
          else
             if (ssc2(i).lt.0.000000108) ssc2(i) = 0.000000108
          end if
cd    adjust the lower limit 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz)
cd    end modifying.
c
          if (orgma2(i).gt.10.) orgma2(i) = 10.
          if ((bd2(i).lt.0.8).and.(bd2(i).gt.0.00)) bd2(i) = 0.8
          if (bd2(i).gt.2.0) bd2(i) = 2.0
          if (rfg2(i).gt.50.0) rfg2(i) = 50.
c
c         CONVERT SOIL THICKNESS (SOLTHK) FROM MM TO METERS.
c
          bd2(i) = bd2(i) * 1000.
          solth1(i,iplane) = solth1(i,iplane) * .001
c
c         CONVERT MM/HR TO M/SEC
c
          ssc2(i) = ssc2(i) / 3.6e6
c
c         CONVERT INPUT TO DECIMAL
c
          sand2(i) = sand2(i) / 100.0
          clay2(i) = clay2(i) / 100.0
          orgma2(i) = orgma2(i) / 100.0
          rfg2(i) = rfg2(i) / 100.
   40   continue
        ddg(1) = solth1(1,iplane)
c
c       The follwing code is from WSU to setup the optional
c       bedrock restricting layer. This line follows the layer
c       lines in 2006 format soil files.
c  
        if (solwpv.eq.2006) then
           read (11,*) slflag(iplane),anisrt(iplane),kslast(iplane)
             kslast(iplane) = kslast(iplane)/3.6e6
           if (slflag(iplane).eq.0) then 
              anisrt(iplane) = 1
              kslast(iplane) = 0
           else
              if (anisrt(iplane).lt.0.0) anisrt(iplane) = 25
           end if
         else
           slflag(iplane) = 0
           anisrt(iplane) = 1;
         end if
c
c       Following section added to limit initial frost and thaw
c       depths to the soil thickness (max of 1.8 meters).  Savabi
c       originally had code to limit in INFILE, but did not know
c       soil depth at that point.     dcf   11/25/96
c       Additionally changed so that if thaw depth is entered as
c       deeper than the soil thickness, both thaw depth and frost
c       depth are reset to zero.  dcf  3/17/97
        if(frdp(iplane).gt.solth1(nslorg(iplane),iplane))
     1    frdp(iplane) = solth1(nslorg(iplane),iplane)
        if(thdp(iplane).gt.solth1(nslorg(iplane),iplane))then
          thdp(iplane) = 0.0
          frdp(iplane) = 0.0
        endif
c       End new section to limit initial frost/thaw depths  dcf
c
        do 50 i = 2, nslorg(iplane)
          ddg(i) = solth1(i,iplane) - solth1(i-1,iplane)
   50   continue
c
        slayth = 0.20
        dep = 0.0
c
c       WARNING: Following 3 do loops (60,70,90) must be kept as
c       separate loops and not combined
        do 60 i = 1, mxnsl - 1
          dep = dep + slayth
          iii = nint(dep*1000.0)
          iiii = nint(solth1(nslorg(iplane),iplane)*1000.0)
          if (iii.le.iiii) n = i
   60   continue
cd    Added by S. Dun, Jan 27, 2005
        solbtm = solth1(nslorg(iplane),iplane)
cd    End adding
        dep = 0.0
c
        do 70 i = 1, n
          dep = dep + slayth
          solth1(i,iplane) = dep
          ksinv(i) = 0.0
   70   continue
cd    Added by S. Dun, Jan 27, 2005
cd        if ((solwpv.eq.2006)
cd     1       .and.(solbtm.gt.(solth1(n,iplane)+0.001))) then
        if (solbtm.gt.(solth1(n,iplane)+0.001)) then
           solth1(n+1,iplane) = solbtm
           n = n + 1
        Endif
cd    End adding
        nslorg(iplane) = n
        j = 1
c
        do 90 i = 1, nslorg(iplane)
cd          if ((solwpv.eq.2006).and.(i.eq.nslorg(iplane)))
          if ((i.eq.nslorg(iplane)).and.(n.gt.1))  then
              slayth= solth1(n,iplane) - solth1(n-1,iplane)
          endif
          del = slayth
          ibdf(i) = 0
          issc(i) = 0
          ithf(i) = 0
          ithd(i) = 0
   80     if (ddg(j).le.del) then
c
c           The following IF statements for bd1, ssc1, thetf1, thetd1
c           maintain the integrity of user input zero values.  Without
c           them, the averaged values may be much too small because of
c           user input zero values, but the model will not recognize
c           that the values must be calculated.  This also means that
c           the user should use a minimum top layer thickness of 200 mm
c           if the model is to recognize the user input K-sat values.
c
            if (bd2(j).lt.0.001) then
              ibdf(i) = 1
            else
              bd1(i,iplane) = bd1(i,iplane) + (ddg(j)/slayth) * bd2(j)
            end if
c
cd    Modified by S. Dun 06/20/2002
cd    adjust the lower limit to 3.0e-14 m/s 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz Table 3.2 for unfractured igneous
cd     and metamorphic rocks.)
c      NOTE that ssc uses a geometric rather than arithmetic mean
            if (solwpv.eq.2006) then
              if (ssc2(j).lt.3.0e-14) then
                issc(i) = 1
              else
                ksinv(i) = ksinv(i) + (ddg(j)/ssc2(j))
              end if
            else
              if (ssc2(j).lt.1.0e-14) then
                issc(i) = 1
              else
                ksinv(i) = ksinv(i) + (ddg(j)/ssc2(j))
              end if
            end if

cd    End modifying.
c
            if (thetf2(j).lt.0.00001) then
              ithf(i) = 1
            else
              thetf1(i,iplane) = thetf1(i,iplane) + (ddg(j)/slayth) *
     1            thetf2(j)
            end if
c
            if (thetd2(j).lt.0.00001) then
              ithd(i) = 1
            else
              thetd1(i,iplane) = thetd1(i,iplane) + (ddg(j)/slayth) *
     1            thetd2(j)
            end if
c
            sand1(i,iplane) = sand1(i,iplane) + ((ddg(j)/slayth)*
     1          sand2(j))
            clay1(i,iplane) = clay1(i,iplane) + ((ddg(j)/slayth)*
     1          clay2(j))
            orgma1(i,iplane) = orgma1(i,iplane) + (ddg(j)/slayth) *
     1          orgma2(j)
            cec1(i,iplane) = cec1(i,iplane) + ((ddg(j)/slayth)*cec2(j))
            rfg1(i,iplane) = rfg1(i,iplane) + ((ddg(j)/slayth)*rfg2(j))
            del = del - ddg(j)
c
            if (abs(del).le.0.001) then
              j = j + 1
              go to 90
            else
              j = j + 1
              go to 80
            end if
c
          else
c
            if (bd2(j).lt.0.001) then
              ibdf(i) = 1
            else
              bd1(i,iplane) = bd1(i,iplane) + (del/slayth) * bd2(j)
            end if
c
cd    Modified by S. Dun 06/20/2002
cd    adjust the lower limit to e-14 m/s 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz)
             if (solwpv.eq.2006) then
               if (ssc2(j).lt.3.0e-14) then
                         issc(i) = 1
                     else
                 ksinv(i) = ksinv(i) + (del/ssc2(j))
               end if
                    else
                    if (ssc2(j).lt.1.0e-14) then
                         issc(i) = 1
                     else
                 ksinv(i) = ksinv(i) + (del/ssc2(j))
               end if
                  end if

cd    End modifying.
c
            if (thetf2(j).lt.0.00001) then
              ithf(i) = 1
            else
              thetf1(i,iplane) = thetf1(i,iplane) + (del/slayth) *
     1            thetf2(j)
            end if
c
            if (thetd2(j).lt.0.00001) then
              ithd(i) = 1
            else
              thetd1(i,iplane) = thetd1(i,iplane) + (del/slayth) *
     1            thetd2(j)
            end if
c
            sand1(i,iplane) = sand1(i,iplane) + ((del/slayth)*sand2(j))
            clay1(i,iplane) = clay1(i,iplane) + ((del/slayth)*clay2(j))
            orgma1(i,iplane) = orgma1(i,iplane) + (del/slayth) *
     1          orgma2(j)
            cec1(i,iplane) = cec1(i,iplane) + ((del/slayth)*cec2(j))
            rfg1(i,iplane) = rfg1(i,iplane) + ((del/slayth)*rfg2(j))
            ddg(j) = ddg(j) - del
c
            if (abs(ddg(j)).le.0.001) then
              j = j + 1
              go to 90
            else
              continue
            end if
c
          end if
c
   90   continue
c
c       The following loop (100) must be outside the previous loop (90)
c       or there will be errors for certain cases of averaging
c
        do 100 i = 1, nslorg(iplane)
          if (ibdf(i).eq.1) bd1(i,iplane) = 0.00
c
          if (issc(i).eq.1) then
cd    Modified by S. Dun 06/20/2002
cd    adjust the lower limit to e-14 m/s 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz)
           if (solwpv.eq.2006) then
               ssc1(i,iplane) = 0.000000108 / 3.6e6
           else 
               ssc1(i,iplane) = 0.07 / 3.6e6
           end if
cd    End modifying.
          else
            ssc1(i,iplane) = slayth / ksinv(i)
          end if
c
          if (ithf(i).eq.1) thetf1(i,iplane) = 0.0
          if (ithd(i).eq.1) thetd1(i,iplane) = 0.0
  100   continue
c
c       MOVED the following lines of CODE from subroutine INFILE, since
c       values for ssc1 are not read in until this point.
c       dcf  11/17/93
c       ... SATURATED CONDUCTIVITY FOR THE SINGLE STORM VERSION
c
        if (imodel.eq.2) ks(iplane) = ssc1(1,iplane)
c
c       prevent tillage depths from being greater
c       than the depths of the soil profile.
c
        tillay(1,iplane) = 0.1
        tillay(2,iplane) = 0.2
        if (tillay(2,iplane).gt.solth1(nslorg(iplane),iplane))
     1      tillay(2,iplane) = solth1(nslorg(iplane),iplane)
c
        if (tillay(1,iplane).gt.solth1(nslorg(iplane),iplane))
     1      tillay(1,iplane) = solth1(nslorg(iplane),iplane)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     MANAGEMENT INITIAL CONDITION INFO READ IN INFILE AS OF VER 92.4
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
  110 continue
c
      close (10)
      close (11)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     DRAINAGE INPUT IN INFILE SJL
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      return
c
 1000 format (/,' Yields Through the Simulation Period: ',/)
 1100 format (6x,'PLANE',i3,1x,2a20)
 1150 format (6x,'CHANNEL',i3,1x,2a20)
 1200 format (/,' *** WARNING ***',/,' SOIL FILE FORMAT IS PRE-94.1',/,
     1    ' Model will recalculate soil layer values',
     1    ' for BD, conductivity,',/,
     1    ' thetfc, thetdr, and will adjust hydraulic',
     1    ' conductivity with time',/,' *** WARNING ***',/)
 1300 format (/,' *** CAUTION ***',/,' SOIL FILE FORMAT IS NON-STANDARD'
     1    ,' WEPP ',/,
     1    ' User is solely responsible for entering CORRECT ',/,
     1    ' values for BD, SSC, THETFC, THETDR',/,' *** CAUTION ***',/)
      end
