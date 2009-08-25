      subroutine pmetcoef(cropname,iorder)
c
c     + + + PURPOSE + + +
c     Read in basal crop coefficient for Penman-Monteith
c     dual coefficient Method  
c
c   Note: this routine is now in SI (meters)
c
c
c     Called from INFILE
c     Author(s): Shuhui Dun, Joan Wu
c     Reference: FAO 56
c
c     Date recoded: 04/11/2003 
c     Recoded by: Shuhui Dun.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      character *8 cropname
      integer iorder
c
c     + + + ARGUMENT DEFINITIONS + + +
c     cropname - crop name
c     iorder   - index of this crop. 
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'ccrpet.inc'
c     readin: kcb
c
c
c     + + + LOCAL VARIABLES + + +
      character *8 names
      integer line, kcbfg,irecord,i   
      character *20 actlnam
c
c     + + + LOCAL DEFINITIONS + + +
c    names: crop names in the  
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c **********************************************************************
c ** This section of the code read in mid-season crop coefficient     **
c **********************************************************************
c
cd    We adpted Claudio Stockle's recommendation. We use an advanced 
cd    method instead of the method in FAO 50 documentation to deal 
cd    with the ET deffience on crop growth stage. please 
cd    check the related documents for detail.
c
c    read in kcb
c    read in coefficient in RAW formula
      rewind(22)
      kcbfg = 0
      read(22,*) irecord
      Do 30 i = 1, irecord
        read(22,*) names
        if(names.eq.cropname) then
            backspace(22)
            read(22,*) names,kcb(iorder),rawp(iorder),line,actlnam
            kcbfg = 1
            goto 40
        endif
   30 continue
   40 continue
      if (kcbfg.ne.1) then        
        rewind(22)
        read(22,*) irecord
        read(22,*) names,kcb(iorder),rawp(iorder),line,actlnam
        write(6,1000)
        write(6,1020)
        write(6,*) cropname
        write(6,1030)
        write(6,*) actlnam
        write(6,1040)
      endif 
c
c
1000    format('*************** Notice *****************')
1010    format(a8)
1020    format(1x,'No Penman-Monteith ET parameters in defaut data for')
1030    format(1x,'Using parameters of the crop')
1040    format(1x,'if you are not satified with the selected crop,',/,1x,
     1    'Please add in corresponding parameters for your crop',/,1x,
     1    'in the file pmetpara.txt')
      return
      end
