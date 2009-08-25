      subroutine open(un,istat,flag,mesg,filen)
c
c     + + + PURPOSE + + +
c
c     Opens files and checks for errors in opening.
c
c     Called from: SRS INFILE & OUTFIL
c     Author(s): Livingston, Whittemore, Ascough II
c     Reference in User Guide:
c
c     Version: This module not yet recoded
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer istat, flag, un
      character*51 filen
      character*65 mesg
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     istat - status for open file (1-new, 2-old, 3-unknown)
c     filen - file name read in
c     flag - flag for to print message or not (> 1 print ; < 0 do not)
c     mesg - prompt for file name
c     un - unit number
c
c     + + + COMMON BLOCKS + + +
c
      include 'cerrid.inc'
c     read:   ibomb
c
      include 'cstruc.inc'
c     read:   ivers
c
c     + + + LOCAL VARIABLES + + +
c
      integer errcod
      character*7 stat(3)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     errcod -
c     stat(3) -
c
c     + + + SAVES + + +
c
      save
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data stat /'new', 'old', 'unknown'/
c
c     + + + END SPECIFICATIONS + + +
c
c
c   ... check to see if this is a file name already read in.
c   ... if it is don't read again if flag = 0 don't print mesg or
c   ... read filen
c
c
   10 if (flag.gt.0) write (6,1000) mesg
      if (flag.gt.0) read (5,1100) filen
c
      write (6,2000) filen
      if (flag.eq.3) return
c
c     ... check if file name was not entered
c
      if (filen(1:1).eq.' ') then
        if (ibomb.eq.1) then
          write (6,1300)
          stop
        else
          write (6,1400)
            go to 10
        end if
      end if
c
c     ... check for file name length > 50 characters
c     ... (LAHEY RESTRICTION)
c
      if (filen(51:51).ne.' ') then
        if (ibomb.eq.1) then
          write (6,1600) mesg, filen
          stop
        else
          write (6,1500) filen
          go to 10
        end if
      end if
c
c     ... open file
c
      open (unit=un,file=filen,status=stat(istat),
     1   err=20,iostat=errcod)
c
      return
c
c     ... error in opening file
c
   20 if (istat.eq.2) then
c
        write (6,2100) mod(errcod,256)
c
        if (ibomb.eq.1) then
          write (6,1200)
          write (6,1700) mesg, filen
          stop
        else
          write (6,1800)
          go to 10
        end if
c
      else
c
        write (6,2100) mod(errcod,256)
c
        if (ibomb.eq.1) then
          write (6,1200)
          write (6,1900) mesg, filen
          stop
        else
          write (6,1900) mesg, filen
          go to 10
        end if
c
      end if
c
 1000 format (/,2x,a,/)
 1100 format (a50)
 1200 format (' ***ERROR***')
 1300 format (/,' ***ERROR1300***',/,2x,a65,/,a50,//,2x,
     1    'No file name entered')
 1400 format (' *** No file name entered',/,2x,' *** Please try again')
 1500 format (//,2x,a51,//,2x,
     1    ' *** File name must be less than 51 characters',/,2x,
     1    ' *** including the path and extension')
 1600 format (//,1x,a65,/,1x,a51,//,2x,'***ERROR***',/,
     1    ' File name must be less than 51 characters'/,
     1    ' including the path and extension'/)
 1700 format (//,1x,a65,/,1x,a50,'File does not exist')
 1800 format (//,2x,' *** Cannot open file'/2x,
     1    ' *** File does not exist')
 1900 format (//,1x,a65,/,1x,a50,/,' file already exists')
 2000 format (' file name read in: ',a)
 2100 format (' LAHEY error code = ',i4)
c2000 format (//2x,' *** file already exists')
c
      end