c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c  function to check if a string y contains string x
c    -- used to check if particular remark code is present
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        21 may 2007
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      logical function ifany(x,y)
      character*(*) x,y

        ifany = .FALSE.
      do 10 i=1,len(y)-len(x)+1
        if(x .eq. y(i:i+len(x)-1)) ifany = .TRUE.
10    continue
      return
      end
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      subroutine readio1(line,nval,alpha)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      
c    parses input line into tokens
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        09 aug 2007
c
c       input variables:
c       ------------------------------------------------------------------------
c            line     char   line of input text to be parsed
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            nval     i       number of tokens
c            alpha    char(*) vector of tokens
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine readio1(line,nval,alpha)
      implicit none
      integer max_tokns
      parameter(max_tokns=10)
      character*132 line,alpha(max_tokns)
      integer i,j,nval,bgtokn(max_tokns),entokn(max_tokns)
      logical start
        
      start = .true.
      j     = 0
      do 10 i=1,len(line)
          if(line(i:i) .ne. ' ' .and. start) then
              j = j+1
              if(j .gt. max_tokns) goto 10
              bgtokn(j) = i
              start = .false.
          else if(line(i:i) .eq. ' ' .and. .not. start) then
              entokn(j) = i-1
              start = .true.
          endif
10    continue
          nval = min(j,max_tokns)
      if(j .eq. 0) then
        alpha(1) =''
      else
        do 20 i=1,min(j,max_tokns)
           alpha(i) = '                    '
          read(line(bgtokn(i):entokn(i)),'(a)',end=20,err=20) 
     1                 alpha(i)(1:(entokn(i)-bgtokn(i)+1))
20      continue
      endif
      return
      end
      
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      subroutine cvrtupcase(a,b)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      
c    parses input line into tokens
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        09 nov 2003
c
c       input variables:
c       ------------------------------------------------------------------------
c            a        char   line of input text to be converted to upper case
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            b        char   line of upper case output
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine cvrtupcase(a,b)
      character*(*) a,b
      
      do 10 i=1,min(len(a),len(b))
        if(a(i:i) .ge. 'a' .and. a(i:i) .le. 'z') then
          b(i:i) = char(ichar(a(i:i)) - ichar('a') + ichar('A'))
        else
          b(i:i) = a(i:i)
        endif
10    continue
      return
      end
            
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      subroutine makefn_out(infile,arg,iunit,outfile)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c      
c    creates filename with new extension, opens file (if possible)
c    and returns filename
c       
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    development history
c
c    timothy a. cohn        09 nov 2003
c
c       input variables:
c       ------------------------------------------------------------------------
c            infile   char   filename -- line of text (with or without suffix)
c            arg      char   extension -- usually '.dat', '.csv', etc.
c            iunit    i*4    fortran unit number
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c       output variables:
c       ------------------------------------------------------------------------
c
c            outfile  char   filename
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
      subroutine makefn_out(infile,arg,iunit,outfile)

      implicit none

      integer
     1  i,ict,iunit,j,nca,nci

      character*(*)
     1  infile,arg,outfile
     
      character*80 fmt1
      
      logical ex
     
        nci = len(infile)
        nca = len(arg)
        ict = 0
      do 10 i=1,nci
        if(infile(i:i) .eq. '.' .or. infile(i:i) .eq. '') goto 20
10    continue
20    continue
      do 30 j=1,nca
        if(arg(j:j) .eq. '') goto 40
30    continue
40    continue
        write(fmt1,'(''(a'',i2,'',a'',i2,'')'')') i-1,j-1
        write(outfile,fmt1) infile(1:i-1),arg(1:j-1)

50    continue
        inquire(file=outfile,exist=ex)
      if(.not. ex) then
        open(unit=iunit,file=outfile,status='new')
      else
          ict = ict+1
        if(ict .lt. 10) then
          write(fmt1,'(''(a'',i2,'',i1,a'',i2,'')'')') i-1,j-1
          write(outfile,fmt1) infile(1:i-1),ict,arg(1:j-1)
        else
          write(*,*) 'File already exists:  ',outfile
          write(*,*) 'Need valid name for output file'
          write(*,*) 'Enter OUTPUT filename (a80)'
          read(*,*) outfile
        endif
        goto 50
      endif

      return
      end
      SUBROUTINE GET_DATE_TIME(IUNITW)
C   BEGINNING OF TAC KLUGE TO GET ROUTINE TO COMPILE
      integer iunitw
      character*30 date
      call fdate(date)
      write(IUNITW,'(t35,a30)') date
      return
      end

!      SUBROUTINE GET_DATE_TIME(UNITW)
!!**********************************************************************
!!     Program Writes Current Run Date and Time
!!    
!!     Input:
!!     Iunitw     = integer specifying the unit number of OUTput file
!!
!!     Output written directly to file (Iunitw)
!!
!!     Subprograms needed: Fortran 95 intrinsic function
!!     http://gcc.gnu.org/onlinedocs/gcc-4.2.1/gfortran/DATE_005fAND_005fTIME.html
!!
!!     DEVELOPED BY:
!!     John F. England, Jr
!!     Bureau of Reclamation
!!     Flood Hydrology & Meteorology Group, 86-68530        
!!     Bldg. 67, Denver Federal Center
!!     Denver, CO USA 80225
!!     Phone: (303) 445-2541
!!     Email: jengland@do.usbr.gov
!!     ftp://ftp.usbr.gov/jengland/  (not permanent; email first)
!!
!!     DATE ORIGINAL VERSION: June 4, 1999
!!
!!     MODIFICATIONS:
!!     26-SEP-2007, jfe
!!     complete rewrite in terms of Fortran 95 intrinsic,
!!     eliminated use of DF/Compaq library
!!
!!**********************************************************************
!!
!      integer,dimension(8) :: values
!      INTEGER :: tmpday, tmpmonth, tmpyear
!      INTEGER :: tmphour, tmpminute, tmpsecond, tmphund
!      CHARACTER(1) mer
!!
!!     get the time and date - all values
!      call date_and_time(VALUES=values)
!!
!!     set values
!!     this is not necessary; only done for clarity
!!     to remember what is in values(.)
!      tmpyear = values(1)
!      tmpmonth = values(2)
!      tmpday = values(3)
!     
!      tmphour = values(5)
!      tmpminute = values(6)
!      tmpsecond = values(7)
!      tmphund = values(8)
!
!!     set am/pm
!!     tmphour = 0 to 23; 0 = midnight; 12 = noon
!      IF (tmphour .GT. 12) THEN
!        mer = 'p'
!        tmphour = tmphour - 12
!      ELSE IF(tmphour .EQ. 12) THEN
!        mer = 'p'
!      ELSE
!        mer = 'a'
!      END IF
!!
!      WRITE (Iunitw, 900) tmpmonth, tmpday, tmpyear
!900   FORMAT(/28x,'PeakfqSA Run Date is:',2x,I2, '/', I2.2, '/', I4.4)
!      WRITE (Iunitw, 901) tmphour,tmpminute,tmpsecond,mer
!901   FORMAT(28x,'PeakfqSA Run Time is:',2x,I2, ':', I2.2, ':', I2.2,
!     *     ' ',A, 'm'/)
!!
!      Return
!      END Subroutine
!
c
       character function trim(ch)
c  quick-and-dirty function to get past compiler issue (tac 22 Oct 2012)
       character*(*) ch
       do 10 i=1,len(ch)
         if(ch(i:i) .ne. ' ') goto 20
10     continue
20     continue
         ifirst=i
       do 30 i=len(ch),1,-1
         if(ch(i:i) .ne. ' ') goto 40
30     continue
40     continue
         ilast=i
       trim = ch(ifirst:ilast)
       return
       end
       