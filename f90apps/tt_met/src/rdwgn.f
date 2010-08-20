      subroutine rdwgn

!!    Read in the SWAT weather generator file
!!    Appropriate line number is ID plus 1
      use parm
      implicit none

      character *2 cdum1
      character *80 cdum2, cdum3
      integer idfilno, jk
      real filno,vars(172)

      Open(unit=103,file='statwgn.txt', STATUS='OLD', err=150)
C      integer IOERR
C      Open(unit=103,file='statwgn.txt', STATUS='OLD',IOSTAT=IOERR)
C      write(*,*) ' iostat returning from open statwgn',IOERR
C      character *80 cmsg
C      call iostat_msg(ioerr,cmsg)
C      write(*,*) ' iostat msg returning from open statwgn',cmsg
C      pause
      goto 200
 150  write(*,'(A)')' error opening statwgn.txt in rdwgn'
      pause
      stop
 200  continue
      if(idwgn.lt.2) then
         write(*,'(A,1x,I4)')' error in rdwgn, idwgn =',idwgn
      endif
C     Do jk=1, IDwgn
C        read(103,'(1x)')
C     end do
      read(103,*)cdum1,cdum2,cdum3,filno,(vars(jk),jk=1,172)
      idfilno=INT(filno)
      if(idfilno.ne.idwgn)then
        write(*,'(A)')' wgn file not matched! End Run'
        write(*,*)idfilno,idwgn
        write(*,'(A)')cdum3
        pause
        stop
      endif
!!     assign the variables    
      wlat(1)=vars(1)
      welev(1)=vars(3)
      do jk=i,12
         tmpmx(jk,1)=vars(jk+4)
         tmpmn(jk,1)=vars(jk+16)
         pr_w(1,jk,1)=vars(jk+88)
         pr_w(2,jk,1)=vars(jk+100)
         pcpd(jk)=vars(jk+112)
         solarav(jk,1)=vars(jk+136)
         dewpt(jk,1)=vars(jk+148)
         wndav(jk,1)=vars(jk+160)
      end do

      close (103)      
      
      Return
      end
