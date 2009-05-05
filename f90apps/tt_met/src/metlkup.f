      subroutine metlkup(idmet,idwgnz)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine looks up the appropriate SWAT weather generator
!!    based on the BASINS met station ID

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    IDmet       |              |BASINS weather station ID - used to look up weather generator 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!   idwgnz       |              |number of SWAT weather generator           
      
      implicit none
      integer idwgnz, id1
      character *8 idmet, idchar

      idwgnz=0      
      open(109,file='metlkup.txt',status='OLD',err=100)
!!     read header line
      read(109,'(1X)')
!!     cycle through the file until correct record is found
 50   continue
        read(109,*,end=150,err=150)idchar, id1
        if(idchar.eq.idmet) then
           idwgnz=id1
           go to 200
        endif
      go to 50
 100  write(*,'(A)')' unable to open metlkup.txt'
      pause
      stop
 150  write(*,'(A)')' Basins IDmet not found in metlkup'
      pause
      stop
 200  continue
      close (109)
      if (idwgnz.eq.0) go to 150
      write(*,*)idmet,idwgnz
      return
      end