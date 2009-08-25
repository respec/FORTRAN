c
      subroutine scenhd(un,name,luse)
c
c     parameter:
c        un - the file unit
c        name - scenario name as read
c        luse - landuse
c
      integer un, luse, i
      character*51 name
c
c     local:
c        com - temp for reading comments
c        i - counter
c     integer i
c
c
c     COM not used  12-21-93 07:28am  sjl
c
c      character*60 com
c
c
c     read the scenario name
c
      call eatcom(un)
      read (un,1000) name
c
c     read in 3 line description (60 char max)
c
      do 10, i = 1, 3
c
        read (un,*)
c
c
c     COM not used  12-21-93 07:28am  sjl
c
c     read (un,1100) com
   10 continue
c
c
c     read the land use
c
      call readin(12,luse,1,4,name)
c
c
      return
 1000 format (a8)
c
c     label 1100 not referenced  12-21-93 07:27am  sjl
c
c     1100 format (a60)
      end
