c
      subroutine readin(un,num,min,max,name)
c
c     parameter:
c        un - the file unit
c        num - number read
c        min - min allowed
c        max - max allowed
c
      character*12 name
      integer num, max, min, un
c
      call eatcom(un)
c
c     num=number of scenarios
c
      read (un,*) num
      if (num.gt.max.or.num.lt.min) then
        write (6,*) ' *** ERROR ***'
        write (6,1000) name, num, min, max
        stop
      end if
c
c
c     print*,'INT: ',num
      return
 1000 format (//' *** ',a6,' read as ',i4,'.  Must be between ',i4,
     1    ' and ',i4,' ***'/)
      end
