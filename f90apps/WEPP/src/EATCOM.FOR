c   eatcomm
c             read past any comment lines in the opened file so that
c             the caller can safely read values.  if there are no
c             comments on the line, eatcomm() repositions the file
c             pointer to the beginning of the current line.
c             handles EOF condition gracefully by positioning the
c             file pointer to the last record before returning.
c
c             dw - Mon Sep 28 14:12:36 EST 1992
c
      subroutine eatcom(handle)
c
c     parameter:
c             handle - the open file unit
c
      integer handle, i
c
c     local variable
c              line - 1st character on file line read.
c
      character*80 line
      character*1 ch
c
c       scan the first character on the line
c       (fortran conveniently throws away the rest of the line for us)
c       if this character is a '#', then we have a comment, else
c       else reposition to the beginning of the record and return.
c
   10 read (handle,1000,err=30) line
      do 20 i = 1, 80
        ch = line(i:i)
        if (ch.eq.'#') then
          go to 10
        else if (ch.ne.' ') then
          go to 30
        end if
   20 continue
      go to 10
c     if (line(1:1).eq.'#'.or.line.eq.'                    ') then
c     if (line(1:1).eq.'#'.or.len(line).eq.80) then
c     print*,'no(',line(1:1),')'
c
c     multi comments?
c
c     go to 10
c     else
c
c     this is not a comment line, exit the loop.
c
c     print*,'ok(',line(1:1),')'
c     go to 20
c     end if
   30 backspace (handle)
      return
 1000 format (a80)
      end
