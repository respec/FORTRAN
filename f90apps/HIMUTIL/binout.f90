      program binoutdriver

      real    values(5)
      integer nval,namelen(5),retcod,i
      integer UnitFlag,PrintLevel,CurDate(6)
      character*255 binname
      character*50 namestring
      character*6  opname,section
      logical      first

      data opname,section/'PERLND','INTEGR'/
      data namestring /'LZS       LZSN      LZRAT     ERRECH    REMFC     '/
      data namelen,opid/3,4,5,6,5,101/
      data curdate,unitflag,printlevel/1996,8,2,0,0,0,1,3/

      nval= 5
      first= .true.

      do 10 i= 1, nval
        values(i)= 1- (i/10)
 10   continue

      binname= 'test.hbn'
      retcod= Write_Binout(binname,Nval,Values,NameLen,NameString,Opname,OpID,Section,UnitFlag,PrintLevel,CurDate,First)

      write (*,*) 'retcod',retcod
     
      end
