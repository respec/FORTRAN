*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/menu.f,v 1.1 1998/07/07 19:32:24 grogers Exp $
*  menu.f
*
	subroutine menu(dsel)

c Purpose: Prints menu and gets datum option
c ******************************************
c  
*  $Log: menu.f,v $
*  Revision 1.1  1998/07/07 19:32:24  grogers
*  PR#0, initial load of nadcon_lib
*
*

	INTEGER LUIN,LUOUT,NOUT,NIN,NAPAR,LUAREA
	INTEGER MXAREA
	CHARACTER*1  ans2
	LOGICAL dsel

	PARAMETER(MXAREA=8)
	COMMON /INOUT/ LUIN,LUOUT,NOUT,NIN,NAPAR,LUAREA(2*MXAREA)

	write(LUOUT,900)
  900	FORMAT(10X,'        AVAILABLE DATUM CONVERSIONS',//,
     +         10x,'              NAD 27, NAD 83       ',/,
     +         10X,'              NAD 83, HPGN         ',//,
     + ' Hit RETURN for NAD 27, NAD 83',/
     + ' Press any other key (except ''Q'') then RETURN for NAD 83,HPGN',
     +    /,
     + ' Press ''Q'' then RETURN to quit',//)
	
	read(LUIN,'(a1)') ans2

	if(ans2.eq.' ') then
	   dsel = .TRUE.
	   return
	else if (ans2.eq.'Q'.or.ans2.eq.'q') then
 9999	  write(LUOUT,*) 'End of Nadcon conversions'
	  STOP
	else
	  dsel = .FALSE.
	end if

	end
