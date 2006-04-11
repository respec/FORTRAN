C   New common for input of intermediate printout summary periods.
      PARAMETER (MAXBE=11)
      COMMON /BE1/ IBE,NUMBE,IBESTART(0:MAXBE),IBEEND(0:MAXBE)
CIM Add JELEV to common to carry to other input routines.
	  COMMON /BE2/ JELEV
