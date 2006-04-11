      SUBROUTINE VERSION
C
C    SUBROUTINE VERSION
C    CREATE 9/15/97 by 
C       Charles I. Moore
C       CDM Annandale, Va.
C    
C    Make it easier to update and trace program version numbers.
C
      INCLUDE 'VERSION.INC'
CIM VERID IS THE VERSION NUMBER THAT APPEARS IN VARIOUS LOCATIONS
CIM IS 6 CHARACTER STRING
      VERID = '4.4GU '
CIM DESTRING IS 45 CHARACTER STRING WITH ADDITIONAL INFORATION
C                   000000000111111111122222222223333333333444444
C                   123456789012345678901234567890123456789012345
      DESTRING(1) ='                CDM/OSU Beta                 '
      DESTRING(2) ='      Release Date - September 28, 1999      '
      DESTRING(3) ='Camp Dresser & McKee and Oregon State Univ.  '
      DESTRING(4) ='         Chuck Moore and Wayne Huber         '
C      DESTRING(5) =' Compiled using KAI GUIDE 3.5 Dual Processor '
      DESTRING(5) =' Compiled using Digital Visual Fortran 6.0   '
      RETURN
      END
