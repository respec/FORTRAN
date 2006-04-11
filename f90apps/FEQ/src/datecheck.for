	program datecheck
      integer i,yr,mn,dy

	open(unit=11,file="datecheck.lis")
	do i = 1200,2000
        call invmjd(i,yr,mn,dy)
      end do
      stop
      end
C
C
C
      SUBROUTINE   INVMJD
     I                   (MJD,
     O                    YR, MN, DY)
 
C     + + + PURPOSE + + +
C     Invert the modified julian date as computed by function MJD
 
C     + + + DUMMY ARGUMENTS + + +
      INTEGER DY, MJD, MN, YR
 
C     + + +DUMMY ARGUMENT DEFINITIONS + + +
C     MJD    - value of modified julian data number to invert
C     YR     - calendar year
C     MN     - number of month(1-12)
C     DY     - day in the month
 
C     Developed from information given in: "Astronomical Formulae
C     for Calculators', Jean Meeus, published by Willmann-Bell.
 
C     + + + LOCAL VARIABLES + + +
      INTEGER A, ALPHA, B, C, D, E, Z
 
C     + + + INTRINSICS + + +
      INTRINSIC DBLE, INT
     

C***********************************************************************
C     CONVERT TO JULIAN TIME PLUS THE .5 DAY CORRECTION. YIELDS AN
C     INTEGER
 
      Z = MJD + 679006 + 1720994 + 1
 
      IF(Z.LT.2299161) THEN
        A = Z
      ELSE
        ALPHA = INT((DBLE(Z) - 1867216.24D0)/36524.25D0)
        A = Z + 1 + ALPHA - ALPHA/4
      ENDIF
 
      B = A + 1524
      C = INT((DBLE(B) - 122.1D0)/365.25D0)
      D = INT(365.25D0*DBLE(C))
      E = INT(DBLE(B-D)/30.6001D0)
 
      DY = B - D - INT(30.6001D0*DBLE(E))
      IF(E.LE.13) THEN
        MN = E - 1
      ELSE
        MN = E - 13
      ENDIF
      IF(MN.GE.3) THEN
        YR = C - 4716
      ELSE
        YR = C - 4715
      ENDIF
 
	write(11,'(11i8)') mjd,z,a,alpha,b,c,d,e,dy,mn,yr

      RETURN
      END
