      SUBROUTINE PFNKEY (CNKEY, CSTRNG, NS)
C
C        SUBROUTINE 'PFNKEY' ALLOWS A USER TO DIRECTLY ACCESS
C        THE CONTENTS OF A FUNCTION KEY.  THE USER SIMPLY
C        CALLS THE SUBROUTINE, SUPPLYING THE GIVEN KEY, AND
C        A CHARACTER*20 VARIABLE.  PFNKEY WILL RETURN THE
C        KEY'S RESPECTIVE FUNCTION OR A VALUE OF '0' FOR NS
C        IF THE FUNCTION KEY COULD NOT BE FOUND.
C
C
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
CADD C.PINT                                                             H
      INCLUDE 'pint.h'                                                  MLlg
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
C
      CHARACTER*(IFUNLN) CSTRNG
      CHARACTER*1 CNKEY
C
C
      DO 10 I=1,NKEY
        IF (CNKEY .EQ. CKEY(I)) GO TO 30
 10   CONTINUE
C   ***
C   ***
C   ** NO MATCH
C   ***
      NS = 0
      RETURN
C   ***
C   ***
C   ** STORE DEFINED FUNCTION IN 'CSTRNG'
C   ***
 30   CSTRNG = CFUNCT(I)(1:IKEY(I))
      NS = IKEY(I)
      RETURN
C
      END
