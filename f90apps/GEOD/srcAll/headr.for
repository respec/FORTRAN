*  $Header: /nwiscvs/watstore/geod/src/nadgrd_lib/headr.f,v 1.1 1998/07/07 20:20:39 grogers Exp $
*  headr.f
*
      SUBROUTINE HEADR (VRSION)

*  Purpose: This subroutine prints the header information
*********************************************************
*
*  $Log: headr.f,v $
*  Revision 1.1  1998/07/07 20:20:39  grogers
*  PR#0, initial add of nadgrd_lib
*
*

*     IMPLICIT REAL (A-H, O-Z)
*     IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      REAL VRSION
      CHARACTER*1 ANS

      INTEGER LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO
      COMMON /INOUT/ LUIN, LUOUT, NINA, NINO, NOUTA, NOUTO

      WRITE (LUOUT,920)
  920 FORMAT (12X,  '                      Welcome', /,
     +        12X,  '                      to  the', /,
     +        12X,  '             National Geodetic Survey', /,
     +        12X,  '  North American Datum Grid Manipulation program.',
     +    //, 12X,  '             For use when NADCON grids', /,
     +        12X,  '               need to be translated', /,
     +        12X,  '             between ASCII and binary', /,
     +        12X,  '                        or', /,
     +        12X,  '    when grids covering a smaller areal extent', /,
     +        12X,  '        need to be extracted from standard', /,
     +        12X,  '                   NADCON grids.', /,
     +        12X,  '                        or', /,
     +        12X,  '             for obtaining information', /,
     +        12X,  '              about the NADCON grids.')

      WRITE (LUOUT,930) VRSION
  930 FORMAT (/, 12X,  '                  (Version', F5.2, ')', /,
     +        12X,  '                   April 1, 1990', /,
     +        12X,  '             Warren T. Dewhurst, Ph.D.', /,
     +        12X,  '            Lieutenant Commander, NOAA', /,
     +        12X,  '                  Alice R. Drew', /,
     +        12X,  '    Senior Geodesist, Horizontal Network Branch',/)

      WRITE (LUOUT,931)
  931 FORMAT (12X,  '             (Hit RETURN to continue.)')

      READ (LUIN,'(A1)') ANS
      WRITE (LUOUT,2)
*   2 FORMAT ('1')
    2 FORMAT ('')

      WRITE (LUOUT,932)
  932 FORMAT ( /, 32X, 'DISCLAIMER' ,//,
     + ' This program and supporting information is furnished by',
     + ' the government of', /,
     + ' the United States of America, and is accepted/used by the',
     + ' recipient with', /,
     + ' the understanding that the U. S. government makes no',
     + ' warranties, express or', /,
     + ' implied, concerning the accuracy, completeness, reliability,',
     + ' or suitability', /,
     + ' of this program, of its constituent parts, or of any',
     + ' supporting data.', //,
     + ' The government of the United States of America shall be',
     + ' under no liability', /,
     + ' whatsoever resulting from any use of this program.',
     + '  This program should', /,
     + ' not be relied upon as the sole basis for solving a problem',
     + ' whose incorrect', /,
     + ' solution could result in injury to person or property.')
        WRITE (LUOUT,933)
  933   FORMAT ( /,
     + ' This program is the property of the government of the',
     + ' United States of', /,
     + ' America. Therefore, the recipient further agrees not to',
     + ' assert proprietary', /,
     + ' rights therein and not to represent this program to anyone as',
     + ' being other', /,
     + ' than a government program.', /)

      WRITE (LUOUT,931)
      READ (LUIN,'(A1)') ANS

      RETURN
      END
