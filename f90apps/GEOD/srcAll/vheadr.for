*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vheadr.f,v 1.1 1998/07/07 20:10:35 grogers Exp $
*  vheadr.f
*
      SUBROUTINE vHEADR(VRSION)
*
*  Purpose: Prints the header information and the disclaimer
************************************************************
*
*  $Log: vheadr.f,v $
*  Revision 1.1  1998/07/07 20:10:35  grogers
*  PR#0, initial add of vertcon_lib
*
*
      CHARACTER*1 ANS
      COMMON /vINOUT/ LUIN, LUOUT, NOUT, NIN, LDUMP, NSPACE(2)
        WRITE (LUOUT,1)
    1   FORMAT ( /,15x,
     + ' National Geodetic Survey Program VERTCON'/25x,
     + ' (VERTical CONversion)'//15x,
     + ' For use when needing to convert between:'/10x,
     + ' National Geodetic Vertical Datum of 1929 (NGVD 29)'/30x,
     + ' and'/,12x,' North American Vertical Datum of 1988',
     + ' (NAVD 88)'/)
        WRITE (LUOUT,2) VRSION
    2   FORMAT ( /24x,'Dennis G. Milbert, Ph.D.'/
     +   27x,'David B. Zilkoski'//28x,' (Version',f4.1,')'/
     +   28x,'September 1994'/)
      WRITE (LUOUT,933)
      READ (LUIN,'(A1)') ANS
      WRITE (LUOUT,930)
  930 FORMAT (//31x'DISCLAIMER', //,
     + ' This program and supporting information is furnished by',
     + ' the government of', /,
     + ' the United States of America, and is accepted/used by the',
     + ' recipient with', /,
     + ' the understanding that the U. S. government makes no',
     + ' warranties, express or', /,
     + ' implied, concerning the accuracy, completeness, reliability,',
     + ' or suitability', /,
     + ' of this program, of its constituent parts, or of any',
     + ' supporting data.')
        WRITE (LUOUT,931)
  931   FORMAT (/' The',
     + ' government of the United States of America shall be',
     + ' under no liability', /,
     + ' whatsoever resulting from any use of this program.',
     + '  This program should', /,
     + ' not be relied upon as the sole basis for solving a problem',
     + ' whose incorrect', /,
     + ' solution could result in injury to person or property.')
        WRITE (LUOUT,932)
  932   FORMAT ( /,
     + ' This program is the property of the government of the',
     + ' United States of', /,
     + ' America. Therefore, the recipient further agrees not to',
     + ' assert proprietary', /,
     + ' rights therein and not to represent this program to anyone as',
     + ' being other', /,
     + ' than a government program.', /)
      WRITE (LUOUT,933)
  933 FORMAT(23x,'(Hit RETURN to continue)')
      READ (LUIN,'(A1)') ANS
c     WRITE (LUOUT,3)
c   3 FORMAT ('')
      RETURN
      END
