*  $Header: /nwiscvs/watstore/geod/src/vertcon_lib/vround.f,v 1.1 1998/07/08 22:13:12 grogers Exp $
*  vround.f
*
      REAL FUNCTION vROUND(VALUE)
*
*  Purpose: Rounding routine for VERTCON
****************************************
*
*  $Log: vround.f,v $
*  Revision 1.1  1998/07/08 22:13:12  grogers
*  PR#0, chgd name of round to vround
*
*
       REAL VALUE
       INTEGER INTHT
       INTHT=(SIGN(1.0,VALUE)*(ABS(VALUE)+0.0005))*1000.
       vROUND=INTHT*0.001
      RETURN
      END
