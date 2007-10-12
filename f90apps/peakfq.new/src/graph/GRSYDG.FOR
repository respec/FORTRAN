C
C
C
      SUBROUTINE   ANGTXA
     I                    (XPOS,YPOS,LEN,STR,HV)
C
C     + + + PURPOSE + + +
C     Routine to write out text strings for graphics
C     for DG AVIION systems USING PRIOR GKS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN, HV
      REAL        XPOS,YPOS
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XPOS   - x-coordinate of position to start writing string
C     YPOS   - y-coordinate of position to start writing string
C     LEN    - length of character*1 array
C     STR    - character*1 array for graphics output
C     HV     - 1-horizontal lettering,   2- vertical lettering
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       ERRIND,WSID, N, OL
      INTEGER       I,LLEN
      REAL          LXPOS,LYPOS,NXPOS,NYPOS,TXEXPX(4),TXEXPY(4)
      CHARACTER*120 ONESTR
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
C     (none)
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, GTX, GQTXX, GQACWK, CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
C     For DG AVIION systems USING PRIOR GKS
C
C     get workstation id
      N = 1
      CALL GQACWK (N,ERRIND,OL,WSID)
C
C     initialize local positions
      LXPOS= XPOS
      LYPOS= YPOS
C     get length of character string
      LLEN = LENSTR (LEN,STR)
C     write out entire string at one time for easier editing in a 
C     word processor
      ONESTR = ''
      CALL CARVAR (LEN,STR,LEN,ONESTR)
      CALL GTX (XPOS,YPOS,ONESTR)
C     write out text one character at a time
C     DO 8 I= 1,LLEN
C       CALL GTX (LXPOS,LYPOS,STR(I))
C       get next characters coordinates
C       CALL GQTXX (WSID,LXPOS,LYPOS,STR(I),
C    O              ERRIND,NXPOS,NYPOS,TXEXPX,TXEXPY)
C       LXPOS = NXPOS
C       LYPOS = NYPOS
C8    CONTINUE
C
      RETURN
      END
C
C
C
      REAL FUNCTION   ANGLEN
     I                       (LEN,CBUF)
C
C     + + + PURPOSE + + +
C     This routine computes the length of a character string in world
C     corrdinates using GKS routines. For CGM as a device special C
C     routines developed by Fulton are used.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN
      CHARACTER*1  CBUF(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of character string
C     CBUF   - character string to be plotted
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, N, OL
      INTEGER   WSID, ERRIND, DEVCOD
      REAL      LXP,LYP,NXPOS,NYPOS,TXEXPX(4),TXEXPY(4),SIZEL
C
C     + + + EXTERNALS + + +
      EXTERNAL   GQTXX, GQACWK, GGDVCD, GGLSIZ
C
C     + + + END SPECIFICATIONS + + +
C
C     get workstation id
      N = 1
      CALL GQACWK (N,ERRIND,OL,WSID)
C
      LYP = 0.0
      LXP = 0.0
      ANGLEN = 0.0
C     WRITE(FE,*) ' Before and after concatenation points, x then y.'
C
      CALL GGDVCD (DEVCOD)
      IF (DEVCOD .GE. 24 .AND. DEVCOD .LE. 26) THEN
C       CGM meta file so use spacing from Fultons routine
C       for Helvetica
        CALL GGLSIZ (SIZEL)
        CALL PSFTSZ (SIZEL, LEN, CBUF, ANGLEN)
      ELSE
C       for all other devices when GKS knows the spacing  
        DO 10 I = 1,LEN
          CALL GQTXX (WSID, LXP,LYP, CBUF(I),
     O                ERRIND,NXPOS,NYPOS,TXEXPX,TXEXPY)
C         WRITE(FE,*) LXP,NXPOS,LYP,NYPOS
C 
          IF (NXPOS-LXP .GT. NYPOS-LYP) THEN
C           horizontal text
            ANGLEN = NXPOS - LXP + ANGLEN
            LXP = NXPOS
          ELSE
C           vertical text
            ANGLEN = NYPOS - LYP + ANGLEN
            LYP = NYPOS
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMETA
     M                     (RETCOD)
C
C     + + + PURPOSE + + +
C     This routine does special processing when a DISSPLA meta file
C     is to be the graphics output on the PRIME computer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RETCOD - error code from opening workstation (0-all ok)
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRGT
     O                    (GPAGE,GMODE)
C
C     + + + PURPOSE + + +
C     dummy routine to emulate getting
C     the graphics mode on the PC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPAGE,GMODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPAGE - graphics page
C     GMODE - graphics mode
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRST
     I                    (GPAGE,GMODE)
C
C     + + + PURPOSE + + +
C     dummy routine to emulate setting
C     the graphics mode on the PC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPAGE,GMODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPAGE - graphics page
C     GMODE - graphics mode
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSFASX
     M                   (PATTRN)
C
C     + + + PURPOSE + + +
C     set pattern after checking for conversion in value assoc with name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PATTRN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PATTRN - pattern id number
C
C     + + + END SPECIFICATIONS + + +
C
C     to be replaced summer '94,
C     temp hard code conversions of fill patterns
      IF (PATTRN.EQ.3) THEN
        PATTRN = -1
      ELSE IF (PATTRN.EQ.4) THEN
        PATTRN = -2
      ELSE IF (PATTRN.EQ.5) THEN
        PATTRN = -3
      ELSE IF (PATTRN.EQ.6) THEN
        PATTRN = -4
      ELSE IF (PATTRN.EQ.7) THEN
        PATTRN = -5
      ELSE IF (PATTRN.EQ.8) THEN
        PATTRN = -6
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE PSFTSZ
     I                  (HEIGHT, NCHAR, STR,
     O                   LENGTH)          
C
C     + + + PURPOSE + + +

C     Postscript font size tables.  Obtained by querying string 
C     sizes in Gostscript.  These tables give approximate widths 
C     of postscript characters, as percentage of character height. 
C     Routine computes total length for character string based on 
C     number and type of characters, and character height.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NCHAR
      REAL        HEIGHT, LENGTH
      CHARACTER*1 STR(NCHAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HEIGHT - character height
C     NCHAR  - number of characters in the string
C     STR    - character string to be placed in graphic output
C     LENGTH - length of character string in same units as height
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J
      REAL      RATIO(95)
C
C     + + + INTRINSICS + + +
      INTRINSIC ICHAR
C
C     + + + DATA INITIALIZATIONS + + + 
C     static unsigned char Courier[] = {
C      62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62};
C     static unsigned char CourierBold[] = {
C      60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60};
C     static unsigned char CourierOblique[] = {
C      62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62
C     ,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62};
C     static unsigned char CourierBoldOblique[] = {
C      60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60
C     ,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60};
C     static unsigned char TimesRoman[] = {
C      27,16,0,54,39,67,71,19,30,30,35,49,19,29,19,39,48,41,46,46
C     ,48,48,46,46,45,46,19,19,0,49,0,38,52,71,54,60,65,54,52,62
C     ,67,28,38,67,57,76,60,62,49,62,59,46,59,67,71,92,65,65,57,22
C     ,41,22,0,0,19,41,46,38,48,38,39,43,52,27,27,54,30,78,52,41
C     ,48,48,38,33,33,52,54,82,48,54,39,30,0,30,0};
C     static unsigned char TimesBold[] = {
C      30,20,0,59,43,71,76,22,30,30,39,48,22,33,22,43,51,43,51,48
C     ,49,49,48,48,46,48,22,22,0,52,0,41,57,71,57,62,70,59,57,67
C     ,71,33,43,70,60,91,73,67,57,70,62,52,65,72,73,97,73,72,62,30
C     ,43,30,0,0,24,51,52,43,54,43,46,48,59,33,30,59,33,84,59,48
C     ,52,54,46,38,40,54,59,83,54,61,43,38,0,38,0};
C     static unsigned char TimesItalic[] = {
C      24,27,0,54,43,71,65,16,27,27,35,49,16,24,16,39,48,43,48,46
C     ,48,48,48,48,48,48,22,22,0,49,0,33,52,72,63,60,71,67,70,65
C     ,86,49,54,80,63,95,82,63,63,60,63,52,60,70,70,91,76,65,60,38
C     ,40,39,0,0,16,46,43,41,52,39,60,49,52,24,39,49,27,71,52,41
C     ,54,43,41,39,30,46,46,65,49,48,43,41,0,39,0};
C     static unsigned char TimesBoldItalic[] = {
C      22,34,49,44,44,75,70,29,29,29,44,51,22,29,22,24,44,44,44,44
C     ,44,44,44,44,44,44,29,29,51,51,51,44,75,60,60,60,65,60,60,65
C     ,70,34,44,60,54,80,65,65,54,65,60,50,54,65,60,80,60,54,54,29
C     ,24,29,51,44,29,44,44,39,44,39,29,44,50,24,24,44,24,70,50,44
C     ,44,44,34,34,24,50,39,60,44,39,34,31,19,31,51};
C     static unsigned char Helvetica[] = {
      DATA RATIO/
     1 24,24,31,48,48,77,58,19,29,29,34,51,24,29,24,24,48,48,48,48
     1,48,48,48,48,48,48,24,24,51,51,51,48,88,58,58,63,63,58,53,68
     1,63,24,43,58,48,73,63,68,58,68,63,58,53,63,58,82,58,58,53,24
     1,24,24,41,48,19,48,48,43,48,48,24,48,48,19,19,43,19,73,48,48
     1,48,48,29,43,24,48,43,63,43,43,43,29,22,29,51/
C     static unsigned char HelveticaBold[] = {
C      24,29,41,48,48,77,63,24,29,29,34,51,24,29,24,24,48,48,48,48
C     ,48,48,48,48,48,48,29,29,51,51,51,53,85,63,63,63,63,58,53,68
C     ,63,24,48,63,53,73,63,68,58,68,63,58,53,63,58,82,58,58,53,29
C     ,24,29,51,48,24,48,53,48,53,48,29,53,53,24,24,48,24,77,53,53
C     ,53,53,34,48,29,53,48,68,48,48,43,34,24,34,51};
C     static unsigned char HelveticaOblique[] = {
C      24,24,31,48,48,77,58,19,29,29,34,51,24,29,24,24,48,48,48,48
C     ,48,48,48,48,48,48,24,24,51,51,51,48,88,58,58,63,63,58,53,68
C     ,63,24,43,58,48,73,63,68,58,68,63,58,53,63,58,82,58,58,53,24
C     ,24,24,41,48,19,48,48,43,48,48,24,48,48,19,19,43,19,73,48,48
C     ,48,48,29,43,24,48,43,63,43,43,43,29,22,29,51};
C     static unsigned char HelveticaBoldOblique[] = {
C      24,29,41,48,48,77,63,24,29,29,34,51,24,29,24,24,48,48,48,48
C     ,48,48,48,48,48,48,29,29,51,51,51,53,85,63,63,63,63,58,53,68
C     ,63,24,48,63,53,73,63,68,58,68,63,58,53,63,58,82,58,58,53,29
C     ,24,29,51,48,24,48,53,48,53,48,29,53,53,24,24,48,24,77,53,53
C     ,53,53,34,48,29,53,48,68,48,48,43,34,24,34,51};
C     static unsigned char Symbol[] = {
C      24,32,69,48,53,81,76,42,32,32,48,53,24,53,24,27,48,48,48,48
C     ,48,48,48,48,48,48,27,27,53,53,53,43,53,70,65,70,59,59,74,59
C     ,70,32,61,70,67,87,70,70,75,72,54,57,59,67,42,75,63,77,59,32
C     ,84,32,64,48,48,61,53,53,48,42,51,40,59,32,59,53,53,56,51,53
C     ,53,51,53,59,42,56,69,67,48,67,48,47,19,47,53};
C
C     static unsigned char *FontTables[] =  
C     Courier, CourierBold, CourierOblique, CourierBoldOblique,
C     TimesRoman, TimesBold, TimesItalic, TimesBoldItalic, Helvetica,
C     HelveticaBold, HelveticaOblique, HelveticaBoldOblique, Symbol 
C
C     static double
C     postscriptStringLengthHack(char *s)
C     {
C     unsigned char *fontTable;
C     Gchrht l = 0, h; 
C     fontTable = FontTables[-2800 - current_font];
C     h = current_character_height * 1.5 / 100.0; 
C     while(*s)
C     {
C         if(*s >= ' ' && *s < 127)
C   	l += h * (fontTable[*s - ' '] + 2);
C         else
C   	l += current_character_height;
C         s++;
C       }
C     return l;
C      }	 
C     
C     + + + END SPECIFICATIONS + + +
C
      LENGTH = 0.0
C   
      DO 20 I = 1,NCHAR
        J = ICHAR(STR(I)) -32 +1
        IF (J .GE. 1 .AND. J .LE. 95) THEN
C         one of the standard characters
          LENGTH = LENGTH + HEIGHT*RATIO(J)/100.0
        ELSE
C         not a standard character so add no length
        END IF
 20   CONTINUE
C
      RETURN
      END    
