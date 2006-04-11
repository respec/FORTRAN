C
C
C
      SUBROUTINE   INWSEG
     I                    (WDMSFL)
C
C     + + + PURPOSE + + +
C     initialize wdm data file unit number for map segments
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - unit number of file containing time series data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      FILUNT = WDMSFL
C
      RETURN
      END
C
C
C
      SUBROUTINE   INISEG
     I                    (CURA,NUMSEG,AT1,AT2,COLOR1,COLOR2,COLOR3,
     I                     DNUM,RNUM1,RNUM2,RNUM3,RNUM4,RNUM5)
C
C     + + + PURPOSE + + +
C     initialize line segments on map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURA,NUMSEG,AT1(NUMSEG),AT2(NUMSEG),COLOR1(NUMSEG),
     1          COLOR2(NUMSEG),COLOR3(NUMSEG),DNUM(NUMSEG)
      REAL      RNUM1(NUMSEG),RNUM2(NUMSEG),
     1          RNUM3(NUMSEG),RNUM4(NUMSEG),RNUM5(NUMSEG)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURA   - which map animation to initialize
C     NUMSEG - how many sets to initialize
C     AT1    - major attribute
C     AT2    - minor attribute
C     COLOR1 - first color for this line segment
C     COLOR2 - second color for this line segment
C     COLOR3 - third color for this line segment
C     DNUM   - data set number for this line segment specs
C     RNUM1  - minimum value for this line segment specs
C     RNUM2  - maximum value for this line segment specs
C     RNUM3  - first width of this line segment
C     RNUM4  - second width of this line segment
C     RNUM5  - third width of this line segment
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      NSEG(CURA) = NUMSEG
      DO 10 I = 1,NSEG(CURA)
        ATMAJ(CURA,I) = AT1(I)
        ATMIN(CURA,I) = AT2(I)
        CLR1(CURA,I)  = COLOR1(I)
        CLR2(CURA,I)  = COLOR2(I)
        CLR3(CURA,I)  = COLOR3(I)
        DSN(CURA,I)   = DNUM(I)
        RMIN(CURA,I)  = RNUM1(I)
        RMAX(CURA,I)  = RNUM2(I)
        WID1(CURA,I)  = RNUM3(I)
        WID2(CURA,I)  = RNUM4(I)
        WID3(CURA,I)  = RNUM5(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MODSEG
     I                   (CURA,I,AT1,AT2,COLOR1,COLOR2,COLOR3,MIN,MAX,
     I                    DSET,WIDTH1,WIDTH2,WIDTH3)
C
C     + + + PURPOSE + + +
C     modify line segment specs
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURA,I,AT1,AT2,COLOR1,COLOR2,COLOR3,DSET
      REAL      MIN,MAX,WIDTH1,WIDTH2,WIDTH3
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURA   - which animation
C     I      - which set of specs to modify
C     AT1    - major attribute
C     AT2    - minor attribute
C     COLOR1 - first color for this line segment
C     COLOR2 - second color for this line segment
C     COLOR3 - third color for this line segment
C     DSET   - data set number for this line segment specs
C     MIN    - minimum value for this line segment specs
C     MAX    - maximum value for this line segment specs
C     WIDTH1 - first width of line segment
C     WIDTH2 - second width of line segment
C     WIDTH3 - third width of line segment
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      ATMAJ(CURA,I) = AT1
      ATMIN(CURA,I) = AT2
      CLR1(CURA,I)  = COLOR1
      CLR2(CURA,I)  = COLOR2
      CLR3(CURA,I)  = COLOR3
      DSN(CURA,I)   = DSET
      RMIN(CURA,I)  = MIN
      RMAX(CURA,I)  = MAX
      WID1(CURA,I)  = WIDTH1
      WID2(CURA,I)  = WIDTH2
      WID3(CURA,I)  = WIDTH3
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETSEG
     I                    (CURA,I,
     O                     AT1,AT2,COLOR1,COLOR2,COLOR3,MIN,MAX,DSET,
     O                     WIDTH1,WIDTH2,WIDTH3)
C
C     + + + PURPOSE + + +
C     get specs for a line segment to be drawn on map
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURA,I,AT1,AT2,COLOR1,COLOR2,COLOR3,DSET
      REAL      MIN,MAX,WIDTH1,WIDTH2,WIDTH3
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURA   - which animation
C     I      - which set of specs to get
C     AT1    - major attribute
C     AT2    - minor attribute
C     COLOR1 - first color for this line segment
C     COLOR2 - second color for this line segment
C     COLOR3 - third color for this line segment
C     DSET   - data set number for this line segment specs
C     MIN    - minimum value for this line segment specs
C     MAX    - maximum value for this line segment specs
C     WIDTH1 - first width of line segment
C     WIDTH2 - second width of line segment
C     WIDTH3 - third width of line segment
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      AT1   = ATMAJ(CURA,I)
      AT2   = ATMIN(CURA,I)
      COLOR1= CLR1(CURA,I)
      COLOR2= CLR2(CURA,I)
      COLOR3= CLR3(CURA,I)
      DSET  = DSN(CURA,I)
      MIN   = RMIN(CURA,I)
      MAX   = RMAX(CURA,I)
      WIDTH1= WID1(CURA,I)
      WIDTH2= WID2(CURA,I)
      WIDTH3= WID3(CURA,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTNSEG
     I                    (CURA,
     O                     NUMSEG)
C
C     + + + PURPOSE + + +
C     get number of specs for a line segments
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMSEG,CURA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURA   - animation number
C     NUMSEG - number of line segment specs
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NUMSEG = NSEG(CURA)
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPSEG
     I                    (CURA,SEGBID,
     O                     SEGID)
C
C     + + + PURPOSE + + +
C     add a new line specs set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SEGBID,SEGID,CURA
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CURA   - which animation
C     SEGBID - spec set base id - default values for new spec set
C     SEGID  - identifier of new specs
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      SEGID = NSEG(CURA)+ 1
      IF (SEGID .LE. MXSEG) THEN
C       room for a new set
        IF (SEGBID.GT.0 .AND. SEGBID.LE.NSEG(CURA)) THEN
C         use base for set defaults
          ATMAJ(CURA,SEGID)= ATMAJ(CURA,SEGBID)
          ATMIN(CURA,SEGID)= ATMIN(CURA,SEGBID)
          CLR1(CURA,SEGID) = CLR1(CURA,SEGBID)
          CLR2(CURA,SEGID) = CLR2(CURA,SEGBID)
          CLR3(CURA,SEGID) = CLR3(CURA,SEGBID)
          DSN(CURA,SEGID)  = DSN(CURA,SEGBID)
          RMIN(CURA,SEGID) = RMIN(CURA,SEGBID)
          RMAX(CURA,SEGID) = RMAX(CURA,SEGBID)
          WID1(CURA,SEGID) = WID1(CURA,SEGBID)
          WID2(CURA,SEGID) = WID2(CURA,SEGBID)
          WID3(CURA,SEGID) = WID3(CURA,SEGBID)
        ELSE
C         general defaults
          ATMAJ(CURA,SEGID)= 0
          ATMIN(CURA,SEGID)= 0
          CLR1(CURA,SEGID) = 0
          CLR2(CURA,SEGID) = 0
          CLR3(CURA,SEGID) = 0
          DSN(CURA,SEGID)  = 0
          RMIN(CURA,SEGID) = 0.0
          RMAX(CURA,SEGID) = 0.0
          WID1(CURA,SEGID) = 1.0
          WID2(CURA,SEGID) = 1.0
          WID3(CURA,SEGID) = 1.0
        END IF
C       update number
        NSEG(CURA) = SEGID
      ELSE
C       no room
        SEGID= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELSEG
     I                   (CURA,SEGDID)
C
C     + + + PURPOSE + + +
C     delete a line spec set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CURA,SEGDID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CURA   - current animation
C     SEGDID - spec set id to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SEGDID.GT.0 .AND. SEGDID.LE.NSEG(CURA)) THEN
C       the set exists to delete
        IF (SEGDID.LT.NSEG(CURA)) THEN
C         move later sets
          DO 10 ID= SEGDID,NSEG(CURA)-1
            ATMAJ(CURA,ID)= ATMAJ(CURA,ID+1)
            ATMIN(CURA,ID)= ATMIN(CURA,ID+1)
            CLR1(CURA,ID) = CLR1(CURA,ID+1)
            CLR2(CURA,ID) = CLR2(CURA,ID+1)
            CLR3(CURA,ID) = CLR3(CURA,ID+1)
            DSN(CURA,ID)  = DSN(CURA,ID+1)
            RMIN(CURA,ID) = RMIN(CURA,ID+1)
            RMAX(CURA,ID) = RMAX(CURA,ID+1)
            WID1(CURA,ID) = WID1(CURA,ID+1)
            WID2(CURA,ID) = WID2(CURA,ID+1)
            WID3(CURA,ID) = WID3(CURA,ID+1)
 10       CONTINUE
        END IF
C       update number of sets
        NSEG(CURA)= NSEG(CURA)- 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STNANI
     I                    (NUMANI)
C
C     + + + PURPOSE + + +
C     set number of animations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMANI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMANI - number of animations
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NUMANI.GT.MXANI) THEN
        NANI = MXANI
      ELSE
        NANI = NUMANI
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTNANI
     O                    (NUMANI)
C
C     + + + PURPOSE + + +
C     get number of animations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMANI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMANI - number of animations
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NUMANI = NANI
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTCURA
     O                    (CANI)
C
C     + + + PURPOSE + + +
C     get current animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CANI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CANI   - current active animation number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CANI = CURANI
C
      RETURN
      END
C
C
C
      SUBROUTINE   STCURA
     I                    (CANI)
C
C     + + + PURPOSE + + +
C     set current animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CANI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CANI   - current active animation number
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CURANI = CANI
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETAN1
     I                    (ANIM,
     O                     NAME)
C
C     + + + PURPOSE + + +
C     get name of one animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ANIM
      CHARACTER*20  NAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ANIM   - number of animation
C     NAME   - animation name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      NAME = ANINAM(ANIM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETANI
     I                    (NUM,
     O                     NAMES)
C
C     + + + PURPOSE + + +
C     get names of animations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NUM
      CHARACTER*20  NAMES(NUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUM    - number of names to get
C     NAMES  - array of animation names
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,NUM
        NAMES(I) = ANINAM(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELANI
     I                   (ANIMID)
C
C     + + + PURPOSE + + +
C     delete an animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ANIMID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ANIMID - animation to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID,I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ANIMID.GT.0 .AND. ANIMID.LE.NANI) THEN
C       the animation exists to delete
        IF (ANIMID.LT.NANI) THEN
C         move later animations
          DO 10 ID= ANIMID,NANI-1
            DO 20 I= 1,NSEG(ID)
              ATMAJ(ID,I)= ATMAJ(ID+1,I)
              ATMIN(ID,I)= ATMIN(ID+1,I)
              CLR1(ID,I) = CLR1(ID+1,I)
              CLR2(ID,I) = CLR2(ID+1,I)
              CLR3(ID,I) = CLR3(ID+1,I)
              DSN(ID,I)  = DSN(ID+1,I)
              RMIN(ID,I) = RMIN(ID+1,I)
              RMAX(ID,I) = RMAX(ID+1,I)
              WID1(ID,I) = WID1(ID+1,I)
              WID2(ID,I) = WID2(ID+1,I)
              WID3(ID,I) = WID3(ID+1,I)
  20        CONTINUE
            NSEG(ID)  = NSEG(ID+1)
            ANINAM(ID)= ANINAM(ID+1)
 10       CONTINUE
        END IF
C       update number of animations
        NANI= NANI- 1
      END IF
      IF (CURANI.EQ.ANIMID) THEN
C       this animation cannot be current anymore
        CURANI = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPANI
     I                    (ANIMID)
C
C     + + + PURPOSE + + +
C     add a new animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ANIMID
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ANIMID - which animation to copy
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  NEWID,I
C
C     + + + END SPECIFICATIONS + + +
C
      NEWID = NANI+ 1
      IF (NEWID .LE. MXANI) THEN
C       room for a new animation
        DO 20 I= 1,NSEG(ANIMID)
          ATMAJ(NEWID,I)= ATMAJ(ANIMID,I)
          ATMIN(NEWID,I)= ATMIN(ANIMID,I)
          CLR1(NEWID,I) = CLR1(ANIMID,I)
          CLR2(NEWID,I) = CLR2(ANIMID,I)
          CLR3(NEWID,I) = CLR3(ANIMID,I)
          DSN(NEWID,I)  = DSN(ANIMID,I)
          RMIN(NEWID,I) = RMIN(ANIMID,I)
          RMAX(NEWID,I) = RMAX(ANIMID,I)
          WID1(NEWID,I) = WID1(ANIMID,I)
          WID2(NEWID,I) = WID2(ANIMID,I)
          WID3(NEWID,I) = WID3(ANIMID,I)
 20     CONTINUE
C       update number
        NSEG(NEWID)  = NSEG(ANIMID)
        ANINAM(NEWID)= '<unnamed animation> '
        NANI = NEWID
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MODANI
     I                    (ID,NAME)
C
C     + + + PURPOSE + + +
C     modify a map animation name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ID
      CHARACTER*20  NAME
C
C     + + + ARGUMENT DEFINTIONS + + +
C     ID     - animation id
C     NAME   - animation name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pmxseg.inc'
      INCLUDE 'pmxani.inc'
      INCLUDE 'cmpseg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ID .LE. MXANI) THEN
C       valid animation
        ANINAM(ID)= NAME
      END IF
C
      RETURN
      END
