      SUBROUTINE MKRAIN
C     RUNOFF BLOCK
C     CALLED BY RHYDRO1
C=======================================================================
C     Read line image rainfall.  Created by R.E.D. July, 1987.
C     Last updated December, 1990.
C     WCH, 8/1/95.  Convert raingage numbers to characters for placement
C       on rainfall interface file.
C     WCH, 8/13/96. Correct the ending time for variable THISTO 
C       intervals.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
CIM  INCREASE HYETOGRAPHS    ~~~~~~~~~~~~~~~~~~~~~~~~
      DIMENSION REIN(MAXRG),TEMP(MAXRG),WTIME(3,50),TOUT(2,5),BMJ(MAXRG)
cim      DIMENSION REIN(50),TEMP(50),WTIME(3,50),TOUT(2,5),BMJ(50)
cim   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER ROPT
C#### WCH, 8/1/95.  ADD CHARACTER VARIABLES FOR RAINFALL STATION ID.
CIM INCREASE HYETOGRAPHS
      CHARACTER RAINID(MAXRG)*8
c initialize RAINID 
      DO I = 1,MAXRG
      WRITE(RAINID(I),'(I8)') I
      ENDDO
cim end
C=======================================================================
      NOUT    = NSCRAT(1)
      MOUT    = NSCRAT(2)
      REWIND NOUT
      REWIND MOUT
cim increase hyetographs    ~~~~~~~~~~~~~~~~~~
      DO 50  J = 1,MAXRG
  50  BMJ(J)  = 0.0
cim   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C=======================================================================
C>>>>>>>> READ DATA GROUP D1 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,ROPT
      IF(ROPT.EQ.1) THEN
                    WRITE(N6,333)
                    RETURN
                    ENDIF
C=======================================================================
C     KTYPE = 0 --> READ USER DEFINED NUMBER OF RAINFALL
C                   VALUES PER LINE. TIME BASED ON THISTO INCREMENTS
C     KTYPE = 1 --> READ USER DEFINED TIME AND RAINFALL PAIRS
C                   PER LINE.  TIME IS EITHER MINUTES OR HOURS FROM
C                   START OF STORM.  RAINFALL IS INTENSITY
C                   FOR THISTO MINUTES.
C     KTYPE = 2 --> READ TIME AND NRGAG RAINFALL VALUES PER LINE
C                   TIME IS EITHER MINUTES OR HOURS FROM START OF STORM
C                   RAINFALL IS INTENSITY FOR THISTO MINUTES
C     NRGAG =       NUMBER OF RAIN GAGES.
C     KTHIS =       VARIABLE RAINFALL INTERVAL
C      KINC =       NUMBER OF RAINFALL VALUES OR
C                   TIME-RAINFALL PAIRS PER LINE
C=======================================================================
C>>>>>>>> READ DATA GROUP E1 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,KTYPE,KINC,KPRINT,KTHIS,KTIME,KPREP,
     +                      NHISTO,THISTO,TZRAIN
      WRITE(N6,370) KTYPE,NHISTO,KINC,KPRINT,KTIME,KPREP,KTHIS,
     +              THISTO,TZRAIN
C=======================================================================
C     CHANGE TZRAIN TO UNITS OF SECONDS
C=======================================================================
      IF(KTIME.EQ.0) TZRAIN = TZRAIN*60.0
      IF(KTIME.EQ.1) TZRAIN = TZRAIN*3600.0
      IF(KTHIS.GT.0.AND.KTYPE.EQ.0) CALL ERROR(19)
C=======================================================================
C>>>>>>>> READ DATA GROUP E2 <<<<<<<<
C=======================================================================
      IF(KTHIS.GT.0) THEN
             READ(N5,*,ERR=888) CC,((WTIME(I,J),I=1,3),J=1,KTHIS)
             WRITE(N6,885)
             WRITE(N6,890) (J,(WTIME(I,J),I=1,3),J=1,KTHIS)
C=======================================================================
C            Change WTIME to seconds
C=======================================================================
             DO 100 K = 1,KTHIS
             DO  99 I = 1,2
             IF(KTIME.EQ.0) WTIME(I,K) = WTIME(I,K) * 60.0   + TZRAIN
  99         IF(KTIME.EQ.1) WTIME(I,K) = WTIME(I,K) * 3600.0 + TZRAIN
             IF(KTIME.EQ.0) WTIME(3,K) = WTIME(3,K) * 60.0
             IF(KTIME.EQ.1) WTIME(3,K) = WTIME(3,K) * 3600.0
 100         CONTINUE
             ENDIF
C=======================================================================
C     Change THISTO to seconds from minutes or hours.
C=======================================================================
      IF(KTIME.EQ.0) THISTO = THISTO *   60.0
      IF(KTIME.EQ.1) THISTO = THISTO * 3600.0
      KLOOP = KINC
      NN    = NRGAG
      IF(KTYPE.EQ.1) KINC = KINC*2
      IF(KTYPE.EQ.2) THEN
                     KLOOP = 1
                     KINC  = 1
                     NN    = 1
                     ENDIF
C=======================================================================
C     Loop thru the raingages.
C=======================================================================
      DO 600 NGAGE = 1,NN
C=======================================================================
C     Read the previously saved raingages.
C=======================================================================
      IF(NGAGE.GT.1.AND.KTYPE.LE.1) THEN
                       REWIND MOUT
                       REWIND NOUT
                       DO 300 K = 1,MRAIN
                       READ(MOUT,END=301,ERR=301) (TEMP(J),J=1,NGAGE)
300                    WRITE(NOUT) (TEMP(J),J=1,NGAGE)
301                    CONTINUE
                       REWIND MOUT
                       REWIND NOUT
                       ENDIF
C=======================================================================
C     LOOP THRU THE RAINFALL VALUES
C=======================================================================
      IF(NGAGE.EQ.1) MRAIN = 0
      DO 500 KK = 1,NHISTO,KLOOP
      K1 = KINC
      IF(KTYPE.EQ.0.AND.KINC+KK.GT.NHISTO)     K1 =    NHISTO - KK + 1
      IF(KTYPE.EQ.1.AND.KINC/2+KK.GT.NHISTO)   K1 =    2*(NHISTO-KK+1)
      IF(KTYPE.EQ.2)                           K1 =    NRGAG       + 1
C=======================================================================
C>>>>>>>> READ DATA GROUP E3 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,(REIN(I),I=1,K1)
C=======================================================================
C     WRITE THE RAINFALL INTERFACE FILE
C=======================================================================
      IF(KTYPE.EQ.0) THEN
               DO 450 INC = 1,K1
               IF(NGAGE.EQ.1) THEN
                        MRAIN = MRAIN + 1
                        TRAIN = THISTO*FLOAT(MRAIN-1) + TZRAIN
                        WRITE(MOUT) TRAIN,REIN(INC)
                        ELSE
                        READ(NOUT)  (TEMP(J),J=1,NGAGE)
                        WRITE(MOUT) (TEMP(J),J=1,NGAGE),REIN(INC)
                        ENDIF
450            CONTINUE
               ENDIF
C=======================================================================
      IF(KTYPE.EQ.1) THEN
             DO 475 INC = 1,K1,2
             MRAIN      = MRAIN + 1
             IF(KTIME.EQ.0) TRAIN = REIN(INC)*60.0   + TZRAIN
             IF(KTIME.EQ.1) TRAIN = REIN(INC)*3600.0 + TZRAIN
             IF(NGAGE.EQ.1) THEN
                         WRITE(MOUT) TRAIN,REIN(INC+1)
                         ELSE
                         READ(NOUT)  (TEMP(J),J=1,NGAGE)
                         WRITE(MOUT) (TEMP(J),J=1,NGAGE),REIN(INC+1)
                         ENDIF
475          CONTINUE
             ENDIF
C=======================================================================
      IF(KTYPE.EQ.2) THEN
                                  MRAIN = MRAIN + 1
                   IF(KTIME.EQ.0) TRAIN = REIN(1)*60.0   + TZRAIN
                   IF(KTIME.EQ.1) TRAIN = REIN(1)*3600.0 + TZRAIN
                   WRITE(MOUT)    TRAIN,(REIN(INC),INC=2,K1)
                   ENDIF
 500  CONTINUE
 600  CONTINUE
C=======================================================================
C     PRINT DEPENDING ON KPRINT
C=======================================================================
      IF(KPRINT.EQ.0) THEN
                      DO 700 KK = 1,NRGAG
                      REWIND MOUT
                      KEND = KK + 1
                      IF(METRIC.EQ.1) THEN
                      IF(KTIME.EQ.0.AND.KPREP.EQ.0) WRITE(N6,810) KK
                      IF(KTIME.EQ.0.AND.KPREP.EQ.1) WRITE(N6,812) KK
                      IF(KTIME.EQ.1.AND.KPREP.EQ.0) WRITE(N6,811) KK
                      IF(KTIME.EQ.1.AND.KPREP.EQ.1) WRITE(N6,813) KK
                      ELSE
                      IF(KTIME.EQ.0.AND.KPREP.EQ.0) WRITE(N6,820) KK
                      IF(KTIME.EQ.0.AND.KPREP.EQ.1) WRITE(N6,822) KK
                      IF(KTIME.EQ.1.AND.KPREP.EQ.0) WRITE(N6,821) KK
                      IF(KTIME.EQ.1.AND.KPREP.EQ.1) WRITE(N6,823) KK
                      ENDIF
                      MM = 0
                      DO 750 JJ = 1,MRAIN
                      READ(MOUT,END=700,ERR=700) (TEMP(J),J=1,KEND)
                      MM         = MM + 1
                      TEMP(1)    = TEMP(1) - TZRAIN
                      IF(KTIME.EQ.0) TOUT(1,MM) = TEMP(1)/60.0
                      IF(KTIME.EQ.1) TOUT(1,MM) = TEMP(1)/3600.0
                      TOUT(2,MM) = TEMP(KEND)
                      IF(MOD(MM,5).EQ.0) THEN
                             WRITE(N6,855) (TOUT(1,J),TOUT(2,J),J=1,5)
                             MM = 0
                             ENDIF
750                   CONTINUE
700                   IF(MM.GT.0) WRITE(N6,855)
     +                            (TOUT(1,J),TOUT(2,J),J=1,MM)
                      ENDIF
C=======================================================================
C     INSERT THISTO VALUES INTO FILE MOUT CREATING FINAL FILE NOUT
C=======================================================================
      REWIND MOUT
      REWIND NOUT
C#### WCH, 8/1/95.  REPLACE INTEGER STATION ID WITH CHARACTER ID.
C####      WRITE(NOUT) NRGAG,MRAIN,(K,K=1,NRGAG)
      WRITE(NOUT) NRGAG,MRAIN,(RAINID(K),K=1,NRGAG)
C
      TLAST    = TZERO
      DO 800 K = 1,MRAIN
      READ(MOUT,END=801,ERR=801) TRAIN,(TEMP(J),J=1,NRGAG)
      IF(KTHIS.EQ.0.AND.K.GT.1.AND.TRAIN-TLAST.LT.THISTO)CALL ERROR(116)
      TLAST    = TRAIN
      IF(KTHIS.GT.0) THEN
               THIS     = THISTO
               DO 850 M = 1,KTHIS
C#### WCH, 8/13/96.  SHOULD BE GE.WTIME(2,M), NOT GT. 
               IF(TRAIN.LT.WTIME(1,M).OR.
     +            TRAIN.GE.WTIME(2,M)) GO TO 850
C####     +            TRAIN.GT.WTIME(2,M)) GO TO 850
               THIS = WTIME(3,M)
               GO TO 851
 850           CONTINUE
 851           CONTINUE
               IF(KPREP.EQ.1) THEN
                              DO 830 J = 1,NRGAG
 830                          TEMP(J)  = TEMP(J)*3600.0/THIS
                              ENDIF
               TRN = TRAIN - TZERO
               CALL NDATE(TRN,JDAY,TMDAY)
               WRITE(NOUT) JDAY,TMDAY,THIS,(TEMP(J),J=1,NRGAG)
               DO 950 J = 1,NRGAG
 950           BMJ(J)   = BMJ(J) + THIS*TEMP(J)/3600.0
               GO TO 800
               ELSE
               IF(KPREP.EQ.1) THEN
                              DO 860 J = 1,NRGAG
 860                          TEMP(J)  = TEMP(J)*3600.0/THISTO
                              ENDIF
               TRN = TRAIN - TZERO
               CALL NDATE(TRN,JDAY,TMDAY)
               WRITE(NOUT) JDAY,TMDAY,THISTO,(TEMP(J),J=1,NRGAG)
               DO 975 J = 1,NRGAG
 975           BMJ(J)   = BMJ(J) + THISTO*TEMP(J)/3600.0
               ENDIF
 800  CONTINUE
 801  CONTINUE
C=======================================================================
C     Write rainfall summary.
C=======================================================================
      WRITE(N6,1050)
      DO 1000 J = 1,NRGAG
      IF(METRIC.EQ.1) WRITE(N6,1100) J,BMJ(J)
 1000 IF(METRIC.EQ.2) WRITE(N6,1200) J,BMJ(J)
      JULDAY = IDATEZ
      TIMDAY = TZERO
      RETURN
 888  CALL IERROR
C=======================================================================
 333  FORMAT(//,
     +' ********************************************************',/,
     +' *   Processed Precipitation will be read on NSCRAT(1)  *',/
     +' ********************************************************')
 370  FORMAT(//,' Rainfall from E3 Data Group ',//,
     *          ' KTYPE  - Rainfall input type..............',I9,/,
     1          ' NHISTO - Total number of rainfall values..',I9,/,
     2          ' KINC   - Rainfall values (pairs) per line.',I9,/,
     3          ' KPRINT - Print rainfall (0-Yes,1-No)......',I9,/,
     3          ' KTIME  - Precipitation time units         ',/,
     3          '    0 --> Minutes  1 --> Hours.............',I9,/,
     3          ' KPREP  - Precipitation unit type          ',/,
     3          '    0 --> Intensity  1 --> Volume..........',I9,/,
     4          ' KTHIS - Variable rainfall intervals       ',/,
     5          '    0 --> No, > 1 --> Yes..................',I9,/,
     6          ' THISTO - Rainfall time interval...........',F9.2,/,
     7          ' TZRAIN - Starting time (KTIME units)......',F9.2)
 810  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(mn)/Rain(in/hr)'))
 811  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(hr)/Rain(in/hr)'))
 812  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(mn)/   Rain(in)'))
 813  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(hr)/   Rain(in)'))
 820  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(mn)/Rain(mm/hr)'))
 821  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(hr)/Rain(mm/hr)'))
 822  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(mn)/   Rain(mm)'))
 823  FORMAT(//,'    Rainfall printout for gage number.... ',I5,/,
     1           1X,5(' Time(hr)/   Rain(mm)'))
 855  FORMAT(1X,5(1F9.2,'/',0PF11.4))
 885  FORMAT(/,
     1' ********************************',/,
     1' *  VARIABLE RAINFALL INTERVALS *',/,
     1' ********************************',//,
     1       ' * * *   START/END/TIME IN MINUTES * * *',/)
 890  FORMAT(I3,'.',' START TIME ',F12.5,
     1 ' END TIME ',F12.5,' TIME INTERVAL ',F10.2)
1050  FORMAT(//,'**************************************',/,
     1          '* Rainfall input summary from Runoff *',/,
     2          '**************************************',/)
1100  FORMAT(' Total rainfall for gage # ',I5,' is ',F9.4,' inches')
1200  FORMAT(' Total rainfall for gage # ',I5,' is ',F9.4,' mm')
C=======================================================================
      END
