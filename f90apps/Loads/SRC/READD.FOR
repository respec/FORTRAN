c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////		
      subroutine readd (pname,punit,cmunit,ipcode,nobs,ihead,ldi,
     i                  xdata,thresh,flow,messfg,cmname,
     O                  ERRCOD,MESCOD)
c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////		
c
c     Author:              Tim Cohn & Greg Baier
c     Date:         July 28, 1992
c      modified:    May 28, 1992 (tac & wgb) ** errors corrected **
c      modified:    Jun 30, 1993 (tac & wgb) ** error corrected **
c                          ** (problem when mod(nvars,8)=0) **
c
c     Program:      readd
c     Purpose:      reads the standard PSTAT output files from QWDATA and
c                          interprets it for the estimator92 program.
c
c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////		
c
c            ***** DO NOT MODIFY THIS PROGRAM WITHOUT THE AUTHOR'S PERMISSION *****
c             Tim Cohn, 703/648-5711, tacohn@rvares.er.usgs.gov
c
c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////		

      implicit double precision (a-h,o-z)
c
      dimension ihead(ldi,5),xdata(*),thresh(*),flow(*),
     1          ipvars(1000)
c
      integer      punit,cmunit,ERRCOD,MESSFG,MESCOD
      character*10 adata(1000)
      character*100 fname,pname,cmname

c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////		
c
      ERRCOD = 0
c     get the pstat file name, open the corresponding '.cmnd' file
c
      FNAME = PNAME
      open(unit=PUNIT,file=fname,status='old')
C
C     get valid parameter codes
      CALL GPCODE (CMUNIT,CMNAME,
     O             NVARS,IPVARS)
      IF (NVARS.GT.0) THEN
c       found some parameters to use
        do 71 k=1,nvars
          if(ipvars(k) .eq. ipcode) then
            ichoice       =      k
            goto 72
          endif
71      continue
C       value not found in list
        ERRCOD = 2
72      continue
        IF (ERRCOD.EQ.0) THEN
          do 73 k=1,nvars
            if(ipvars(k) .eq. 61) then
              iflow  =      k
              goto 74
            endif
73        continue
            MESCOD = 5
            IF (MESSFG.EQ.1) THEN
              write(*,*) ' INSTANTANEOUS FLOW NOT AVAILABLE '
            END IF
            iflow  =  0
            flow(1)       =      -99.d0
74        continue

c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c         begin processing the data in the pstat file
c         ihead(i,*) contains the st. id, yr, mo, day and hour, respectively
c
          do 80 i=1,1000000
            read(PUNIT,1000,end=99) (ihead(i,k),k=1,5)
1000        format(i8,t18,i4,2i2,t28,i4)
            do 85 k2=1,nvars,8
              read(PUNIT,'(8a10)') (adata(k),k=k2,min(k2+7,nvars) )
85          continue
c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c           check each observation for remark codes (e.g. no-detects) and overflows
c
            j=ichoice
            do 95 k=1,9
              if( adata(j)(k:k) .eq. '<') then
                read(adata(j)(k+1:10),'(f10.0)',err=95) thresh(i)
                xdata(i)            =      thresh(i)/2.d0
                goto 90
              else if( adata(j)(k:k) .eq. 'E') then
                xdata(i)            =      -99.d0
                thresh(i)           =      -99.d0
                goto 90
              else if( adata(j)(k:k+1) .eq. '- ') then
                xdata(i)            =      -99.d0
                thresh(i)           =      -99.d0
                goto 90
              else if( adata(j)(k:k+1) .eq. '**') then
                xdata(i)            =      1.d308
                thresh(i)           =      -99.d0
                goto 90
              endif
95          continue
c====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c           everything OK--read the data
c
            if(iflow .ne. 0) then
              read(adata(iflow),'(f10.0)',err=101) flow(i)
              goto 102
101           continue
              flow(i)       =      0.d0
            endif
102         continue
c
            read(adata(j),'(f10.0)',err=100) xdata(i)
            thresh(i)     =      -99.d0
            if(xdata(i) .le. 0.d0) then
              MESCOD = 6
              IF (MESSFG.EQ.1) THEN
                write(*,*) ' zero or negative value found in line',i
                write(*,'(i10,i5,2i3,i5,1x,a10)') (ihead(i,k),k=1,5),
     1              adata(j)
              END IF
            endif
            goto 90
100         continue
              MESCOD = 7
              IF (MESSFG.EQ.1) THEN
                write(*,*) ' unrecognized remark code found in line',i
                write(*,'(i10,i5,2i3,i5,1x,a10)') (ihead(i,k),k=1,5),
     1              adata(j)
                write(*,*) ' recoded as missing value'
              END IF
              xdata(i)            =      -99.d0
              thresh(i)           =      -99.d0
90          continue
80        continue
        END IF
c
99      continue
      END IF
      nobs   =      i-1
      close(PUNIT)
      return
      end
C
C
C
      SUBROUTINE   GPCODE
     I                    (CMUNIT,CMNAME,
     O                     NVARS,IPVARS)
C
C     + + + PURPOSE + + +
C     read cmnd file and return valid pcodes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        CMUNIT,NVARS,IPVARS(1000)
      CHARACTER*100  CMNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CMUNIT - cmnd file unit number
C     CMAME  - cmnd file name
C     NVARS  - number of parameters found
C     IPVARS - integer parameter values found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I,K
      CHARACTER*7    QVARS(11)
      CHARACTER*100  TNAME
C
C     + + + END SPECIFICATIONS + + +
C
C     open the corresponding '.cmnd' file
      TNAME = CMNAME
      DO 10 I=100,1,-1
        IF(TNAME(I:I) .NE. ' ') THEN
          OPEN(UNIT=CMUNIT,FILE=TNAME,STATUS='OLD')
          GOTO 30
        END IF
10    CONTINUE
30    CONTINUE
C
      NVARS  =  0
C     read the '.cmnd' file
      READ(CMUNIT,'(/)',END=99)
      DO 50 I=1,10
        READ(CMUNIT,'(11(A7))',END=70) (QVARS(K),K=1,11)
        DO 50 K=1,11
          READ(QVARS(K),'(2X,I5)') IPVARS(NVARS+1)
          IF(IPVARS(NVARS+1) .EQ. 0) GOTO 70
          NVARS = NVARS + 1
50    CONTINUE
70    CONTINUE
      CLOSE (CMUNIT)
99    CONTINUE
C
      RETURN
      END
