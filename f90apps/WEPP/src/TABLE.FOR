      subroutine table(flag,length,colmn1,colmn2,given,found)
c
c     + + + PURPOSE + + +
c
c     SR TABLE inserts an additional value into the first
c     column of an ordered, paired data set and uses linear
c     interpolation to compute the corresponding value of the
c     column
c
c     Called from: SRS CHNCON, HYDCHN, DCAP, AND OTHERS
c     Author(s): CREAMS file, C. Baffaut
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer length, flag
      real colmn1(length), colmn2(length), given, found, mxfound
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     colmn1(length) -
c     colmn2(length) -
c     given          -
c     found          -
c     length         -
c     flag           -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real intrpl, tval
      integer i, npos, j, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     intrpl -
c     tval     -
c     i      -
c     npos   -
c     j      -
c     k      -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     intrpl
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (flag.ne.2) then
        if (flag.eq.3) go to 60
        if (flag.eq.4) go to 80
c
        do 10 i = 1, length
          npos = i
          if (colmn1(i).lt.given) go to 100
   10   continue
c
        go to 120
      end if
c
      do 20 i = 1, length
        npos = i
c
        if (colmn1(i).gt.given) then
          if (npos.eq.1) go to 120
          mxfound = intrpl(colmn1(npos-1),colmn2(npos-1),colmn1(npos),
     1        colmn2(npos),given)
c         exit
          go to 30
        end if
c
   20 continue
c
   30 do 50 k = 2, length
        tval = colmn1(k)
c
        do 40 j = npos, length
c
          if (colmn1(j)-tval.gt.given) then
            found = intrpl(colmn1(j-1),colmn2(j-1),colmn1(j),colmn2(j),
     1          given+tval) - colmn2(k)
            if (found.gt.mxfound) mxfound = found
            npos = j
c           exit
            go to 50
          end if
c
   40   continue
c
   50 continue
c
      found = mxfound
      return
c
   60 continue
c
      do 70 i = 1, length
        npos = i
        if (colmn2(i).lt.given) go to 110
   70 continue
c
      go to 120
c
   80 continue
c
      do 90 i = 1, length
        npos = i
        if (colmn2(i).gt.given) go to 110
   90 continue
c
      go to 120
c
  100 continue
c
      if (npos.eq.1) go to 120
      found = intrpl(colmn1(npos-1),colmn2(npos-1),colmn1(npos),
     1    colmn2(npos),given)
      return
c
  110 continue
c
      if (npos.ne.1) then
        found = intrpl(colmn2(npos-1),colmn1(npos-1),colmn2(npos),
     1      colmn1(npos),given)
        return
      end if
c
  120 continue
c
      write (38,1000) flag, given, colmn1(3), colmn2(3)
      write (6,1000) flag, given, colmn1(3), colmn2(3)
c
      stop
 1000 format (' ',/,12x,'**********ERROR**********',/,12x,
     1    'GIVEN is outside the range of the table',//,1x,
     1    'use this info to identify the function where',
     1    ' the problem occured',//,4x,
     1    'flag: 1 - given column 1 find column 2 ',
     1    '(column 1 decreases)',/,4x,
     1    '      2 - given column 1 find column 2 ',
     1    '(column 1 increases)',//,4x,
     1    '      3 - given column 2 find column 1 ',
     1    '(column 2 decreases)',/,4x,
     1    '      4 - given column 2 find column 1 ',
     1    '(column 2 increases)',//,12x,'the flag                  = ',
     1    i2,/,12x,'given value               = ',e10.3,/,12x,
     1    'third value from column 1 = ',e10.3,/,12x,
     1    'third value from column 2 = ',e10.3)
      end
