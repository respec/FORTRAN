CIMT  change made by C. Moore CDM to increase number of constituents in 
CIMT  tranport from 4 to MQUAL - maximum number of quality components in 
CIMT  transport and all other blocks.
CIMT  change dimensions on PFILE from 10 to MQUAL
CIMT CIMT PF1 and PF2 should be dimensioned to MQAUL.
CIMT see correction in TRANS at loop for line number 9310 that was correct 
CIMT fix for this out of bounds error.
C**** INCREASE ARRAYS PF1,PF2 FROM 4 TO 7 TO PREVENT ARRAYS OUT OF BOUNDS ERROR
C****         JANUARY 1992      LISA BENSON, CSC
CIMT
      COMMON/NEW81A/KORDER(NTHR),VAR(6),RATIO(6),DEF(6),DECAY(MQUAL),
     1SPG(MQUAL),PSIZE(5,MQUAL),PGR(5,MQUAL),QE1(NTHR),QE2(NTHR),
     2PE1(MQUAL,NTHR)
      COMMON/NEW81B/PE2(MQUAL,NTHR),QF1(NTHI),QF2(NTHI),
     1             PF1(MQUAL,NTHI),PF2(MQUAL,NTHI),JN(NTHO),CPPP(MQUAL),
     2             METRIC,IPOLX(MQUAL),GNU,DS(NET,MQUAL),DB(NET,MQUAL),
     3             POLDWF(MQUAL),CPINF(MQUAL),PSDWF(MQUAL)
