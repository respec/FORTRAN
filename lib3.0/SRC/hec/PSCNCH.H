      PARAMETER (KIB=30,KSCN=12000,KSLOTS=11)
C     PARAMETER (KIB=30,KSCN=8000,KSLOTS=11)
C------ KIB is number of variables in IB needed to define a screen
C------ KSLOTS is the max number of screens that may be in buffer
C------        at any one time
C------ KSCN is size of screen buffer to hold any screens.  Any one
C------      screen may not exceed this value of bytes.
      CHARACTER*1 CSCN(KSCN)
      COMMON/PSCNCH/CSCN
      COMMON/PSCNIN/IB(KIB,KSLOTS),IC(4)
