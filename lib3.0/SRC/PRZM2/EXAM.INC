C
C        Include file EXAM.INC 
C
C +++ Date: Wednesday, 7 October 1992.  Time: 10:48:25.
C     Change all INTEGER*2 to *4 - D. Disney
         INTEGER*2  EXMFLG,IYOLD,CNAP2,OFFST
         INTEGER*4  APDEX(NAPP),APMEX(NAPP)
C         INTEGER*2  EXMFLG,IYOLD,APDEX(NAPP),APMEX(NAPP),CNAP2,OFFST
C +++
C
         COMMON /EXMSI/ EXMFLG,IYOLD,CNAP2,OFFST
C
         COMMON /EXMSI1/ APDEX,APMEX
C
