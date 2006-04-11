      BLOCK DATA STATA
C=======================================================================
C     THIS IS A 'BLOCK DATA' SUBPROGRAM UTILIZED BY THE STATS BLOCK.
C     IT IS NOT CALLED FROM ANY BLOCK OR SUBROUTINE BUT IS USED ONLY
C     TO INITIALIZE DATA INTO LABELED COMMON BLOCKS.  IN PARTICULAR,
C     IT INITIALIZES LABELS AND MESSAGES USED IN PRINTING TABLES,
C     GRAPHS AND OTHER OUTPUT.
C=======================================================================
      INCLUDE 'STCOM.INC'
C=======================================================================
      DATA ENGLAB/'(inches)','(in/hr) ',
     1            '(in/hr) ','(hours) ',
     2            '(hours) ','(pounds)',
     4            '(lbs/hr)','(lbs/hr)','(mg/l)  ',
     5            '(mg/l)  ','        '/
      DATA  SILAB/'  (mm)  ','(mm/hr) ',
     1            '(mm/hr) ','(hours) ',
     2            '(hours) ','  (kg)  ',
     4            '(kg/hr) ',' (kg/hr)','(mg/l)  ',
     5            '(mg/l)  ','        '/
      DATA RAINF /'    Rain','    Flow'/
      DATA CNOTE /'See note'/
      DATA PARLAB/'          Total Flow',
     2            '        Average Flow',
     3            '           Peak Flow',
     4            '      Event Duration',
     5            ' Interevent Duration',
     6            '          Total Load',
     7            '        Average Load',
     8            '           Peak Load',
     9            '    Event Mean (EMC)',
     1            '  Peak Concentration',
     2            '         Rain Volume',
     3            '   Average Intensity',
     4            '      Peak Intensity',
     5            '      Event Duration',
     6            ' Interevent Duration',
     7            'Log Event Mean (EMC)'/
      END
