      SUBROUTINE ASCHEADA
C	EXTRAN BLOCK
      INCLUDE 'TAPES.INC'
      INCLUDE 'ASCOUT1.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'ORF.INC'
C=======================================================================
C
C     WRITE HEADER INFORMATION TO DETAILED ASCII INTERFACE FILE 
C
C     CREATED 9/11/97 by Charles I. Moore
C                        Camp Dresser & McKee
C                        Annandale, Va.
C
C=======================================================================
C   FIRST WRITE A HEADER LINE DESCRIBING WHERE THIS FILE COMES FROM.
C   "* " IN COLUMNS 1 AND 2 INDICATE COMMENT LINE IN ASCII TRANSFER
C   FILE
C=======================================================================
      WRITE(IASCII,7000)
C     WRITE TIME DESCRIPTOR LINE
      WRITE(IASCII,7020)
      WRITE(IASCII,7040)
C     SWMMDATE created to return current month day year hour minute second
C     and millisecond as integers.  Year now needs to include 4 digits to
C     avoid millineum problem.
      CALL SWMMDATE(IMON,IDAY,IYR,IHR,IMIN,ISEC,IHHH)
      WRITE(IASCII,7060) IYR,IMON,IDAY,IHR,IMIN,ISEC
      CALL DATED
      WRITE(IASCII,7080) NYEAR,MONTH,NDAY,JHR,MINUTE,JSEC
      WRITE(IASCII,7100) NJ,1,'Number of nodes including outfalls'
      WRITE(IASCII,7100) NPUMP,2,'Number of pumps'
      WRITE(IASCII,7100) NWEIR,3,'Number of weirs'
      WRITE(IASCII,7100) NC,4,'Number of conduits (branches)'
      WRITE(IASCII,7100) 3457,5,'Model number indicates SWMM EXTRAN'
	stepsss = INT(1.0+NTCYC/INTER)
      WRITE(IASCII,7110) stepsss,1,
     a'Number of output time steps in file'
      WRITE(IASCII,7110) NTCYC*DELT/3600.0,2,
     a 'Total simulation time in hours'
      WRITE(IASCII,7110) DELT,3,'Time step in seconds'
      WRITE(IASCII,7110) DELT*INTER,4,'Output time step seconds'
      WRITE(IASCII,7110) 0.0,6,'not used'
      WRITE(IASCII,7120) 0
C
C start item descriptors here
C
C time is first
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 2,' 2 is time'
      WRITE(IASCII,7200) 1
      WRITE(IASCII,*)
C now do Water Level Junctions
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 103,'103 is junction water level'
      WRITE(IASCII,7200) NJ,'number of junctions'
      WRITE(IASCII,*) 'SWMM EXTRAN, WATER LEVEL JUNCTIONS',
     a'                                    FEET'
C now do pump discharge
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 203,' 203 is pump discharge'
      WRITE(IASCII,7200) NPUMP,'Number of Pumps'
      WRITE(IASCII,*) 'SWMM EXTRAN, DISCHARGE PUMP',
     a'                                           CFS'
c now do weir discharge
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 204,'204 is weir discharge'
      WRITE(IASCII,7200) NWEIR,'Number of Weirs'
      WRITE(IASCII,*) 'SWMM EXTRAN, DISCHARGE WEIR',
     a'                                           CFS'
c now do orifice discharge
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 220,'220 is orifice discharge'
      WRITE(IASCII,7200) NORIF,'Number of Orifices'
      WRITE(IASCII,*) 'SWMM EXTRAN, DISCHARGE ORIFICE',
     a'                                        CFS'
C now do water level links
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 100,'100 is link water level'
      WRITE(IASCII,7200) NC*2,
     a'Number of grid level points, 2 for each conduit'
      WRITE(IASCII,*) 'SWMM EXTRAN, WATER LEVEL LINKS',
     a'                                        FEET'
C now do discharge in branches
      WRITE(IASCII,7140)
      WRITE(IASCII,7160) 0,'0 indicates item descriptor'
      WRITE(IASCII,7180) 200,'200 is flow in conduits'
      WRITE(IASCII,7200) NC,
     a'Number of discharge points, 1 for each conduit'
      WRITE(IASCII,*) 'SWMM EXTRAN, DISCHARGE BRANCHES',
     a'                                       CFS'
C
C now start channel descriptors
C
C    Time is first
C
      IOUTPUT = 0
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 2,' 2 is time'
      WRITE(IASCII,7220) 0
      WRITE(IASCII,*)
      WRITE(IASCII,7240) 0.0,0.0,'Hours'
      WRITE(IASCII,7260) NTCYC*DELT/3600.0,NTCYC*DELT/3600.0,'HOURS'
      WRITE(IASCII,7100) 0,1
      WRITE(IASCII,7100) 0,2
      WRITE(IASCII,7100) 0,3
      WRITE(IASCII,7100) 0,4
      WRITE(IASCII,7100) 0,5
      WRITE(IASCII,7110) 3600.0,1,'Time unit in seconds'
      WRITE(IASCII,7110) 0.0,2
      WRITE(IASCII,7110) 0.0,3
      WRITE(IASCII,7110) 0.0,4
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 0
C
C  now start channel descriptor for node water level
C
      DO J = 1,NJ
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 103,' Junction Water Level'
      WRITE(IASCII,7220) 0
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7340) JUN(J)
      ELSE
      WRITE(IASCII,7360) AJUN(J)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
C need to identify if it is an outfall  ITYPE 1 circular
C                                             2 basin
C                                             3 outfall
      ITYPE = 1
      I = 1
      DO WHILE ((I.LE.NFREE).AND.(JFREE(I).NE.J))
      I = I + 1
      ENDDO
      IF (JFREE(I).EQ.J) ITYPE = 3
      I = 1
      DO WHILE ((I.LE.NGATE).AND.(JGATE(I).NE.J))
      I = I + 1
      ENDDO
      IF (JGATE(I).EQ.J) ITYPE = 3
      WRITE(IASCII,7100) ITYPE,2,
     a' junction type 1 manhole, 2 basin, 3 outfall'
      WRITE(IASCII,7100) 0,3
      WRITE(IASCII,7100) 0,4
      WRITE(IASCII,7100) 0,5
C find if weir flows from junction  and get weir elevation
C this finds the minimum elevation of all weirs to a junction.
      WELEV = 999999.0
      DO I = 1, NWEIR
      IF (NJUNC(LWEIR(I),1).EQ.J) WELEV = AMIN1(WELEV,YCREST(I)+Z(J))
      ENDDO
      IF (WELEV.GE.999998.5) THEN
      WELEV = 0.0
      IEXIST = 0
      ELSE
      IEXIST = 1
      ENDIF
      WRITE(IASCII,7110) WELEV,1,'Weir elevation'
      WRITE(IASCII,7110) 0.0,2,'Critical elevation input'
      WRITE(IASCII,7110) 0.0,3
      WRITE(IASCII,7110) 0.0,4
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) IEXIST,1,'Indicates that there is a weir'
      WRITE(IASCII,7280) 0,2,
     a'indicates that critical elevation was input'
      WRITE(IASCII,7120) 1
      WRITE(IASCII,7300)
      WRITE(IASCII,7160) 6,' 6 identifies real desc'
      WRITE(IASCII,7320) Z(J),1,' junction invert'
      WRITE(IASCII,7320) GRELEV(J),2,' ground elevation'
      WRITE(IASCII,7320) XLOC(J),3,' x location'
      WRITE(IASCII,7320) YLOC(J),4,' y location'
      DO I = 5,20
      WRITE(IASCII,7320) 0.0,I
      ENDDO
      ENDDO
c have now completed water level nodes section
c pump flow section follows
c
      DO J = 1, NPUMP
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 203,' Pump Discharge'
      WRITE(IASCII,7220) 0
      LPNK = LPUMP(J)
      J1 = NJUNC(LPNK,1)
      J2 = NJUNC(LPNK,2)
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) 1,2,' Number of pumps Extran only has one pump'
      WRITE(IASCII,7100) IPTYP(J),3,'This is extran pump type '
      WRITE(IASCII,7100) 0,4
      WRITE(IASCII,7100) 0,5
c extran pump doesn't have a length get from x,y or put in 50
      PUMPLEN = SQRT((XLOC(J1)-XLOC(J2))**2+(XLOC(J1)-XLOC(J2))**2)
      IF (PUMPLEN.EQ.0.0) PUMPLEN = 50.
      WRITE(IASCII,7110) PUMPLEN,1,'LENGTH OF PUMP LINK'
      WRITE(IASCII,7110) 0.0,2
      WRITE(IASCII,7110) 0.0,3
      WRITE(IASCII,7110) 0.0,4
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 1
      WRITE(IASCII,7300)
      WRITE(IASCII,7160) 6,' 6 identifies real desc'
      WRITE(IASCII,7320) Z(J1),1,' pumped junction invert'
      WRITE(IASCII,7320) GRELEV(J1),2,
     a' pumped junction ground elevation'
      WRITE(IASCII,7320) XLOC(J1),3,' pumped junction x location'
      WRITE(IASCII,7320) YLOC(J1),4,' pumped junction y location'
      DO I = 5,20
      WRITE(IASCII,7320) 0.0,I
      ENDDO
      ENDDO
c now have pumps done
c next part is orifices
      DO J = 1, NORIF
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 220,' Orifice Discharge'
      WRITE(IASCII,7220) 0
      LPNK = LORIF(J)
      J1 = NJUNC(LPNK,1)
      J2 = NJUNC(LPNK,2)
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) 0,2
      WRITE(IASCII,7100) 0,3
      WRITE(IASCII,7100) 0,4
      WRITE(IASCII,7100) 0,5
      WRITE(IASCII,7110) 0.0,1
      WRITE(IASCII,7110) 0.0,2
      WRITE(IASCII,7110) 0.0,3
      WRITE(IASCII,7110) 0.0,4
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 0
      ENDDO
c now have orifices done
c next part is Weirs
      DO J = 1, NWEIR
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 204,' Weir Discharge'
      WRITE(IASCII,7220) 0
      LWNK = LWEIR(J)
      J1 = NJUNC(LWNK,1)
      J2 = NJUNC(LWNK,2)
      IF (J2.EQ.0) THEN
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),0
      ELSE
      WRITE(IASCII,7400) AJUN(J1),'0'
      ENDIF
      ElSE
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) KWEIR(J),2,' Extran weir type'
      IF ((KWEIR(J).EQ.3).OR.(KWEIR(J).EQ.4)) THEN
      ITRAN = 1
      ELSE
      ITRAN = 2
      ENDIF
      WRITE(IASCII,7100) ITRAN,3,' 1 side, 2 transverse'
      WRITE(IASCII,7100) 1,4,
     a' 1 sharp, 2 broad extran does not differentiate'
      WRITE(IASCII,7100) 0,5
c extran weir doesn't have a length get from x,y or put in 50
      IF (J2.EQ.0) THEN
      PUMPLEN = 0.0
      ELSE
      PUMPLEN = SQRT((XLOC(J1)-XLOC(J2))**2+(XLOC(J1)-XLOC(J2))**2)
      IF (PUMPLEN.EQ.0.0) PUMPLEN = 50.
      ENDIF
      WRITE(IASCII,7110) PUMPLEN,1,'LENGTH OF PUMP LINK'
      WRITE(IASCII,7110) YCREST(J)+Z(J1),2,' crest elevation'
      WRITE(IASCII,7110) 0.0,3,' sill elevation'
      WRITE(IASCII,7110) WLEN(J),4,' weir length'
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 1,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 1
      WRITE(IASCII,7300)
      WRITE(IASCII,7160) 6,' 6 identifies real desc'
      WRITE(IASCII,7320) Z(J1),1,' pumped junction invert'
      WRITE(IASCII,7320) GRELEV(J1),2,
     a' pumped junction ground elevation'
      WRITE(IASCII,7320) XLOC(J1),3,' pumped junction x location'
      WRITE(IASCII,7320) YLOC(J1),4,' pumped junction y location'
      DO I = 5,20
      WRITE(IASCII,7320) 0.0,I
      ENDDO
      ENDDO
c now have weirs done
C now do conduit grids
      DO J = 1, NC
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 100,' Water Level Branch'
      WRITE(IASCII,7220) 0
      J1 = NJUNC(J,1)
      J2 = NJUNC(J,2)
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) NKLASS(J),2,' Internal Extran pipe type'
      WRITE(IASCII,7100) 3,3,' Number of grid points 3 for extran'
      WRITE(IASCII,7100) 1,4,' grid point number'
      WRITE(IASCII,7100) 0,5
      WRITE(IASCII,7110) 0.0,1,'Distance from upstream end'
      WRITE(IASCII,7110) DEEP(J),2,' conduit height'
      WRITE(IASCII,7110) ZU(J),3,' invert elevation'
      WRITE(IASCII,7110) GRELEV(J1),4,' ground elevation'
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 1
      WRITE(IASCII,7300)
      WRITE(IASCII,7160) 6,' 6 identifies real desc'
      WRITE(IASCII,7320) ZU(J),1,
     a' Pipe invert elevation at upstream end'
      WRITE(IASCII,7320) ZD(J),2,
     a' Pipe invert elevation at downstream end'
      WRITE(IASCII,7320) DEEP(J),3,' Pipe height'
      WRITE(IASCII,7320) LEN(J),4,' Pipe length'
      WRITE(IASCII,7320) Z(J1),5,' Invert of upstream node'
      WRITE(IASCII,7320) QFULL(J),6,' full flow q (mannings)'
      DO I = 7,20
      WRITE(IASCII,7320) 0.0,I
      ENDDO
c end of grid point 1
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 200,' Discharge Branch'
      WRITE(IASCII,7220) 0
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) NKLASS(J),2,' Internal Extran pipe type'
      WRITE(IASCII,7100) 3,3,' Number of grid points 3 for extran'
      WRITE(IASCII,7100) 2,4,' grid point number'
      WRITE(IASCII,7100) 0,5
      WRITE(IASCII,7110) 0.5*LEN(J),1,'Distance from upstream end'
      WRITE(IASCII,7110) QFULL(J),2,' qfull, manning'
      WRITE(IASCII,7110) 0.5*(ZU(J)+ZD(J)),3,' invert elevation'
      WRITE(IASCII,7110) 0.5*(GRELEV(J1)+GRELEV(J2)),4,
     a' ground elevation'
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 0
c end of grid point 2
      IOUTPUT = IOUTPUT + 1
      WRITE(IASCII,7210) IOUTPUT
      WRITE(IASCII,7160) 1,'1 indicates Channel Desc.'
      WRITE(IASCII,7180) 100,' Water Level Branc'
      WRITE(IASCII,7220) 0
      IF (JCE.EQ.0) THEN
      WRITE(IASCII,7380) JUN(J1),JUN(J2)
      ELSE
      WRITE(IASCII,7400) AJUN(J1),AJUN(J2)
      ENDIF
      WRITE(IASCII,7240) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7260) 0.0,0.0,'do not know these set to zero for now'
      WRITE(IASCII,7100) J,1
      WRITE(IASCII,7100) NKLASS(J),2,' Internal Extran pipe type'
      WRITE(IASCII,7100) 3,3,' Number of grid points 3 for extran'
      WRITE(IASCII,7100) 3,4,' grid point number'
      WRITE(IASCII,7100) 0,5
      WRITE(IASCII,7110) LEN(J),1,'Distance from upstream end'
      WRITE(IASCII,7110) DEEP(J),2,' Pipe height'
      WRITE(IASCII,7110) ZD(J),3,' invert elevation'
      WRITE(IASCII,7110) GRELEV(J2),4,' ground elevation'
      WRITE(IASCII,7110) 0.0,5
      WRITE(IASCII,7280) 0,1
      WRITE(IASCII,7280) 0,2
      WRITE(IASCII,7120) 0
      ENDDO

      RETURN
C=======================================================================
C    format statements follow
7000  FORMAT('* ================================================',/,
     1       '* ==  SWMM EXTRAN DETAILED ASCII INTERFACE FILE ==',/,
     2       '* ==  CREATED BY SWMM 4.4                       ==',/,
     3       '* ==  CAMP DRESSER & McKEE   SEPT 1997          ==',/,
     4       '* ================================================')
7020  FORMAT('* ============================================',/,
     1'    2  * RecSel                               <<< Always 2',/,
     2'    2  * Rectype                              <<< Always 2')
7040  FORMAT('* =    TIME DESC.                           ==')
7060  FORMAT(I4,'-',I2,'-',I2,' ',I2,':',I2,':',I2,
     1'   * File Creation Date    : YYYY-MM-DD HH:MM:SS')
7080  FORMAT(I4,'-',I2,'-',I2,' ',I2,':',I2,':',I2,
     1'   * Simulation Start Date : YYYY-MM-DD HH:MM:SS')
7100  FORMAT(I10,'  * Div_Intg[',i1,']          <<<',A50)
7110  FORMAT(F10.4,'  * Div_Real[',i1,']          <<<',A50)
7120  FORMAT(I10,' * Number of extra records ')
7140  FORMAT('* ============================================',/,
     a'* =    ITEM DESC.                           ==')
7160  FORMAT(I10,' * RecSel     <<< ',A50)
7180  FORMAT(I10,' * Rectype    <<< ',A50)
7200  FORMAT(I10,' * Number of  <<< ',A50)
7210  FORMAT('* ============================================',/,
     a'* =    CHAN DESC.   ',I10,'              ==')
7220  FORMAT(I10,' * Unittype   <<< Always zero, not used')
7240  FORMAT(1PE20.10,E20.10,' * Minimum value and corresponding time',
     1A50)
7260  FORMAT(1PE20.10,E20.10,' * Maximum value and corresponding time',
     1A50)
7280  FORMAT(I10,' * Bool',I1,' (1=True, 0=False)',A50)
7300  FORMAT('* =    REAL DESC.                           ==')
7320  FORMAT(1PE20.10,'   <<< ',i2,A50)
7340  FORMAT(I10)
7360  FORMAT(A10)
7380  FORMAT(I10,' TO ',I10)
7400  FORMAT(A10,' TO ',A10)
      END
