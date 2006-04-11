C     TAPES.INC
C=======================================================================
C     THIS IS THE PRIMARY COMMON BLOCK FOR SETTING BROAD SWMM ARRAY
C       DIMENSIONS USING PARAMETER STATEMENT.
C     SEE ALSO SUB. UNIT.FOR FOR NO. S/T PLUGS AND PRECIP.INC FOR
C       RAIN BLOCK ARRAYS (MOVED HERE, 7/30/97).
C     PARAMETER STATEMENTS ARE ALSO USED A FEW OTHER LOCATIONS:
C       INCLUDE FILES: FSPLIT, HUGO, FDII, STCOM.
C       SUBROUTINES: RDIIRES, G9RAIN, GETCUR.
C     THIS FILE ALSO INCLUDES PRIMARY INPUT/OUTPUT FILE NAMES AND
C       UNIT NUMBERS.
C#### WCH (CDM), 8/93.  Additional parameter NTSP for type 26 flow divider
C                       in TRANSPORT block.
C#### WCH, 8/4/95.      Add parameter NCHN for number of channels
C                       connecting to any junction in EXTRAN block.
C#### CIM, 5/1/97.      Add number of rainfall gages MAXRG.
C#### CIM, 5/1/97.      Add number of VRATE/PRATE points for pump input.
C#### WCH, 7/30/97.     Add NVORF, NVTOIM from ORF.INC.
C#### WCH, 7/30,97.     Add LIMRN, LSTORM from PRECIP.INC.
C#### WCH, 7/30/97.     Add NPLUG from Subroutine UNIT.
CIM       10/27/98      Clean up dimensioning in Transport NTH removed
CIM                     replaced with NTHI, NTHO, NTHOA, NTHR
CIM        4/1/99       Added MQUAL
C=======================================================================
C     NW    = Number of subcatchments in the RUNOFF block
C     NG    = Number of channel/pipes in the RUNOFF block
C     NLU   = Number of different land uses in the RUNOFF block
C     NCP   = Number of RUNOFF connecting channel/pipes of subcatchments
C     NET   = Number of elements in the TRANSPORT block
C     NTSE  = Number of storage elements in the TRANSPORT block
C     NTHI  = Number of input hydrographs in TRANSPORT
C     NTHO  = Number of output hydrographs in TRANSPORT
C     NTOA  = Number of printed hydrographs and heads in TRANSPORT
C     NTHR  = Number of input hydrographs on R lines.
C     NTSP  = Number of tabular flow splits (NTYPE 26) in TRANSPORT block
C     NEE   = Number of elements in EXTRAN block
C     NGW   = Number of subcatchments with groundwater compartments
C             in RUNOFF block
C     NIE   = Number of interface locations for all blocks
C     NEP   = Number of pumps in EXTRAN block
C     NEO   = Number of orifices in EXTRAN block
C     NTG   = Number of tide gates or free outfalls in EXTRAN block
C     NEW   = Number of weirs in EXTRAN block
C     NPO   = Number of printout locations in EXTRAN block
C     NTE   = Number of tide elements in EXTRAN block
C     NNC   = Number of natural channels in EXTRAN and TRANSPORT blocks
C     NVSE  = Number of storage junctions in EXTRAN block
C     NTVAL = Number of tidal or time history points in Extran.
C     NVST  = Number of data points for variable storage elements
C             in EXTRAN block
C     NEH   = Number of input hydrographs in the EXTRAN block
C     NCHN  = Number of connecting channels to a junction in EXTRAN block
C     MAXRG = Maximum number of rainfall gages in RAIN and RUNOFF blocks
C     MAXPRA= Maximum number of PRATE/VRATE points on pump input in
C             EXTRAN block
C     NVORF = Number of variable orifices in EXTRAN
C     NVOTIM= Number of data points for variable orifices in EXTRAN
CIM  WRITING RESULTS TO ASCII FILE
C     MXFLOW= Maximum number of conduits to write EXTRAN results to
C             ASCII file
C     LIMRN = Number of allowable precip. values/yr in RAIN
C     LSTORM= Number of storm events to be analyzed in RAIN
C     NPLUG = Number of plugs for plug-flow in S/T
C
C     MQUAL = Maximum number of quality parameter simulated in all
C             blocks.  Suggest that maximum be set no greater than
C             20 or write statement formats may need to be modified.
C
C     MSTU  = Maximum number of storage treatment block units.
C
C
C     INSTRUCTIONS - Increase dimensions of subcatchments etc.
C                    by modifying the parameter statement
C                    and recompiling your program.
C=======================================================================
      PARAMETER(NW=1000,NG=1000,NLU=20,NGW=100,NCP=6,NET=1000,
     +         NTSE=100,NEE=4000,NTHI=500,NTHO=500,NTOA=80,NTHR=80,
     +         NIE=1000,NTE=50,NEW=400,NEO=200,NEP=75,NTG=200,
     +         NPO=150,NVSE=1000,NVST=25,NNC=1200,NEH=500,NTVAL=500,
     +         NTSP=50,NCHN=15,MAXRG=200,MAXPRA=10,MXFLOW=400,NVORF=50,
     +         NVOTIM=50,LIMRN=5000,LSTORM=20000,NPLUG=3000,MQUAL=20,
     +         MSTU=5)
      CHARACTER*2 CC,FFNAME*60
      COMMON /TAPES/INCNT,IOUTCT,JIN(25),JOUT(25),JCE,NOQ,
     +              NSCRAT(8),N5,N6,JKP(58),CMET(11,2),FFNAME(58),CC
