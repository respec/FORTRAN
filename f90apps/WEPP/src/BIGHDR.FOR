      subroutine bighdr(nyear,nofe,ver)
      integer nyear, nofe
c
c
c     Local variables
c
      integer numhil, numofe, i
      real ver
      character*45 hillnm(13), ofenm1(15), ofenm2(15), ofenm3(15),
     1    ofenm4(15), ofenm5(15), ofenm6(4)
c
      data numhil /13/
      data numofe /79/
c
c
c     header names
c
      data hillnm /
     1    '{Days In Simulation}',
     1    '{Hillslope: Precipitation (mm)}',
     1    '{Hillslope: Average detachment (kg/m**2)}',
     1    '{Hillslope: Maximum detachment (kg/m**2)}',
     1    '{Hillslope: Point of maximum detachment (m)}',
     1    '{Hillslope: Average deposition (kg/m**2)}',
     1    '{Hillslope: Maximum deposition (kg/m**2)}',
     1    '{Hillslope: Point of maximum deposition (m)}',
     1    '{Hillslope: Sediment Leaving Profile (kg/m)}',
     1    '{Hillslope: 5 day average mimimum temp. (C)}',
     1    '{Hillslope: 5 day average maximum temp. (C)}',
     1    '{Hillslope: daily minimum temp. (C)}',
     1    '{Hillslope: daily maximum temp. (C)}'/
      data ofenm1 /
     1    '{Irrigation depth (mm)}',
     1    '{Irrigation_volume_supplied/unit_area (mm)}',
     1    '{Runoff (mm)}',
     1    '{Interrill net soil loss (kg/m**2)}',
     1    '{Canopy height (m)}',
     1    '{Canopy cover (0-1)}',
     1    '{Leaf area index}',
     1    '{Interrill cover (0-1)}',
     1    '{Rill cover (0-1)}',
     1    '{Above ground live biomass (kg/m**2)}',
     1    '{Live root mass for OFE (kg/m**2)}',
     1    '{Live root mass 0-15 cm depth (kg/m**2)}',
     1    '{Live root mass 15-30 cm depth (kg/m**2)}',
     1    '{Live root mass 30-60 cm depth (kg/m**2)}',
     1    '{Root depth (m)}'/
      data ofenm2 /
     1    '{Standing dead biomass (kg/m**2)}',
     1    '{Current residue mass on ground (kg/m**2)}',
     1    '{Previous residue mass on ground (kg/m**2)}',
     1    '{Old residue mass on the ground (kg/m**2)}',
     1    '{Current submerged residue mass (kg/m**2)}',
     1    '{Previous submerged residue mass (kg/m**2)}',
     1    '{Old submerged residue mass (kg/m**2)}',
     1    '{Current dead root mass (kg/m**2)}',
     1    '{Previous dead root mass (kg/m**2)}',
     1    '{Old dead root mass (kg/m**2)}',
     1    '{Porosity (%)}',
     1    '{Bulk density (g/cc)}',
     1    '{Effective hydraulic conductivity (mm/hr)}',
     1    '{Suction across wetting front (mm)}',
     1    '{Evapotranspiration (mm)}'/
      data ofenm3 /
     1    '{Drainage flux (m/day)}',
     1    '{Depth to drainable zone (m)}',
     1    '{Effective intensity (mm/h)}',
     1    '{Peak runoff (mm/h)}',
     1    '{Effective runoff duration (h)}',
     1    '{Enrichment ratio}',
     1    '{Adjusted Ki (millions kg-s/m**4)}',
     1    '{Adjusted Kr (x 1000 s/m)}',
     1    '{Adjusted Tauc (Pascals)}',
     1    '{Rill width (m)}',
     1    '{Plant Transpiration (mm)}',
     1    '{Soil Evaporation (mm)}',
     1    '{Seepage (mm)}',
     1    '{Water stress}',
     1    '{Temperature stress}'/
      data ofenm4 /'{Total soil water (mm)}',
     1    '{Soil water in layer 1 (mm)}',
     1    '{Soil water in layer 2 (mm)}',
     1    '{Soil water in layer 3 (mm)}',
     1    '{Soil water in layer 4 (mm)}',
     1    '{Soil water in layer 5 (mm)}',
     1    '{Soil water in layer 6 (mm)}',
     1    '{Soil water in layer 7 (mm)}',
     1    '{Soil water in layer 8 (mm)}',
     1    '{Soil water in layer 9 (mm)}',
     1    '{Soil water in layer 10 (mm)}',
     1    '{Random roughness (mm)}',
     1    '{Ridge height (mm)}',
     1    '{Frost depth (mm)}',
     1    '{Thaw depth (mm)}'/
      data ofenm5 /
     1    '{Snow depth (mm)}',
     1    '{Water from snow melt (mm)}',
     1    '{Snow density (kg/m**3)}',
     1    '{Rill cover fric fac (crop)}',
     1    '{Fric. fac. due to live plant}',
     1    '{Rill total fric fac (crop)}',
     1    '{Composite area total friction factor}',
     1    '{Rill cov fric fac (range)}',
     1    '{Live basal area fric fac (range)}',
     1    '{Live plant canopy fric fac (range)}',
     1    '{Days since last disturbance}',
     1    '{Current crop type}',
     1    '{Current residue on ground type}',
     1    '{Previous residue on ground type}',
     1    '{Old residue on ground type}'/
      data ofenm6 /
     1    '{Current dead root type}',
     1    '{Previous dead root type}',
     1    '{Old dead root type}',
     1    '{Sediment leaving OFE (kg/m)}'/
c
c     write version and header information.
c
      write (40,*) '#'
      write (40,*) '#       WEPP Daily Output File'
      write (40,*) '#'
      write (40,*) 'float   WeppVersion     ', ver
      write (40,*) 'int     NumYears        ', nyear
      write (40,*) 'int     NumOFEs         ', nofe
      write (40,*) '#'
      write (40,*) '#       Hillslope-specific variables'
      write (40,*) '#'
      write (40,*) 'char    HillVarNames[', numhil, ']'
      do 10, i = 1, numhil
        write (40,*) '        ', hillnm(i)
   10 continue
c
      write (40,*) '#'
      write (40,*) '#       OFE-specific variables'
      write (40,*) '#'
      write (40,*) 'char    OFEVarNames[', numofe, ']'
      do 20, i = 1, 15
        write (40,*) '        ', ofenm1(i)
   20 continue
      do 30, i = 1, 15
        write (40,*) '        ', ofenm2(i)
   30 continue
      do 40, i = 1, 15
        write (40,*) '        ', ofenm3(i)
   40 continue
      do 50, i = 1, 15
        write (40,*) '        ', ofenm4(i)
   50 continue
      do 60, i = 1, 15
        write (40,*) '        ', ofenm5(i)
   60 continue
      do 70, i = 1, 4
        write (40,*) '        ', ofenm6(i)
   70 continue
      write (40,*) '#'
      write (40,*) '#       Daily values:'
      write (40,*) '#'
c
      return
      end