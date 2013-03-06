program main
!     + + + END SPECIFICATIONS + + +
!
!     variables to pass in (need to be added to the osv):
!     DTS           -- integer, dynamic wave time step
!
!     note: assume last one in list is the outfall, type is free
!     NNODE         -- integer, number of nodes
!     NODEX(NNODE)  -- integer, node index
!     NELEV(NNODE)  -- real, invert elevation
!     NDMAX(NNODE)  -- real, max depth for this node
!     NDINIT(NNODE) -- real, initial node depth
!     NDSURC(NNODE) -- real, surcharge depth for this node
!     NPONDA(NNODE) -- real, ponded area for this node
!     NLOCIN(NNODE) -- real, local inflow percentage to this node
!
!     NCOND         -- integer, number of conduits
!     CNODE1(NCOND) -- integer, upstream conduit node index 
!     CNODE2(NCOND) -- integer, downstream conduit node index
!     CLEN(NCOND)   -- real, conduit length
!     CMANN(NCOND)  -- real, conduit mannings n
!     COFF1(NCOND)  -- real, inlet offset
!     COFF2(NCOND)  -- real, outlet offset
!     CQ0(NCOND)    -- real, initial flow
!     CSHAPE(NCOND) -- integer, conduit shape type
!     CGEOM1(NCOND) -- real, first item of shape geometry
!     CGEOM2(NCOND) -- real, second item of shape geometry
!     CGEOM3(NCOND) -- real, third item of shape geometry
!     CGEOM4(NCOND) -- real, fourth item of shape geometry
!
!     conduit types
!     DUMMY: 0
!     CIRCULAR: 1
!     FORCE_MAIN: 2                                                          
!     FILLED_CIRCULAR: 3
!     EGGSHAPED: 4
!     HORSESHOE: 5
!     GOTHIC: 6
!     CATENARY: 7
!     SEMIELLIPTICAL: 8
!     BASKETHANDLE: 9
!     SEMICIRCULAR: 10
!     RECT_CLOSED: 11
!     RECT_OPEN: 12
!     RECT_TRIANG: 13
!     RECT_ROUND: 14
!     MOD_BASKET: 15
!     TRAPEZOIDAL: 16
!     TRIANGULAR: 17
!     PARABOLIC: 18
!     POWERFUNC: 19
!     HORIZ_ELLIPSE: 20
!     VERT_ELLIPSE: 21
!     ARCH: 22
!
      use headers
       
      integer, parameter :: NNODE = 3
      integer :: J, LTYPE
      real, dimension(NNODE) :: XN
      real, dimension(NNODE) :: NDINIT = (/0.05, 0.05, 0.016/)
      real, dimension(NNODE) :: NELEV = (/10.14, 10.14, 9.23/)
      real, dimension(NNODE) :: NDMAX = (/11.14, 11.14, 10.23/)
      real, dimension(NNODE) :: NDSURC = (/12.14, 12.14, 11.23/)
      real, dimension(NNODE) :: NPONDA = (/0, 0, 0/)
      
      DO 10 J= 1,NNODE
        LTYPE = 0
        IF (J.EQ.NNODE) THEN
!         this is the outfall
          LTYPE = 1
        END IF 
        XN(1) = NELEV(J)
        XN(2) = NDMAX(J)
        XN(3) = NDINIT(J)
        XN(4) = NDSURC(J)
        XN(5) = NPONDA(J)
        node_setParams(J, LTYPE, 0, XN)
 10   CONTINUE
!
      LTYPE = 0
      DO 20 J= 1,NCOND 
        XC(1) = CLEN(J)
        XC(2) = CMANN(J)
        XC(3) = COFF1(J)
        XC(4) = COFF2(J)
        XC(5) = CQ0(J)
        XC(6) = 0.0
        link_setParams(J, LTYPE, CNODE1, CNODE2, J, XC)
        XX(1) = CGEOM1(J)
        XX(2) = CGEOM2(J)
        XX(3) = CGEOM3(J)
        XX(4) = CGEOM4(J)
        xsect_setParams(J, CSHAPE(J), 1, X, 0.0)   !haven't quite understood this yet
 20   CONTINUE
!
      NTS = DELTS/DTS
      DO 100 ITS = 1,NTS
         dynwave_execute(links,DTS)
 100  CONTINUE
!
!     have to figure out how to get the output back here
      O = OS
      RO = ROS
      VOL = VOLT
      OVOL = 0.0
      ROVOL = OVOL
!
      RETURN
      END
end program
