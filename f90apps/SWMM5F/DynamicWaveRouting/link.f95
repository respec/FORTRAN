module modLink

!-----------------------------------------------------------------------------
!  Constants
!-----------------------------------------------------------------------------
double precision, parameter :: MIN_DELTA_Z = 0.001 ! minimum elevation change for conduit slopes (ft)

contains

!=============================================================================

double precision function conduit_getInflow(j)
!
!  Input:   j = link index
!  Output:  returns flow in link (cfs)
!  Purpose: finds inflow to conduit from upstream node.
!
    use headers
    implicit none
    integer, intent(in) :: j
    
    double precision :: qIn
    
    if ( arrLink(j)%qLimit > 0.0 ) qIn = MIN(qIn, arrLink(j)%qLimit)                !(5.0.012 - LR)
    conduit_getInflow = qIn
end function conduit_getInflow

!=============================================================================

!!  New function added to release 5.0.015  !!                              !(5.0.015 - LR)

double precision function conduit_getLength(j)
!
!  Input:   j = link index
!  Output:  returns conduit's length (ft)
!  Purpose: finds true length of a conduit.
!
!  Note: for irregular natural channels, user inputs length of main
!        channel (for FEMA purposes) but program should use length
!        associated with entire flood plain. Transect.lengthFactor
!        is the ratio of these two lengths.
!
    use headers
    implicit none
    integer, intent(in) :: j
    integer :: k, t
    k = arrLink(j)%subIndex
    if ( arrLink(j)%xsect%datatype /= IRREGULAR ) then
       conduit_getLength = Conduit(k)%clength
       return
    end if
    t = arrLink(j)%xsect%transect
    if ( t < 0 .or. t >= Nobjects(E_TRANSECT) ) then
      conduit_getLength = Conduit(k)%clength
      return
    end if
    conduit_getLength = Conduit(k)%clength / Transect(t)%lengthFactor
end function conduit_getLength

!=============================================================================

double precision function conduit_getLengthFactor(j, k, roughness)                 !(5.0.010 - LR)
!
!  Input:   j = link index
!           k = conduit index
!           roughness = conduit Manning's n
!  Output:  returns factor by which a conduit should be lengthened
!  Purpose: computes amount of conduit lengthing to improve numerical stability.
!
!  The following form of the Courant criterion is used:
!      L = t * v * (1 + Fr) / Fr
!  where L = conduit length, t = time step, v = velocity, & Fr = Froude No.
!  After substituting Fr = v / sqrt(gy), where y = flow depth, we get:
!    L = t * ( sqrt(gy) + v )
!

    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    integer(kind=K4), intent(in) :: k
    double precision, intent(in) :: roughness
    double precision :: ratio
    double precision ::  yFull
    double precision ::  vFull
    double precision ::  tStep

    ! --- evaluate flow depth and velocity at full normal flow condition
    yFull = arrLink(j)%xsect%yFull
    if ( xsect_isOpen(arrLink(j)%xsect%datatype) ) then
        yFull = arrLink(j)%xsect%aFull / xsect_getWofY(arrLink(j)%xsect, yFull)
    end if
    vFull = PHI / roughness * arrLink(j)%xsect%sFull * &                          !(5.0.010 - LR)
           &sqrt(abs(Conduit(k)%slope)) / arrLink(j)%xsect%aFull                !(5.0.010 - LR)

    ! --- determine ratio of Courant length to actual length
    if ( abs(LengtheningStep - 0.0) < P_TINY ) then
       tStep = RouteStep
    else
       tStep = MIN(RouteStep, LengtheningStep)
    end if
    ratio = (sqrt(GRAVITY*yFull) + vFull) * tStep / conduit_getLength(j)      !(5.0.015 - LR)

    ! --- return max. of 1.0 and ratio
    if ( ratio > 1.0 ) then
       conduit_getLengthFactor = ratio
       return
    else 
       conduit_getLengthFactor = 1.0
       return
    end if
end function conduit_getLengthFactor

!=============================================================================
!! --- New function added for release 5.0.014 --- !!                       !(5.0.014 - LR)
double precision function conduit_getSlope(j)                                                 !(5.0.015 - LR)
!
!  Input:   j = link index
!  Output:  returns conduit slope
!  Purpose: computes conduit slope.
!
    use headers
    use report
    implicit none
    integer, intent(in) :: j
    double precision :: elev1, elev2, delta, mslope
    double precision :: mlength                                      !(5.0.015 - LR)
    mlength = conduit_getLength(j)                                      !(5.0.015 - LR)

    ! --- check that elevation drop > minimum allowable drop
    elev1 = arrLink(j)%offset1 + Node(arrLink(j)%node1)%invertElev
    elev2 = arrLink(j)%offset2 + Node(arrLink(j)%node2)%invertElev
    delta = abs(elev1 - elev2)
    if ( delta < MIN_DELTA_Z ) then
        call report_writeWarningMsg(WARN04, arrLink(j)%ID)                            !(5.0.015 - LR)
        delta = MIN_DELTA_Z
    end if

    ! --- elevation drop cannot exceed conduit length
    if ( delta >= mlength ) then                                                     !(5.0.015 - LR)
        call report_writeWarningMsg(WARN08, arrLink(j)%ID)                            !(5.0.017 - LR)
        mslope = delta / mlength                                                !(5.0.017 - LR)
    ! --- slope = elev. drop / horizontal distance
    else 
        mslope = delta / sqrt(SQR(mlength) - SQR(delta))                       !(5.0.015 - LR)
    end if

    ! -- check that slope exceeds minimum allowable slope
    if ( MinSlope > 0.0 .and. mslope < MinSlope ) then
        call report_writeWarningMsg(WARN05, arrLink(j)%ID)                            !(5.0.015 - LR)
        mslope = MinSlope
    end if

    ! --- change sign for adverse slope
    if ( elev1 < elev2 ) mslope = -1.0 * mslope

    conduit_getSlope = mslope
end function conduit_getSlope

!=============================================================================

subroutine conduit_initState(j, k)
!
!  Input:   j = link index
!           k = conduit index
!  Output:  none
!  Purpose: sets initial conduit depth to normal depth of initial flow
!
    use headers
    implicit none
    
    integer, intent(in) :: j
    integer(kind=K4), intent(in) :: k
    arrLink(j)%newDepth = link_getYnorm(j, arrLink(j)%q0 / Conduit(k)%barrels)
    arrLink(j)%oldDepth = arrLink(j)%newDepth
end subroutine conduit_initState

!=============================================================================

subroutine conduit_validate(j, k)
!
!  Input:   j = link index
!           k = conduit index
!  Output:  none
!  Purpose: validates a conduit's properties.
!
    use headers
    implicit none
    integer, intent(in) :: j
    integer(kind=K4), intent(in) :: k
    double precision :: aa
    double precision :: lengthFactor, roughness, mSlope                    !(5.0.018 - LR)
    
    double precision :: forcemain_getRoughFactor, forcemain_getEquivN, UCF

    ! --- if custom xsection, then set its parameters                         !(5.0.010 - LR)
    if ( arrLink(j)%xsect%datatype == CUSTOM ) &                                        !(5.0.010 - LR)
       &call xsect_setCustomXsectParams(arrLink(j)%xsect)                            !(5.0.010 - LR)

    ! --- if irreg. xsection, assign transect roughness to conduit
    if ( arrLink(j)%xsect%datatype == IRREGULAR ) then
        call xsect_setIrregXsectParams(arrLink(j)%xsect)
        Conduit(k)%roughness = Transect(arrLink(j)%xsect%transect)%roughness
    end if

    ! --- if force main xsection, adjust units on D-W roughness height        !(5.0.010 - LR)
    if ( arrLink(j)%xsect%datatype == FORCE_MAIN ) then                       !(5.0.010 - LR)
        if ( ForceMainEqn == D_W ) &
          &arrLink(j)%xsect%rBot = arrLink(j)%xsect%rBot / UCF(RAINDEPTH)       !(5.0.010 - LR)
        if ( arrLink(j)%xsect%rBot <= 0.0 ) &                                      !(5.0.010 - LR)
          &call report_writeErrorMsg(ERR_XSECT, arrLink(j)%ID)                       !(5.0.010 - LR)
    end if                                                                     !(5.0.010 - LR)

    ! --- check for valid length & roughness
    if ( Conduit(k)%clength <= 0.0 ) call report_writeErrorMsg(ERR_LENGTH, arrLink(j)%ID)
    if ( Conduit(k)%roughness <= 0.0 ) call report_writeErrorMsg(ERR_ROUGHNESS, arrLink(j)%ID)
    if ( Conduit(k)%barrels <= 0 ) call report_writeErrorMsg(ERR_BARRELS, arrLink(j)%ID)

    ! --- check for valid xsection
    if ( arrLink(j)%xsect%datatype /= DUMMY ) then
        if ( arrLink(j)%xsect%datatype < 0 ) then
           call report_writeErrorMsg(ERR_NO_XSECT, arrLink(j)%ID)
        else if ( arrLink(j)%xsect%aFull <= 0.0 ) then
           call report_writeErrorMsg(ERR_XSECT, arrLink(j)%ID)
        end if
    end if
    if ( ErrorCode /= 0 ) return

    ! --- check for negative offsets                                          !(5.0.012 - LR)
    if ( arrLink(j)%offset1 < 0.0 ) then
        call report_writeWarningMsg(WARN03, arrLink(j)%ID)                            !(5.0.015 - LR)
        arrLink(j)%offset1 = 0.0                                                 !(5.0.012 - LR)
    end if
    if ( arrLink(j)%offset2 < 0.0 ) then
        call report_writeWarningMsg(WARN03, arrLink(j)%ID)                            !(5.0.015 - LR)
        arrLink(j)%offset2 = 0.0                                                 !(5.0.012 - LR)
    end if

    ! --- adjust conduit offsets for partly filled circular xsection
    if ( arrLink(j)%xsect%datatype == FILLED_CIRCULAR ) then
        arrLink(j)%offset1 = arrLink(j)%offset1 + arrLink(j)%xsect%yBot
        arrLink(j)%offset2 = arrLink(j)%offset2 + arrLink(j)%xsect%yBot
    end if

    ! --- compute conduit mSlope 
    mSlope = conduit_getSlope(j)                                               !(5.0.018 - LR)
    Conduit(k)%slope = mSlope                                                  !(5.0.018 - LR)

    ! --- reverse orientation of conduit if using dynamic wave routing 
    !     and mSlope is negative
    if ( RouteModel == DW .and. &
        &mSlope < 0.0 .and. &                                                      !(5.0.018 - LR)
        &arrLink(j)%xsect%datatype /= DUMMY ) then
        call conduit_reverse(j, k)
    end if

    ! --- get equivalent Manning roughness for Force Mains                    !(5.0.010 - LR)
    !     for use when pipe is partly full                                    !(5.0.010 - LR)
    roughness = Conduit(k)%roughness                                          !(5.0.010 - LR)
    if ( RouteModel == DW .and. arrLink(j)%xsect%datatype == FORCE_MAIN ) then        !(5.0.010 - LR)
        roughness = forcemain_getEquivN(j, k)                                 !(5.0.010 - LR)
    end if                                                                          !(5.0.010 - LR)

    ! --- adjust roughness for meandering natural channels                    !(5.0.015 - LR)
    if ( arrLink(j)%xsect%datatype == IRREGULAR ) then
        lengthFactor = Transect(arrLink(j)%xsect%transect)%lengthFactor
        roughness = roughness * sqrt(lengthFactor)
    end if

    ! --- lengthen conduit if lengthening option is in effect
    lengthFactor = 1.0                                                        !(5.0.015 - LR)
    if ( RouteModel == DW .and. &
        &LengtheningStep > 0.0 .and. &
        &arrLink(j)%xsect%datatype /= DUMMY ) then
        lengthFactor = conduit_getLengthFactor(j, k, roughness)               !(5.0.010 - LR)
    end if
        
    if ( abs(lengthFactor - 1.0) > P_TINY ) then   !lengthFactor /= 1.0    !(5.0.015 - LR)
        !Conduit(k).modLength = lengthFactor * Conduit(k).length             !(5.0.015 - LR)
        Conduit(k)%modLength = lengthFactor * conduit_getLength(j)            !(5.0.015 - LR)
        mSlope = mSlope / lengthFactor                                          !(5.0.018 - LR)
        roughness = roughness / sqrt(lengthFactor)                            !(5.0.015 - LR)
    end if                                                                          !(5.0.015 - LR)

    ! --- compute roughness factor used when computing friction               !(5.0.010 - LR)
    !     mSlope term in Dynamic Wave flow routing                             !(5.0.010 - LR)

    ! --- special case for non-Manning Force Mains                            !(5.0.010 - LR)
    !     (roughness factor for full flow is saved in xsect.sBot)             !(5.0.010 - LR)     
    if ( RouteModel == DW .and. arrLink(j)%xsect%datatype == FORCE_MAIN ) then !(5.0.010 - LR)
        arrLink(j)%xsect%sBot = forcemain_getRoughFactor(j, lengthFactor)       !(5.0.010 - LR)
    end if                                                                 !(5.0.010 - LR)
    Conduit(k)%roughFactor = GRAVITY * SQR(roughness/PHI)                     !(5.0.010 - LR)

    ! --- compute full flow through cross section
    if ( arrLink(j)%xsect%datatype == DUMMY ) then
        Conduit(k)%beta = 0.0
    else 
        Conduit(k)%beta = PHI * sqrt(abs(mSlope)) / roughness                !(5.0.018 - LR)
    end if
    arrLink(j)%qFull = arrLink(j)%xsect%sFull * Conduit(k)%beta
    Conduit(k)%qMax = arrLink(j)%xsect%sMax * Conduit(k)%beta

    ! --- see if flow is supercritical most of time
    !     by comparing normal & critical velocities.
    !     (factor of 0.3 is for circular pipe 95% full)
    ! NOTE: this factor was used in the past for a modified version of        !(5.0.014 - LR)
    !       Kinematic Wave routing but is now deprecated.                     !(5.0.014 - LR)
    aa = Conduit(k)%beta / sqrt(32.2) * (arrLink(j)%xsect%yFull ** 0.1666667) * 0.3
    if ( aa >= 1.0 ) then
        Conduit(k)%superCritical = .TRUE.
    else             
        Conduit(k)%superCritical = .FALSE.
    end if

    ! --- set value of hasLosses flag
    if ( arrLink(j)%cLossInlet  == 0.0 .and. &
        &arrLink(j)%cLossOutlet == 0.0 .and. &
        &arrLink(j)%cLossAvg    == 0.0 ) then
         Conduit(k)%hasLosses = .FALSE.
    else 
         Conduit(k)%hasLosses = .TRUE.
    end if
end subroutine conduit_validate

!=============================================================================

!!  ------  This function was completely re-written.  ------  !!           !(5.0.014 - LR)

double precision function link_getFroude(j, v, y)
!
!  Input:   j = link index
!           v = flow velocity (fps)
!           y = flow depth (ft)
!  Output:  returns Froude Number
!  Purpose: computes Froude Number for given velocity and flow depth
!

    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: v, y
    double precision :: ym
    ym = y
    
    !TXsect*  xsect = &arrLink(j)%xsect

    ! --- return 0 if link is not a conduit
    if ( arrLink(j)%datatype /= E_CONDUIT ) then 
       link_getFroude = 0.0
       return
    end if
    
    ! --- return 0 if link empty or closed conduit is full
    if ( ym <= FUDGE ) then
       link_getFroude = 0.0
       return
    end if
    if ( .not. xsect_isOpen(arrLink(j)%xsect%datatype) .and. &
        &arrLink(j)%xsect%yFull - ym <= FUDGE ) then
        link_getFroude = 0.0
        return
    end if

    ! --- compute hydraulic depth
    ym = xsect_getAofY(arrLink(j)%xsect, ym) / xsect_getWofY(arrLink(j)%xsect, ym)

    ! --- compute Froude No.
    link_getFroude = abs(v) / sqrt(GRAVITY * ym)
end function link_getFroude

!=============================================================================

double precision function link_getInflow(j)
!
!  Input:   j = link index
!  Output:  returns link flow rate (cfs)
!  Purpose: finds total flow entering a link during current time step.
!
    use headers
    implicit none
    integer, intent(in) :: j
    double precision :: node_getOutflow
    if ( arrLink(j)%setting == 0 .or. arrLink(j)%isClosed ) then
       link_getInflow = 0.0
       return
    end if
    select case ( arrLink(j)%datatype )
      case (E_CONDUIT) 
         link_getInflow = conduit_getInflow(j)
      case (E_PUMP)    
         !link_getInflow = pump_getInflow(j)
      case (E_ORIFICE) 
         link_getInflow = orifice_getInflow(j)
      case (E_WEIR)    
         link_getInflow = weir_getInflow(j)
      case (E_OUTLET)  
         link_getInflow = outlet_getInflow(j)
      case default
         link_getInflow = node_getOutflow(arrLink(j)%node1, j)
    end select
end function link_getInflow

!=============================================================================

!!  New function added to release 5.0.015  !!                              !(5.0.015 - LR)

double precision function link_getLength(j)
!
!  Input:   j = link index
!  Output:  returns length (ft)
!  Purpose: finds true length of a link.
!
    use headers
    implicit none
    integer, intent(in) :: j
    if ( arrLink(j)%datatype == E_CONDUIT ) then
       link_getLength = conduit_getLength(j)
    else
       link_getLength = 0.0
    end if
end function link_getLength


!=============================================================================

subroutine link_validate(j)
!
!  Input:   j = link index
!  Output:  none
!  Purpose: validates a link's properties.
!
    use headers
    implicit none
    integer, intent(in) :: j
    integer :: n

    if ( LinkOffsets == ELEV_OFFSET ) call link_convertOffsets(j)                  !(5.0.012 - LR)
    select case ( arrLink(j)%datatype )
      case (E_CONDUIT)
         call conduit_validate(j, arrLink(j)%subIndex) !break
      case (E_PUMP)
         call pump_validate(j, arrLink(j)%subIndex)    !break
      case (E_ORIFICE)
         call orifice_validate(j, arrLink(j)%subIndex) !break
      case (E_WEIR)
         call weir_validate(j, arrLink(j)%subIndex)    !break
    end select

!!  The following code segment was revised in release 5.0.019  !!          !(5.0.019 - LR)

    ! --- force max. depth of end nodes to be >= link crown height
    !     at non-storage nodes (except for bottom orifices)
    if ( arrLink(j)%datatype /= E_ORIFICE .or. &
        &Orifice(arrLink(j)%subIndex)%datatype /= BOTTOM_ORIFICE ) then
        n = arrLink(j)%node1
        if ( Node(n)%datatype /= E_STORAGE ) then
            Node(n)%fullDepth = MAX(Node(n)%fullDepth, arrLink(j)%offset1 + arrLink(j)%xsect%yFull)
        end if
        n = arrLink(j)%node2
        if ( Node(n)%datatype /= E_STORAGE ) then
            Node(n)%fullDepth = MAX(Node(n)%fullDepth, arrLink(j)%offset2 + arrLink(j)%xsect%yFull)
        end if
    end if
!!  End of revisions  !!
end subroutine link_validate

!=============================================================================

subroutine link_initState(j)
!
!  Input:   j = link index
!  Output:  none
!  Purpose: initializes a link's state variables at start of simulation.
!
    use headers
    implicit none
    
    integer, intent(in) :: j
    integer ::   p

    ! --- initialize hydraulic state
    arrLink(j)%oldFlow   = arrLink(j)%q0
    arrLink(j)%newFlow   = arrLink(j)%q0
    arrLink(j)%oldDepth  = 0.0
    arrLink(j)%newDepth  = 0.0
    arrLink(j)%oldVolume = 0.0
    arrLink(j)%newVolume = 0.0
    arrLink(j)%isClosed  = .FALSE.
    arrLink(j)%setting   = 1.0                                                   !(5.0.010 - LR)
    arrLink(j)%targetSetting = 1.0                                               !(5.0.010 - LR)
    if ( arrLink(j)%datatype == E_CONDUIT ) call conduit_initState(j, arrLink(j)%subIndex)
    if ( arrLink(j)%datatype == E_PUMP    ) call pump_initState(j, arrLink(j)%subIndex)
    
    ! --- initialize water quality state
    do p =1, Nobjects(E_POLLUT)
        arrLink(j)%oldQual(p) = 0.0
        arrLink(j)%newQual(p) = 0.0
    end do
end subroutine link_initState

!=============================================================================

integer function link_readXsectParams(tok, ntoks)
!!
!!  Input:   char* tok() = array of string tokens
!!           ntoks = number of tokens   
!!  Output:  returns an error code
!!  Purpose: reads a link's cross section parameters from a tokenized
!!           line of input data.
!!
!    use headers
!    use swmm5f
!    use modXsect
!    implicit none
!    integer, intent(in) :: ntoks
!    character(*), dimension(:), intent(in) :: tok
!    
!    integer ::    i, j, k
!    double precision :: x(4)
!
!    ! --- get index of link
!    if ( ntoks < 6 ) then
!       link_readXsectParams = error_setInpError(ERR_ITEMS, '')
!       return
!    end if
!    j = project_findObject(LINK, tok(1))
!    if ( j < 0 ) then
!       link_readXsectParams = error_setInpError(ERR_NAME, tok(1))
!       return
!    end if
!
!    ! --- get code of xsection shape
!    k = findmatch(tok(2), XsectTypeWords)
!    if ( k < 0 ) then
!       link_readXsectParams = error_setInpError(ERR_KEYWORD, tok(2))
!       return
!    end if
!
!    ! --- assign default number of barrels to conduit
!    if ( arrLink(j)%datatype == E_CONDUIT ) Conduit(arrLink(j)%subIndex)%barrels = 1
!    
!    ! --- assume link is not a culvert                                        !(5.0.014 - LR)
!    arrLink(j)%xsect%culvertCode = 0                                             !(5.0.014 - LR)
!
!    ! --- for irregular shape, find index of transect object
!    if ( k == IRREGULAR ) then
!        i = project_findObject(E_TRANSECT, tok(3))
!        if ( i < 0 ) then
!           link_readXsectParams = error_setInpError(ERR_NAME, tok(3))
!           return
!        end if
!        arrLink(j)%xsect%datatype = k
!        arrLink(j)%xsect%transect = i
!    else
!        ! --- parse max. depth & shape curve for a custom shape               !(5.0.010 - LR)
!        if ( k == CUSTOM ) then                                                !(5.0.010 - LR)
!            if ( abs(getDouble(tok(3), x(1)) - 0) < P_TINY .or. x(1) <= 0.0 ) then       !(5.0.010 - LR) 
!               link_readXsectParams = error_setInpError(ERR_NUMBER, tok(2))                   !(5.0.010 - LR)
!               return
!            end if
!            i = project_findObject(E_CURVE, tok(4))                             !(5.0.010 - LR)
!            if ( i < 0 ) then
!               link_readXsectParams = error_setInpError(ERR_NAME, tok(4))           !(5.0.010 - LR)
!               return
!            end if
!            arrLink(j)%xsect%datatype = k                                            !(5.0.010 - LR)
!            arrLink(j)%xsect%transect = i                                        !(5.0.010 - LR)
!            arrLink(j)%xsect%yFull = x(1) / UCF(LENGTH)                          !(5.0.010 - LR) 
!        ! --- parse and save geometric parameters
!        else 
!            do i = 3, 6                                          !(5.0.010 - LR)
!               if ( abs(getDouble(tok(i), x(i-2)) - 0) < P_TINY ) then
!                   link_readXsectParams = error_setInpError(ERR_NUMBER, tok(i))
!                   return
!               end if
!            end do
!        end if
!        if ( .not. xsect_setParams(arrLink(j)%xsect, k, x, UCF(LENGTH)) ) then
!            link_readXsectParams = error_setInpError(ERR_NUMBER, '')
!            return
!        end if
!
!        ! --- parse number of barrels if present
!        if ( arrLink(j)%datatype == E_CONDUIT .and. ntoks >= 7 ) then
!            i = atoi(tok(7))
!            if ( i <= 0 ) then
!               link_readXsectParams = error_setInpError(ERR_NUMBER, tok(7))
!               return
!            else 
!               Conduit(arrLink(j)%subIndex)%barrels = i !(char)i
!            end if
!        end if
!
!        ! --- parse culvert code if present                                   !(5.0.014 - LR)
!        if ( arrLink(j)%datatype == E_CONDUIT .and. ntoks >= 8 ) then
!            read(tok(8), '(i)') i !i = atoi(tok(8))
!            if ( i < 0 ) then
!               link_readXsectParams = error_setInpError(ERR_NUMBER, tok(8))
!               return
!            else 
!               arrLink(j)%xsect%culvertCode = i
!            end if
!        end if
!    end if
    link_readXsectParams = 0
end function link_readXsectParams

!=============================================================================
!!  This function was re-named and re-written for release 5.0.014.  !!     !(5.0.014 - LR)
logical function link_setFlapGate( j,  n1,  n2,  q)
!
!  Input:   j = link index
!           n1 = index of node on upstream end of link
!           n2 = index of node on downstream end of link 
!           q = signed flow value (value and units don't matter) 
!  Output:  returns TRUE if there is reverse flow through a flap gate
!           associated with the link. 
!  Purpose: based on the sign of the flow, determines if a flap gate
!           associated with the link should close or not.
!
    use enums
    use consts
    use headers
    implicit none
    integer, intent(in) :: j, n1, n2
    double precision, intent(in) :: q
    
    integer :: n
     n = -1

    ! --- check for reverse flow through link's flap gate
    if ( arrLink(j)%hasFlapGate ) then
        if ( q * arrLink(j)%direction < 0.0 ) then
            link_setFlapGate = .true.
            return
        end if
    end if

    ! --- check for Outfall with flap gate node on inflow end of link
    if ( q < 0.0 ) n = n2
    if ( q > 0.0 ) n = n1
    if ( n >= 0 .and. &
        &Node(n)%datatype == E_OUTFALL .and. &
        &Outfall(Node(n)%subIndex)%hasFlapGate ) then
        link_setFlapGate = .true.
        return
     end if
     link_setFlapGate = .false.
     return
end function link_setFlapGate

!=============================================================================

double precision function link_getYcrit(j, q)
!
!  Input:   j = link index
!           q = link flow rate (cfs)
!  Output:  returns critical depth (ft)
!  Purpose: computes critical depth for given flow rate.
!
    use enums
    use consts
    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: q
   !link_getYcrit = xsect_getYcrit(&arrLink(j)%xsect, q)
    link_getYcrit = xsect_getYcrit(arrLink(j)%xsect, q)
end function link_getYcrit

!=============================================================================

double precision function link_getYnorm(j, q)
!
!  Input:   j = link index
!           q = link flow rate (cfs)
!  Output:  returns normal depth (ft)
!  Purpose: computes normal depth for given flow rate.
!
    use enums
    use consts
    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: q
    integer ::    k
    double precision :: s, a, y, mq
    mq = q

    if ( arrLink(j)%datatype /= E_CONDUIT ) then
        link_getYnorm = 0.0
        return
    end if
    if ( arrLink(j)%xsect%datatype == DUMMY ) then
        link_getYnorm = 0.0
        return
    end if
    mq = abs(mq)
    if ( mq <= 0.0 ) then
        link_getYnorm = 0.0
        return
    end if
    k = arrLink(j)%subIndex
    if ( mq > Conduit(k)%qMax ) then
        link_getYnorm = arrLink(j)%xsect%yFull
        return
    end if
    s = mq / Conduit(k)%beta
    a = xsect_getAofS(arrLink(j)%xsect, s) !xsect_getAofS(&arrLink(j)%xsect, s)
    y = xsect_getYofA(arrLink(j)%xsect, a) !xsect_getYofA(&arrLink(j)%xsect, a)
    link_getYnorm = y
    return
end function link_getYnorm

!=============================================================================

subroutine link_setOutfallDepth(j)
!
!  Input:   j = link index
!  Output:  none
!  Purpose: sets depth at outfall node connected to link j.
!
    use enums
    use consts
    use headers
    implicit none
    integer, intent(in) :: j
    integer ::     k                         ! conduit index
    integer ::     n                         ! outfall node index
    double precision ::  z                         ! invert offset height (ft)
    double precision :: q                         ! flow rate (cfs)
    double precision :: yCrit               ! critical flow depth (ft)
    double precision :: yNorm               ! normal flow depth (ft)
    !double precision :: link_getYnorm, link_getYcrit

    yCrit = 0.0
    yNorm = 0.0

    ! --- find which end node of link is an outfall
    if ( Node(arrLink(j)%node2)%datatype == E_OUTFALL ) then
        n = arrLink(j)%node2
        z = arrLink(j)%offset2
    else if ( Node(arrLink(j)%node1)%datatype == E_OUTFALL ) then
        n = arrLink(j)%node1
        z = arrLink(j)%offset1
    else 
        return
    end if
    
    ! --- find both normal & critical depth for current flow
    if ( arrLink(j)%datatype == E_CONDUIT ) then
        k = arrLink(j)%subIndex
        q = abs(arrLink(j)%newFlow / Conduit(k)%barrels)
        yNorm = link_getYnorm(j, q)
        yCrit = link_getYcrit(j, q)
    end if

    ! --- set new depth at node
    call node_setOutletDepth(n, yNorm, yCrit, z)
end subroutine link_setOutfallDepth

!=============================================================================

subroutine link_setParams(j, datatype, n1, n2, k, x)
!
!  Input:   j   = arrLink index
!           type = arrLink type code
!           n1   = index of upstream node
!           n2   = index of downstream node
!           k    = index of arrLink's sub-type
!           x    = array of parameter values
!  Output:  none
!  Purpose: sets parameters for a arrLink.
!
    use consts
    use enums
    use headers
    use swmm5f
    implicit none
    integer, intent(in) :: j, n1, n2, k, datatype
    double precision, dimension(:), intent(in) :: x
    
    double precision, dimension(:), allocatable :: lp

    arrLink(j)%node1       = n1
    arrLink(j)%node2       = n2
    arrLink(j)%datatype    = datatype
    arrLink(j)%subIndex    = k
    arrLink(j)%offset1     = 0.0
    arrLink(j)%offset2     = 0.0
    arrLink(j)%q0          = 0.0
    arrLink(j)%qFull       = 0.0
    arrLink(j)%setting     = 1.0
    arrLink(j)%targetSetting = 1.0       !(5.0.010 - LR)
    arrLink(j)%hasFlapGate = .false. !0
    arrLink(j)%qLimit      = 0.0         ! 0 means that no limit is defined
    arrLink(j)%direction   = 1

    select case (datatype)
      case (E_CONDUIT)
        Conduit(k)%clength    = x(1) / UCF(LENGTH)
        Conduit(k)%modLength = Conduit(k)%clength
        Conduit(k)%roughness = x(2)
        arrLink(j)%offset1      = x(3) / UCF(LENGTH)
        arrLink(j)%offset2      = x(4) / UCF(LENGTH)
        arrLink(j)%q0           = x(6) / UCF(FLOW)
        arrLink(j)%qLimit       = x(6) / UCF(FLOW)
        !break

      case (E_PUMP)
        Pump(k)%pumpCurve    = int(x(1))
        arrLink(j)%hasFlapGate  = .FALSE.
        Pump(k)%initSetting  = x(2)                                           !(5.0.010 - LR)
        Pump(k)%yOn          = x(3) / UCF(LENGTH)                             !(5.0.012 - LR)
        Pump(k)%yOff         = x(4) / UCF(LENGTH)                             !(5.0.012 - LR)
        Pump(k)%xMin         = 0.0                                            !(5.0.014 - LR)
        Pump(k)%xMax         = 0.0                                            !(5.0.014 - LR)
        !break

      case (E_ORIFICE)
        Orifice(k)%datatype      = int(x(1))
        arrLink(j)%offset1      = x(2) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Orifice(k)%cDisch    = x(3)
        
        !arrLink(j)%hasFlapGate  = (x(3) > 0.0) ? 1 : 0
        if (x(4) >0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        
        Orifice(k)%orate     = x(5) * 3600.0                                  !(5.0.010 - LR) 
        !break

      case (E_WEIR)
        Weir(k)%datatype         = int(x(1))
        arrLink(j)%offset1      = x(2) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Weir(k)%cDisch1      = x(3)
        !arrLink(j)%hasFlapGate  = (x(3) > 0.0) ? 1 : 0
        if (x(4) > 0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        Weir(k)%endCon       = x(5)
        Weir(k)%cDisch2      = x(6)
        !break

      case (E_OUTLET)
        arrLink(j)%offset1      = x(1) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Outlet(k)%qCoeff     = x(2)
        Outlet(k)%qExpon     = x(3)
        Outlet(k)%qCurve     = int(x(4))
        !arrLink(j)%hasFlapGate  = (x(4) > 0.0) ? 1 : 0
        if (x(5) > 0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        Outlet(k)%curveType  = int(x(6))                                      !(5.0.014 - LR)

        deallocate(lp)
        call xsect_setParams(arrLink(j)%xsect, DUMMY, lp, 0.0)
        !break

    end select
end subroutine link_setParams

!=============================================================================

!!  Function re-written to incorporate flap gate head loss.  !!            !(5.0.012 - LR)

double precision function orifice_getFlow(j, k,  head, f, hasFlapGate)
!
!  Input:   j = link index
!           k = orifice index
!           head = head across orifice
!           f = fraction of critical depth filled
!           hasFlapGate = flap gate indicator
!  Output:  returns flow through an orifice
!  Purpose: computes flow through an orifice as a function of head.
!
    use headers
    implicit none
    integer, intent(in) :: j, k
    double precision, intent(in) :: head, f
    logical(kind=K2), intent(in) :: hasFlapGate
    
    double precision :: area, q
    double precision :: veloc, hLoss

!    ! --- case where orifice is closed
!    if ( head == 0.0 .or. f <= 0.0  )                                            !(5.0.013 - LR)
!    {
!        arrLink(j).dqdh = 0.0
!        return 0.0
!    }
!
!    ! --- case where inlet depth is below critical depth
!    !     orifice behaves as a weir
!    else if ( f < 1.0 )
!    {
!        q = Orifice(k).cWeir * pow(f, 1.5)
!        arrLink(j).dqdh = 1.5 * q / (f * Orifice(k).hCrit)
!    }
!
!    ! --- case where normal orifice flow applies
!    else
!    {
!        q = Orifice(k).cOrif * sqrt(head)
!        arrLink(j).dqdh = q / (2.0 * head)
!    }
!
!    ! --- apply ARMCO adjustment for headloss from flap gate
!    if ( hasFlapGate )
!    {
!        ! --- compute velocity for current orifice flow
!        area = xsect_getAofY(&arrLink(j).xsect,
!                             arrLink(j).setting * arrLink(j).xsect.yFull)
!        veloc = q / area
!
!        ! --- compute head loss from gate
!        hLoss = (4.0 / GRAVITY) * veloc * veloc *
!                 exp(-1.15 * veloc / sqrt(head) )
!
!        ! --- update head (for orifice flow) 
!        !     or critical depth fraction (for weir flow)
!        if ( f < 1.0 )
!        {
!            f = f - hLoss/Orifice(k).hCrit
!            if ( f < 0.0 ) f = 0.0
!        }
!        else
!        {
!            head = head - hLoss
!            if ( head < 0.0 ) head = 0.0
!        }
!
!        ! --- make recursive call to this function, with hasFlapGate
!        !     set to false, to find flow values at adjusted head value
!        q = orifice_getFlow(j, k, head, f, FALSE)
!    }
!    return q
end function orifice_getFlow

!=============================================================================

!!  Function re-written to better handle bottom orifices.  !!              !(5.0.012 - LR)

double precision function orifice_getInflow(j)
!
!  Input:   j = link index
!  Output:  returns orifice flow rate (cfs)
!  Purpose: finds the flow through an orifice.
!
    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    integer ::    k, n1, n2
    double precision :: head, h1, h2, y1, dir
    double precision :: f
	double precision :: hcrest = 0.0
	double precision :: hcrown = 0.0
    double precision :: hmidpt
    double precision :: q, ratio                                                           !(5.0.019 - LR)

    ! --- get indexes of end nodes and link's orifice
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2
    k  = arrLink(j)%subIndex

    ! --- find heads at upstream & downstream nodes
    if ( RouteModel == DW ) then
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n2)%newDepth + Node(n2)%invertElev
    else
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n1)%invertElev
    end if
    !dir = (h1 >= h2) ? +1.0 : -1.0 
    if (h1 >= h2) then
       dir = 1.0
    else
       dir = -1.0
    end if
           
    ! --- exchange h1 and h2 for reverse flow
    y1 = Node(n1)%newDepth
    if ( dir < 0.0 ) then
        head = h1
        h1 = h2
        h2 = head
        y1 = Node(n2)%newDepth
    end if

    ! --- orifice is a bottom orifice (oriented in horizontal plane)
    if ( Orifice(k)%datatype == BOTTOM_ORIFICE ) then
        ! --- compute crest elevation
        hcrest = Node(n1)%invertElev + arrLink(j)%offset1

        ! --- compute head on orifice
        if (h1 < hcrest) then 
            head = 0.0
        else if (h2 > hcrest) then
            head = h1 - h2
        else 
            head = h1 - hcrest
        end if

        ! --- find fraction of critical height for which weir flow occurs
        f = head / Orifice(k)%hCrit
        f = MIN(f, 1.0)
    ! --- otherwise orifice is a side orifice (oriented in vertical plane)
    else
        ! --- compute elevations of orifice crest and crown
        hcrest = Node(n1)%invertElev + arrLink(j)%offset1
        hcrown = hcrest + arrLink(j)%xsect%yFull * arrLink(j)%setting
        hmidpt = (hcrest + hcrown) / 2.0
    
        ! --- compute degree of inlet submergence
        if ( h1 < hcrown .and. hcrown > hcrest ) then
            f = (h1 - hcrest) / (hcrown - hcrest)
        else 
            f = 1.0
        end if

        ! --- compute head on orifice
        if ( f < 1.0 ) then
            head = h1 - hcrest
        else if ( h2 < hmidpt ) then
            head = h1 - hmidpt
        else
            head = h1 - h2
        end if
    end if

    ! --- return if head is negligible or flap gate closed
    if ( head <= FUDGE .or. y1 <= FUDGE .or. &
        &link_setFlapGate(j, n1, n2, dir) ) then                               !(5.0.014 - LR)
        arrLink(j)%newDepth = 0.0
        arrLink(j)%flowClass = DRY
        Orifice(k)%surfArea = FUDGE * Orifice(k)%length
        arrLink(j)%dqdh = 0.0
        orifice_getInflow = 0.0
        return
    end if

    ! --- determine flow class
    arrLink(j)%flowClass = SUBCRITICAL
    if ( hcrest > h2 ) then
        if ( dir == 1.0 ) then
            arrLink(j)%flowClass = DN_CRITICAL
        else
            arrLink(j)%flowClass = UP_CRITICAL
        end if
    end if

    ! --- compute flow depth and surface area
    y1 = arrLink(j)%xsect%yFull * arrLink(j)%setting
    if ( Orifice(k)%datatype == SIDE_ORIFICE ) then
        arrLink(j)%newDepth = y1 * f
        Orifice(k)%surfArea = xsect_getWofY(arrLink(j)%xsect, arrLink(j)%newDepth) * Orifice(k)%length
    else
        arrLink(j)%newDepth = y1
        Orifice(k)%surfArea = xsect_getAofY(arrLink(j)%xsect, y1)
    end if

!!  Following segment modified for release 5.0.019.  !!                    !(5.0.019 - LR)
    ! --- find flow through the orifice
    q = dir * orifice_getFlow(j, k, head, f, arrLink(j)%hasFlapGate)

    ! --- apply Villemonte eqn. to correct for submergence
    if ( f < 1.0 .and. h2 > hcrest ) then
        ratio = (h2 - hcrest) / (h1 - hcrest)
        q = q * ( (1.0 - (ratio ** 1.5)) ** 0.385)
    end if
    orifice_getInflow = q
end function orifice_getInflow

!=============================================================================

double precision function outlet_getFlow(k, head)
!
!  Input:   k    = outlet index
!           head = head across outlet (ft)
!  Output:  returns outlet flow rate (cfs)
!  Purpose: computes flow rate through an outlet given head.
!
    use headers
    implicit none
    
    integer, intent(in) :: k
    double precision, intent(in) :: head
    integer ::    m
    double precision :: h

!    ! --- convert head to original units
!    h = head * UCF(LENGTH)
!
!    ! --- look-up flow in rating curve table if provided
!    m = Outlet(k)%qCurve
!    if ( m >= 0 ) then
!       outlet_getFlow = table_lookup(Curve(m), h) / UCF(FLOW)
!    ! --- otherwise use function to find flow
!    else 
!       outlet_getFlow = Outlet(k)%qCoeff * (h ** Outlet(k)%qExpon) / UCF(FLOW)
!    end if
end function outlet_getFlow

!=============================================================================

double precision function outlet_getInflow(j)
!
!  Input:   j = link index
!  Output:  outlet flow rate (cfs)
!  Purpose: finds the flow through an outlet.
!
    use headers
    implicit none
    integer, intent(in) :: j
    integer ::    k, n1, n2
    double precision :: head, hcrest, h1, h2, y1, dir

    ! --- get indexes of end nodes
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2
    k  = arrLink(j)%subIndex

    ! --- find heads at upstream & downstream nodes
    if ( RouteModel == DW ) then
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n2)%newDepth + Node(n2)%invertElev
    else
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n1)%invertElev
    end if
    !dir = (h1 >= h2) ? +1.0 : -1.0 
    if (h1 >= h2) then
      dir = 1.0
    else
      dir = -1.0
    end if

    ! --- exchange h1 and h2 for reverse flow
    y1 = Node(n1)%newDepth
    if ( dir < 0.0 ) then
        h1 = h2
        y1 = Node(n2)%newDepth
    end if

    ! --- for a NODE_DEPTH rating curve the effective head across the
    !     outlet is the depth above the crest elev. while for a NODE_HEAD
    !     curve it is the difference between upstream & downstream heads
    hcrest = Node(n1)%invertElev + arrLink(j)%offset1                            !(5.0.012 - LR)
    if ( Outlet(k)%curveType == NODE_HEAD .and. RouteModel == DW ) then          !(5.0.014 - LR)
        head = h1 - MAX(h2, hcrest)                                           !(5.0.014 - LR)
    else 
        head = h1 - hcrest                                                   !(5.0.014 - LR)
    end if

    ! --- no flow if either no effective head difference,
    !     no upstream water available, or closed flap gate
    if ( head <= FUDGE .or. y1 <= FUDGE .or. &
        &link_setFlapGate(j, n1, n2, dir) ) then                               !(5.0.014 - LR)
        arrLink(j)%newDepth = 0.0
        arrLink(j)%flowClass = DRY
        outlet_getInflow = 0.0
        return
    end if

    ! --- otherwise use rating curve to compute flow
    arrLink(j)%newDepth = head
    arrLink(j)%flowClass = SUBCRITICAL
    outlet_getInflow = dir * arrLink(j)%setting * outlet_getFlow(k, head)
end function outlet_getInflow

!=============================================================================

double precision function weir_getInflow(j)
!
!  Input:   j = link index
!  Output:  returns weir flow rate (cfs)
!  Purpose: finds the flow over a weir.
!
    use headers
    use modXsect
    implicit none
    integer, intent(in) :: j
    integer ::    n1          ! index of upstream node
    integer ::    n2          ! index of downstream node
    integer ::    k           ! index of weir
    double precision :: q1          ! flow through central part of weir (cfs)
    double precision :: q2          ! flow through end sections of weir (cfs)
    double precision :: head        ! head on weir (ft)
    double precision :: h1          ! upstrm nodal head (ft)
    double precision :: h2          ! downstrm nodal head (ft)
    double precision :: hcrest      ! head at weir crest (ft)
    double precision :: hcrown      ! head at weir crown (ft)
    double precision :: y           ! water depth in weir (ft)
    double precision :: dir         ! direction multiplier
    double precision :: ratio
    double precision, dimension(4) :: weirPower = (/ 1.5,&      ! transverse weir
                         &5./3.,&    ! side flow weir
                         &2.5,&      ! v-notch weir
                         &1.5 /)      ! trapezoidal weir

    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2
    k  = arrLink(j)%subIndex
    if ( RouteModel == DW ) then
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n2)%newDepth + Node(n2)%invertElev
    else
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n1)%invertElev
    end if
    !dir = (h1 > h2) ? +1.0 : -1.0            
    if (h1 > h2) then
       dir = 1.0
    else
       dir = -1.0
    end if

    ! --- exchange h1 and h2 for reverse flow
    if ( dir < 0.0 ) then
        head = h1
        h1 = h2
        h2 = head
    end if

    ! --- find head of weir's crest and crown
    hcrest = Node(n1)%invertElev + arrLink(j)%offset1                            !(5.0.012 - LR)
    hcrown = hcrest + arrLink(j)%xsect%yFull

    ! --- adjust crest ht. for partially open weir
    hcrest = hcrest + (1.0 - arrLink(j)%setting) * arrLink(j)%xsect%yFull

    ! --- compute head relative to weir crest
    head = h1 - hcrest

    ! --- return if head is negligible or flap gate closed
    arrLink(j)%dqdh = 0.0
    if ( head <= FUDGE .or. hcrest >= hcrown .or. link_setFlapGate(j, n1, n2, dir) ) then                                !(5.0.014 - LR)
        arrLink(j)%newDepth = 0.0
        arrLink(j)%flowClass = DRY
        weir_getInflow = 0.0
        return
    end if

    ! --- determine flow class
    arrLink(j)%flowClass = SUBCRITICAL
    if ( hcrest > h2 ) then
        if ( dir == 1.0 ) then
           arrLink(j)%flowClass = DN_CRITICAL
        else
           arrLink(j)%flowClass = UP_CRITICAL
        end if
    end if

    ! --- compute new equivalent surface area
    y = arrLink(j)%xsect%yFull - (hcrown - MIN(h1, hcrown))
    Weir(k)%surfArea = xsect_getWofY(arrLink(j)%xsect, y) * Weir(k)%length

    ! --- if under surcharge condition then use equiv. orifice eqn.
    if ( h1 >= hcrown ) then
        y = (hcrest + hcrown) / 2.0                                           !(5.0.010 - LR)
        if ( h2 < y ) then
           head = h1 - y                                           !(5.0.010 - LR)
        else 
           head = h1 - h2                                          !(5.0.010 - LR)
        end if
        y = hcrown - hcrest                                                   !(5.0.012 - LR)
        q1 = weir_getOrificeFlow(j, head, y, Weir(k)%cSurcharge)              !(5.0.012 - LR)
        arrLink(j)%newDepth = y                                                  !(5.0.012 - LR)
        weir_getInflow = dir * q1
        return
    end if

    ! --- otherwise use weir eqn. to find flows through central (q1)
    !     and end sections (q2) of weir, q1 and q2 are inout
    call weir_getFlow(j, k, head, dir, arrLink(j)%hasFlapGate, q1, q2)              !(5.0.012 - LR)

    ! --- apply Villemonte eqn. to correct for submergence
    if ( h2 > hcrest ) then
        ratio = (h2 - hcrest) / (h1 - hcrest)
        q1 = q1 * ( (1.0 - (ratio ** weirPower(Weir(k)%datatype))) ** 0.385)
        if ( q2 > 0.0 ) &
           &q2 = q2 * ( (1.0 - (ratio ** weirPower(VNOTCH_WEIR))) ** 0.385)
    end if

    ! --- return total flow through weir
    arrLink(j)%newDepth = h1 - hcrest                                             !(5.0.011 - LR)
    weir_getInflow = dir * (q1 + q2)
end function weir_getInflow

!=============================================================================

!!  New function added to compute flow thru surcharged weir. !!            !(5.0.012 - LR)

double precision function weir_getOrificeFlow(j, head, y, cOrif)
!
!  Input:   j = link index
!           head = head across weir (ft)
!           y = height of upstream water level above weir crest (ft)
!           cOrif = orifice flow coefficient
!  Output:  returns flow through weir
!  Purpose: finds flow through a surcharged weir using the orifice equation.
!
    use headers
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: head, y, cOrif
    
    double precision :: a, q, v, hloss

!    ! --- evaluate the orifice flow equation
!    q = cOrif * sqrt(head)
!
!    ! --- apply Armco adjustment if weir has a flap gate
!    if ( arrLink(j).hasFlapGate )
!    {
!        a = weir_getOpenArea(j, y)
!        if ( a > 0.0 )
!        {
!            v = q / a
!            hloss = (4.0 / GRAVITY) * v * v * exp(-1.15 * v / sqrt(y) )
!            head -= hloss
!            head = MAX(head, 0.0)
!            q = cOrif * sqrt(head)
!        }
!    }
!    if ( head > 0.0 ) arrLink(j).dqdh = q / (2.0 * head)
!    else arrLink(j).dqdh = 0.0
!    return q
end function weir_getOrificeFlow

end module modLink
