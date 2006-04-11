subroutine MySub (Ivar, Rvar)
	implicit none
	dll_export :: MySub  ! export subroutine name  
	!
	! variable declarations
	!
	integer, intent (in out) :: Ivar
	real, intent (in out) :: Rvar
	!
	!rvar = ivar
	! executable statements
	!
end subroutine MySub

function MyFun (Ivar, Rvar) result (Ires)
	implicit none
	dll_export :: MyFun  ! export function name
	!
	! variable declarations
	!
	integer :: Ires
	integer, intent (in out) :: Ivar
	real, intent (in out) :: Rvar
	!
	! executable statements
	!
	Ires = 0   ! Assign function result 
end function