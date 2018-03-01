module DataSizeSpecs

integer, parameter :: K2 = selected_int_kind(2) !kind= 1
integer, parameter :: K4 = selected_int_kind(4) !kind= 2
integer, parameter :: K8 = selected_int_kind(8)  !kind =4
!integer, parameter :: dp = selected_real_kind(15, 307)
integer, parameter :: dp = kind(1.d0)
end module