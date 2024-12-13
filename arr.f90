program array_example
implicit none
    
integer, dimension(500) ::onedim
integer, dimension(10) ::resultarray
real, dimension(5):: realarray
integer :: i,j
integer, dimension(10,10):: twodim


onedim(1) = 999
do i = 1,10
    do j = 1,10
    twodim(i,j)=i
    end do

end do
!print*,onedim
print*,twodim
end program array_example
