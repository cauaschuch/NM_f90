program linear_regression
    implicit none

    ! Parameters
    integer, parameter :: n = 5  
    integer, parameter :: p = 2  

    ! Variables
    real(8), dimension(n, p) :: X  ! Pre
    real(8), dimension(n)     :: y  ! Ob
    real(8), dimension(p, p) :: XT_X,XT_X_inv  !  X
    real(8), dimension(p)     :: XT_y  
    real(8), dimension(p)     :: beta  !
    integer :: i, j

    !  2X-X**2
    X(:, 1) = [1.,2.,3.,4.,5.] 
    X(:, 2) = [1.,4.,9.,16.,25.]  
    y = [1.98, 0.02,-2.8, -8.3,-15.] 

    
    XT_X = matmul(transpose(X), X)

    XT_y = matmul(transpose(X), y)
    call matrix_inverse(XT_X, XT_X_inv)
    beta = matmul(XT_X_inv, XT_y)
    print *,beta(1)
    print *,beta(2)

contains
    subroutine matrix_inverse(A, A_inv)
        real(8), intent(in) :: A(2, 2)
        real(8), intent(out) :: A_inv(2, 2)
        real(8) :: det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)

        if (abs(det) < 1.0E-12) then
            print *, "matriz singular"
            stop
        end if

        A_inv(1, 1) =  A(2, 2) / det
        A_inv(1, 2) = -A(1, 2) / det
        A_inv(2, 1) = -A(2, 1) / det
        A_inv(2, 2) =  A(1, 1) / det
    end subroutine matrix_inverse

end program linear_regression
