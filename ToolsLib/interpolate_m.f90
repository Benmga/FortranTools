!! @author
!!   MA Gang
!!   magang@hrbeu.edu.cn 
    
module interpolate_m
    implicit none
    private
    
    public :: interpolate
    interface interpolate
        procedure :: get_A
        procedure :: get_B    
    end interface

    
    contains
     
    ! 线性插值
    function get_A(x,x0,x1,F0,F1) result(F)
        implicit none
        real(8) :: x      ! x0 < x < x1
        real(8) :: x0, x1
        real(8) :: F0, F1
        real(8) :: F      ! return
        real(8) :: A, B
        
        if(x1==x0) then
            F = F0
            return
        endif
        
        A = (x1-x)/(x1-x0)
        B = 1.0d0 - A
        F = A*F0 + B*F1
        
    end function
    
    
    ! 曲线插值
    function get_B(u, x, y) result(v)
        implicit none
        real(8) :: u
        real(8) :: x(:)
        real(8) :: y(:)        
        real(8) :: v
        
        v = splint(u, x, y, size(x))    
    end function
    

    function splint(u, x, y, n)
        implicit none
        real(8) splint
        integer n
        real(8) u, x(n), y(n)
        integer i, j, k
        real(8) dx
        real(8) b

        ! if u is ouside the x() interval take a boundary value (left or right)
        if(u <= x(1)) then
          splint = y(1)
          return
        end if
        if(u >= x(n)) then
          splint = y(n)
          return
        end if

        !*
        !  binary search for for i, such that x(i) <= u <= x(i+1)
        !*
        i = 1
        j = n+1
        do while (j > i+1)
          k = (i+j)/2
          if(u < x(k)) then
            j=k
            else
            i=k
           end if
        end do
        !*
        !  evaluate spline interpolation
        !*
        b = (y(i+1)-y(i))/(x(i+1)-x(i))
        dx = u - x(i)
        splint = y(i) + dx*b
    end function      
    
end module