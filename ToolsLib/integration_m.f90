module integration_m
    
    implicit none
    private
    public integrate
    
    ! ����Fortran2003�Ĺ��������������ʽ����Ҫ������˷�ʽ��
    abstract interface
        function func_f(x) result(matrix)   
            implicit none
            real(8) :: x
            real(8), allocatable :: matrix(:,:)
            
        end function
    end interface
    
    ! Gauss
    integer :: numGS = 5
    real(8) :: GSPoints(5), GSWeights(5)
    
    
    data GSPoints / -9.0617984593866407D-01, &
                   -5.3846931010568300D-01, &
                   -0.0000000000000000D+00, &
                    5.3846931010568300D-01, &
                    9.0617984593866407D-01  /

    data GSWeights / 2.3692688505618897D-01, &
                    4.7862867049936664D-01, &
                    5.6888888888888889D-01, &
                    4.7862867049936664D-01, &
                    2.3692688505618897D-01  /  
    
    contains
    
    
    
    function integrate(func,a,b) result(sums)
        implicit none
        procedure(func_f) :: func  ! ����external��
        real(8), optional :: a, b
        real(8), allocatable :: sums(:,:)
    
        real(8) :: coeLimitA, coeLimitB
        real(8) :: lowerLimit, upperLimit
        integer :: i
        real(8) :: x


        if(present(a) .and. present(b)) then
            lowerLimit = a
            upperLimit = b
        else
            lowerLimit = 0.0d0
            upperLimit = 1.0d0  
        end if
    
        coeLimitA  = (upperLimit-lowerLimit)/2.0d0
        coeLimitB  = (upperLimit+lowerLimit)/2.0d0
    
        do i = 1, numGS
            x = coeLimitA*GSPoints(i) + coeLimitB
            if(i==1) then
                ! ��һ������Ҫ��ͨ����ֵ����̬�ڴ�
                sums = func(x)*GSWeights(i)
            else
                sums = sums + func(x)*GSWeights(i)
            end if
        end do      
        sums = sums/2.0d0
    end function    

    
end module