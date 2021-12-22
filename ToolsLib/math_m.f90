!>
!! @author
!!   MA Gang
!!   magang@hrbeu.edu.cn    
!! @version A60514   
!! @revision 20210622
!!    
    

!    
module math_m

    !implicit none
    
    real(8) :: pi = 3.141592653589793d0
    
    !private MatrixInverse
    
    interface plus
    
        procedure :: selfPlus_int
        procedure :: selfPlus_real
        procedure :: selfPlus_double
        
        procedure :: selfPlus_int_array
        procedure :: selfPlus_real_array
        procedure :: selfPlus_double_array
        procedure :: selfPlus_double_matrix
        procedure :: selfPlus_double_matrix3
        
    end interface
    
    
    interface toDeg
        procedure :: toDeg_scalar
        procedure :: toDeg_array
    end interface
    
    
    interface toRad
        procedure :: toRad_scalar
        procedure :: toRad_array    
    end interface
    
    
    interface trapz
        procedure :: trapezoid
        procedure :: trapezoid_1
    end interface
    
    
    interface zero
        procedure :: zero_array
        procedure :: zero_matrix
        procedure :: zero_matrix3        
    end interface
    
    
    interface zeroInt
        procedure :: zeroInt_array
        procedure :: zeroInt_matrix
        procedure :: zeroInt_matrix3        
    end interface
    
    
    interface zero_cx
        procedure :: zero_array_cx
        procedure :: zero_matrix_cx
    end interface    
	
    
    interface modulus
        procedure :: modulusMatrix
		procedure :: modulusVector
    end interface	    
    
    
    contains
    
    !>
    !! return the normalized vector whose modulus equals one.
    !! A = A/|A|
    !!
    function normalize(vector) result(unitVector)
        implicit none
        real(8) :: vector(:)
        real(8), allocatable :: unitVector(:)
        real(8) :: modulusValu
        
        modulusValu = modulus(vector)
        if(modulusValu==0.0d0) modulusValu = 1.0d0
        unitVector = vector/modulusValu
        
    end function  
	
	
    function modulusMatrix(matrix)
        implicit none
        real(8), intent(in) :: matrix(:,:)
        real(8) :: modulusMatrix
        modulusMatrix = modulus(matrix(:,1))
        return
    end function   	
	
    
    !>
    !! function for Calculating modulus of a vector. |A|
    !!
    !!   For example in 3-D:
    !!   VectorModulus = sqrt( vector(1)*vector(1) 
    !!                        +vector(2)*vector(2) 
    !!                        +vector(3)*vector(3))
    !!
    !!   return |vector|
    !!  
    function modulusVector(vector) result(valu)
        implicit none
        real(8), intent(in) :: vector(:)
        real(8) :: valu
        valu = sqrt(sum(vector(:)*vector(:)))
        return
    end function   
    
    
    function modulusSquare(vector) result(valu)
        implicit none
        real(8), intent(in) :: vector(:)
        real(8) :: valu
        valu = sum(vector(:)*vector(:))
        return
    end function       
    
    !>
    !! cross product for two column vector in 3-dimension.
    !!   C = A x B
    !!
    !! A new vector will be returned.
    !!  
    function crossProduct(vectorA,vectorB) result (vectorC)
        implicit none
        real(8), intent(in) :: vectorA(3)
        real(8), intent(in) :: vectorB(3)
        real(8) :: vectorC(3)

        vectorC(1) = vectorA(2)*vectorB(3)-vectorA(3)*vectorB(2)
        vectorC(2) = vectorA(3)*vectorB(1)-vectorA(1)*vectorB(3)
        vectorC(3) = vectorA(1)*vectorB(2)-vectorA(2)*vectorB(1)
    !
    end function       
    
    !>
    !! outer product for two column vector in 3-dimension.
    !!   C = A Ox B
    !!
    !! A matrix will be returned.
    !!  
    function outerProduct(vectorA,vectorB) result (C)
        implicit none
        real(8), intent(in) :: vectorA(3)
        real(8), intent(in) :: vectorB(3)
        real(8) :: C(3,3)

        C = 0.0d0
        C(1,1) = vectorA(1)*vectorB(1)
        C(2,2) = vectorA(2)*vectorB(2)
        C(3,3) = vectorA(3)*vectorB(3)
    !
    end function       
    
    !>
    !! Identity matrix.
    !!
    function eye(numDim) result (matrix)
        implicit none
        integer, intent(in) :: numDim
        real(8) :: matrix(numDim, numDim)
        ! temp
        integer :: i
        
        matrix = 0.0d0
        do i = 1, numDim
            matrix(i,i) = 1.0d0
        end do
    end function
    
    !>
    !! To get the inverse of the square matrix.
    !!
    function inv(matrix) result(inverse)
        implicit none
        integer*4 :: norder
        real(8), intent(in)  :: matrix(:,:)
        real(8), allocatable :: matrixnew(:,:)
        real(8), allocatable :: inverse(:,:)
        
        norder = size(matrix, 1)
        allocate(inverse(norder, norder))
        allocate(matrixnew, source=matrix)  ! to keep matrix not changed.
        inverse = matrixinverse(matrixnew, norder)
        
    end function
    
    
    function MatrixInverse(Matrix, nOrder) result(Inverse)
    !
    !     This subroutine finds the inverse of Matrix using a simplied
    !       version of Gauss-Jordan elimination and places this inverse
    !       into the matrix Inverse
    !
          integer*4 iRow                ! row of matrix or vector
          integer*4 jCol                ! column of matrix
          integer*4 nPivot              ! index of the cell in Matrix used for pivoting
          integer*4 nOrder              ! the dimension of Matrix
    !
          real(8) Matrix (nOrder,nOrder) ! the matrix to be inverted
          real(8) Inverse(nOrder,nOrder) ! the inverse of Matrix
          real(8) Term                   ! a term used in the pivoting process
          real(8) Pivot                  ! pivot value used for current elimination step
    !
    !     Set matrix Inverse equal to the identity matrix
    !
          Inverse(:,:) = 0.0d+00
          do iRow=1,nOrder
             Inverse(iRow,iRow) = 1.0d+00
          end do
    !
    !     Note: the algorithm below is the simplest but not the most accurate
    !       scheme.  However, the more accurate form of Gauss-Jordan elimination
    !       involves finding the largest element available for pivoting. It
    !       involves more coding and makes a slower routine.  As long as element
    !       Matrix(1,1) is not zero and the matrix is reasonably well conditioned,
    !       this routine is usually successful.
    !
          do nPivot=1,nOrder
    !
    !     Normalize pivot row
    !
             Pivot = Matrix(nPivot,nPivot)
             Matrix(nPivot,:)  = Matrix (nPivot,:)/Pivot
             Inverse(nPivot,:) = Inverse(nPivot,:)/Pivot
    !
    !     Pivot about each row except pivot row itself
    !
             do iRow=1,nOrder
                Term = Matrix(iRow,nPivot)
                if (iRow .ne. nPivot) then
                   Matrix(iRow,:)  =   Matrix(iRow   ,:) &
                                     - Matrix(nPivot ,:)*Term
                   Inverse(iRow,:) =   Inverse(iRow  ,:) &
                                     - Inverse(nPivot,:)*Term
                end if
             end do
          end do
    !
          return
    end function    
    
    !
    ! This subroutine finds the inverse of Matrix using a simplied
    !   version of Gauss-Jordan elimination and places this inverse
    !   into the matrix Inverse
    !
    ! Note: the algorithm below is the simplest but not the most accurate
    !   scheme.  However, the more accurate form of Gauss-Jordan elimination
    !   involves finding the largest element available for pivoting. It
    !   involves more coding and makes a slower routine.  As long as element
    !   Matrix(1,1) is not zero and the matrix is reasonably well conditioned,
    !   this routine is usually successful.
    ! 
    !function inv(MatrixIn) result(Inverse)
    !    implicit real*8 (a-h,o-z)
    !    real(8), intent(in) :: MatrixIn(:,:)
    !    real(8), allocatable :: Inverse(:,:) 
    !    
    !    real(8), allocatable :: Matrix(:,:)
    !    
    !    Matrix = MatrixIn
    !
    !    ! Set matrix Inverse equal to the identity matrix
    !    nOrder = size(Matrix,1)
    !    Inverse(:,:) = eye(nOrder)
    !
    !    do i=1,nOrder
    !        ! Normalize pivot row
    !        Pivot = Matrix(i,i)
    !        Matrix(i,:)  = Matrix (i,:)/Pivot
    !        Inverse(i,:) = Inverse(i,:)/Pivot
    !
    !        ! Pivot about each row except pivot row itself
    !        do iRow=1,nOrder
    !            Term = Matrix(iRow,i)
    !            if (iRow .ne. i) then
    !                Matrix(iRow,:) = Matrix(iRow,:) - Matrix(i ,:)*Term
    !                Inverse(iRow,:) = Inverse(iRow,:) - Inverse(i,:)*Term
    !            end if
    !        end do
    !    end do
    !
    !    return
    !end function        
    
    
    ! return the location when the first time x great than array value
    function getLoc(A,x) result(row)
        implicit none
        real(8), intent(in) :: A(:)
        real(8), intent(in) :: x
        integer :: rows( size(A) )
        integer :: row
        
        rows = 0
        rows = maxloc(A, mask=(A<=x))
        row  = rows(1)
    end function
    
    
    subroutine init_random_seed()
        integer :: i,n,clock
        integer,dimension(:),allocatable :: seed
        call random_seed(size=n)
        allocate(seed(n))
        call system_clock(count=clock)
        seed=clock+37*(/(i-1,i=1,n)/)
        call random_seed(PUT=seed) 
        deallocate(seed)
    end subroutine    
    
    
    ! The greatest common divisor (gcd) of two or more integers, 
    ! when at least one of them is not zero, is the largest positive integer 
    ! that divides the numbers without a remainder. 
    ! For example, the GCD of 8 and 12 is 4.    
    function gcd(a, b) 
        implicit none
        integer, intent(in) :: a, b
        integer :: gcd
        ! temp
        integer :: q, r
        integer :: rem

        if(a>b) then
            q = a
            r = b
        else
            q = b
            r = a
        end if

        gcd = 0
        if(r==0) then
            write(*,*) "The number 0 is not allowed."
        end if
        
        do while(1)
	        rem = mod(q,r)
            q = r    
            if(rem==0) then
                gcd = r
                exit
            else
                r = rem
            end if
        end do
    
    end function    
    
    
    ! least common multiple (LCM) of a and b
    ! Attention, biggest value is 2147483647 for integer 
    function lcm(a, b)
        implicit none
        integer, intent(in) :: a, b
        integer :: lcm
        ! temp
        integer :: g
        
        lcm = 0
        g = gcd(a, b)
        if(g /= 0) then
            lcm = a/g*b
        end if
        
    end function
    
    
    ! least common multiple (LCM) of array
    function lcms(Array)
        implicit none
        integer, intent(in) :: Array(:)  
        integer :: lcms
        ! temp
        integer :: i, num
        integer :: a, b
        
        lcms = 0
        num = size(array)
        if(num==1) then
            lcms = Array(1)
            return
        end if
        
        a = array(1)
        do i = 2,num
            b = array(i)
            a = lcm(a,b)
        end do
        lcms = a
    end function
    
    
    ! The root mean square is also known by its initial RMS (or rms), 
    !   and as the quadratic mean.
    ! http://rosettacode.org/wiki/Averages/Root_mean_square
    !
    function rms(x) 
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: rms
        
        rms = sqrt( sum(x**2)/size(x) )
    end function    
    
    
    function mean(x) 
        implicit none
        real(8) :: x(:)
        real(8) :: mean
        
        mean = sum(x)/size(x)
    end function   
    
    
    ! variance
    function var(x) 
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: var
        real(8) :: valu
        
        valu = mean(x)
        var = sum((x-valu)**2 )/size(x)
    end function     
    
    
    ! Standard deviation
    function sd(x) 
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: sd
        
        sd = sqrt(var(x) )
    end function    
    
    
    function toDeg_scalar(x) result(xNew)
        implicit none
        real(8), intent(in) :: x
        real(8) :: xNew
        
        xNew = x*180.0d0/pi
    end function    
    
    
    function toRad_scalar(x) result(xNew)
        implicit none
        real(8), intent(in) :: x
        real(8) :: xNew
        
        xNew = x*pi/180.0d0
    end function     
    
    
    function toDeg_array(x) result(xNew) 
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: xNew(size(x))
        
        xNew = x*180.0d0/pi
    end function      
    
    
    function toRad_array(x) result(xNew) 
        implicit none
        real(8), intent(in) :: x(:)
        real(8) :: xNew(size(x))
        
        xNew = x*pi/180.0d0
    end function      
    
    
    function trapezoid(x, y) result(f)
        implicit none
        real(8), intent(in) :: x(:)
        real(8), intent(in) :: y(:)
        real(8) :: f, dx
        integer :: i,n
        
        n = size(x)
        f = 0.
        do i = 1, n-1
            dx = x(i+1) - x(i)
            f = f + ( y(i+1) + y(i) ) * dx 
        end do
        f = f/2.0
    end function    
    
    
    function trapezoid_1(dx, y) result(f)
        implicit none
        real(8), intent(in) :: dx
        real(8), intent(in) :: y(:)
        real(8) :: f
        integer :: n  ! n>=4
        
        n = size(y)
        f = sum( y(2:n-1) )
        f = f*2.0 + y(1) + y(n)
        f = f*dx/2.0
    end function
    
    
    function zeroInt_array(num) result(A)
        implicit none
        integer, intent(in) :: num
        integer :: A(num)
        
        A = 0
    end function
    
    
    function zeroInt_matrix(row, col) result(A)
        implicit none
        integer :: row, col
        integer :: A(row, col)
        
        A = 0
    end function   
    
    
    function zeroInt_matrix3(row, col, col3) result(A)
        implicit none
        integer, intent(in) :: row, col, col3
        integer :: A(row, col, col3)
        
        A = 0
    end function
    
    
    function zero_array(num) result(A)
        implicit none
        integer, intent(in) :: num
        real(8) :: A(num)
        
        A = 0.0d0
    end function
    
    
    function zero_matrix(row, col) result(A)
        implicit none
        integer, intent(in) :: row, col
        real(8) :: A(row, col)
        
        A = 0.0d0
    end function   
    
    
    function zero_matrix3(row, col, col3) result(A)
        implicit none
        integer, intent(in) :: row, col, col3
        real(8) :: A(row, col, col3)
        
        A = 0.0d0
    end function    
    
    
    function zero_array_cx(num) result(A)
        implicit none
        integer, intent(in) :: num
        complex(8) :: A(num)
        
        A = 0.0d0
    end function
    
    
    function zero_matrix_cx(row, col) result(A)
        implicit none
        integer, intent(in) :: row, col
        complex(8) :: A(row, col)
        
        A = 0.0d0
    end function     
    
    
    subroutine selfPlus_int(a, b)
        implicit none
        integer :: a, b
        a = a + b
    end subroutine   
    
    
    subroutine selfPlus_real(a, b)
        implicit none
        real :: a, b
        a = a + b
    end subroutine    
    
    
    subroutine selfPlus_double(a, b)
        implicit none
        real(8) :: a, b
        a = a + b
    end subroutine
    
    
    subroutine selfPlus_real_array(a, b)
        implicit none
        real :: a(:), b(:)
        a = a + b
    end subroutine  
    
    
    subroutine selfPlus_double_array(a, b)
        implicit none
        real(8) :: a(:), b(:)
        a = a + b
    end subroutine    
    
    
    subroutine selfPlus_int_array(a, b)
        implicit none
        integer :: a(:), b(:)
        a = a + b
    end subroutine     
    
    
    subroutine selfPlus_double_matrix(a, b)
        implicit none
        real(8) :: a(:,:), b(:,:)
        a = a + b
    end subroutine     
    
    subroutine selfPlus_double_matrix3(a, b)
        implicit none
        real(8) :: a(:,:,:), b(:,:,:)
        a = a + b
    end subroutine  
    
    ! 字符串转数字
    function toInt(str1) result(num)
        implicit none
        character(*) :: str1
        integer :: num
        
        read(str1,"(i)") num
        
    end function
    
end module

