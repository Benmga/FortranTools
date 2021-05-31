! version 20210507
    
module operator_m
    use math_m
    implicit none
    private
    
    ! interface operator
    public :: operator(.x.)    
    public :: operator(.xt.)
    public :: operator(.tx.)
    public :: operator(.i.)
    public :: operator(.ix.)
    public :: operator(.xi.)
    public :: operator(.t.)
    public :: operator(.abs.)
    public :: outer_product
    public :: skew
    
    ! defination
    
    interface operator(.t.)
        procedure transp    
    end interface    
    
    interface operator(.xt.)
        !procedure dyadic   
        procedure productABT
        procedure outer_product
    end interface
    
    interface operator(.tx.)
        procedure productATB   
        procedure productATc
        procedure dotProductATB
    end interface
    
    interface operator(.i.)
        procedure inv
    end interface
    
    interface operator(.ix.)
        procedure ix
        procedure ixvector
    end interface
    
    interface operator(.xi.)
        procedure xi
    end interface    
    
    interface operator(.x.)
        procedure crossProduct 
        procedure productMatrix 
        procedure productMatrixVector
        procedure productVectorMatrix
    end interface
    
    interface operator(.abs.)
        procedure modulusVector 
        procedure modulusMatrix
    end interface    
    
    !interface operator(.abss.)
    !    procedure modulusSquare 
    !end interface       
    
    contains
    
    
    !! abT
    !function dyadic(a,b) result(c)
    !    implicit none
    !    real(8),intent(in) :: a(:), b(:)
    !    real(8) :: c(size(a),size(b))
    !    real(8) :: d(size(a),1)
    !    real(8) :: e(1,size(b))
    !    
    !    c = 0.0d0
    !    d(:,1) = a 
    !    e(1,:) = b 
    !    c = matmul(d,e)
    !    
    !end function
    
    !function modulusMatrix(A) result (C)
    !    implicit none
    !    real(8), intent(in) :: A(:,:)
    !    real(8) C
    !
    !    C = 0.0d0
    !    C = sqrt(sum(A(:,1)*A(:,1)))
    !end function  
   
    
    
    ! AB
    function productMatrix(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:,:)
        real(8) C(size(a,1),size(b,2))
    
        C = 0.0d0
        C = matmul(A,B)
    end function      
    
    
    ! Ab
    function productMatrixVector(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:)
        real(8) C(size(a,1))
        
        C = 0.0d0
        C = matmul(A,B)
    end function         
    
    
    ! aB
    function productVectorMatrix(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:)
        real(8), intent(in) :: B(:,:)
        real(8) C(size(B,2))
    
        C = 0.0d0
        C = matmul(A,B)
    end function      
    
    
    ! AT
    function transp(A) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8) C(size(A,2),size(A,1))
    
        C = 0.0d0
        C = transpose(A)
    end function      
    
    
    ! ABT
    function productABT(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:,:)
        real(8) C(size(a,1),size(b,1))
    
        C = 0.0d0
        C = matmul(A,transpose(B))
    end function       
    
    
    ! ix
    function ix(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:,:)
        real(8) C(size(a,1),size(b,1))
    
        C = 0.0d0
        C = matmul(inv(A),B)
    end function   
    
    ! ix, matrix with vector
    function ixvector(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:)
        real(8) C(size(a,1))
    
        C = 0.0d0
        C = matmul(inv(A),B)
    end function     
    
    
    ! xi
    function xi(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:,:)
        real(8) C(size(a,1),size(b,1))
    
        C = 0.0d0
        C = matmul(A,inv(B))
    end function    
   
    
    ! perform the outer product of two vectors a and b
    ! a ox b,  a xt b
    function outer_product(a,b)
        implicit none
        real*8, intent(in), dimension(:) :: a, b
        real*8, dimension(size(a), size(b)) :: outer_product
        
        outer_product = spread(a, dim=2, ncopies=size(b)) * &
                        spread(b, dim=1, ncopies=size(a))
    end function    
    
    
    ! ATB
    function productATB(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:,:)
        real(8) :: C(size(A,2),size(B,2))
        
        C = 0.0d0
        C = matmul(transpose(A),B)
    end function    
    
    
    ! ATc
    function productATc(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:,:)
        real(8), intent(in) :: B(:)
        real(8) :: C(size(A,2))
        
        C = 0.0d0
        C = matmul(transpose(A),B)
    end function     
    
    
    ! aTb
    function dotProductATB(A,B) result (C)
        implicit none
        real(8), intent(in) :: A(:)
        real(8), intent(in) :: B(:)
        real(8) :: C
        
        C = 0.0d0
        C = sum(A(:)*B(:))
        
    end function      
    
    
    ! skew-symmetric
    function skew(A) result (C)
        implicit none
        real(8), intent(in) :: A(:)
        real(8) C(size(A),size(A))
        
        C(1,1:3) = [0.0d0, -A(3),  A(2)]
        C(2,1:3) = [ A(3), 0.0d0, -A(1)]
        C(3,1:3) = [-A(2),  A(1), 0.0d0]
        
    end function      
     
end module
