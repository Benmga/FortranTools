!>
!! @author
!!   MA Gang
!!   magang@hrbeu.edu.cn    
!! @version A60514   
!! @revision 20190929
!!    
    
module math_root_m
    implicit none
    !private
    
    interface calCubic
        procedure rootCubicEquation
        procedure rootCubicEquationII
    end interface
    
    
    contains

    
function cubicEquation(x,a,b,c,d)
    implicit none
    real(8) :: x
    real(8) :: a,b,c,d
    real(8) :: cubicEquation
    cubicEquation = a*x**3 + b*x**2 +c*x + d
end function

    
    
subroutine rootCubicEquation(a,b,c,d,xa,xb,numRoots,Roots)
    !
    !   a x^3 + b x^2 + c x + d = 0
    !   Also can be used for a=0, b=0, c=0 
    !
    implicit none
    ! INPUT
    real(8) :: a,b,c,d
    real(8) :: xa, xb  ! limit range in which the roots should be

    ! OUPUT
    integer :: numRoots  ! return the number of roots
    real(8) :: Roots(3)     ! return the roots.

    ! TEMP
    integer :: maxCycles,i  ! Counter for bisection method. maxCycles=50 here.
    real(8) :: delta
    real(8) :: xm, xn  ! the variable limit, xa <= xm < xn <= xb
    real(8) :: ym, yn  ! the variable limit
    real(8) :: xk, yk  ! the center in [xm,xn], used in bisection method
    real(8), parameter :: incre = 0.01d0  ! used to find the right side limit of the first root.
    logical flag


    flag = 0
    numRoots = 0
    maxCycles = 50
    !
    !    special case for a = 0
    !
    if(a==0.0d0) then
        if(b==0.0d0) then
            if(c==0.0d0) then
                return
            else
                numRoots = 1
                Roots(1) = -d/c
                return
            endif
        else
            delta = c**2 - 4.0d0*b*d
            if(delta==0.0d0) then
                numRoots = 1
                Roots(1) = -c/(2.0d0*b)
            else if(delta<0.0d0) then
                return
            else
                delta = sqrt(delta)
                xm = (-c+delta)/(2.0d0*b)
                xn = (-c-delta)/(2.0d0*b)
                call reOrderCheck(numRoots, xm, xn, xa, xb, Roots)
                return
            endif
        endif
    endif
    !
    !    Ordinary case for a /= 0
    !
    xm = xa
    ym = cubicEquation(xm,a,b,c,d)

    !    root on the left variable limit
    if(abs(ym) <= 1.0d-10) then
        numRoots = 1
        Roots(1) = xm
    endif

    !   get the right variable limit of the first root 
    i = 0
    xn = xa + incre
    do while(xn<=xb)
        xn = xm + incre
        if(xn>xb) xn=xb
        yn = cubicEquation(xn,a,b,c,d)
        if(abs(yn) == 1.0d-8) then
            numRoots = 1
            Roots(1) = xn
            exit
        else if(ym*yn > 0.0d0) then
            xm = xn
            ym = yn
            cycle
        else if(ym*yn < 0.0d0) then
            flag=1
            exit ! find the solution range, get into bisection method
        endif
        i = i+1
        if(i>maxCycles) then
            write(*,*) "Problem happens for root."
            exit
        endif        
    end do

    !   find the first root by bisection method
    i = 0
    do while(flag)
        xk = (xm+xn)/2.0d0
        yk = cubicEquation(xk,a,b,c,d)
        if(abs(yk) <= 1.0d-10) then
            numRoots = 1
            Roots(1) = xk
            exit
        else if(yk*ym < 0.0d0) then
            xn = xk
            yn = yk
        else
            xm = xk
            ym = yk
        endif
        i = i+1
        !write(*,*) "Root",i
        if(i>maxCycles) then
            write(*,*) "Problem happens with equation solver, check the input data"
            exit
        endif
    end do

    if(numRoots==1) then
    !    find other two root with analytical method
        delta = (b/a+Roots(1))**2 + 4.0d0*d/(a*Roots(1))
        if (delta < 0.0d0) return
        if (delta == 0.0d0) then
           xk = -(b/a+Roots(1))/2.0d0
           if ((xk>=xa) .and. (xk<=xb)) then
              numRoots = 2
              Roots(2) = xk
           endif
           return
        endif
        xm = -(b/a+Roots(1)+sqrt(delta))/2.0d0
        xn = -(b/a+Roots(1)-sqrt(delta))/2.0d0
        call reOrderCheck(numRoots, xm, xn, xa, xb, Roots)
    endif
end subroutine


subroutine reOrderCheck(numRoots, rooti, rootk, xa, xb, Roots)
    implicit none
    integer :: numRoots
    real(8) :: Roots(3)
    real(8) :: rooti, rootk
    real(8) :: xa, xb
    real(8) :: temp

    if(rooti>rootk) then
        temp  = rooti
        rooti = rootk
        rootk = temp
    end if

    if(rooti>=xa .and. rooti<=xb) then
        numRoots = numRoots +1
        Roots(numRoots) = rooti
    end if
    if(rootk>=xa .and. rootk<=xb) then
        numRoots = numRoots +1
        Roots(numRoots) = rootk
    end if
end subroutine

    
subroutine rootCubicEquationII(coefs,xa,xb,numRoots,Roots)
    !
    !   a x^3 + b x^2 + c x + d = 0
    !   Also can be used for a=0, b=0, c=0 
    !
    implicit none
    ! INPUT
    real(8) :: coefs(4)
    real(8) :: xa, xb  ! limit range in which the roots should be

    ! OUPUT
    integer :: numRoots  ! return the number of roots
    real(8) :: Roots(3)     ! return the roots.
    
    real(8) :: a,b,c,d
    
    a = coefs(1)
    b = coefs(2)
    c = coefs(3)
    d = coefs(4)
    
    call rootCubicEquation(a,b,c,d,xa,xb,numRoots,Roots)
    
end subroutine  

end module     
    