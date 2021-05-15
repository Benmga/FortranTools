
!! @date
!!   2021-01-31 edited.        
    
    
module EulerRotation_m
    use math_m
    implicit none
    
    contains    
    
      
    
    !> 
    !! R123
    !! ������ϵOXYZ�Ͷ�ϵOxyz��ʼ�غϡ�
    !! ���ȣ������ƶ�ϵox��ת��phi�Ƕȣ�
    !! Ȼ�󣬸����ƶ�ϵoy'��ת��theta�Ƕȣ�
    !! ��󣬸����ƶ�ϵoz''��ת��psi�Ƕȡ�
    !! [X] = Rx(phi)Ry(theta)Rz(psi)[x]
    !!
    !!
    !! It can be used to tranfer the vector between two coordiantes.
    !!   [X] = [R][x],
    !!   [X] capital letter means the vector in global coordinate,
    !!   [x] lower-case means the vector in local coordinate.
     function Rot(a) result(R)
        ! get global from local
        implicit none
        real*8 a(3)  !< radian angles
        real*8 R(3,3)
        
        R(1,1) =  cos(a(2))*cos(a(3))
        R(1,2) = -cos(a(2))*sin(a(3))
        R(1,3) =  sin(a(2))
    
        R(2,1) =  cos(a(1))*sin(a(3)) + sin(a(1))*sin(a(2))*cos(a(3))
        R(2,2) =  cos(a(1))*cos(a(3)) - sin(a(1))*sin(a(2))*sin(a(3))
        R(2,3) = -sin(a(1))*cos(a(2))
    
        R(3,1) =  sin(a(1))*sin(a(3)) - cos(a(1))*sin(a(2))*cos(a(3))
        R(3,2) =  sin(a(1))*cos(a(3)) + cos(a(1))*sin(a(2))*sin(a(3))
        R(3,3) =  cos(a(1))*cos(a(2))        
    end function
    

    
    !>
    !! \f${\bf{\tilde R}} = {{\bf{R}}^{ -1}} = {{\bf{R}}^{\rm{T}}}\f$
    !! 
    !! [x] = [R][X],
    function rRot(angles) result(R)
        ! get local from global
        ! the transposed rotation
        implicit none
        real*8 angles(3)  !< radian angles
        real*8 R(3,3)
        R = Rot(angles)
        R = transpose(R)
    end function    
    
    
    
    !>    
    !!  [w] = [R][dPhi/dt]
    function TRot(angles) result(R)
        ! get local from global, for first order derivative
        ! the transposed rotation
        implicit none
        real*8 angles(3)  !< radian angles
        real*8 R(3,3)
        real*8 :: a,b,c
        
        a = angles(1)
        b = angles(2)
        c = angles(3)
        
        R(1,1:3) = [ cos(c)*cos(b), sin(c), 0.0d0]
        R(2,1:3) = [-sin(c)*cos(b), cos(c), 0.0d0]
        R(3,1:3) = [ sin(b),        0.0d0,  1.0d0]
    end function    
    
    !>
    !! get global from local, for first order derivative. 
    !!
    !! [dPhi/dt] = [R][w],
    !!    w   is the angular velocity,
    !!    Phi means the Euler angle here. 
    !!
    !! This routine return the matrix for the transformation
    !!   betweenthe first order derivative with respect to the 
    !!   time of Euler angle and angular velocity.
    !!
    !! The Euler rotation order is in 1-2-3.
   
    function rTRot(angles) result(R)
        implicit none
        real*8 angles(3)  !< radian angles
        real*8 R(3,3)
        real*8 :: a,b,c
        
        a = angles(1)
        b = angles(2)
        c = angles(3)
        
        R(1,1:3) = [ cos(c)/cos(b), -sin(c)/cos(b), 0.0d0]
        R(2,1:3) = [ sin(c),         cos(c),        0.0d0]
        R(3,1:3) = [-cos(c)*tan(b),  sin(c)*tan(b), 1.0d0]        
       
    end function    

    !
    !!! 123�Ƕȵ�
    !!! [X] = [R][x]
    !function Rotd(a) result(R)
    !    ! get global from local
    !    implicit none
    !    real*8 a(3)  ! deg
    !    real*8 R(3,3)
    !    
    !    R(1,1) =  cosd(a(2))*cosd(a(3))
    !    R(1,2) = -cosd(a(2))*sind(a(3))
    !    R(1,3) =  sind(a(2))
    !
    !    R(2,1) =  cosd(a(1))*sind(a(3)) + sind(a(1))*sind(a(2))*cosd(a(3))
    !    R(2,2) =  cosd(a(1))*cosd(a(3)) - sind(a(1))*sind(a(2))*sind(a(3))
    !    R(2,3) = -sind(a(1))*cosd(a(2))
    !
    !    R(3,1) =  sind(a(1))*sind(a(3)) - cosd(a(1))*sind(a(2))*cosd(a(3))
    !    R(3,2) =  sind(a(1))*cosd(a(3)) + cosd(a(1))*sind(a(2))*sind(a(3))
    !    R(3,3) =  cosd(a(1))*cosd(a(2))   
    !
    !end function    
    !
    !!!  [x] = [R][X]
    !function rRotd(anglesDeg) result(R)
    !    ! get local from global
    !    ! the transposed rotation
    !    implicit none
    !    real*8 anglesDeg(3)
    !    real*8 R(3,3)
    !    
    !    R = Rotd(anglesDeg)
    !    R = transpose(R)
    !end function 
    !
    !!!  [w] = [R][dPhi/dt]
    !function TRotd(angles) result(R)
    !    ! get local from global, for first order derivative
    !    ! the transposed rotation
    !    implicit none
    !    real*8 angles(3)  !< deg angles
    !    real*8 R(3,3)
    !    real*8 :: a,b,c
    !    
    !    a = angles(1)
    !    b = angles(2)
    !    c = angles(3)
    !    
    !    R(1,1:3) = [ cosd(c)*cosd(b), sind(c), 0.0d0]
    !    R(2,1:3) = [-sind(c)*cosd(b), cosd(c), 0.0d0]
    !    R(3,1:3) = [ sind(b),           0.0d0, 1.0d0]
    !end function        
    !
    !! [dPhi/dt] = [R][w]
    !function rTRotd(angles) result(R)
    !    implicit none
    !    real*8 angles(3)  !< deg angles
    !    real*8 R(3,3)
    !    real*8 :: a,b,c
    !    
    !    a = angles(1)
    !    b = angles(2)
    !    c = angles(3)
    !    
    !    R(1,1:3) = [ cosd(c)/cosd(b), -sind(c)/cosd(b), 0.0d0]
    !    R(2,1:3) = [ sind(c),          cosd(c),         0.0d0]
    !    R(3,1:3) = [-cosd(c)*tand(b),  sind(c)*tand(b), 1.0d0]        
    !   
    !end function        
    !



    !>
    !! \f${\bf{R}} = \left[ {\matrix{
    !!    1 & { - \psi } & \theta   \cr 
    !!    \psi  & 1 & { - \phi }  \cr 
    !!    { - \theta } & \phi  & 1  \cr 
    !!  } } \right]\f$ 
    function getRotationSmall(angles) result(R)
        ! used to get global from local
        implicit none
        
        ! INPUT
        real*8 angles(3)  !< radian angles              
        ! RETURN
        real*8 R(3,3)
        ! TEMP
        real*8 a,b,c  ! phi, theta, psi, the three Euler angle in 1-2-3 order.
                      ! phi is not used in this routine.
                              
        a = angles(1)
        b = angles(2)
        c = angles(3)
        R = 0.0d0
    
        R(1,1) =  1.0d0
        R(1,2) = -c
        R(1,3) =  b
    
        R(2,1) =  c
        R(2,2) =  1.0d0
        R(2,3) = -a
    
        R(3,1) = -b
        R(3,2) =  a
        R(3,3) =  1.0d0
    end function
    
    
    
    !!  312��ת��
    
    function Rot312(a) result(R)
        ! get global from local
        implicit none
        real*8 a(3)  !< radian angles
        real*8 R(3,3)
        
        R(1,1) = -sin(a(1))*sin(a(2))*sin(a(3)) + cos(a(2))*cos(a(3))
        R(1,2) = -cos(a(1))*sin(a(3))
        R(1,3) =  sin(a(1))*cos(a(2))*sin(a(3)) + sin(a(2))*cos(a(3)) 
    
        R(2,1) =  sin(a(1))*sin(a(2))*cos(a(3)) + cos(a(2))*sin(a(3))
        R(2,2) =  cos(a(1))*cos(a(3))
        R(2,3) = -sin(a(1))*cos(a(2))*cos(a(3)) + sin(a(2))*sin(a(3))
    
        R(3,1) = -cos(a(1))*sin(a(2))
        R(3,2) =  sin(a(1))
        R(3,3) =  cos(a(1))*cos(a(2))
    end function  


    ! ŷ������������ٶ�֮��Ĺ�ϵ
    ! [w] = [T][da/dt]
    function TRot312(a) result(R)
        ! get global from local
        implicit none
        real*8 a(3)  !< radian angles
        real*8 R(3,3)
        
        R(1,1) = 1.0d0
        R(1,2) = 0.0d0
        R(1,3) = -sin(a(2))
    
        R(2,1) = 0.0d0
        R(2,2) = cos(a(1))
        R(2,3) = sin(a(1))*cos(a(2))
    
        R(3,1) = 0.0d0
        R(3,2) = -sin(a(1))
        R(3,3) = cos(a(1))*cos(a(2))
    end function  

    ! ŷ������������ٶ�֮��Ĺ�ϵ
    ! [da/dt] = [T][w]
    function rTRot312(a) result(R)
        ! get global from local
        implicit none
        real*8 a(3)  !< radian angles
        real*8 R(3,3)
        
        R(1,1) = 1.0d0
        R(1,2) = sin(a(1))*tan(a(2))
        R(1,3) = cos(a(1))*tan(a(2))
    
        R(2,1) = 0.0d0
        R(2,2) = cos(a(1))
        R(2,3) = -sin(a(1))
    
        R(3,1) = 0.0d0
        R(3,2) = sin(a(1))/cos(a(2))
        R(3,3) = cos(a(1))/sin(a(2))
    end function      

end module