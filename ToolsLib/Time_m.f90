module time_m
    
    contains
    
    ! 显示当前的时间
    subroutine showTime()
        implicit none
        CHARACTER*10 DATE1, TIME1, PRETTY_TIME1
        CALL DATE_AND_TIME( DATE1, TIME1 )
        PRETTY_TIME1 = TIME1(1:2) // ':' // TIME1(3:4) // ':' // TIME1(5:10)
        PRINT*, 'TIME:  ', DATE1, PRETTY_TIME1
    end subroutine 
    
end module    