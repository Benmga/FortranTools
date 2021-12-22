
!>
!! @warning
!!   Dynamic allocation of Fortran strings is used.
!!   It is one feature of Fortran 2003. And routines are tested under ivf.
!!   - character(:), allocatable
!!   - character(len=*)
!! @author
!!   G. Ma
!! @date
!!   2015-04-08 created.
!!   2019-07-17 edited.    
!!
module string_m
    implicit none

    !>
    !! enhanced for // operation to get conglutinated pure contents of strings.
    !!
    interface operator(+)
        procedure addstr
    end interface
    
    interface str
        procedure num2str
    end interface

    contains

    ! return an allocatable string with target length
    function string(num) result(cc)
        implicit none
        integer :: num
        character(:), allocatable :: cc

        allocate(character(num) :: cc)
    end function    

    !>
    !! conglutinate pure contents of two strings.
    !! -# adjust left for both of the strings.
    !! -# trim the spaces after the contents.
    !! -# conglutinate them together.
    !!
    function addstr(aa,bb) result(cc)
        implicit none
        character(len=*), intent(in) :: aa  ! intent must be written here.
        character(len=*), intent(in) :: bb
        character(:), allocatable :: cc

        cc = trim(adjustl(aa))//trim(adjustl(bb))
    end function


    !>
    !! return the length of the given string after adjust left.
    !! @see lenl
    !!
    function len_triml(aa) result(num)
        implicit none
        character(len=*) :: aa
        integer :: num

        num = len_trim(adjustl(aa))
    end function


    !>
    !! return the length of the given string after adjust left.
    !! An alternative interface for len_triml.
    !! @see len_triml
    !!
    function lenl(aa) result(num)
        implicit none
        character(len=*) :: aa
        integer :: num

        num = len_triml(aa)
    end function


    !>
    !! return the pure content of the given string after adjust left and trim.
    !!
    function triml(aa) result(cc)
        implicit none
        character(len=*), intent(in) :: aa  ! intent must be written here.
        character(:), allocatable :: cc

        cc = trim(adjustl(aa))
    end function

    ! replace all term in the string
    function replace(string,text,rep)  result(outs)
        character(*) :: string,text,rep
        character(:), allocatable :: outs    
        integer :: i,j, nt, nr

        outs = string 
        nt = len(text) 
        nr = len(rep)
        j = 1
        do
           i = index(outs(j:),text(:nt)) 
           if(i == 0) exit
           i = i + j -1 
           outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
           j = i+nr
        end do
    end function
    

    !>
    !! 通过识别单个分隔符将字符串InStr进行分割，返回被分割后的数目.
    !! @date
    !! G. Ma 2015-04-10 modified \n
    !! 吴徐平(wxp07@qq.com) 2011-04-29 created
    !!
    function getstringsplitednumber(instr,delimiter) result(nsize)
        implicit none
        !>
        !! 输入的字符串。字符串长度任意。
        character(len = *), intent( in ) :: instr
        !>
        !! 分隔符号。\n
        !! 可以将多个分割符号放在这一个变量里，程序会自动甄别，按单符号进行分割。\n
        !! 只要符合其中任意一个符号，即进行分割。\n
        !! 重复的分隔符将被忽略。\n
        !! 例如';,,' 使用;和,分割字符串
        character(len = *), intent( in ) :: delimiter
        integer :: nsize  !< 字符串最终被分割出来的数目
        integer :: i, j
        integer :: istart  ! split index for start position

        nsize=0
        istart=1
        do i=1,len(instr)
	        do j=1,len(delimiter)
		        if (instr(i:i) == delimiter(j:j)) then
			        if (istart == i) then
			        istart=i+1 ! 可防止分隔符相连的情况
			        end if
			        if (istart<i) then
				        nsize=nsize+1
				        istart=i+1
			        end if
		        end if
	        end do
        end do

        ! 匹配最后一个子字符串
        if (nsize>0) then
	        if (istart<len(instr)) then
		        nsize=nsize+1
	        end if
        end if

        ! 如果无可分割的子字符串,则包含整个字符串为数组的第一元素
        if ( (nsize<1) .and. (len(trim(instr)) > 0 )) then
		        nsize=1
        end if

    end function


    !>
    !! 通过识别单个分隔符将字符串InStr进行分割，返回被分割后的字符串数组.
    !! @date
    !! G. Ma 2015-04-10 modified \n
    !! 吴徐平(wxp07@qq.com) 2011-04-29 created
    !!
    function getstringsplited(instr,delimiter) result(stringarray)
        implicit none
        !>
        !! 输入的字符串。字符串长度任意。
        character(len = *), intent( in ) :: instr
        !>
        !! 分隔符号。\n
        !! 可以将多个分割符号放在这一个变量里，程序会自动甄别，按单符号进行分割。\n
        !! 只要符合其中任意一个符号，即进行分割。\n
        !! 重复的分隔符将被忽略。\n
        !! 例如';,,' 使用;和,分割字符串
        character(len = *), intent( in ) :: delimiter
        !
        ! temp array for the split operation
        character(len = len(instr)),dimension(len(instr)) :: strarray
        !>
        !! @return
        !! return the compressed string array.
        character(len=:), allocatable :: stringarray(:)

        integer :: nsize  !< 字符串最终被分割出来的数目
        integer :: i, j
        integer :: istart  ! split index for start position

        nsize=0
        istart=1
        do i=1,len(instr)
	        do j=1,len(delimiter)
		        if (instr(i:i) == delimiter(j:j)) then
			        if (istart == i) then
			        istart=i+1 ! 可防止分隔符相连的情况
			        end if
			        if (istart<i) then
				        nsize=nsize+1
                        strarray(nsize)=instr(istart:i-1)
				        istart=i+1
			        end if
		        end if
	        end do
        end do

        ! 匹配最后一个子字符串
        if (nsize>0) then
	        if (istart<len(instr)) then
		        nsize=nsize+1
                strarray(nsize)=instr(istart:len(instr))
	        end if
        end if

        ! 如果无可分割的子字符串,则包含整个字符串为数组的第一元素
        if ( (nsize<1) .and. (len(trim(instr)) > 0 )) then
		    nsize=1
            strarray(1)=instr
        end if

        stringarray = trimlarray(strarray)
    end function


    !>
    !! 去除字符串数组中首尾的空格. 按照数组元素最大长度重新设定数组.
    !!
    function trimlarray(stringarrayinput) result(stringarray)
        implicit none
        !>
        !! the input string array. \n
        !! It can be any character length for the string array.
        character(len=*) :: stringarrayinput(:)
        !>
        !! @return
        !! then compressed string array.
        character(len=:), allocatable :: stringarray(:)
        ! temp
        integer :: i
        integer :: numsize   ! size of array
        integer :: maxlength ! length of element in array that has the max length.
        integer :: strlength

        maxlength = 0
        numsize = size(stringarrayinput)

        do i = 1, numsize
            strlength = lenl(stringarrayinput(i))
            if(strlength > maxlength) then
                maxlength = strlength
            end if
        end do

        allocate(character(len=maxlength) :: stringarray(numsize))

        do i = 1, numsize
            stringarray(i) = triml(stringarrayinput(i))
        end do

    end function


    !>
    !! tranfer integer num to str
    function num2str(num) result(str)
        implicit none
        integer :: num
        character(12) :: string
        !>
        !! @return
        !! the str length will be compressed as mush as possible.
        character(:), allocatable :: str

        write(string, *) num
        str = triml(string)
    end function


    !>
    !! tranfer str to integer num
    function str2num(str) result(num)
        implicit none
        integer :: num
        character(*) :: str  ! here str can not use triml that surprise me.

        read(str, *) num
    end function


    !>
    !! To check if stringA endswith stringB. \n
    !! Both of the strings are trimed to ignore the space in the front and rear. \n
    !!
    function isendswith(stringa, stringb, iscasesensitive) result(issame)
        implicit none
        logical :: issame
        character(*) :: stringa
        character(*) :: stringb
        logical, optional :: iscasesensitive !< The default is not case sensitive.
        ! temp
        character(:), allocatable :: stra
        character(:), allocatable :: strb
        integer :: posbegin
        integer :: posend
        integer :: lenstra
        integer :: lenstrb
        logical :: issensitive

        issame = .false.
        issensitive = .false.
        if(present(iscasesensitive)) issensitive = iscasesensitive

        ! check the pure content in this routine.

        stra = triml(stringa)
        strb = triml(stringb)

        ! check length first.

        lenstra = lenl(stra)
        lenstrb = lenl(strb)

        if(lenstrb > lenstra) then
            issame = .false.
            return
        end if

        ! then check the strings in lower case.
        if(.not. issensitive) then
            stra = tolowercase(stra)
            strb = tolowercase(strb)
        end if

        posbegin = lenstra - lenstrb + 1
        posend   = lenstra

        if(stra(posbegin:posend) == strb(:)) then
            issame = .true.
        end if

    end function


    !>
    !! To check if stringA startswith stringB. \n
    !! Both of the strings are trimed to ignore the space in the front and rear. \n
    !!
    function isstartswith(stringa, stringb, iscasesensitive) result(issame)
        implicit none
        logical :: issame
        character(*) :: stringa
        character(*) :: stringb
        logical, optional :: iscasesensitive !< The default is not case sensitive.
        ! temp
        character(:), allocatable :: stra
        character(:), allocatable :: strb
        integer :: posbegin
        integer :: posend
        integer :: lenstra
        integer :: lenstrb
        logical :: issensitive  ! flag for case sensitive

        issame = .false.
        issensitive = .false.
        if(present(iscasesensitive)) issensitive = iscasesensitive

        ! check the pure content in this routine.

        stra = triml(stringa)
        strb = triml(stringb)

        ! check length first.

        lenstra = lenl(stra)
        lenstrb = lenl(strb)

        if(lenstrb > lenstra) then
            issame = .false.
            return
        end if

        ! then check the strings in lower case.
        if(.not. issensitive) then
            stra = tolowercase(stra)
            strb = tolowercase(strb)
        end if

        posbegin = 1
        posend   = lenstrb

        if(stra(posbegin:posend) == strb(:)) then
            issame = .true.
        end if

    end function


    !>
    !! return if stringA contains stringB
    !!
    function iscontains(stringa, stringb, iscasesensitive) result(issame)
        implicit none
        logical :: issame
        character(*) :: stringa
        character(*) :: stringb
        logical, optional :: iscasesensitive !< The default is not case sensitive.
        ! temp
        integer :: pos
        character(:), allocatable :: stra
        character(:), allocatable :: strb
        integer :: lenstra
        integer :: lenstrb
        logical :: issensitive  ! flag for case sensitive

        issame = .false.
        issensitive = .false.
        if(present(iscasesensitive)) issensitive = iscasesensitive

        stra = stringa
        strb = stringb

        ! check length first.

        lenstra = lenl(stra)
        lenstrb = lenl(strb)

        if(lenstrb > lenstra) then
            issame = .false.
            return
        end if

        ! then check the strings in lower case.
        if(.not. issensitive) then
            stra = tolowercase(stra)
            strb = tolowercase(strb)
        end if

        ! if not present, pos = 0
        pos = index(stra, strb)

        if(pos > 0) then
            issame = .true.
        end if
    end function


    !>
    !! toUpperCase \n
    !! The ASCII chart is used to determine if the character's case. \n
    !! iachar("a")=97, iachar("z")=122, A and Z in lower case. \n
    !! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    !! @author: Clive Page
    !! @author: G. Ma modified 2015-04-14
    !! @image html ascii_chart_windows.gif
    function touppercase(strin) result(strout)
         implicit none
         character(len=*), intent(in) :: strin
         character(len=len(strin)) :: strout
         integer :: i,j

         do i = 1, len(strin)
              j = iachar(strin(i:i))
              if (j>=97 .and. j<=122 ) then
                   strout(i:i) = achar(iachar(strin(i:i))-32)
              else
                   strout(i:i) = strin(i:i)
              end if
         end do
    end function


    !>
    !! toLowerCase \n
    !! The ASCII chart is used to determine if the character's case. \n
    !! iachar("A")=65, iachar("Z")=90, A and Z in upper case.
    !! @author: G. Ma modified 2015-04-14
    !! @image html ascii_chart_windows.gif
    function tolowercase(strin) result(strout)
         implicit none
         character(len=*), intent(in) :: strin
         character(len=len(strin)) :: strout
         integer :: i,j

         do i = 1, len(strin)
              j = iachar(strin(i:i))
              if (j>= 65 .and. j<=90 ) then
                   strout(i:i) = achar(iachar(strin(i:i))+32)
              else
                   strout(i:i) = strin(i:i)
              end if
         end do
    end function

end module

