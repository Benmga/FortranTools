!! @author
!!   G. Ma
!! @date
!!   20191226
    
! 这些文件的处理只能用于Windows环境    
module file_m
    use string_m    
    implicit none
    
    contains
    
    
function loadtxt(filepath) result(array)
    implicit none
    character(*) :: filepath
    real(8), allocatable :: array(:,:)
    integer :: rows, cols
    logical alive
    integer :: iin = 9012
    Integer :: IOStatus, i
    character(256) :: str


    inquire(file=trim(filepath), exist=alive)
    if(.not. alive) then
        write(*,*) "文件不存在，", trim(filepath)
        return
    endif    
    
    cols = getfilecol(filepath,2)  ! 获得列数
    
    ! 获得数据区域行数
    open(iin, file=filepath)
    read(iin,*)  !标题行    
    !rows = getfilerow(filepath)
    rows = 0
    iostatus = 0
    do while (iostatus == 0 )
        str = ""  ! 这句必须要有
	    read( iin,*,iostat=iostatus) str
        if(lenl(str)==0) exit  !空行终止
	    rows = rows + 1
        !write(*,*) rows,str(1:10)
    end do
    
    rewind(iin) ! 回到第一行

    ! 存放数据的数组开辟内存
    allocate(array(rows, cols))
    array = 0.0d0
    
    ! 读数据
    read(iin,*)  !标题行  
    do i = 1, rows
        read(iin,*,iostat=IOStatus) array(i,:)
        !write(*,*) i, array(i,1:3)
        if(IOStatus /= 0) exit  
    end do    
        
    close(iin)
    

end function    
    
!function loadtxt(filepath) result(array)
!    implicit none
!    character(*) :: filepath
!    real(8), allocatable :: array(:,:)
!    integer :: rows, cols
!    logical alive
!    integer :: iin = 9012
!    Integer :: IOStatus, i
!
!
!    inquire(file=trim(filepath), exist=alive)
!    if(.not. alive) then
!        write(*,*) "文件不存在，", trim(filepath)
!        return
!    endif    
!    
!    rows = getfilerow(filepath)
!    cols = getfilecol(filepath,2)
!    allocate(array(rows, cols))
!    array = 0.0d0
!    
!    open(iin, file=filepath)
!    read(iin,*)  !标题行
!    
!    i = 1
!    do while(1)
!        read(iin,*,iostat=IOStatus) array(i,:)
!        i = i+1
!        if(IOStatus /= 0) exit  
!    end do    
!        
!    close(iin)
!    
!
!end function

    
!>
!! Any length is supported.
!! The returned file name is like 'abc.doc'.
!! The filename can be got from file path.
!! The filepath is like
!! \verbatim
!! c:\abc.doc
!! \endverbatim
function getfilename(filepath) result (filename)
    implicit none
    integer*4 ind
    integer*4 pathlength
    character(*) :: filepath
    character(:), allocatable :: filename

    ! get filename by indexing the last symbol \.
    !   if the symbol \ is in the last positon, then
    !   no filename is set.
    filename = ''
    pathlength = len(trim(filepath))
    ! in order to use doxygen, i have to use trim("\ ")
    ind = index(filepath,trim("\ "),.true.)  ! ref the index document
    if(ind>=pathlength) return
    filename = filepath(ind+1:pathlength)
end function

!>
!! here 'filepath' cab be 'filename'. Any length is supported.
!! for file 'abc.doc', return 'abc'.
function getfilenamebody(filepath) result(filenamebody)
    implicit none
    integer*4 ind, indend
    integer*4 pathlength
    character(*) :: filepath
    character(:), allocatable :: filenamebody

    ! get filename by indexing the last symbol \.
    !   if the symbol \ is in the last positon, then
    !   no filename is set.
    filenamebody = ''
    pathlength = len(trim(filepath))
    ind = index(filepath,trim("\ "),.true.)  ! ref the index document
    if(ind>=pathlength) return
    indend = index(filepath,'.',.true.)
    if(indend>=pathlength) return
    filenamebody = filepath(ind+1:indend-1)
end function

!>
!! here 'filepath' cab be 'filename'. Any length is supported.
!! return suffix like '.doc'.
function getfilenamesuffix(filepath) result(filenamesuffix)
    implicit none
    integer*4 ind
    integer*4 pathlength
    character(*) :: filepath
    character(:), allocatable ::  filenamesuffix

    ! get filename by indexing the last symbol \.
    !   if the symbol \ is in the last positon, then
    !   no filename is set.
    filenamesuffix = ''
    pathlength = len(trim(filepath))
    ind = index(filepath,'.',.true.)  ! ref the index document
    if(ind>=pathlength) return
    filenamesuffix = filepath(ind+1:pathlength)
end function

!>
!! Any length is supported.
!! file name is like 'abc.doc'.
!! folder path is like 'c:'.
function getfolderpath(filepath) result(folderpath)
    implicit none
    integer*4 ind
    integer*4 pathlength
    character(*) :: filepath
    character(:), allocatable :: folderpath

    ! get filename by indexing the last symbol \.
    !   if the symbol \ is in the last positon, then
    !   no filename is set.
    folderpath = ''
    pathlength = len(trim(filepath))
    ind = index(filepath,trim("\ "),.true.)  ! ref the index document
    if(ind>=pathlength) return
    folderpath = filepath(1:ind-1)
end function


!>
!! get the row number of a text file. \n
!! unit=9001 is used inside for file operation.
function getfilerow(filepath) result(row)
    implicit none
    !>
    !! the absolute file path which can be any length.
    character(len=*) :: filepath
    !>
    !! @return
    !! the row number of the given file.
    integer :: row
    logical :: alive
    integer*4 :: iostatus  ! never set value in the defination.

    row = -1

    inquire(file=trim(filepath), exist=alive)
    if(.not. alive) then
        return
    end if

    open(unit=9001,file=filepath,status='old')

    iostatus = 0
    do while (iostatus == 0 )
	    row = row + 1
	    read( 9001,*,iostat=iostatus)
    end do
    close(9001)

end function


!>
!! get the row number of a text file. \n
!! unit=9001 is used inside for file operation.
function getfilerow2(filepath,comments,skiprows) result(row)
    implicit none
    !>
    !! the absolute file path which can be any length.
    character(len=*) :: filepath
    integer :: skiprows       ! 跳过前多少行
    character(*) :: comments  ! 注释行的符号开头的，会被忽略
    !>
    !! @return
    !! the row number of the given file.
    integer :: row
    logical :: alive
    integer*4 :: iostatus  ! never set value in the defination.
    character(256) :: char
    integer :: i

    row = -1

    inquire(file=trim(filepath), exist=alive)
    if(.not. alive) then
        return
    end if

    open(unit=9001,file=filepath,status='old')

    iostatus = 0
    do i=1, skiprows
        read( 9001,*,iostat=iostatus)
    end do
    
    do while (iostatus == 0 )

	    read( 9001,*,iostat=iostatus) char
        if(len(trim(char))==0) cycle
        if(isstartswith(triml(char),triml(comments))) cycle
	    row = row + 1        
    end do
    close(9001)

end function


!>
!! get the col number of a text file in the given row. \n
!! unit=9001 is used inside for file operation.
!! @note The max length for the given row is supposed to be less than 1000.
!!
function getfilecol(filepath, row) result(col)
    implicit none
    integer :: iostatus
    !>
    !! the absolute file path which can be any length.
    character(*) :: filepath
    character*1000 :: oneline
    !>
    !! get col in the given row.
    integer :: row
    integer :: col
    integer :: i
    real*8 :: valu(1000)
    logical :: isok

    open(unit=9001,file=filepath,status='old')
    do i = 1, row-1
	    read( 9001,*,iostat=iostatus)
    end do

    read(9001,"(a1000)") oneline
    oneline = trim(oneline)
    col = getstringsplitednumber(oneline," ")

    close(9001)
end function

!>
!! if the folder is not exist, the create it.
!! if exist then exit.
!! Both full path and relative path are supported.
!! only one folder is created at once.
!! the parent path should be exist.
subroutine createonefolder(folderpath)
    use ifport
    implicit none
    character(*) :: folderpath
    logical alive, res

    inquire(directory=trim(folderpath), exist=alive)
    if(.not. alive) then
        ! ref to makedirqq
        ! https://software.intel.com/en-us/node/511921
        res = makedirqq(trim(folderpath))
        if(.not. res) then
            write(*,*) trim(folderpath),' is not created correctly.'
        end if
    end if
end subroutine

!>
!! To create folders with "md" commmand. \n
!! All folders in the given path will be checked and created one by one.
!! @note
!! The "execute_command_line" command is used here.
!! This routine is compiled with the support from
!! Intel Fortran Composer XE 2015 (compiler 15.0) August 5, 2014.
!! The "system" command can also do the work.
!!
subroutine createfolders(folderpath)
    !use ifport
    implicit none
    character(*) :: folderpath
    logical alive
    integer res

    inquire(directory=trim(folderpath), exist=alive)
    if(.not. alive) then
        !res = system("md "//trim(folderpath))
        call execute_command_line("md "//trim(folderpath))
    end if
end subroutine

!>
!! the folder must be empty, then it can be deleted.
subroutine deleteemptyfolder(folderpath)
    use ifport
    implicit none
    character(*) folderpath
    logical alive, res

    inquire(directory=trim(folderpath), exist=alive)
    if(alive) then
        res = deldirqq(trim(folderpath))
        if(.not. res) then
            write(*,*) trim(folderpath),' is not deleted correctly.'
        end if
    end if
end subroutine

!>
!! the folder can be deleted including the sub-folders and files.
!! No matter what if folder is empty.
!!
subroutine deletefolder(folderpath)
    use ifport
    implicit none
    character(*) folderpath
    character(:), allocatable :: command
    logical alive, res
    integer*4 sta

    inquire(directory=trim(folderpath), exist=alive)
    if(alive) then
        ! /q quiet,
        ! /s sub-folders, to make sure the delete no matter what if folder is empty.
        command ='rd /q /s '//trim(folderpath)
        sta = system(trim(command))
        if(sta == -1) then
            write(*,*) trim(folderpath),' is not deleted correctly.'
        end if
    end if
end subroutine


!>
!! Forced to delete the specified file.
!!
subroutine deletefile(filepath)
    use ifport
    implicit none
    character(*) filepath
    character(:), allocatable :: command
    logical alive, res
    integer*4 sta

    inquire(file=trim(filepath), exist=alive)
    if(alive) then
        ! /q quiet, no need to confirm
        ! /a file property like read-only, hidden and system
        ! /f forced to delete file
        command ='del /a /f /q '//trim(filepath)
        sta = system(trim(command))
        if(sta == -1) then
            write(*,*) trim(filepath),' is not deleted correctly.'
        end if
    end if
end subroutine

!>
!! return the filepath containing the desired files' path.
!!
!! The input should be like
!! \verbatim
!! C:\todoA\*.f90
!! \endverbatim
!!
!! The auto generated whole command is like
!! \verbatim
!! dir /b /s C:\todoA\*.f90>c:\dir125.txt
!! \endverbatim
!! /b means only to list file path without other info such as time. \n
!! /s means to include the sub-folders.
!!
function getpathlistedfiles(strin) result(resultfilepath)
    implicit none
    !>
    !! The input string should be like
    !! \verbatim
    !! C:\todoA\*.f90
    !! \endverbatim
    character(*) :: strin
    !>
    !! The returned path is fixed to
    !! \verbatim
    !! c:\dir125.txt
    !! \endverbatim
    character(:), allocatable :: resultfilepath
    character(:), allocatable :: strcmd

    resultfilepath = "c:\dir125.txt"
    strcmd = "dir /b /s " // trim(strin) // " > " // trim(resultfilepath)
    !write(*,*) strcmd
    call execute_command_line(trim(strcmd))
end function


!>
!! return array contains the desired files' path.
!!
function getlistedfiles(strin) result(strfilepaths)
    
    implicit none
    !>
    !! The input string should be like
    !! \verbatim
    !! C:\todoA\*.f90
    !! \endverbatim
    character(*) :: strin
    character(:), allocatable :: strfilepaths(:)
    character(:), allocatable :: sourcefilepath
    logical :: alive
    integer :: idunit
    integer :: numrow
    integer :: i

    idunit = 9002

    sourcefilepath = ""
    sourcefilepath = getpathlistedfiles(strin)
    inquire(file=trim(sourcefilepath), exist=alive)
    if(.not. alive) return

    numrow = getfilerow(sourcefilepath)
    if(numrow<1) then
        call deletefile(sourcefilepath)
        return ! do nothing
    end if
    allocate(character(512) :: strfilepaths(numrow) )

    open(idunit, file=sourcefilepath, status="old")
    rewind(idunit)
    do i = 1, numrow
        read(idunit,*) strfilepaths(i)
    end do
    close(idunit)

    strfilepaths = trimlarray(strfilepaths)

    call deletefile(sourcefilepath)
end function


function getNumFiles(strin) result(numrow)
    
    implicit none
    !>
    !! The input string should be like
    !! \verbatim
    !! C:\todoA\*.f90
    !! \endverbatim
    character(*) :: strin
    character(:), allocatable :: strfilepaths(:)
    character(:), allocatable :: sourcefilepath
    logical :: alive
    integer :: idunit
    integer :: numrow
    integer :: i

    idunit = 9002

    sourcefilepath = ""
    sourcefilepath = getpathlistedfiles(strin)
    inquire(file=trim(sourcefilepath), exist=alive)
    if(.not. alive) return

    numrow = getfilerow(sourcefilepath)

    call deletefile(sourcefilepath)
end function

end module
