! Copyright 2015 Fyodorov S. A.

module Source_IO
   use Environment

   implicit none
   public :: read_input_file, read_line_from_console, print_line

   type :: Node
      character(len=1) :: value
      type(Node), pointer :: next => null()
   end type Node

   type :: StringList
      type(Node), pointer :: head => null()
   contains
      procedure, public :: insert_character => insert_character_impl
      procedure, public :: delete_character => delete_character_impl
      procedure, public :: to_string => to_string_impl
   end type StringList


contains

   ! Процедура для чтения строки из стандартного ввода и создания списка символов
   recursive subroutine read_line_from_console(head, char_count)
       type(ListElement), pointer :: head
       integer :: char_count

       character(len=100) :: line
       integer :: i

       ! Считываем строку из стандартного ввода
       read(*, '(A)') line
       do i = 1, len(trim(line))
           ! Вставляем символы в список
           call insert(head, char_count + 1, line(i))
           char_count = char_count + 1
       end do
   end subroutine read_line_from_console

   ! Процедура для вывода списка символов как строки
   recursive subroutine print_line(node)
       type(ListElement), pointer :: node
       type(CharNode), pointer :: char_node

       if (associated(node)) then
           select type(node)
               type is (CharNode)
                   char_node => node
                   ! Выводим значение текущего узла
                   write(*, '(A)', advance='no') char_node%value
           end select
           ! Рекурсивно вызываем для следующего узла
           call print_line(node%next)
       end if
   end subroutine print_line

   ! Процедура для чтения команд из входного файла
   subroutine read_input_file(filename, commands, n_commands)
       character(len=*), intent(in) :: filename
       type(ListElement), allocatable, intent(out) :: commands(:)
       integer, intent(out) :: n_commands

       integer :: unit, i
       character(len=100) :: operation
       integer :: position, count
       character(len=100) :: text

       ! Открываем файл для чтения
       open(newunit=unit, file=filename, status='old', action='read')

       ! Предварительный подсчет количества команд в файле
       n_commands = 0
       do
           read(unit, '(A)', iostat=i)
           if (i /= 0) exit
           n_commands = n_commands + 1
       end do

       ! Возвращаемся в начало файла
       rewind(unit)

       ! Выделяем память для команд
       allocate(commands(n_commands))

       ! Считываем команды из файла
       n_commands = 0
       do
           read(unit, '(A, I, I)', iostat=i) operation, position, count
           if (i /= 0) exit
           n_commands = n_commands + 1
           select case (operation)
               case ('D')
                   ! Создаем команду удаления
                   commands(n_commands) = DeleteCommand(position, count)
               case ('I')
                   read(unit, '(A)', iostat=i) text
                   if (i /= 0) exit
                   ! Создаем команду вставки
                   commands(n_commands) = InsertCommand(position, trim(text))
               case default
                   ! Неподдерживаемая операция
           end select
       end do

       close(unit)
   end subroutine read_input_file
!    ! Структура данных для хранения строки исходного текста.
!    type Commands
!       character(CH_)   :: command
!       integer(I_) :: place
!       character(CH_) :: 
!       type(Commands), pointer        :: Next  => Null()
!    end type Commands

! contains
! function Read_first_line(InputFile) 
!   ! Чтение исходного кода. 
! function Read_Source_File(InputFile) result (First_line, Commands)
!    character :: Command
!    type(Commands), pointer  :: Command
!    character(*), intent(in) :: InputFile
!    integer  :: In
   
!    open (file=InputFile, encoding=E_, newunit=In)
!       Code => Read_Source_Line(in)
!    close (In)
! end function Read_Source_File

! ! Чтение строки исходного кода.
! recursive function Read_Source_Line(in) result(Code)
!    type(SourceLine), pointer  :: Code
!    integer, intent(in)        :: In
!    integer, parameter      :: max_len = 1024
!    character(max_len, CH_) :: string
!    integer                 :: IO

!    ! Чтение строки во временную строку бОльшей длины.
!    read (In, "(a)", iostat=IO) string
!    call Handle_IO_Status(IO, "reading line from source code")
!    if (IO == 0) then
!       allocate (Code)
!       ! Хранение в размещаемом поле символов без завершающих пробелов.
!       Code%String = Trim(string)
!       Code%Next => Read_Source_Line(In)
!    else
!       Code => Null()
!    end if
! end function Read_Source_Line

! ! Вывод строки
! subroutine printString(str)
!    character(len=*), intent(in) :: str
!    print *, str
!  end subroutine printString

! ! Вывод исходного кода.
! subroutine Output_Source_File(OutputFile, Code)
!    character(*), intent(in)      :: OutputFile 
!    type(SourceLine), intent(in)  :: Code 
!    integer  :: Out
   
!    open (file=OutputFile, encoding=E_, newunit=Out)
!       call Output_Source_Line(Out, Code)
!    close (Out)
! end subroutine Output_Source_File

! ! Вывод строки исходного кода.
! recursive subroutine Output_Source_Line(Out, Code)
!    integer, intent(in)           :: Out
!    type(SourceLine), intent(in)  :: Code
!    integer  :: IO

!    write (Out, "(a)", iostat=IO) Code%String
!    call Handle_IO_Status(IO, "writing line to file")
!    if (Associated(Code%next)) &
!       call Output_Source_Line(Out, Code%next)
! end subroutine Output_Source_Line

end module Source_IO 
