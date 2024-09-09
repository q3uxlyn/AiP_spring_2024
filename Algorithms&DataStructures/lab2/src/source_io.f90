module Source_IO
   use Environment
   implicit none

   ! структура для хранения символа строки
   type string
      character(kind=CH_)       :: symbol = ""
      type(string), allocatable :: next
   end type string

   type command
      character                 :: cmd_type   ! тип команды: I - insert, D - delete 
      integer(kind=I_)          :: pos        ! позиция для вставки/удаления
      integer(kind=I_)          :: num        ! количество символов для удаления (если тип D)
      type(string), allocatable :: insert_str ! строка для вставки (если тип I)
   end type command

   contains

      ! Чтение исходной строки
      function read_string(In) result (source_line)
         type(string), allocatable :: source_line
         integer(I_), intent(in)   :: In

         ! open (file=input_file, encoding=E_, newunit=In)
         call read_char(In, source_line)
         ! close (In)
      end function read_string

      recursive subroutine read_char(In, elem)
         integer(I_), intent(in)   :: In
         type(string), allocatable :: elem
         integer                   :: IO

         allocate(elem)
         read(In, "(a1)", iostat=IO, advance="no") elem%symbol
         call Handle_IO_Status(IO, "Reading symbol from source line")
         if(IO == 0) then
            call read_char(In, elem%next)
         else
            deallocate(elem)
         end if
      end subroutine read_char

      ! Вывод исходной строки
      subroutine write_string(output_file, source_line, position)
         character(*), intent(in)  :: output_file, position
         type(string), allocatable :: source_line
         integer                   :: Out

         open (file=output_file, position=position, encoding=E_, newunit=Out)
            call write_symbol(Out, source_line)
         close (Out)
      end subroutine write_string

      recursive subroutine write_symbol(Out, elem)
         integer, intent(in)       :: Out
         type(string), allocatable :: elem
         integer :: IO
         
         if(allocated(elem)) then
            write (Out, "(a1)", iostat=IO, advance="no") elem%symbol
            call Handle_IO_Status(IO, "Writing symbol to file")
            call write_symbol(Out, elem%next)
         end if
      end subroutine write_symbol 

      ! подпрограмма для чтения команды из файла
      function read_command(In) result (command_line)
         type(command), allocatable :: command_line
         integer(I_), intent(in)    :: In

         integer :: IO

         read(In, '(a1)', iostat=IO, advance="no") command_line%cmd_type
         select case (command_line%cmd_type)
            case ("I")
               read(In, '(I1)') command_line%pos
               call read_insert_string(In, command_line%insert_str)
            case ("D")
               read(In, *) command_line%pos, command_line%num
         end select
      end function read_command

      recursive subroutine read_insert_string(In, elem)
         integer(I_), intent(in)   :: In
         type(string), allocatable :: elem
         integer :: IO

         allocate (elem)
         read(In, "(A1)", iostat=IO, advance="no") elem%symbol
         call Handle_IO_Status(IO, "Reading symbol for insert string")
         if (IO == 0) then
            call read_insert_string(In, elem%next)
         else
            deallocate(elem)
         end if
      end subroutine read_insert_string

      subroutine write_command(output_file, cmd, position)
         character(*), intent(in)  :: output_file, position
         integer :: Out
         type(command), intent(in) :: cmd
         integer :: IO

         open (file=output_file, position=position, encoding=E_, newunit=Out)

         select case (cmd%cmd_type)
         case ('I')
            write (Out, "(A, I0, A)", iostat=IO, advance="no") cmd%cmd_type, cmd%pos, ' '
            call write_insert_string(Out, cmd%insert_str)
         case ('D') 
            write (Out, "(A, A, I0, A, I0)", iostat=IO, advance="no") cmd%cmd_type, ' ', cmd%pos, ' ', cmd%num
         end select

         call Handle_IO_Status(IO, "Writing command")

         close(Out)
      end subroutine write_command

      recursive subroutine write_insert_string(Out, elem)
         integer, intent(in) :: Out
         type(string), allocatable :: elem
         integer :: IO

         if (allocated(elem)) then
            write(Out, "(a1)", iostat=IO, advance="no") elem%symbol
            call Handle_IO_Status(IO, "Writing symbol for insert string")
            call write_insert_string(Out, elem%next)
         end if

      end subroutine write_insert_string

end module Source_IO 
