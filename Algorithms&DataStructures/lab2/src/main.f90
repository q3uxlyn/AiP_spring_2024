! Феллер Г.М., группа 30022
! ЛР 2, Вариант 8

! Задание:
! Разработать чистую подпрограмму вставки в строке на N-ое место М символов 
! и чистую подпрограмму удаления в строке с N-го места М символов. 
! После каждой вставки или удаления выводить обновленную строку. 
! Режим ввода или удаления, а также число символов задаются во входном файле.

! Пример входного файла:
! Just for what?
! D 10 5
! I 10 fun!
! Пример выходного файла:
! Just for what?
! D 10 5
! Just for_
! I 10 fun!
! Just for fun!

! Указания:
! Задание выполняется в виде программного проекта из двух модулей, 
! в котором необходимо использовать динамические однонаправленные списки.
! Списки необходимо обрабатывать чистой хвостовой рекурсией при всех операциях с ними.
! При возможности применяется регулярное программирование.
! Элементом списка является символ строки.
! Головная программа должна состоять из вызова процедур: 
! чтение строки, вывод строки, а затем в цикле: 
! чтение команды, выполнение команды, вывод строки.

program lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", &
                              output_file = "output.txt"
  
   
   type(string), allocatable :: source_line ! исходная строка
   type(command), allocatable :: command_line 
   logical :: end_of_file
   integer(I_) :: In, IO

   open(file=input_file, newunit=In, iostat=IO, encoding=E_)
   ! чтение исходной строки из файла к которой будут применяться команды
   source_line = read_string(In)
   ! вывод исходной строки
   call write_string(output_file, source_line, "rewind")

   ! открываем файл для чтения команд
   
   ! read(In, "(A)", advance="no") ! пропускаем первую строку
   ! call Handle_IO_Status(IO, "Read first line")

   ! чтение и выполнение команд
   end_of_file = .false.

   do while (.not. end_of_file)
      command_line = read_command(In)
      if (command_line%cmd_type == "") then
         end_of_file = .true.
         exit
      end if

      ! вывод команды перед выполнением
      call write_command(output_file, command_line, "append")

     ! select case (cmd%cmd_type)
     ! case ('I') ! вставка
     !    call insert_chars(source_line, cmd%pos, cmd%insert_str)
     !    call write_string(output_file, source_line, "append")

      !case ('D') ! удаление
      !   call delete_chars(source_line, cmd%pos, cmd%num)
      !   call write_string(output_file, source_line, "append")
      !end select
   end do

   close(In)

end program lab_2
