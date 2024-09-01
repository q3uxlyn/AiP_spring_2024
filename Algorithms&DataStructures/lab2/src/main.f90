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

   character(*), parameter  :: input_file  = "../data/input.txt", &
                               output_file = "output.txt"


   type(StringList) :: string

   ! Чтение строки из стандартного ввода
   call read_line_from_console(head, char_count)
   ! Выводим считанную строку
   call print_line(head)
   ! print *, ""

   ! ! Чтение команд из входного файла
   ! call read_input_file('input.txt', commands, n_commands)

   ! ! Выполнение команд из файла
   ! do i = 1, n_commands
   !    ! Выполняем текущую команду
   !    call commands(i)%execute(head)
   !    ! Вывод обновленной строки
   !    call print_line(head)
   !    print *, ""
   ! end do

   ! type(ListLines), pointer :: line => Null() 

   ! Line = Read_first_line(input_file, line)
   
   ! Input => Read_commands(input_file)

   ! ! Вывод исходной строки
   ! call printString(Input)
   
   ! call Output_Source_File(output_file, Input)
  
   ! ! if (Associated(InitialCode) .and. Associated(ModdedCode)) then
   ! !    DiffCode => Diff_Codes(InitialCode, ModdedCode)
      
   ! !    if (Associated(DiffCode)) &
   ! !    call Output_Source_Code(F3, DiffCode)
   ! ! end if

end program lab_2
