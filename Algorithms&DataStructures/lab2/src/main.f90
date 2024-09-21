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
   use List_IO
   use List_Process

   implicit none

   ! Инициализация файлов ввода/вывода
   character(*), parameter   :: input_file = "../data/input.txt", &
                                output_file = "output.txt"

   ! Буфер для вставки текста
   character(:), allocatable :: trimmed_insert_buf 
   character(1024)           :: insert_buf
   ! Хранение типа команды ('I' - вставка, 'D' - удаление)
   character(1)              :: command
   ! Позиция для выполнения команды, количество удалений 
   integer(I_)               :: command_pos, delete_count
   integer(I_)               :: In, IO = 0
   ! Хранение списка
   type(list)                :: string 

   ! Чтение исходной строки
   call string%read_from_file(input_file)
   ! Вывод исходной строки
   call string%output_to_file(output_file, 'rewind')

   open(file=input_file, encoding=E_, newunit=In)
   ! Пропускаем первую строку
   read(In, *)
   ! Цикл чтения/выполнения команд
   do while (IO == 0)
      ! Чтение типа команды и позиции
      read(In, '(a1, 1x, i2, 1x)', iostat=io, advance='no') command, command_pos
      if (command .eq. 'I') then
         ! Считываем текст для вставки
         read(In, '(a)', iostat=IO) insert_buf
         ! Убираем лишние пробелы
         trimmed_insert_buf = trim(insert_buf)
         ! Вставка текста в строку
         call insert_into_text(string, command_pos, trimmed_insert_buf)
         ! Вывод команды в файл
         call write_insert_com_to_file(output_file, command, command_pos, insert_buf, 'append')
         ! Вывод итоговой строки в файл
         call string%output_to_file(output_file, 'append')

      else if(command .eq. 'D') then
         ! Считываем количество символов для удаления
         read(In, '(i2)', iostat=io) delete_count
         ! Удаление символов из строки
         call delete_from_text(string, command_pos, delete_count)
         ! Вывод команды в файл
         call write_delete_com_to_file(output_file, command, command_pos, delete_count, 'append')
         ! Вывод итоговой строки в файл
         call string%output_to_file(output_file, 'append')
      end if
   end do
   close(In)

end program lab_2
