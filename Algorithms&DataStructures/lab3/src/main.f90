! Феллер Г.М., группа 30022
! ЛР 3, Вариант 8

! Задание:
! В текстовом файле In задана последовательность символов, 
! в Delete – дpугая последовательность.
! Разpаботать пpоцедуpы:
! ◦ Фopмиpования линейного списка S из символов стpоки текстового файла In.
! ◦ Фоpмиpования множества M из символов стpоки заданного текстового файла Delete.
! ◦ Исключения из заданного множества M букв латинского алфавита, входящих в заданный список S.
! ◦ Вывода линейного списка символов S в текстовый файл Out.
! ◦ Уничтожения динамического списка S.
! Множество наследовать от списка. Множество отличается от списка тем, что в нём элементы не повторяются.
! Используя эти пpоцедуpы, создать список из букв файла In, множество – из букв стpоки файла Delete, 
! а элементы множества, за исключением входящих в In, вывести в файл Out.
! После вывода удалить динамический список S и множество M.

! Указания:
! Задание выполняется в виде программного проекта из двух модулей, 
! в котором необходимо использовать динамические линейные списки. 
! Списки необходимо обрабатывать чистой хвостовой рекурсией при всех операциях с ними. 
! При возможности применяется регулярное программирование.

program lab3
   use Environment
   use List_IO
   use List_Process
   
   implicit none
   
   character(:), allocatable :: input_file, output_file, delete_file, header
   ! Объявление списка
   type(list)                :: string
   ! Объявление множества
   type(set)                 :: delete_string 

   input_file = '../data/input.txt'
   delete_file = '../data/delete.txt'
   output_file = 'output.txt'

   ! Чтение списка из файла
   call string%read_from_file(input_file)
   ! Чтение множества из файла
   call delete_string%read_from_file(delete_file)
   
   ! Вывод списка и множества в файл
   header = 'Строка в списке'
   call string%output_to_file(output_file, header, 'rewind')
   header = 'Строка в множестве'
   call delete_string%output_to_file(output_file, header, 'append')

   ! Вычитание списка из множества
   !call subtract_list_from_set(delete_string, string)
   ! Вычитание множества из списка
   call subtract_set_from_list(string, delete_string)

   header = 'Список после вычитания'
   call string%output_to_file(output_file, header, 'append')
   ! header = 'Множество после вычитания'
   ! call delete_string%output_to_file(output_file, header, 'append')

   ! Удаление списка и множества из памяти
   call string%clear()
   call delete_string%clear()
end program lab3
