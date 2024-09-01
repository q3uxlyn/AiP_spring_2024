! Феллер Г.М., группа 30022
! ЛР 1, Вариант 8
! Средства:
!   - массивы строк
!   - массивы символов 
!   - массивы структур
!   - структура массивов
!   - файлы записей
!   + хвостовая рекурсия
!   - динамические однонаправленные списки
!   + рекурсивный размещаемый тип

! Необходимо прочитать список из не менее чем 12 строк.
! Данные в одной строке имеют заданный формат и отделяются друг от друга дополнительным пробелом.

! Задание.
! Дан список владельцев телфонов в виде:
! ФАМИЛИЯ    ТЕЛЕФОН
! 15 симв.   10 симв.
! Пример входного файла:
! Петров         9111634576
! Фёдоров        9111635687
! Отсортировать этот список в порядке убывания номеров телефонов, используя метод вставок.
! Пример выходного файла:
! Фёдоров        9111635687
! Петров         9111634576

program phone_book
   use Environment
   use Phonebook_Process
   use Phonebook_IO

   implicit none
   
   character(*), parameter  :: input_file  = "../data/input.txt", &
                               output_file = "output.txt"

   type(phonebook), allocatable :: Phone_List

   real :: start = 0, finish = 0
   
   Phone_List = Read_phone_list(input_file)
   
   call Output_phone_list(output_file, Phone_List, "Исходный список:", "rewind")

   call cpu_time(start)

   call Sort_phone_list(Phone_List, Phone_List)

   call cpu_time(finish)
   print '("Time = ", f0.3)', (finish-start)

   call Output_phone_list(output_file, Phone_List, "Отсортированный список:", "append")

end program phone_book
