! Феллер Г.М., группа 30022
! ЛР 1, Вариант 8
! Средства:
!   - массивы строк
!   + массивы символов Array(j, i)
!   - массивы структур
!   - структура массивов
!   - файлы записей
!   - хвостовая рекурсия
!   - динамические однонаправленные списки
!   - рекурсивный размещаемый тип

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
   use Phonebook_IO
   use Phonebook_Process

   implicit none
   character(*), parameter :: input_file  = "../data/input_kr.txt", &
                              output_file = "output.txt"

   ! Массивы фамилий и телефонов. Array(j, i): j - столбец, i - строка
   character(kind=CH_) :: Owners(OWNER_LEN, PHONE_AMOUNT) = ""
   integer(I_)         :: Phones(PHONE_AMOUNT)

   real :: start = 0, finish = 0

   call Read_phonebook(input_file, Owners, Phones)

   call Write_phonebook(output_file, Owners, Phones, "Исходный список:", "rewind")

   call cpu_time(start)

   call Sort_phone_book(Owners, Phones)

   call cpu_time(finish) 
   print '("Time = ", f0.3)', (finish-start)

   call Write_phonebook(output_file, Owners, Phones, "Отсортированный список:", "append")

end program phone_book