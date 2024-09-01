! Феллер Г.М., группа 30022
! ЛР №5, вариант 14а

! Дан массив X (25). Зафиксировать индексы элементов,
! значения которых: равно 0

! Алгоритм решения:
! 1. Создать пустой список Y для хранения индексов элементов со значением 0
! 2. Пройти по всем элементам массива Х, для каждого элемента:
!    2.1 Проверить, равен ли текущий > 0
!    2.2 Если > 0 -> добавить его индекс в список Y
! 3. После завершения прохода по всем элементам массива, вернуть список Y 

program paragraf5_v14a
   use Environment
   
   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt"
   integer                 :: In = 0, Out = 0, i = 0, k = 0
   integer                 :: N = 0
   real, allocatable       :: X(:)
   integer, allocatable    :: poz_indices(:)

   ! Считываем с файла размер массива
   ! Выделяем память для массива Х
   ! Записываем элементы  массива X
   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (X(N))
      read (In, *) X
   close (In)

   open (file=output_file, newunit=Out)
      write(Out, "(a)") "X:"
      write(Out, "(f6.2)") (X(i), i = 1, N)
   close (Out)

   ! Выделяем память для массива zero_indices
   allocate (poz_indices(N))
   ! Используем Pack для получения одномерного массива по заданной маске
   poz_indices = Pack([(k, k = 1, N)], X > 0)
   
   open (file=output_file, newunit=Out, position='append')
      if (size(poz_indices) > 0) then
         write(Out, "(a)") "indices of pozitive elements:"
         write(Out, "(I5)") (poz_indices(i), i = 1, size(poz_indices))
      else
         write(Out, "(a)") "There is no elements > 0"
      endif
  close (Out)

  ! Освобождение памяти
  deallocate (X)
  deallocate (poz_indices)
end program paragraf5_v14a
