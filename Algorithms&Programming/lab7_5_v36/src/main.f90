! Феллер Г.М., группа 30022
! ЛР №7.5, вариант 36

! Задание:
! Преобразовать одномерный массив из 100 элементов в двумерный массив B(5,20).
! Формирование проводить по столбцам

! Указания:
! Размер массива должен быть кратен 5. В результирующем двумерном массиве должно быть 5 строк. Reshape

program paragraf7_36
   use Environment
   
   implicit none
   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, rows = 0, cols = 0, i = 0
   real(R_), allocatable   :: A(:), B(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N, rows, cols
      allocate (A(N))
      read (In, *) A
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *)
      write (Out, *) "Исходный одномерный массив:"
      write (Out, '('//N//'f7.2)') (A(i), i = 1, N)
   close (Out)
 
   allocate (B(rows, cols))

   ! Преобразуем одномерный массив A в двумерный
   B = reshape(A, (/rows, cols/))
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, *) "Двумерный массив:"
      write (Out, '('//cols//'f7.2)') (B(i, :), i = 1, rows) 
   close (Out)

end program paragraf7_36
