! Феллер Г.М., группа 30022
! ЛР №7.4, вариант 26

! Задание:
! Заменить k-ю строку матрицы A(50,50) заданным вектором X(50),
! а l-й столбец - вектором Y(50)

! Указания:
! Использовать сечения двумерного массива.

program paragraf7_26
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   integer                 :: k = 0, l = 0
   real(R_), allocatable   :: A(:, :)
   real(R_), allocatable   :: X(:), Y(:)

   open (file=input_file, newunit=In)
      read (In, *) N, k, l
      allocate (A(N, N))
      allocate (X(N))
      allocate (Y(N))
      read (In, *) (A(i, :), i = 1, N)
      read (In, *) X
      read (In, *) Y
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *) "Исходная матрица A(N,N):"
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
      write (Out, *)
      write (Out, *) "Вектор X(N):"
      write (Out, "("//N//"f6.2)") (X(i), i = 1, N)
      write (Out, *)
      write (Out, *) "Вектор Y(N):"
      write (Out, "("//N//"f6.2)") (Y(i), i = 1, N)
   close (Out)
   
   ! Замена k-ой строки и l-го столбца
   A(k, :) = X
   A(:, l) = Y
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, *) "Матрица после замены:"
      write (Out, '('//N//'f6.2)') (A(i, :), i = 1, N)
   close (Out)

   deallocate(A)
   deallocate(X)
   deallocate(Y)

end program paragraf7_26
