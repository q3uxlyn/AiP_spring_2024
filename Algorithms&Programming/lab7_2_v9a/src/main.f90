! Феллер Г.М., группа 30022
! ЛР №7.2, вариант 9a

! Задание:
! Вычислить нормы квадратной матрицы А, содержащей 100 элементов (N = 10):
! a) || A || = sqrt(sum_{i=1}{N}sum{j=1}{N}a_ij^2)

! Указания: Norm2

program paragraf7_9a
   use Environment
   
   implicit none
   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt"
   integer                 :: In = 0, Out = 0, i, N = 0
   real(R_), allocatable   :: A(:, :)
   real                    :: norm

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N, N))
      read (In, *) (A(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f7.2)') (A(i, :), i = 1, N)
   close (Out)

   ! Вычисление нормы матрицы
   ! reshape(A, [N*N]) - преобразует двумерную матрицу в одномерный массив
   norm = Norm2([A])

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, *) "Эвклидова норма вектора:"
      write (Out, '(f0.2)') norm
   close (Out)

   deallocate(A)

end program paragraf7_9a
