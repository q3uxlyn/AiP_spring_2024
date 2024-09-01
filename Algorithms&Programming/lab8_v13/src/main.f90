! Феллер Г.М., группа 30022
! ЛР №8, вариант 13

! Задание:
! Составить процедуру-функцию, определяющую минимальный элемент массива A(M).
! Применить ее для построения массива B(5) из минимальных элементов строк заданной матрицы C(5,15)

! Указания:
! Реализация проводится с модулями. Все процедуры, предлагаемые для разработки, должны быть чистыми – иметь квалификатор pure. 
! Они не должны использовать встроенные функции по обработке массивов или сечений.

program paragraf8_13
   use Environment
   use Matrix_IO
   use Matrix_process 

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_), allocatable   :: C(:,:), B(:)
   integer                 :: N = 0, i = 0

   C = ReadMatrix(input_file)

   call OutputMatrix(output_file, C)

   call matrix_find_min(C, B)
   
   call OutputArray(output_file, B)

end program paragraf8_13
