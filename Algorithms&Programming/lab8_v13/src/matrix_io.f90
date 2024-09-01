module Matrix_IO
   use Environment

   implicit none
contains
   ! Чтение матрицы из файла
   function ReadMatrix(input_file) result(C)
      character(*), intent(in) :: input_file
      !dir$ attributes align : 32 :: C
      real(R_), allocatable    :: C(:, :)

      integer :: In = 0, N = 0, M = 0
   
      open (file=input_file, newunit=In)
         read (In, *) N, M
         ! Размещаем матрицу по строкам (нужно для обхода по строкам - так они будут сплошные)
         ! 1 индекс - столбца, 2 индекс - строки.
         allocate (C(M, N))
         read (In, *) C
      close (In)
   end function ReadMatrix
  
   ! Вывод матрицы C
   subroutine OutputMatrix(output_file, C)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: C(:, :)

      integer :: Out = 0, i = 0
   
      open (file=output_file, encoding=E_, position='rewind', newunit=Out)
         write(out, "(a)") "matrix C:"
         write(out, "(" // UBound(C, 1) // "f6.2)") (C(:, i), i = 1, UBound(C, 2))
      close (Out)
   end subroutine OutputMatrix
   
   ! Вывод массива
   subroutine OutputArray(output_file, B)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: B(:)

      integer :: Out = 0, i = 0
   
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         write (Out, *)
      do i = 1, size(B)
         write (Out, '(a, I0, a, f6.2)') "Минимальный элемент строки ", i, ": ", B(i)
      end do
      close (Out)
   end subroutine OutputArray
end module Matrix_IO
