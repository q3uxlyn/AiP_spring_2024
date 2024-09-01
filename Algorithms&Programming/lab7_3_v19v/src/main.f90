! Феллер Г.М., группа 30022
! ЛР №7.3, вариант 19в

! Задание:
! Найти число элементов матрицы B(15,25), являющихся:
! в) нулевыми
! Напечатать индексы элементов, обладающих каждым из этих свойств.

! Указания:
! Count. Составить маску. Упаковать массив индексов

program paragraf7_19v
   use Environment
   
   implicit none
   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, count_zeros = 0
   real(R_), allocatable   :: B(:, :)
   integer, allocatable    :: Indexes(:,:), Ind_zero(:,:)
   logical, allocatable    :: Mask(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (B(N, M))
      read (In, *) (B(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (B(i, :), i = 1, N)
   close (Out)

   ! Размеры маски и исходной матрицы должны совпадать,
   ! т.к. каждый элемент в маске соответствует элементу в исходной матрице для проверки, равен ли он нулю
   ! Выделяем память для массива mask размером N*M и заполняем его ложными значениями
   allocate (Mask(N*M), source=.false.)
   allocate (Indexes(N*M, 2))

   call ZeroIndexes(B, count_zeros, Mask, Indexes, Ind_zero)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, *)
      write (Out, '(a, I5)') "Число нулевых элементов матрицы:", count_zeros
      write (Out, *)
      write (Out, *) "Индексы нулевых элементов:"
      write (Out, '(2i3)') (Ind_zero(i, :), i = 1, UBound(Ind_zero, 1)) 
   close (Out)

contains
   pure subroutine ZeroIndexes(B, count_zeros, Mask, Indexes, Ind_zero)
      real(R_), intent(in)              :: B(:,:)
      integer, intent(out)              :: count_zeros, Indexes(:,:)
      integer, allocatable, intent(out) :: Ind_zero(:,:)
      logical, intent(out)              :: Mask(:)
      
      integer  i, j

      ! Формируем двумерный массив индексов
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      ! Создание маски для нулевых элементов
      ! Записываем логическое true, если соответствующий элемент массива В == 0
      Mask = [B == 0]

      ! Подсчет нулевых элементов по созданной маске
      count_zeros = count(Mask)

      ! Размещение массива индексов
      allocate(Ind_zero(count_zeros, 2))

      ! Упаковка массива индексов
      Ind_zero = Reshape(Pack(Indexes, Spread(Mask, 2, 2)), [count_zeros, 2])

   end subroutine ZeroIndexes
end program paragraf7_19v
