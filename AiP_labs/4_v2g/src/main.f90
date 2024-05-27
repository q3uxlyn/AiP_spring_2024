! Даны значения вещественных переменных x и y
! Вычислить значение z, пользуясь функцией вида ABS(w) для определения модуля w при вещественном аргументе w
! и исходя из следующего правила вычисления z:
! z = (|x|+|y|)/2 при x<y; z = 1+2|x| при x>=y

! Алгоритм решения
! 1. Получить значения x и y из файла
! 2. Если x < y:
! 3. Вычислить z = (|x|+|y|)/2
! 4. Иначе:
! 5. Вычислить z = 1+2*|x|
! 6. Записать полученное значение в файл

! Значения для расчетов: x = -6.7, y = 3.20, z = 4.95
! x = 3.20, y = -6.70, z = 7.40

program paragraf2_7
   use Environment

   implicit none
   ! Путь к файлам, формат вывода данных
   character(*), parameter :: input_file  = "../data/input.txt", &
                              output_file = "output.txt",        &
                              fmt         = "(a, ' = ', f0.2)"
   ! Переменные ввода/вывода
   integer                 :: In = 0, Out = 0
   ! Переменные
   real(R_)                :: x = 0, y = 0, z = 0

   open (file=input_file, newunit=In)
      read (In, *) x, y
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, fmt) "x", x
      write (Out, fmt) "y", y
   close (Out)

   ! Вычисляем z используя чистую функцию
   z = F(x, y)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, fmt) "z", z
   close (Out)

contains
   ! Функция для вычисления значения z
   ! На вход получает x, y. Возвращает z
   pure function F(x, y) result(z)
      ! intent(in) - указывает, что переменные x, y RO и используются только для вычислений
      real(R_), intent(in) :: x, y
      real(R_)             :: z

      ! Сравниваем x и y и, в зависимости от результата, вычисляем z
      if (x<y) then
         z = (abs(x) + abs(y)) / 2.0
      else
         z = 1.0 + 2.0 * abs(x)
      end if
   end function F

end program paragraf2_7