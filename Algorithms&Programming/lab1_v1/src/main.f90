! Решить систему двух линейных алгебраических уравнений: ax_1+bx_2=c,  dx_1+ex_2=f
! по формулам Крамера x_1=(-bf+ec)/(ae-db),  x_2=(af-cd)/(ae-db)
! предполагая, что заданные значения a,b,c,d,e,f таковы, что определитель системы не равен нулю

! Алгоритм:
! 1. Проверить, что определитель (det=ae-bd) не равен 0
! 2. Если равен 0, система не имеет единственного решения
! 3. Если определитель не равен 0, продолжить вычисления
! 4. Вычислить значения x_1 и x_2
! 5. Вывести ответ

! ненулевой определитель: 1.5 2.7 3.6 2.4 0.8 1.2
! нулевой определитель: 0.2 0.3 0.1 0.4 0.6 0.2

program paragraf1_v1
   use Environment

   implicit none
   ! Путь к файлам, формат вывода данных
   character(*), parameter    :: input_file  = "../data/input.txt", &
                                 output_file = "output.txt",        &
                                 fmt         = "(a, ' = ', f0.2)"
   ! Переменные ввода/вывода
   integer                    :: In = 0, Out = 0
   ! Массив значений коэффициентов уравнений
   real(R_)                   :: a = 0, b = 0, c = 0, d = 0, e = 0, f = 0
   ! Переменные
   real(R_)                   :: det = 0, x1 = 0, x2 = 0

   ! Берем из файла input.txt значения коэффициентов
   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) a, b, c, d, e, f
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      ! Записываем полученные данные в файл output.txt
      write (Out, fmt) "a", a
      write (Out, fmt) "b", b
      write (Out, fmt) "c", c
      write (Out, fmt) "d", d
      write (Out, fmt) "e", e
      write (Out, fmt) "f", f
   close (Out)

   ! Вычисляем значение определителя системы
   det = a*e - b*d

   ! Открываем файл для записи решения и добавляем пустую строку-разделитель
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, *)

      ! Проверка на ненулевой определитель
      if (det == 0.0) then
         ! Сообщение об ошибке
          write (Out, *) "Определитель системы равен 0. Решение невозможно."

      else
         ! Вычисление решений
         x1 = (-b*f + e*c) / det
         x2 = (a*f - c*d) / det
         write (Out, *) "Решение системы уравнений:"
         write (Out, fmt) "x1", x1
         write (Out, fmt) "x2", x2
      endif
   close (Out)

end program paragraf1_v1