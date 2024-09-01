module Phonebook_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Phonebook_IO

   implicit none
   
contains
   ! Сортировка списка телефонов по убыванию.
   subroutine Sort_phone_list(phonepage)
      type(phonebook), intent(inout) :: phonepage
      integer                        :: i, j

      ! Проходим по всем элементам массива, начиная с индекса 2.
      do concurrent (i = 2:PHONE_AMOUNT)
         j = i - 1
         do while (j >= 1 .and. phonepage%phone(j) < phonepage%phone(i))
            j = j - 1
            if (j == 0) exit
         end do
         phonepage%owner(j+1:i) = cshift(phonepage%owner(j+1:i), -1)
         phonepage%phone(j+1:i) = cshift(phonepage%phone(j+1:i), -1)
      end do

   end subroutine Sort_phone_list

end module Phonebook_Process