module Phonebook_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Phonebook_IO

   implicit none
   
contains
   ! Сортировка списка телефонов по убыванию.
   pure recursive subroutine Sort_phone_list(phonepage, i)
      type(phonebook), intent(inout) :: phonepage
      integer :: j
      integer, intent(in) :: i

      j = i - 1

      do while (j >= 1 .and. (phonepage%phone(j) < phonepage%phone(i)))
         j = j - 1
      end do

      phonepage%owner(j+1:i) = cshift(phonepage%owner(j+1:i), -1)
      phonepage%phone(j+1:i) = cshift(phonepage%phone(j+1:i), -1)
      
      if (i <= PHONE_AMOUNT) &
         call Sort_phone_list(phonepage, i+1)

   end subroutine Sort_phone_list

end module Phonebook_Process
