module Phonebook_Process
   use Environment
   use Phonebook_IO

   implicit none
   
contains
   ! Сортировка списка телефонов по убыванию.
   pure subroutine Sort_phone_list(Phone_list)
      type(phonebook), intent(inout) :: Phone_list(:)
      integer                        :: i, j

      ! Проходим по всем элементам массива, начиная с второго
      do i = 2, PHONE_AMOUNT
         j = i - 1
         do while (j >= 1 .and. Phone_list(j)%Phones < Phone_list(i)%Phones)
             j = j - 1
         end do
         Phone_list(j + 1:i) = cshift(Phone_list(j+1:i), -1)
      end do
   end subroutine Sort_phone_list

end module Phonebook_Process
