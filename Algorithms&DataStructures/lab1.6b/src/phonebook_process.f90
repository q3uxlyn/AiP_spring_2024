module Phonebook_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Phonebook_IO

   implicit none

contains
   pure recursive subroutine Sort_phone_list(Phone_list, Sorted_list)
      type(phonebook), allocatable, intent(inout) :: Phone_list, Sorted_list
      type(phonebook), allocatable :: temp

      if (Allocated(Sorted_list%next)) then
         if (Sorted_list%phone < Sorted_list%next%phone) then
            call move_alloc(Sorted_list%next, temp)
            call move_alloc(temp%next, Sorted_list%next)

            call Insertion(Phone_list, temp)
            call Sort_phone_list(Phone_List, Sorted_list)

         else
            call Sort_phone_list(Phone_List, Sorted_list%next)
         end if
      end if
   end subroutine Sort_phone_list

   pure recursive subroutine Insertion(Sorted_list, temp)
      type(phonebook), allocatable, intent(inout) :: Sorted_list, temp

      if (Sorted_list%phone < temp%phone) then
         ! Временный элемент становится первым в списке.
         call move_alloc(Sorted_list, temp%next)
         call move_alloc(temp, Sorted_list)
      else 
         ! Рекурсивно вызываем эту же функцию для следующего элемента списка.
         call Insertion(Sorted_list%next, temp)
      end if

   end subroutine Insertion
   
end module Phonebook_process