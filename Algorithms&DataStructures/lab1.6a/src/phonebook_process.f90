module Phonebook_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Phonebook_IO

   implicit none

contains
   pure recursive subroutine Sort_phone_list(Phone_list, Sorted_list)
      type(phonebook), pointer :: Phone_list, temp, Sorted_list

      if (associated(Phone_list)) then
         ! Указатель на текущий элемент сохраняется во временной переменной.
         temp => Phone_list
         ! Перемещаем указатель на следующий элемент списка.
         Phone_list => Phone_list%next
         ! Обнуляем указатель на следующий элемент временной переменной.
         temp%next => null()
         ! Вызываем подпрограмму вставки для вставки временного элемента в отсортированный список.
         call Insertion(Sorted_list, temp)
         ! Рекурсивно вызываем эту же функцию для оставшихся элементов списка.
         call Sort_phone_list(Phone_list, Sorted_list)
      end if
   end subroutine Sort_phone_list

   pure recursive subroutine Insertion(Sorted_list, temp)
      type(phonebook), pointer :: Sorted_list, temp

      if (.not. associated(Sorted_list) .or. Sorted_list%phone < temp%phone) then
         ! Временный элемент становится первым в списке.
         temp%next => Sorted_list
         Sorted_list => temp
      else 
         ! Рекурсивно вызываем эту же функцию для следующего элемента списка.
         call Insertion(Sorted_list%next, temp)
      end if

   end subroutine Insertion
   
end module Phonebook_process
 

   

   