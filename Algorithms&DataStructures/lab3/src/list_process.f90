module List_Process
   use Environment
   use List_IO

contains
   ! Вычитание списка из множества
   subroutine subtract_list_from_set(first, second)
      type(set)     :: first
      type(list)    :: second
      intent(inout) :: first
      intent(in)    :: second

      ! Рекурсивная обработка элементов списка
      call iterate_list(first, second%first)

      contains 
         recursive subroutine iterate_list(set_val, list_current)
            type(node), allocatable :: list_current
            type(set)               :: set_val

            ! Удаление элемента
            call recursive_delete_from_set(set_val%first, list_current%char)

            if(allocated(list_current%next)) &
               call iterate_list(set_val, list_current%next)
         end subroutine iterate_list

         ! Удаление элемента из множества
         recursive subroutine recursive_delete_from_set(current, char_delete)
            type(node),allocatable :: current, tmp
            character(1, CH_)      :: char_delete

            ! Если нашли символ, эту рекурсию можно схлопывать т.к. в множестве элементы уникальны
            if(letters_equal(current%char, char_delete)) then
               call move_alloc(current%next, tmp)
               call move_alloc(tmp, current)
            else if(allocated(current%next)) then
               call recursive_delete_from_set(current%next, char_delete)
            end if
         end subroutine recursive_delete_from_set
   end subroutine subtract_list_from_set
   
   ! Вычитание множества из списка
   subroutine subtract_set_from_list(first, second)   
      type(list)    :: first
      type(set)     :: second
      intent(inout) :: first
      intent(in)    :: second

      call iterate_set(first, second%first)

      contains 
         recursive subroutine iterate_set(list_val, set_current)
            type(node), allocatable :: set_current
            type(list)              :: list_val

            call recursive_delete_from_list(list_val%first, set_current%char)

            if(allocated(set_current%next)) &
               call iterate_set(list_val, set_current%next)
         end subroutine iterate_set

         recursive subroutine recursive_delete_from_list(current, char_delete)
            type(node),allocatable :: current, tmp
            character(1, CH_)      :: char_delete

            ! Даже если нашли символ, эту рекурсию нельзя схлопывать т.к. в списке элементы НЕ уникальны
            if(letters_equal(current%char, char_delete)) then
               call move_alloc(current%next, tmp)
               call move_alloc(tmp, current)
            end if

            if(allocated(current%next)) then
               call recursive_delete_from_list(current%next, char_delete)
            end if
         end subroutine recursive_delete_from_list
   end subroutine subtract_set_from_list

   ! Сравнение двух букв (не символов), например вызов с аргументами ('H', 'h') вернет true
   pure function letters_equal(first, second) result(res)
      character(1, CH_), intent(in) :: first, second
      logical                       :: res

      ! Сравнение символов, где разница в 32 соответствует разнице между строчной и заглавной буквой
      res = first == second .or. (abs(iachar(second) - iachar(first)) == 32)

   end function letters_equal
end module List_Process
