module List_Process
   use Environment
   use List_IO

   contains
      ! Подпрограмма удаления элементов из списка
      subroutine delete_from_text(container, position, count)
         ! Список из которого удаляются элементы
         type(list), intent(inout) :: container
         ! Позиция и количество элементов для удаления
         integer, intent(in)       :: position, count
         
         ! Рекурсивный поиск и удаление
         call find_and_delete(container%first, position, count)

         contains
            ! Рекурсивная процедура для поиска узла по позиции и удаления элементов
            recursive subroutine find_and_delete(current, num, total)
               type(node), allocatable :: current                  
               intent(inout)           :: current  
               integer, intent(in)     :: num, total                      
                              
               ! Если позиция найдена
               if(num == 1) then
                  ! Удаляем нужное количество узло                   
                  call counted_delete(current, total)                   
               else 
                  ! Если позиция не найдена - ищем дальше
                  call find_and_delete(current%next, num-1, total)       
               end if
            end subroutine find_and_delete                        
                                           
            ! Рекурсивная процедура удаления указанного количества узлов
            recursive subroutine counted_delete(current, num)           
               type(node), allocatable :: current, tmp                  
               intent(inout)           :: current  
               integer, intent(in)     :: num                      
               
               ! Перемещаем из следующего узла в временную переменную
               call move_alloc(current%next, tmp)
               ! Перемещаем из временной переменной в текущий узел 
               call move_alloc(tmp, current) 
               
               ! Продолжаем удаление, если нужно удалить больше одного узла
               if(num /= 1) &
                  call counted_delete(current, num-1)
            end subroutine counted_delete                          
      end subroutine delete_from_text

      ! Подпрограмма вставки текста в список
      subroutine insert_into_text(container, position, text)
         ! Список в который вставляем текст
         type(list), intent(inout) :: container
         ! Позиция для вставки
         integer                   :: position
         ! Текст для вставки
         character(:), allocatable :: text
         intent(in)                :: position, text
         
         ! Рекурсивный поиск позиции для вставки
         call find_and_insert(container%first, position)
   
         contains
            ! Рекурсивная процедура для поиска позиции и вставки текста
            recursive subroutine find_and_insert(current, num)
               type(node), allocatable   :: current               
               intent(inout)             :: current  
               integer, intent(in)       :: num                 
                                                                    
               if(num == 1) then
                  ! Вставляем текст, если найдена позиция                
                  call recursive_insert(current, text)
               else                   
                  call find_and_insert(current%next, num-1)       
               end if
            end subroutine find_and_insert       
  
            ! Рекурсивная процедура для вставки текста по символам
            recursive subroutine recursive_insert(current, txt_in)
               type(node), allocatable   :: current, temp             
               intent(inout)             :: current  
               character(:), allocatable :: txt_in

               ! Если текст пуст, завершаем вставку
               if(txt_in%len == 0) return

               ! Проверяем существует ли текущий узел (выделена память)
               if(allocated(current)) then
                  ! Перемещаем текущий узел в temp, current освобождается
                  call move_alloc(current, temp)
                  ! Выделяем память для текущешл узла
                  allocate(current)
                  ! Устанавливаем ссылку на следующий узел
                  call move_alloc(temp, current%next)
               else
                  ! Выделяем память для текущего узла
                  allocate(current)
               end if
                  
               ! Вставляем первый символ текста
               current%char = txt_in(1:1)
               ! Удаляем вставленный символ из текста
               txt_in = txt_in(2:)

               ! Продолжаем вставку следующего символа
               call recursive_insert(current%next, txt_in)

            end subroutine recursive_insert
      end subroutine insert_into_text

end module List_Process
