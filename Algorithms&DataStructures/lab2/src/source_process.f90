! Copyright 2015 Fyodorov S. A.

module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none
    ! Базовый тип данных для элемента списка и команды


contains

   ! Метод для выполнения команды вставки
   subroutine insert_execute(self, head)
       type(InsertCommand), intent(inout) :: self
       type(ListElement), pointer :: head
       integer :: i

       ! Вставляем текст посимвольно, начиная с конца строки
       do i = len(trim(self%text)), 1, -1
           call insert(head, self%position, self%text(i:i))
       end do
   end subroutine insert_execute

   ! Метод для выполнения команды удаления
   subroutine delete_execute(self, head)
       type(DeleteCommand), intent(inout) :: self
       type(ListElement), pointer :: head

       ! Удаляем заданное количество символов, начиная с указанной позиции
       call delete(head, self%position, self%count)
   end subroutine delete_execute

   ! Пустой метод для выполнения элемента списка
   subroutine charnode_execute(self, head)
       type(CharNode), intent(inout) :: self
       type(ListElement), pointer :: head
       ! Элементы списка не выполняют никаких действий
   end subroutine charnode_execute

   ! Процедура для вставки символа в список по указанной позиции
   recursive subroutine insert(head, position, ch)
       type(ListElement), pointer :: head
       integer :: position
       character :: ch

       type(CharNode), pointer :: new_node

       if (position == 1) then
           ! Создаем новый узел и вставляем его в начало списка
           allocate(new_node)
           new_node%value = ch
           new_node%next => head
           head => new_node
       elseif (position > 1) then
           ! Рекурсивно вызываем для следующего узла
           call insert(head%next, position - 1, ch)
       end if
   end subroutine insert

   ! Процедура для удаления символов из списка начиная с указанной позиции
   recursive subroutine delete(head, position, count)
       type(ListElement), pointer :: head
       integer :: position, count

       type(ListElement), pointer :: temp

       if (associated(head)) then
           if (position == 1) then
               if (count > 0) then
                   ! Удаляем символы из списка
                   temp => head%next
                   deallocate(head)
                   head => temp
                   ! Рекурсивно вызываем для удаления следующего символа
                   call delete(head, position, count - 1)
               end if
           else
               ! Рекурсивно вызываем для следующего узла
               call delete(head%next, position - 1, count)
           end if
       end if
   end subroutine delete

! contains
!    ! Формирование разницы двух кодов в виде новых строк.
!    pure recursive function Diff_Codes(InitialCode, ModdedCode) result(DiffCode)
!       type(SourceLine), pointer     :: DiffCode
!       type(SourceLine), intent(in)  :: InitialCode
!       type(SourceLine), intent(in)  :: ModdedCode

!       ! Поиск и запись отличных строк в рамках исходного файла InitialCode.
!       ! Если строки равны:
!       if (InitialCode%String == ModdedCode%String) then
!          ! Если остались ещё строки, то переход к следующей.
!          if (Associated(InitialCode%next)) then
!             DiffCode => Diff_Codes(InitialCode%next, ModdedCode%Next)
!          ! В противном случае если остались строки в модифицированном файле, то добавление их в список.
!          else if (Associated(ModdedCode%next)) then
!             ! Запись всех строк оставшейся части ModdedCode.
!             DiffCode => Add_Recent_Source_Lines(ModdedCode%next)
!          end if !  ELSE DiffCode => Null()
!       ! Если строки не равны, то добавление её в список.
!       else
!          allocate (DiffCode)
!          DiffCode%String = CH__"++ " // ModdedCode%String
!          DiffCode%next => Diff_Codes(InitialCode, ModdedCode%Next)
!       end if
!    end function Diff_Codes

!    pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
!       type(SourceLine), pointer     :: DiffCode
!       type(SourceLine), intent(in)  :: ModdedCode

!       allocate (DiffCode)
!       DiffCode%String = CH__"++ " // ModdedCode%String
!       if (Associated(ModdedCode%next)) &
!          DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
!    end function Add_Recent_Source_Lines

end module Source_process
