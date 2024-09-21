module List_IO
    use Environment
 
    ! Структура узла списка
    type node
        character(1, kind=CH_)  :: char
        type(node), allocatable :: next 
    end type node
 
    ! Определение типа список
    type list
        ! Ссылка на первый элемент списка
        type(node), allocatable :: first
        contains
            ! Чтение из файла
            procedure, pass :: read_from_file => read_list_from_file
            ! Запись в файл
            procedure, pass :: output_to_file => output_list_to_file
            ! Очистка списка
            procedure, pass :: clear => delete_list
    end type list
 
    ! Определение типа множество (расширение списка)
    type, extends(list) :: set
        contains 
            ! Чтение множества из файла
            procedure, pass :: read_from_file => read_set_from_file
    end type
 
    contains
        ! Чтение списка из файла
        subroutine read_list_from_file(self, Input_File) 
            class(list)              :: self
            character(*), intent(in) :: Input_File
            integer                  :: In
            
            open(file=input_file, encoding=E_, newunit=In)
                ! Рекурсивное чтение элементов
                call recursive_read_list_node(In, self%first)
            close(In)
        
            contains
                ! Рекурсивное чтение записей из файла в узел списка
                recursive subroutine recursive_read_list_node(In, elem)
                type(node), allocatable :: elem                                 
                integer, intent(in)     :: In 
                integer                 :: IO
                character(1, CH_)       :: temp

                ! Чтение символа
                read(In, '(a1)', iostat=io, advance='no') temp
                if(io == 0) then
                    ! Выделение памяти для узла
                    allocate(elem)
                    ! Присваивание символа
                    elem%char = temp
                    ! Рекурсивный вызов для следующего узла
                    call recursive_read_list_node(In, elem%next)
                end if
                end subroutine recursive_read_list_node
        end subroutine read_list_from_file

        ! Запись в файл
        subroutine output_list_to_file(self, output_file, header, Position)
            class(list)              :: self
            character(*), intent(in) :: output_File, header, Position
            integer                  :: Out
            
            open(file=output_file, encoding=E_, position=Position, newunit=Out)            
                write(Out, '(/a)') header
                write(Out, '(a)') "--------------------------------------------"
                ! Рекурсивная запись узлов списка
                call recursive_write_list_node(Out, self%first)
            close(Out)

            contains
                ! Рекурсивная запись в файл из узлов списка
                recursive subroutine recursive_write_list_node(Out, curr)                
                type(node), intent(in) :: curr                   
                integer, intent(in)    :: Out                    
                integer                :: IO                        

                write(Out, '(a)', iostat=io, advance='no') curr%char                       
                call Handle_IO_status(io, 'writing value to output')     
                ! Если есть следующий узел    
                if(allocated(curr%next)) then                    
                    call recursive_write_list_node(Out, curr%next)        
                end if                                                       
                end subroutine recursive_write_list_node                       
        end subroutine output_list_to_file

        ! Чтение множества из файла
        subroutine read_set_from_file(self, Input_file)
            class(set)              :: self
            character(*), intent(in) :: Input_File
            integer                  :: In

            open(file=input_file, encoding=E_, newunit=In)
                call recursive_read_set_node(In, self%first)
            close(In)
        
            contains
                ! Рекурсивное чтение записей из файла в узел списка
                recursive subroutine recursive_read_set_node(In, elem)
                type(node), allocatable :: elem                                 
                integer, intent(in)     :: In 
                integer                 :: IO
                character(1, CH_)       :: temp
                logical                 :: has_symbol
                read(In, '(a1)', iostat=io, advance='no') temp
                call Handle_IO_status(io, 'reading value from file')
                if(io == 0) then
                    ! Проверка на уникальность символа
                    call look_for_char(self%first, temp, has_symbol)
                    ! Если символ уникален - добавляем
                    if(.not. has_symbol) then                     
                        allocate(elem)
                        elem%char = temp
                        call recursive_read_set_node(In, elem%next)
                    end if
                    call recursive_read_set_node(In, elem)
                end if
                end subroutine recursive_read_set_node

                ! Проверка наличия символа в множестве
                recursive subroutine look_for_char(current, char, found)
                type(node), allocatable :: current
                character(1, CH_)       :: char
                logical                 :: found
                intent(inout)           :: found

                if(allocated(current)) then
                    ! Если символ не совпадает, продолжаем поиск
                    if(current%char .ne. char) then
                        call look_for_char(current%next, char, found)
                    else 
                        ! Символ найден
                        found = .true.
                    end if
                else
                    ! Символ не найден
                    found = .false.
                end if
                end subroutine look_for_char
        end subroutine read_set_from_file

        ! Очистка списка (освобождение памяти)
        subroutine delete_list(self)
            class(list) :: self
            
            call recursive_delete_list_node(self%first)

            contains 
                ! Рекурсивное удаление узлов
                recursive subroutine recursive_delete_list_node(current)
                type(node), allocatable :: current, temp
                intent(inout)           :: current

                ! Перемещение указателей для освобождения памяти
                if(allocated(current%next)) &
                    call move_alloc(current%next, temp)

                ! Освобождение памяти текущего узла
                deallocate(current)

                ! Удаление следующего узла
                if(allocated(temp)) &
                    call recursive_delete_list_node(temp)
                end subroutine recursive_delete_list_node
        end subroutine delete_list
 end module List_IO
 