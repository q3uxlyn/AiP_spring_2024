module List_IO
   use Environment


   ! структура для хранения символа строки
   type node
      character(1, kind=CH_)  :: char
      type(node), allocatable :: next
   end type node

   ! Структура списка хранения узлов
   type list
      type(node), allocatable :: first
      contains
         ! Чтение списка из файла
         procedure, pass :: read_from_file => read_list_from_file
         ! Запись списка в файл
         procedure, pass :: output_to_file => output_list_to_file
   end type list

   contains
      subroutine read_list_from_file(self, input_file)
         class(list)              :: self
         character(*), intent(in) :: input_file
         integer                  :: In

         open(file=input_file, encoding=E_, newunit=In)
            call recursive_read_list_node(In, self%first)
         close(In)
         
         contains
            !Рекурсивное чтение записей из файла в узел списка
            recursive subroutine recursive_read_list_node(In, elem)
               type(node), allocatable :: elem
               integer, intent(in)     :: In
               integer                 :: IO
               character(1, CH_)       :: temp

               ! Чтение символа
               read(In, '(a1)', iostat=IO, advance='no') temp
               call Handle_IO_status(IO, 'reading value from file')
               if (IO == 0) then
                  ! Выделение памяти для узла
                  allocate(elem)
                  ! Запись символа в узел
                  elem%char = temp
                  ! Рекурсивное чтение следующего узла
                  call recursive_read_list_node(In, elem%next)
               end if
            end subroutine recursive_read_list_node
      end subroutine read_list_from_file

      subroutine output_list_to_file(self, output_file, position)
         class(list)              :: self
         character(*), intent(in) :: output_file, position
         integer                  :: out

         open(file=output_file, encoding=E_, position=position, newunit=out)
            call recursive_write_list_node(Out, self%first)
         close(out)

         contains
            ! Рекурсивная запись в файл из узлов списка
            recursive subroutine recursive_write_list_node(out, curr)
               type(node), intent(in) :: curr
               integer, intent(in)    :: out
               integer                :: IO

               ! Запись символа в файл
               write(out, '(a)', iostat=IO, advance='no') curr%char
               call Handle_IO_status(IO, 'writing value to output')
               ! При наличии, рекурсивная запись следующего узла
               if (allocated(curr%next)) then
                  call recursive_write_list_node(out, curr%next)
               end if
            end subroutine recursive_write_list_node
      end subroutine output_list_to_file

      subroutine write_insert_com_to_file(output_file, command, command_pos, text_to_insert, position)
         character(*), intent(in)    :: output_file, position
         character(1024), intent(in) :: text_to_insert
         character(1)                :: command
         integer                     :: command_pos, Out

         open(file=output_file, encoding=E_, position=position, newunit=out)
            write(Out, '(a1, 1x, i2, 1x, a)') command, command_pos, trim(text_to_insert)
         close(Out)
         
      end subroutine write_insert_com_to_file

      subroutine write_delete_com_to_file(output_file, command, command_pos, delete_count, position)
         character(*), intent(in) :: output_file, position
         integer(I_), intent(in)  :: delete_count
         character(1)             :: command
         integer                  :: command_pos, Out

         open(file=output_file, encoding=E_, position=position, newunit=out)
            write(Out, '(a1, 1x, i2, 1x, i0)') command, command_pos, delete_count
         close(Out)
         
      end subroutine write_delete_com_to_file

end module List_IO 
