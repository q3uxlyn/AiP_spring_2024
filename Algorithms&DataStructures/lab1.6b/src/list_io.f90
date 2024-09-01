module List_IO
   use Environment

   implicit none
   ! Определение параметров для работы с данными
   integer, parameter :: PHONE_AMOUNT = 12, &
                         OWNER_LEN    = 15, &
                         PHONE_LEN    = 10

   ! Структура данных для узла списка.
   ! Инициализация обязательна!
   type phonebook
      character(OWNER_LEN, kind=CH_) :: owner            = ""
      integer(I_)                    :: phone(PHONE_LEN) = 0
      type(phonebook), allocatable :: next
   end type phonebook

contains
   ! Чтение списка.
   function Read_list(Input_File) result(Phone_List)
      type(phonebook), allocatable :: Phone_List
      character(*), intent(in)     :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, encoding=E_, newunit=In)
        call Read_value(In, Phone_List)
      close (In)
   end function Read_list

   ! Чтение следующего значения.
   recursive subroutine Read_value(In, Elem)
      type(phonebook), allocatable :: Elem
      integer, intent(in)     :: In
      character(:), allocatable :: format
      integer  IO
      
      allocate (Elem)
      format = '(A15, 1x, ' // PHONE_LEN // 'i1)'
      read (In, format, iostat=IO, advance='no') Elem%value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
          call Read_value(In, Elem%next)
      else
         deallocate (Elem)
      end if
   end subroutine Read_value

   ! Вывод списка.
   subroutine Output_list(Output_File, Phone_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(phonebook), allocatable    :: Phone_List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, Phone_List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      type(phonebook), allocatable :: Elem
      character(:), allocatable :: format
      
      integer  :: IO

      format = '(A15, 1x, ' // PHONE_LEN // 'i1)'
      if (allocated(Elem)) then 
         write (Out, format, advance='no', iostat=IO) Elem%value 
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value
   
end module List_IO 
