module Phonebook_IO
   use Environment

   implicit none
   ! Определение параметров для работы с данными
   integer, parameter :: PHONE_AMOUNT = 13500, &
                         OWNER_LEN    = 15, &
                         PHONE_LEN    = 10

   ! Структура данных для хранения фамилий и телефонов.
   type phonebook
      character(OWNER_LEN, kind=CH_) :: owner = ""
      integer(I_)                    :: phone = 0
      type(phonebook), pointer       :: next  => Null()
   end type phonebook

contains
   ! Чтение списка фамилии и телефоны.
   function Read_phone_list(Input_File) result(Phone_List)
      type(phonebook), pointer   :: Phone_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Phone_List => Read_phone(In)
      close (In)
   end function Read_phone_list

   ! Чтение следующего значения.
   recursive function Read_phone(In) result(Phone_List)
      type(phonebook), pointer  :: Phone_List
      integer, intent(in)       :: In
      character(:), allocatable :: format
      integer  IO
      
      allocate (Phone_List)
      format = '(A15, 1x, i10)'
      read (In, format, iostat=IO) Phone_List%owner, Phone_List%phone
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         Phone_List%next => Read_phone(In)
      else
         deallocate (Phone_List)
      end if
   end function Read_phone

   ! Вывод списка.
   subroutine Output_phone_list(Output_File, Phone_List, List_Name, Position)
      character(*), intent(in)    :: Output_File, Position, List_Name
      type(phonebook), intent(in) :: Phone_List
      integer                     :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_phones(Out, Phone_List)
      close (Out)
   end subroutine Output_phone_list

   recursive subroutine Output_phones(Out, phonepage)
      type(phonebook), intent(in) :: phonepage
      integer, intent(in)         :: Out
      character(:), allocatable   :: format
      integer                     :: IO
      
      format = '(A15, 1x, i10)'
      write (Out, format, iostat=IO) phonepage%owner, phonepage%phone
      call Handle_IO_status(IO, "writing owners&phones")
      if (Associated(phonepage%next)) &
         call Output_phones(Out, phonepage%next)
   end subroutine Output_phones
end module Phonebook_IO 
