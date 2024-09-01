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
      type(phonebook), allocatable   :: next
   end type phonebook

contains
   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_phone_list(Input_File) result(Phone_List)
      type(phonebook), allocatable :: Phone_List
      character(*), intent(in)     :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         call Read_phone(In, Phone_List)
      close (In)
   end function Read_phone_list

   ! Чтение следующего студента.
   recursive subroutine Read_phone(In, phonepage)
      type(phonebook), allocatable :: phonepage
      integer, intent(in)          :: In
      character(:), allocatable    :: format
      integer  IO
      
      allocate (phonepage)
      format = '(A15, 1x, i10)'
      read (In, format, iostat=IO) phonepage%owner, phonepage%phone
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         call Read_phone(In, phonepage%next)
      else
         deallocate (phonepage)
      end if
   end subroutine Read_phone

   ! Вывод списка.
   subroutine Output_phone_list(Output_File, Phone_List, List_Name, Position)
      character(*), intent(in)     :: Output_File, Position, List_Name
      type(phonebook), allocatable :: Phone_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_phones(Out, Phone_List)
      close (Out)
   end subroutine Output_phone_list

   recursive subroutine Output_phones(Out, phonepage)
      type(phonebook), allocatable :: phonepage
      character(:), allocatable    :: format
      integer, intent(in)          :: Out
      integer                      :: IO

      format = '(A15, 1x, i10)'
      if (allocated(phonepage)) then 
         write (Out, format, iostat=IO) phonepage%owner, phonepage%phone 
         call Handle_IO_status(IO, "writing owners&phones")
         call Output_phones(Out, phonepage%next)
      end if
   end subroutine Output_phones

end module Phonebook_IO 
