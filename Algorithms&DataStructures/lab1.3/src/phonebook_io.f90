module Phonebook_IO
   use Environment

   implicit none
   integer, parameter :: PHONE_AMOUNT = 90000, &
                         OWNER_LEN    = 15, &
                         PHONE_LEN    = 10

   ! Структура данных для хранения фамилий и телефонов.
   type phonebook
      character(OWNER_LEN, kind=CH_) :: Owners = ""
      integer(I_)                    :: Phones = 0
   end type phonebook
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(phonebook)            :: phone
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = OWNER_LEN * CH_ + I_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(A15, 1x, I10)'
         do i = 1, PHONE_AMOUNT
            read (In, format, iostat=IO) phone
            call Handle_IO_status(IO, "reading formatted phonebook list, line " // i)
            
            write (Out, iostat=IO, rec=i) phone
            call Handle_IO_status(IO, "creating unformatted file with phonebook list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка: фамилии, телефонные номера.
   function Read_phone_list(Data_File) result(Phone_list)
      character(*), intent(in) :: Data_File
      type(phonebook) Phone_list(PHONE_AMOUNT)

      integer In, IO, recl
      
      recl = (OWNER_LEN * CH_ + I_) * PHONE_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Phone_list
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_phone_list
 
   ! Вывод списка.
   subroutine Output_phone_list(Output_File, Phone_list, List_name, Position)
      character(*), intent(in)     :: Output_File, Position, List_name
      type(phonebook), intent(in)  :: Phone_list(:)

      integer                      :: Out, IO
      character(:), allocatable    :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(A15, 1x, I10)'
         write (Out, format, iostat=IO) Phone_list
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_phone_list

end module Phonebook_IO
