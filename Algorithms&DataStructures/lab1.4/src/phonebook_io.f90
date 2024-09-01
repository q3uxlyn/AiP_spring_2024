module Phonebook_IO
   use Environment

   implicit none
   integer, parameter :: PHONE_AMOUNT = 100000, &
                         OWNER_LEN    = 15, &
                         PHONE_LEN    = 10

   ! Структура данных для хранения фамилий и телефонов.
   type phonebook
      character(OWNER_LEN, kind=CH_), allocatable :: owner(:)
      integer(I_), allocatable                    :: phone(:)
   end type phonebook
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)  :: Input_File, data_file
      character(:), allocatable :: format
      integer                   :: In, Out, IO, i

      character(OWNER_LEN, kind=CH_), allocatable :: tmp_owner(:)
      integer(I_), allocatable                    :: tmp_phone(:)


      allocate(tmp_owner(PHONE_AMOUNT), tmp_phone(PHONE_AMOUNT))

      open (file=input_file, encoding=E_, newunit=In)
            format = '(a15, 1x, i10)'
            read (In, format, iostat=IO) (tmp_owner(i), tmp_phone(i), i = 1, PHONE_AMOUNT)
            call Handle_IO_status(IO, "reading formatted group list, line " // i)
         close (In)

         open (file=data_file, form='unformatted', newunit=Out, access='stream')               
               write (Out, iostat=IO) tmp_owner, tmp_phone
               call Handle_IO_status(IO, "creating unformatted file with group list, record " // i)
         close (Out)
   end subroutine Create_data_file
   
   ! Чтение списка: фамилии, телефонные номера.
   function Read_phone_list(Data_File) result(phonepage)
      character(*), intent(in) :: Data_File
      type(phonebook)          :: phonepage
      integer                  :: In, IO
      
      open (file=Data_File, form='unformatted', newunit=In, access='stream')
         allocate(phonepage%owner(PHONE_AMOUNT), phonepage%phone(PHONE_AMOUNT))
         read (In, iostat=IO) phonepage%owner, phonepage%phone
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_phone_list
 
   ! Вывод списка.
   subroutine Output_phone_list(Output_File, phonepage, List_name, Position)
      character(*), intent(in)    :: Output_File, Position, List_name
      type(phonebook), intent(in) :: phonepage
      character(:), allocatable   :: format
      integer                     :: Out, IO, i
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(A15, 1x I10)'
         write (Out, format, iostat=IO) (phonepage%owner(i), phonepage%phone(i), i = 1, PHONE_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_phone_list

end module Phonebook_IO
