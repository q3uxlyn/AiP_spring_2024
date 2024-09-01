module Phonebook_IO
    use Environment

    implicit none
    integer, parameter :: PHONE_AMOUNT = 90000, &
                          OWNER_LEN    = 15, &
                          PHONE_LEN    = 10

contains
   ! Чтение списка: владельцы и телефоны.
   subroutine Read_phonebook(Input_File, Owners, Phones)
      character(*), intent(in)  :: Input_File
      character(kind=CH_)       :: Owners(:, :)
      integer(I_)               :: Phones(:)
      character(:), allocatable :: format
      intent(out)               :: Owners, Phones

      integer  In, IO, i
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // OWNER_LEN // 'A1, 1x, I10)'
         read (In, format, iostat=IO) (Owners(i, :), Phones(i), i = 1, PHONE_AMOUNT)
         call Handle_IO_status(IO, "reading phones list")
      close (In)
   end subroutine Read_phonebook

   ! Вывод списка: владельцы и телефоны.
   subroutine Write_phonebook(Output_File, Owners, Phones, List_name, Position)
      character(*), intent(in)  :: Output_File, Position, List_name
      character(kind=CH_)       :: Owners(:, :)
      integer(I_)               :: Phones(:)
      character(:), allocatable :: format

      intent(in)                :: Owners, Phones
      integer Out, i, IO
      
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/A)') List_name
         format = '(' // OWNER_LEN // 'A1, 1x, I10)'
         write (Out, format, iostat=IO) (Owners(i, :), Phones(i), i = 1, PHONE_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Write_phonebook

end module Phonebook_IO