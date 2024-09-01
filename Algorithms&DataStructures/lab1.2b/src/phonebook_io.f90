module Phonebook_IO
    use Environment

    implicit none
    integer, parameter :: PHONE_AMOUNT = 100000, &
                          OWNER_LEN    = 15, &
                          PHONE_LEN    = 10

contains
    ! Чтение списка: владельцы и телефоны.
    subroutine Read_phonebook(Input_File, Owners, Phones)
        character(*), intent(in)         :: Input_File
        character(kind=CH_), intent(out) :: Owners(:, :)
        integer(I_), intent(out)         :: Phones(:)
        character(:), allocatable        :: format

        integer In, IO, i

        open (file=Input_File, encoding=E_, newunit=In)
            format = '(' // OWNER_LEN // 'A1, 1x, I10)'
            ! Храним по столбцам (j, i)
            read (In, format, iostat=IO) (Owners(:, i), Phones(i), i = 1, PHONE_AMOUNT)
            call Handle_IO_status(IO, "reading phone list")
        close (In)
    end subroutine Read_phonebook

    ! Вывод списка: владельцы и телефоны.
    subroutine Write_phonebook(Output_File, Owners, Phones, List_name, Position)
        character(*), intent(in)        :: Output_File, Position, List_name
        character(kind=CH_), intent(in) :: Owners(:, :)
        integer(I_), intent(in)         :: Phones(:)
        integer                         :: Out, i, IO
        character(:), allocatable       :: format

        open (file=output_file, encoding=E_, position=position, newunit=Out)
            write (out, '(/A)') List_name
            format = '(' // OWNER_LEN // 'A1, 1x, I10)'
            write (Out, format, iostat=IO) (Owners(:, i), Phones(i), i = 1, PHONE_AMOUNT)
            call Handle_IO_status(IO, "writing " // List_name)
        close (Out)
    end subroutine Write_phonebook
    
end module Phonebook_IO