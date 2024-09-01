module Phonebook_IO
    use Environment

    implicit none
    integer, parameter :: PHONE_AMOUNT = 90000, &
                          OWNER_LEN    = 15, &
                          PHONE_LEN    = 10

contains
    subroutine Read_phonebook(input_file, Owners, Phones) 
        character(*), intent(in)  :: input_file
        character(:), allocatable :: format
        integer                   :: In, IO, i

        character(OWNER_LEN, kind=CH_), intent(out) :: Owners(:)
        integer(I_), intent(out)                    :: Phones(:)
        

        open (file=input_file, encoding=E_, newunit=In)
            format = '(A15, 1x, I10)'
            read(In, format, iostat=IO) (Owners(i), Phones(i), i = 1, PHONE_AMOUNT)
            call Handle_IO_status(IO, "reading phonebook list")
        close (In)
    
    end subroutine Read_phonebook

    subroutine Write_phonebook(output_file, Owners, Phones, list_name, position)
        character(OWNER_LEN, kind=CH_) :: Owners(:)
        character(:), allocatable      :: format
        character(*)                   :: output_file, list_name, position
        integer(I_)                    :: Phones(:)
        integer                        :: Out, IO, i
        intent(in)                     :: Owners, output_file, list_name, position, Phones

        open(file=output_file, encoding=E_, position=position, newunit=Out)
            write(Out, '(/a)') list_name
            format = '(A15, 1x, I10)'
            write(Out, format, iostat=io) (Owners(i), Phones(i), i = 1, PHONE_AMOUNT)
            call Handle_IO_status(IO, "writing phonebook")
        close(Out)
    end subroutine Write_phonebook

end module Phonebook_IO
