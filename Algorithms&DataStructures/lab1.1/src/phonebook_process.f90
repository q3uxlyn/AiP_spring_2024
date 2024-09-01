module Phonebook_Process
    use Environment
    use Phonebook_IO

    implicit none
    
contains

    pure subroutine Sort_phonebook(Owners, Phones)
        character(OWNER_LEN, kind=CH_) :: Owners(:)
        integer(I_)                    :: Phones(:)
        integer                        :: i, j

        intent(inout)                  :: Owners, Phones

        ! Проходим по всем элементам массива, начиная с второго
        do concurrent(i = 2:PHONE_AMOUNT)
            j = i - 1
            do while (j >= 1 .and. Phones(j) < Phones(i))
                j = j - 1
            end do
            Owners(j+1:i) = cshift(Owners(j+1:i), -1)
            Phones(j+1:i) = cshift(Phones(j+1:i), -1)
        end do

    end subroutine Sort_phonebook
    
end module Phonebook_Process
