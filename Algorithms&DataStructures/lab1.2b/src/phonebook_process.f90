module Phonebook_Process
    use Environment
    use Phonebook_IO

    implicit none
    
contains
    ! Сортировка телефонной книги
    pure subroutine Sort_phone_book(Owners, Phones)
        character(kind=CH_), intent(inout) :: Owners(:, :)
        integer(I_), intent(inout)         :: Phones(:)
        integer                            :: i, j

        ! Проходим по всем элементам массива, начиная с второго
        do concurrent(i = 2:PHONE_AMOUNT)
            j = i - 1
            do while (j >= 1 .and. Phones(j) < Phones(i))
                j = j - 1
            end do
            Owners(:, j+1:i) = cshift(Owners(:, j+1:i), -1, dim=2)
            Phones(j+1:i) = cshift(Phones(j+1:i), -1)
        end do

    end subroutine Sort_phone_book
    
end module Phonebook_Process