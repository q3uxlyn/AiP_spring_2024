module Matrix_process
   use Environment

   implicit none

contains

    pure real(R_) function find_min(A, l)
        real(R_)    :: A(:, :)
        intent(in)  :: A, l
        integer     :: l, j
        
        find_min = A(1, l)
        !dir$ attributes align : 32 :: C(j,:)
        do j = 2, UBound(A, 1)
           if (A(j, l) < find_min) then
              find_min = A(j, l)
           end if
        end do
    end function find_min

    pure subroutine matrix_find_min(A, B)
       real(R_), intent(in)               :: A(:, :)
       integer                            :: N, i
       real(R_), allocatable, intent(out) :: B(:)

       N = UBound(A, 2)
      
       allocate (B(N))

       do concurrent (i = 1:N)
          B(i) = find_min(A, i)
       end do

    end subroutine matrix_find_min

end module Matrix_process 
