module List_process
   use Environment

   implicit none

   type node
      character(kind=CH_) :: value = ''
      type(node), allocatable :: next
   end type node

contains

   function Read_list(Input_File, is_set) result(List)
      type(node), allocatable :: List
      character(*), intent(in) :: Input_File
      logical, intent(in) :: is_set
      integer(I_)                :: In

      open (file=Input_File, newunit=In)
         call Read_value(In, List, is_set)
      close (In)

   end function Read_list

   recursive subroutine Read_value(In, Elem, is_set)
      type(node), allocatable :: Elem
      integer, intent(in) :: In
      logical, intent(in) :: is_set
      integer :: IO

      allocate (Elem)
      read (In, '(a1)', iostat=IO, advance='no') Elem%value
      call Handle_IO_status(IO, "reading value from file")

      if (IO == 0) then
         call Read_value(In, Elem%next, is_set)
      else
         deallocate(Elem)
      end if
   end subroutine Read_value

   subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in) :: Output_File, List_Name, Position
      type(node), allocatable :: List
      integer :: Out

      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)

   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in) :: Out
      type(node), allocatable :: Elem
      integer :: IO

      if (allocated(Elem)) then
         write (Out, '(a1)', advance='no', iostat=IO) Elem%value
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value

end module List_process
