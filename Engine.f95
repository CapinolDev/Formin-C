program Engine
    implicit none

    character(len=1) :: board(8,8)
    integer :: rank, file


    board = ' '

    
    board(1,:) = ['R','N','B','Q','K','B','N','R']  
    board(2,:) = ['P','P','P','P','P','P','P','P']  


    board(8,:) = ['r','n','b','q','k','b','n','r']  
    board(7,:) = ['p','p','p','p','p','p','p','p'] 


    do rank = 8, 1, -1   
        write(*,'(A)', advance='no') trim(adjustl(itoa(rank))) // " | "
        do file = 1,8
            write(*,'(A)', advance='no') board(rank,file) // ' '
        end do
        print*  
        if (rank > 1) print*, '  ----------------'
    end do

    print*, "   a b c d e f g h"

contains
  
    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=2) :: s
        write(s,'(I1)') i
    end function itoa
    

end program Engine
