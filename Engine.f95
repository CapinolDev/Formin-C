program Engine
    implicit none

    character(len=1) :: board(8,8)
    integer :: rank, file
    character(len=5) :: legalMoves(218)
    integer :: nMoves
    nMoves = 0
    board = ' '

    call initBoard(board)


    do rank = 8, 1, -1   
        write(*,'(A)', advance='no') trim(adjustl(itoa(rank))) // " | "
        do file = 1,8
            write(*,'(A)', advance='no') board(rank,file) // ' '
        end do
        print*  
        if (rank > 1) print*, '  ----------------'
    end do

    print*, "   a b c d e f g h"


    do file = 1,8
        do rank = 1, 8
            call genPawnMovesW(board, file, rank, legalMoves, nMoves)
            call genPawnMovesB(board, file, rank, legalMoves, nMoves)
        end do
        
    end do

    print*, "Legal moves:"
    do rank = 1, nMoves
        print*, legalMoves(rank)
    end do

contains
  
    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=2) :: s
        write(s,'(I1)') i
    end function itoa

    
    subroutine initBoard(board)
        implicit none
        character(len=1), intent(out) :: board(8,8)
        board = ' '

        board(1,:) = ['R','N','B','Q','K','B','N','R']
        board(2,:) = ['P','P','P','P','P','P','P','P']

        board(8,:) = ['r','n','b','q','k','b','n','r']
        board(7,:) = ['p','p','p','p','p','p','p','p']

    end subroutine initBoard


    subroutine genPawnMovesW(gameBoard, fileP, rankP, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileP, rankP
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: nextRank
        character :: fileChar, rankChar, nextRankChar


        nextRank = rankP + 1
        if (nextRank <= 8) then
    
            if (fileP > 1 .and. gameBoard(nextRank, fileP-1) >= 'a' .and. gameBoard(nextRank, fileP-1) <= 'z') then
                numMoves = numMoves + 1
                fileChar = achar(iachar('a') + fileP - 1)
                rankChar = achar(iachar('0') + rankP)
                nextRankChar = achar(iachar('0') + nextRank)
                legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP - 2) // nextRankChar
            end if
   
            if (fileP < 8 .and. gameBoard(nextRank, fileP+1) >= 'a' .and. gameBoard(nextRank, fileP+1) <= 'z') then
                numMoves = numMoves + 1
                fileChar = achar(iachar('a') + fileP - 1)
                rankChar = achar(iachar('0') + rankP)
                nextRankChar = achar(iachar('0') + nextRank)
                legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP) // nextRankChar
            end if
        end if
        if (nextRank <= 8 .and. gameBoard(nextRank, fileP) == ' ') then
            numMoves = numMoves + 1
            fileChar = achar(iachar('a') + fileP - 1)
            rankChar = achar(iachar('0') + rankP)
            nextRankChar = achar(iachar('0') + nextRank)
            legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
        end if


        if (rankP == 2 .and. gameBoard(3, fileP) == ' ' .and. gameBoard(4, fileP) == ' ') then
            numMoves = numMoves + 1
            fileChar = achar(iachar('a') + fileP - 1)
            rankChar = achar(iachar('0') + rankP)
            nextRankChar = achar(iachar('0') + (rankP + 2))
            legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
        end if


    end subroutine genPawnMovesW

    subroutine genPawnMovesB(gameBoard, fileP, rankP, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileP, rankP
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: nextRank
        character :: fileChar, rankChar, nextRankChar

        nextRank = rankP - 1
        
        if (nextRank >= 1) then
            if (fileP > 1 .and. gameBoard(nextRank, fileP-1) >= 'A' .and. gameBoard(nextRank, fileP-1) <= 'Z') then
                numMoves = numMoves + 1
                fileChar = achar(iachar('a') + fileP - 1)
                rankChar = achar(iachar('0') + rankP)
                nextRankChar = achar(iachar('0') + nextRank)
                legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP - 2) // nextRankChar
            end if
 
            if (fileP < 8 .and. gameBoard(nextRank, fileP+1) >= 'A' .and. gameBoard(nextRank, fileP+1) <= 'Z') then
                numMoves = numMoves + 1
                fileChar = achar(iachar('a') + fileP - 1)
                rankChar = achar(iachar('0') + rankP)
                nextRankChar = achar(iachar('0') + nextRank)
                legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP) // nextRankChar
            end if
        end if

        if (nextRank >= 1 .and. gameBoard(nextRank, fileP) == ' ') then
            numMoves = numMoves + 1
            fileChar = achar(iachar('a') + fileP - 1)
            rankChar = achar(iachar('0') + rankP)
            nextRankChar = achar(iachar('0') + nextRank)
            legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
        end if
        if (rankP == 7 .and. gameBoard(6, fileP) == ' ' .and. gameBoard(5, fileP) == ' ') then
            numMoves = numMoves + 1
            fileChar = achar(iachar('a') + fileP - 1)
            rankChar = achar(iachar('0') + rankP)
            nextRankChar = achar(iachar('0') + (rankP - 2))
            legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
        end if


    end subroutine

end program Engine
