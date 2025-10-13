program Engine
    implicit none

    character(len=1) :: board(8,8)
    integer :: rank, file
    character(len=5) :: legalMoves(218)
    integer :: nMoves
    nMoves = 0
    board = ' '

    call system('clear')

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
            call genKingMovesW(board, file, rank, legalMoves, nMoves)
            call genKingMovesB(board, file, rank, legalMoves, nMoves)
        end do

    end do

    print*, "Legal moves (", nMoves, "):"
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
        integer :: maxMoves

        maxMoves = size(legalMoves)

        if (gameBoard(rankP, fileP) /= 'P') return

        nextRank = rankP + 1
        if (nextRank <= 8) then

            if (fileP > 1) then
                if (gameBoard(nextRank, fileP-1) >= 'a' .and. gameBoard(nextRank, fileP-1) <= 'z') then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        fileChar = achar(iachar('a') + fileP - 1)
                        rankChar = achar(iachar('0') + rankP)
                        nextRankChar = achar(iachar('0') + nextRank)
                        legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP - 2) // nextRankChar
                    end if
                end if
            end if

            if (fileP < 8) then
                if (gameBoard(nextRank, fileP+1) >= 'a' .and. gameBoard(nextRank, fileP+1) <= 'z') then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        fileChar = achar(iachar('a') + fileP - 1)
                        rankChar = achar(iachar('0') + rankP)
                        nextRankChar = achar(iachar('0') + nextRank)
                        legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP) // nextRankChar
                    end if
                end if
            end if
        end if

        if (nextRank <= 8) then
            if (gameBoard(nextRank, fileP) == ' ') then
                if (numMoves < maxMoves) then
                    numMoves = numMoves + 1
                    fileChar = achar(iachar('a') + fileP - 1)
                    rankChar = achar(iachar('0') + rankP)
                    nextRankChar = achar(iachar('0') + nextRank)
                    legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
                end if
            end if
        end if

        if (rankP == 2) then
            if (gameBoard(3, fileP) == ' ' .and. gameBoard(4, fileP) == ' ') then
                if (numMoves < maxMoves) then
                    numMoves = numMoves + 1
                    fileChar = achar(iachar('a') + fileP - 1)
                    rankChar = achar(iachar('0') + rankP)
                    nextRankChar = achar(iachar('0') + (rankP + 2))
                    legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
                end if
            end if
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
        integer :: maxMoves

        maxMoves = size(legalMoves)

        if (gameBoard(rankP, fileP) /= 'p') return

        nextRank = rankP - 1

        if (nextRank >= 1) then
            if (fileP > 1) then
                if (gameBoard(nextRank, fileP-1) >= 'A' .and. gameBoard(nextRank, fileP-1) <= 'Z') then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        fileChar = achar(iachar('a') + fileP - 1)
                        rankChar = achar(iachar('0') + rankP)
                        nextRankChar = achar(iachar('0') + nextRank)
                        legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP - 2) // nextRankChar
                    end if
                end if
            end if

            if (fileP < 8) then
                if (gameBoard(nextRank, fileP+1) >= 'A' .and. gameBoard(nextRank, fileP+1) <= 'Z') then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        fileChar = achar(iachar('a') + fileP - 1)
                        rankChar = achar(iachar('0') + rankP)
                        nextRankChar = achar(iachar('0') + nextRank)
                        legalMoves(numMoves) = fileChar // rankChar // achar(iachar('a') + fileP) // nextRankChar
                    end if
                end if
            end if
        end if

        if (nextRank >= 1) then
            if (gameBoard(nextRank, fileP) == ' ') then
                if (numMoves < maxMoves) then
                    numMoves = numMoves + 1
                    fileChar = achar(iachar('a') + fileP - 1)
                    rankChar = achar(iachar('0') + rankP)
                    nextRankChar = achar(iachar('0') + nextRank)
                    legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
                end if
            end if
        end if

        if (rankP == 7) then
            if (gameBoard(6, fileP) == ' ' .and. gameBoard(5, fileP) == ' ') then
                if (numMoves < maxMoves) then
                    numMoves = numMoves + 1
                    fileChar = achar(iachar('a') + fileP - 1)
                    rankChar = achar(iachar('0') + rankP)
                    nextRankChar = achar(iachar('0') + (rankP - 2))
                    legalMoves(numMoves) = fileChar // rankChar // fileChar // nextRankChar
                end if
            end if
        end if

    end subroutine genPawnMovesB

    subroutine genKingMovesW(gameBoard, fileK, rankK, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileK, rankK
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)

        if (gameBoard(rankK, fileK) /= 'K') return

        do df = -1, 1
            do dr = -1, 1
                if (df == 0 .and. dr == 0) cycle

                newFile = fileK + df
                newRank = rankK + dr

                if (newFile >= 1 .and. newFile <= 8 .and. newRank >= 1 .and. newRank <= 8) then
                    if (.not.(gameBoard(newRank, newFile) >= 'A' .and. gameBoard(newRank, newFile) <= 'Z')) then
                        if (numMoves < maxMoves) then
                            numMoves = numMoves + 1
                            fromFile = achar(iachar('a') + fileK - 1)
                            fromRank = achar(iachar('0') + rankK)
                            toFile   = achar(iachar('a') + newFile - 1)
                            toRank   = achar(iachar('0') + newRank)
                            legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                        end if
                    end if
                end if
            end do
        end do
    end subroutine genKingMovesW

        subroutine genKingMovesB(gameBoard, fileK, rankK, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileK, rankK
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)

        if (gameBoard(rankK, fileK) /= 'k') return

        do df = -1, 1
            do dr = -1, 1
                if (df == 0 .and. dr == 0) cycle

                newFile = fileK + df
                newRank = rankK + dr

                if (newFile >= 1 .and. newFile <= 8 .and. newRank >= 1 .and. newRank <= 8) then
                    if (.not.(gameBoard(newRank, newFile) >= 'a' .and. gameBoard(newRank, newFile) <= 'z')) then
                        if (numMoves < maxMoves) then
                            numMoves = numMoves + 1
                            fromFile = achar(iachar('a') + fileK - 1)
                            fromRank = achar(iachar('0') + rankK)
                            toFile   = achar(iachar('a') + newFile - 1)
                            toRank   = achar(iachar('0') + newRank)
                            legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                        end if
                    end if
                end if
            end do
        end do
    end subroutine genKingMovesB

end program Engine
