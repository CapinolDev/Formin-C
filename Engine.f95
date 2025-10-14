program Engine
    implicit none

    character(len=1) :: board(8,8)
    integer :: rank, file
    character(len=5) :: legalMoves(218)
    integer :: nMoves, showChoices, ios, showValidator, moveInputValidator
    integer :: playerSelectValidator
    character(len=5) :: selectedPlayer
    real :: randHelper, posEval
    character(len=5) :: userMove
    integer :: cf, cr, gf, gr
    logical :: valid, game


    game = .true.
    moveInputValidator = 0
    showValidator = 0
    nMoves = 0
    board = ' '
    showChoices = 0
    playerSelectValidator = 0

    do while (playerSelectValidator ==  0)
        call system('clear')   

        print*,'Which color do you want to be?'
        print*,'1. White'
        print*,'2. Black'
        print*,'3. Random'
        read(*,*,IOSTAT = ios) playerSelectValidator
        if (ios /= 0 .or. playerSelectValidator < 1 .or. playerSelectValidator > 3) then
            print*, 'Invalid input - please input an int between 1 and 3'
            playerSelectValidator = 0
        else
            select case (playerSelectValidator)
                case (1)
                    selectedPlayer = 'White'
                case (2)
                    selectedPlayer ='Black'
                case (3)
                    call random_number(randHelper)
                    playerSelectValidator = floor(randHelper*2+1)
                    select case (playerSelectValidator)
                        case (1)
                            selectedPlayer = 'White'
                        case (2)
                            selectedPlayer ='Black'
                        end select
            end select
        end if

    end do
    call initBoard(board)
    do while (game)
        call system('clear')
        moveInputValidator = 0
        print*, 'You are ',selectedPlayer

        

        

        do rank = 8, 1, -1
            write(*,'(A)', advance='no') trim(adjustl(itoa(rank))) // " | "
            do file = 1,8
                write(*,'(A)', advance='no') board(rank,file) // ' '
            end do
            print*
            if (rank > 1) print*, '  ----------------'
        end do

        print*, '   a b c d e f g h'

        nMoves = 0
        legalMoves = ''

        do file = 1,8
            do rank = 1,8
                
                call genPawnMovesW(board, file, rank, legalMoves, nMoves)
                call genKingMovesW(board, file, rank, legalMoves, nMoves)
                call genKnightMovesW(board, file, rank, legalMoves, nMoves)                    
                call genRookMovesW(board, file, rank, legalMoves, nMoves)
                call genBishopMovesW(board, file, rank, legalMoves, nMoves)
                call genQueenMovesW(board, file, rank, legalMoves, nMoves)          
                
                call genPawnMovesB(board, file, rank, legalMoves, nMoves)
                call genKingMovesB(board, file, rank, legalMoves, nMoves)
                call genKnightMovesB(board, file, rank, legalMoves, nMoves)
                call genRookMovesB(board, file, rank, legalMoves, nMoves)
                call genBishopMovesB(board, file, rank, legalMoves, nMoves)                    
                call genQueenMovesB(board, file, rank, legalMoves, nMoves)
                              
            end do

        end do

        call evalPos(board, posEval)
        print*, posEval

        do while (moveInputValidator == 0)

            print*, 'Enter your move (e.g., e2e4):'
            read(*,'(A)') userMove

            call parseMoveAndValidate(trim(userMove), legalMoves, nMoves, cf, cr, gf, gr, valid)

            if (valid) then
                print*, 'Move accepted:', trim(userMove)
       
                call makeMove(board, cf, cr, gf, gr, board(cr, cf))
                moveInputValidator = 1
            else
                print*, 'Invalid or illegal move! Try again.'
            end if
        end do
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

    subroutine makeMove(board, currentFile, currentRank, goalFile, goalRank, piece)
        character(len=1), intent(inout) :: board(8,8)
        integer, intent(in) :: currentFile, currentRank, goalFile, goalRank
        character(len=1), intent(inout) :: piece

        board(goalRank, goalFile) = piece
        board(currentRank, currentFile) = ' '
        
               
           
    end subroutine makeMove

    subroutine parseMoveAndValidate(move, legalMoves, nMoves, currentFile, currentRank, goalFile, goalRank, isValid)
        implicit none
        character(len=*), intent(in) :: move
        character(len=5), intent(in) :: legalMoves(:)
        integer, intent(in) :: nMoves
        integer, intent(out) :: currentFile, currentRank, goalFile, goalRank
        logical, intent(out) :: isValid

        character :: c1, c2, c3, c4
        integer :: i

        isValid = .false.
        currentFile = 0
        currentRank = 0
        goalFile = 0
        goalRank = 0

        if (len_trim(move) < 4) return

        c1 = move(1:1)
        c2 = move(2:2)
        c3 = move(3:3)
        c4 = move(4:4)

        if (c1 < 'a' .or. c1 > 'h') return
        if (c3 < 'a' .or. c3 > 'h') return
        if (c2 < '1' .or. c2 > '8') return
        if (c4 < '1' .or. c4 > '8') return

        currentFile = iachar(c1) - iachar('a') + 1
        goalFile    = iachar(c3) - iachar('a') + 1

        read(c2, '(I1)') currentRank
        read(c4, '(I1)') goalRank

        do i = 1, nMoves
            if (trim(legalMoves(i)) == trim(move)) then
                isValid = .true.
                exit
            end if
        end do
    end subroutine parseMoveAndValidate




    subroutine evalPos(board, posEval)
        character(len=1), intent(in) :: board(8,8)
        real, intent(inout) :: posEval
        integer :: f, r
        
        do f = 1,8
            do r = 1,8
                select case (board(f,r))
                    case ('P')
                        posEval = posEval + 1
                    case ('R')
                        posEval = posEval + 4
                    case ('B')
                        posEval = posEval + 3
                    case ('N')
                        posEval = posEval + 3
                    case ('Q')
                        posEval = posEval + 8
                    case ('p')
                        posEval = posEval - 1
                    case ('r')
                        posEval = posEval - 4
                    case ('b')
                        posEval = posEval - 3
                    case ('n')
                        posEval = posEval - 3
                    case ('q')
                        posEval = posEval - 8


                end select
                
            end do
        end do
    end  subroutine evalPos


    subroutine genPawnMovesW(gameBoard, fileP, rankP, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileP, rankP
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: nextRank, maxMoves
        character :: fileChar, rankChar, nextRankChar

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

    subroutine genKnightMovesW(gameBoard, fileN, rankN, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileN, rankN
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: maxMoves
        integer, parameter :: nOffsets = 8
        integer :: i, newFile, newRank
        integer, dimension(nOffsets) :: df = (/  1,  2,  2,  1, -1, -2, -2, -1 /)
        integer, dimension(nOffsets) :: dr = (/  2,  1, -1, -2, -2, -1,  1,  2 /)
        character :: fromFile, fromRank, toFile, toRank

        maxMoves = size(legalMoves)

        if (gameBoard(rankN, fileN) /= 'N') return

        fromFile = achar(iachar('a') + fileN - 1)
        fromRank = achar(iachar('0') + rankN)

        do i = 1, nOffsets
            newFile = fileN + df(i)
            newRank = rankN + dr(i)

            if (newFile >= 1 .and. newFile <= 8 .and. newRank >= 1 .and. newRank <= 8) then
                
                if (.not.(gameBoard(newRank, newFile) >= 'A' .and. gameBoard(newRank, newFile) <= 'Z')) then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        toFile = achar(iachar('a') + newFile - 1)
                        toRank = achar(iachar('0') + newRank)
                        legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                    end if
                end if
            end if
        end do
    end subroutine genKnightMovesW


    subroutine genKnightMovesB(gameBoard, fileN, rankN, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileN, rankN
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: maxMoves
        integer, parameter :: nOffsets = 8
        integer :: i, newFile, newRank
        integer, dimension(nOffsets) :: df = (/  1,  2,  2,  1, -1, -2, -2, -1 /)
        integer, dimension(nOffsets) :: dr = (/  2,  1, -1, -2, -2, -1,  1,  2 /)
        character :: fromFile, fromRank, toFile, toRank

        maxMoves = size(legalMoves)

        if (gameBoard(rankN, fileN) /= 'n') return

        fromFile = achar(iachar('a') + fileN - 1)
        fromRank = achar(iachar('0') + rankN)

        do i = 1, nOffsets
            newFile = fileN + df(i)
            newRank = rankN + dr(i)

            if (newFile >= 1 .and. newFile <= 8 .and. newRank >= 1 .and. newRank <= 8) then
                
                if (.not.(gameBoard(newRank, newFile) >= 'a' .and. gameBoard(newRank, newFile) <= 'z')) then
                    if (numMoves < maxMoves) then
                        numMoves = numMoves + 1
                        toFile = achar(iachar('a') + newFile - 1)
                        toRank = achar(iachar('0') + newRank)
                        legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                    end if
                end if
            end if
        end do
    end subroutine genKnightMovesB
    
    subroutine genRookMovesW(gameBoard, fileR, rankR, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileR, rankR
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)
        if (gameBoard(rankR, fileR) /= 'R') return

        fromFile = achar(iachar('a') + fileR - 1)
        fromRank = achar(iachar('0') + rankR)

        do df = -1, 1, 2
            newFile = fileR
            do
                newFile = newFile + df
                if (newFile < 1 .or. newFile > 8) exit
                if (gameBoard(rankR, newFile) >= 'A' .and. gameBoard(rankR, newFile) <= 'Z') exit
                numMoves = numMoves + 1
                toFile = achar(iachar('a') + newFile - 1)
                toRank = fromRank
                legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                if (gameBoard(rankR, newFile) >= 'a' .and. gameBoard(rankR, newFile) <= 'z') exit
            end do
        end do

        do dr = -1, 1, 2
            newRank = rankR
            do
                newRank = newRank + dr
                if (newRank < 1 .or. newRank > 8) exit
                if (gameBoard(newRank, fileR) >= 'A' .and. gameBoard(newRank, fileR) <= 'Z') exit
                numMoves = numMoves + 1
                toFile = fromFile
                toRank = achar(iachar('0') + newRank)
                legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                if (gameBoard(newRank, fileR) >= 'a' .and. gameBoard(newRank, fileR) <= 'z') exit
            end do
        end do
    end subroutine genRookMovesW

    subroutine genRookMovesB(gameBoard, fileR, rankR, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileR, rankR
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)
        if (gameBoard(rankR, fileR) /= 'r') return

        fromFile = achar(iachar('a') + fileR - 1)
        fromRank = achar(iachar('0') + rankR)

        do df = -1, 1, 2
            newFile = fileR
            do
                newFile = newFile + df
                if (newFile < 1 .or. newFile > 8) exit
                if (gameBoard(rankR, newFile) >= 'a' .and. gameBoard(rankR, newFile) <= 'z') exit
                numMoves = numMoves + 1
                toFile = achar(iachar('a') + newFile - 1)
                toRank = fromRank
                legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                if (gameBoard(rankR, newFile) >= 'A' .and. gameBoard(rankR, newFile) <= 'Z') exit
            end do
        end do

        do dr = -1, 1, 2
            newRank = rankR
            do
                newRank = newRank + dr
                if (newRank < 1 .or. newRank > 8) exit
                if (gameBoard(newRank, fileR) >= 'a' .and. gameBoard(newRank, fileR) <= 'z') exit
                numMoves = numMoves + 1
                toFile = fromFile
                toRank = achar(iachar('0') + newRank)
                legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                if (gameBoard(newRank, fileR) >= 'A' .and. gameBoard(newRank, fileR) <= 'Z') exit
            end do
        end do
    end subroutine genRookMovesB

    subroutine genBishopMovesW(gameBoard, fileB, rankB, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileB, rankB
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)
        if (gameBoard(rankB, fileB) /= 'B') return

        fromFile = achar(iachar('a') + fileB - 1)
        fromRank = achar(iachar('0') + rankB)

        do df = -1, 1, 2
            do dr = -1, 1, 2
                newFile = fileB
                newRank = rankB
                do
                    newFile = newFile + df
                    newRank = newRank + dr
                    if (newFile < 1 .or. newFile > 8 .or. newRank < 1 .or. newRank > 8) exit
                    if (gameBoard(newRank, newFile) >= 'A' .and. gameBoard(newRank, newFile) <= 'Z') exit
                    numMoves = numMoves + 1
                    toFile = achar(iachar('a') + newFile - 1)
                    toRank = achar(iachar('0') + newRank)
                    legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                    if (gameBoard(newRank, newFile) >= 'a' .and. gameBoard(newRank, newFile) <= 'z') exit
                end do
            end do
        end do
    end subroutine genBishopMovesW

    subroutine genBishopMovesB(gameBoard, fileB, rankB, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileB, rankB
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        integer :: df, dr, newFile, newRank
        character :: fromFile, fromRank, toFile, toRank
        integer :: maxMoves

        maxMoves = size(legalMoves)
        if (gameBoard(rankB, fileB) /= 'B') return

        fromFile = achar(iachar('a') + fileB - 1)
        fromRank = achar(iachar('0') + rankB)

        do df = -1, 1, 2
            do dr = -1, 1, 2
                newFile = fileB
                newRank = rankB
                do
                    newFile = newFile + df
                    newRank = newRank + dr
                    if (newFile < 1 .or. newFile > 8 .or. newRank < 1 .or. newRank > 8) exit
                    if (gameBoard(newRank, newFile) >= 'a' .and. gameBoard(newRank, newFile) <= 'z') exit
                    numMoves = numMoves + 1
                    toFile = achar(iachar('a') + newFile - 1)
                    toRank = achar(iachar('0') + newRank)
                    legalMoves(numMoves) = fromFile // fromRank // toFile // toRank
                    if (gameBoard(newRank, newFile) >= 'A' .and. gameBoard(newRank, newFile) <= 'Z') exit
                end do
            end do
        end do
    end subroutine genBishopMovesB
    subroutine genQueenMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileQ, rankQ
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves

        call genRookMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
        call genBishopMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    end subroutine genQueenMovesW

    subroutine genQueenMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: fileQ, rankQ
        character(len=5), intent(inout) :: legalMoves(:)
        integer, intent(inout) :: numMoves
        call genRookMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
        call genBishopMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    end subroutine genQueenMovesB


end program Engine