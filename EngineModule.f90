    module EngineModule
        implicit none

        character(len=1) :: board(8,8) 
        integer :: rank, file, i
        integer :: legalMoves(218)
        integer :: nMoves, showChoices, ios, showValidator, moveInputValidator
        integer :: playerSelectValidator
        character(len=5) :: selectedPlayer, playingPlayer, engineColor, winningPlayer
        real :: randHelper, posEval
        character(len=5) :: userMove, engineMove
        integer :: cf, cr, gf, gr
        logical :: valid, game
        character (len=5) :: osClear
        character (len=1024) :: osSeperator
        logical :: whiteCanCastleKingside, whiteCanCastleQueenside
        logical :: blackCanCastleKingside, blackCanCastleQueenside
        character(len=1) :: promo
        character(len=1) :: pieceToMove

        integer, parameter :: PROMO_NONE=0, PROMO_Q=1, PROMO_R=2, PROMO_B=3, PROMO_N=4
        

        
    contains

        function itoa(i) result(s)
            integer, intent(in) :: i
            character(len=2) :: s
            write(s,'(I1)') i
        end function itoa


    pure integer function sq(file, rank) result(s)
        integer, intent(in) :: file, rank      
        s = (rank-1)*8 + (file-1)              
    end function sq

    pure subroutine sq_to_fr(s, file, rank)
        integer, intent(in)  :: s
        integer, intent(out) :: file, rank
        rank = s / 8 + 1
        file = mod(s,8) + 1
    end subroutine sq_to_fr

    pure integer function promo_code_from_char(c) result(pc)
        character(len=1), intent(in) :: c
        select case(c)
        case('Q','q'); pc = PROMO_Q
        case('R','r'); pc = PROMO_R
        case('B','b'); pc = PROMO_B
        case('N','n'); pc = PROMO_N
        case default ; pc = PROMO_NONE
        end select
        end function promo_code_from_char

    pure character(len=1) function promo_char_from_code(pc, byWhite) result(c)
        integer, intent(in) :: pc
        logical, intent(in) :: byWhite
        select case(pc)
        case (PROMO_Q); c = merge('Q','q',byWhite)
        case (PROMO_R); c = merge('R','r',byWhite)
        case (PROMO_B); c = merge('B','b',byWhite)
        case (PROMO_N); c = merge('N','n',byWhite)
        case default   ; c = ' '
        end select
    end function promo_char_from_code

        pure integer function packMove(cf,cr,gf,gr, promoChar) result(m)
        integer, intent(in) :: cf,cr,gf,gr             
        character(len=1), intent(in), optional :: promoChar
        integer :: fromS, toS, pc
        fromS = sq(cf,cr); toS = sq(gf,gr)
        pc = PROMO_NONE
        if (present(promoChar)) pc = promo_code_from_char(promoChar)
        m = ior( ior( fromS, ishft(toS,6) ), ishft(pc,12) )
        end function packMove

    pure subroutine unpackMove(m, cf,cr,gf,gr, promoChar)
        integer, intent(in) :: m
        integer, intent(out) :: cf,cr,gf,gr
        character(len=1), intent(out) :: promoChar
        integer :: fromS, toS, pc
        fromS = iand(m,     z'3F')           
        toS   = iand(ishft(m,-6), z'3F')      
        pc    = iand(ishft(m,-12), z'7')      
        call sq_to_fr(fromS, cf, cr)
        call sq_to_fr(toS,   gf, gr)
        promoChar = promo_char_from_code(pc, .true.)  
    end subroutine unpackMove

        pure logical function sameMove(a,b) result(eq)
        integer, intent(in) :: a,b
        eq = (a == b)
        end function sameMove

        
        pure subroutine moveToUci(m, s)
        integer, intent(in) :: m
        character(len=*), intent(out) :: s
        integer :: cf,cr,gf,gr
        character(len=1) :: p
        call unpackMove(m, cf,cr,gf,gr, p)
        s = achar(iachar('a')+cf-1)//achar(iachar('0')+cr)// &
            achar(iachar('a')+gf-1)//achar(iachar('0')+gr)
        if (p /= ' ') s = trim(s)//p
        end subroutine moveToUci

        logical function parseUciMove(str, cf,cr,gf,gr, promoChar) result(ok)
        character(len=*), intent(in) :: str
        integer, intent(out) :: cf,cr,gf,gr
        character(len=1), intent(out) :: promoChar
        character :: c1,c2,c3,c4
        ok = .false.; promoChar = ' '
        if (len_trim(str) < 4) return
        c1 = str(1:1); c2 = str(2:2); c3 = str(3:3); c4 = str(4:4)
        if (c1<'a' .or. c1>'h') return
        if (c3<'a' .or. c3>'h') return
        if (c2<'1' .or. c2>'8') return
        if (c4<'1' .or. c4>'8') return
        cf = iachar(c1)-iachar('a')+1
        gf = iachar(c3)-iachar('a')+1
        read(c2,'(I1)') cr
        read(c4,'(I1)') gr
        if (len_trim(str) >= 5) promoChar = str(5:5)
        ok = .true.
        end function parseUciMove

        pure subroutine addMove(list, n, cf,cr,gf,gr, promo)
            integer, intent(inout) :: list(:)
            integer, intent(inout) :: n
            integer, intent(in)    :: cf,cr,gf,gr
            character(len=1), intent(in), optional :: promo
            integer :: m
            if (n < size(list)) then
                if (present(promo)) then
                m = packMove(cf,cr,gf,gr,promo)
                else
                m = packMove(cf,cr,gf,gr)
                end if
                n = n + 1
                list(n) = m
            end if
        end subroutine addMove


        subroutine initBoard(board)
            implicit none
            character(len=1), intent(out) :: board(8,8)
            board = ' '

            board(1,:) = ['R','N','B','Q','K','B','N','R']
            board(2,:) = ['P','P','P','P','P','P','P','P']

            board(8,:) = ['r','n','b','q','k','b','n','r']
            board(7,:) = ['p','p','p','p','p','p','p','p']

        end subroutine initBoard    
        subroutine makeMove(board, currentFile, currentRank, goalFile, goalRank, piece, promotionPiece)
            implicit none
            character(len=1), intent(inout) :: board(8,8)
            integer, intent(in) :: currentFile, currentRank, goalFile, goalRank
            character(len=1), intent(in) :: piece
            character(len=1), intent(in), optional :: promotionPiece

            call updateCastleRightsForMove(board, currentFile, currentRank, goalFile, goalRank, &
                piece, whiteCanCastleKingside, whiteCanCastleQueenside, &
                blackCanCastleKingside, blackCanCastleQueenside)

            if (piece == 'K' .and. currentRank == 1 .and. currentFile == 5) then
                if (goalFile == 7 .and. goalRank == 1) then
                    board(1,5) = ' '; board(1,7) = 'K'
                    board(1,8) = ' '; board(1,6) = 'R'
                    return
                else if (goalFile == 3 .and. goalRank == 1) then
                    board(1,5) = ' '; board(1,3) = 'K'
                    board(1,1) = ' '; board(1,4) = 'R'
                    return
                end if
            end if
            if (piece == 'k' .and. currentRank == 8 .and. currentFile == 5) then
                if (goalFile == 7 .and. goalRank == 8) then
                    board(8,5) = ' '; board(8,7) = 'k'
                    board(8,8) = ' '; board(8,6) = 'r'
                    return
                else if (goalFile == 3 .and. goalRank == 8) then
                    board(8,5) = ' '; board(8,3) = 'k'
                    board(8,1) = ' '; board(8,4) = 'r'
                    return
                end if
            end if


            if (present(promotionPiece)) then
                board(goalRank, goalFile) = promotionPiece
            else
                board(goalRank, goalFile) = piece
            end if
            board(currentRank, currentFile) = ' '
        end subroutine makeMove

        subroutine makeMoveSim(board, currentFile, currentRank, goalFile, goalRank, piece, promotionPiece)
            implicit none
            character(len=1), intent(inout) :: board(8,8)
            integer, intent(in) :: currentFile, currentRank, goalFile, goalRank
            character(len=1), intent(in) :: piece
            character(len=1), intent(in), optional :: promotionPiece

            if (piece == 'K' .and. currentRank == 1 .and. currentFile == 5) then
                if (goalFile == 7 .and. goalRank == 1) then
                    board(1,5) = ' '; board(1,7) = 'K'
                    board(1,8) = ' '; board(1,6) = 'R'
                    return
                else if (goalFile == 3 .and. goalRank == 1) then
                    board(1,5) = ' '; board(1,3) = 'K'
                    board(1,1) = ' '; board(1,4) = 'R'
                    return
                end if
            end if
            if (piece == 'k' .and. currentRank == 8 .and. currentFile == 5) then
                if (goalFile == 7 .and. goalRank == 8) then
                    board(8,5) = ' '; board(8,7) = 'k'
                    board(8,8) = ' '; board(8,6) = 'r'
                    return
                else if (goalFile == 3 .and. goalRank == 8) then
                    board(8,5) = ' '; board(8,3) = 'k'
                    board(8,1) = ' '; board(8,4) = 'r'
                    return
                end if
            end if

            if (present(promotionPiece)) then
                board(goalRank, goalFile) = promotionPiece
            else
                board(goalRank, goalFile) = piece
            end if
            board(currentRank, currentFile) = ' '
        end subroutine makeMoveSim

        subroutine updateCastleRightsForMove(board, cf, cr, gf, gr, piece, &
            wK, wQ, bK, bQ)
            implicit none
            character(len=1), intent(in) :: board(8,8)
            integer, intent(in) :: cf, cr, gf, gr
            character(len=1), intent(in) :: piece
            logical, intent(inout) :: wK, wQ, bK, bQ

            if (piece == 'K') then
                wK = .false.; wQ = .false.
            else if (piece == 'k') then
                bK = .false.; bQ = .false.
            end if

            if (piece == 'R') then
                if (cf == 1 .and. cr == 1) wQ = .false.
                if (cf == 8 .and. cr == 1) wK = .false.
            else if (piece == 'r') then
                if (cf == 1 .and. cr == 8) bQ = .false.
                if (cf == 8 .and. cr == 8) bK = .false.
            end if

            if (board(gr, gf) == 'R') then
                if (gf == 1 .and. gr == 1) wQ = .false.
                if (gf == 8 .and. gr == 1) wK = .false.
            else if (board(gr, gf) == 'r') then
                if (gf == 1 .and. gr == 8) bQ = .false.
                if (gf == 8 .and. gr == 8) bK = .false.
            end if
        end subroutine updateCastleRightsForMove

        subroutine pickRandomMove(legalMoves, nMoves, randomMove)
            integer, intent(in) :: legalMoves(:)
            integer, intent(in) :: nMoves
            integer, intent(out) :: randomMove
            real :: r
            integer :: index

            if (nMoves <= 0) then
                randomMove = 0
                return
            end if

            call random_number(r)
            index = int(r * nMoves) + 1
            if (index > nMoves) index = nMoves

            randomMove = legalMoves(index)
        end subroutine pickRandomMove

        subroutine parseMoveAndValidate(m, legalMoves, nMoves, cf, cr, gf, gr, isValid, promoChar)
            implicit none
            integer, intent(in) :: m
            integer, intent(in) :: legalMoves(:)
            integer, intent(in) :: nMoves
            integer, intent(out) :: cf, cr, gf, gr
            logical, intent(out) :: isValid
            character(len=1), intent(out), optional :: promoChar

            integer :: i, lm, fromF, fromR, toF, toR
            character(len=1) :: p

            isValid = .false.
            cf = 0; cr = 0; gf = 0; gr = 0
            if (present(promoChar)) promoChar = ' '

          
            do i=1,nMoves
                lm = legalMoves(i)
                if (lm == m) then
                    isValid = .true.
                    call unpackMove(m, cf,cr,gf,gr,p)
                    if (present(promoChar)) promoChar = p
                    return
                end if
            end do
        end subroutine parseMoveAndValidate

        subroutine isSquareAttacked(board, rk, fl, byColor, attacked)
            implicit none
            character(len=1), intent(in) :: board(8,8)
            integer, intent(in) :: rk, fl
            character(len=*), intent(in) :: byColor
            logical, intent(out) :: attacked

            integer :: r, f, nr, nf, i
            integer, dimension(8) :: kdr = (/  2,  1, -1, -2, -2, -1,  1,  2 /)
            integer, dimension(8) :: kdf = (/  1,  2,  2,  1, -1, -2, -2, -1 /)
            integer, dimension(4) :: rdr = (/ 1,  1, -1, -1 /)
            integer, dimension(4) :: rdf = (/ 1, -1, -1,  1 /)
            integer, dimension(4) :: rcr = (/ 1, -1,  0,  0 /)
            integer, dimension(4) :: rcf = (/ 0,  0,  1, -1 /)
            character(len=1) :: pawnC, knightC, bishopC, rookC, queenC, kingC

            attacked = .false.

            if (trim(byColor) == 'White') then
                pawnC   = 'P'
                knightC = 'N'
                bishopC = 'B'
                rookC   = 'R'
                queenC  = 'Q'
                kingC   = 'K'
                
                if (rk-1 >= 1) then
                    if (fl-1 >= 1) then
                        if (board(rk-1, fl-1) == pawnC) attacked = .true.
                    end if
                    if (.not. attacked .and. fl+1 <= 8) then
                        if (board(rk-1, fl+1) == pawnC) attacked = .true.
                    end if
                end if
            else
                pawnC   = 'p'
                knightC = 'n'
                bishopC = 'b'
                rookC   = 'r'
                queenC  = 'q'
                kingC   = 'k'
            
                if (rk+1 <= 8) then
                    if (fl-1 >= 1) then
                        if (board(rk+1, fl-1) == pawnC) attacked = .true.
                    end if
                    if (.not. attacked .and. fl+1 <= 8) then
                        if (board(rk+1, fl+1) == pawnC) attacked = .true.
                    end if
                end if
            end if

            if (attacked) return

            do i = 1, 8
                nr = rk + kdr(i)
                nf = fl + kdf(i)
                if (nr >= 1 .and. nr <= 8 .and. nf >= 1 .and. nf <= 8) then
                    if (board(nr, nf) == knightC) then
                        attacked = .true.
                        return
                    end if
                end if
            end do

            do r = -1, 1
                do f = -1, 1
                    if (r == 0 .and. f == 0) cycle
                    nr = rk + r
                    nf = fl + f
                    if (nr >= 1 .and. nr <= 8 .and. nf >= 1 .and. nf <= 8) then
                        if (board(nr, nf) == kingC) then
                            attacked = .true.
                            return
                        end if
                    end if
                end do
            end do

            do i = 1, 4
                nr = rk
                nf = fl
                do
                    nr = nr + rdr(i)
                    nf = nf + rdf(i)
                    if (nr < 1 .or. nr > 8 .or. nf < 1 .or. nf > 8) exit
                    if (board(nr, nf) == bishopC .or. board(nr, nf) == queenC) then
                        attacked = .true.
                        return
                    end if
                
                    if (board(nr, nf) /= ' ') exit
                end do
            end do

        
            do i = 1, 4
                nr = rk
                nf = fl
                do
                    nr = nr + rcr(i)
                    nf = nf + rcf(i)
                    if (nr < 1 .or. nr > 8 .or. nf < 1 .or. nf > 8) exit
                    if (board(nr, nf) == rookC .or. board(nr, nf) == queenC) then
                        attacked = .true.
                        return
                    end if
                    if (board(nr, nf) /= ' ') exit
                end do
            end do

        end subroutine isSquareAttacked

        subroutine isInCheck(board, color, inCheck)
            implicit none
            character(len=1), intent(in) :: board(8,8)
            character(len=*), intent(in) :: color
            logical, intent(out) :: inCheck
            integer :: r, f
            character(len=1) :: kingC

            inCheck = .false.
            if (trim(color) == 'White') then
                kingC = 'K'
            else
                kingC = 'k'
            end if

            do r = 1, 8
                do f = 1, 8
                    if (board(r,f) == kingC) then
                        call isSquareAttacked(board, r, f, merge('Black','White', trim(color) == 'White'), inCheck)
                        return
                    end if
                end do
            end do
            
            inCheck = .true.
        end subroutine isInCheck

       subroutine filterLegalMoves(gameBoard, legalMoves, nMoves, sideColor)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(inout) :: legalMoves(:)
        integer, intent(inout) :: nMoves
        character(len=*), intent(in) :: sideColor

        integer :: i, j, cf, cr, gf, gr, m
        character(len=1) :: promoC, piece
        character(len=1) :: tmpBoard(8,8)
        logical :: stillInCheck
        integer, allocatable :: keepMoves(:)

        if (nMoves <= 0) return

        allocate(keepMoves(nMoves))
        j = 0

        do i=1,nMoves
            m = legalMoves(i)
            call unpackMove(m, cf,cr,gf,gr,promoC)

            tmpBoard = gameBoard
            piece = tmpBoard(cr,cf)

            if (promoC /= ' ') then
                call makeMoveSim(tmpBoard, cf,cr,gf,gr,piece,promoC)
            else
                call makeMoveSim(tmpBoard, cf,cr,gf,gr,piece)
            end if

            call isInCheck(tmpBoard, sideColor, stillInCheck)
            if (.not. stillInCheck) then
                j = j + 1
                keepMoves(j) = m
            end if
        end do
        nMoves = j
        legalMoves(1:j) = keepMoves(1:j)
        if (j < size(legalMoves)) legalMoves(j+1:) = 0

        deallocate(keepMoves)
    end subroutine filterLegalMoves

       subroutine evalPos(board, posEval)
    character(len=1), intent(in) :: board(8,8)
    real, intent(out) :: posEval
    integer :: f, r

   

    real, parameter :: knightValuesW(8,8) = reshape( &
        [2.1, 2.3, 2.4, 2.4, 2.4, 2.4, 2.3, 2.1, &
         2.3, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 2.3, &
         2.4, 3.0, 3.2, 3.2, 3.2, 3.2, 3.0, 2.4, &
         2.4, 3.0, 3.2, 4.0, 4.0, 3.2, 3.0, 2.4, &
         2.4, 3.0, 3.2, 4.0, 4.0, 3.2, 3.0, 2.4, &
         2.4, 3.0, 3.2, 3.2, 3.2, 3.2, 3.0, 2.4, &
         2.3, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 2.3, &
         2.1, 2.3, 2.4, 2.4, 2.4, 2.4, 2.3, 2.1], [8,8] )
    posEval = 0.0

    do r = 1,8       
        do f = 1,8    
            select case (board(r,f))
                case ('P')
                    posEval = posEval + 1.0
                case ('R')
                    posEval = posEval + 4.0
                case ('B')
                    posEval = posEval + 3.0
                case ('N')
                    posEval = posEval + knightValuesW(r,f)
                case ('Q')
                    posEval = posEval + 8.0

                case ('p')
                    posEval = posEval - 1.0
                case ('r')
                    posEval = posEval - 4.0
                case ('b')
                    posEval = posEval - 3.0
                case ('n')
                    posEval = posEval - knightValuesW(9-r,f) 
                case ('q')
                    posEval = posEval - 8.0
            end select
        end do
    end do
end subroutine evalPos

    recursive function negamax(gameBoard, sideColor, legalMoves, nMoves, depth, alpha, beta, outBestMove) result(score)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        character(len=*), intent(in) :: sideColor
        integer, intent(in) :: legalMoves(:)   
        integer, intent(in) :: nMoves, depth
        integer, intent(out) :: outBestMove    
        real, intent(in) :: alpha, beta

        real :: score
        real, parameter :: MATE_SCORE = 1.0e6
        real, parameter :: STALEMATE_SCORE = 0.0
        logical :: inCheck
        character(len=1) :: tmpBoard(8,8)
        integer :: childMoves(218), childN
        integer :: i, cf, cr, gf, gr
        character(len=1) :: promo
        character(len=1) :: pieceToMove
        real :: bestScore, curScore
        integer :: candidateMove, dummyMove
        character(len=5) :: oppositeColor
        logical :: wK_old, wQ_old, bK_old, bQ_old
        logical :: wK_new, wQ_new, bK_new, bQ_new
        real :: a, b

        outBestMove = 0
        score = 0.0
        a = alpha
        b = beta

        if (trim(sideColor) == 'White') then
            oppositeColor = 'Black'
        else
            oppositeColor = 'White'
        end if

        call isInCheck(gameBoard, sideColor, inCheck)
        if (nMoves == 0) then
            if (inCheck) then
                score = -MATE_SCORE + depth
            else
                score = STALEMATE_SCORE
            end if
            return
        end if

        if (depth <= 0) then
            call evalPos(gameBoard, score)
            if (trim(sideColor) == "Black") score = -score
            return
        end if

        bestScore = -1.0e30

        do i = 1, nMoves
            candidateMove = legalMoves(i)
            if (candidateMove == 0) cycle

            call unpackMove(candidateMove, cf, cr, gf, gr, promo)
            tmpBoard = gameBoard
            pieceToMove = tmpBoard(cr, cf)

            if (promo /= ' ') then
                call makeMoveSim(tmpBoard, cf, cr, gf, gr, pieceToMove, promo)
            else
                call makeMoveSim(tmpBoard, cf, cr, gf, gr, pieceToMove)
            end if

            
            wK_old = whiteCanCastleKingside
            wQ_old = whiteCanCastleQueenside
            bK_old = blackCanCastleKingside
            bQ_old = blackCanCastleQueenside

            wK_new = wK_old; wQ_new = wQ_old
            bK_new = bK_old; bQ_new = bQ_old
            call updateCastleRightsForMove(gameBoard, cf, cr, gf, gr, pieceToMove, &
                wK_new, wQ_new, bK_new, bQ_new)

            whiteCanCastleKingside = wK_new
            whiteCanCastleQueenside = wQ_new
            blackCanCastleKingside = bK_new
            blackCanCastleQueenside = bQ_new

            childMoves = 0
            childN = 0
            call genAllMoves(tmpBoard, oppositeColor, childMoves, childN)

            curScore = -negamax(tmpBoard, oppositeColor, childMoves, childN, depth-1, -b, -a, dummyMove)

            
            whiteCanCastleKingside = wK_old
            whiteCanCastleQueenside = wQ_old
            blackCanCastleKingside = bK_old
            blackCanCastleQueenside = bQ_old

            if (curScore > bestScore) then
                bestScore = curScore
                outBestMove = candidateMove
            end if

            if (bestScore > a) a = bestScore
            if (a >= b) exit
        end do

        score = bestScore
    end function negamax
    recursive subroutine lookIntoFuture(gameBoard, legalMoves, nMoves, engineMove, engineColor, depth)
        implicit none
        character(len=1), intent(in) :: gameBoard(8,8)
        integer, intent(in) :: legalMoves(:)   
        integer, intent(in) :: nMoves
        integer, intent(out) :: engineMove      
        character(len=5), intent(in) :: engineColor
        integer, intent(in) :: depth

        real :: score
        integer :: bestMoveLocal

        engineMove = 0
        bestMoveLocal = 0
        score = 0.0

        if (nMoves <= 0) return

        score = negamax(gameBoard, trim(engineColor), legalMoves, nMoves, depth, -1.0e30, 1.0e30, bestMoveLocal)
        engineMove = bestMoveLocal
    end subroutine lookIntoFuture

        subroutine genAllMoves(gameBoard, sideColor, outMoves, outN)
            implicit none
            character(len=1), intent(in) :: gameBoard(8,8)
            character(len=*), intent(in) :: sideColor
            integer, intent(out) :: outMoves(:)
            integer, intent(out) :: outN

            integer :: file, rank
            integer :: maxMoves

            maxMoves = size(outMoves)
            outN = 0
            outMoves = 0

            do file = 1,8
                do rank = 1,8
                    if (trim(sideColor) == 'White') then
                        call genPawnMovesW(gameBoard, file, rank, outMoves, outN)
                        call genKingMovesW(gameBoard, file, rank, outMoves, outN)
                        call genKnightMovesW(gameBoard, file, rank, outMoves, outN)
                        call genRookMovesW(gameBoard, file, rank, outMoves, outN)
                        call genBishopMovesW(gameBoard, file, rank, outMoves, outN)
                        call genQueenMovesW(gameBoard, file, rank, outMoves, outN)
                    else
                        call genPawnMovesB(gameBoard, file, rank, outMoves, outN)
                        call genKingMovesB(gameBoard, file, rank, outMoves, outN)
                        call genKnightMovesB(gameBoard, file, rank, outMoves, outN)
                        call genRookMovesB(gameBoard, file, rank, outMoves, outN)
                        call genBishopMovesB(gameBoard, file, rank, outMoves, outN)
                        call genQueenMovesB(gameBoard, file, rank, outMoves, outN)
                    end if
                end do
            end do
            
            call filterLegalMoves(gameBoard, outMoves, outN, sideColor)
        end subroutine genAllMoves


        subroutine genCastlingMoves(board, sideColor, canCastleK, canCastleQ, legalMoves, nMoves)
            implicit none
            character(len=1), intent(in) :: board(8,8)
            character(len=*), intent(in) :: sideColor
            logical, intent(in) :: canCastleK, canCastleQ
            integer, intent(inout) :: legalMoves(:)
            integer, intent(inout) :: nMoves
            logical :: attacked

            if (trim(sideColor) == "White") then
               
                if (canCastleK .and. board(1,8) == 'R') then
                    if (board(1,6) == ' ' .and. board(1,7) == ' ') then
                        call isSquareAttacked(board,1,5,"Black",attacked)
                        if (.not. attacked) then
                            call isSquareAttacked(board,1,6,"Black",attacked)
                            if (.not. attacked) then
                                call isSquareAttacked(board,1,7,"Black",attacked)
                                if (.not. attacked) then
                                    call addMove(legalMoves, nMoves, 5,1, 7,1)
                                end if
                            end if
                        end if
                    end if
                end if
                
                if (canCastleQ .and. board(1,1) == 'R') then
                    if (board(1,2) == ' ' .and. board(1,3) == ' ' .and. board(1,4) == ' ') then
                        call isSquareAttacked(board,1,5,"Black",attacked)
                        if (.not. attacked) then
                            call isSquareAttacked(board,1,4,"Black",attacked)
                            if (.not. attacked) then
                                call isSquareAttacked(board,1,3,"Black",attacked)
                                if (.not. attacked) then
                                    call addMove(legalMoves, nMoves, 5,1, 3,1)
                                end if
                            end if
                        end if
                    end if
                end if
            else
                
                if (canCastleK .and. board(8,8) == 'r') then
                    if (board(8,6) == ' ' .and. board(8,7) == ' ') then
                        call isSquareAttacked(board,8,5,"White",attacked)
                        if (.not. attacked) then
                            call isSquareAttacked(board,8,6,"White",attacked)
                            if (.not. attacked) then
                                call isSquareAttacked(board,8,7,"White",attacked)
                                if (.not. attacked) then
                                    call addMove(legalMoves, nMoves, 5,8, 7,8)
                                end if
                            end if
                        end if
                    end if
                end if
                
                if (canCastleQ .and. board(8,1) == 'r') then
                    if (board(8,2) == ' ' .and. board(8,3) == ' ' .and. board(8,4) == ' ') then
                        call isSquareAttacked(board,8,5,"White",attacked)
                        if (.not. attacked) then
                            call isSquareAttacked(board,8,4,"White",attacked)
                            if (.not. attacked) then
                                call isSquareAttacked(board,8,3,"White",attacked)
                                if (.not. attacked) then
                                    call addMove(legalMoves, nMoves, 5,8, 3,8)
                                end if
                            end if
                        end if
                    end if
                end if
            end if
        end subroutine genCastlingMoves

        subroutine genPawnMovesW(gameBoard, fileP, rankP, legalMoves, numMoves)
            implicit none
            character(len=1), intent(in) :: gameBoard(8,8)
            integer, intent(in) :: fileP, rankP
            integer, intent(inout) :: legalMoves(:)  
            integer, intent(inout) :: numMoves
            integer :: nextRank, maxMoves, pi
            character(len=1), dimension(4) :: promos

            promos = (/'Q','R','B','N'/)
            maxMoves = size(legalMoves)

            if (gameBoard(rankP, fileP) /= 'P') return

            nextRank = rankP + 1

           
            if (nextRank == 8) then
                if (fileP > 1 .and. gameBoard(nextRank, fileP-1) >= 'a' .and. gameBoard(nextRank, fileP-1) <= 'z') then
                    do pi = 1, 4
                        call addMove(legalMoves, numMoves, fileP, rankP, fileP-1, nextRank, promos(pi))
                    end do
                end if
                if (fileP < 8 .and. gameBoard(nextRank, fileP+1) >= 'a' .and. gameBoard(nextRank, fileP+1) <= 'z') then
                    do pi = 1, 4
                        call addMove(legalMoves, numMoves, fileP, rankP, fileP+1, nextRank, promos(pi))
                    end do
                end if
             
                if (gameBoard(nextRank, fileP) == ' ') then
                    do pi = 1, 4
                        call addMove(legalMoves, numMoves, fileP, rankP, fileP, nextRank, promos(pi))
                    end do
                end if
            else
               
                if (fileP > 1 .and. gameBoard(nextRank, fileP-1) >= 'a' .and. gameBoard(nextRank, fileP-1) <= 'z') then
                    call addMove(legalMoves, numMoves, fileP, rankP, fileP-1, nextRank)
                end if
                if (fileP < 8 .and. gameBoard(nextRank, fileP+1) >= 'a' .and. gameBoard(nextRank, fileP+1) <= 'z') then
                    call addMove(legalMoves, numMoves, fileP, rankP, fileP+1, nextRank)
                end if
                
                if (gameBoard(nextRank, fileP) == ' ') then
                    call addMove(legalMoves, numMoves, fileP, rankP, fileP, nextRank)
                end if
               
                if (rankP == 2 .and. gameBoard(3,fileP) == ' ' .and. gameBoard(4,fileP) == ' ') then
                    call addMove(legalMoves, numMoves, fileP, rankP, fileP, 4)
                end if
            end if
        end subroutine genPawnMovesW


        subroutine genPawnMovesB(gameBoard, fileP, rankP, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileP, rankP
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: nextRank, pi
    character(len=1), dimension(4) :: promos

    promos = (/'q','r','b','n'/)

    
    if (gameBoard(rankP,fileP) /= 'p') return

    nextRank = rankP - 1   

  
    if (nextRank == 1) then
        
        if (fileP > 1 .and. gameBoard(nextRank,fileP-1) >= 'A' .and. gameBoard(nextRank,fileP-1) <= 'Z') then
            do pi=1,4
                call addMove(legalMoves, numMoves, fileP,rankP,fileP-1,nextRank,promos(pi))
            end do
        end if
        if (fileP < 8 .and. gameBoard(nextRank,fileP+1) >= 'A' .and. gameBoard(nextRank,fileP+1) <= 'Z') then
            do pi=1,4
                call addMove(legalMoves, numMoves, fileP,rankP,fileP+1,nextRank,promos(pi))
            end do
        end if
        
        if (gameBoard(nextRank,fileP) == ' ') then
            do pi=1,4
                call addMove(legalMoves, numMoves, fileP,rankP,fileP,nextRank,promos(pi))
            end do
        end if

    else
      
        if (fileP > 1 .and. gameBoard(nextRank,fileP-1) >= 'A' .and. gameBoard(nextRank,fileP-1) <= 'Z') then
            call addMove(legalMoves, numMoves, fileP,rankP,fileP-1,nextRank)
        end if
       
        if (fileP < 8 .and. gameBoard(nextRank,fileP+1) >= 'A' .and. gameBoard(nextRank,fileP+1) <= 'Z') then
            call addMove(legalMoves, numMoves, fileP,rankP,fileP+1,nextRank)
        end if
     
        if (gameBoard(nextRank,fileP) == ' ') then
            call addMove(legalMoves, numMoves, fileP,rankP,fileP,nextRank)
        end if

        if (rankP == 7 .and. gameBoard(6,fileP)==' ' .and. gameBoard(5,fileP)==' ') then
            call addMove(legalMoves, numMoves, fileP,rankP,fileP,5)
        end if
    end if
end subroutine genPawnMovesB

        subroutine genKingMovesW(gameBoard, fileK, rankK, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileK, rankK
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankK, fileK) /= 'K') return

    
    if (rankK == 1 .and. fileK == 5) then
        call genCastlingMoves(gameBoard, "White", whiteCanCastleKingside, whiteCanCastleQueenside, legalMoves, numMoves)
    end if

    do df = -1, 1
        do dr = -1, 1
            if (df == 0 .and. dr == 0) cycle
            newFile = fileK + df
            newRank = rankK + dr
            if (newFile>=1 .and. newFile<=8 .and. newRank>=1 .and. newRank<=8) then
                if (.not.(gameBoard(newRank,newFile)>='A' .and. gameBoard(newRank,newFile)<='Z')) then
                    call addMove(legalMoves, numMoves, fileK,rankK,newFile,newRank)
                end if
            end if
        end do
    end do
end subroutine genKingMovesW

subroutine genKingMovesB(gameBoard, fileK, rankK, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileK, rankK
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankK, fileK) /= 'k') return

    
    if (rankK == 8 .and. fileK == 5) then
        call genCastlingMoves(gameBoard, "Black", blackCanCastleKingside, blackCanCastleQueenside, legalMoves, numMoves)
    end if

    do df = -1, 1
        do dr = -1, 1
            if (df == 0 .and. dr == 0) cycle
            newFile = fileK + df
            newRank = rankK + dr
            if (newFile>=1 .and. newFile<=8 .and. newRank>=1 .and. newRank<=8) then
                if (.not.(gameBoard(newRank,newFile)>='a' .and. gameBoard(newRank,newFile)<='z')) then
                    call addMove(legalMoves, numMoves, fileK,rankK,newFile,newRank)
                end if
            end if
        end do
    end do
end subroutine genKingMovesB
subroutine genKnightMovesW(gameBoard, fileN, rankN, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileN, rankN
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer, dimension(8) :: df = (/  1,  2,  2,  1, -1, -2, -2, -1 /)
    integer, dimension(8) :: dr = (/  2,  1, -1, -2, -2, -1,  1,  2 /)
    integer :: i, newFile, newRank

    if (gameBoard(rankN,fileN) /= 'N') return
    do i=1,8
        newFile = fileN + df(i)
        newRank = rankN + dr(i)
        if (newFile>=1 .and. newFile<=8 .and. newRank>=1 .and. newRank<=8) then
            if (.not.(gameBoard(newRank,newFile)>='A' .and. gameBoard(newRank,newFile)<='Z')) then
                call addMove(legalMoves, numMoves, fileN,rankN,newFile,newRank)
            end if
        end if
    end do
end subroutine genKnightMovesW

subroutine genKnightMovesB(gameBoard, fileN, rankN, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileN, rankN
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer, dimension(8) :: df = (/  1,  2,  2,  1, -1, -2, -2, -1 /)
    integer, dimension(8) :: dr = (/  2,  1, -1, -2, -2, -1,  1,  2 /)
    integer :: i, newFile, newRank

    if (gameBoard(rankN,fileN) /= 'n') return
    do i=1,8
        newFile = fileN + df(i)
        newRank = rankN + dr(i)
        if (newFile>=1 .and. newFile<=8 .and. newRank>=1 .and. newRank<=8) then
            if (.not.(gameBoard(newRank,newFile)>='a' .and. gameBoard(newRank,newFile)<='z')) then
                call addMove(legalMoves, numMoves, fileN,rankN,newFile,newRank)
            end if
        end if
    end do
end subroutine genKnightMovesB
subroutine genRookMovesW(gameBoard, fileR, rankR, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileR, rankR
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankR,fileR) /= 'R') return
    do df=-1,1,2
        newFile=fileR
        do
            newFile=newFile+df
            if (newFile<1 .or. newFile>8) exit
            if (gameBoard(rankR,newFile)>='A' .and. gameBoard(rankR,newFile)<='Z') exit
            call addMove(legalMoves, numMoves, fileR,rankR,newFile,rankR)
            if (gameBoard(rankR,newFile)/=' ') exit
        end do
    end do
    do dr=-1,1,2
        newRank=rankR
        do
            newRank=newRank+dr
            if (newRank<1 .or. newRank>8) exit
            if (gameBoard(newRank,fileR)>='A' .and. gameBoard(newRank,fileR)<='Z') exit
            call addMove(legalMoves, numMoves, fileR,rankR,fileR,newRank)
            if (gameBoard(newRank,fileR)/=' ') exit
        end do
    end do
end subroutine genRookMovesW
subroutine genRookMovesB(gameBoard, fileR, rankR, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileR, rankR
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankR,fileR) /= 'r') return
    do df=-1,1,2
        newFile=fileR
        do
            newFile=newFile+df
            if (newFile<1 .or. newFile>8) exit
            if (gameBoard(rankR,newFile)>='a' .and. gameBoard(rankR,newFile)<='z') exit
            call addMove(legalMoves, numMoves, fileR,rankR,newFile,rankR)
            if (gameBoard(rankR,newFile)/=' ') exit
        end do
    end do
    do dr=-1,1,2
        newRank=rankR
        do
            newRank=newRank+dr
            if (newRank<1 .or. newRank>8) exit
            if (gameBoard(newRank,fileR)>='a' .and. gameBoard(newRank,fileR)<='z') exit
            call addMove(legalMoves, numMoves, fileR,rankR,fileR,newRank)
            if (gameBoard(newRank,fileR)/=' ') exit
        end do
    end do
end subroutine genRookMovesB
subroutine genBishopMovesW(gameBoard, fileB, rankB, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileB, rankB
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankB,fileB) /= 'B') return
    do df=-1,1,2
        do dr=-1,1,2
            newFile=fileB
            newRank=rankB
            do
                newFile=newFile+df
                newRank=newRank+dr
                if (newFile<1 .or. newFile>8 .or. newRank<1 .or. newRank>8) exit
                if (gameBoard(newRank,newFile)>='A' .and. gameBoard(newRank,newFile)<='Z') exit
                call addMove(legalMoves, numMoves, fileB,rankB,newFile,newRank)
                if (gameBoard(newRank,newFile)/=' ') exit
            end do
        end do
    end do
end subroutine genBishopMovesW

subroutine genBishopMovesB(gameBoard, fileB, rankB, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileB, rankB
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves
    integer :: df, dr, newFile, newRank

    if (gameBoard(rankB,fileB) /= 'b') return
    do df=-1,1,2
        do dr=-1,1,2
            newFile=fileB
            newRank=rankB
            do
                newFile=newFile+df
                newRank=newRank+dr
                if (newFile<1 .or. newFile>8 .or. newRank<1 .or. newRank>8) exit
                if (gameBoard(newRank,newFile)>='a' .and. gameBoard(newRank,newFile)<='z') exit
                call addMove(legalMoves, numMoves, fileB,rankB,newFile,newRank)
                if (gameBoard(newRank,newFile)/=' ') exit
            end do
        end do
    end do
end subroutine genBishopMovesB


subroutine genQueenMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileQ, rankQ
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves

    if (gameBoard(rankQ,fileQ) /= 'Q') return
    call genRookMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    call genBishopMovesW(gameBoard, fileQ, rankQ, legalMoves, numMoves)
end subroutine genQueenMovesW

subroutine genQueenMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    implicit none
    character(len=1), intent(in) :: gameBoard(8,8)
    integer, intent(in) :: fileQ, rankQ
    integer, intent(inout) :: legalMoves(:)
    integer, intent(inout) :: numMoves

    if (gameBoard(rankQ,fileQ) /= 'q') return
    call genRookMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
    call genBishopMovesB(gameBoard, fileQ, rankQ, legalMoves, numMoves)
end subroutine genQueenMovesB


    end module EngineModule