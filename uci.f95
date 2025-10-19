program FortranUCI
    use EngineModule
    implicit none

    character(len=512) :: line
    logical :: running
    integer :: p, depth
    character(len=5) :: bestMove

    call init_state()

    running = .true.
    do while (running)
        if (.not. read_line_nonempty(line)) cycle

        select case (first_token(line))
        case ("uci")
            call uci_id()
        case ("isready")
            call write_line("readyok")
        case ("ucinewgame")
            call init_state()
        case ("position")
            call handle_position(trim(line))
        case("d")
            call printBoard(board)
        case ("go")
            depth = parse_depth(line)
            if (depth <= 0) depth = 3
            call genAllMoves(board, playingPlayer, legalMoves, nMoves)
            if (nMoves <= 0) then
                call write_line("bestmove 0000")
            else
                call lookIntoFuture(board, legalMoves, nMoves, bestMove, playingPlayer, depth)
                call write_line("bestmove "//trim(bestMove))
            end if
        case ("quit")
            running = .false.
        case default
            
        end select
    end do
contains
    subroutine printBoard(board)
        implicit none
        character(len=1), intent(in) :: board(8,8)
        integer :: r, f

        do r = 8,1,-1
            write(*,'(I1, " |", 8(A2))') r, ( " " // board(r,f), f=1,8 )
        end do
        write(*,'("    a b c d e f g h")')
    end subroutine printBoard

    subroutine init_state()
        
        call initBoard(board)
        whiteCanCastleKingside  = .true.
        whiteCanCastleQueenside = .true.
        blackCanCastleKingside  = .true.
        blackCanCastleQueenside = .true.
        playingPlayer = 'White'
        nMoves = 0
        legalMoves = ''
    end subroutine init_state

    subroutine uci_id()
        call write_line("id name Formin-C")
        call write_line("id author Capinol")
        call write_line("uciok")
    end subroutine uci_id

    subroutine handle_position(cmd)
        character(len=*), intent(in) :: cmd
        integer :: posMoves, i, start, stop
        character(len=16) :: tok

       
        if (index(cmd, "startpos") > 0) then
            call init_state()
            posMoves = index(cmd, "moves")
            if (posMoves > 0) then
                
                start = posMoves + len("moves")
                do
                    call next_token(cmd, start, tok, stop)
                    if (len_trim(tok) == 0) exit
                    call apply_uci_move(trim(tok))
                    start = stop
                end do
            end if
        else
          ! TODO: FEN !!!!
        end if
    end subroutine handle_position

    subroutine apply_uci_move(m)
        
        character(len=*), intent(in) :: m
        integer :: cf, cr, gf, gr
        character(len=1) :: promoC, piece

        if (len_trim(m) < 4) return
        cf = iachar(m(1:1)) - iachar('a') + 1
        cr = iachar(m(2:2)) - iachar('0')
        gf = iachar(m(3:3)) - iachar('a') + 1
        gr = iachar(m(4:4)) - iachar('0')

        piece = board(cr, cf)
        promoC = ' '
        if (len_trim(m) >= 5) promoC = m(5:5)

        if (promoC /= ' ') then
            call makeMove(board, cf, cr, gf, gr, piece, promoC)
        else
            call makeMove(board, cf, cr, gf, gr, piece)
        end if

       
        if (playingPlayer == 'White') then
            playingPlayer = 'Black'
        else
            playingPlayer = 'White'
        end if
    end subroutine apply_uci_move

    function parse_depth(cmd) result(d)
        character(len=*), intent(in) :: cmd
        integer :: d, k, v
        d = 0
        k = index(cmd, "depth")
        if (k > 0) then
            read(cmd(k+len("depth"):), *, err=99) v
            d = v
        end if
99      continue
    end function parse_depth

    function first_token(s) result(tok)
        character(len=*), intent(in) :: s
        character(len=32) :: tok
        integer :: i, j, n
        tok = ""
        n = len_trim(s)
        if (n == 0) return
        i = 1
        do while (i <= n .and. s(i:i) == ' ')
            i = i + 1
        end do
        j = i
        do while (j <= n .and. s(j:j) /= ' ')
            j = j + 1
        end do
        if (j > i) tok = adjustl(s(i:j-1))
        tok = lower(tok)
    end function first_token

    subroutine next_token(s, start, tok, nextPos)
        character(len=*), intent(in) :: s
        integer,           intent(in) :: start
        character(len=*), intent(out) :: tok
        integer,           intent(out) :: nextPos
        integer :: n, i, j
        n = len_trim(s)
        i = start
        do while (i <= n .and. s(i:i) == ' ')
            i = i + 1
        end do
        j = i
        do while (j <= n .and. s(j:j) /= ' ')
            j = j + 1
        end do
        if (i <= n) then
            tok = s(i:j-1)
            nextPos = j
        else
            tok = ""
            nextPos = n+1
        end if
    end subroutine next_token

    logical function read_line_nonempty(out)
        character(len=*), intent(out) :: out
        integer :: ios
        read(*,'(A)', iostat=ios) out
        if (ios /= 0) then
            read_line_nonempty = .false.
        else
            read_line_nonempty = len_trim(out) > 0
        end if
    end function read_line_nonempty

    subroutine write_line(s)
        character(len=*), intent(in) :: s
        write(*,'(A)') trim(s)  
    end subroutine write_line

    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i, ic
        t = s
        do i = 1, len(s)
            ic = iachar(s(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) t(i:i) = achar(ic+32)
        end do
    end function lower
end program FortranUCI
