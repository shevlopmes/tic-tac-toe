
win_positions([0,1,2]).
win_positions([3,4,5]).
win_positions([6,7,8]).
win_positions([0,3,6]).
win_positions([1,4,7]).
win_positions([2,5,8]).
win_positions([0,4,8]).
win_positions([2,4,6]).

won(Grid, Player) :-
    win_positions([A,B,C]),
    nth0(A, Grid, Player),
    nth0(B, Grid, Player),
    nth0(C, Grid, Player).

full(Grid) :- \+ member('_', Grid).

replace(0, Value, [_|Tail], [Value|Tail]).
replace(Ind, Value, [Head|Tail], [Head|Res]) :-
    Ind > 0,
    Ind1 is Ind - 1,
    replace(Ind1, Value, Tail, Res).

opposite_player('x', 'o').
opposite_player('o', 'x').

empty_grid(['_', '_', '_', '_', '_', '_', '_', '_', '_']).
initial_boards([G,G,G,G,G,G,G,G,G]) :- empty_grid(G).
ongoing('ongoing').
initial_status([X,X,X,X,X,X,X,X,X]) :- ongoing(X).

is_legal(state(_, Boards, Statuses, NextBoard), move(BoardIndex, Pos)) :-
    nth0(BoardIndex, Boards, Board),
    nth0(Pos, Board, '_'),
    nth0(BoardIndex, Statuses, 'ongoing'),
    (NextBoard = 'any'; NextBoard = BoardIndex).

do_move(state(Player, Boards, Statuses, NextBoard), move(BoardIndex, Pos),
        state(NewPlayer, NewBoards, NewStatuses, NewNextBoard)) :-
    is_legal(state(Player, Boards, Statuses, NextBoard), move(BoardIndex, Pos)),
    nth0(BoardIndex, Boards, OldBoard),
    replace(Pos, Player, OldBoard, NewBoard),
    replace(BoardIndex, NewBoard, Boards, NewBoards),
    (won(NewBoard, Player) -> replace(BoardIndex, Player, Statuses, NewStatuses);
     full(NewBoard) -> replace(BoardIndex, 'draw', Statuses, NewStatuses);
     NewStatuses = Statuses),
    (nth0(Pos, NewStatuses, 'ongoing') -> NewNextBoard = Pos;
     NewNextBoard = 'any'),
    opposite_player(Player, NewPlayer).

game_over(state(_, _, Statuses, _), Player) :-
    member(Player, ['x', 'o']),
    opposite_player(Player, Opponent),
    findall(I, (between(0, 8, I), nth0(I, Statuses, Player)), PlayerBoards),
    findall(I, (between(0, 8, I), nth0(I, Statuses, Opponent)), OpponentBoards),
    length(PlayerBoards, PlayerCount),
    length(OpponentBoards, OpponentCount),
    PlayerCount > OpponentCount,
    \+ member('ongoing', Statuses).

game_over(state(_, _, Statuses, _), 'draw') :-
    \+ member('ongoing', Statuses),
    findall(I, (between(0, 8, I), nth0(I, Statuses, 'x')), XBoards),
    findall(I, (between(0, 8, I), nth0(I, Statuses, 'o')), OBoards),
    length(XBoards, XCount),
    length(OBoards, OCount),
    XCount =:= OCount.

count_won_boards(Statuses, Player, Count) :-
    findall(I, (between(0, 8, I), nth0(I, Statuses, Player)), WonBoards),
    length(WonBoards, Count).

initial_state(state('x', Boards, Statuses, 'any')) :-
    initial_boards(Boards),
    initial_status(Statuses).

display_all_boards(Boards) :-
    write('Coordinates are:'), nl,
    write('[0 1 2]'), nl,
    write('[3 4 5]'), nl,
    write('[6 7 8]'), nl,
    display_boards_row(Boards, 0),
    display_boards_row(Boards, 3),
    display_boards_row(Boards, 6).

display_boards_row(Boards, StartIndex) :-
    Row1 is StartIndex, Row2 is StartIndex + 1, Row3 is StartIndex + 2,
    nth0(Row1, Boards, B1), nth0(Row2, Boards, B2), nth0(Row3, Boards, B3),
    display_board_line(B1, B2, B3, 0),
    display_board_line(B1, B2, B3, 3),
    display_board_line(B1, B2, B3, 6),
    nl.

display_board_line(B1, B2, B3, Offset) :-
    I1 is Offset, I2 is Offset + 1, I3 is Offset + 2,
    nth0(I1, B1, C1), nth0(I2, B1, C2), nth0(I3, B1, C3),
    nth0(I1, B2, C4), nth0(I2, B2, C5), nth0(I3, B2, C6),
    nth0(I1, B3, C7), nth0(I2, B3, C8), nth0(I3, B3, C9),
    format('  ~w ~w ~w   ~w ~w ~w   ~w ~w ~w',
          [C1, C2, C3, C4, C5, C6, C7, C8, C9]), nl.

display_state(state(Player, Boards, Statuses, NextBoard)) :-
    nl, write('Current player: '), write(Player), nl,
    write('Next board: '), write(NextBoard), nl, nl,
    display_all_boards(Boards),
    nl, write('Board statuses: '), write(Statuses), nl.

read_move(move(BoardIndex, Pos)) :-
    write('Enter move as move(BoardIndex,Pos). Example: move(0,4).'), nl,
    read(Move),
    (   Move = move(BoardIndex, Pos) ->  true;
     write('Invalid input format.'), nl,
        read_move(move(BoardIndex, Pos))).

play(state(Player, Boards, Statuses, NextBoard)) :-
    display_state(state(Player, Boards, Statuses, NextBoard)),
    count_won_boards(Statuses, 'x', WonX),
    count_won_boards(Statuses, 'o', WonO),
    write('Won Boards - X: '), write(WonX), write(', O: '), write(WonO), nl,
    evaluate_game_state(state(Player, Boards, Statuses, NextBoard), Player, Eval),
    write('Evaluation for '), write(Player), write(': '), write(Eval), nl,
    (   game_over(state(Player, Boards, Statuses, NextBoard), Winner)
    ->  nl, write('Game over! Winner: '), write(Winner), nl;
       read_move(Move),
        (   is_legal(state(Player, Boards, Statuses, NextBoard), Move)
        -> do_move(state(Player, Boards, Statuses, NextBoard), Move, NewState),
            play(NewState);
            write('Illegal move. Try again.'), nl, play(state(Player, Boards, Statuses, NextBoard))
        )
    ).

play_ai(state(Player, Boards, Statuses, NextBoard), Depth) :-
    display_state(state(Player, Boards, Statuses, NextBoard)),
    count_won_boards(Statuses, 'x', WonX),
    count_won_boards(Statuses, 'o', WonO),
    write('Won Boards - X: '), write(WonX), write(', O: '), write(WonO), nl,
    evaluate_game_state(state(Player, Boards, Statuses, NextBoard), Player, Eval),
    write('Evaluation for '), write(Player), write(': '), write(Eval), nl,
    (   game_over(state(Player, Boards, Statuses, NextBoard), Winner)
    ->  nl, write('Game over! Winner: '), write(Winner), nl;
      (   Player = 'x'
        ->  read_move(Move),
            (   is_legal(state(Player, Boards, Statuses, NextBoard), Move)
            ->  do_move(state(Player, Boards, Statuses, NextBoard), Move, NewState),
                play_ai(NewState, Depth);
                write('Illegal move. Try again.'), nl,
                play_ai(state(Player, Boards, Statuses, NextBoard), Depth));
            Player = 'o',
            minimax(state(Player, Boards, Statuses, NextBoard), Depth, Move, _),
            write('AI plays: '), write(Move), nl,
            do_move(state(Player, Boards, Statuses, NextBoard), Move, NewState),
            play_ai(NewState, Depth)
        )
    ).

minimax(State, Depth, BestMove, Value) :-
    Depth > 0,
    all_possible_moves(State, Moves),
    Moves \= [],
    evaluate_moves(State, Depth, Moves, BestMove, Value).

evaluate_moves(State, Depth, [Move], Move, Value) :-
    do_move(State, Move, NewState),
    NextDepth is Depth - 1,
    minimax_value(NewState, NextDepth, Value).

evaluate_moves(State, Depth, [Move|Rest], BestMove, BestValue) :-
    do_move(State, Move, NewState),
    NextDepth is Depth - 1,
    minimax_value(NewState, NextDepth, Value),
    evaluate_moves(State, Depth, Rest, RestMove, RestValue),
    (   State = state('o', _, _, _)
    ->  (Value > RestValue -> BestMove = Move, BestValue = Value ; BestMove = RestMove, BestValue = RestValue)
    ;   (Value < RestValue -> BestMove = Move, BestValue = Value ; BestMove = RestMove, BestValue = RestValue)  
    ).

minimax_value(State, 0, Value) :- evaluate_game_state(State, 'o', Value).
minimax_value(State, _, Value) :- game_over(State, _), evaluate_game_state(State, 'o', Value).
minimax_value(State, Depth, Value) :-
    Depth > 0,
    all_possible_moves(State, Moves),
    Moves \= [],
    evaluate_moves(State, Depth, Moves, _, Value).

evaluate_game_state(state(_, Boards, Statuses, _), Player, Score) :-
    opposite_player(Player, Opponent),
    findall(100000, (between(0, 8, I), nth0(I, Statuses, Player)), WonPoints),
    findall(-100000, (between(0, 8, I), nth0(I, Statuses, Opponent)), LostPoints),
    sum_list(WonPoints, TotalWon),
    sum_list(LostPoints, TotalLost),
    findall(10, (between(0, 8, I), nth0(I, Boards, Board), nth0(I, Statuses, 'ongoing'), two_and_empty(Board, Player)), TwoPlayer),
    findall(-10, (between(0, 8, I), nth0(I, Boards, Board), nth0(I, Statuses, 'ongoing'), two_and_empty(Board, Opponent)), TwoOpponent),
    sum_list(TwoPlayer, TotalTwoPlayer),
    sum_list(TwoOpponent, TotalTwoOpponent),
    findall(1, (between(0, 8, I), nth0(I, Boards, Board), nth0(I, Statuses, 'ongoing'), one_and_empty(Board, Player)), OnePlayer),
    findall(-1, (between(0, 8, I), nth0(I, Boards, Board), nth0(I, Statuses, 'ongoing'), one_and_empty(Board, Opponent)), OneOpponent),
    sum_list(OnePlayer, TotalOnePlayer),
    sum_list(OneOpponent, TotalOneOpponent),
    Score is TotalWon + TotalLost + TotalTwoPlayer + TotalTwoOpponent + TotalOnePlayer + TotalOneOpponent.

two_and_empty(Board, Player) :-
    win_positions([A, B, C]),
    (   (nth0(A, Board, Player), nth0(B, Board, Player), nth0(C, Board, '_'));
        (nth0(A, Board, Player), nth0(B, Board, '_'), nth0(C, Board, Player));
        (nth0(A, Board, '_'), nth0(B, Board, Player), nth0(C, Board, Player))
    ).

one_and_empty(Board, Player) :-
    win_positions([A, B, C]),
    (   (nth0(A, Board, Player), nth0(B, Board, '_'), nth0(C, Board, '_'));
        (nth0(A, Board, '_'), nth0(B, Board, '_'), nth0(C, Board, Player));
        (nth0(A, Board, '_'), nth0(B, Board, Player), nth0(C, Board, '_'))
    ).
all_possible_moves(state(_, Boards, Statuses, NextBoard), Moves) :-
    findall(move(BoardIndex, Pos),
            (between(0, 8, BoardIndex), between(0, 8, Pos),
             is_legal(state('_', Boards, Statuses, NextBoard), move(BoardIndex, Pos))),
            Moves).
main :-
    write('Choose mode: 1 for PvP, 2 for PvAI'), nl,
    read(Mode),
    initial_state(State),
    (Mode = 1->  play(State);
     Mode = 2 ->  write('Enter AI search depth (e.g., 1, 2, 3): '), nl, read(Depth),
                     (integer(Depth), Depth > 0  ->  play_ai(State, Depth);
                        write('Invalid depth. Must be a positive integer.'), nl,main )  ;
     write('Invalid mode.'), nl, main).