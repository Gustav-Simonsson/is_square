%%%-------------------------------------------------------------------
%%% @author gustav <gustav.simonsson@gmail.com>
%%% @copyright (C) 2012, gustav
%%% @doc
%%% Function to determine if four 2-D points makes square.
%%%
%%% Solution to olimex weekend challenge:
%%% http://olimex.wordpress.com/2013/09/27/weekend-programming-challenge-week-26-square/
%%% @end
%%% Created : 28 Sep 2013 by gustav <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(square).
-compile(export_all).

-type point() :: {integer() | float(),
                  integer() | float()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec(is_square(P1 :: point(),
                P2 :: point(),
                P3 :: point(),
                P4 :: point()) -> boolean()).
is_square(P1,P2,P3,P4) ->
    case lists:usort([P1,P2,P3,P4]) of
        [_,_,_,_] = L ->
            lists:all(fun(Pd) -> case lists:usort(Pd) of
                                     [_Zero, SideLenSq, DiagonalSq] ->
                                         (SideLenSq * 2) == DiagonalSq;
                                     _ -> false
                                 end
                      end,
                      [[point_dist_sq(Pa, Pb) || Pb <- L] || Pa <- L]);
        _ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
point_dist_sq({X1,Y1}, {X2,Y2}) ->
    math:pow(abs(X2 - X1), 2) + math:pow(abs(Y2 - Y1), 2).

%%%===================================================================
%%% Tests and printouts
%%%===================================================================
tests() ->
    PointsLists =
        [
         {[{0,0},{0,1},{1,1},{1,0}],   "normal square"},
         {[{0,0},{2,1},{3,-1},{1,-2}], "not aligned square"},
         {[{0,0},{1,1},{0,1},{1,0}],   "different order of the points"},
         {[{0,0},{0,2},{3,2},{3,0}],   "rectangle"},
         {[{0,0},{3,4},{8,4},{5,0}],   "rhombus"},
         {[{0,0},{0,0},{1,1},{0,0}],   "three points are the same"},
         {[{0,0},{0,0},{1,0},{0,1}],   "two points are the same"}
        ],
    lists:map(fun print_result/1, PointsLists),
    ok.

print_result({L, Desc}) ->
    Res = case apply(fun is_square/4, L) of
              true  -> "is_square  : ";
              false -> "not_square : "
          end,
    io:format("~p~p (~p)~n", [Res, L, Desc]).
