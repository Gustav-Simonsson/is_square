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

-type point_2d() :: {integer() | float(),
                     integer() | float()}.

-type point_3d() :: {integer() | float(),
                     integer() | float(),
                     integer() | float()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec(is_square(P1 :: point_2d(),
                P2 :: point_2d(),
                P3 :: point_2d(),
                P4 :: point_2d()) -> boolean()).
is_square(P1,P2,P3,P4) ->
    L = [P1,P2,P3,P4],
    ValidPointDists = fun(Pd) -> case lists:usort(Pd) of
                                     [_Zero, SideLenSq, DiagonalSq] ->
                                         (SideLenSq * 2) == DiagonalSq;
                                     _ -> false
                                 end
                      end,
    lists:all(ValidPointDists, [[point_dist_2d_sq(Pa, Pb) || Pb <- L] || Pa <- L]).

-spec(is_cube(PointsList :: [point_3d()]) -> boolean()).
is_cube(PointsList) ->
    ValidPointDists =
        fun(Pd) -> case lists:usort(Pd) of
                       [_Zero, SideLenSq, FaceDiagonalSq, SpaceDiagonalSq] ->
                           (SideLenSq * 2) == FaceDiagonalSq andalso
                           (SideLenSq * 3) == SpaceDiagonalSq;
                       _ -> false
                   end
        end,
    lists:all(ValidPointDists, [[point_dist_3d_sq(Pa, Pb) ||
                                    Pb <- PointsList]     ||
                                    Pa <- PointsList]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
point_dist_2d_sq({X1,Y1}, {X2,Y2}) ->
    math:pow(abs(X2 - X1), 2) + math:pow(abs(Y2 - Y1), 2).

point_dist_3d_sq({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    math:pow(abs(X2 - X1), 2) +
    math:pow(abs(Y2 - Y1), 2) +
    math:pow(abs(Z2 - Z1), 2).

%%%===================================================================
%%% Tests and printouts
%%%===================================================================
tests_square() ->
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
    lists:map(fun print_result_2d/1, PointsLists),
    ok.

tests_cube() ->
    PointsLists =
        [
         {[{0,0,0},{0,0,1},{0,1,0},{0,1,1},
           {1,0,0},{1,0,1},{1,1,0},{1,1,1}], "normal cube"},
         {[{0,0,0},{0,0,1},{0,1,0},{0,1,1},
           {4,0,0},{4,0,1},{4,1,0},{4,1,1}], "rectangular box"}

        ],
    lists:map(fun print_result_3d/1, PointsLists),
    ok.

print_result_2d({L, Desc}) ->
    Res = case apply(fun is_square/4, L) of
              true  -> "is_square  : ";
              false -> "not_square : "
          end,
    io:format("~p~p (~p)~n", [Res, L, Desc]).

print_result_3d({L, Desc}) ->
    Res = case is_cube(L) of
              true  -> "is_cube  : ";
              false -> "not_cube : "
          end,
    io:format("~p~p (~p)~n", [Res, L, Desc]).
