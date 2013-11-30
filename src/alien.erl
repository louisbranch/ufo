-module(alien).
-export([total/1]).
-include("records.hrl").

%addAlien(Aliens, Color) ->
%  Alien = findAlien(Color, Aliens),
%  if
%    Alien =:= 0 -> {game_over, alien_color_reached_zero};
%    Alien > 0 ->
%      NewAliens = lists:keyreplace(Color, 1, Aliens, {Color, Alien - 1}),
%      {ok, NewAliens};
%    true -> exit(alien_color_number_mismatch)
%  end.

%find(Color, Aliens) ->
  %case lists:keyfind(Color, 1, Aliens) of
    %false -> exit(alien_color_not_found);
    %Alien -> Alien
  %end.

%% Return the total aliens
%% ignoring their colors
total(Aliens) ->
  lists:foldl(fun({_, Qty}, Sum) -> Qty + Sum end, 0, Aliens).
