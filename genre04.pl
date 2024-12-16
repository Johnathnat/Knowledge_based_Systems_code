% Define weights for features in each genre
genre_weights(metal, [distorted-3, fast-3, band-3, heavy-2]).
genre_weights(punk, [distorted-3, fast-3, band-3, heavy-2, political-1]).
genre_weights(pop, [slow-3, studio-3, light-3]).
genre_weights(classical, [slow-3, band-3, light-2]).
genre_weights(rap, [fast-3, studio-3, heavy-2, rhymes-2, political-1]).
genre_weights(electro, [distorted-3, slow-2, studio-3, light-1]).
genre_weights(dnb, [distorted-3, fast-3, studio-3, heavy-2, rhymes-1]).

% Main predicate to start the program
go :-
    write('Heya! Welcome to genre suggester! Please answer the following questions and I will then guess what music genre you may like! '), nl,
    collect_user_preferences,
    calculate_and_display_scores,
    undo.

% Collect user preferences for all features
collect_user_preferences :-
    forall(feature(Feature), ask_user_preference(Feature)).

% Calculate and display scores for each genre
calculate_and_display_scores :-
    findall(Genre-Score, (genre_weights(Genre, Weights), calculate_score(Weights, Score)), Scores),
    sort(2, @>=, Scores, SortedScores),
    display_genres(SortedScores).

% Calculate score for a genre based on user preferences
calculate_score([], 0).
calculate_score([Feature-Weight | Rest], Score) :-
    (user_likes(Feature) -> FeatureScore is Weight ; FeatureScore is 0),
    calculate_score(Rest, RestScore),
    Score is FeatureScore + RestScore.

% Ask user for each feature and store their preference
ask_user_preference(Feature) :-
    format('Do you like ~w (y/n)? ', [Feature]),
    read(Response),
    (member(Response, [yes, y]) -> assert(yes(Feature)) ; assert(no(Feature))).

% Check if user likes a feature
user_likes(Feature) :- yes(Feature).

% Dynamic predicates to store user preferences
:- dynamic yes/1, no/1.

% Undo user preferences
undo :- retractall(yes(_)), retractall(no(_)).

% Rank genres
display_scores(Scores) :-
	group_by_score(Scores, MostLike, MaybeLike, LeastLike),
	
	(MostLike \= [] ->
		write('Based on your answers, you would like these genres the most: '), nl,
		display_genres(MostLike)
	; true),
	
	(MaybeLike \= [] ->
		write('You may also like these genres: '), nl,
		display_genres(MaybeLike)
	; true),
	
	(LeastLike \= [] ->
		write('In terms of other genres, how is how they ranked: '), nl,
		display_genres(LeastLike)
	; true).
	
%setting group ranges
group_by_score([], [], [], []).
group_by_score([Genre-Score | Rest], [Genre-Score | MostLike], MaybeLike, LeastLike) :-
	Score >= 8, !,
	group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, [Genre-Score | MaybeLike], LeastLike) :-
	Score =< 7, Score > 5, !,
	group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, MaybeLike, [Genre-Score | LeastLike]) :-
	Score =< 5,
	group_by_score(Rest, MostLike, MaybeLike, LeastLike).

% Display scores for all genres
display_genres([]).
display_genres([Genre-Score | Rest]) :-
    format('Genre: ~w, Score: ~d~n', [Genre, Score]),
    display_genres(Rest).

% List of features to be asked
feature(distorted).
feature(fast).
feature(slow).
feature(band).
feature(studio).
feature(light).
feature(heavy).
feature(rhymes).
feature(political).
