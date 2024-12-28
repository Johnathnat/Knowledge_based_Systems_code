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
    write('Heya! Welcome to the music genre suggester! Please answer the following questions and I will then guess what music genre you may like! '), nl,
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
    group_and_display_scores(SortedScores).

% Calculate score for a genre based on user preferences
calculate_score(Weights, Score) :-
    findall(FeatureScore, (
        member(Feature-Weight, Weights),
        (user_likes(Feature) -> FeatureScore is Weight ; FeatureScore is 0)
    ), FeatureScores),
    sum_list(FeatureScores, Score).

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

% Group and display scores
group_and_display_scores(Scores) :-
    group_by_score(Scores, MostLike, MaybeLike, LeastLike),
    display_group('Based on your answers, you would like these genres the most: ', MostLike),
    suggestions(Genre),
	display_group('You may also like these genres: ', MaybeLike),
    display_group('In terms of other genres, here is how they ranked: ', LeastLike).

% Group scores by ranges
group_by_score([], [], [], []).
group_by_score([Genre-Score | Rest], [Genre-Score | MostLike], MaybeLike, LeastLike) :-
    Score >= 10, !,
    group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, [Genre-Score | MaybeLike], LeastLike) :-
    Score =< 9, Score >= 6, !,
    group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, MaybeLike, [Genre-Score | LeastLike]) :-
    Score =< 5,
    group_by_score(Rest, MostLike, MaybeLike, LeastLike).

% Display grouped genres and scores
display_group(_, []) :- !.
display_group(Message, Group) :-
    write(Message), nl,
    display_genres(Group).

% Display genres and scores
display_genres([]).
display_genres([Genre-Score | Rest]) :-
    format('Genre: ~w, Score: ~d~n', [Genre, Score]),
    display_genres(Rest).


% list of songs for each genre
	
suggestions(Genre = metal) :-
	write(' Since you like metal, here are some songs and bands you may enjoy: '), nl,
	write(' Gojira | Gift of Guilt'), nl,
	write(' Black Sabbath | Children of the Grave'), nl,
	write(' Rob Zombie | Living Dead Girl') ,nl.
	
suggestions(Genre = punk) :-
	write(' Since you like punk, here are some songs and bands you may enjoy: '), nl,
	write(' Minor Threat | Minor Threat'), nl,
	write(' Dog Park Dissidents | Queer as in Fuck You'), nl,
	write(' Death | Politicians In My Eyes'), nl.

suggestions(Genre = pop) :-
	write(' Since you like pop, here are some songs and bands you may enjoy: '), nl,
	write(' Killers | Mr Brightside'), nl,
	write(' Ed Sheeran | Shape of You'), nl,
	write(' Taylor Swift | Shake It Off'), nl.
	
suggestions(Genre = classical) :-
	write(' Since you like classical, here are some songs and bands you may enjoy: '), nl,
	write(' Chopin | Noturne in E-flat Major'), nl,
	write(' Mozart | Eine Kleine Nachtmusik'), nl,
	write(' Beethoven | Symphony No. 5'), nl.

suggestions(Genre = rap) :-
	write(' Since you like rap, here are some songs and bands you may enjoy: '), nl,
	write(' Eminem | Like Toy Soldiers'), nl,
	write(' DAMAG3 | LOBOTOMY'), nl,
	write(' Gorrilaz | Feel Good Inc'), nl.

suggestions(Genre = electro) :-
	write(' Since you like electro, here are some songs and bands you may enjoy: '), nl,
	write(' Daft Punk | Get Lucky'), nl,
	write(' MGMT, Justice | Electric Feel - Justice Remix'), nl,
	write(' Deadmau5 | Strobe'), nl.

suggestions(Genre = dnb) :-
	write(' Since you like dnb, here are some songs and bands you may enjoy: '), nl,
	write(' Pendulum | Tarantula '), nl,
	write(' Turno, Skepsis, Charlotte Plank | Rave Out'), nl,
	write(' Becky Hill, Chase & Status | Disconnect'), nl.

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
