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
    welcome_message,
    help_message,
    collect_user_preferences,
    calculate_and_display_scores,
    display_preferences,
	goodbye_message,
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
    repeat,
    format('Do you like ~w (y/n)? ', [Feature]),
    read(Response),
    (member(Response, [yes, y, no, n]) -> true ; (write('Invalid input. Please enter y/n.'), nl, fail)),
    (member(Response, [yes, y]) -> assert(yes(Feature)) ; assert(no(Feature))),
    !.

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
    display_group('You may also like these genres: ', MaybeLike),
    display_group('In terms of other genres, here is how they ranked: ', LeastLike),
    display_suggestions(Scores).

% Group scores by ranges
group_by_score([], [], [], []).
group_by_score([Genre-Score | Rest], [Genre-Score | MostLike], MaybeLike, LeastLike) :-
    Score >= 8, !,
    group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, [Genre-Score | MaybeLike], LeastLike) :-
    Score =< 7, Score >= 5, !,
    group_by_score(Rest, MostLike, MaybeLike, LeastLike).
group_by_score([Genre-Score | Rest], MostLike, MaybeLike, [Genre-Score | LeastLike]) :-
    Score =< 4,
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

% List of genres and their songs
genre_songs(metal, ['Gojira | Gift of Guilt', 'Black Sabbath | Children of the Grave', 'Rob Zombie | Living Dead Girl']).
genre_songs(punk, ['Minor Threat | Minor Threat', 'Dog Park Dissidents | Queer as in Fuck You', 'Death | Politicians In My Eyes']).
genre_songs(pop, ['Killers | Mr Brightside', 'Ed Sheeran | Shape of You', 'Taylor Swift | Shake It Off']).
genre_songs(classical, ['Chopin | Noturne in E-flat Major', 'Mozart | Eine Kleine Nachtmusik', 'Beethoven | Symphony No. 5']).
genre_songs(rap, ['Eminem | Like Toy Soldiers', 'DAMAG3 | LOBOTOMY', 'Gorrilaz | Feel Good Inc']).
genre_songs(electro, ['Daft Punk | Get Lucky', 'MGMT, Justice | Electric Feel - Justice Remix', 'Deadmau5 | Strobe']).
genre_songs(dnb, ['Pendulum | Tarantula', 'Turno, Skepsis, Charlotte Plank | Rave Out', 'Becky Hill, Chase & Status | Disconnect']).

% Display songs for a genre
suggestions(Genre) :-
    genre_songs(Genre, Songs),
    write('Since you like '), write(Genre), write(', here are some songs and bands you may enjoy: '), nl,
    display_songs(Songs).

% Display songs
display_songs([]).
display_songs([Song | Rest]) :-
    write(Song), nl,
    display_songs(Rest).

% Display suggestions based on scores
display_suggestions([]).
display_suggestions([Genre-Score | Rest]) :-
    Score >= 8, % Threshold for displaying suggestions
    suggestions(Genre),
    display_suggestions(Rest).
display_suggestions([_ | Rest]) :- % Skip genres that don't meet the threshold
    display_suggestions(Rest).


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

% Additional rules
welcome_message :-
    write('Welcome to the music genre suggester!'), nl.

goodbye_message :-
    write('Thank you for using the music genre suggester! Goodbye!'), nl.

help_message :-
    write('This program suggests music genres based on your preferences. Answer the questions with "yes" or "no".'), nl.

display_preferences :-
    findall(Feature, yes(Feature), Preferences),
    write('Your preferences: '), write(Preferences), nl.

reset_program :-
    undo,
    write('The program has been reset.'), nl.
