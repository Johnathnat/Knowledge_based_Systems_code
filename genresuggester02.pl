% helps a user work out what genres and bands to try.
% this document will be done in plain english to create ideas

% genres metal, punk, pop, classical, rap, electro, dnb
% defining genre rules: sound(clean, distorted), tempo(slow,
% fast), manufacture (band, studio),lyrics(light,
% heavy,),rhyming(lots, some), political(not,political).

go :-
    write('Are you in the mood for music? '),
    read(Genre), get_single_char(Code),
    suggestion(Genre, Genre_type),
    write_list(['Your genre may be ', Genre_type, '.']),nl,undo.

go :-
    write('Sorry, I have no clue what you like'),nl,
    write('What music do you even want to listen to?'),nl,undo.

feature(Genre,distorted) :-
    verify(Genre, "Are the lyrics distorted, (y/n) ?").
feature(Genre,fast) :-
    verify(Genre, "Is the tempo fast, (y/n) ?").
feature(Genre,slow) :-
    verify(Genre, "Is the tempo slow, (y/n) ?").
feature(Genre,band) :-
    verify(Genre, "Do you like your music to be made by a band, (y/n) ?").
feature(Genre,studio) :-
    verify(Genre, "Do you like your music to be produced by a studio, (y/n) ?").
feature(Genre,light) :-
    verify(Genre, "Do you like lyrics to be light (y/n) ?").
feature(Genre,heavy) :-
    verify(Genre, "Do you like lyrics to be heavy (y/n) ?").
feature(Genre,rhymes) :-
    verify(Genre, "Is rhyming a main focus (y/n) ?").
feature(Genre,political) :-
    verify(Genre, "Are the themes political (y/n) ?").




ask(Genre,Question) :-
    write(Genre),write(' Do you like music that is: '), write(Question),
    read(N),
    (   (N == yes ; N == y)
    ->
        assert(yes(Question)) ;
        assert(no(Question)), fail).

:- dynamic yes/1, no/1.

verify(P,S) :-
    (   yes(S) -> true ;
      (no(S) -> fail ;
       ask(P,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.


%metal (distorted,fast,band,heavy)
suggestion(Genre,metal):-
    feature(Genre,distorted),
    feature(Genre,fast),
    feature(Genre,band),
    feature(Genre,heavy).

%punk(distorted,fast,band,heavy,political)
suggestion(Genre,punk):-
    feature(Genre,distorted),
    feature(Genre,fast),
    feature(Genre,band),
    feature(Genre,heavy),
    feature(Genre,political).

%pop(slow,produced,light)
suggestion(Genre,pop):-
    feature(Genre,slow),
    feature(Genre,studio),
    feature(Genre,light).

%classical(slow,band,light)
suggestion(Genre,classical):-
    feature(Genre,slow),
    feature(Genre,band),
    feature(Genre,light).

%rap(fast,produced,heavy,rhymes,political)
suggestion(Genre,rap):-
    feature(Genre,fast),
    feature(Genre,studio),
    feature(Genre,varied),
    feature(Genre,rhymes),
    feature(Genre,political).


%electro(distorted,slow,produced,light)
suggestion(Genre,electro):-
    feature(Genre,distorted),
    feature(Genre,slow),
    feature(Genre,studio),
    feature(Genre,light).


%dnb(distorted,fast,produced,heavy,rhymes).
suggestion(Genre,dnb):-
    feature(Genre,distorted),
    feature(Genre,fast),
    feature(Genre,studio),
    feature(Genre,heavy),
    feature(Genre,rhymes).


write_list([]).
write_list([Term| Terms]) :-
    write(Term),
    write_list(Terms).

response(Reply) :-
    get_single_char(Code),
    put_code(Code), nl,
    char_code(Reply, Code).




































