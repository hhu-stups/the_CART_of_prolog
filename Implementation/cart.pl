:- module(cart, [gini/2, count_class/3, part_class/3, zaehl/3, calc_class/3,
     split_class/4, combgini/3, impurity/3, best_split/3, get_max/3, get_classes/2, get_value/3,
     get_values_n/3, get_values_k/3,  my_powerset/3, combination/3, all_combinations/4, take/3,
     create_candidates/3, check_attribute_number/2, split_candidates_attribute/3, split_candidates/3,
     find_best_split/3, split/4, check_classes/2, induce_tree/3, check_sample/3, check_all_sample/3]).

:- load_test_files([]).

% Bisher Vorgesehenes Datenformat:Daten = [example(Class, [att1 = value1, att2 = value2,..]), example(...), ...]
%                                 Attribute = [att1, att2, ..]

%! check_all_sample(+Tree:Entscheidungsbaum, +Examples:list, -Res:list) is det
%
%  Gibt zu jedem Datenpunkt aus der Liste eine Vorhersage vom Entscheidungsbaum aus
check_all_sample(Tree, Examples, Res) :-
    maplist(check_sample(Tree), Examples, Res).

%! check_sample(+Tree:Entscheidungsbaum, +Example:list, -Res:Class) is det
%
%  Gibt die vom Entscheidungsbaum vorhergesehene Klasse zu einem Datenpunkt([att1=val1,...]) aus
check_sample(leaf(Class), _, Class).
check_sample(tree(Att = Val1, SubTree1, SubTree2), Example, Result) :-
    get_value(Example, Att, Val2),
    (is_list(Val1) ->
        (member(Val2,Val1) ->
            check_sample(SubTree1,Example,Result)
        ;   check_sample(SubTree2,Example,Result))
    ;   (Val2 < Val1 ->
            check_sample(SubTree1,Example,Result)
        ;   check_sample(SubTree2,Example,Result))).

%! induce_tree(+Daten:list, +Attribute:list, -Tree:term) is det
%
%  induziert den Entscheidungsbaum durch 3 verschiedene Faelle:
%  1) Tree = null, wenn keine Daten existieren
%  2) Tree = leaf(Class), wenn alle Beispiele zur selben Klasse gehoeren
%            oder nicht mehr unterschieden werden koennen
%  3) Tree = tree(Attribut = Wert, SubTree1, SubTree2), wenn es mehr als eine vorhandene Klasse gibt.
induce_tree([], _, []) :- !.
induce_tree([example(Klasse, _)|T], _, leaf(Klasse)) :-
    check_classes(T, Klasse), !.
induce_tree(Daten, Attribute, tree(BestAttVal, SubTree1, SubTree2)) :-
    find_best_split(Daten, Attribute, BestAttVal),
    split(Daten, BestAttVal, Split1, Split2),
    !,
    induce_tree(Split1, Attribute, SubTree1),
    induce_tree(Split2, Attribute, SubTree2).
induce_tree(Daten, Attribute, leaf(Klasse)) :-
    find_best_split(Daten, Attribute, err = Klasse), !.

%! check_classes(+Daten:list, +Klasse:atom) is det
%
%  true, wenn alle Klassen aus Daten gleich Klasse sind.
check_classes([], _).
check_classes([example(KlasseX, _)|T], Klasse) :-
    Klasse == KlasseX,
    check_classes(T, Klasse).

%! split(+Daten:list, +BestAttVal:Att = Val, -Split1:list, -Split2:list) is det
%
%  Teilt den Datensatz in zwei kleinere auf, mithilfe des besten Attribut-Wert Paars
split(Daten, Att = Val, Split1, Split2) :-
    (is_list(Val) ->
        split_k(Daten, Att = Val, Split1, Split2)
    ;   split_n(Daten, Att = Val, Split1, Split2)).

%split_k und split_n sind Abwandlungen voneinander die sich um kategorische oder numerische Varaiblen kuemmern
split_k([], _, [], []) :- !.
split_k([example(Class, Obj)|T], Att = Val1, [example(Class,Obj)|Split1], Split2) :-
    get_value(Obj, Att, Val2),
    member(Val2, Val1),
    !,
    split_k(T, Att = Val1, Split1, Split2).
split_k([example(Class, Obj)|T], Att = Val1, Split1, [example(Class,Obj)|Split2]) :-
    get_value(Obj, Att, Val2),
    \+ member(Val2, Val1),
    !,
    split_k(T, Att = Val1, Split1, Split2).

split_n([], _, [], []) :- !.
split_n([example(Class, Obj)|T], Att = Val1, [example(Class,Obj)|Split1], Split2) :-
    get_value(Obj, Att, Val2),
    Val2 < Val1,
    !,
    split_n(T, Att = Val1, Split1, Split2).
split_n([example(Class, Obj)|T], Att = Val1, Split1, [example(Class,Obj)|Split2]) :-
    get_value(Obj, Att, Val2),
    \+ Val2 < Val1,
    !,
    split_n(T, Att = Val1, Split1, Split2).

%! find_best_split(+Daten:list, +Attribute:list, -BestAttVal:Att = Val) is det
%
% Findet den besten Split zur aktuellen Datenlage
find_best_split(Daten, Attribute, BestAttVal) :-
    split_candidates(Daten, Attribute, Kandidaten),
    ([] \= Kandidaten ->
        best_split(Kandidaten, Daten, BestAttVal)
    ;   get_classes(Daten, Klassen),
        sort(Klassen, Klassenliste),
        get_max(Klassenliste, Klassen, Max),
        best_split(Max, err, BestAttVal)).

%! get_classes(+Daten:list, -Klassen:list) is det
%
%  erstellt eine Liste, die die Klasse von jedem Datenpunkt enthaelt
get_classes([], []).
get_classes([example(Klasse,_)|T], [Klasse|Klassen]) :-
    get_classes(T, Klassen).

%! get_max(+Klassen:list, +Ergebnisse:Klassen, -Res:atom) is det
%
%  gibt die Klasse mit den meisten vorkomnissen in den Ergebnissen aus
get_max([Single], _, Single) :- !.
get_max([H|T], Ergebnisse, Res) :-
    zaehl(Ergebnisse, H, N),
    get_max(T, Ergebnisse, H, N, Res).
get_max([], _, Class, _, Class) :- !.
get_max([H|T], Ergebnisse, Cur, N, Res) :-
    zaehl(Ergebnisse, H, M),
    (N > M ->
        get_max(T, Ergebnisse, Cur, N, Res)
    ;   get_max(T, Ergebnisse, H, M, Res)).

%! split_candidates(+Daten:list, +Attribute:list, -Kandidaten:list) is det
%
%  Erstellt ein Liste mit allen moeglichen Splits zu allen Attribut
split_candidates(Daten, Attribute, Kandidaten) :-
    split_candidates(Attribute, Daten, [], Kandidaten).
split_candidates([], _, Kandidaten, Kandidaten) :- !.
split_candidates([H|T], Daten, Acc, Kandidaten) :-
    split_candidates_attribute(Daten, H, Res),
    append(Res, Acc, NewAcc),
    split_candidates(T, Daten, NewAcc, Kandidaten).
    
%! split_candidates_attribute(+Daten:list, +Attribut:atom, -Kandidaten:list) is det
%
%  Erstellt ein Liste mit allen moeglichen Splits zu einem Attribut
split_candidates_attribute(Daten, Attribut, Kandidaten) :-
    (check_attribute_number(Daten, Attribut) -> 
      get_values_n(Daten, Attribut, Values)
    ; get_values_k(Daten, Attribut, Values)),
    create_candidates(Values, Attribut, Kandidaten).

%! check_attribute_number(+Daten:list, +Attribut:atom) is det
%
%  true, wenn das Attribut numerisch ist; false, wenn das Attribut kategorisch ist 
check_attribute_number([example(_,[Attribut = Value|_])|_], Attribut) :- !, number(Value).
check_attribute_number([example(_,[Att1 = _|T])|_], Att2) :-
    Att1 \= Att2,
    check_attribute_number([example(_,T)|_], Att2).

%! create_candidates(+Values:list, +Attribut:atom, -Kandidaten:list) is det
%
%  Erstellt eine Liste von Split Kandidaten, indem die Mitte von jeweils zwei nebeneinander liegenden Werten
%  in die Ergebnisliste eingefuegt wird oder im Fall von kategorsichen Werten, die Haelfte aller moeglichen Mengen.
create_candidates([Val|T], Attribut, Kandidaten) :-
    number(Val), !,
    create_candidates_n([Val|T], Attribut, Kandidaten).
create_candidates(Values, Attribut, Kandidaten) :-
    my_powerset(Values, Attribut, Kandidaten).
create_candidates_n([_], _, []).
create_candidates_n([H1,H2|T], Att, [Att = Mitte|Kandidaten]) :-
    Tmp is H1 + H2,
    Mitte is Tmp / 2,
    create_candidates_n([H2|T], Att, Kandidaten).

%! my_powerset(+Values:list, +Attribut:atom, Kandiaten:list) is det
%
%  Erstellt 2^(m-1) - 1 verschiedene Teilmengen aus der Potenzmenge von SortValues
my_powerset(Values, Attribut, Kandidaten) :- % Beachte Format: att = [val], att= [val,val]...!!!!!
    length(Values, L),
    Exp is L - 1,
    Tmp is 2**Exp,
    Zaehler is Tmp - 1,
    my_powerset(Values, Attribut, Zaehler, 1, [], Kandidaten).
my_powerset(_, _, 0, _, Acc, Kandiaten) :- !, Kandiaten = Acc.
my_powerset(Values, Attribut, Zaehler, N, Acc, Kandiaten) :-
    all_combinations(N, Values, Attribut, Res),
    length(Res, L),
    NewZaehler is Zaehler - L,
    NewZaehler < 0,
    !,
    ZuNehmen is L + NewZaehler,
    take(Res, ZuNehmen, FirstElements), % in sicstus ersetzen, siehe Issue
    append(Acc, FirstElements, NewAcc),
    my_powerset(Values, Attribut, 0, N, NewAcc, Kandiaten).
my_powerset(Values, Attribut, Zaehler, N, Acc, Kandiaten) :-
    all_combinations(N, Values, Attribut, Res),
    length(Res, L),
    NewZaehler is Zaehler - L,
    NewZaehler >= 0,
    !,
    NewN is N + 1,
    append(Acc, Res, NewAcc),
    my_powerset(Values, Attribut, NewZaehler, NewN, NewAcc, Kandiaten).

%! take(+List:list, +N:int, -FirstElements:list) is det
%
%  nimmt die ersten N Elemente aus der Liste und gibt sie in FirstElements aus
take(_, 0, []) :- !.
take([H|T], N, [H|FirstElements]) :-
    NewN is N - 1,
    take(T, NewN, FirstElements).

%! all_combinations(+N:int, +Values:list, +Attribut:atom, -Kandidaten) is nondet
%
%  Erstellt alle moeglichen Kombinationen von N elementigen Mengen aus Values
all_combinations(N, L, Attribut, AllC) :-
    findall(Attribut = X,combination(L,N,X),AllC).

%! combination(+Values:list, +N:int, -Kandidaten:list) is nondet
%
%  Erstellt eine moegliche N elementige Kombination aus Values
combination(_,0,[]).
combination([H|T], N, [H|TRes]) :-
  N > 0,
  NewN is N - 1,
  combination(T, NewN, TRes).
combination([_|T], N, Res) :-
  N > 0,
  combination(T, N, Res).

%! get_values_k(+Daten:list, +Attribut:atom, -Values:list) is det
%
%  Erstellt eine Liste mit allen Werten die im aktuellen Datensatz zu einem kategorischen Attribut verfuegbar sind
get_values_k([], _, []).
get_values_k(Daten, Attribut, Values) :-
    get_values_k(Daten, Attribut, [], Values).
get_values_k([], _, Acc, Acc).
get_values_k([example(_,Obj)|T], Attribut, Acc, Values) :-
    get_value(Obj, Attribut, Val),
    (member(Val, Acc) ->
        get_values_k(T, Attribut, Acc, Values)
    ;   get_values_k(T, Attribut, [Val|Acc], Values)).

%! get_values_n(+Daten:list, +Attribut:atom, -Values:list) is det
%
%  Erstellt eine Liste mit allen Werten die im aktuellen Datensatz zu einem numerischen Attribut verfuegbar sind
get_values_n([], _, []).
get_values_n(Daten, Attribut, Values) :-
    get_values_n(Daten, Attribut, [], Values).
get_values_n([], _, Acc, Res) :- !, sort(Acc, Res).
get_values_n([example(_,Obj)|T], Attribut, Acc, Values) :-
    get_value(Obj, Attribut, Val),
    (member(Val, Acc) ->
        get_values_n(T, Attribut, Acc, Values)
    ;   append([Val], Acc, NewAcc),
        get_values_n(T, Attribut, NewAcc, Values)).

%! get_value(+Values:list, +Attribut:atom, -Val:int or atom) is det
%
%  Gibt den Wert des jewiligen Attributs, aus der Attribut = Wert Liste, von einem Datenpunkt aus
get_value([Attribut = Value|_], Attribut, Value) :- !.
get_value([Att1 = _|T], Att2, Value) :-
    Att1 \= Att2,
    get_value(T, Att2, Value).

%! best_split(+Kandidaten:list, +Daten:list, -BestAttVal: Att = Val) is det
%! Kandidaten = [Att = Val1, Att = Val2, ...]
%
%  Verwendet alle Kandidaten aus der Liste um den besten Split zu finden und auszugeben
best_split(Klasse, err, err = Klasse).
best_split([H|T], Daten, Res) :-
    impurity(Daten, H, Min),
    best_split(T, Daten, Min, H, Res).
best_split([], _, _, Res, Res) :- !.
best_split([H|T], Daten, Min, Cur, Res) :-
    impurity(Daten, H, CombUnreinheit),
    (CombUnreinheit < Min ->
        best_split(T, Daten, CombUnreinheit, H, Res)
    ;   best_split(T, Daten, Min, Cur, Res)).

%! impurity(+Daten:list, +AttVal:Attribute = Value, -Unreinheit:double) is  det
%
%  Gibt die kombinierte Unreinheit, von dem Split des Datensatzes, aus
impurity(Daten, AttVal, CombUnreinheit) :-
    split_class(Daten, AttVal, Split1, Split2),
    combgini(Split1, Split2, CombUnreinheit).

%! combgini(+Split1:list, +Split2:list, -CombUnreinheit:double) is det
%
%  Berechnet die gewichtete Summe von zwei Splits. Beachte Split ist eine Liste von Klassen!
combgini(Split1, Split2, CombUnreinheit) :-
    gini(Split1, Unreinheit1),
    gini(Split2, Unreinheit2),
    length(Split1, LSplit1),
    length(Split2, LSplit2),
    LDaten is LSplit1 + LSplit2,
    Gewicht1 is LSplit1 / LDaten,
    Gewicht2 is LSplit2 / LDaten,
    GUnreinheit1 is Gewicht1 * Unreinheit1,
    GUnreinheit2 is Gewicht2 * Unreinheit2,
    CombUnreinheit is GUnreinheit1 + GUnreinheit2.

%! split_class(+Daten:list, +AttVal:Attribute = Value, -Split1:list, -Split2:list) is det
%
%  Erstellt 2 Splits aus Daten. Es wird mithilfe von Attribut getrennt.
%  kategorische werte werden in Listen verpackt, numerische nicht.
split_class(Daten, AttVal, Split1, Split2) :-
    split_class(Daten, AttVal, [], [], Split1, Split2).
split_class([], _, Acc1, Acc2, Acc1, Acc2).
split_class([example(Class, [Att1 = _ | TEx])|TLi], Att2 = Val, Acc1, Acc2, Split1, Split2) :- %%sucht nach dem richtigen Feature
    Att1 \= Att2,
    !,
    split_class([example(Class,TEx)|TLi], Att2 = Val, Acc1, Acc2, Split1, Split2).
split_class([example(Class, [Att = Val1 | _])|TLi], Att = Val2, Acc1, Acc2, Split1, Split2) :-
    \+ is_list(Val2),
    (Val1 < Val2 ->
        !, split_class(TLi, Att = Val2, [Class|Acc1], Acc2, Split1, Split2)
    ;   !, split_class(TLi, Att = Val2, Acc1, [Class|Acc2], Split1, Split2)).
split_class([example(Class, [Att = Val1 | _])|TLi], Att = Val2, Acc1, Acc2, Split1, Split2) :-
    is_list(Val2),
    (member(Val1, Val2) ->
        !, split_class(TLi, Att = Val2, [Class|Acc1], Acc2, Split1, Split2)
    ;   !, split_class(TLi, Att = Val2, Acc1, [Class|Acc2], Split1, Split2)).

%! gini(+Daten:list, -Unreinheit:double) is det
%
%  Berechnet die Gini Unreinheit (eines Splits); ab hier sind Daten eine Liste von Klassen!
gini([], 0) :- !.
gini(Daten, Unreinheit) :-
    count_class(Daten, Laenge, Klassen),
    part_class(Klassen, Daten, KlassenAnteil),
    calc_class(KlassenAnteil, Laenge, Res),
    Unreinheit is 1- Res.

%! count_class(+Daten:list, -Laenge:int, -Klassen:list) is det
%
%  Bestimmt die Laenge des Datensatz und erstellt eine Liste der vorhandenen Klassen
count_class(Daten, Laenge, Klassen) :-
    count_class(Daten, 0, [], Laenge, Klassen).
count_class([], Laenge, Klassen, Laenge, Klassen).
count_class([Class|T], AccL, AccK, Laenge, Klassen) :-
    NewAccL is AccL + 1,
    (member(Class, AccK) ->
        count_class(T, NewAccL, AccK, Laenge, Klassen)
    ;   count_class(T, NewAccL, [Class|AccK], Laenge, Klassen)).

%! part_class(+Klassen:list, +Daten:list, -KlassenAnteil:list) is det
%
%  Zaehlt die Anteile der einzelnen Klassen
part_class([], _, []) :- !.
part_class([Klasse|T], Daten, [Res|KlassenAnteil]) :-
    zaehl(Daten, Klasse, Res),
    part_class(T, Daten, KlassenAnteil).

%! zaehl(+Daten:list, +Klasse:Attribute, -Res:int) is det
%
%  Zaehlt das vorkommen einer einzelnen Klasse im Datensatz
zaehl(Daten, Klasse, Res) :-
    zaehl(Daten, Klasse, 0, Res).
zaehl([], _, Acc, Acc) :- !.
zaehl([Class|T], Klasse, Acc, Res) :-
    (Class == Klasse ->
        NewAcc is Acc + 1,
        zaehl(T, Klasse, NewAcc, Res)
    ;   zaehl(T, Klasse, Acc, Res)).

%! calc_class(+KlassenAnteil:list, +Laenge:int, -Res:double) is det
%
%  Berechnet die quadrierten Wahrscheinlichkeiten der Klassen
calc_class(KlassenAnteil, Laenge, Res) :-
    calc_class(KlassenAnteil, Laenge, 0, Res).
calc_class([], _, Acc, Acc) :- !.
calc_class([H|T], Laenge, Acc, Res) :-
    Tmp is H / Laenge,
    TmpSq is Tmp * Tmp,
    NewAcc is Acc + TmpSq,
    calc_class(T, Laenge, NewAcc, Res).
