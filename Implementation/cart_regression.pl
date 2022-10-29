:- module(cart_regression, [residual_sum_sq/3, split_class/4, mean/2, residual_split/3,
    impurity/3, best_split/3, get_value/3, get_values_n/3,get_values_k/3, 
    my_powerset/3, combination/3, all_combinations/4,create_candidates/3, take/3,
    split_candidates_attribute/3, split_candidates/3,find_best_split/3,check_attribute_number/2,
    split/4, check_classes/2, induce_tree/3, check_sample/3, check_all_sample/3]).

:- load_test_files([]).

% Bisher Vorgesehenes Datenformat:Daten = [example(Value, [att1 = value1, att2 = value2,..]), example(...), ...]
%                                 Attribute = [att1, att2, ..]

%! check_all_sample(+Tree:Entscheidungsbaum, +Examples:list, -Res:list) is det
%
%  Gibt zu jedem Datenpunkt aus der Liste eine Vorhersage vom Entscheidungsbaum aus
check_all_sample(Tree, Examples, Res) :-
    maplist(check_sample(Tree), Examples, Res).

%! check_sample(+Tree:Entscheidungsbaum, +Example:Datenpunkt/list, -Res:Class) is det
%
%  gibt einen korrekten Datenpunkt ein, um dann die vom Entscheidungsbaum berechnete Klasse zu nehmen
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

%! induce_tree(+Daten:list, +Attribute:list, Tree) is nondet
%
%  induziert den Entscheidungsbaum durch 3 verschiedene Faelle:
%  1) Tree = null, wenn keine Daten existieren
%  2) Tree = leaf(Class), wenn alle Beispiele zur selben Klasse gehoeren -> Abbruchbedingung, hab mich gegen MaxTiefe entschieden
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

%! split(+Daten:list, +BestAttVal:Att = Val, -Split1:list, -Split2:list)
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
        mean(Klassen, Res),
        best_split(Res, err, BestAttVal)).

%! get_classes(+Daten:list, Klassen:list) is det
%
%  erstellt eine Liste, die die Klasse von jedem Datenpunkt enthaelt
get_classes([], []).
get_classes([example(Klasse,_)|T], [Klasse|Klassen]) :-
    get_classes(T, Klassen).

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
    %sort(Values, SortValues), -> Designaenderung! gibt jetzt eine Liste von unsortierten Att = Val Paaren aus!!!
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
%  Erster Fall ist ein Randfall fuer Random Forest
best_split(Klasse, err, err = Klasse).
best_split([H|T], Daten, Res) :-
    impurity(Daten, H, Min),
    best_split(T, Daten, Min, H, Res).
best_split([], _, _, Res, Res) :- !.
best_split([H|T], Daten, Min, Cur, Res) :-
    impurity(Daten, H, ResSumSq),
    (ResSumSq < Min ->
        best_split(T, Daten, ResSumSq, H, Res)
    ;   best_split(T, Daten, Min, Cur, Res)).

%! impurity(+Daten:list, +AttVal:Attribute = Value, -Unreinheit:double) is  det
%
%  Gibt die kombinierte Summe der quadrierten Residuen, von dem Split des Datensatzes, aus
impurity(Daten, AttVal, ResSumSq) :-
    split_class(Daten, AttVal, Split1, Split2),
    residual_sum_sq(Split1, Split2, ResSumSq).

%! residual_sum_sq(+Split1:list, +Split2:list, -ResSumSq:double) is det
%
%  Berechnet die Summe der quadrierten Residuen von zwei Splits. Beachte Split ist eine Liste von Target Werten!
residual_sum_sq(Split1, Split2, ResSumSq) :-
    mean(Split1, Mean1),
    mean(Split2, Mean2),
    residual_split(Split1, Mean1, SumSq1),
    residual_split(Split2, Mean2, SumSq2),
    ResSumSq is SumSq1 + SumSq2.

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

%! mean(+Daten:list, -Mean:double) is det
%
%  berechnet den arithmetische Mittel des Datensatzes
mean([], 0) :- !.
mean(Daten, Res) :-
    mean(Daten, 0, 0, Res).
mean([], Sum, L, Res) :- !, Res is Sum / L.
mean([H|T], Sum, L, Res) :-
    NewSum is Sum + H,
    NewL is L + 1,
    mean(T, NewSum, NewL, Res).

%! residual_split(+Daten:list, +Mean:double, -SumSq:double) is det
%
%  Berechnet die Summe der quadrierten Abweichnungen der einzelnen Werte zum arithmetischen Mittel
residual_split([], _, 0) :- !.
residual_split(Daten, Mean, SumSq) :-
    residual_split(Daten, Mean, 0, SumSq).
residual_split([], _, SumSq, SumSq) :- !.
residual_split([H|T], Mean, Acc, SumSq) :-
    Diff is H - Mean,
    Sq is Diff**2,
    NewAcc is Acc + Sq,
    residual_split(T, Mean, NewAcc, SumSq).

