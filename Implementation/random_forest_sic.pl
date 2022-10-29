:- module(random_forest_sic, [check_sample_forest/3, majority/2, induce_forest/4,
    bootstrappen/3, choose_attributes/3, induce_trees/3, get_sample/3,
    choose_members/4, choose_all_members/4, check_sample_forest/3, check_all_sample_forest/3]).

:- use_module(cart_sic).
:- use_module(library(lists)).
:- use_module(library(random)).

%-----------Auswertung-----------------------

%! check_all_sample_forest(+Tree:Entscheidungswald, +Examples:list, -Res:list) is det
%
%  Gibt zu jedem Datenpunkt aus der Liste eine Vorhersage vom Entscheidungswald aus
check_all_sample_forest(_, [], []).
check_all_sample_forest(RandomForest, [H|T], [Pred|Preds]) :-
    check_sample_forest(RandomForest, H, Pred),
    check_all_sample_forest(RandomForest, T, Preds).

%! check_sample_forest(+RandomForest:list, +Example:list, -Res:atom) is det
%
%  gibt die vom Entscheidungswald fuer einen Datenpunkt getroffene Vorhersage aus
check_sample_forest(RandomForest, Example, Res) :-
    check_forest(RandomForest, Example, Ergebnisse),
    majority(Ergebnisse,Res).

%! check_forest(+RandomForest:list, +Example:list, -Ergenisse:list) is det
%
%  stellt eine Liste aus den Ergebnissen der Entscheidungsbaeume, fuer den eingegebenen Datenpunkt zusammen
check_forest([], _, []).
check_forest([H|T], Example, [Ergebniss|Ergebnisse]) :-
    check_sample(H, Example, Ergebniss),
    check_forest(T, Example, Ergebnisse).

%! majority(+Ergebnisse:list, -Res:atom)
%
%  gibt die am meisten vorhandene Klasse aus
majority(Ergebnisse, Res) :-
    sort(Ergebnisse, Klassen),
    get_max(Klassen, Ergebnisse, Res).

%-----------Algortithmus---------------------

%! induce_forest(+Daten:list, Attribute:list, N:int,-RandomForest:list) is det
%
%  erstellt aus den Daten einen Entscheidungswald; Die Anzahl der Baueme wird angegeben;
%  die Anzahl pro Schritt benutzten Attribute orientiert sich an einer in der praxis bewaehrten Faustregel
induce_forest(Daten, Attribute, N, RandomForest) :-
    bootstrappen(Daten, N, Samples),
    choose_attributes(Attribute, N, AttributSets),
    induce_trees(Samples, AttributSets, RandomForest).

%! bootstrappen(+Daten:list, +Trees:int, -Samples:list) is det
%
%  erstellt eine Liste von Listen, wobei jede der N Listen ein bootstrap sample von dem Datensatz ist
bootstrappen(Daten, N, Samples) :-
    length(Daten, Len),
    bootstrappen(Daten, Len, N, Samples).
bootstrappen(_, _, 0, []) :- !.
bootstrappen(Daten, Len, N, [Sample|Samples]) :-
    get_sample(Daten, Len, Sample),
    NewN is N - 1,
    bootstrappen(Daten, Len, NewN, Samples).

%! get_sample(+Daten:list, +Len:int, -Sample:list) is det
%
%  kreiert durch Len-faches anwenden von random_member ein Bootstrap Sample des Datensatzes
get_sample(_, 0, []) :- !.
get_sample(Daten, Len, [Random|Sample]) :-
    random_member(Random, Daten),
    NewLen is Len - 1,
    get_sample(Daten, NewLen, Sample).

%! choose_attributes(+Attribute:list, +N:int, -AttributSets) is det
%
%  erstellt N Listen, welche aus zufaellig ausgewahelten Attributen bestehen. Die Anzahl der Ausgewaehlten Attributen ist,
%  bei Klassifikation, die abgerundete Wurzel, der Anzahl, der Attribute
choose_attributes(Attribute, N, AttributSets) :-
    length(Attribute, Int),
    TRes is sqrt(Int),
    Res is floor(TRes),
    choose_all_members(Attribute, Res, N, AttributSets).

%! choose_all_members(+Attribute:list, +Len:int, +N:int, -AttributSets:list) is det
%
%  erstell N, Len Lange Listen, welche, zufaellig ausgewaehlte, unterschiedliche Elemente aus Attribute beinhalten
choose_all_members(_, _, 0, []) :- !.
choose_all_members(Attribute, Len, N, [AttributSet|AttributSets]) :-
    random_permutation(Attribute, Permutation),
    prefix_length(Permutation, AttributSet, Len),
    NewN is N - 1,
    choose_all_members(Attribute, Len, NewN, AttributSets).

%! induce_trees(+Samples:list, +AttributSets:list, -RandomForest:list) is det
%
%  erstellt eine Liste aus Entscheidungsbaeumen, die aus den verschiedenen Bootstrap Samples und Attribut Sets hergestellt werden
induce_trees([], [], []) :- !.
induce_trees([S|ST],[A|AT], [Tree|RandomForest]) :-
    induce_tree(S,A,Tree), % Uebrprufe ob das verkleinern des Datensatzes, auf die ausgewaehlten Attribute, die Performance verbessert
    induce_trees(ST,AT,RandomForest).