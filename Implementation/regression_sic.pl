:- use_module(cart_regression_sic).
:- use_module(random_forest_regression_sic).
:- use_module(library(csv)).
:- use_module(library(random)).


run_california_tree :-
    train_test_data_tree('Datensets/california_train1.csv', 'Datensets/california_test1.csv', Time1, Gen1),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/california_train2.csv', 'Datensets/california_test2.csv', Time2, Gen2),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/california_train3.csv', 'Datensets/california_test3.csv', Time3, Gen3),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/california_train4.csv', 'Datensets/california_test4.csv', Time4, Gen4),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/california_train5.csv', 'Datensets/california_test5.csv', Time5, Gen5),
    mean([Time1, Time2, Time3, Time4, Time5],AvgT),
    mean([Gen1, Gen2, Gen3, Gen4, Gen5],AvgG),
    calc_metrics([Time1, Time2, Time3, Time4, Time5], AvgT, _, _, Err),
    format("california:~n Average train time: ~w sec.~n Standard Error of Mean(Time): ~w ~n Average MSE: ~w ~n", [AvgT,Err,AvgG]).


run_california_forest :-
    setrand(1),
    train_test_data_forest('Datensets/california_train1.csv', 'Datensets/california_test1.csv', Time1, Gen1),
    format("-------------------------------------------~n",[]),
    setrand(2),
    train_test_data_forest('Datensets/california_train2.csv', 'Datensets/california_test2.csv', Time2, Gen2),
    format("-------------------------------------------~n",[]),
    setrand(3),
    train_test_data_forest('Datensets/california_train3.csv', 'Datensets/california_test3.csv', Time3, Gen3),
    format("-------------------------------------------~n",[]),
    setrand(4),
    train_test_data_forest('Datensets/california_train4.csv', 'Datensets/california_test4.csv', Time4, Gen4),
    format("-------------------------------------------~n",[]),
    setrand(5),
    train_test_data_forest('Datensets/california_train5.csv', 'Datensets/california_test5.csv', Time5, Gen5),
    mean([Time1, Time2, Time3, Time4, Time5],AvgT),
    mean([Gen1, Gen2, Gen3, Gen4, Gen5],AvgG),
    calc_metrics([Time1, Time2, Time3, Time4, Time5], AvgT, _, _, Err),
    format("california:~n Average train time: ~w sec.~n Standard Error of Mean(Time): ~w ~n Average MSE: ~w ~n", [AvgT,Err,AvgG]).

run_diabetes_tree :-
    train_test_data_tree('Datensets/diabetes_train1.csv', 'Datensets/diabetes_test1.csv', Time1, Gen1),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/diabetes_train2.csv', 'Datensets/diabetes_test2.csv', Time2, Gen2),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/diabetes_train3.csv', 'Datensets/diabetes_test3.csv', Time3, Gen3),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/diabetes_train4.csv', 'Datensets/diabetes_test4.csv', Time4, Gen4),
    format("-------------------------------------------~n",[]),
    train_test_data_tree('Datensets/diabetes_train5.csv', 'Datensets/diabetes_test5.csv', Time5, Gen5),
    mean([Time1, Time2, Time3, Time4, Time5],AvgT),
    mean([Gen1, Gen2, Gen3, Gen4, Gen5],AvgG),
    calc_metrics([Time1, Time2, Time3, Time4, Time5], AvgT, _, _, Err),
    format("diabetes:~n Average train time: ~w sec.~n Standard Error of Mean(Time): ~w ~n Average MSE: ~w ~n", [AvgT,Err,AvgG]).

run_diabetes_forest :-
    setrand(1),
    train_test_data_forest('Datensets/diabetes_train1.csv', 'Datensets/diabetes_test1.csv', Time1, Gen1),
    format("-------------------------------------------~n",[]),
    setrand(2),
    train_test_data_forest('Datensets/diabetes_train2.csv', 'Datensets/diabetes_test2.csv', Time2, Gen2),
    format("-------------------------------------------~n",[]),
    setrand(3),
    train_test_data_forest('Datensets/diabetes_train3.csv', 'Datensets/diabetes_test3.csv', Time3, Gen3),
    format("-------------------------------------------~n",[]),
    setrand(4),
    train_test_data_forest('Datensets/diabetes_train4.csv', 'Datensets/diabetes_test4.csv', Time4, Gen4),
    format("-------------------------------------------~n",[]),
    setrand(5),
    train_test_data_forest('Datensets/diabetes_train5.csv', 'Datensets/diabetes_test5.csv', Time5, Gen5),
    mean([Time1, Time2, Time3, Time4, Time5],AvgT),
    mean([Gen1, Gen2, Gen3, Gen4, Gen5],AvgG),
    calc_metrics([Time1, Time2, Time3, Time4, Time5], AvgT, _, _, Err),
    format("diabetes:~n Average train time: ~w sec.~n Standard Error of Mean(Time): ~w ~n Average MSE: ~w ~n", [AvgT,Err,AvgG]).


create_forest(File, N, Forest, Time) :-
    open(File, read, Stream),
    read_records(Stream, [H|T]),
    get_attribute_list(H, Attribute),
    fit_format(T, Attribute, Data),
    statistics(walltime, _),
    induce_forest(Data, Attribute, N, Forest),
    statistics(walltime, [_,Time]).


create_tree(File, Tree, Time) :-
    open(File, read, Stream),
    read_records(Stream, [H|T]),
    get_attribute_list(H, Attribute),
    fit_format(T, Attribute, Data),
    statistics(walltime, _),
    induce_tree(Data, Attribute, Tree),
    statistics(walltime, [_,Time]).

%! get_attribute_list(+Terme:list,-Attribute:list)
%
%  Holt aus dem row Term alle Attribute aus, bis auf das letzte, da es die Klasse darstellt
get_attribute_list([_], []) :- !.
get_attribute_list([H|T], [Attribut|Attribute]) :-
    H =.. [_|[Attribut|_]],
    get_attribute_list(T, Attribute).

%! fit_format(+Terme:list, +Attribute:list, -Daten:list) is det
%
%  formiert den Datensatz in das benoetigte Format
fit_format([], _, []).
fit_format([H|T], Attribute, [example(Klasse,Datenpunkt)|Daten]) :-
    get_all_values(H, AlleWerte),
    append(Werte, [Klasse], AlleWerte),
    zip(Attribute, Werte, Datenpunkt),
    fit_format(T, Attribute, Daten).

%! get_all_values(+Terme:list, -Example:term) is det
%
%  extrahiert alle Werte aus einer Liste von Termen, mit dem Format read_record
get_all_values([], []).
get_all_values([H|T], [Wert|Werte]) :- !,
    H =.. [_|[Wert|_]],
    get_all_values(T, Werte).

%! zip(+Attribute:list, +Values:list, -ZipList:list) is det
%
%  fuegt die Werte mit den Attributen in forlgenden Format zusammen: att1=val1, att2=val2,...
zip([],[],[]) :- !.
zip([Att|TAtt], [Val|TVal], [Att=Val|ZipList]) :-
    zip(TAtt, TVal, ZipList).


%! fit_test_format(+Terms:list, +Attribute:list, -Daten:list, -Klassen:list) is det
%
%  formiert den Datensatz in das benoetigte Format
fit_test_format([], _, [], []).
fit_test_format([H|T], Attribute, [Obj|Daten], [Klasse|Klassen]) :-
    %fit_test_format_single(Attribute, H, Obj, Klasse),
    get_all_values(H, Werte),
    append(TmpObj, [Klasse], Werte),
    zip(Attribute, TmpObj, Obj),
    fit_test_format(T, Attribute, Daten, Klassen).

%! fit_test_format_single(+Attribute:list, +Term:term, -Example:term, -Klasse:atom) is det
%
%  bringt eine einzelnen Term in das geforderte Format
%fit_test_format_single(Attribute, Term, Obj, Klasse) :- !,
%    Term =.. [_|Values],
%    append(TmpObj, [Klasse], Values),
%    zip(Attribute, TmpObj, Obj).

%! precision(+Klassen:list, +Pred:list, -Gen:double) is det
%
%  Berechnet den Anteil der Elemente die in beiden Listen miteinander uebereinstimmen
precision(Klassen, Pred, Gen) :-
    precision(Klassen, Pred, 0, 0, Gen).
precision([], [], Len, Acc, Gen) :- !, Gen is Acc / Len.
precision([KH|KT], [PH|PT], Len, Acc, Gen) :-
    NewLen is Len + 1,
    (KH == PH ->
        NewAcc is Acc + 1,
        precision(KT, PT, NewLen, NewAcc, Gen)
    ;   precision(KT, PT, NewLen, Acc, Gen)).


%! test_data_tree(+Tree:compund, +Test:filename, -Gen:double) is det
%
%  Bringt die Testdaten fuer Baueme ins richtige Format und berechnet die Praezision
test_data_tree(Tree, Test, Gen) :-
    open(Test, read, Stream),
    read_records(Stream, [H|T]),
    get_attribute_list(H, Attribute),
    fit_test_format(T, Attribute, Examples, Klassen),
    check_all_sample(Tree, Examples, Pred),
    mean_sq_error(Klassen, Pred, Gen).

%! test_data_forest(+Forest:list, +Test:filename, -Gen:double) is det
%
%  Bringt die Testdaten fuer Random Forest ins richtige Format und berechnet die Praezision
test_data_forest(Forest, Test, Gen) :-
    open(Test, read, Stream),
    read_records(Stream, [H|T]),
    get_attribute_list(H, Attribute),
    fit_test_format(T, Attribute, Examples, Klassen),
    check_all_sample_forest(Forest, Examples, Pred),
    mean_sq_error(Klassen, Pred, Gen).

train_test_data_tree(Train, Test, Time, Gen) :-
    format("Running ~w ....~n", [Train]),
    create_tree(Train, Tree, MTime),
    !,
    Time is MTime / 1000,
    format("Tree creation ~w : ~w sec.~n",[Train,Time]),!,
    test_data_tree(Tree, Test, Gen),
    format("Tree has an MSE of: ~w .~n",[Gen]),!.

train_test_data_forest(Train, Test, Time, Gen) :-
    format("Running ~w ....~n", [Train]),
    create_forest(Train, 100, Forest, MTime),
    !,
    Time is MTime / 1000,
    format("Forest creation ~w : ~w sec.~n",[Train,Time]),!,
    test_data_forest(Forest, Test, Gen),
    format("Forest has an MSE of: ~w .~n",[Gen]),!.


%! mean_sq_error(+Klassen:list, +Pred:list, -Gen:double) is det
%
%  berechnet den Mean-Squared-Error zwischen Klassen und Pred.
mean_sq_error(Klassen, Pred, Gen) :-
    mean_sq_error(Klassen, Pred, 0, 0, Gen).
mean_sq_error([], [], Len, Acc, Gen) :- !, Gen is Acc / Len.
mean_sq_error([HK|TK], [HP|TP], L, Acc, Gen) :-
    Diff is HK - HP,
    Sq is Diff**2,
    NewAcc is Acc + Sq,
    NewL is L + 1,
    mean_sq_error(TK, TP, NewL, NewAcc, Gen).

%! calc_metrics(+Targets:list, +Mean:double, -Varianz:double, -Abw:double, -Err:double) is det
%
%  Berechnet die Sample Varianz, Sample Standardabweichung und Standard error of mean
calc_metrics([], _, 0, 0, 0) :- !.
calc_metrics(Targets, Mean, Varianz, Abw, Err) :-
    calc_metrics(Targets, Mean, -1, 0, Varianz, Abw, Err).
calc_metrics([], _, Len, SumSq, Varianz, Abw, Err) :- 
    !,
    Varianz is SumSq / Len,
    Abw is sqrt(Varianz),
    RealLen is Len + 1,
    RootLen is sqrt(RealLen),
    Err is Abw / RootLen.
calc_metrics([H|T], Mean, Len, Acc, Varianz, Abw, Err) :-
    Diff is H - Mean,
    Sq is Diff**2,
    NewAcc is Acc + Sq,
    NewLen is Len + 1,
    calc_metrics(T, Mean, NewLen, NewAcc, Varianz, Abw, Err).