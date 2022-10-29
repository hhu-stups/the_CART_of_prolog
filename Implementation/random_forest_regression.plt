:- use_module(random_forest_regression).
:- use_module(library(plunit)).

:- begin_tests(check_sample_forest).

test(check_sample_forest1, [true(Res == 0)]) :-
    check_sample_forest([leaf(0),leaf(0),leaf(0)], [att1=1,att2=2], Res).

test(check_sample_forest2, [true(Res == 1)]) :-
    check_sample_forest([leaf(0),leaf(1),leaf(2)], [att1=1,att2=2], Res).

test(check_sample_forest3, [true(Res == 0.6666666666666666)]) :-
    check_sample_forest([tree(att1 = 1.5, leaf(0), leaf(1)),tree(att2 = 2.5, leaf(0), tree(att1=1, leaf(1), leaf(2))),tree(att2 = 0.5, tree(att1=0.5, leaf(0), leaf(1)), tree(att1=2.5, leaf(2), leaf(4)))], [att1=1,att2=2], Res).

:- end_tests(check_sample_forest).

:- begin_tests(check_forest).

test(check_forest1, [true(Ergebnisse = [0])]) :-
    check_forest([leaf(0)], [], Ergebnisse).

test(check_forest2, [true(Ergebnisse = [0,1,0])]) :-
    check_forest([leaf(0),leaf(1),leaf(0)], [], Ergebnisse).

test(check_forest3, [true(Ergebnisse == [0,0,2])]) :-
    check_forest([tree(att1 = 1.5, leaf(0), leaf(1)),tree(att2 = 2.5, leaf(0), tree(att1=1, leaf(1), leaf(2))),tree(att2 = 0.5, tree(att1=0.5, leaf(0), leaf(1)), tree(att1=2.5, leaf(2), leaf(d)))], [att1=1,att2=2], Ergebnisse).

:- end_tests(check_forest).

:- begin_tests(induce_forest).

test(induce_forest1, [true(Forest == [leaf(0),leaf(0),leaf(0)])]) :-
    induce_forest([example(0,[]),example(0,[]),example(0,[])], [], 3, Forest).

:- end_tests(induce_forest).

:- begin_tests(bootstrappen).

test(bootstrappen1, [true(Res == [[example(0,[]),example(0,[]),example(0,[])]])]) :-
    bootstrappen([example(0,[]),example(0,[]),example(0,[])],1,Res).

test(bootstrappen2, [true(Res == [[example(0,[]),example(0,[]),example(0,[])],[example(0,[]),example(0,[]),example(0,[])],[example(0,[]),example(0,[]),example(0,[])]])]) :-
    bootstrappen([example(0,[]),example(0,[]),example(0,[])],3,Res).

:- end_tests(bootstrappen).

:- begin_tests(get_sample).

test(get_sample1, [true(Res = [])]) :-
    get_sample([], 0, Res).

test(get_sample1, [true(Res = [example(0,[]),example(0,[]),example(0,[])])]) :-
    get_sample([example(0,[]),example(0,[]),example(0,[])], 3, Res).

:- end_tests(get_sample).

:- begin_tests(choose_attributes).

test(choose_attributes1, [true(Res == [[example(0,[])]])]) :-
    choose_attributes([example(0,[]),example(0,[]),example(0,[]),example(0,[])],1,Res).

test(choose_attributes2, [true(Res == [[example(0,[])],[example(0,[])],[example(0,[])]])]) :-
    choose_attributes([example(0,[]),example(0,[]),example(0,[]),example(0,[])],3,Res).

:- end_tests(choose_attributes).

:- begin_tests(choose_all_members).

test(choose_all_members1, [true(Res == [[example(0,[]),example(0,[])]])]) :-
    choose_all_members([example(0,[]),example(0,[]),example(0,[])],2,1,Res).

test(choose_all_members2, [true(Res == [[example(0,[]),example(0,[]),example(0,[])],[example(0,[]),example(0,[]),example(0,[])],[example(0,[]),example(0,[]),example(0,[])]])]) :-
    choose_all_members([example(0,[]),example(0,[]),example(0,[])],3,3,Res).

:- end_tests(choose_all_members).

:- begin_tests(induce_trees).

test(induce_trees1, [true(Tree == [])]) :-
    induce_trees([],[],Tree).

test(induce_tree2, [true(Tree == [tree(att1 = 1.5, leaf(0), leaf(1))])]) :-
    induce_trees([[example(0, [att1 = 0]), example(0, [att1 = 1]), example(1, [att1 = 2])]], [[att1]], Tree).

test(induce_tree3, [true(Tree == [tree(att1 = 1.5, leaf(0), leaf(1)), tree(att2 = 0.5, leaf(0), leaf(1))])]) :-
    induce_trees([[example(0, [att1 = 0]), example(0, [att1 = 1]), example(1, [att1 = 2])], [example(0, [att1 = 0, att2 = 0]), example(0, [att1 = 1, att2 = 0]), example(1, [att1 = 1, att2 = 1])]], [[att1],[att1, att2]], Tree).

:- end_tests(induce_trees).
