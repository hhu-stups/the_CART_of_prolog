:- use_module(random_forest).
:- use_module(library(plunit)).

:- begin_tests(check_sample_forest).

test(check_sample_forest1, [true(Res == a)]) :-
    check_sample_forest([leaf(a),leaf(a),leaf(a)], [att1=1,att2=2], Res).

test(check_sample_forest2, [true(Res == a)]) :-
    check_sample_forest([leaf(a),leaf(b),leaf(a)], [att1=1,att2=2], Res).

test(check_sample_forest3, [true(Res == a)]) :-
    check_sample_forest([tree(att1 = 1.5, leaf(a), leaf(b)),tree(att2 = 2.5, leaf(a), tree(att1=1, leaf(b), leaf(c))),tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), tree(att1=2.5, leaf(c), leaf(d)))], [att1=1,att2=2], Res).

:- end_tests(check_sample_forest).

:- begin_tests(check_forest).

test(check_forest1, [true(Ergebnisse = [a])]) :-
    check_forest([leaf(a)], [], Ergebnisse).

test(check_forest2, [true(Ergebnisse = [a,b,a])]) :-
    check_forest([leaf(a),leaf(b),leaf(a)], [], Ergebnisse).

test(check_forest3, [true(Ergebnisse == [a,a,c])]) :-
    check_forest([tree(att1 = 1.5, leaf(a), leaf(b)),tree(att2 = 2.5, leaf(a), tree(att1=1, leaf(b), leaf(c))),tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), tree(att1=2.5, leaf(c), leaf(d)))], [att1=1,att2=2], Ergebnisse).

:- end_tests(check_forest).

:- begin_tests(majority).

test(majority1, [true(Res == a)]) :-
    majority([a,a,a], Res).

test(majority2, [true(Res == b)]) :-
    majority([a,b,a,b,a,b,b], Res).

test(majority3, [true(Res == c)]) :-
    majority([c,a,b,a,c,a,c,c], Res).

:- end_tests(majority).

:- begin_tests(induce_forest).

test(induce_forest1, [true(Forest == [leaf(a),leaf(a),leaf(a)])]) :-
    induce_forest([example(a,[]),example(a,[]),example(a,[])], [], 3, Forest).

:- end_tests(induce_forest).

:- begin_tests(bootstrappen).

test(bootstrappen1, [true(Res == [[example(a,[]),example(a,[]),example(a,[])]])]) :-
    bootstrappen([example(a,[]),example(a,[]),example(a,[])],1,Res).

test(bootstrappen2, [true(Res == [[example(a,[]),example(a,[]),example(a,[])],[example(a,[]),example(a,[]),example(a,[])],[example(a,[]),example(a,[]),example(a,[])]])]) :-
    bootstrappen([example(a,[]),example(a,[]),example(a,[])],3,Res).

:- end_tests(bootstrappen).

:- begin_tests(get_sample).

test(get_sample1, [true(Res = [])]) :-
    get_sample([], 0, Res).

test(get_sample1, [true(Res = [example(a,[]),example(a,[]),example(a,[])])]) :-
    get_sample([example(a,[]),example(a,[]),example(a,[])], 3, Res).

:- end_tests(get_sample).

:- begin_tests(choose_attributes).

test(choose_attributes1, [true(Res == [[example(a,[]),example(a,[])]])]) :-
    choose_attributes([example(a,[]),example(a,[]),example(a,[]),example(a,[])],1,Res).

test(choose_attributes2, [true(Res == [[example(a,[]),example(a,[])],[example(a,[]),example(a,[])],[example(a,[]),example(a,[])]])]) :-
    choose_attributes([example(a,[]),example(a,[]),example(a,[]),example(a,[])],3,Res).

:- end_tests(choose_attributes).

:- begin_tests(choose_all_members).

test(choose_all_members1, [true(Res == [[example(a,[]),example(a,[])]])]) :-
    choose_all_members([example(a,[]),example(a,[]),example(a,[])],2,1,Res).

test(choose_all_members2, [true(Res == [[example(a,[]),example(a,[]),example(a,[])],[example(a,[]),example(a,[]),example(a,[])],[example(a,[]),example(a,[]),example(a,[])]])]) :-
    choose_all_members([example(a,[]),example(a,[]),example(a,[])],3,3,Res).

:- end_tests(choose_all_members).

:- begin_tests(induce_trees).

test(induce_trees1, [true(Tree == [])]) :-
    induce_trees([],[],Tree).

test(induce_tree2, [true(Tree == [tree(att1 = 1.5, leaf(a), leaf(b))])]) :-
    induce_trees([[example(a, [att1 = 0]), example(a, [att1 = 1]), example(b, [att1 = 2])]], [[att1]], Tree).

test(induce_tree3, [true(Tree == [tree(att1 = 1.5, leaf(a), leaf(b)), tree(att2 = 0.5, leaf(a), leaf(b))])]) :-
    induce_trees([[example(a, [att1 = 0]), example(a, [att1 = 1]), example(b, [att1 = 2])], [example(a, [att1 = 0, att2 = 0]), example(a, [att1 = 1, att2 = 0]), example(b, [att1 = 1, att2 = 1])]], [[att1],[att1, att2]], Tree).

:- end_tests(induce_trees).
