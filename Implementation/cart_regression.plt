:- use_module(cart_regression).
:- use_module(library(plunit)).


:- begin_tests(check_all_sample).

test(check_all_sample1, [true(Result == [0,0,0])]) :-
    check_all_sample(leaf(0), [[att1=0], [att1=1], [att1=2]], Result).

test(check_all_sample2, [true(Result == [0,0,1])]) :-
    check_all_sample(tree(att1 = 1.5, leaf(0), leaf(1)), [[att1=0], [att1=1], [att1=2]], Result).


test(check_all_sample3, [true(Result == [0,1,2])]) :-
    check_all_sample(tree(att2 = 0.5, tree(att1=0.5, leaf(0), leaf(1)), tree(att1=2.5, leaf(2), leaf(4))), [[att1=0, att2=0], [att1=1,att2=0], [att1=2,att2=1]], Result).

:- end_tests(check_all_sample).

:- begin_tests(check_sample).

test(check_sample1, [true(Result == 0)]) :-
    check_sample(leaf(0), [], Result).

test(check_sample2, [true(Result == 0)]) :-
    check_sample(tree(att1 = 1.5, leaf(0), leaf(1)), [att1=1,att2=2], Result).


test(check_sample3, [true(Result == 0)]) :-
    check_sample(tree(att2 = 0.5, tree(att1=0.5, leaf(0), leaf(1)), tree(att1=2.5, leaf(2), leaf(4))), [att1=0,att2=0], Result).

:- end_tests(check_sample).

:- begin_tests(induce_tree).

test(induce_tree1, [true(Tree == leaf(0))]) :-
    induce_tree([example(0, [att1 = 0]), example(0, [att1 = 1]), example(0, [att1 = 2])], [att1], Tree).

test(induce_tree2, [true(Tree == tree(att1 = 1.5, leaf(0), leaf(1)))]) :-
    induce_tree([example(0, [att1 = 0]), example(0, [att1 = 1]), example(1, [att1 = 2])], [att1], Tree).

test(induce_tree3, [true(Tree == tree(att2 = 0.5, leaf(0), leaf(1)))]) :-
    induce_tree([example(0, [att1 = 0, att2 = 0]), example(0, [att1 = 1, att2 = 0]), example(1, [att1 = 1, att2 = 1])], [att1, att2], Tree).

test(induce_tree4, [true(Tree == tree(att1 = 2.5, tree(att2=0.5, tree(att1=0.5,leaf(0),leaf(1)), leaf(2)), leaf(4)))]) :-
    induce_tree([example(0, [att1 = 0, att2 = 0]), example(1, [att1 = 1, att2 = 0]), example(2, [att1 = 2, att2 = 1]), example(4, [att1 = 3, att2 = 1])], [att1, att2], Tree).

test(induce_tree5, [true(Tree == tree(att2 = 0.5, tree(att1=0.5, leaf(0), leaf(1)), leaf(2)))]) :-
    induce_tree([example(0, [att1 = 0, att2 = 0]), example(1, [att1 = 1, att2 = 0]), example(2, [att1 = 2, att2 = 1])], [att1, att2], Tree).

test(induce_tree6, [true(Tree == tree(att3 = 2.5, leaf(0), tree(att1=1, leaf(1), leaf(2))))]) :-
    induce_tree([example(0, [att1 = 0.25, att2 = hi, att3 = 1]), example(1, [att1 = 0, att2 = hi, att3 = 10]), example(2, [att1 = 2, att2 = bye, att3 = 5]), example(0, [att1 = 0.5, att2 = hi, att3 = 2]), example(1, [att1 = -1, att2 = bye, att3 = 3]), example(2, [att1 = 3, att2 = bye, att3 = 7]), example(0, [att1 = 0, att2 = bye, att3 = 1.5])], [att1, att2, att3], Tree).

test(induce_tree7, [true(Tree == tree(att2 = [hi], leaf(0), tree(att1 = 2, leaf(1), leaf(2))))]) :-
    induce_tree([example(0,[att1=1,att2=hi]),example(0,[att1=2,att2=hi]),example(0,[att1=3,att2=hi]),example(1,[att1=0,att2=wie]),example(1,[att1=1,att2=gehts]),example(2,[att1=3,att2=wie]),example(2,[att1=4,att2=gehts])],[att1,att2],Tree).

:- end_tests(induce_tree).

:- begin_tests(check_classes).

test(check_classes1) :-
    check_classes([],_).

test(check_classes2) :-
    check_classes([example(0,[att1=1]),example(0,[att1=2])],0).

test(check_classes3, [fail]) :-
    check_classes([example(0,[att1=1]),example(1,[att1=2])],0).

:- end_tests(check_classes).

:- begin_tests(split).

test(split1_1, [true(Split1 == [example(0, [att = 0]),example(0, [att = 0.25]),example(0, [att = 0.75])])]) :-
    split([example(0, [att = 0]),example(0, [att = 0.25]),example(0, [att = 0.75]),example(1, [att = 1])], att = 0.875, Split1, _).


test(split1_2, [true(Split2 == [example(1, [att = 1])])]) :-
    split([example(0, [att = 0]),example(0, [att = 0.25]),example(0, [att = 0.75]),example(1, [att = 1])], att = 0.875, _, Split2).

:- end_tests(split).

:- begin_tests(find_best_split).

test(find_best_split1, [all(BestAttVal == [att = 0.875])]) :-
    find_best_split([example(0, [att = 0]),example(0, [att = 0.25]),example(0, [att = 0.75]),example(1, [att = 1])], [att], BestAttVal).

test(find_best_split2, [all(BestAttVal == [att1 = 12.5])]) :-
    find_best_split([example(0, [att1 = 10, att2 = 0]),example(1, [att1 = 15, att2 = 1]),example(0, [att1 = 10, att2 = 2])], [att1, att2], BestAttVal).

:- end_tests(find_best_split).

:- begin_tests(split_candidates).

test(split_candidates1, [true(Kandidaten == [att2 = 0.5, att2 = 1.5, att1 = [b]])]) :-
    split_candidates([example(a, [att1 = a, att2 = 0]),example(b, [att1 = b, att2 = 1]),example(a, [att1 = b, att2 = 2])], [att1, att2], Kandidaten).

test(split_candidates2, [true(Kandidaten == [att2 = 0.5, att2 = 1.5, att1 = 12.5])]) :-
    split_candidates([example(a, [att1 = 10, att2 = 0]),example(b, [att1 = 15, att2 = 1]),example(a, [att1 = 10, att2 = 2])], [att1, att2], Kandidaten).

:- end_tests(split_candidates).

:- begin_tests(split_candidates_attribute).

test(split_candidates_attribute1, [true(Kandidaten == [att1=0.5, att1=1.5, att1=2.25])]) :-
    split_candidates_attribute([example(0, [att1 = 0, att2 = 0]), example(1, [att1 = 1, att2 = 1]), example(2, [att1 = 2, att2 = 2]), example(0, [att1 = 2.5, att2 = 0])], att1, Kandidaten).

test(split_candidates_attribute2, [true(Kandidaten == [att2=[c],att2=[b],att2=[a]])]) :-
    split_candidates_attribute([example(0, [att1 = 0, att2 = a]), example(1, [att1 = 1, att2 = b]), example(2, [att1 = 2, att2 = c]), example(0, [att1 = 2.5, att2 = a])], att2, Kandidaten).

:- end_tests(split_candidates_attribute).

:- begin_tests(check_attribute_number).

test(check_attribute_number1) :-
    check_attribute_number([example(0,[att1=1,att2=hi,att3=3])],att1).

test(check_attribute_number2, [fail]) :-
    check_attribute_number([example(0,[att1=1,att2=hi,att3=3])],att2).

:- end_tests(check_attribute_number).

:- begin_tests(create_candidates).

test(create_candidates1, [true(Kandidaten == [att=0.5, att=1.5])]) :-
    create_candidates([0,1,2], att, Kandidaten).

test(create_candidates2, [true(Kandidaten == [att=[a],att=[b],att=[c]])]) :-
    create_candidates([a,b,c], att, Kandidaten).

:- end_tests(create_candidates).

:- begin_tests(my_powerset).

test(my_powerset1, [true(Kandiaten == [])]) :-
    my_powerset([a], att, Kandiaten).

test(my_powerset2, [true(Kandiaten == [att=[a]])]) :-
    my_powerset([a,b], att, Kandiaten).

test(my_powerset3, [true(Kandiaten == [att=[a],att=[b],att=[c]])]) :-
    my_powerset([a,b,c], att, Kandiaten).

test(my_powerset4, [true(Kandiaten == [att=[a],att=[b],att=[c],att=[d],att=[a,b],att=[a,c],att=[a,d]])]) :-
    my_powerset([a,b,c,d], att, Kandiaten).

:- end_tests(my_powerset).

:- begin_tests(take).

tests(take1, [true(Res = [])]) :-
    take([a,b,c], 0, Res).

tests(take2, [true(Res = [a,b])]) :-
    take([a,b,c], 2, Res).

tests(take3, [true(Res = [a,b,c])]) :-
    take([a,b,c], 3, Res).

:- end_tests(take).

:- begin_tests(combination).

test(combination1, [true(Set == [hi])]) :-
    combination([hi, wie, gehts], 1, Set).

test(combination2, [true(Set == [hi, wie, gehts])]) :-
    combination([hi, wie, gehts], 3, Set).

:- end_tests(combination).

:- begin_tests(get_values_n).

test(get_values_n1, [true(Values == [1,2])]) :-
    get_values_n([example(a,[att1 = 1, att2 = 2]), example(b, [att1 = 2, att2 = 3])], att1, Values).

test(get_values_n2, [true(Values == [2,3])]) :-
    get_values_n([example(a,[att1 = 1, att2 = 2]), example(b, [att1 = 2, att2 = 3]), example(a, [att1 = 2, att2 = 2])], att2, Values).

:- end_tests(get_values_n).

:- begin_tests(get_values_k).

test(get_values_k1, [true(Values == [there,hello])]) :-
    get_values_k([example(a,[att1 = hello, att2 = 2]), example(b, [att1 = there, att2 = 3])], att1, Values).

:- end_tests(get_values_k).

:- begin_tests(get_value).

test(get_value1, [true(Value == 2)]) :-
    get_value([att1 = 1, att2 = 2, att3 = 3], att2, Value).

test(get_value2, [true(Value == hi)]) :-
    get_value([att1 = 1, att2 = 2, att3 = hi], att3, Value).

:- end_tests(get_value).

:- begin_tests(best_split).

test(best_split1, [all(BestAttVal == [val1 = 0.5])]) :- % hier muss geschummelt werden, da beim compound term vergleich ein priority clash auftaucht
    best_split([val1 = 0.5, val1 = 0.125], [example(0, [val1 = 0]),example(0, [val1 = 0.25]),example(0, [val1 = 0.75]),example(1, [val1 = 1])], BestAttVal).

:- end_tests(best_split).

:- begin_tests(residual_sum_sq).

test(residual_sum_sq1, [true(ResSumSq == 2)]) :-
    residual_sum_sq([],[0,1,2], ResSumSq).

test(residual_sum_sq2, [true(ResSumSq == 4)]) :-
    residual_sum_sq([1,2,0], [-1,-2,-3], ResSumSq).

:- end_tests(residual_sum_sq).

:- begin_tests(mean).

test(mean1, [true(Res == 2.5)]) :-
    mean([1,2,3,4], Res).

test(mean2, [true(Res == 0)]) :-
    mean([0,0,0,0], Res).

test(mean3, [true(Res == 1.0)]) :-
    mean([0.5,1.5,-1,10,-6], Res).

:- end_tests(mean).

:- begin_tests(residual_split).

test(residual_split1, [true(SumSq == 0)]) :-
    residual_split([], 1, SumSq).

test(residual_split2, [true(SumSq == 2)]) :-
    residual_split([0,1,2], 1, SumSq).

test(residual_split2, [true(SumSq == 93.25)]) :-
    residual_split([-3,1.5,4,10], 2, SumSq).

:- end_tests(residual_split).