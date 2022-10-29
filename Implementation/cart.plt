
:- use_module(cart).
:- use_module(library(plunit)).


:- begin_tests(check_all_sample).

test(check_all_sample1, [true(Result == [a,a,a])]) :-
    check_all_sample(leaf(a), [[att1=0], [att1=1], [att1=2]], Result).

test(check_all_sample2, [true(Result == [a,a,b])]) :-
    check_all_sample(tree(att1 = 1.5, leaf(a), leaf(b)), [[att1=0], [att1=1], [att1=2]], Result).


test(check_all_sample3, [true(Result == [a,b,c])]) :-
    check_all_sample(tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), tree(att1=2.5, leaf(c), leaf(d))), [[att1=0, att2=0], [att1=1,att2=0], [att1=2,att2=1]], Result).

:- end_tests(check_all_sample).

:- begin_tests(check_sample).

test(check_sample1, [true(Result == a)]) :-
    check_sample(leaf(a), [], Result).

test(check_sample2, [true(Result == a)]) :-
    check_sample(tree(att1 = 1.5, leaf(a), leaf(b)), [att1=1,att2=2], Result).


test(check_sample3, [true(Result == a)]) :-
    check_sample(tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), tree(att1=2.5, leaf(c), leaf(d))), [att1=0,att2=0], Result).

:- end_tests(check_sample).

:- begin_tests(induce_tree).

test(induce_tree1, [true(Tree == leaf(a))]) :-
    induce_tree([example(a, [att1 = 0]), example(a, [att1 = 1]), example(a, [att1 = 2])], [att1], Tree).

test(induce_tree2, [true(Tree == tree(att1 = 1.5, leaf(a), leaf(b)))]) :-
    induce_tree([example(a, [att1 = 0]), example(a, [att1 = 1]), example(b, [att1 = 2])], [att1], Tree).

test(induce_tree3, [true(Tree == tree(att2 = 0.5, leaf(a), leaf(b)))]) :-
    induce_tree([example(a, [att1 = 0, att2 = 0]), example(a, [att1 = 1, att2 = 0]), example(b, [att1 = 1, att2 = 1])], [att1, att2], Tree).

test(induce_tree4, [true(Tree == tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), tree(att1=2.5, leaf(c), leaf(d))))]) :-
    induce_tree([example(a, [att1 = 0, att2 = 0]), example(b, [att1 = 1, att2 = 0]), example(c, [att1 = 2, att2 = 1]), example(d, [att1 = 3, att2 = 1])], [att1, att2], Tree).

test(induce_tree5, [true(Tree == tree(att2 = 0.5, tree(att1=0.5, leaf(a), leaf(b)), leaf(c)))]) :-
    induce_tree([example(a, [att1 = 0, att2 = 0]), example(b, [att1 = 1, att2 = 0]), example(c, [att1 = 2, att2 = 1])], [att1, att2], Tree).

test(induce_tree6, [true(Tree == tree(att3 = 2.5, leaf(a), tree(att1=1, leaf(b), leaf(c))))]) :-
    induce_tree([example(a, [att1 = 0.25, att2 = hi, att3 = 1]), example(b, [att1 = 0, att2 = hi, att3 = 10]), example(c, [att1 = 2, att2 = bye, att3 = 5]), example(a, [att1 = 0.5, att2 = hi, att3 = 2]), example(b, [att1 = -1, att2 = bye, att3 = 3]), example(c, [att1 = 3, att2 = bye, att3 = 7]), example(a, [att1 = 0, att2 = bye, att3 = 1.5])], [att1, att2, att3], Tree).

test(induce_tree7, [true(Tree == tree(att2 = [hi], leaf(a), tree(att1 = 2, leaf(b), leaf(c))))]) :-
    induce_tree([example(a,[att1=1,att2=hi]),example(a,[att1=2,att2=hi]),example(a,[att1=3,att2=hi]),example(b,[att1=0,att2=wie]),example(b,[att1=1,att2=gehts]),example(c,[att1=3,att2=wie]),example(c,[att1=4,att2=gehts])],[att1,att2],Tree).

:- end_tests(induce_tree).

:- begin_tests(check_classes).

test(check_classes1) :-
    check_classes([],_).

test(check_classes2) :-
    check_classes([example(a,[att1=1]),example(a,[att1=2])],a).

test(check_classes3, [fail]) :-
    check_classes([example(a,[att1=1]),example(b,[att1=2])],a).

:- end_tests(check_classes).

:- begin_tests(split).

test(split1_1, [true(Split1 == [example(a, [att = 0]),example(a, [att = 0.25]),example(a, [att = 0.75])])]) :-
    split([example(a, [att = 0]),example(a, [att = 0.25]),example(a, [att = 0.75]),example(b, [att = 1])], att = 0.875, Split1, _).


test(split1_2, [true(Split2 == [example(b, [att = 1])])]) :-
    split([example(a, [att = 0]),example(a, [att = 0.25]),example(a, [att = 0.75]),example(b, [att = 1])], att = 0.875, _, Split2).

:- end_tests(split).

:- begin_tests(find_best_split).

test(find_best_split1, [all(BestAttVal == [att = 0.875])]) :-
    find_best_split([example(a, [att = 0]),example(a, [att = 0.25]),example(a, [att = 0.75]),example(b, [att = 1])], [att], BestAttVal).

test(find_best_split2, [all(BestAttVal == [att1 = 12.5])]) :-
    find_best_split([example(a, [att1 = 10, att2 = 0]),example(b, [att1 = 15, att2 = 1]),example(a, [att1 = 10, att2 = 2])], [att1, att2], BestAttVal).

:- end_tests(find_best_split).

:- begin_tests(get_classes).

test(get_classes0, [true(Klassen == [])]) :-
    get_classes([], Klassen).

test(get_classes1, [true(Klassen == [a,a,a])]) :-
    get_classes([example(a,[]),example(a,[]),example(a,[])], Klassen).

test(get_classes2, [true(Klassen == [b,b,a,a])]) :-
    get_classes([example(b,[att1=a]),example(b,[att1=b]),example(a,[]),example(a,[att1=b,att2=a])], Klassen).

test(get_classes3, [true(Klassen == [a,a,c,a,b,a])]) :-
    get_classes([example(a,[]),example(a,[]),example(c,[]),example(a,[]),example(b,[]),example(a,[])], Klassen).

:- end_tests(get_classes).

:- begin_tests(get_max).

test(get_max1, [true(Res = a)]) :-
    get_max([a], [a,a,a,a], Res).

test(get_max2, [true(Res = a)]) :-
    get_max([a,b], [a,b,a,b,a,a], Res).

test(get_max3, [true(Res = b)]) :-
    get_max([a,b,c], [c,b,b,c,a,b,a,c,b], Res).

:- end_tests(get_max).

:- begin_tests(split_candidates).

test(split_candidates1, [true(Kandidaten == [att2 = 0.5, att2 = 1.5, att1 = [b]])]) :-
    split_candidates([example(a, [att1 = a, att2 = 0]),example(b, [att1 = b, att2 = 1]),example(a, [att1 = b, att2 = 2])], [att1, att2], Kandidaten).

test(split_candidates2, [true(Kandidaten == [att2 = 0.5, att2 = 1.5, att1 = 12.5])]) :-
    split_candidates([example(a, [att1 = 10, att2 = 0]),example(b, [att1 = 15, att2 = 1]),example(a, [att1 = 10, att2 = 2])], [att1, att2], Kandidaten).

:- end_tests(split_candidates).

:- begin_tests(split_candidates_attribute).

test(split_candidates_attribute1, [true(Kandidaten == [att1=0.5, att1=1.5, att1=2.25])]) :-
    split_candidates_attribute([example(a, [att1 = 0, att2 = a]), example(b, [att1 = 1, att2 = b]), example(c, [att1 = 2, att2 = c]), example(a, [att1 = 2.5, att2 = a])], att1, Kandidaten).

test(split_candidates_attribute2, [true(Kandidaten == [att2=[c],att2=[b],att2=[a]])]) :-
    split_candidates_attribute([example(a, [att1 = 0, att2 = a]), example(b, [att1 = 1, att2 = b]), example(c, [att1 = 2, att2 = c]), example(a, [att1 = 2.5, att2 = a])], att2, Kandidaten).

:- end_tests(split_candidates_attribute).

:- begin_tests(check_attribute_number).

test(check_attribute_number1) :-
    check_attribute_number([example(a,[att1=1,att2=hi,att3=3])],att1).

test(check_attribute_number2, [fail]) :-
    check_attribute_number([example(a,[att1=1,att2=hi,att3=3])],att2).

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
    best_split([val1 = 0.5, val1 = 0.125], [example(a, [val1 = 0]),example(a, [val1 = 0.25]),example(a, [val1 = 0.75]),example(b, [val1 = 1])], BestAttVal).

:- end_tests(best_split).

:- begin_tests(impurity).

test(impurity1, [true(CombUnreinheit == 0.25)]) :-
    impurity([example(a, [val1 = 0]),example(a, [val1 = 0.25]),example(a, [val1 = 0.75]),example(b, [val1 = 1])], val1 = 0.5, CombUnreinheit).

test(impurity2, [true(CombUnreinheit == 0.5)]) :-
    impurity([example(a, [val1 = 0, val2 = [hi]]),example(b, [val1 = 0, val2 = [bonjour]]),example(a, [val1 = 1, val2 = [salve]]),example(b, [val1 = 1, val2 = [ola]])], val2 = [hi, bonjour], CombUnreinheit).

test(impurity1, [true(CombUnreinheit == 0.3333333333333333)]) :-
    impurity([example(a, [val1 = 0]),example(a, [val1 = 0.25]),example(a, [val1 = 0.75]),example(b, [val1 = 1])], val1 = 0.125, CombUnreinheit).

:- end_tests(impurity).

:- begin_tests(split_class).

test(split_class1_1, [true(Split1 == [a])]) :-
    split_class([example(a,[val1 = 1, val2 = 1]),example(b,[val1 = 1, val2 = 2]),example(c,[val1 = 1, val2 = 3])], val2 = 2, Split1, _Split2).

test(split_class1_2, [true(Split2 == [c,b])]) :-
    split_class([example(a,[val1 = 1, val2 = 1]),example(b,[val1 = 1, val2 = 2]),example(c,[val1 = 1, val2 = 3])], val2 = 2, _Split1, Split2).

test(split_class2_1, [true(Split1 == [b,a])]) :-
    split_class([example(a,[val1 = none, val2 = 1]),example(b,[val1 = one, val2 = 2]),example(c,[val1 = two, val2 = 3]),example(b,[val1 = three, val2 = 2])], val1 = [none,one], Split1, _Split2).

test(split_class2_2, [true(Split2 == [b,c])]) :-
    split_class([example(a,[val1 = none, val2 = 1]),example(b,[val1 = one, val2 = 2]),example(c,[val1 = two, val2 = 3]),example(b,[val1 = three, val2 = 2])], val1 = [none,one], _Split1, Split2).


:- end_tests(split_class).


:- begin_tests(combgini).

test(combgini1, [true(CombUnreinheit == 0.25)]) :-
    combgini([a,a], [a,b], CombUnreinheit).

test(combgini2, [true(CombUnreinheit == 0.5)]) :-
    combgini([a,b], [a,b], CombUnreinheit).

test(combgini3, [true(CombUnreinheit == 0.5)]) :-
    combgini([a,c,b], [a], CombUnreinheit).

:- end_tests(combgini).

:- begin_tests(gini).

test(gini1, [true(Unreinheit == 0)]) :-
    gini([a,a,a], Unreinheit).

test(gini2, [true(Unreinheit == 0.4444444444444444)]) :-
    gini([b,b,c], Unreinheit).

test(gini3, [true(Unreinheit == 0.6666666666666667)]) :-
    gini([a,b,c], Unreinheit).

test(gini4, [true(Unreinheit == 0.7)]) :-
    gini([a,b,c,c,c,d,d,c,a,d], Unreinheit).

:- end_tests(gini).

:- begin_tests(count_class).

test(count_class_laenge, [true(Laenge == 3)]) :-
    count_class([a,b,c], Laenge, _).

test(count_class_klassen1, [true(Klassen == [a])]) :-
    count_class([a,a,a], _, Klassen).

test(count_class_klassen2, [true(Klassen == [c,b,a])]) :-
    count_class([a,b,c], _, Klassen).


:- end_tests(count_class).

:- begin_tests(part_class).

test(part_class1, [true(KlassenAnteil == [3])]) :-
    part_class([a], [a,a,a], KlassenAnteil).

test(part_class2, [true(KlassenAnteil == [2,1])]) :-
    part_class([a,b], [a,b,a], KlassenAnteil).

test(part_class3, [true(KlassenAnteil == [1,1,1])]) :-
    part_class([a,b,c], [a,b,c], KlassenAnteil).

:- end_tests(part_class).

:- begin_tests(zaehl).

test(zaehl1, [true(Res == 1)]) :-
    zaehl([a,b,c], a, Res).

test(zaehl2, [true(Res == 3)]) :-
    zaehl([a,b,b,b,c], b, Res).

:- end_tests(zaehl).

:- begin_tests(calc_class).

test(calc_class1, [true(Res == 1)]) :-
    calc_class([10], 10, Res).

test(calc_class2, [true(Res == 0.5)]) :-
    calc_class([2,2], 4, Res).

test(calc_class3, [true(Res == 0.3888888888888889)]) :-
    calc_class([3,2,1], 6, Res).

:- end_tests(calc_class).
