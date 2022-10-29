# The CART of Prolog: Entscheidungsbäume mittels logischer Programmierung
Eine Implementierung von CART und Random Forest in puren Prolog, die mit Referenzimplementierungen aus R und Python verglichen wird.
Die Implementationen befinden sich im Ordner `Implementation/`, die ausgeschriebene Bachelorarbeit mit Latex-Skript in `thesis/`.

## Dependencies
__Python__
  * sklearn==1.1.1
  * pandas==1.4.3

__R__
  * rpart==4.1.16
  * randomForest==4.7-1.1
  * tictoc==1.0.1

## Dateien
Zuerst eine kleine Erklärung des Inhalts von `Implementation/`. In dem Ordner `Datensets/` befinden sich die von
`datenbeschaffung.py` erzuegten CSV-Dateien. Das umfasst jeweils 5 Train-Test-Splits zu den Datensätzen Iris, Diabetes, Digits und California Housing, sowie das jeweilige Original.

Der Ordner `Testergebnisse` beinhaltet alle erzeugten Konsolenausgaben. Die Dateiennamen stehen für den benutzten Datensatz und Algorithmus.
Jede Datei beinhaltet die Ausgaben zu allen 5 Train-Test-Splits zu einem Datensatz.
Für SWI- und SICSTus-Prolog fehlen aufgrund zu langer Ausfürhungszeit die Dateien zu `california_tree_...` und  `california_forest_...`
und nur bei SICSTus-Prolog auch `digits_forest_sic.txt`.

Nun zu den Dateien. Es gibt jeweils 2 R und Python Dateien. Diese sind ihrem Namen entsprechend nach den Algorithmus der Implementiert ist unterschieden. SWI- und SICSTus-Prolog haben jeweils 4 Dateien für Algortihmen. Um keine Methode zur Unterscheidung von Regressions und Entscheidungsbäumen machen zu müssen implementiert `cart.pl` für SWI-Prolog Klassifikationsbäume und `cart_regression.pl` Regressionsbäume. Analog für Random Forest und für SICSTus `_sic.pl` Dateien. `classification_test.pl` und  `regression_test.pl` implementieren das Auslesen von CSV-Dateien und rufen die entsprechenden Train-Test-Splits auf, um sie auszuwerten. Für SICSTus-Prolog sind entsprechend `classification_sic.pl` und  `regression_sic.pl` zuständig.

## Benutzung
Um einen Entscheidungsbaum oder Random Forest zu erstellen werden folgende Prädikate verwendet:
```Prolog
% Entscheidungsbaeume
induce_tree(+Daten:list, +Attribute:list, -Tree:term)
% Random Forest
induce_forest(+Daten:list, Attribute:list, N:int,-RandomForest:list)
```
Diese benötigen folgendes Eingabeformat:
```Prolog
Daten = [example(Class, [att1 = value1, att2 = value2,..]), example(...), ...]
Attribute = [att1, att2, ..]
```
Im Fall von Random Forest gibt N die Anzahl zu erzeugenden Bäume an.

Ein erzeugter Baum besteht aus `tree/3` und `leaf/1` Termen.
`tree/3` sind die Entscheidungsknoten und `leaf/1` die Blätter.
`tree/3` hat folgende Form:
```Prolog
tree(Att = Val, LeftSubtree, RightSubtree)
```
* `Att` steht für das Attribut mit dem Aufgeteilt wird
* `Val` steht für den Wert mit den für das entsprechende Attribut aufgeteilt wird
* `LeftSubtree, RightSubtree` stehen für den linken oder rechten Teilbaum. Können wieder `tree/3` oder `leaf/1` sein

```Prolog
leaf(Class)
```
* `Class` steht für die Klasse, die bei dem erreichen des Blatts ausgegeben wird

Ein Random Forest ist eine Liste aus N vielen Bäumen.

Um eine Vorhersage zu einem oder mehreren Datenpunkte zu machen werden folgende Prädikate verwendet:
```Prolog
% Entscheidungsbaeume
check_sample(+Tree:Entscheidungsbaum, +Example:list, -Res:Class) 
check_all_sample(+Tree:Entscheidungsbaum, +Examples:list, -Res:list)
% Random Forest
check_sample_forest(+RandomForest:list, +Example:list, -Res:atom)
check_all_sample_forest(+Tree:Entscheidungswald, +Examples:list, -Res:list)
```
* `Tree`/`RandomForest` steht für einen Entscheidungsbaum, bzw. Random Forest
* `Example`/`Examples` steht für einen Datenpunkt in dem Format `[att1=val1,att2=val2,...]`, bzw. eine Liste davon.
* `Res` wird der Vorhersage, bzw. einer Liste von Vorhersagen unifiziert.


