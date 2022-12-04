# Macchiato
###### tags: `JPP`
[toc]

## Ogólny opis
Rozwiązanie dzieli się na moduły:
-    **src/Interpretation** -> rzeczy związane z wykonywaniem kodu
-    **src/Mem** -> reprezentacja pamięci
-    **src/Parsing** -> lexer/parser wygenerowany przez Alex i Happy
-    **src/StaticAnalysis** -> typecheck i inne podobne testy wykonane przed rozpoczęciem wykonywania programu
-    **src/Util** -> funkcje dla wygody

Moduły Interpretation i StaticAnalysis są podobne w strukturze, oba posiadają:
- **Traverser.hs**, czyli stos monad transformerów i związane funkcje
- **Err.hs**, zawierający kody błędów
- **MacchiatoVals.hs/MacchiatoTypes.hs**, czyli wewnętrzna reprezentacja danych z AST

## Budowanie
Mamy opcje:
- make, która wykonuje
    - make update -> cabal update
    - make build -> cabal build
    - make install -> cabal install ...
- make clean
    - cabal clean + rm interpreter

## Uwagi
Interpreter spełnia wszystkie wymagania na zadeklarowane 29 punktów. Gdybym go pisał od nowa to dodałbym ReaderT do Traverser w StaticAnalysis zamiast podawać dodatkowy parametr po wszystkich metodach bo się przydaję rzadziej niż myślałem

### Zmiany
Nie obyło się bez paru zmian od tego co było w deklaracji języka. Są one następujące:
- Dodałem *break* i *continue* do gramatyki bo zapomniałem o nich wcześniej
- Ident nie był zgody z zadeklarowanym formatem identyfikatorów więc zmieniłem go na własny UIdent
- Zmieniłem nazwę boolean na bool bo się ciągle myliłem
- Zmieniłem parę nazw aby uniknąć konfliktów, rzeczy w stylu *Program. Program*
- Zmieniłem produkcję nawiasów kwadratowych do tablic bo stara zmuszała do mało czytelnych przypadków podczas interpretacji

### Konflikty w gramatyce
Widać je w src/Parsing/Parmacchiato.info.
- Jak mamy "[]" to gramatyka idzie do przodu i sprawdza czy jest wiecej nawiasów zamiast kończyć z jednym, co jest pożądanym efektem (State 11).
-  Gdy po if jest else to rezolucja zakłada, że mamy statement typu if else a nie if i potem coś innego, co jest pożądanym efektem (State 144).

### Misc
- Definicja funkcji w stylu *int foo(int foo)* jest poprawna i powoduję shadowing (zakrywanie?)
- Stack pamięci jest ograniczony przez Int nie Integer, ale nie da się go sztucznie zapychać rekurencją więc wydaje mi się że Haskell będzie miał stack overflow zanim to się stanie.
- Jeśli ktoś zadeklaruje lub zrobi access w więcej niż INT_MAX wymiarach, czego nie sprawdzam bo uznałem że to zbyt daleki edge case.

