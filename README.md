# Macchiato
###### tags: `JPP`
[toc]

## Ogólny opis
Rozwiązanie dzieli się na moduły:
-    **src/Mem** -> reprezentacja pamięci
-    **src/Parsing** -> lexer/parser wygenerowany przez Alex i Happy
-    **src/StaticAnalysis** -> typecheck i inne podobne testy wykonane przed rozpoczęciem wykonywania programu
-    **src/Util** -> funkcje dla wygody

Moduły Interpretation i StaticAnalysis są podobne w strukturze, oba posiadają:
- **{CFG|TC}Traverser.hs**, czyli stos monad transformerów i związane funkcje
- **Err.hs**, zawierający kody błędów
- **MacchiatoVals.hs/MacchiatoTypes.hs**, czyli wewnętrzna reprezentacja danych z AST

## Budowanie
Mamy opcje:
- make, która wykonuje
- make clean



##
Todo
- [ ] var collision instead of func name collision
- [ ] make ssa representation first

