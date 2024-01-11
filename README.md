# Macchiato

###### tags: `JPP`

[toc]

## Ogólny opis

Rozwiązanie dzieli się na moduły:

- **src/Mem** -> reprezentacja pamięci
- **src/Parsing** -> lexer/parser wygenerowany przez Alex i Happy
- **src/StaticAnalysis** -> typecheck i inne podobne testy wykonane przed rozpoczęciem wykonywania programu
- **src/Util** -> funkcje dla wygody

Moduły Interpretation i StaticAnalysis są podobne w strukturze, oba posiadają:

- **{CFG|TC}Traverser.hs**, czyli stos monad transformerów i związane funkcje
- **Err.hs**, zawierający kody błędów
- **MacchiatoVals.hs/MacchiatoTypes.hs**, czyli wewnętrzna reprezentacja danych z AST

## Budowanie

Mamy opcje:

- make
- make clean

## Ustalnie dodatkowe

- max i min int zaleza od max i min int w haskellu na danej arhcitekturze, wiec zwykle tak jak c++ dziala. Opcja ta jest w Staticanalysis/Params.hs
- Jest tam tez gloebokosc propagacji staych, narazie bez porpagacji ale sprawdza arguenty wiec znajdzie np `f(i%0);`

- Gramatyka nie akceptuje instrukcji typu`int a = - -5;`, jes ot umyslne tak samo dla negacji logicznej
- infinite loop to undefined behaviour
  - wiec jesli jest jakis osiaglany return w srodku to zakladamy ze kiedys sie skonczy petla
- program nie sprawdza narazie integer overflow
- program nie zwraca unused variables jako blad
