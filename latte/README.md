# MRJP kompilator latte

## Frontend

Rozwiązanie napisałem w języku Haskell. W pliku `Typechecker.hs` znajduje się implementacja frontendu.
Plik `Errors.hs` zawiera wszystkie błędy i informacje o nich, które są wypisywane w przypadku 
niepoprawnych programów.

W typecheckerze używam monady **State** do przechowywania stanu, czyli krotki zawierającej środowisko
(mapę identyfikatorów na informacje o typie), używane zmienne (zbiór wypełniony identyfikatorami 
zmiennych, które można w danym momencie przesłonić), typ obecnie sprawdzanej funkcji oraz informacje o 
tym czy sprawdzana funkcja ma w każdej ścieżce instrukcję `return`.

## Backend
Rozwiązanie to backend do LLVMa napisany w języku Haskell. W pliku `Compiler.hs` znajduje się implementacja.
W katalogu *lib* znajduje się plik `runtime.c` zawierający funkcje biblioteczne oraz `runtime.ll` 
wygenerowany przy pomocy clanga na wydziałowym komputerze. 

Do trzymania stanu używam monady **State**. Stan to krotka zawierająca środowisko (mapę identyfikatorów
na lokacje), skład (mapę lokacji na rejestry), globalne stałe, obecny blok oraz różne liczniki używane np.
do generowania kolejnych rejestrów.

W rozwiązaniu korzystam z funkcji *phi* dzięki czemu nie używam instrukcji *alloca*, *store* ani *load*.

Zaimplementowane rozszerzenia:
- tablice
- struktury
- obiekty

## Uruchamianie

Wystarczy wywołać polecenie `make`. Spowoduje to utworzenie katalogu z parserem oraz pliku wykonywalnego
`latc_llvm` tak jak w poleceniu.

Do czyszczenia `make clean`.

Jeśli program nie jest uruchamiany na studentsie, to w Makefilu trzeba podmienić ścieżkę do `bnfc`.
