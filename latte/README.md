# MRJP kompilator latte

## Frontend

Rozwiązanie napisałem w języku Haskell. W pliku `Typechecker.hs` znajduje się implementacja frontendu.
Plik `Errors.hs` zawiera wszystkie błędy i informacje o nich, które są wypisywane w przypadku 
niepoprawnych programów.

W typecheckerze używam monady **State** do przechowywania stanu, czyli krotki zawierającej środowisko
(mapę identyfikatorów na informacje o typie), używane zmienne (zbiór wypełniony identyfikatorami 
zmiennych, które można w danym momencie przesłonić), typ obecnie sprawdzanej funkcji oraz informacje o 
tym czy sprawdzana funkcja ma w każdej ścieżce instrukcję `return`.

## Uruchamianie

Wystarczy wywołać polecenie `make`. Spowoduje to utworzenie katalogu z parserem oraz pliku wykonywalnego
`latc` tak jak w poleceniu.

Do czyszczenia `make clean`.

Jeśli program nie jest uruchamiany na studentsie, to w Makefilu trzeba podmienić ścieżkę do `bnfc`.
