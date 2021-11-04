# MRJP kompilatory Instant

## Rozwiązanie
Rozwiązanie napisałem w języku Haskell. W pliku `InscJVM.hs` znajduje się implementacja 
kompilatora do **JVM**, a w pliku `InscLLVM.hs` do **LLVM**. Oba te pliki wraz z dostarczonym 
plikiem `Instant.cf` znajdują się w katalogu *src*. W katalogu *lib* znajduje się plik 
`jasmin.jar`.

## Uruchamianie
Wystarczy wywołać polecenie `make`. Spowoduje to utworzenie katalogu z parserem oraz plików
`insc_jvm` i `insc_llvm` tak jak w poleceniu.

Do czyszczenia `make clean`.

Jeśli program nie jest uruchamiany na studentsie, to w Makefileu trzeba podmienić scieżkę do 
*bnfc*.
