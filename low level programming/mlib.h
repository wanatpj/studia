/*
* stos ciągów byte'ów
* wszystkie funkcje zwracają -1 w przypadku gdy operacja zakończyła się niepowodzeniem
* funkcje push, pop zwracają 0 jeśli operacja zakończyła się sukcesem 
* w przypadku sukcesu zmienna sstatus jest ustawiana na 0
* w przypadku porażki zmienna sstatus jest ustawiana na 
* 1 	- jeśli ostatnią operacją była próba odczytania z pustego stosu
* 2	- jeśli parametry funkcji były niepoprawne
* 3	- jeśli nie udało się zaalokować pamięci
*/


/* dokłada na szczyt stosu ciąg byte'ów o długości length zaczynający się od adresu buf
* ciąg byte'ów powinien być skopiowany
*/
int push(long length, char * buf);

/* zwraca długość ciągu byte'ów znajdującego się na szczycie stosu */
int top_length();

/* usuwa ciąg byte'ów ze szczytu stosu i kopiuje go do bufora wskazanego przez buf */
int pop(char * buf);

/* zwraca liczbę elementów (ciągów byte'ów) na stosie */
int count();

extern int sstatus;  // status ostatniej operacji 

