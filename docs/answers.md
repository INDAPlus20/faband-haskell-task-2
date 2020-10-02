## Fråga 1

    Utrycket fungerar i Haskell eftersom det går att lagra nästan vad som helst i variabler. Detta inkluderar oändligt långa listor som `[1..]`.

## Fråga 2

    Rekursion innebär att en funktion kallar på sig själv, varpå en loop uppstår. I Haskell så används detta för att kunna hantera loopar, som ej finns i språket till skillnad från många andra språk.

## Fråga 3

    Däreför att JavaScript inte hanterar rekursion likt Haskell. I Haskell så undersöks fallet `sum s 0 = s` innan funktionen blir kallad igen. Inom JavaScript så skulle samma funktion enbart bli körd varpå en kontroll skulle behövas skrivas i funktionen för att undvika problem.