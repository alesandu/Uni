import os
import re


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "2")
    outputFolder = os.path.join("soluzioni", "output", "2")
    
    @staticmethod
    def solve(input: str) -> int:
        result = 0
        i = 0
        while i < len(input):
            # Trova la posizione di una delle parole chiave (sum, mul, con)
            if input[i:i+3] == 'sum':
                # Trova la parentesi aperta
                start = i + 4  # La posizione subito dopo 'sum('
                end = input.find(')', start)  # Trova la parentesi chiusa
                digits = input[start:end]  # Estrai il numero tra parentesi
                if digits:  # Verifica che non sia una stringa vuota
                    result += sum(int(digit) for digit in digits)  # Somma le cifre
                i = end + 1  # Vai alla posizione successiva
            elif input[i:i+3] == 'mul':
                start = i + 4
                end = input.find(')', start)
                digits = input[start:end]
                if digits:  # Verifica che non sia una stringa vuota
                    product = 1
                    for digit in digits:
                        product *= int(digit)  # Moltiplica le cifre
                    result += product  # Aggiungi il risultato della moltiplicazione
                i = end + 1
            elif input[i:i+3] == 'con':
                start = i + 4
                end = input.find(')', start)
                digits = input[start:end]
                if digits:  # Verifica che non sia una stringa vuota
                    result += int(digits)  # Concatenazione delle cifre come numero
                i = end + 1
            else:
                i += 1  # Passa al prossimo carattere

        return result
        pass

    @staticmethod
    def loadInput(i: int) -> str:
        """
        Carica il file di input i-esimo, contenuto all'interno della cartella dei file input.
        Questo metodo deve restituire il valore da passare al metodo solve.
        """
        files = os.listdir(Solution.inputFolder)
        files.sort()

        with open(os.path.join(Solution.inputFolder, files[i])) as file:
            string = ""
            for line in file:
                string += str(line.strip())
        return string

    @staticmethod
    def loadOutput(i: int) -> int:
        """
        Carica il file di output i-esimo, contenuto all'interno della cartella dei file output.
        Questo metodo deve restituire il valore presente nel file di output.
        """
        files = os.listdir(Solution.outputFolder)
        files.sort()

        with open(os.path.join(Solution.outputFolder, files[i])) as file:
            value = ""
            for line in file:
                value += line
        return int(value)
