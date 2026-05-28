import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "0")
    outputFolder = os.path.join("soluzioni", "output", "0")

    @staticmethod
    def solve(n: int, k: int) -> int:
        distanza = n - k
        if distanza == 1:
            return 1
        elif distanza == 0:
            return 0
        else:
            return (n + k + 1) * distanza // 2
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
            line = file.readline()
            line = line.split(",")
            n, k = int(line[0]), int(line[1])

        return n, k

    @staticmethod
    def loadOutput(i: int) -> int:
        """
        Carica il file di output i-esimo, contenuto all'interno della cartella dei file output.
        Questo metodo deve restituire il valore presente nel file di output.
        """
        files = os.listdir(Solution.outputFolder)
        files.sort()

        with open(os.path.join(Solution.outputFolder, files[i])) as file:
            line = file.readline()
        return int(line)
