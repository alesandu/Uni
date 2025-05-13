import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "8")
    outputFolder = os.path.join("soluzioni", "output", "8")

    @staticmethod
    def solve(firstStack: list[int], secondStack: list[int], maxWeight: int) -> tuple[int, int]:
        A = [0]
        for x in firstStack:
            A.append(A[-1] + x)

        C = [0]
        for x in secondStack:
            C.append(C[-1] + x)

        maxCost = 0
        maxIndex = (0, 0)

        j = len(C) - 1

        # Per ogni prefisso in A, scorro C all’indietro finché la somma è accettabile
        for i in range(len(A)):
            while j >= 0 and A[i] + C[j] > maxWeight:
                j -= 1
            if j >= 0:
                total = A[i] + C[j]
                if total > maxCost:
                    maxCost = total
                    if i == 0 or i == len(A):
                        maxIndex = (i, j - 1)
                    else:
                        maxIndex = (i - 1, j - 1)

        return maxIndex
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
            lines = file.readlines()
            maxWeight = int(lines[0].strip())
            first = list(map(int, lines[1].strip().split(", ")))
            second = list(map(int, lines[2].strip().split(", ")))
        return first, second, maxWeight

    @staticmethod
    def loadOutput(i: int) -> int:
        """
        Carica il file di output i-esimo, contenuto all'interno della cartella dei file output.
        Questo metodo deve restituire il valore presente nel file di output.
        """
        files = os.listdir(Solution.outputFolder)
        files.sort()

        with open(os.path.join(Solution.outputFolder, files[i])) as file:
            return int(file.readline().strip())

    @staticmethod
    def evaluateSolution(
        firstStack: list[int],
        secondStack: list[int],
        maxWeight: int,
    ) -> int:

        n = len(firstStack)
        m = len(secondStack)

        firstSum = []
        newFirst = 0
        for t in range(n):
            newFirst += firstStack[t]
            firstSum.append(newFirst)

        secondSum = []
        newSecond = 0
        for k in range(m):
            newSecond += secondStack[k]
            secondSum.append(newSecond)

        i, j = Solution.solve(firstStack, secondStack, maxWeight)
        output = 0
        if i != -1:
            output += firstSum[i]
        if j != -1:
            output += secondSum[j]

        return output
