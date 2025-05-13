import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "7")
    outputFolder = os.path.join("soluzioni", "output", "7")

    @staticmethod
    def solve(wagons: list[tuple[int, int]]) -> int:
        n = len(wagons)
        
        # Costo minimo per combinare i vagoni, inizializziamo la matrice con valori infiniti
        dp = [[float('inf')] * n for _ in range(n)]
        
        # Per ogni coppia di vagoni, calcoliamo il costo di combinazione
        for i in range(n):
            dp[i][i] = 0  # Costo di combinare un singolo vagone è zero
        
        # Per le combinazioni di più di un vagone
        for length in range(2, n+1):  # lunghezza della combinazione (da 2 vagoni a n vagoni)
            for i in range(n - length + 1):
                j = i + length - 1  # il vagone finale della combinazione
                # Calcoliamo il costo minimo per combinare i vagoni da i a j
                for k in range(i, j):
                    # Il costo di combinare il segmento [i, k] e [k+1, j]
                    cost = dp[i][k] + dp[k+1][j] + wagons[i][0] * wagons[k][1] * wagons[j][1]
                    dp[i][j] = min(dp[i][j], cost)
        
        # Il costo minimo per combinare tutti i vagoni sarà memorizzato in dp[0][n-1]
        return dp[0][n-1]
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
            input = []
            for line in file.readlines():
                temp = line.strip().split(", ")
                input.append(tuple(map(int, temp)))
        return input

    @staticmethod
    def loadOutput(i: int) -> str:
        """
        Carica il file di output i-esimo, contenuto all'interno della cartella dei file output.
        Questo metodo deve restituire il valore presente nel file di output.
        """
        files = os.listdir(Solution.outputFolder)
        files.sort()

        with open(os.path.join(Solution.outputFolder, files[i])) as file:
            for line in file:
                return int(line.strip())
