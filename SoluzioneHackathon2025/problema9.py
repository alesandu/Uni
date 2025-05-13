import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "9")
    outputFolder = os.path.join("soluzioni", "output", "9")

    @staticmethod
    def solve(costs: list[int], k: int):
        n = len(costs) + 1  # numero di interruttori
        dp = [[[float('-inf')] * 2 for _ in range(k + 1)] for _ in range(n)]

        # base case: primo interruttore abbassato o alzato
        dp[0][0][0] = 0
        if k > 0:
            dp[0][1][1] = 0

        for i in range(1, n):
            for switches_up in range(k + 1):
                for curr_state in [0, 1]:  # 0 = basso, 1 = alto
                    for prev_state in [0, 1]:
                        if curr_state == 1 and switches_up == 0:
                            continue  # non posso alzare senza slot
                        prev_k = switches_up - curr_state
                        if prev_k < 0:
                            continue
                        gain = costs[i - 1] if curr_state != prev_state else 0
                        dp[i][switches_up][curr_state] = max(
                            dp[i][switches_up][curr_state],
                            dp[i - 1][prev_k][prev_state] + gain
                        )
        return max(dp[n - 1][switches_up][state] for switches_up in range(k + 1) for state in [0, 1])
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
            maxColor = int(lines[0].strip())
            costs = list(map(int, lines[1].strip().split(", ")))
        return costs, maxColor

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
