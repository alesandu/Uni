import os
import pprint as pp


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "3")
    outputFolder = os.path.join("soluzioni", "output", "3")

    @staticmethod
    def solve(matrix: list[list[int]], k: int, start: str) -> int:
        def visitaDFS(matrix, rows, columns, i, j, k):
            count = 0
            directions = [
                (-1, -1),  # diagonale su-sinistra
                (-1, 0),   # su
                (-1, +1),  # diagonale su-destra
                (0, -1),   # sinistra
                (0, +1),   # destra
                (+1, -1),  # diagonale giù-sinistra
                (+1, 0),   # giù
                (+1, +1),  # diagonale giù-destra
            ]

            for dx, dy in directions:
                string = visitaDFSex(matrix, i, j, dx, dy, rows, columns, k)
                if string is not None:
                    count += 1
            return count

        def visitaDFSex(matrix, i, j, dx, dy, rows, columns, k):
            string = ""
            while 0 <= i < rows and 0 <= j < columns and len(string) < k:
                string += str(matrix[i][j])
                i += dx
                j += dy
            if len(string) == k:
                if string == string[::-1]:
                    return string
                else:
                    return None
                
            return None
        count = 0
        rows = len(matrix)
        columns = len(matrix[0])
        for i in range(rows):
            for j in range(columns):
                if matrix[i][j] == start:
                    count += visitaDFS(matrix, rows, columns, i, j, k)
        return count//2
        pass

    @staticmethod
    def loadInput(i: int) -> str:
        """
        Carica il file di input i-esimo, contenuto all'interno della cartella dei file input.
        Questo metodo deve restituire il valore da passare al metodo solve.
        """
        files = os.listdir(Solution.inputFolder)
        files.sort()

        matrix = []
        with open(os.path.join(Solution.inputFolder, files[i])) as file:
            for i, line in enumerate(file):
                if i == 0:
                    len, key = line.strip().split(",")
                else:
                    newLine = []
                    for char in line.strip():
                        newLine.append(char)
                    matrix.append(newLine)
        return matrix, int(len), key

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
                value += line.strip()
        return int(value)
