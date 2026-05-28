import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "6")
    outputFolder = os.path.join("soluzioni", "output", "6")

    emotes = {
        "SKULL": "💀",
        "CANDY": "🍬",
        "MUSHROOM": "🍄",
        "MIRROR": "🪞",
        "LEFT": "🠔",
        "UP": "🠕 ",
        "RIGHT": "🠖",
        "DOWN": "🠗 ",
        "END": "🏁",
    }

    @staticmethod
    def solve(y: int, x: int, grid: list[list[int]]) -> int:
        def BFS(grid, y, x):
            righe, colonne = len(grid), len(grid[0])
            coda = [(y, x, 0)]
            visitati = set()
            genitori = {}
            direction = [(1, 0), (0, 1), (-1, 0), (0, -1)]
            direction_diag = [(1, 1), (-1, -1), (1, -1), (-1, 1)]
            simboli_speciali = ["🪞", "🍄", "🍬", "🠕 ", "🠗 ", "🠖", "🠔"]

            while coda:
                r, c, dist = coda.pop(0)

                if not (0 <= r < righe and 0 <= c < colonne):
                    continue

                if (r, c) in visitati:
                    continue
                visitati.add((r, c))

                cella = grid[r][c]

                if cella == "🏁":
                    percorso = []
                    simboli = []
                    genitori_percorso = []
                    nodo = (r, c)
                    while nodo != (y, x):
                        percorso.append(nodo)
                        simboli.append(grid[nodo[0]][nodo[1]])
                        genitori_percorso.append((nodo, genitori[nodo]))
                        nodo = genitori[nodo]
                    percorso.append((y, x))
                    simboli.append(grid[y][x])
                    percorso.reverse()
                    simboli.reverse()
                    genitori_percorso.reverse()
                    genitore_nodo = percorso[0]
                    for i in range(len(percorso) - 1):
                        nodo = genitori_percorso[i][1]
                        nodo = genitore_nodo
                    
                    for i in simboli:
                        if i == "🪞":
                            simboli.remove(i)
                        elif i == "🍄":
                            simboli.remove(i)
                        elif i == "🠕 ":
                            simboli.remove(i)
                        elif i == "🠗 ":
                            simboli.remove(i)
                        elif i == "🠖":
                            simboli.remove(i)
                        elif i == "🠔":
                            simboli.remove(i)
                            
                    return len(simboli) 

                vicini = []

                if cella == "🪞":
                    new_r, new_c = c, r
                    if 0 <= new_r < righe and 0 <= new_c < colonne and grid[new_r][new_c] != "💀":
                        if (new_r, new_c) not in visitati:
                            coda.append((new_r, new_c, dist))
                            genitori[(new_r, new_c)] = (r, c)

                if cella == "🠖":
                    nc = colonne - 1
                    if (r, nc) not in visitati:
                        coda.append((r, nc, dist))
                        genitori[(r, nc)] = (r, c)

                if cella == "🠔":
                    nc = 0
                    if (r, nc) not in visitati:
                        coda.append((r, nc, dist))
                        genitori[(r, nc)] = (r, c)

                if cella == "🠗 ":
                    nr = righe - 1
                    if (nr, c) not in visitati:
                        coda.append((nr, c, dist))
                        genitori[(nr, c)] = (r, c)

                if cella == "🠕 ":
                    nr = 0
                    if (nr, c) not in visitati:
                        coda.append((nr, c, dist))
                        genitori[(nr, c)] = (r, c)

                if cella == "--" or cella == "🐹":
                    for dr, dc in direction:
                        nr, nc = r + dr, c + dc
                        if 0 <= nr < righe and 0 <= nc < colonne and grid[nr][nc] != "💀":
                            vicini.append((nr, nc))

                if cella == "🍬":
                    for dr, dc in direction_diag + direction:
                        nr, nc = r + dr, c + dc
                        if 0 <= nr < righe and 0 <= nc < colonne and grid[nr][nc] != "💀":
                            if (nr, nc) not in visitati:
                                vicini.append((nr, nc))

                if cella == "🍄":
                    for dr, dc in direction:
                        nr, nc = r + 2 * dr, c + 2 * dc
                        if 0 <= nr < righe and 0 <= nc < colonne and grid[nr][nc] != "💀":
                            if (nr, nc) not in visitati:
                                coda.append((nr, nc, dist))
                                genitori[(nr, nc)] = (r, c)

                for nr, nc in vicini:
                    if (nr, nc) not in visitati:
                        coda.append((nr, nc, dist + 1))
                        genitori[(nr, nc)] = (r, c)

            return [],[],[],-1  # Nessun traguardo trovato

        return BFS(grid, y, x)
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
            grid = []
            for i, line in enumerate(file):
                if i == 0:
                    startY, startX = line.strip().split(" ")
                else:
                    grid.append(line.strip().split(", "))

        return int(startY), int(startX), grid

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
