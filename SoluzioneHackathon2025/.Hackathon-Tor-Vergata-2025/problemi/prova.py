def solve(y: int, x: int, grid: list[list[int]]) -> tuple[list[tuple[int, int]], list[str], list[tuple[tuple[int, int], tuple[int, int]]]]:
    
    def BFS(grid, y, x):
        righe, colonne = len(grid), len(grid[0])
        coda = [(y, x, 0)]
        visitati = set()
        genitori = {}
        direction = [(1, 0), (0, 1), (-1, 0), (0, -1)]
        direction_diag = [(1, 1), (-1, -1), (1, -1), (-1, 1)]

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
                    print("nodo", nodo, end=" ")

                    print("genitore_nodo", genitore_nodo, end=" ")
                    print("distanza", dist)
                    nodo = genitore_nodo
                
                for i in simboli:
                    if i == "🪞":
                        simboli.remove(i)
                    elif i == "🍄":
                        simboli.remove(i)
                    elif i == "🍬":
                        simboli.remove(i)
                    elif i == "🠕 ":
                        simboli.remove(i)
                    elif i == "🠗 ":
                        simboli.remove(i)
                    elif i == "🠖":
                        simboli.remove(i)
                    elif i == "🠔":
                        simboli.remove(i)
                        
                return percorso, simboli, genitori_percorso, len(simboli) 

            vicini = []

            if cella == "🪞":
                new_r, new_c = c, r
                if 0 <= new_r < righe and 0 <= new_c < colonne and grid[new_r][new_c] != "💀":
                    if (new_r, new_c) not in visitati:
                        coda.append((new_r, new_c, dist-1))
                        genitori[(new_r, new_c)] = (r, c)

            if cella == "🠖":
                nc = colonne - 1
                if (r, nc) not in visitati:
                    coda.append((r, nc, dist-1))
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

def parse_testcase(path: str):
    with open(path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    startY, startX = map(int, lines[0].strip().split())
    grid = [line.strip().split(", ") for line in lines[1:] if line.strip()]
    return startY, startX, grid

def stampa_risultato(risultato):
    percorso, simboli, genitori_percorso, distanza = risultato
    print("--------------------------------------------------")
    print("Percorso:", percorso)
    print("Simboli:", simboli)
    print("Genitori del percorso:", genitori_percorso)
    print("Distanza:", distanza)
    numero_passi = 0
    numero_specchi = 0
    numero_funghetti = 0
    numero_candies = 0
    numero_frecce_su = 0
    numero_frecce_giu = 0
    numero_frecce_destra = 0
    numero_frecce_sinistra = 0
    for i in range(len(simboli)):
        if result[1][i] == "🪞":
            numero_specchi += 1
        elif result[1][i] == "🍄":
            numero_funghetti += 1
        elif result[1][i] == "🍬":
            numero_candies += 1
        elif result[1][i] == "🠕 ":
            numero_frecce_su += 1
        elif result[1][i] == "🠗 ":
            numero_frecce_giu += 1
        elif result[1][i] == "🠖":
            numero_frecce_destra += 1
        elif result[1][i] == "🠔":
            numero_frecce_sinistra += 1
        elif result[1][i] == "--":
            numero_passi += 1
    print("--:", numero_passi)
    print("🪞", numero_specchi)
    print("🍄", numero_funghetti)
    print("🍬", numero_candies)
    print("🠕 ", numero_frecce_su)
    print("🠗 ", numero_frecce_giu)
    print("🠖", numero_frecce_destra)
    print("🠔", numero_frecce_sinistra)

startY, startX, grid = parse_testcase("soluzioni/input/6/testcase2.txt")
result = solve(startY, startX, grid)
stampa_risultato(result)
