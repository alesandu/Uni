# from hashlib import sha256
# import hashlib
# nonce = "16114492071"
# text = "Francesco " + nonce
# print(sha256(text.encode("utf8")).hexdigest())

# for (nonce) in range(100000000000):
#     text = "Ale " + str(nonce)
#     current_hash = hashlib.sha256(text.encode("utf8")).hexdigest()
#     if current_hash.startswith("00000000"):
#          print("Found nonce: " + str(nonce))
#          print("Hash: " + current_hash)
#          break
# print("Done")

import hashlib
from multiprocessing import Pool, cpu_count

def check_nonce(range_start, range_end, target_prefix="00000000"):
    for nonce in range(range_start, range_end):
        text = f"Ale {nonce}"
        current_hash = hashlib.sha256(text.encode("utf-8")).hexdigest()
        
        if current_hash.startswith(target_prefix):
            return nonce, current_hash
    return None

import hashlib
from multiprocessing import Pool, cpu_count

def cerca_nonce(range_dati):
    """
    Funzione eseguita in parallelo su ogni core.
    Riceve un range (inizio, fine) per evitare sovrapposizioni.
    """
    inizio, fine = range_dati
    prefisso_ricercato = "000000000"
    
    for n in range(inizio, fine):
        # L'uso di f-string è il metodo più rapido in Python 3.10+
        testo = f"Ale {n}".encode("utf-8")
        h = hashlib.sha256(testo).hexdigest()
        
        if h.startswith(prefisso_ricercato):
            return n, h
    return None

def main():
    core_totali = cpu_count()-1
    dimensione_blocco = 500000  # Numero di tentativi per ogni processo prima di aggiornare
    punto_partenza = 0
    
    print(f"Ricerca avviata utilizzando {core_totali} processi in parallelo...")

    with Pool(processes=core_totali) as pool:
        while True:
            # Prepariamo i segmenti di lavoro per i core
            segmenti = [
                (punto_partenza + i * dimensione_blocco, punto_partenza + (i + 1) * dimensione_blocco)
                for i in range(core_totali)
            ]
            
            # Distribuiamo il lavoro sui core
            risultati = pool.map(cerca_nonce, segmenti)
            
            # Controlliamo se uno dei processi ha trovato la soluzione
            for r in risultati:
                if r:
                    print(f"\nSoluzione trovata!")
                    print(f"Nonce: {r[0]}")
                    print(f"Hash: {r[1]}")
                    return

            punto_partenza += dimensione_blocco * core_totali
            print(f"Controllati: {punto_partenza}", end="\r")

if __name__ == "__main__":
    main()