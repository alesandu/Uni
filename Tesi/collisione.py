import hashlib
import random
import numpy as np
import matplotlib.pyplot as plt
from typing import List
from tqdm import tqdm
from scipy.stats import chi2
import gmpy2
from gmpy2 import mpz,powmod
import multiprocessing
from multiprocessing import Pool 
import csv 
import os
# ------------------------------
# Implementazione Poseidon256 (basata sul paper)
# ------------------------------
PRIME = mpz(2**256 - 2**32 - 977)  # Primo vicino a 2^256 (usato in Ethereum)
T = 3  # Dimensione stato (2 input, 1 capacità)
ROUNDS_F = 8  # Round completi
ROUNDS_P = 56  # Round parziali
ALPHA = 5  # Esponente S-box

def generate_round_constants(prime: mpz, total_constants: int) -> List[mpz]:
    constants = []
    for i in range(total_constants):
        # Seed deterministico: "PoseidonRoundConstant" + indice
        data = f"PoseidonRoundConstant{i}".encode()
        digest = hashlib.sha256(data).digest()
        value = int.from_bytes(digest, byteorder='big') % prime
        constants.append(mpz(value))
    return constants

def generate_mds_matrix(prime: mpz, t: int) -> List[List[mpz]]:
    x = [mpz(i + 1) for i in range(t)]  # valori distinti arbitrari
    mds = []
    for i in range(t):
        row = []
        for j in range(t):
            if i == j:
                # Diagonale: somma dei x_i / (x_i - x_k) per k ≠ i
                s = mpz(0)
                for k in range(t):
                    if k != i:
                        denom = (x[i] - x[k]) % prime
                        inv_denom = gmpy2.invert(denom, prime)
                        s = (s + x[i] * inv_denom) % prime
                row.append(s)
            else:
                denom = (x[i] - x[j]) % prime
                inv_denom = gmpy2.invert(denom, prime)
                row.append(inv_denom)
        mds.append(row)
    return mds

# Costanti precalcolate
ROUND_CONSTANTS = generate_round_constants(PRIME, (ROUNDS_F + ROUNDS_P) * T)
MDS_MATRIX = generate_mds_matrix(PRIME, T)

def add(a: int, b: int) -> int:
    return (a + b) % PRIME

def mul(a: int, b: int) -> int:
    return (a * b) % PRIME

def poseidon_permutation(state: List[int]) -> List[int]:
    """
    Permutazione Poseidon con round completi e parziali.
    """
    # Round completi inizialic
    for r in range(ROUNDS_F // 2):
        # Add round constants
        for i in range(T):
            state[i] = (state[i] + ROUND_CONSTANTS[r * T + i]) % PRIME
        
        # S-box su tutti gli elementi
        for i in range(T):
            state[i] = gmpy2.powmod(state[i], ALPHA, PRIME)

        # MDS matrix multiply
        new_state = [mpz(0), mpz(0), mpz(0)]
        for i in range(T):
            acc = mpz(0)
            for j in range(T):
                acc += MDS_MATRIX[i][j] * state[j]
            new_state[i] = acc % PRIME
        state = new_state

    # Round parziali
    for r in range(ROUNDS_P):
        for i in range(T):
            state[i] = (state[i] + ROUND_CONSTANTS[(ROUNDS_F // 2 + r) * T + i]) % PRIME
        
        # Solo il primo elemento passa per S-box
        state[0] = gmpy2.powmod(state[0], ALPHA, PRIME)

        # MDS matrix multiply
        new_state = [mpz(0), mpz(0), mpz(0)]
        for i in range(T):
            acc = mpz(0)
            for j in range(T):
                acc += MDS_MATRIX[i][j] * state[j]
            new_state[i] = acc % PRIME
        state = new_state

    # Round completi finali
    for r in range(ROUNDS_F // 2, ROUNDS_F):
        for i in range(T):
            state[i] = (state[i] + ROUND_CONSTANTS[r * T + i]) % PRIME
        
        for i in range(T):
            state[i] = gmpy2.powmod(state[i], ALPHA, PRIME)

        new_state = [mpz(0), mpz(0), mpz(0)]
        for i in range(T):
            acc = mpz(0)
            for j in range(T):
                acc += MDS_MATRIX[i][j] * state[j]
            new_state[i] = acc % PRIME
        state = new_state

    return state

def poseidon256(msg: bytes) -> bytes:
    """
    Hash Poseidon256 basato su sponge per messaggi di lunghezza arbitraria.
    Usa rate=2 elementi (32 byte) e capacità=1 elemento.
    """
    # Padding: multi-rate padding standard (pad con '80...00')
    pad_len = (32 - (len(msg) % 32)) % 32
    if pad_len == 0:
        pad_len = 32
    msg_padded = msg + b'\x80' + b'\x00' * (pad_len - 1)

    # Stato iniziale: 2 input + capacità
    state = [mpz(0), mpz(0), mpz(0)]

    # Assorbimento blocco per blocco
    for i in range(0, len(msg_padded), 32):
        block = msg_padded[i:i+32]
        x = mpz(int.from_bytes(block[:16], 'big')) % PRIME
        y = mpz(int.from_bytes(block[16:32], 'big')) % PRIME

        # XOR nel rate
        state[0] = (state[0] + x) % PRIME
        state[1] = (state[1] + y) % PRIME

        # Permutazione
        state = poseidon_permutation(state)

    # Squeeze: output = primo elemento come digest
    return state[0].to_bytes(32, 'big')

# ------------------------------
# Configurazione
# ------------------------------
NUM_SAMPLES = 1000000  # sample
INPUT_SIZE = 1024    # 1KB per input
RANDOM_SEED = 42      # Per riproducibilità

# ------------------------------
# Funzioni di hash (sostituisci hash_poseidon con la tua implementazione reale)
# ------------------------------

def hash_md5(msg):
    return hashlib.md5(msg).digest()

def hash_sha256(msg):
    return hashlib.sha256(msg).digest()

def hash_poseidon(msg: bytes) -> bytes:
    return poseidon256(msg)

# ------------------------------
# Generazione input condivisi 
# ------------------------------

random.seed(RANDOM_SEED)
shared_inputs = [bytes([random.getrandbits(8) for _ in range(INPUT_SIZE)]) for _ in range(NUM_SAMPLES)]
msg_to_index = {msg: idx for idx, msg in enumerate(shared_inputs)}

def worker(msg_chunk_hash_func):
    # msg_chunk_hash_func can be either:
    # (msg_chunk, hash_func) for basic hash functions
    # OR
    # (msg_chunk, hash_func, n_bits) for n-bit hash functions
    if len(msg_chunk_hash_func) == 2:
        msg_chunk, hash_func = msg_chunk_hash_func
        return [hash_func(msg) for msg in msg_chunk]
    else:
        msg_chunk, hash_func, n_bits = msg_chunk_hash_func
        return [hash_func(msg, n_bits) for msg in msg_chunk]

def parallel_hash(hash_func, inputs, n_bits=None, n_cores=16, chunks=160):
    chunk_size = (len(inputs) + chunks - 1) // chunks
    chunks_list = [inputs[i*chunk_size:(i+1)*chunk_size] for i in range(chunks)]

    results = []
    with multiprocessing.Pool(processes=n_cores) as pool:
        with tqdm(total=len(chunks_list), desc=f"Precalcolo {hash_func.__name__}") as pbar:
            # Prepare arguments based on whether n_bits is needed
            if n_bits is None:
                args = [(chunk, hash_func) for chunk in chunks_list]
            else:
                args = [(chunk, hash_func, n_bits) for chunk in chunks_list]
            
            for res in pool.imap_unordered(worker, args, chunksize=1):
                results.append(res)
                pbar.update()

    return [h for sublist in results for h in sublist]

output_dir = "hash_outputs"
os.makedirs(output_dir, exist_ok=True)

import os
import csv

precomputed_hashes = {}

# ================================
# Funzioni Poseidon n-bit dinamiche
# ================================
def poseidon_n(msg: bytes, n_bits: int) -> bytes:
    n_bytes = (n_bits + 7) // 8  # Arrotonda verso l'alto
    mask = (1 << n_bits) - 1
    
    idx = msg_to_index[msg]
    full_hash = precomputed_hashes["hash_poseidon"][idx]
    needed_bytes = full_hash[:n_bytes]
    value = int.from_bytes(needed_bytes, 'big') & mask
    return value.to_bytes(n_bytes, 'big')

# ================================
# Funzioni SHA256 n-bit dinamiche (top-level)
# ================================
def sha256_n(msg: bytes, n_bits: int) -> bytes:
    n_bytes = (n_bits + 7) // 8
    mask = (1 << n_bits) - 1
    
    idx = msg_to_index[msg]
    full_hash = precomputed_hashes["hash_sha256"][idx]
    needed_bytes = full_hash[:n_bytes]
    value = int.from_bytes(needed_bytes, 'big') & mask
    return value.to_bytes(n_bytes, 'big')


# ================================
# Precalcolo hash completi
# ================================

precomputed_hashes = {}

# for hash_func in [hash_md5, hash_sha256, hash_poseidon]:
#     print(f"Precalcolo {hash_func.__name__} (parallelizzato)...")
#     hashes = parallel_hash(hash_func, shared_inputs, n_cores=16)
#     precomputed_hashes[hash_func.__name__] = hashes
    
    
def load_precomputed_hashes(output_dir):
    precomputed_hashes = {}
    
    # Carica gli hash completi dai CSV
    for hash_name in ["hash_md5", "hash_sha256", "hash_poseidon"]:
        csv_filename = os.path.join(output_dir, f"{hash_name}_hashes.csv")
        
        if not os.path.exists(csv_filename):
            raise FileNotFoundError(f"File {csv_filename} non trovato.")
        
        print(f"Caricamento {hash_name} da file CSV...")
        hashes = []
        
        with open(csv_filename, 'r', newline='') as csvfile:
            reader = csv.reader(csvfile)
            next(reader)  # Salta intestazione
            
            for row in reader:
                hash_hex = row[1]
                hash_bytes = bytes.fromhex(hash_hex)
                hashes.append(hash_bytes)
        
        precomputed_hashes[hash_name] = hashes
    
    return precomputed_hashes

# Utilizzo:
output_dir = "hash_outputs/"  # Sostituisci con il percorso corretto
precomputed_hashes = load_precomputed_hashes(output_dir)

print("="*100)


# ================================
# Scrittura CSV solo per 256 bit
# ================================
# for hash_name in ["hash_md5", "hash_sha256", "hash_poseidon"]:
#     hashes = precomputed_hashes[hash_name]
#     csv_filename = os.path.join(output_dir, f"{hash_name}_hashes.csv")
    
#     with open(csv_filename, 'w', newline='') as csvfile:
#         writer = csv.writer(csvfile)
#         writer.writerow(["input_index", "hash"])  # Intestazione
        
#         for idx, hash_bytes in enumerate(hashes):
#             hash_hex = hash_bytes.hex()
#             writer.writerow([idx, hash_hex])
    
#     print(f"Scritto {len(hashes)} hash in {csv_filename}")




def extract_n_bits(hash_bytes, n_bits):
    """Estrai i primi N bit dall'hash (come bytes)"""
    n_bytes = (n_bits + 7) // 8  # Calcola il numero di byte necessari
    truncated = hash_bytes[:n_bytes]
    
    # Se N non è multiplo di 8, maschera gli ultimi bit
    if n_bits % 8 != 0:
        mask = 0xFF << (8 - (n_bits % 8))
        last_byte = truncated[-1] & mask
        truncated = truncated[:-1] + bytes([last_byte])
    
    return truncated

# Estrai versioni N-bit dagli hash completi
print("\nGenerazione hash N-bit dagli hash completi...")

# Per Poseidon (1-50 bit)
for n_bits in range(1, 50):
    full_hashes = precomputed_hashes["hash_poseidon"]
    n_bit_hashes = [extract_n_bits(h, n_bits) for h in full_hashes]
    precomputed_hashes[f"poseidon{n_bits}"] = n_bit_hashes

# Per SHA256 (1-50 bit)
for n_bits in range(1, 50):
    full_hashes = precomputed_hashes["hash_sha256"]
    n_bit_hashes = [extract_n_bits(h, n_bits) for h in full_hashes]
    precomputed_hashes[f"sha256_{n_bits}"] = n_bit_hashes

def check_collisions_precomputed(hashes):
    seen = set()
    collisions = 0
    for h in hashes:
        if h in seen:
            collisions += 1
        seen.add(h)
    return collisions

# Calcolo collisioni per tutti i Poseidon (1-128 bit)
poseidon_collisions = {}
for n_bits in range(1, 50):
    key = f'poseidon{n_bits}'
    poseidon_collisions[n_bits] = check_collisions_precomputed(precomputed_hashes[key])

# Calcolo collisioni per tutti gli SHA256 (1-128 bit)
sha256_collisions = {}
for n_bits in range(1, 50):
    key = f'sha256_{n_bits}'
    sha256_collisions[n_bits] = check_collisions_precomputed(precomputed_hashes[key])

# Stampa risultati in formato tabellare
print("\nCollisioni Poseidon:")
print("Bits | Collisioni")
print("-----|-----------")
for n_bits in sorted(poseidon_collisions.keys()):
    print(f"{n_bits:4} | {poseidon_collisions[n_bits]:9}")

print("\nCollisioni SHA256:")
print("Bits | Collisioni")
print("-----|-----------")
for n_bits in sorted(sha256_collisions.keys()):
    print(f"{n_bits:4} | {sha256_collisions[n_bits]:9}")




def check_collisions_precomputed(hashes):
    seen = set()
    collisions = 0
    for h in hashes:
        if h in seen:
            collisions += 1
        seen.add(h)
    return collisions

coll_md5 = check_collisions_precomputed(precomputed_hashes['hash_md5'])
coll_sha = check_collisions_precomputed(precomputed_hashes['hash_sha256'])
coll_pos = check_collisions_precomputed(precomputed_hashes['hash_poseidon'])


print("\nCollisioni Precalcolate:")
print(f"MD5: {coll_md5}")
print(f"SHA-256: {coll_sha}")
print(f"Poseidon: {coll_pos}")


# Dati di esempio (sostituisci con i tuoi dati reali)
bits = np.arange(1, 50)
poseidon_collisions = np.array([poseidon_collisions[b] for b in bits])  # Sostituisci con i tuoi dati
sha256_collisions = np.array([sha256_collisions[b] for b in bits])    # Sostituisci con i tuoi dati

# Grafico per Poseidon
plt.figure(figsize=(12, 6))
plt.plot(bits, poseidon_collisions, color='blue', linewidth=2)
plt.xlabel('Lunghezza dell\'hash (bit)', fontsize=12)
plt.ylabel('Numero di collisioni', fontsize=12)
plt.title('Collisioni di Poseidon al variare della lunghezza dell\'hash', fontsize=14)

# Disabilita la notazione scientifica e formatta i tick
plt.ticklabel_format(axis='y', style='plain', useOffset=False)
plt.xticks(np.arange(0, 51, 5))  # Tick ogni 5 bit sull'asse x
plt.yticks(np.arange(0, max(poseidon_collisions)+1, max(poseidon_collisions)//10))  # Tick sull'asse y
plt.grid(True, linestyle='--', alpha=0.4)
plt.tight_layout()
plt.savefig('collisioni_poseidon.png', dpi=300)
plt.show()

# Grafico per SHA-256
plt.figure(figsize=(12, 6))
plt.plot(bits, sha256_collisions, color='orange', linewidth=2)
plt.xlabel('Lunghezza dell\'hash (bit)', fontsize=12)
plt.ylabel('Numero di collisioni', fontsize=12)
plt.title('Collisioni di SHA-256 al variare della lunghezza dell\'hash', fontsize=14)

# Disabilita la notazione scientifica e formatta i tick
plt.ticklabel_format(axis='y', style='plain', useOffset=False)
plt.xticks(np.arange(0, 51, 5))  # Tick ogni 5 bit sull'asse x
plt.yticks(np.arange(0, max(sha256_collisions)+1, max(sha256_collisions)//10))  # Tick sull'asse y
plt.grid(True, linestyle='--', alpha=0.4)
plt.tight_layout()
plt.savefig('collisioni_sha256.png', dpi=300)
plt.show()
