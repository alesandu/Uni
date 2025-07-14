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
import os
import csv

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


def worker(msg_chunk_hash_func):
    # msg_chunk_hash_func è una tupla (msg_chunk, hash_func)
    msg_chunk, hash_func = msg_chunk_hash_func
    return [hash_func(msg) for msg in msg_chunk]


def parallel_hash(hash_func, inputs, n_cores=16, chunks=160):
    chunk_size = (len(inputs) + chunks - 1) // chunks
    chunks_list = [inputs[i*chunk_size:(i+1)*chunk_size] for i in range(chunks)]

    results = []
    with multiprocessing.Pool(processes=n_cores) as pool:
        with tqdm(total=len(chunks_list), desc=f"Precalcolo {hash_func.__name__}") as pbar:
            for res in pool.imap_unordered(worker, [(chunk, hash_func) for chunk in chunks_list], chunksize=1):
                results.append(res)
                pbar.update()

    return [h for sublist in results for h in sublist]


# ------------------------------
# 0. Precalcolo parallelo
# ------------------------------

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
    
# ------------------------------
# 4. Uniformità dei Bit
# ------------------------------

def bit_uniformity_precomputed(hashes):
    bits = [0, 0]  # Contatore per 0 e 1
    byte_counts = [0] * 256  # Contatore per ogni possibile byte (0-255)
    total_bytes = 0  # Contatore totale dei byte analizzati
    
    for h in hashes:
        for byte in h:
            # Conta i bit 0 e 1
            binary = format(byte, '08b')
            bits[0] += binary.count('0')
            bits[1] += binary.count('1')
            
            # Conta le occorrenze del byte
            byte_counts[byte] += 1
            total_bytes += 1
    
    # Calcola P(i) per ogni byte
    if total_bytes > 0:
        byte_probabilities = [count / total_bytes for count in byte_counts]
    else:
        byte_probabilities = [0] * 256
    
    return bits, byte_counts, byte_probabilities

bits_md5, byte_counts_md5, byte_probs_md5 = bit_uniformity_precomputed(precomputed_hashes['hash_md5'])
bits_sha, byte_counts_sha, byte_probs_sha = bit_uniformity_precomputed(precomputed_hashes['hash_sha256'])
bits_pos, byte_counts_pos, byte_probs_pos = bit_uniformity_precomputed(precomputed_hashes['hash_poseidon'])

# ------------------------------
# 7. Chi-Square Test
# ------------------------------

def chi_square_test_precomputed(precomputed, hash_func_name):
    """
    Calcola il test Chi-Square per verificare se la distribuzione dei byte è uniforme.
    Restituisce la statistica Chi-Square e il p-value.
    """
    byte_counts = np.zeros(256, dtype=np.int64)
    total_bytes = 0

    for h in tqdm(precomputed, desc=f"Chi-Square {hash_func_name}"):
        for byte in h:
            byte_counts[byte] += 1
        total_bytes += len(h)

    expected = total_bytes / 256
    chi_square = np.sum((byte_counts - expected)**2 / expected)
    p_value = 1 - chi2.cdf(chi_square, df=255)

    return chi_square, p_value

chi_sq_md5, p_md5 = chi_square_test_precomputed(precomputed_hashes['hash_md5'], 'MD5')
chi_sq_sha, p_sha = chi_square_test_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
chi_sq_pos, p_pos = chi_square_test_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')


















# Bit Uniformity Results
print("Bit Uniformity Results:")
print(f"MD5 - 0s: {bits_md5[0]}, 1s: {bits_md5[1]}, Ratio: {bits_md5[0]/bits_md5[1] if bits_md5[1] != 0 else 0}")
print(f"SHA256 - 0s: {bits_sha[0]}, 1s: {bits_sha[1]}, Ratio: {bits_sha[0]/bits_sha[1] if bits_sha[1] != 0 else 0}")
print(f"Poseidon - 0s: {bits_pos[0]}, 1s: {bits_pos[1]}, Ratio: {bits_pos[0]/bits_pos[1] if bits_pos[1] != 0 else 0}")

# Byte Counts (first 10 for brevity)
print("\nByte Counts (first 10):")
print(f"MD5: {byte_counts_md5[:10]}")
print(f"SHA256: {byte_counts_sha[:10]}")
print(f"Poseidon: {byte_counts_pos[:10]}")

# Byte Probabilities (first 10 for brevity)
print("\nByte Probabilities (first 10):")
print(f"MD5: {byte_probs_md5[:10]}")
print(f"SHA256: {byte_probs_sha[:10]}")
print(f"Poseidon: {byte_probs_pos[:10]}")

# Chi-Square Test Results
print("\nChi-Square Test Results:")
print(f"MD5 - Chi-Square: {chi_sq_md5}, p-value: {p_md5}")
print(f"SHA256 - Chi-Square: {chi_sq_sha}, p-value: {p_sha}")
print(f"Poseidon - Chi-Square: {chi_sq_pos}, p-value: {p_pos}")