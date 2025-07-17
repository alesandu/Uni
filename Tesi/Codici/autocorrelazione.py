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

def autocorrelation_test_precomputed(precomputed, hash_func_name):
    sum_autocorrs = None
    count = 0
    
    for h in tqdm(precomputed, desc=f"Autocorrelazione {hash_func_name}"):
        bits = np.array([int(b) for byte in h for b in format(byte, '08b')], dtype=int)
        n = len(bits)
        autocorr = []
        
        for k in range(n):
            if n - k == 0:
                autocorr.append(0)
            else:
                r_k = np.sum(bits[:n-k] * bits[k:]) / (n - k)
                autocorr.append(r_k)
        
        # Accumula la somma invece di memorizzare tutti i risultati
        if sum_autocorrs is None:
            sum_autocorrs = np.array(autocorr)
        else:
            sum_autocorrs += np.array(autocorr)
        count += 1
    
    # Calcola la media finale
    return sum_autocorrs / count

autocorr_md5 = autocorrelation_test_precomputed(precomputed_hashes['hash_md5'], 'MD5')
autocorr_sha = autocorrelation_test_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
autocorr_pos = autocorrelation_test_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')


# def intra_hash_precomputed(precomputed, hash_func_name, window_sizes=[32, 24, 16]):
#     results = {}
    
#     for window_size in window_sizes:
#         collisions_total = 0
#         total_windows = 0
#         per_hash_rates = []  # per salvare i rate dei singoli hash
        
#         for h in tqdm(precomputed, desc=f"Intra-Hash {hash_func_name} {window_size}bit"):
#             bits = ''.join(format(byte, '08b') for byte in h)
#             windows = [bits[i:i+window_size] for i in range(len(bits) - window_size + 1)]
#             total_windows += len(windows)
            
#             seen = set()
#             collisions = 0
#             for w in windows:
#                 if w in seen:
#                     collisions += 1
#                 seen.add(w)
            
#             # Calcola rate per il singolo hash
#             rate = collisions / len(windows) if len(windows) > 0 else 0
#             per_hash_rates.append(rate)
            
#             collisions_total += collisions
        
#         # Calcola media dei tassi di collisione per singolo hash
#         mean_collision_rate_per_hash = np.mean(per_hash_rates)
        
#         results[window_size] = {
#             'collisions': collisions_total,
#             'windows_analyzed': total_windows,
#             'collision_rate': collisions_total / total_windows if total_windows > 0 else 0,
#             'mean_collision_rate_per_hash': mean_collision_rate_per_hash
#         }
        
#     return results

# intra_md5 = intra_hash_precomputed(precomputed_hashes['hash_md5'], 'MD5')
# intra_sha = intra_hash_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
# intra_pos = intra_hash_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')

# Autocorrelation Test Results (showing first 10 lags)
print("Autocorrelation Test Results (first 10 lags):")
print("MD5 - Autocorrelation values:")
print(autocorr_md5[:10])
print("\nSHA-256 - Autocorrelation values:")
print(autocorr_sha[:10])
print("\nPoseidon - Autocorrelation values:")
print(autocorr_pos[:10])

# # Intra-Hash Collision Results
# print("\nIntra-Hash Collision Results:")
# for hash_name, results in [('MD5', intra_md5), ('SHA-256', intra_sha), ('Poseidon', intra_pos)]:
#     print(f"\n{hash_name}:")
#     for window_size in results:
#         print(f"  Window {window_size} bits:")
#         print(f"    Total collisions: {results[window_size]['collisions']}")
#         print(f"    Windows analyzed: {results[window_size]['windows_analyzed']}")
#         print(f"    Global collision rate: {results[window_size]['collision_rate']:.6f}")
#         print(f"    Mean collision rate per hash: {results[window_size]['mean_collision_rate_per_hash']:.6f}")
        
        
import matplotlib.pyplot as plt
import numpy as np


# Set up the plot
plt.figure(figsize=(12, 6))

# Plot autocorrelation curves
plt.plot(autocorr_md5[:256], label='MD5', color='#4e79a7', linewidth=2)
plt.plot(autocorr_sha[:256], label='SHA-256', color='#f28e2b', linewidth=2)
plt.plot(autocorr_pos[:256], label='Poseidon', color='#e15759', linewidth=2)

# Add ideal reference line at 0.5
plt.axhline(y=0.5, color='gray', linestyle='--', alpha=0.7)
plt.text(260, 0.51, 'Ideal (0.5)', va='center', ha='right', color='gray')

# Customize the plot
plt.title('Autocorrelation Test Results (First 256 Lags)', fontsize=14, pad=20)
plt.xlabel('Lag (k)', fontsize=12)
plt.ylabel('Autocorrelation Coefficient (r_k)', fontsize=12)
plt.legend(fontsize=12)
plt.grid(alpha=0.3)

# Set appropriate limits
plt.xlim(0, 256)
plt.ylim(0.45, 0.55)

# Add inset for close-up view of first 32 lags
ax_inset = plt.axes([0.5, 0.2, 0.4, 0.4])
ax_inset.plot(autocorr_md5[:32], color='#4e79a7', linewidth=1.5)
ax_inset.plot(autocorr_sha[:32], color='#f28e2b', linewidth=1.5)
ax_inset.plot(autocorr_pos[:32], color='#e15759', linewidth=1.5)
ax_inset.axhline(y=0.5, color='gray', linestyle='--', alpha=0.5)
ax_inset.set_title('First 32 Lags (Zoom)', fontsize=10)
ax_inset.grid(alpha=0.2)
ax_inset.set_ylim(0.48, 0.52)

plt.tight_layout()

# Save as high-quality PNG and PDF
plt.savefig('autocorrelation_comparison.png', dpi=300, bbox_inches='tight')
plt.savefig('autocorrelation_comparison.pdf', bbox_inches='tight')

plt.show()