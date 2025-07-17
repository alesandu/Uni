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


def parallel_hash(hash_func, inputs, n_cores=15, chunks=160):
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
# 2. Avalanche Effect (su primo input)
# ------------------------------

def avalanche_worker(args):
    msgs_chunk, precomputed_chunk, hash_func = args
    diffs = []
    for msg, h_orig in zip(msgs_chunk, precomputed_chunk):
        modified = bytearray(msg)
        modified[0] ^= 0x01
        h_mod = hash_func(bytes(modified))

        diff_bits = 0
        for a, b in zip(h_orig, h_mod):
            diff_bits += bin(a ^ b).count('1')

        diff_percent = (diff_bits / (len(h_orig) * 8)) * 100
        diffs.append(diff_percent)

    return diffs

def parallel_avalanche_precomputed(hash_func, shared_inputs, precomputed_hashes, n_cores=15, chunks=160):
    chunk_size = (len(shared_inputs) + chunks - 1) // chunks

    chunks_inputs = [shared_inputs[i*chunk_size:(i+1)*chunk_size] for i in range(chunks)]
    chunks_precomputed = [precomputed_hashes[i*chunk_size:(i+1)*chunk_size] for i in range(chunks)]

    with Pool(n_cores) as pool:
        results = []
        with tqdm(total=len(chunks_inputs), desc=f"Avalanche {hash_func.__name__}") as pbar:
            for res in pool.imap_unordered(avalanche_worker,
                                           [(chunks_inputs[i], chunks_precomputed[i], hash_func) for i in range(chunks)],
                                           chunksize=1):
                results.extend(res)
                pbar.update()

    mean_diff = np.mean(results)
    std_diff = np.std(results)
    return mean_diff, std_diff

avalanche_md5 = parallel_avalanche_precomputed(hash_md5, shared_inputs, precomputed_hashes['hash_md5'])
avalanche_sha = parallel_avalanche_precomputed(hash_sha256, shared_inputs, precomputed_hashes['hash_sha256'])
avalanche_pos = parallel_avalanche_precomputed(hash_poseidon, shared_inputs, precomputed_hashes['hash_poseidon'])

# Avalanche Effect Results
print("Avalanche Effect Results (1-bit flip in first byte):")
print(f"MD5 - Mean bit difference: {avalanche_md5[0]:.2f}%, Std Dev: {avalanche_md5[1]:.2f}%")
print(f"SHA-256 - Mean bit difference: {avalanche_sha[0]:.2f}%, Std Dev: {avalanche_sha[1]:.2f}%")
print(f"Poseidon - Mean bit difference: {avalanche_pos[0]:.2f}%, Std Dev: {avalanche_pos[1]:.2f}%")

hash_algorithms = ['MD5', 'SHA-256', 'Poseidon']
mean_diff = [avalanche_md5[0], avalanche_sha[0], avalanche_pos[0]]
std_dev = [avalanche_md5[1], avalanche_sha[1], avalanche_pos[1]]

# Colors for each algorithm
colors = ['#4e79a7', '#f28e2b', '#e15759']

# Create figure
plt.figure(figsize=(10, 6))

# Bar plot with error bars
bars = plt.bar(hash_algorithms, mean_diff, color=colors, 
               yerr=std_dev, capsize=10, alpha=0.8, width=0.6)

# Add horizontal line at 50% (ideal avalanche effect)
plt.axhline(y=50, color='gray', linestyle='--', alpha=0.7)
plt.text(2.7, 51, 'Ideal Avalanche (50%)', va='center', ha='right', color='gray')

# Add value labels on top of bars
for bar, std in zip(bars, std_dev):
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2., height + 1,
             f'{height:.1f}% ± {std:.1f}%',
             ha='center', va='bottom', fontsize=10)

# Customize plot
plt.title('Avalanche Effect Comparison (1-bit flip test)', fontsize=14, pad=20)
plt.ylabel('Percentage of Flipped Bits in Hash Output', fontsize=12)
plt.xlabel('Hash Algorithm', fontsize=12)
plt.ylim(0, 70)
plt.grid(axis='y', alpha=0.3)

plt.tight_layout()

# Save as high-quality PNG and PDF for thesis
plt.savefig('avalanche_effect_comparison.png', dpi=300, bbox_inches='tight')
plt.savefig('avalanche_effect_comparison.pdf', bbox_inches='tight')

plt.show()