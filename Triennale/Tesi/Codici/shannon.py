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
output_dir = "../hash_outputs/"  # Sostituisci con il percorso corretto
precomputed_hashes = load_precomputed_hashes(output_dir)

print("="*100)


# ------------------------------
# 6. Shannon Entropy
# ------------------------------

def shannon_entropy_precomputed(precomputed, hash_func_name):
    byte_counts = np.zeros(256, dtype=np.float64)
    total_bytes = 0

    for h in tqdm(precomputed, desc=f"Shannon Entropy {hash_func_name}"):
        for byte in h:
            byte_counts[byte] += 1
        total_bytes += len(h)
    
    probabilities = byte_counts / total_bytes
    entropy = -np.sum(probabilities * np.log2(probabilities + 1e-12))
    return entropy

entropy_md5 = shannon_entropy_precomputed(precomputed_hashes['hash_md5'], 'MD5')
entropy_sha = shannon_entropy_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
entropy_pos = shannon_entropy_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')

# # ------------------------------
# # 9. Bit Psition Analysis
# # ------------------------------

# def bit_position_analysis_precomputed(precomputed, hash_func_name, threshold=0.10):
#     bit_positions = 8 * len(precomputed[0])
#     bit_counts = np.zeros((bit_positions, 2), dtype=np.int32)
    
#     for h in tqdm(precomputed, desc=f"Bit Analysis {hash_func_name}"):
#         for bit_pos in range(bit_positions):
#             byte_pos = bit_pos // 8
#             bit_in_byte = bit_pos % 8
#             byte = h[byte_pos]
#             bit_value = (byte >> (7 - bit_in_byte)) & 1
#             bit_counts[bit_pos][bit_value] += 1
    
#     results = {}
#     for pos in range(bit_positions):
#         total = bit_counts[pos][0] + bit_counts[pos][1]
#         perc_0 = (bit_counts[pos][0] / total) * 100 if total > 0 else 0
#         perc_1 = (bit_counts[pos][1] / total) * 100 if total > 0 else 0
        
#         if abs(50.0 - perc_0) > threshold or abs(50.0 - perc_1) > threshold:
#             results[pos] = {'0': perc_0, '1': perc_1}
    
#     return results

# bit_md5 = bit_position_analysis_precomputed(precomputed_hashes['hash_md5'], 'MD5')
# bit_sha = bit_position_analysis_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
# bit_pos = bit_position_analysis_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')









# # Shannon Entropy Results
# print("Shannon Entropy Results:")
# print(f"MD5: {entropy_md5}")
# print(f"SHA-256: {entropy_sha}")
# print(f"Poseidon: {entropy_pos}")

# # Bit Position Analysis Results (showing only positions with significant bias)
# print("\nBit Position Analysis Results (positions with >0.5% bias):")
# print(f"MD5 - Biased positions: {len(bit_md5)}")
# if bit_md5:
#     print("First 5 biased positions:")
#     for pos in list(bit_md5.keys()):
#         print(f"  Bit {pos}: 0={bit_md5[pos]['0']:.2f}%, 1={bit_md5[pos]['1']:.2f}%")

# print(f"\nSHA-256 - Biased positions: {len(bit_sha)}")
# if bit_sha:
#     print("First 5 biased positions:")
#     for pos in list(bit_sha.keys()):
#         print(f"  Bit {pos}: 0={bit_sha[pos]['0']:.2f}%, 1={bit_sha[pos]['1']:.2f}%")

# print(f"\nPoseidon - Biased positions: {len(bit_pos)}")
# if bit_pos:
#     print("First 5 biased positions:")
#     for pos in list(bit_pos.keys()):
#         print(f"  Bit {pos}: 0={bit_pos[pos]['0']:.2f}%, 1={bit_pos[pos]['1']:.2f}%")







# Shannon Entropy data
hash_algorithms = ['Poseidon', 'SHA-256']
entropy_values = [entropy_pos, entropy_sha]
max_entropy = 8.0  # Maximum possible entropy for 256-bit hash (8 bits per byte)

# Colors for each algorithm
colors = ['#4e79a7', '#e15759']

plt.figure(figsize=(8, 8))

# Set y-axis limits to highlight tiny differences
y_min = 7.99990
y_max = 8.00005
plt.ylim(y_min, y_max)

# Reference line for theoretical maximum entropy
max_entropy_line = plt.axhline(y=max_entropy, 
                              color='grey', 
                              linestyle='--', 
                              linewidth=1.5, 
                              alpha=0.7,
                              zorder=1)

# Custom bars with precision labels
bar_width = 0.6
bars = []
for algo, val, color in zip(hash_algorithms, entropy_values, colors):
    bar = plt.bar(algo, val, width=bar_width, color=color, alpha=0.85, 
                 edgecolor='black', linewidth=0.8, zorder=2)
    bars.append(bar)
    
    # Precision value label (6 decimal places)
    plt.text(algo, val + 0.000005, f'{val:.6f}', 
            ha='center', va='bottom', 
            fontsize=11,
            bbox=dict(facecolor='white', alpha=0.8, edgecolor='none', pad=2))

# Custom grid and ticks
plt.grid(axis='y', linestyle=':', alpha=0.3, which='major')
plt.grid(axis='y', linestyle=':', alpha=0.1, which='minor')

# Custom y-ticks for better precision visualization
plt.yticks(np.linspace(y_min, y_max, num=7), 
          [f"{y:.5f}" for y in np.linspace(y_min, y_max, num=7)],
          fontsize=10)

# Titles and labels
plt.title('Shannon Entropy Comparison\nPrecision Analysis (6 decimal places)', 
         pad=20, fontsize=14)
plt.xlabel('Hash Algorithm', labelpad=10, fontsize=12)
plt.ylabel('Entropy (bits/byte)', labelpad=10, fontsize=12)

# Remove top and right spines
for spine in ['top', 'right']:
    plt.gca().spines[spine].set_visible(False)

# Add legend for reference line
plt.legend([max_entropy_line], ['Theoretical Maximum (8.0)'],
          loc='upper right', framealpha=0.9)

plt.tight_layout()

# Save high-quality image
plt.savefig('entropy_comparison_precision.png', 
           dpi=350, 
           bbox_inches='tight',
           transparent=False)

plt.show()