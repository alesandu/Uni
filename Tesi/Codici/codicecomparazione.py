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
NUM_SAMPLES = 10000  # sample
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

for hash_func in [hash_md5, hash_sha256, hash_poseidon]:
    print(f"Precalcolo {hash_func.__name__} (parallelizzato)...")
    hashes = parallel_hash(hash_func, shared_inputs, n_cores=16)
    precomputed_hashes[hash_func.__name__] = hashes
    
    
# def load_precomputed_hashes(output_dir):
#     precomputed_hashes = {}
    
#     # Carica gli hash completi dai CSV
#     for hash_name in ["hash_md5", "hash_sha256", "hash_poseidon"]:
#         csv_filename = os.path.join(output_dir, f"{hash_name}_hashes.csv")
        
#         if not os.path.exists(csv_filename):
#             raise FileNotFoundError(f"File {csv_filename} non trovato.")
        
#         print(f"Caricamento {hash_name} da file CSV...")
#         hashes = []
        
#         with open(csv_filename, 'r', newline='') as csvfile:
#             reader = csv.reader(csvfile)
#             next(reader)  # Salta intestazione
            
#             for row in reader:
#                 hash_hex = row[1]
#                 hash_bytes = bytes.fromhex(hash_hex)
#                 hashes.append(hash_bytes)
        
#         precomputed_hashes[hash_name] = hashes
    
#     return precomputed_hashes

# # Utilizzo:
# output_dir = "hash_outputs/"  # Sostituisci con il percorso corretto
# precomputed_hashes = load_precomputed_hashes(output_dir)

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

def parallel_avalanche_precomputed(hash_func, shared_inputs, precomputed_hashes, n_cores=16, chunks=160):
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

# ------------------------------
# 3. Analisi Collisioni
# ------------------------------

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

# ------------------------------
# 4. Uniformità dei Bit
# ------------------------------

def bit_uniformity_precomputed(hashes):
    bits = [0, 0]
    for h in hashes:
        for byte in h:
            binary = format(byte, '08b')
            bits[0] += binary.count('0')
            bits[1] += binary.count('1')
    return bits

bits_md5 = bit_uniformity_precomputed(precomputed_hashes['hash_md5'])
bits_sha = bit_uniformity_precomputed(precomputed_hashes['hash_sha256'])
bits_pos = bit_uniformity_precomputed(precomputed_hashes['hash_poseidon'])

# ------------------------------
# 5. Autocorrelazione
# ------------------------------

def autocorrelation_test_precomputed(precomputed, hash_func_name):
    autocorrs = []
    
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
        
        autocorrs.append(autocorr)
    
    # Media tra tutti gli hash analizzati
    return np.mean(autocorrs, axis=0)

autocorr_md5 = autocorrelation_test_precomputed(precomputed_hashes['hash_md5'], 'MD5')
autocorr_sha = autocorrelation_test_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
autocorr_pos = autocorrelation_test_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')


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


# ------------------------------
# 8. Intra-Hash Test
# ------------------------------

def intra_hash_precomputed(precomputed, hash_func_name, window_sizes=[32, 24, 16]):
    results = {}
    
    for window_size in window_sizes:
        collisions_total = 0
        total_windows = 0
        per_hash_rates = []  # per salvare i rate dei singoli hash
        
        for h in tqdm(precomputed, desc=f"Intra-Hash {hash_func_name} {window_size}bit"):
            bits = ''.join(format(byte, '08b') for byte in h)
            windows = [bits[i:i+window_size] for i in range(len(bits) - window_size + 1)]
            total_windows += len(windows)
            
            seen = set()
            collisions = 0
            for w in windows:
                if w in seen:
                    collisions += 1
                seen.add(w)
            
            # Calcola rate per il singolo hash
            rate = collisions / len(windows) if len(windows) > 0 else 0
            per_hash_rates.append(rate)
            
            collisions_total += collisions
        
        # Calcola media dei tassi di collisione per singolo hash
        mean_collision_rate_per_hash = np.mean(per_hash_rates)
        
        results[window_size] = {
            'collisions': collisions_total,
            'windows_analyzed': total_windows,
            'collision_rate': collisions_total / total_windows if total_windows > 0 else 0,
            'mean_collision_rate_per_hash': mean_collision_rate_per_hash
        }
        
    return results

intra_md5 = intra_hash_precomputed(precomputed_hashes['hash_md5'], 'MD5')
intra_sha = intra_hash_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
intra_pos = intra_hash_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')

# ------------------------------
# 9. Bit Psition Analysis
# ------------------------------

def bit_position_analysis_precomputed(precomputed, hash_func_name, threshold=0.5):
    bit_positions = 8 * len(precomputed[0])
    bit_counts = np.zeros((bit_positions, 2), dtype=np.int32)
    
    for h in tqdm(precomputed, desc=f"Bit Analysis {hash_func_name}"):
        for bit_pos in range(bit_positions):
            byte_pos = bit_pos // 8
            bit_in_byte = bit_pos % 8
            byte = h[byte_pos]
            bit_value = (byte >> (7 - bit_in_byte)) & 1
            bit_counts[bit_pos][bit_value] += 1
    
    results = {}
    for pos in range(bit_positions):
        total = bit_counts[pos][0] + bit_counts[pos][1]
        perc_0 = (bit_counts[pos][0] / total) * 100 if total > 0 else 0
        perc_1 = (bit_counts[pos][1] / total) * 100 if total > 0 else 0
        
        if abs(50.0 - perc_0) > threshold or abs(50.0 - perc_1) > threshold:
            results[pos] = {'0': perc_0, '1': perc_1}
    
    return results

bit_md5 = bit_position_analysis_precomputed(precomputed_hashes['hash_md5'], 'MD5')
bit_sha = bit_position_analysis_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
bit_pos = bit_position_analysis_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')

# ------------------------------
# Stampa risultati finali
# ------------------------------

print("\nRISULTATI RIEPILOGO:")
print(f"2.1 Avalanche Mean (%): MD5={avalanche_md5[0]:.2f}%, SHA-256={avalanche_sha[0]:.2f}%, Poseidon={avalanche_pos[0]:.2f}%")

print(f"2.2 Avalanche Deviazione Standard (%): MD5={avalanche_md5[1]:.2f}%, SHA-256={avalanche_sha[1]:.2f}%, Poseidon={avalanche_pos[1]:.2f}%")

print(f"\n3. Collisioni: MD5={coll_md5}, SHA-256={coll_sha}, Poseidon={coll_pos}")

print(f"\n4. Uniformità bit 0/1:")
print(f"   MD5       -> 0: {bits_md5[0]}, 1: {bits_md5[1]}")
print(f"   SHA-256   -> 0: {bits_sha[0]}, 1: {bits_sha[1]}")
print(f"   Poseidon  -> 0: {bits_pos[0]}, 1: {bits_pos[1]}")

print("\n6. Shannon Entropy (bit/byte):")
print(f"  MD5: {entropy_md5:.4f}, SHA-256: {entropy_sha:.4f}, Poseidon: {entropy_pos:.4f}")


print("\n7. Chi-Square:")
print(f"MD5:  Statistica Chi-Square = {chi_sq_md5:.2f} p-value = {p_md5:.4f} {'Distribuzione NON uniforme (p < 0.05)' if p_md5 < 0.05 else 'Distribuzione uniforme (p ≥ 0.05)'}")
print(f"SHA-256: Statistica Chi-Square = {chi_sq_sha:.2f} p-value = {p_sha:.4f} {'Distribuzione NON uniforme (p < 0.05)' if p_sha < 0.05 else 'Distribuzione uniforme (p ≥ 0.05)'}")
print(f"Poseidon: Statistica Chi-Square = {chi_sq_pos:.2f} p-value = {p_pos:.4f} {'Distribuzione NON uniforme (p < 0.05)' if p_pos < 0.05 else 'Distribuzione uniforme (p ≥ 0.05)'}")


print("\n8. Birthday Paradox:")
def print_intra_results(name, results):
    print(f"{name} Intra-Hash Collisions:")
    for size, data in results.items():
        print(f"  Window {size} bits: Total collisions: {data['collisions']} Windows analyzed: {data['windows_analyzed']} Collision rate: {data['collision_rate']:.6f} Intra-rate: {data['mean_collision_rate_per_hash']:.6f}")

print_intra_results("MD5", intra_md5)
print_intra_results("SHA-256", intra_sha)
print_intra_results("Poseidon", intra_pos)


print("\n9. Bit Position Analysis:")
def print_bit_stats(name, bit_data, threshold=2, num_bits=None):
    print(f"{name} - Bit con deviazione > {threshold}%:")
    filtered_bits = {
        pos: vals for pos, vals in bit_data.items() 
        if abs(50.0 - vals['0']) > threshold or abs(50.0 - vals['1']) > threshold
    }
    if not filtered_bits:
        print("  Nessun bit supera la soglia")
        return
    displayed_bits = sorted(filtered_bits.items())
    if num_bits is not None:
        displayed_bits = displayed_bits[:num_bits]
    
    for pos, vals in displayed_bits:
        deviation = abs(50.0 - vals['0'])
        print(f"  Bit {pos + 1}: 0={vals['0']:.2f}% | 1={vals['1']:.2f}% (Deviazione: {deviation:.2f}%)")

print_bit_stats("MD5", bit_md5)
print_bit_stats("SHA-256", bit_sha)
print_bit_stats("Poseidon", bit_pos)

def print_autocorrelation_results(autocorr_data, algo_name):
    """Stampa statistiche riassuntive per l'autocorrelazione."""
    mean_val = np.mean(autocorr_data)
    max_val = np.max(autocorr_data)
    min_val = np.min(autocorr_data)
    argmax = np.argmax(autocorr_data)  # Posizione del massimo
    
    print(f"\n🔍 {algo_name} Autocorrelazione:")
    print("-" * 50)
    print(f"• Media: {mean_val:.4f}")
    print(f"• Massimo: {max_val:.4f} (alla posizione {argmax})")
    print(f"• Minimo: {min_val:.4f}")
    print(f"\nPrimi 10 valori:")
    print(np.array2string(autocorr_data[:10], precision=4, separator=', ', suppress_small=True))
    print("-" * 50)

# Stampa risultati per tutti gli algoritmi
print("\n")
print("=" * 50)
print("📊 RISULTATI AUTOCORRELAZIONE".center(50))
print("=" * 50)

print_autocorrelation_results(autocorr_md5, "MD5")
print_autocorrelation_results(autocorr_sha, "SHA-256")
print_autocorrelation_results(autocorr_pos, "Poseidon")

# Dati per i grafici (già calcolati)
data = {
    'Avalanche': {
        'MD5': {'mean': avalanche_md5[0], 'std': avalanche_md5[1]},
        'SHA-256': {'mean': avalanche_sha[0], 'std': avalanche_sha[1]},
        'Poseidon': {'mean': avalanche_pos[0], 'std': avalanche_pos[1]},
        'ylabel': '% bit cambiati',
        'title': 'Avalanche Effect'
    },
    'Collisioni': {
        'MD5': coll_md5,
        'SHA-256': coll_sha,
        'Poseidon': coll_pos,
        'ylabel': 'N. collisioni',
        'title': f'Collisioni (su {NUM_SAMPLES} input)'
    },
    'Uniformità': {
        'MD5': {'0': bits_md5[0], '1': bits_md5[1]},
        'SHA-256': {'0': bits_sha[0], '1': bits_sha[1]},
        'Poseidon': {'0': bits_pos[0], '1': bits_pos[1]},
        'ylabel': 'Conteggio bit',
        'title': 'Distribuzione bit 0/1'
    },
    'Entropia': {
        'MD5': entropy_md5,
        'SHA-256': entropy_sha,
        'Poseidon': entropy_pos,
        'ylabel': 'Entropia (bit/byte)',
        'title': 'Shannon Entropy'
    },
    'Chi-Square': {
        'MD5': {'chi_sq': chi_sq_md5, 'p': p_md5},
        'SHA-256': {'chi_sq': chi_sq_sha, 'p': p_sha},
        'Poseidon': {'chi_sq': chi_sq_pos, 'p': p_pos},
        'ylabel': 'Valore Chi-Square',
        'title': 'Test Chi-Square (Uniformità)'
    },
    'Autocorrelazione': {
        'MD5': autocorr_md5,
        'SHA-256': autocorr_sha,
        'Poseidon': autocorr_pos,
        'ylabel': 'Correlazione',
        'title': 'Autocorrelazione'
    },
    'Birthday Paradox': {
        'MD5': intra_md5,
        'SHA-256': intra_sha,
        'Poseidon': intra_pos,
        'ylabel': 'Tasso di collisioni',
        'title': 'Birthday Paradox (Intra-Hash)'
    },
    'Bit Position': {
        'MD5': bit_md5,
        'SHA-256': bit_sha,
        'Poseidon': bit_pos,
        'ylabel': 'Deviazione da 50% (%)',
        'title': 'Analisi Posizione Bit'
    }
}

from matplotlib.gridspec import GridSpec
# ------------------------------
# Creazione della figura con griglia personalizzata
# ------------------------------
plt.figure(figsize=(20, 20))
gs = GridSpec(4, 2, width_ratios=[1, 1], height_ratios=[1, 1, 1, 1])
colors = {'MD5': 'red', 'SHA-256': 'lightgreen', 'Poseidon': 'lightblue'}

# ------------------------------
# 1. Avalanche Effect (Barre con errori)
# ------------------------------
ax1 = plt.subplot(gs[0, 0])
x = np.arange(3)
means = [data['Avalanche'][h]['mean'] for h in ['MD5', 'SHA-256', 'Poseidon']]
stds = [data['Avalanche'][h]['std'] for h in ['MD5', 'SHA-256', 'Poseidon']]
ax1.bar(x, means, yerr=stds, capsize=10, color=[colors[h] for h in ['MD5', 'SHA-256', 'Poseidon']])
ax1.set_xticks(x)
ax1.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax1.set_ylabel(data['Avalanche']['ylabel'])
ax1.set_title(data['Avalanche']['title'])
ax1.grid(axis='y', linestyle='--', alpha=0.7)
for i, (m, s) in enumerate(zip(means, stds)):
    ax1.text(i, m + 0.5, f'{m:.2f}% ± {s:.2f}', ha='center', va='bottom')

# ------------------------------
# 2. Collisioni (Barre)
# ------------------------------
ax2 = plt.subplot(gs[0, 1])
x = np.arange(3)
collisions = [data['Collisioni'][h] for h in ['MD5', 'SHA-256', 'Poseidon']]
ax2.bar(x, collisions, color=[colors[h] for h in ['MD5', 'SHA-256', 'Poseidon']])
ax2.set_xticks(x)
ax2.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax2.set_ylabel(data['Collisioni']['ylabel'])
ax2.set_title(data['Collisioni']['title'])
ax2.grid(axis='y', linestyle='--', alpha=0.7)
for i, c in enumerate(collisions):
    ax2.text(i, c + 0.1, str(c), ha='center', va='bottom')

# ------------------------------
# 3. Uniformità dei Bit (Barre sovrapposte)
# ------------------------------
ax3 = plt.subplot(gs[1, 0])
x = np.arange(3)
width = 0.35
bit0 = [data['Uniformità'][h]['0'] for h in ['MD5', 'SHA-256', 'Poseidon']]
bit1 = [data['Uniformità'][h]['1'] for h in ['MD5', 'SHA-256', 'Poseidon']]
ax3.bar(x - width/2, bit0, width, label='Bit 0', color='blue')
ax3.bar(x + width/2, bit1, width, label='Bit 1', color='red')
ax3.set_xticks(x)
ax3.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax3.set_ylabel(data['Uniformità']['ylabel'])
ax3.set_title(data['Uniformità']['title'])
ax3.legend()
ax3.grid(axis='y', linestyle='--', alpha=0.7)

# ------------------------------
# 4. Shannon Entropy (Barre)
# ------------------------------
ax4 = plt.subplot(gs[1, 1])
x = np.arange(3)
entropies = [data['Entropia'][h] for h in ['MD5', 'SHA-256', 'Poseidon']]
ax4.bar(x, entropies, color=[colors[h] for h in ['MD5', 'SHA-256', 'Poseidon']])
ax4.set_xticks(x)
ax4.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax4.set_ylabel(data['Entropia']['ylabel'])
ax4.set_title(data['Entropia']['title'])
ax4.grid(axis='y', linestyle='--', alpha=0.7)
for i, e in enumerate(entropies):
    ax4.text(i, e + 0.01, f'{e:.4f}', ha='center', va='bottom')

# ------------------------------
# 5. Chi-Square (Barre con annotazione p-value)
# ------------------------------
ax5 = plt.subplot(gs[2, 0])
x = np.arange(3)
chi_sq = [data['Chi-Square'][h]['chi_sq'] for h in ['MD5', 'SHA-256', 'Poseidon']]
p_values = [data['Chi-Square'][h]['p'] for h in ['MD5', 'SHA-256', 'Poseidon']]
bars = ax5.bar(x, chi_sq, color=[colors[h] for h in ['MD5', 'SHA-256', 'Poseidon']])
ax5.set_xticks(x)
ax5.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax5.set_ylabel(data['Chi-Square']['ylabel'])
ax5.set_title(data['Chi-Square']['title'])
ax5.grid(axis='y', linestyle='--', alpha=0.7)
for i, (chi, p) in enumerate(zip(chi_sq, p_values)):
    ax5.text(i, chi + 5, f'p={p:.1e}', ha='center', va='bottom', fontsize=9)
    color = 'black'
    ax5.text(i, chi/2, 'NON uniforme' if p < 0.05 else 'Uniforme', ha='center', va='center', color=color, weight='bold')

# ------------------------------
# 6. Autocorrelazione (Linee)
# ------------------------------

ax6 = plt.subplot(gs[2, 1])
for algo in ['MD5', 'SHA-256', 'Poseidon']:
    ax6.plot(data['Autocorrelazione'][algo], label=algo, color=colors[algo])
ax6.set_xlabel('Lag')
ax6.set_ylabel(data['Autocorrelazione']['ylabel'])
ax6.set_title(data['Autocorrelazione']['title'])
ax6.legend()
ax6.grid(linestyle='--', alpha=0.7)

# ------------------------------
# 7. Birthday Paradox (Barre raggruppate)
# ------------------------------
ax7 = plt.subplot(gs[3, 0])
x = np.arange(3)  # MD5, SHA-256, Poseidon
width = 0.25
window_sizes = list(data['Birthday Paradox']['MD5'].keys())
for i, size in enumerate(window_sizes):
    rates = [
        data['Birthday Paradox']['MD5'][size]['collision_rate'],
        data['Birthday Paradox']['SHA-256'][size]['collision_rate'],
        data['Birthday Paradox']['Poseidon'][size]['collision_rate']
    ]
    ax7.bar(x + i*width - width, rates, width, label=f'{size} bit', color=plt.cm.viridis(i/len(window_sizes)))
ax7.set_xticks(x)
ax7.set_xticklabels(['MD5', 'SHA-256', 'Poseidon'])
ax7.set_ylabel(data['Birthday Paradox']['ylabel'])
ax7.set_title(data['Birthday Paradox']['title'])
ax7.legend()
ax7.grid(axis='y', linestyle='--', alpha=0.7)

# ------------------------------
# 8. Bit Position Analysis (Scatter)
# ------------------------------
ax8 = plt.subplot(gs[3, 1])
for algo in ['MD5', 'SHA-256', 'Poseidon']:
    bit_data = data['Bit Position'][algo]
    if bit_data:
        positions = list(bit_data.keys())
        deviations = [abs(50 - bit_data[pos]['0']) for pos in positions]
        ax8.scatter(positions, deviations, label=algo, color=colors[algo], alpha=0.6)
ax8.set_xlabel('Posizione del bit')
ax8.set_ylabel(data['Bit Position']['ylabel'])
ax8.set_title(data['Bit Position']['title'])
ax8.legend()
ax8.grid(linestyle='--', alpha=0.7)

# ------------------------------
# Titolo generale e layout
# ------------------------------
plt.suptitle('Analisi Statistiche delle Funzioni Hash: MD5 vs SHA-256 vs Poseidon\n', fontsize=16, fontweight='bold')
plt.tight_layout()
plt.subplots_adjust(top=0.92)
plt.show()
