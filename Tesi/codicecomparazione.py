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

# ------------------------------
# Implementazione Poseidon256 (basata sul paper)
# ------------------------------
PRIME = mpz(2**256 - 2**32 - 977)  # Primo vicino a 2^256 (usato in Ethereum)
T = 3  # Dimensione stato (2 input, 1 capacità)
ROUNDS_F = 8  # Round completi
ROUNDS_P = 56  # Round parziali
ALPHA = 5  # Esponente S-box

# Costanti precalcolate (esempio semplificato)
ROUND_CONSTANTS = [mpz((i * 0xdeadbeef) % PRIME) for i in range(1, (ROUNDS_F + ROUNDS_P) * T + 1)]
MDS_MATRIX = [
    [mpz(0x123456789), mpz(0x987654321), mpz(0xabcdef12)],
    [mpz(0x456789abc), mpz(0x321fedcba), mpz(0x789abcdef)],
    [mpz(0x987654321), mpz(0xabcdef123), mpz(0x654321fed)]
]

def add(a: int, b: int) -> int:
    return (a + b) % PRIME

def mul(a: int, b: int) -> int:
    return (a * b) % PRIME

def pow_alpha(x: int) -> int:
    return pow(x, ALPHA, PRIME)

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
NUM_SAMPLES = 100000  # sample
INPUT_SIZE = 1024     # 1 KB per input
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
    

# # ------------------------------
# # 1. Benchmark Prestazioni (Tempo medio di hashing, inutile poseidon impiega troppo)
# # ------------------------------

# def benchmark(hash_func):
#     start = time.time()
#     for msg in tqdm(shared_inputs, desc=f"Benchmark {hash_func.__name__}"):
#         hash_func(msg)
#     return (time.time() - start) * 1000 / NUM_SAMPLES  # ms per hash

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

# Esempio d’uso:
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
    all_bits = []
    for h in tqdm(precomputed, desc=f"Autocorrelazione {hash_func_name}"):
        bits = []
        for byte in h:
            bits.extend([int(b) for b in format(byte, '08b')])
        all_bits.append(bits)
    
    bit_matrix = np.array(all_bits, dtype=int)
    autocorr = np.mean([np.correlate(row, row, mode='full') for row in bit_matrix], axis=0)
    autocorr = autocorr / len(bits)
    return autocorr

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
# 8. Birthday Paradox Test
# ------------------------------

def birthday_paradox_intra_hash_precomputed(precomputed, hash_func_name, window_sizes=[32, 24, 16]):
    results = {}
    
    for window_size in window_sizes:
        collisions_total = 0
        total_windows = 0
        
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
            collisions_total += collisions
        
        results[window_size] = {
            'collisions': collisions_total,
            'windows_analyzed': total_windows,
            'collision_rate': collisions_total / total_windows if total_windows > 0 else 0
        }
    
    return results

intra_md5 = birthday_paradox_intra_hash_precomputed(precomputed_hashes['hash_md5'], 'MD5')
intra_sha = birthday_paradox_intra_hash_precomputed(precomputed_hashes['hash_sha256'], 'SHA-256')
intra_pos = birthday_paradox_intra_hash_precomputed(precomputed_hashes['hash_poseidon'], 'Poseidon')

# ------------------------------
# 9. Bit Psition Analysis
# ------------------------------

def bit_position_analysis_precomputed(precomputed, hash_func_name, threshold=2):
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

# # ------------------------------
# # Visualizzazione Risultati
# # ------------------------------

# plt.figure(figsize=(12, 6))

# # Avalanche
# plt.subplot(2, 2, 1)
# means = [avalanche_md5[0], avalanche_sha[0], avalanche_pos[0]]
# stds = [avalanche_md5[1], avalanche_sha[1], avalanche_pos[1]]
# plt.bar(['MD5', 'SHA-256', 'Poseidon'], means, yerr=stds, color=['red', 'green', 'blue'], capsize=5)
# plt.title('Avalanche Effect (% bit cambiati)')
# plt.ylabel('%')

# # Collisioni
# plt.subplot(2, 2, 2)
# plt.bar(['MD5', 'SHA-256', 'Poseidon'], [coll_md5, coll_sha, coll_pos], color=['red', 'green', 'blue'])
# plt.title(f'Collisioni su {NUM_SAMPLES} input')
# plt.ylabel('N. collisioni')

# # Uniformità
# plt.subplot(2, 2, 3)
# plt.bar(['MD5-0', 'MD5-1', 'SHA-0', 'SHA-1', 'POS-0', 'POS-1'],
#         [bits_md5[0], bits_md5[1], bits_sha[0], bits_sha[1], bits_pos[0], bits_pos[1]],
#         color=['red', 'red', 'green', 'green', 'blue', 'blue'])
# plt.title('Distribuzione bit 0/1')
# plt.ylabel('Conteggio')

# # Autocorrelazione
# plt.subplot(2, 2, 4)
# plt.plot(autocorr_md5, label="MD5", color='red')
# plt.plot(autocorr_sha, label="SHA-256", color='green')
# plt.plot(autocorr_pos, label="Poseidon", color='blue')
# plt.title("Autocorrelazione degli hash")
# plt.legend()

# plt.tight_layout()
# plt.show()

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
        print(f"  Window {size} bits: Total collisions: {data['collisions']} Windows analyzed: {data['windows_analyzed']} Collision rate: {data['collision_rate']:.6f}")

print_intra_results("MD5", intra_md5)
print_intra_results("SHA-256", intra_sha)
print_intra_results("Poseidon", intra_pos)


print("\n9. Bit Position Analysis:")
def print_bit_stats(name, bit_data, threshold=5.0, num_bits=None):
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

# ------------------------------
# Preparazione dati
# ------------------------------

# Avalanche (media, std)
avalanche_means = [avalanche_md5[0], avalanche_sha[0], avalanche_pos[0]]
avalanche_stds = [avalanche_md5[1], avalanche_sha[1], avalanche_pos[1]]

# Collisioni
collisions = [coll_md5, coll_sha, coll_pos]

# Uniformità dei bit (numero di 1)
bits_ones = [bits_md5[1], bits_sha[1], bits_pos[1]]

# Shannon Entropy
entropies = [entropy_md5, entropy_sha, entropy_pos]

# Chi-Square
chi_squares = [chi_sq_md5, chi_sq_sha, chi_sq_pos]
p_values = [p_md5, p_sha, p_pos]

# Funzioni di hash
labels = ['MD5', 'SHA-256', 'Poseidon']

# ------------------------------
# Creazione figure
# ------------------------------

fig, axs = plt.subplots(2, 3, figsize=(18, 10))
fig.suptitle("Confronto Hash Functions: MD5 vs SHA-256 vs Poseidon", fontsize=16)

# ------------------------------
# Plot 1 - Avalanche Effect
# ------------------------------
axs[0, 0].bar(labels, avalanche_means, yerr=avalanche_stds, capsize=5, color=['skyblue', 'lightgreen', 'salmon'])
axs[0, 0].set_ylabel('Percentuale di bit cambiati')
axs[0, 0].set_title('Avalanche Effect')

# ------------------------------
# Plot 2 - Collisioni
# ------------------------------
axs[0, 1].bar(labels, collisions, color=['skyblue', 'lightgreen', 'salmon'])
axs[0, 1].set_ylabel('Numero collisioni')
axs[0, 1].set_title('Collisioni (su input condivisi)')

# ------------------------------
# Plot 3 - Uniformità dei Bit
# ------------------------------
axs[0, 2].bar(labels, bits_ones, color=['skyblue', 'lightgreen', 'salmon'])
axs[0, 2].set_ylabel('Numero totale di bit = 1')
axs[0, 2].set_title('Uniformità dei Bit (conteggio di 1)')

# ------------------------------
# Plot 4 - Shannon Entropy
# ------------------------------
axs[1, 0].bar(labels, entropies, color=['skyblue', 'lightgreen', 'salmon'])
axs[1, 0].set_ylabel('Entropia')
axs[1, 0].set_title('Shannon Entropy')

# ------------------------------
# Plot 5 - Chi-Square
# ------------------------------
axs[1, 1].bar(labels, chi_squares, color=['skyblue', 'lightgreen', 'salmon'])
axs[1, 1].set_ylabel('Chi-Square')
axs[1, 1].set_title('Chi-Square Test (Uniformità dei byte)')

# Annotazione p-value sopra ogni barra
for i, p in enumerate(p_values):
    axs[1, 1].text(i, chi_squares[i], f"p={p:.2e}", ha='center', va='bottom', fontsize=9)

# ------------------------------
# Plot 6 - Placeholder / Autocorrelazione
# ------------------------------
# Ad esempio visualizza la media dell’autocorrelazione per un confronto compatto
auto_means = [np.mean(autocorr_md5), np.mean(autocorr_sha), np.mean(autocorr_pos)]
axs[1, 2].bar(labels, auto_means, color=['skyblue', 'lightgreen', 'salmon'])
axs[1, 2].set_ylabel('Media autocorrelazione')
axs[1, 2].set_title('Autocorrelazione media')

# ------------------------------
# Layout finale
# ------------------------------
plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.show()


