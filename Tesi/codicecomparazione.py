import hashlib
import random
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict, Counter
import time
import os
from typing import List
from tqdm import tqdm

# ------------------------------
# Implementazione Poseidon256 (basata sul paper)
# ------------------------------
PRIME = 2**256 - 2**32 - 977  # Primo vicino a 2^256 (usato in Ethereum)
T = 3  # Dimensione stato (2 input, 1 capacità)
ROUNDS_F = 8  # Round completi
ROUNDS_P = 56  # Round parziali
ALPHA = 5  # Esponente S-box

# Costanti precalcolate (esempio semplificato)
ROUND_CONSTANTS = [(i * 0xdeadbeef) % PRIME for i in range(1, (ROUNDS_F + ROUNDS_P) * T + 1)]
MDS_MATRIX = np.array([
    [0x123456789, 0x987654321, 0xabcdef12],
    [0x456789abc, 0x321fedcba, 0x789abcdef],
    [0x987654321, 0xabcdef123, 0x654321fed]
], dtype=object)

def add(a: int, b: int) -> int:
    return (a + b) % PRIME

def mul(a: int, b: int) -> int:
    return (a * b) % PRIME

def pow_alpha(x: int) -> int:
    return pow(x, ALPHA, PRIME)

def poseidon_permutation(state: List[int]) -> List[int]:
    """Permutazione Poseidon con round completi e parziali."""
    # Round completi iniziali
    for r in range(ROUNDS_F // 2):
        state = [add(state[i], ROUND_CONSTANTS[r * T + i]) for i in range(T)]
        state = [pow_alpha(x) for x in state]
        state = [int(sum(mul(MDS_MATRIX[i][j], state[j]) for j in range(T))) % PRIME for i in range(T)]
    
    # Round parziali
    for r in range(ROUNDS_P):
        state = [add(state[i], ROUND_CONSTANTS[(ROUNDS_F // 2 + r) * T + i]) for i in range(T)]
        state[0] = pow_alpha(state[0])
        state = [int(sum(mul(MDS_MATRIX[i][j], state[j]) for j in range(T))) % PRIME for i in range(T)]
    
    # Round completi finali
    for r in range(ROUNDS_F // 2, ROUNDS_F):
        state = [add(state[i], ROUND_CONSTANTS[r * T + i]) for i in range(T)]
        state = [pow_alpha(x) for x in state]
        state = [int(sum(mul(MDS_MATRIX[i][j], state[j]) for j in range(T))) % PRIME for i in range(T)]
    
    return state

def poseidon256(msg: bytes) -> bytes:
    """Hash Poseidon256 per messaggi fino a 32 byte."""
    if len(msg) > 32:
        msg = msg[:32]  # Tronca a 32 byte
    elif len(msg) < 32:
        msg = msg.ljust(32, b'\x00')  # Padding a 32 byte
    
    x = int.from_bytes(msg[:16], 'big') % PRIME
    y = int.from_bytes(msg[16:32], 'big') % PRIME
    state = [x, y, 0]
    state = poseidon_permutation(state)
    return state[0].to_bytes(32, 'big')

# ------------------------------
# Configurazione
# ------------------------------
NUM_SAMPLES = 10000  # 100k sample
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
shared_inputs = [os.urandom(INPUT_SIZE) for _ in range(NUM_SAMPLES)]

# ------------------------------
# 1. Benchmark Prestazioni (Tempo medio di hashing)
# ------------------------------
def benchmark(hash_func):
    start = time.time()
    for msg in tqdm(shared_inputs, desc=f"Benchmark {hash_func.__name__}"):
        hash_func(msg)
    return (time.time() - start) * 1000 / NUM_SAMPLES  # ms per hash



time_md5 = benchmark(hash_md5)
time_sha = benchmark(hash_sha256)
time_pos = benchmark(hash_poseidon)

# ------------------------------
# 2. Avalanche Effect (su primo input)
# ------------------------------
def avalanche(hash_func):
    original = shared_inputs[0]
    modified = bytearray(original)
    modified[0] ^= 0x01  # Flip del primo bit
    h_orig = hash_func(original)
    h_mod = hash_func(bytes(modified))
    return sum(bin(a ^ b).count('1') for a, b in zip(h_orig, h_mod)) / (len(h_orig) * 8) * 100  # % bit cambiati

avalanche_md5 = avalanche(hash_md5)
avalanche_sha = avalanche(hash_sha256)
avalanche_pos = avalanche(hash_poseidon)

# ------------------------------
# 3. Analisi Collisioni (su tutti gli input)
# ------------------------------
def check_collisions(hash_func):
    hashes = set()
    collisions = 0
    for msg in tqdm(shared_inputs, desc=f"Collisioni {hash_func.__name__}"):
        h = hash_func(msg)
        if h in hashes:
            collisions += 1
        hashes.add(h)
    return collisions

coll_md5 = check_collisions(hash_md5)
coll_sha = check_collisions(hash_sha256)
coll_pos = check_collisions(hash_poseidon)

# ------------------------------
# 4. Uniformità dei Bit (primi 1000 sample)
# ------------------------------
def bit_uniformity(hash_func, samples=1000):
    bits = [0, 0]
    for msg in tqdm(shared_inputs[:samples], desc=f"Uniformità {hash_func.__name__}"):
        h = hash_func(msg)
        for byte in h:
            bits[0] += bin(byte).count('0')
            bits[1] += bin(byte).count('1')
    return bits

bits_md5 = bit_uniformity(hash_md5)*2
bits_sha = bit_uniformity(hash_sha256)
bits_pos = bit_uniformity(hash_poseidon)

# ------------------------------
# Visualizzazione Risultati
# ------------------------------
# Grafico Prestazioni
plt.figure(figsize=(12, 6))
plt.subplot(2, 2, 1)
plt.bar(['MD5', 'SHA-256', 'Poseidon'], [time_md5, time_sha, time_pos], color=['red', 'green', 'blue'])
plt.title('Tempo medio per hash (ms)')
plt.ylabel('Millisecondi')

# Grafico Avalanche Effect
plt.subplot(2, 2, 2)
plt.bar(['MD5', 'SHA-256', 'Poseidon'], [avalanche_md5, avalanche_sha, avalanche_pos], color=['red', 'green', 'blue'])
plt.title('Avalanche Effect (% bit cambiati)')
plt.ylabel('%')

# Grafico Collisioni
plt.subplot(2, 2, 3)
plt.bar(['MD5', 'SHA-256', 'Poseidon'], [coll_md5, coll_sha, coll_pos], color=['red', 'green', 'blue'])
plt.title(f'Collisioni su {NUM_SAMPLES} input')
plt.ylabel('N. collisioni')

# Grafico Uniformità Bit
plt.subplot(2, 2, 4)
plt.bar(['MD5-0', 'MD5-1', 'SHA-0', 'SHA-1', 'POS-0', 'POS-1'], 
        [bits_md5[0], bits_md5[1], bits_sha[0], bits_sha[1], bits_pos[0], bits_pos[1]],
        color=['red', 'red', 'green', 'green', 'blue', 'blue'])
plt.title('Distribuzione bit 0/1')
plt.ylabel('Conteggio')

plt.tight_layout()
plt.show()

# ------------------------------
# Dati Numerici
# ------------------------------
print("\nRISULTATI RIEPILOGO:")
print(f"1. Prestazioni (ms/hash): MD5={time_md5:.4f}, SHA-256={time_sha:.4f}, Poseidon={time_pos:.4f}")
print(f"2. Avalanche Effect (%): MD5={avalanche_md5:.2f}%, SHA-256={avalanche_sha:.2f}%, Poseidon={avalanche_pos:.2f}%")
print(f"3. Collisioni: MD5={coll_md5}, SHA-256={coll_sha}, Poseidon={coll_pos}")
print(f"4. Uniformità bit 0/1: MD5={bits_md5[0]}/{bits_md5[1]}, SHA-256={bits_sha[0]}/{bits_sha[1]}, Poseidon={bits_pos[0]}/{bits_pos[1]}")