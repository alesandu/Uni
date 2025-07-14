import numpy as np
import matplotlib.pyplot as plt

# Dati di esempio (sostituisci con i tuoi dati reali)
bits = np.arange(1, 50)
poseidon_collisions = np.random.randint(0, 1_000_000, size=len(bits))  # Esempio
sha256_collisions = np.random.randint(0, 1_000, size=len(bits))        # Esempio

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