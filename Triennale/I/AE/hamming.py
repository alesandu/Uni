def calcola_sindrome(hex_string):
    # Determina il numero di bit dalla lunghezza della stringa esadecimale
    num_bits = len(hex_string) * 4
    # Converte in binario e formatta per avere tutti i bit (inclusi gli zeri iniziali)
    bin_string = bin(int(hex_string, 16))[2:].zfill(num_bits)
    
    # Determina quanti bit di parità (r) servono (cioè coprono fino a num_bits)
    r = 0
    while (2 ** r) <= num_bits:
        r += 1
        
    syndrome_bits = []
    
    # Calcola il bit di parità per ogni posizione potenza di 2
    # partendo dal più significativo fino al meno significativo (c8, c4, c2, c1...)
    for i in range(r - 1, -1, -1):
        pos = 2 ** i
        parity = 0
        for j in range(1, num_bits + 1):
            # Controlla se il bit in posizione j (1-based da destra) appartiene a questo bit di parità
            if (j & pos) != 0:
                # Prende il j-esimo bit partendo da destra
                bit_val = int(bin_string[-j])
                parity ^= bit_val
        syndrome_bits.append(str(parity))
        
    return "".join(syndrome_bits)

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        valore_hex = sys.argv[1].strip()
        if valore_hex:
            print(calcola_sindrome(valore_hex))
    else:
        while True:
            try:
                hex_input = input("Inserisci il valore esadecimale (o 'q' per uscire): ").strip()
                if hex_input.lower() == 'q':
                    break
                if hex_input:
                    print(calcola_sindrome(hex_input))
            except (KeyboardInterrupt, EOFError):
                print()
                break