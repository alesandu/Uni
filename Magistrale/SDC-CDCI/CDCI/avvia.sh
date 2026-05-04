#!/bin/bash

# Configurazione variabili
DISK_IMAGE="kali-linux-2026.1-qemu-amd64.qcow2"
RAM="4G"
CORES="2"

echo "Avvio di Kali Linux in corso..."

qemu-system-x86_64 \
  -enable-kvm \
  -m $RAM \
  -smp $CORES \
  -drive file="$DISK_IMAGE",format=qcow2,if=virtio
