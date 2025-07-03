import hashlib
import random
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict, Counter
import time
import os
from typing import List
from tqdm import tqdm
import poseidon

print("aaaaaaaaa")

print("aaaaaaaaa")


NUM_SAMPLES = 10000  # 100k sample
INPUT_SIZE = 1024     # 1 KB per input
RANDOM_SEED = 42 
random.seed(RANDOM_SEED)
shared_inputs = [os.urandom(INPUT_SIZE) for _ in range(NUM_SAMPLES)]