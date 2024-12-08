import numpy as np
import matplotlib.pyplot as plt
import random

n = 60
p = 0.5
N = 10**3

o1 = np.random.binomial(n, p, N)
o2 = np.ndarray((N))
o2.fill(60)
o2 = o2 - o1

e1 = np.ndarray((N))
e1.fill(int(n / 2))
e2 = e1.copy()

simulated_chi_dist = (o1 - e1)**2 / e1 + (o2 - e2)**2 / e2

plt.hist(simulated_chi_dist)
plt.show()