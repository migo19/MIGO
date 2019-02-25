import numpy as np
import random
import pandas as pd
import matplotlib.pyplot as plt
import commands

n_trials = 20 # number of experiments run
n_it = 200 # number of iterations

score_transfer = np.zeros((n_trials,n_it))
score_no_transfer = np.zeros((n_trials,n_it))
time_transfer = np.zeros((n_trials,n_it+1))
time_no_transfer = np.zeros((n_trials,n_it+1))

for k in range(0,n_trials):
    with open('score%s.txt' %k,'r') as f:
        results = f.readlines()
        for p in range(0,len(results)-1):
            score_transfer[k,p] = results[p]
    with open('time%s.txt' %k,'r') as f:
        results = f.readlines()
        for p in range(0,len(results)):
            time_transfer[k,p] = results[p]
    print(score_transfer[k,:])
    print(time_transfer[k,:])

    with open('score_nt%s.txt' %k,'r') as f:
        results = f.readlines()
        for p in range(0,len(results)-1):
            score_no_transfer[k,p] = results[p]
    with open('time_nt%s.txt' %k,'r') as f:
        results = f.readlines()
        for p in range(0,len(results)):
            time_no_transfer[k,p] = results[p]
    print(score_no_transfer[k,:])
    print(time_no_transfer[k,:])

score_transfer = np.cumsum(score_transfer, axis = 1)
score_transfer_av = np.mean(score_transfer, axis = 0)
score_transfer_std = np.std(score_transfer, axis = 0)

score_no_transfer = np.cumsum(score_no_transfer, axis = 1)
score_no_transfer_av = np.mean(score_no_transfer, axis = 0)
score_no_transfer_std = np.std(score_no_transfer, axis = 0)

time_transfer_av = np.mean(time_transfer, axis = 0)
time_transfer_std = np.std(time_transfer, axis = 0)

time_no_transfer_av = np.mean(time_no_transfer, axis = 0)
time_no_transfer_std = np.std(time_no_transfer, axis = 0)

plt.figure()
range_n = range(1,n_it+1)
plt.errorbar(range_n, score_transfer_av[:n_it], score_transfer_std[:n_it], fmt='o-', capsize=3, capthick=1, label = 'Transfer from OX')
plt.errorbar(range_n, score_no_transfer_av[:n_it], score_no_transfer_std[:n_it], fmt='o-', capsize=3, capthick=1, label = 'No Transfer')
#plt.title("Cumulative Minimax Regret")
plt.axis([0, n_it+1, None, None])
plt.grid(True)
plt.xlabel('Number of plays', fontsize=40)
plt.ylabel('Cumulative Minimax Regret', fontsize=40)
plt.legend(loc='upper left', fontsize = 'x-large')
plt.show()

plt.figure()
range_n = range(1,n_it+1)
plt.errorbar(range_n, time_transfer_av[:n_it], time_transfer_std[:n_it], fmt='o-', capsize=3, capthick=1, label = 'Transfer from OX')
plt.errorbar(range_n, time_no_transfer_av[:n_it], time_no_transfer_std[:n_it], fmt='o-', capsize=3, capthick=1, label = 'No Transfer')
plt.title("Running time (s)")
plt.axis([0, n_it+1, None, None])
plt.grid(True)
plt.xlabel('Number of plays', fontsize=20)
plt.ylabel('Cumulative Time', fontsize=20)
plt.legend(loc='upper left', fontsize = 'x-large')
plt.show()
