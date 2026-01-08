#!/usr/bin/env python3
import csv
import glob
from collections import defaultdict

# Find CSVs that include max_border in header
files = glob.glob('scripts/bench_results/*.csv')
rows = []
for f in files:
    with open(f, newline='') as fh:
        r = csv.DictReader(fh)
        if 'max_border' in r.fieldnames:
            for row in r:
                # convert numeric fields
                try:
                    rows.append({
                        'case': row['case'],
                        'n': int(row['text_len']),
                        'm': int(row['pat_len']),
                        'max_border': int(row['max_border']),
                        'index_of_us': int(row['index_of_us']),
                        'index_of_auto_us': int(row['index_of_auto_us']),
                        'kmp_us': int(row['kmp_us']),
                        'sliding_us': int(row['sliding_us']),
                        'iter': int(row.get('iter', '1'))
                    })
                except Exception as e:
                    print('skip row', e)

if not rows:
    print('No CSVs with max_border found under scripts/bench_results')
    raise SystemExit(1)

# Parameter grid
min_m_vals = [8, 16, 24, 32, 48, 64]
large_n_vals = [50_000, 100_000, 200_000]
large_m_vals = [6, 8, 12, 16]
border_mul_vals = [1, 2, 3]

results = []

for min_m in min_m_vals:
    for large_n in large_n_vals:
        for large_m in large_m_vals:
            for border_mul in border_mul_vals:
                total_time = 0
                total_auto_time = 0
                count = 0
                for r in rows:
                    n = r['n']
                    m = r['m']
                    mb = r['max_border']
                    # choose strategy
                    if m == 0:
                        strat = 'Sliding'
                    elif m >= min_m:
                        strat = 'Kmp'
                    elif n >= large_n and m >= large_m:
                        strat = 'Kmp'
                    elif mb * border_mul >= m:
                        strat = 'Kmp'
                    else:
                        strat = 'Sliding'
                    # predicted time for index_of_auto (use kmp_us or sliding_us)
                    t = r['kmp_us'] if strat == 'Kmp' else r['sliding_us']
                    total_time += t
                    total_auto_time += r['index_of_auto_us']
                    count += 1
                # compare: lower is better
                results.append((total_time, min_m, large_n, large_m, border_mul, total_auto_time))

results.sort()

print('Rows analyzed:', len(rows))
print('Top 10 parameter sets minimizing predicted index_of_auto time:')
print('pred_us,min_m,large_n,large_m,border_mul,baseline_auto_us')
for item in results[:10]:
    print(','.join(map(str,item)))

# Show best and percent improvement over baseline
best = results[0]
best_pred = best[0]
baseline = best[5]
impr = (baseline - best_pred) / baseline * 100 if baseline>0 else 0
print('\nBest config: min_m=%d large_n=%d large_m=%d border_mul=%d' % (best[1],best[2],best[3],best[4]))
print('Predicted total auto-time:', best_pred, 'baseline auto-time:', baseline, 'improvement: %.2f%%' % impr)
