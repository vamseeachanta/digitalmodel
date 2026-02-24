import pandas as pd
import numpy as np

# ---------- CONFIGURE ----------
#SRC_RANGE = "B11:CE36012"    # Adjust for your pivot table location
BIN_SIZE  = xl("R2")             # <-- user-specified bin width (change as needed)
# --------------------------------

# ---- Rainflow: pure python stack-based implementation ----
def rainflow(series):
    """
    Return list of cycles as tuples: (range, mean, count, high, low)
    count is 1.0 for full cycles and 0.5 for half-cycles.
    """
    vals = [float(x) for x in series if pd.notna(x)]
    if len(vals) < 2:
        return []

    # remove consecutive duplicates
    comp = [vals[0]]
    for v in vals[1:]:
        if v != comp[-1]:
            comp.append(v)
    if len(comp) < 2:
        return []

    # build turning points
    tp = [comp[0]]
    for i in range(1, len(comp)-1):
        if (comp[i] > comp[i-1] and comp[i] >= comp[i+1]) or (comp[i] < comp[i-1] and comp[i] <= comp[i+1]):
            tp.append(comp[i])
    tp.append(comp[-1])

    stack, cycles = [], []
    for x in tp:
        stack.append(x)
        while len(stack) >= 3:
            s0, s1, s2 = stack[-3], stack[-2], stack[-1]
            R1 = abs(s1 - s0)
            R2 = abs(s2 - s1)
            if R1 >= R2:
                rng = R2
                mean = (s2 + s1) / 2.0
                mx, mn = max(s2, s1), min(s2, s1)
                cycles.append((rng, mean, 1.0, mx, mn))
                del stack[-2]
            else:
                break

    # half cycles from remaining stack
    for i in range(len(stack) - 1):
        a, b = stack[i], stack[i+1]
        rng = abs(b - a)
        mean = (b + a) / 2.0
        mx, mn = max(a, b), min(a, b)
        cycles.append((rng, mean, 0.5, mx, mn))

    return cycles


def rainflow_summary_from_df(df, bin_size):
    """Bin cycles into user-specified range bins."""
    traces = [c for c in df.columns if str(c).strip().lower() != "time"]
    results = []

    for trace in traces:
        series = df[trace].dropna().values
        if len(series) < 2:
            continue

        cycles = rainflow(series)
        if not cycles:
            continue

        ranges = np.array([c[0] for c in cycles])
        means  = np.array([c[1] for c in cycles])
        counts = np.array([c[2] for c in cycles])
        highs  = np.array([c[3] for c in cycles])
        lows   = np.array([c[4] for c in cycles])

        # Create bin edges from min to max with user bin size
        r_min, r_max = ranges.min(), ranges.max()
        bins = np.arange(np.floor(r_min/bin_size)*bin_size,
                         np.ceil(r_max/bin_size)*bin_size + bin_size,
                         bin_size)

        digitized = np.digitize(ranges, bins)

        for i in range(1, len(bins)):
            mask = digitized == i
            if not np.any(mask):
                continue
            count_sum = counts[mask].sum()
            mean_avg  = np.average(means[mask], weights=counts[mask]) if count_sum > 0 else np.mean(means[mask])
            max_high  = np.max(highs[mask])
            min_low   = np.min(lows[mask])
            results.append({
                "Trace": trace,
                "RangeBinLow": float(bins[i-1]),
                "RangeBinHigh": float(bins[i]),
                "Mean": float(mean_avg),
                "Count": float(count_sum),
                "MaxHigh": float(max_high),
                "MinLow": float(min_low)
            })

    return pd.DataFrame(results)


# ---- ENTRY / Execute ----
df_src = xl("'Combined Time History'!B11:CE36012", headers=True)
df_src = pd.DataFrame(df_src)
summary_df = rainflow_summary_from_df(df_src, BIN_SIZE)

summary_df