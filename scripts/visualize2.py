"""
visualize_comparison.py - Compare translation validation tool results.
 
Usage:
    python3 visualize_comparison.py --mine verify.csv --wu report.csv [--output results/]
"""
 
import argparse
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from pathlib import Path
 
 
# ── Parsing ──────────────────────────────────────────────────────────────────
 
def parse_time(t):
    """Accept '4.747s' (Haskell style) or plain float (Wu's CSV)."""
    if isinstance(t, str):
        return float(t.replace('s', ''))
    return float(t)
 
 
def load_mine(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df['smt_time']   = df['smt-time'].apply(parse_time)
    df['total_time'] = df['total-time'].apply(parse_time)
    df['enc_time']   = df['total_time'] - df['smt_time']
    df['test_num']   = df['java-file'].str.extract(r'Test(\d+)\.java').astype(int)
    df = df.rename(columns={'smt-result': 'result'})
    return df[['test_num', 'result', 'smt_time', 'total_time', 'enc_time']]
 
 
def load_wu(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df['smt_time']   = df['smt-time'].astype(float)
    df['total_time'] = df['total-time'].astype(float)
    df['enc_time']   = df['total_time'] - df['smt_time']
    df['test_num']   = df['java-file'].str.extract(r'Test(\d+)\.java').astype(int)
    return df[['test_num', 'result', 'smt_time', 'total_time', 'enc_time']]
 
 
def merge(mine: pd.DataFrame, wu: pd.DataFrame) -> pd.DataFrame:
    m = mine.merge(wu, on='test_num', suffixes=('_mine', '_wu'))
    return m
 
 
# ── Statistics ───────────────────────────────────────────────────────────────
 
def geo_mean(values):
    v = np.array(values, dtype=float)
    v = v[np.isfinite(v) & (v > 0)]
    if len(v) == 0:
        return float('nan')
    return np.exp(np.mean(np.log(v)))
 
 
def print_summary(merged: pd.DataFrame):
    total = len(merged)
 
    my_success  = merged['result_mine'].isin(['Unsat'])
    wu_success  = merged['result_wu'].isin(['Verified'])
    my_timeout  = merged['result_mine'].isin(['Unknown'])
    wu_timeout  = merged['result_wu'].isin(['Timeout'])
    my_error    = ~my_success & ~my_timeout
    wu_error    = ~wu_success & ~wu_timeout
 
    both_ok     = my_success & wu_success
    only_mine   = my_success & wu_timeout
    only_wu     = wu_success & my_timeout
    both_to     = my_timeout & wu_timeout
 
    print("=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Total programs:               {total}")
    print(f"Both succeeded:               {both_ok.sum()}")
    print(f"Only mine succeeded:          {only_mine.sum()}  (Wu timed out)")
    print(f"Only Wu succeeded:            {only_wu.sum()}  (mine timed out)")
    print(f"Both timed out:               {both_to.sum()}")
    print(f"My errors (parse/build):      {my_error.sum()}")
    print(f"Wu errors/unsupported:        {wu_error.sum()}")
    print()
 
    sub = merged[both_ok].copy()
    speedup_smt   = sub['smt_time_wu']   / sub['smt_time_mine']
    speedup_total = sub['total_time_wu'] / sub['total_time_mine']
    speedup_enc   = sub['enc_time_wu']   / sub['enc_time_mine']
 
    # Filter zeros for ratios
    mask_smt = (sub['smt_time_mine'] > 0) & (sub['smt_time_wu'] > 0)
    mask_enc = (sub['enc_time_mine'] > 0) & (sub['enc_time_wu'] > 0)
 
    print("On programs both tools solved successfully:")
    print(f"  Geomean SMT speedup   (mine vs Wu): {geo_mean(speedup_smt[mask_smt]):.3f}x")
    print(f"  Geomean total speedup (mine vs Wu): {geo_mean(speedup_total):.3f}x")
    print(f"  Geomean enc  speedup  (mine vs Wu): {geo_mean(speedup_enc[mask_enc]):.3f}x")
    print()
    print(f"  Median SMT   time — mine: {sub['smt_time_mine'].median():.4f}s  Wu: {sub['smt_time_wu'].median():.4f}s")
    print(f"  Median enc   time — mine: {sub['enc_time_mine'].median():.4f}s  Wu: {sub['enc_time_wu'].median():.4f}s")
    print(f"  Median total time — mine: {sub['total_time_mine'].median():.4f}s  Wu: {sub['total_time_wu'].median():.4f}s")
    print("=" * 60)
 
 
# ── Plots ─────────────────────────────────────────────────────────────────────
 
COLORS = {
    'mine':     '#4C9BE8',
    'wu':       '#E8834C',
    'diagonal': '#888888',
    'noise':    '#EEEEEE',
}
 
def _log_scatter(ax, x, y, xlabel, ylabel, title, noise_threshold=None):
    """Scatter plot on log-log scale with diagonal and optional noise region."""
    ax.set_xscale('log')
    ax.set_yscale('log')
 
    if noise_threshold:
        ax.axhspan(0, noise_threshold, alpha=0.15, color=COLORS['noise'], zorder=0)
        ax.axvspan(0, noise_threshold, alpha=0.15, color=COLORS['noise'], zorder=0)
 
    lims = [
        min(x.min(), y.min()) * 0.8,
        max(x.max(), y.max()) * 1.2,
    ]
    ax.plot(lims, lims, '--', color=COLORS['diagonal'], linewidth=1, alpha=0.7, label='Equal')
    ax.set_xlim(lims)
    ax.set_ylim(lims)
 
    ax.scatter(x, y, alpha=0.4, s=12, color=COLORS['mine'], edgecolors='none')
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.legend(fontsize=8)
    ax.xaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f'{v:g}s'))
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f'{v:g}s'))
 
 
def plot_scatter(merged: pd.DataFrame, output_dir: Path):
    both_ok = merged[
        merged['result_mine'].isin(['Unsat']) &
        merged['result_wu'].isin(['Verified'])
    ].copy()
 
    # Replace zeros with small epsilon so log scale works
    eps = 1e-4
    for col in ['smt_time_mine', 'smt_time_wu', 'total_time_mine', 'total_time_wu']:
        both_ok[col] = both_ok[col].clip(lower=eps)
 
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    fig.suptitle('Per-program timing comparison (log-log scale)\nPoints below diagonal = mine is faster', fontsize=12)
 
    _log_scatter(
        axes[0],
        both_ok['smt_time_wu'], both_ok['smt_time_mine'],
        "Wu SMT time (s)", "My SMT time (s)",
        "SMT solve time",
        noise_threshold=0.05,
    )
    _log_scatter(
        axes[1],
        both_ok['total_time_wu'], both_ok['total_time_mine'],
        "Wu total time (s)", "My total time (s)",
        "Total time",
        noise_threshold=0.05,
    )
 
    plt.tight_layout()
    out = output_dir / 'scatter.png'
    plt.savefig(out, dpi=150)
    plt.close()
    print(f"Saved: {out}")
 
 
def plot_cdf(merged: pd.DataFrame, output_dir: Path):
    """CDF of SMT solve time for each tool (successful runs only)."""
    mine_times = merged.loc[merged['result_mine'] == 'Unsat', 'smt_time_mine'].sort_values()
    wu_times   = merged.loc[merged['result_wu']   == 'Verified', 'smt_time_wu'].sort_values()
 
    fig, ax = plt.subplots(figsize=(8, 5))
 
    for times, label, color in [
        (mine_times, 'Mine', COLORS['mine']),
        (wu_times,   'Wu',   COLORS['wu']),
    ]:
        cdf = np.arange(1, len(times) + 1) / len(times)
        ax.plot(times.values, cdf, label=f'{label} (n={len(times)})', color=color, linewidth=1.8)
 
    ax.set_xscale('log')
    ax.set_xlabel('SMT solve time (s)')
    ax.set_ylabel('Fraction of programs solved')
    ax.set_title('CDF of SMT solve time\n(further right = more timeouts / harder programs)')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.xaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f'{v:g}s'))
 
    plt.tight_layout()
    out = output_dir / 'cdf_smt.png'
    plt.savefig(out, dpi=150)
    plt.close()
    print(f"Saved: {out}")
 
 
def plot_encoding_overhead(merged: pd.DataFrame, output_dir: Path):
    """Compare encoding (total - smt) time distributions side by side."""
    both_ok = merged[
        merged['result_mine'].isin(['Unsat']) &
        merged['result_wu'].isin(['Verified'])
    ].copy()
 
    fig, ax = plt.subplots(figsize=(8, 5))
 
    mine_enc = both_ok['enc_time_mine'].clip(lower=1e-4)
    wu_enc   = both_ok['enc_time_wu'].clip(lower=1e-4)
 
    bins = np.logspace(np.log10(1e-4), np.log10(both_ok[['enc_time_mine','enc_time_wu']].max().max() * 1.1), 50)
 
    ax.hist(mine_enc, bins=bins, alpha=0.6, color=COLORS['mine'], label=f'Mine (median {mine_enc.median():.3f}s)')
    ax.hist(wu_enc,   bins=bins, alpha=0.6, color=COLORS['wu'],   label=f'Wu   (median {wu_enc.median():.4f}s)')
 
    ax.set_xscale('log')
    ax.set_xlabel('Encoding overhead (total − SMT) (s)')
    ax.set_ylabel('Number of programs')
    ax.set_title('Encoding overhead distribution')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.xaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f'{v:g}s'))
 
    plt.tight_layout()
    out = output_dir / 'encoding_overhead.png'
    plt.savefig(out, dpi=150)
    plt.close()
    print(f"Saved: {out}")
 
 
def plot_outcome_breakdown(merged: pd.DataFrame, output_dir: Path):
    """2×2 outcome matrix as a simple bar chart."""
    my_ok  = merged['result_mine'].isin(['Unsat'])
    wu_ok  = merged['result_wu'].isin(['Verified'])
    my_to  = merged['result_mine'].isin(['Unknown'])
    wu_to  = merged['result_wu'].isin(['Timeout'])
 
    categories = {
        'Both succeeded':        (my_ok  & wu_ok).sum(),
        'Only mine succeeded\n(Wu timeout)':  (my_ok  & wu_to).sum(),
        'Only Wu succeeded\n(mine timeout)':  (my_to  & wu_ok).sum(),
        'Both timed out':        (my_to  & wu_to).sum(),
        'Error / other':         (~my_ok & ~my_to | ~wu_ok & ~wu_to).sum(),
    }
 
    fig, ax = plt.subplots(figsize=(8, 4))
    bars = ax.barh(list(categories.keys()), list(categories.values()),
                   color=[COLORS['mine'], COLORS['mine'], COLORS['wu'], '#AAAAAA', '#CCCCCC'])
    ax.bar_label(bars, padding=4)
    ax.set_xlabel('Number of programs')
    ax.set_title('Outcome breakdown (n=1000)')
    ax.set_xlim(0, max(categories.values()) * 1.15)
    plt.tight_layout()
 
    out = output_dir / 'outcome_breakdown.png'
    plt.savefig(out, dpi=150)
    plt.close()
    print(f"Saved: {out}")
 
 
# ── Main ──────────────────────────────────────────────────────────────────────
 
def main():
    parser = argparse.ArgumentParser(description='Compare translation validation tool results.')
    parser.add_argument('--mine', required=True, help='Path to your verify.csv')
    parser.add_argument('--wu',   required=True, help="Path to Wu's report.csv")
    parser.add_argument('--output', default='results', help='Output directory for plots')
    args = parser.parse_args()
 
    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)
 
    print(f"Loading {args.mine} ...")
    mine = load_mine(args.mine)
    print(f"Loading {args.wu} ...")
    wu   = load_wu(args.wu)
 
    merged = merge(mine, wu)
 
    print_summary(merged)
 
    print("\nGenerating plots...")
    plot_scatter(merged, output_dir)
    plot_cdf(merged, output_dir)
    plot_encoding_overhead(merged, output_dir)
    plot_outcome_breakdown(merged, output_dir)
    print("Done.")
 
 
if __name__ == '__main__':
    main()
