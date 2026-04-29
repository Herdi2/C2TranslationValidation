#!/usr/bin/env python3
"""
visualize_times.py — Visualize smt-time and total-time per java-file from benchmark CSVs.

Usage:
    python visualize_times.py results.csv
    python visualize_times.py results1.csv results2.csv          # overlay multiple files
    python visualize_times.py results.csv --filter-result sat    # filter by smt-result
    python visualize_times.py results.csv --out chart.png        # save instead of show
    python visualize_times.py results.csv --sort total-time      # sort bars by a column
"""

import argparse
import sys
from pathlib import Path

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np


# ── Colour palette ──────────────────────────────────────────────────────────
SMT_COLOUR   = "#4e9af1"
TOTAL_COLOUR = "#f07b3f"
GRID_COLOUR  = "#e0e0e0"
ERROR_ALPHA  = 0.35        # alpha for error/timeout bars


def load_csv(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"java-file", "smt-time", "total-time"}
    missing = required - set(df.columns)
    if missing:
        sys.exit(f"[error] {path!r} is missing columns: {missing}")

    df["java-file"] = df["java-file"].apply(lambda p: Path(p).name)
    for col in ("smt-time", "total-time"):
        df[col] = (
            df[col].astype(str)
                   .str.rstrip("s")
                   .pipe(pd.to_numeric, errors="coerce")
                   * 1000  # convert seconds → ms
        )
    return df


def aggregate(df: pd.DataFrame) -> pd.DataFrame:
    """If the same java-file appears multiple times (multiple seeds), aggregate."""
    agg = (
        df.groupby("java-file")
        .agg(
            smt_mean=("smt-time",   "mean"),
            smt_std =("smt-time",   "std"),
            tot_mean=("total-time", "mean"),
            tot_std =("total-time", "std"),
            runs    =("smt-time",   "count"),
        )
        .reset_index()
    )
    agg["smt_std"] = agg["smt_std"].fillna(0)
    agg["tot_std"] = agg["tot_std"].fillna(0)
    return agg


def plot_dot(agg: pd.DataFrame, title: str, out: str | None) -> None:
    """Dot plot: smt-time and total-time per java-file on the x-axis."""
    n = len(agg)
    x = np.arange(n)

    fig, ax = plt.subplots(figsize=(max(10, n * 0.55 + 2), 6))
    fig.patch.set_facecolor("#fafafa")
    ax.set_facecolor("#fafafa")

    # Vertical dotted lines connecting the two dots for each file
    for xi, row in zip(x, agg.itertuples()):
        ax.plot([xi, xi], [row.smt_mean, row.tot_mean],
                color="#ccc", linewidth=1.2, linestyle=":", zorder=2)

    # Error bars (if multiple seeds)
    ax.errorbar(x, agg["smt_mean"], yerr=agg["smt_std"],
                fmt="o", color=SMT_COLOUR, markersize=8,
                capsize=4, linewidth=0, elinewidth=1.5,
                label="smt-time", zorder=4)
    ax.errorbar(x, agg["tot_mean"], yerr=agg["tot_std"],
                fmt="o", color=TOTAL_COLOUR, markersize=8,
                capsize=4, linewidth=0, elinewidth=1.5,
                label="total-time", zorder=4)

    ax.set_xticks(x)
    ax.set_xticklabels(agg["java-file"], rotation=40, ha="right", fontsize=8)
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f"{v:,.0f} ms"))
    ax.set_ylabel("Time (ms)")
    ax.set_title(title, fontsize=13, fontweight="bold", pad=12)
    ax.legend(framealpha=0.7)
    ax.grid(axis="y", color=GRID_COLOUR, linestyle=":", zorder=0)
    ax.spines[["top", "right"]].set_visible(False)

    # Annotate run count if > 1 seed
    if agg["runs"].max() > 1:
        for xi, row in zip(x, agg.itertuples()):
            ax.text(xi, -ax.get_ylim()[1] * 0.04, f"n={row.runs}",
                    ha="center", va="top", fontsize=6.5, color="#888")

    fig.tight_layout()
    _save_or_show(fig, out, "dot_plot")


def plot_scatter(agg: pd.DataFrame, title: str, out: str | None) -> None:
    """Scatter: smt-time (x) vs total-time (y) — reveals overhead."""
    fig, ax = plt.subplots(figsize=(7, 6))
    fig.patch.set_facecolor("#fafafa")
    ax.set_facecolor("#fafafa")

    ax.scatter(agg["smt_mean"], agg["tot_mean"],
               color=SMT_COLOUR, s=70, zorder=3, alpha=0.85)

    # diagonal y = x reference
    lim = max(agg["tot_mean"].max(), agg["smt_mean"].max()) * 1.05
    ax.plot([0, lim], [0, lim], "--", color="#aaa", linewidth=1, label="y = x (no overhead)")

    for _, row in agg.iterrows():
        ax.annotate(row["java-file"], (row["smt_mean"], row["tot_mean"]),
                    fontsize=7, xytext=(4, 4), textcoords="offset points",
                    color="#444")

    ax.set_xlabel("smt-time (ms)")
    ax.set_ylabel("total-time (ms)")
    ax.set_title(f"{title}\nSMT time vs total time", fontsize=12, fontweight="bold")
    ax.legend(fontsize=8)
    ax.grid(color=GRID_COLOUR, zorder=0)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    _save_or_show(fig, out, "scatter")


def plot_overhead(agg: pd.DataFrame, title: str, out: str | None) -> None:
    """Stacked bar: SMT portion vs overhead (total - smt) per file."""
    overhead = (agg["tot_mean"] - agg["smt_mean"]).clip(lower=0)
    n = len(agg)
    x = np.arange(n)

    fig, ax = plt.subplots(figsize=(max(10, n * 0.55 + 2), 5))
    fig.patch.set_facecolor("#fafafa")
    ax.set_facecolor("#fafafa")

    ax.bar(x, agg["smt_mean"], color=SMT_COLOUR,   label="smt-time",      zorder=3)
    ax.bar(x, overhead,        bottom=agg["smt_mean"],
           color=TOTAL_COLOUR, label="overhead (total − smt)", zorder=3, alpha=0.85)

    ax.set_xticks(x)
    ax.set_xticklabels(agg["java-file"], rotation=40, ha="right", fontsize=8)
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda v, _: f"{v:,.0f} ms"))
    ax.set_ylabel("Time (ms)")
    ax.set_title(f"{title}\nSMT vs non-SMT overhead", fontsize=12, fontweight="bold")
    ax.legend(framealpha=0.7)
    ax.grid(axis="y", color=GRID_COLOUR, zorder=0)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    _save_or_show(fig, out, "overhead")


def _save_or_show(fig: plt.Figure, out: str | None, suffix: str) -> None:
    if out:
        base = Path(out)
        path = base.with_stem(f"{base.stem}_{suffix}") if base.suffix else base / suffix
        fig.savefig(path, dpi=150, bbox_inches="tight")
        print(f"[saved] {path}")
        plt.close(fig)
    else:
        plt.show()


# ── CLI ──────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("csvfiles", nargs="+", help="One or more CSV result files")
    p.add_argument("--filter-result", metavar="RESULT",
                   help="Only include rows where smt-result == RESULT (e.g. sat, unsat, timeout)")
    p.add_argument("--sort", choices=["java-file", "smt-time", "total-time"],
                   default="java-file", help="Sort bars by this column (default: java-file)")
    p.add_argument("--out", metavar="PATH",
                   help="Save figures to PATH (suffixes _grouped_bars, _scatter, _overhead added). "
                        "If omitted, figures are displayed interactively.")
    p.add_argument("--no-scatter",   action="store_true", help="Skip the scatter plot")
    p.add_argument("--no-overhead",  action="store_true", help="Skip the overhead stacked bar")
    return p.parse_args()


def main() -> None:
    args = parse_args()

    frames = []
    for path in args.csvfiles:
        df = load_csv(path)
        df["_source"] = Path(path).name
        frames.append(df)

    combined = pd.concat(frames, ignore_index=True)

    if args.filter_result:
        before = len(combined)
        combined = combined[combined["smt-result"] == args.filter_result]
        print(f"[filter] smt-result=={args.filter_result!r}: {before} → {len(combined)} rows")
        if combined.empty:
            sys.exit("[error] No rows remain after filtering.")

    agg = aggregate(combined)

    sort_col = {"java-file": "java-file", "smt-time": "smt_mean", "total-time": "tot_mean"}[args.sort]
    agg = agg.sort_values(sort_col).reset_index(drop=True)

    sources = combined["_source"].unique()
    title = ", ".join(sources) if len(sources) <= 3 else f"{len(sources)} files"

    print(f"\n{'java-file':<40} {'smt-time (ms)':>14} {'total-time (ms)':>16}  runs")
    print("─" * 75)
    for _, row in agg.iterrows():
        print(f"{row['java-file']:<40} {row['smt_mean']:>13.1f}  {row['tot_mean']:>15.1f}  {int(row['runs'])}")
    print()

    plot_dot(agg, title, args.out)
    if not args.no_scatter:
        plot_scatter(agg, title, args.out)
    if not args.no_overhead:
        plot_overhead(agg, title, args.out)


if __name__ == "__main__":
    main()
