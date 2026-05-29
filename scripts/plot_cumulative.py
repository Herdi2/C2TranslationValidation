#!/usr/bin/env python3
"""
Plot cumulative verified ratio over per-test total time (log x-axis),
styled after Wu et al. with annotated timeout marker.

Usage:
    python plot_cumulative.py <my_csv> [output.pdf]
"""

import sys
import csv
import re
from pathlib import Path
from datetime import datetime
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


def parse_seconds(s: str) -> float:
    return float(s.rstrip("s"))


def parse_timestamp(s: str) -> datetime:
    s = re.sub(r'(\.\d{6})\d+', r'\1', s).replace(" UTC", "")
    return datetime.strptime(s, "%Y-%m-%d %H:%M:%S.%f")


def load(path: str) -> list[dict]:
    rows = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append({
                "timestamp":  parse_timestamp(row["time-stamp"]),
                "smt_result": row["smt-result"].strip(),
                "total_time": parse_seconds(row["total-time"]),
            })
    rows.sort(key=lambda r: r["timestamp"])
    return rows


def ratio_at(sorted_times, ratios, t):
    """Ratio at a given time threshold (fraction of tests with total_time <= t that are verified)."""
    idx = np.searchsorted(sorted_times, t, side='right') - 1
    if idx < 0:
        return 0.0
    return ratios[idx]


def main():
    if len(sys.argv) < 2:
        print("Usage: python plot_cumulative.py <my_csv> [output.pdf]")
        sys.exit(1)

    csv_path = sys.argv[1]
    out_path = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("cumulative_verified.pdf")

    rows = load(csv_path)
    n = len(rows)

    # sort by total_time for the cumulative plot
    rows_by_time = sorted(rows, key=lambda r: r["total_time"])

    times  = [0.0]
    ratios = [0.0]
    verified = 0

    for i, row in enumerate(rows_by_time, start=1):
        if row["smt_result"] == "Unsat":
            verified += 1
        times.append(row["total_time"])
        ratios.append(verified / n)  # fraction of ALL n tests

    times  = np.array(times)
    ratios = np.array(ratios)

    # annotation at timeout threshold
    timeout = 45.0
    r_at_timeout = ratio_at(times, ratios, timeout)

    fig, ax = plt.subplots(figsize=(5, 4))
    ax.plot(times, ratios * 100, linewidth=1.5, color="steelblue")

    # 1s marker
    one_sec = 1.0
    r_at_one = ratio_at(times, ratios, one_sec)
    ax.axvline(one_sec, color="red", linestyle="--", linewidth=1.0)
    ax.plot(one_sec, r_at_one * 100, "o", color="steelblue", markersize=5, zorder=5)
    ax.annotate(f"{r_at_one*100:.2f}%",
                xy=(one_sec, r_at_one * 100),
                xytext=(8, -12),
                textcoords="offset points",
                fontsize=9)

    # timeout marker
    ax.axvline(timeout, color="red", linestyle="--", linewidth=1.0)
    ax.plot(timeout, r_at_timeout * 100, "o", color="steelblue", markersize=5, zorder=5)
    ax.annotate(f"{r_at_timeout*100:.2f}%",
                xy=(timeout, r_at_timeout * 100),
                xytext=(8, -12),
                textcoords="offset points",
                fontsize=9)

    ax.set_xscale("log")
    ax.set_xlabel("Time (s)")
    ax.set_ylabel("Ratio")
    ax.set_ylim(0, 100)
    ax.yaxis.set_major_formatter(matplotlib.ticker.PercentFormatter())
    ax.grid(True, which="both", linestyle="--", alpha=0.3)
    fig.tight_layout()
    fig.savefig(out_path, format="pdf")
    print(f"Written: {out_path}  (ratio at {timeout}s timeout: {r_at_timeout*100:.2f}%)")


if __name__ == "__main__":
    main()
