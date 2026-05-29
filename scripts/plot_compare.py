#!/usr/bin/env python3
"""
Cactus plot comparing Herdi's tool vs Wu's tool.

X axis: number of tests solved (sorted by time taken)
Y axis: time to solve (log scale)

Each point (x, y) means: "the x-th fastest solve took y seconds."
A wider line = more tests solved = better coverage.
A lower line = faster solves = better performance.

Usage:
    python plot_cactus.py <my_csv> <wu_csv> [output.pdf]

My CSV columns:   time-stamp, java-file, seed, smt-result, error-message, smt-time, total-time
Wu's CSV columns: java-file, result, msg, size, smt-time, total-time
"""

import sys
import csv
import re
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


MY_TIMEOUT = 60.0   # Herdi's single-attempt timeout (seconds)
WU_TIMEOUT = 120.0  # Wu's two-attempt timeout (seconds)


# ── helpers ───────────────────────────────────────────────────────────────────

def parse_seconds_mine(s: str) -> float:
    return float(s.rstrip("s"))


def parse_seconds_wu(s: str) -> float:
    return float(s)


def extract_test_id(path: str) -> str | None:
    m = re.search(r"(Test\d+)\.java", path)
    return m.group(1) if m else None


# ── loaders ───────────────────────────────────────────────────────────────────

def load_mine(path: str) -> dict:
    rows = {}
    with open(path, newline="") as f:
        for row in csv.DictReader(f):
            tid = extract_test_id(row["java-file"])
            if tid is None:
                continue
            rows[tid] = {
                "smt_result": row["smt-result"].strip(),
                "smt_time":   parse_seconds_mine(row["smt-time"]),
                "total_time": parse_seconds_mine(row["total-time"]),
            }
    return rows


def load_wu(path: str) -> dict:
    rows = {}
    with open(path, newline="") as f:
        for row in csv.DictReader(f):
            tid = extract_test_id(row["java-file"])
            if tid is None:
                continue
            rows[tid] = {
                "result":     row["result"].strip(),
                "smt_time":   parse_seconds_wu(row["smt-time"]),
                "total_time": parse_seconds_wu(row["total-time"]),
            }
    return rows


# ── plot ──────────────────────────────────────────────────────────────────────

def cactus(my_data: dict, wu_data: dict, out_path: Path):
    common = my_data.keys() & wu_data.keys()

    # exclude tests Wu can't handle at all — not a fair comparison point
    valid = {t for t in common
             if wu_data[t]["result"] not in ("Unsupported", "Parsing Error")}

    my_solved = sorted(
        [my_data[t]["total_time"] for t in valid
         if my_data[t]["smt_result"] == "Unsat"]
    )
    wu_solved = sorted(
        [wu_data[t]["total_time"] for t in valid
         if wu_data[t]["result"] == "Verified"]
    )

    fig, ax = plt.subplots(figsize=(6, 4))

    ax.plot(range(1, len(my_solved) + 1), my_solved,
            color="steelblue", linewidth=1.5, label=f"C2tv ({len(my_solved)} solved)")
    ax.plot(range(1, len(wu_solved) + 1), wu_solved,
            color="tomato", linewidth=1.5, label=f"Wu's tool ({len(wu_solved)} solved)")

    ax.axhline(MY_TIMEOUT, color="steelblue", linestyle=":", linewidth=1.0,
               label=f"C2tv timeout ({MY_TIMEOUT}s)")
    ax.axhline(WU_TIMEOUT, color="tomato",    linestyle=":", linewidth=1.0,
               label=f"Wu's tool timeout ({WU_TIMEOUT}s)")

    ax.set_xlabel("Tests solved (sorted by time)")
    ax.set_ylabel("Total time (s)")
    ax.set_yscale("log")
    ax.set_xlim(left=0)
    ax.legend(fontsize=8)
    ax.grid(True, which="both", linestyle="--", alpha=0.3)

    fig.tight_layout()
    fig.savefig(out_path, format="pdf")
    print(f"Written: {out_path}")
    print(f"  Herdi solved: {len(my_solved)} / {len(valid)}")
    print(f"  Wu    solved: {len(wu_solved)} / {len(valid)}")


# ── main ──────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) < 3:
        print("Usage: python plot_cactus.py <my_csv> <wu_csv> [output.pdf]")
        sys.exit(1)

    my_data = load_mine(sys.argv[1])
    wu_data = load_wu(sys.argv[2])
    out_path = Path(sys.argv[3]) if len(sys.argv) > 3 else Path("cactus.pdf")

    cactus(my_data, wu_data, out_path)


if __name__ == "__main__":
    main()
