#!/usr/bin/env python3
"""
Generate PGFPlots-ready .dat files from my verification results CSV.

NOTE: This file was fully generated using Claude Sonnet 4.6
      It has been manually verified.

Outputs:
  cumulative_verified.dat  -- cumulative verified ratio over elapsed time
  summary.dat              -- counts of each outcome category
  error_types.dat          -- breakdown of error types

Usage:
    python generate_my_stats.py <my_csv> [output_dir]
"""

import sys
import csv
import re
from pathlib import Path
from datetime import datetime


# ── helpers ───────────────────────────────────────────────────────────────────

def parse_seconds(s: str) -> float:
    return float(s.rstrip("s"))


def parse_timestamp(s: str) -> datetime:
    # "2026-05-19 20:43:01.931946367 UTC"
    # strip nanoseconds to microseconds (Python only handles 6 decimal places)
    s = re.sub(r'(\.\d{6})\d+', r'\1', s).replace(" UTC", "")
    return datetime.strptime(s, "%Y-%m-%d %H:%M:%S.%f")


# ── load ──────────────────────────────────────────────────────────────────────

def load(path: str) -> list[dict]:
    rows = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append({
                "timestamp":     parse_timestamp(row["time-stamp"]),
                "java_file":     row["java-file"].strip(),
                "smt_result":    row["smt-result"].strip(),
                "error_message": row.get("error-message", "").strip(),
                "smt_time":      parse_seconds(row["smt-time"]),
                "total_time":    parse_seconds(row["total-time"]),
            })
    # sort by timestamp just in case
    rows.sort(key=lambda r: r["timestamp"])
    return rows


# ── cumulative verified ratio ─────────────────────────────────────────────────

def cumulative_verified(rows: list[dict], out_path: Path):
    """
    For each test (in timestamp order), emit:
      elapsed_s   -- seconds since first test started
      verified    -- cumulative count of Unsat results
      ratio       -- verified / tests_processed_so_far
    """
    t0 = rows[0]["timestamp"]
    verified = 0
    lines = ["elapsed_s verified total ratio"]

    for i, row in enumerate(rows, start=1):
        elapsed = (row["timestamp"] - t0).total_seconds()
        if row["smt_result"] == "Unsat":
            verified += 1
        # timeouts and errors also count toward total
        ratio = verified / i
        lines.append(f"{elapsed:.3f} {verified} {i} {ratio:.6f}")

    out_path.write_text("\n".join(lines) + "\n")
    print(f"Written: {out_path}")


# ── summary counts ────────────────────────────────────────────────────────────

def summary(rows: list[dict], out_path: Path):
    """
    outcome   count
    """
    counts = {"Unsat": 0, "Sat": 0, "Timeout": 0, "Error": 0}
    for row in rows:
        r = row["smt_result"]
        if r == "Unsat":
            counts["Unsat"] += 1
        elif r == "Sat":
            counts["Sat"] += 1
        elif r == "Unknown":
            counts["Timeout"] += 1
        else:
            counts["Error"] += 1

    lines = ["outcome count"]
    for outcome, count in counts.items():
        lines.append(f"{outcome} {count}")

    out_path.write_text("\n".join(lines) + "\n")
    print(f"Written: {out_path}")
    for outcome, count in counts.items():
        print(f"  {outcome:<10} {count}")


# ── error type breakdown ──────────────────────────────────────────────────────

def error_types(rows: list[dict], out_path: Path):
    """
    error_type   count
    Spaces in labels replaced with underscores for PGFPlots compatibility.
    """
    from collections import Counter
    errors = Counter()
    for row in rows:
        if row["smt_result"] not in ("Unsat", "Sat", "Unknown"):
            # use the smt_result field as type (e.g. BuilderError)
            label = row["smt_result"].replace(" ", "_")
            errors[label] += 1

    lines = ["error_type count"]
    for label, count in sorted(errors.items(), key=lambda x: -x[1]):
        lines.append(f"{label} {count}")

    if not errors:
        lines.append("none 0")

    out_path.write_text("\n".join(lines) + "\n")
    print(f"Written: {out_path}")
    for label, count in errors.items():
        print(f"  {label:<30} {count}")


# ── main ──────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) < 2:
        print("Usage: python generate_my_stats.py <my_csv> [output_dir]")
        sys.exit(1)

    csv_path = sys.argv[1]
    out_dir  = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(".")
    out_dir.mkdir(parents=True, exist_ok=True)

    rows = load(csv_path)
    print(f"Loaded {len(rows)} rows from {csv_path}\n")

    cumulative_verified(rows, out_dir / "cumulative_verified.dat")
    print()
    summary(rows,            out_dir / "summary.dat")
    print()
    error_types(rows,        out_dir / "error_types.dat")


if __name__ == "__main__":
    main()
