#!/usr/bin/env python3
"""
Benchmark comparison: Herdi's tool vs Wu's tool.

Usage:
    python compare_results.py <my_csv> <wu_csv>

My CSV columns:   time-stamp, java-file, seed, smt-result, error-message, smt-time, total-time
Wu's CSV columns: java-file, result, msg, size, smt-time, total-time
"""

import sys
import csv
import re
import math
from pathlib import Path


# ── helpers ──────────────────────────────────────────────────────────────────

def parse_seconds(s: str) -> float:
    """Accept '0.013665628s' or '0.013665628' or '60.032230'."""
    return float(s.rstrip("s"))


def extract_test_id(path: str) -> str | None:
    """Return 'Test42' from any absolute path ending in TestN.java."""
    m = re.search(r"(Test\d+)\.java", path)
    return m.group(1) if m else None


# ── loaders ──────────────────────────────────────────────────────────────────

def load_mine(path: str) -> dict:
    """
    Returns {test_id: {smt_result, smt_time, total_time, error_message}}
    """
    rows = {}
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            tid = extract_test_id(row["java-file"])
            if tid is None:
                continue
            rows[tid] = {
                "smt_result":    row["smt-result"].strip(),
                "error_message": row.get("error-message", "").strip(),
                "smt_time":      parse_seconds(row["smt-time"]),
                "total_time":    parse_seconds(row["total-time"]),
            }
    return rows


def load_wu(path: str) -> dict:
    """
    Returns {test_id: {result, smt_time, total_time, size}}
    """
    rows = {}
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            tid = extract_test_id(row["java-file"])
            if tid is None:
                continue
            rows[tid] = {
                "result":    row["result"].strip(),
                "smt_time":  parse_seconds(row["smt-time"]),
                "total_time": parse_seconds(row["total-time"]),
                "size":      int(row.get("size", 0) or 0),
            }
    return rows


# ── classification ────────────────────────────────────────────────────────────

def my_status(r: dict) -> str:
    """'ok' | 'timeout' | 'error'"""
    sr = r["smt_result"]
    if sr == "Unsat":
        return "ok"
    if sr == "Unknown" or "timeout" in r["error_message"].lower():
        return "timeout"
    return "error"


def wu_status(r: dict) -> str:
    """'ok' | 'timeout' | 'error'"""
    res = r["result"]
    if res == "Verified":
        return "ok"
    if res == "Timeout":
        return "timeout"
    return "error"  # Parsing Error, Unsupported, …


# ── statistics ────────────────────────────────────────────────────────────────

def geometric_mean(values: list[float]) -> float:
    if not values:
        return float("nan")
    log_sum = sum(math.log(v) for v in values)
    return math.exp(log_sum / len(values))


def median(values: list[float]) -> float:
    if not values:
        return float("nan")
    s = sorted(values)
    n = len(s)
    mid = n // 2
    return s[mid] if n % 2 else (s[mid - 1] + s[mid]) / 2


# ── main ──────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) != 3:
        print("Usage: python compare_results.py <my_csv> <wu_csv>")
        sys.exit(1)

    my_data = load_mine(sys.argv[1])
    wu_data = load_wu(sys.argv[2])

    common_ids = sorted(my_data.keys() & wu_data.keys(),
                        key=lambda x: int(x.replace("Test", "")))

    only_mine = sorted(my_data.keys() - wu_data.keys(),
                       key=lambda x: int(x.replace("Test", "")))
    only_wu   = sorted(wu_data.keys() - my_data.keys(),
                       key=lambda x: int(x.replace("Test", "")))

    # ── categorise shared tests ───────────────────────────────────────────────
    # Key categories (for the shared set):
    #   both_ok          – both gave a definitive answer  → timing comparison
    #   wu_unsupported   – Wu errored with Unsupported/Parsing Error          → excluded
    #   both_timeout
    #   me_timeout_wu_ok
    #   wu_timeout_me_ok
    #   me_error_wu_ok   – my tool errored (BuilderError etc.)
    #   other

    both_ok          = []
    wu_unsupported   = []
    both_timeout     = []
    me_timeout_wu_ok = []
    wu_timeout_me_ok = []
    me_error         = []
    other            = []

    for tid in common_ids:
        ms = my_status(my_data[tid])
        ws = wu_status(wu_data[tid])
        wu_res = wu_data[tid]["result"]

        if wu_res in ("Unsupported", "Parsing Error"):
            wu_unsupported.append(tid)
        elif ms == "ok"      and ws == "ok":
            both_ok.append(tid)
        elif ms == "timeout" and ws == "timeout":
            both_timeout.append(tid)
        elif ms == "timeout" and ws == "ok":
            me_timeout_wu_ok.append(tid)
        elif ms == "ok"      and ws == "timeout":
            wu_timeout_me_ok.append(tid)
        elif ms == "error":
            me_error.append(tid)
        else:
            other.append(tid)

    # ── timing data for both_ok ───────────────────────────────────────────────
    my_smt   = [my_data[t]["smt_time"]   for t in both_ok]
    wu_smt   = [wu_data[t]["smt_time"]   for t in both_ok]
    my_total = [my_data[t]["total_time"] for t in both_ok]
    wu_total = [wu_data[t]["total_time"] for t in both_ok]

    # Speedup ratios: wu / mine  (>1 means mine is faster)
    smt_ratios   = [w / m for m, w in zip(my_smt,   wu_smt)   if m > 0]
    total_ratios = [w / m for m, w in zip(my_total, wu_total) if m > 0]

    # ── overhead: encoding = total - smt ─────────────────────────────────────
    my_enc  = [my_data[t]["total_time"] - my_data[t]["smt_time"] for t in both_ok]
    wu_enc  = [wu_data[t]["total_time"] - wu_data[t]["smt_time"] for t in both_ok]

    # ── print report ─────────────────────────────────────────────────────────
    SEP = "=" * 64

    print(SEP)
    print("  BENCHMARK COMPARISON: Herdi vs Wu")
    print(SEP)

    print(f"\n{'── Dataset overlap':}")
    print(f"  Tests in my CSV       : {len(my_data)}")
    print(f"  Tests in Wu's CSV     : {len(wu_data)}")
    print(f"  Tests in common       : {len(common_ids)}")
    if only_mine:
        print(f"  Only in my CSV        : {len(only_mine)}  ({', '.join(only_mine[:8])}{'…' if len(only_mine)>8 else ''})")
    if only_wu:
        print(f"  Only in Wu's CSV      : {len(only_wu)}  ({', '.join(only_wu[:8])}{'…' if len(only_wu)>8 else ''})")

    print(f"\n── Outcome breakdown (shared {len(common_ids)} tests)")
    print(f"  Both definitive (Unsat / Verified) : {len(both_ok)}")
    print(f"  Both timed out                     : {len(both_timeout)}")
    print(f"  I timed out, Wu solved             : {len(me_timeout_wu_ok)}")
    print(f"  Wu timed out, I solved             : {len(wu_timeout_me_ok)}")
    print(f"  My tool errored (BuilderError etc) : {len(me_error)}")
    print(f"  Wu unsupported / parse error       : {len(wu_unsupported)}  (excluded from all stats)")
    if other:
        print(f"  Other / unclassified               : {len(other)}")

    # effective comparison base = all shared minus wu_unsupported
    effective = len(common_ids) - len(wu_unsupported)
    print(f"\n  Effective comparison base          : {effective}  (shared minus Wu-unsupported)")
    if effective > 0:
        print(f"    → both solved   : {len(both_ok):3d}  ({100*len(both_ok)/effective:.1f}%)")
        print(f"    → both timeout  : {len(both_timeout):3d}  ({100*len(both_timeout)/effective:.1f}%)")
        print(f"    → I solved only : {len(wu_timeout_me_ok):3d}  ({100*len(wu_timeout_me_ok)/effective:.1f}%)")
        print(f"    → Wu solved only: {len(me_timeout_wu_ok):3d}  ({100*len(me_timeout_wu_ok)/effective:.1f}%)")
        print(f"    → my errors     : {len(me_error):3d}  ({100*len(me_error)/effective:.1f}%)")

    print(f"\n── Timing comparison  (on the {len(both_ok)} tests both tools solved)")
    if both_ok:
        print(f"\n  SMT solve time")
        print(f"    Herdi  – geomean {geometric_mean(my_smt):.4f}s   median {median(my_smt):.4f}s")
        print(f"    Wu     – geomean {geometric_mean(wu_smt):.4f}s   median {median(wu_smt):.4f}s")
        gm_smt = geometric_mean(smt_ratios)
        print(f"    Speedup (Wu/Herdi) geomean : {gm_smt:.3f}×  "
              f"({'Herdi faster' if gm_smt > 1 else 'Wu faster'})")

        print(f"\n  Total time (encode + solve)")
        print(f"    Herdi  – geomean {geometric_mean(my_total):.4f}s   median {median(my_total):.4f}s")
        print(f"    Wu     – geomean {geometric_mean(wu_total):.4f}s   median {median(wu_total):.4f}s")
        gm_total = geometric_mean(total_ratios)
        print(f"    Speedup (Wu/Herdi) geomean : {gm_total:.3f}×  "
              f"({'Herdi faster' if gm_total > 1 else 'Wu faster'})")

        print(f"\n  Encoding overhead (total − smt)")
        print(f"    Herdi  – geomean {geometric_mean(my_enc):.4f}s   median {median(my_enc):.4f}s")
        print(f"    Wu     – geomean {geometric_mean(wu_enc):.4f}s   median {median(wu_enc):.4f}s")
        enc_ratios = [m / w for m, w in zip(my_enc, wu_enc) if w > 0]
        if enc_ratios:
            print(f"    Herdi overhead vs Wu geomean : {geometric_mean(enc_ratios):.1f}×  "
                  f"(Herdi encodes ~{geometric_mean(enc_ratios):.1f}× slower)")
    else:
        print("  (no tests in common where both produced a result)")

    # ── per-test table for both_ok ────────────────────────────────────────────
    if both_ok:
        print(f"\n── Per-test detail ({len(both_ok)} jointly-solved tests)")
        print(f"  {'Test':<10} {'Herdi-SMT':>11} {'Wu-SMT':>11} {'SMT-ratio':>10}  "
              f"{'Herdi-tot':>11} {'Wu-tot':>11} {'tot-ratio':>10}")
        print(f"  {'-'*9} {'-'*11} {'-'*11} {'-'*10}  {'-'*11} {'-'*11} {'-'*10}")
        for t in both_ok:
            ms_  = my_data[t]["smt_time"]
            ws_  = wu_data[t]["smt_time"]
            mt_  = my_data[t]["total_time"]
            wt_  = wu_data[t]["total_time"]
            sr   = ws_ / ms_ if ms_ > 0 else float("nan")
            tr   = wt_ / mt_ if mt_ > 0 else float("nan")
            print(f"  {t:<10} {ms_:>11.4f}s {ws_:>11.4f}s {sr:>10.3f}×  "
                  f"{mt_:>11.4f}s {wt_:>11.4f}s {tr:>10.3f}×")

    # ── cases where Wu timed out but I solved ─────────────────────────────────
    if wu_timeout_me_ok:
        print(f"\n── Tests Wu timed out on that I solved ({len(wu_timeout_me_ok)})")
        print(f"  {'Test':<10} {'Herdi-SMT':>11} {'Herdi-tot':>11}")
        for t in wu_timeout_me_ok:
            print(f"  {t:<10} {my_data[t]['smt_time']:>11.4f}s {my_data[t]['total_time']:>11.4f}s")

    # ── cases where I timed out but Wu solved ─────────────────────────────────
    if me_timeout_wu_ok:
        print(f"\n── Tests I timed out on that Wu solved ({len(me_timeout_wu_ok)})")
        print(f"  {'Test':<10} {'Wu-SMT':>11} {'Wu-tot':>11}")
        for t in me_timeout_wu_ok:
            print(f"  {t:<10} {wu_data[t]['smt_time']:>11.4f}s {wu_data[t]['total_time']:>11.4f}s")

    # ── my errors ─────────────────────────────────────────────────────────────
    if me_error:
        print(f"\n── My tool errored ({len(me_error)})")
        for t in me_error:
            print(f"  {t:<10}  {my_data[t]['smt_result']}  {my_data[t]['error_message']}")

    print(f"\n{SEP}\n")


if __name__ == "__main__":
    main()
