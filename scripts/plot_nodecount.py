#!/usr/bin/env python3
"""
Scatter plots of node counts vs SMT/total time, colored by outcome.

Produces two PDFs:
  nodecount_smt_time.pdf   -- total nodes vs SMT time
  nodecount_total_time.pdf -- total nodes vs total time

Usage:
    python plot_nodecount.py <nodecount_csv> [output_dir]
"""

import sys
import csv
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def parse_seconds(s: str) -> float:
    return float(s.rstrip("s"))


def load(path: str) -> list[dict]:
    rows = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append({
                "smt_result":  row["smt-result"].strip(),
                "smt_time":    parse_seconds(row["smt-time"]),
                "total_time":  parse_seconds(row["total-time"]),
                "data_nodes":  int(row["data-nodes"]),
                "ctrl_nodes":  int(row["ctrl-nodes"]),
                "mem_nodes":   int(row["mem-nodes"]),
                "total_nodes": int(row["data-nodes"]) + int(row["ctrl-nodes"]) + int(row["mem-nodes"]),
            })
    return rows


OUTCOME_STYLE = {
    "Unsat":   {"color": "steelblue",  "marker": "o", "label": "Verified (Unsat)", "alpha": 0.6, "zorder": 2},
    "Unknown": {"color": "tomato",     "marker": "x", "label": "Timeout",          "alpha": 0.8, "zorder": 3},
    "Error":   {"color": "goldenrod",  "marker": "^", "label": "Error",            "alpha": 0.9, "zorder": 4},
}


def classify(row: dict) -> str:
    r = row["smt_result"]
    if r == "Unsat":
        return "Unsat"
    if r == "Unknown":
        return "Unknown"
    return "Error"


def scatter_plot(rows, x_key, y_key, xlabel, ylabel, out_path, log_y=True):
    groups: dict[str, list] = {k: ([], []) for k in OUTCOME_STYLE}

    for row in rows:
        cat = classify(row)
        groups[cat][0].append(row[x_key])
        groups[cat][1].append(row[y_key])

    fig, ax = plt.subplots(figsize=(6, 4))

    for cat, style in OUTCOME_STYLE.items():
        xs, ys = groups[cat]
        if not xs:
            continue
        ax.scatter(xs, ys,
                   color=style["color"],
                   marker=style["marker"],
                   label=style["label"],
                   alpha=style["alpha"],
                   s=18,
                   zorder=style["zorder"])

    if log_y:
        ax.set_yscale("log")

    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.legend(fontsize=8)
    ax.grid(True, which="both", linestyle="--", alpha=0.3)
    fig.tight_layout()
    fig.savefig(out_path, format="pdf")
    print(f"Written: {out_path}")


def main():
    if len(sys.argv) < 2:
        print("Usage: python plot_nodecount.py <nodecount_csv> [output_dir]")
        sys.exit(1)

    csv_path = sys.argv[1]
    out_dir  = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(".")
    out_dir.mkdir(parents=True, exist_ok=True)

    rows = load(csv_path)

    # total nodes vs SMT time
    scatter_plot(rows,
                 x_key="total_nodes", y_key="smt_time",
                 xlabel="Total nodes (data + ctrl + mem)",
                 ylabel="SMT time (s)",
                 out_path=out_dir / "nodecount_smt_time.pdf")

    # total nodes vs total time
    scatter_plot(rows,
                 x_key="total_nodes", y_key="total_time",
                 xlabel="Total nodes (data + ctrl + mem)",
                 ylabel="Total time (s)",
                 out_path=out_dir / "nodecount_total_time.pdf")

    # also individual node types vs SMT time, in a 1x3 subplot figure
    fig, axes = plt.subplots(1, 3, figsize=(12, 4), sharey=True)
    node_types = [
        ("data_nodes", "Data nodes"),
        ("ctrl_nodes", "Control nodes"),
        ("mem_nodes",  "Memory nodes"),
    ]

    for ax, (key, label) in zip(axes, node_types):
        for cat, style in OUTCOME_STYLE.items():
            xs = [r[key]      for r in rows if classify(r) == cat]
            ys = [r["smt_time"] for r in rows if classify(r) == cat]
            if not xs:
                continue
            ax.scatter(xs, ys,
                       color=style["color"],
                       marker=style["marker"],
                       label=style["label"],
                       alpha=style["alpha"],
                       s=18,
                       zorder=style["zorder"])
        ax.set_yscale("log")
        ax.set_xlabel(label)
        ax.grid(True, which="both", linestyle="--", alpha=0.3)
        ax.legend(fontsize=7)

    axes[0].set_ylabel("SMT time (s)")
    fig.tight_layout()
    out_path = out_dir / "nodetype_smt_time.pdf"
    fig.savefig(out_path, format="pdf")
    print(f"Written: {out_path}")

    print()
    correlation_report(rows)


def correlation_report(rows: list[dict]):
    from scipy.stats import spearmanr

    node_types = [
        ("data_nodes",  "Data nodes"),
        ("ctrl_nodes",  "Control nodes"),
        ("mem_nodes",   "Memory nodes"),
        ("total_nodes", "Total nodes"),
    ]

    solved = [r for r in rows if r["smt_result"] == "Unsat"]

    print("=" * 60)
    print("Spearman correlation with SMT time")
    print("=" * 60)

    print(f"\n{'Node type':<20} {'rho (all)':>10} {'p (all)':>10} {'rho (solved)':>13} {'p (solved)':>11}")
    print("-" * 68)
    for key, label in node_types:
        # all rows (timeouts clamped at ~60s)
        xs_all = [r[key]       for r in rows]
        ys_all = [r["smt_time"] for r in rows]
        rho_all, p_all = spearmanr(xs_all, ys_all)

        # solved only (no clamping distortion)
        xs_sol = [r[key]       for r in solved]
        ys_sol = [r["smt_time"] for r in solved]
        rho_sol, p_sol = spearmanr(xs_sol, ys_sol)

        print(f"{label:<20} {rho_all:>10.3f} {p_all:>10.3e} {rho_sol:>13.3f} {p_sol:>11.3e}")

    print(f"\n  n (all)    = {len(rows)}")
    print(f"  n (solved) = {len(solved)}")
    print("=" * 60)


if __name__ == "__main__":
    main()
