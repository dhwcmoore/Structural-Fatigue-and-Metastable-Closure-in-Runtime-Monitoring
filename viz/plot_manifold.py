#!/usr/bin/env python3
"""Visualise the STAK-PSAL 2D action-space trajectories.

Usage:
  python3 viz/plot_manifold.py trajectory.csv

Reads the CSV produced by the OCaml demonstrator and renders:
  1. An ASCII manifold plot showing both trajectories converging.
  2. A compact status timeline.
  3. The JSON Rupture Certificate (if present).
"""

import sys
import csv
import json
import os
import math


# ── Data ingestion ────────────────────────────────────────────────────

def load_csv(path):
    rows = []
    with open(path) as f:
        for r in csv.DictReader(f):
            row = {
                't':        int(r['timestep']),
                'mx0':      float(r['mx_0']),
                'mx1':      float(r['mx_1']),
                'my0':      float(r['my_0']),
                'my1':      float(r['my_1']),
                'dist':     float(r['dist']),
                'status':   r['status'].strip(),
                'tension':  float(r['tension']),
                'gradient': float(r['gradient']),
            }
            # Predictive instability columns (present in v0.2+ CSVs)
            if 'tension_slope' in r:
                row['tension_slope'] = float(r['tension_slope'])
                row['rupt_press']    = float(r['rupt_press'])
                row['elast']         = float(r['elast'])
                row['instab']        = r['instab'].strip()
            rows.append(row)
    return rows


# ── ASCII manifold ────────────────────────────────────────────────────

GLYPH = {
    # (is_x_trajectory, status)
    (True,  'CLOSED'):       '·',
    (True,  'META_REVIEW'):  'o',
    (True,  'HARD_RUPTURE'): 'X',
    (False, 'CLOSED'):       ',',
    (False, 'META_REVIEW'):  'O',
    (False, 'HARD_RUPTURE'): '#',
}

def ascii_manifold(rows, width=72, height=28):
    all_x = [r['mx0'] for r in rows] + [r['my0'] for r in rows]
    all_y = [r['mx1'] for r in rows] + [r['my1'] for r in rows]
    xmin  = min(all_x) - 0.04;  xmax = max(all_x) + 0.04
    ymin  = min(all_y) - 0.04;  ymax = max(all_y) + 0.04
    xspan = max(xmax - xmin, 1e-6)
    yspan = max(ymax - ymin, 1e-6)

    def grid_pos(px, py):
        c = int((px - xmin) / xspan * (width  - 1))
        r = int((1 - (py - ymin) / yspan) * (height - 1))
        return max(0, min(width-1, c)), max(0, min(height-1, r))

    cells = [[' '] * width for _ in range(height)]

    for i, row in enumerate(rows):
        st = row['status']
        for is_x, px, py in [(True, row['mx0'], row['mx1']),
                              (False, row['my0'], row['my1'])]:
            c, r = grid_pos(px, py)
            g = GLYPH.get((is_x, st), '?')
            # Let later (worse) statuses overwrite
            cur = cells[r][c]
            priority = {'·': 0, ',': 0, 'o': 1, 'O': 1, 'X': 2, '#': 2, ' ': -1}
            if priority.get(g, 0) >= priority.get(cur, 0):
                cells[r][c] = g
        # Mark start
        if i == 0:
            c, r = grid_pos(row['mx0'], row['mx1'])
            cells[r][c] = 'S'
            c, r = grid_pos(row['my0'], row['my1'])
            cells[r][c] = 's'

    bar = '─' * (width + 2)
    print(bar)
    print("  2D Action Space  ·/,=CLOSED   o/O=META_REVIEW   X/#=HARD_RUPTURE")
    print("  S = Normal Pressure start     s = Critical Surge start")
    print(bar)
    for row in cells:
        print('│' + ''.join(row) + '│')
    print(bar)


# ── Status timeline ───────────────────────────────────────────────────

def print_timeline(rows):
    print("\nTimeline (status changes only):")
    print(f"  {'t':>4}  {'dist':>7}  {'status':<14}  {'tension':>9}  {'gradient':>10}")
    print(f"  {'─'*4}  {'─'*7}  {'─'*14}  {'─'*9}  {'─'*10}")
    prev = None
    for r in rows:
        s = r['status']
        if s != prev or s == 'HARD_RUPTURE':
            tag = '' if prev is None else '  ← status change'
            print(f"  {r['t']:>4}  {r['dist']:>7.4f}  {s:<14}  "
                  f"{r['tension']:>9.4f}  {r['gradient']:>10.4f}{tag}")
        prev = s

    print(f"\n  Total timesteps : {rows[-1]['t']}")
    dists = [r['dist'] for r in rows]
    print(f"  Initial dist    : {dists[0]:.4f}")
    print(f"  Final dist      : {dists[-1]:.4f}")
    print(f"  Min dist seen   : {min(dists):.4f}")
    nr = sum(1 for r in rows if r['status'] == 'META_REVIEW')
    print(f"  META_REVIEW steps: {nr}")


# ── Rupture certificate ───────────────────────────────────────────────

def print_certificate(cert_path='rupture_certificate.json'):
    if not os.path.exists(cert_path):
        return
    with open(cert_path) as f:
        cert = json.load(f)
    w = cert.get('width', 72)
    bar = '═' * 72
    print(f"\n{bar}")
    print("RUPTURE CERTIFICATE")
    print(bar)
    print(json.dumps(cert, indent=2))
    print(bar)


# ── Tension sparkline ─────────────────────────────────────────────────

SPARK = ' ▁▂▃▄▅▆▇█'

def tension_sparkline(rows, width=60):
    tensions = [r['tension'] for r in rows]
    peak = max(tensions) or 1.0
    buckets = []
    step = max(1, len(tensions) // width)
    for i in range(0, len(tensions), step):
        chunk = tensions[i:i+step]
        buckets.append(sum(chunk) / len(chunk))
    buckets = buckets[:width]
    line = ''.join(SPARK[min(8, int(v / peak * 8))] for v in buckets)
    print(f"\nTension (rolling):  [{line}]")
    print(f"  0{'─'*(width-2)}{peak:.3f}")


# ── Instability signals panel ─────────────────────────────────────────

INSTAB_GLYPH = {'STABLE': '.', 'STRAINED': 's', 'CRITICAL': 'C'}

def print_instability_panel(rows, width=60):
    if 'tension_slope' not in rows[0]:
        return  # old CSV format — skip

    print("\nPredictive Instability Signals:")

    # Rupture pressure sparkline
    rp_vals   = [r['rupt_press'] for r in rows]
    rp_peak   = max(rp_vals) or 1.0
    rp_step   = max(1, len(rp_vals) // width)
    rp_chunks = [rp_vals[i:i+rp_step] for i in range(0, len(rp_vals), rp_step)][:width]
    rp_line   = ''.join(SPARK[min(8, int(sum(c)/len(c) / rp_peak * 8))] for c in rp_chunks)
    print(f"  Rupture pressure: [{rp_line}]")
    print(f"  0{'─'*(width-2)}{rp_peak:.2f}")

    # Elasticity sparkline (clipped to [-1, 1] for display)
    el_vals  = [max(-1.0, min(1.0, r['elast'])) for r in rows]
    el_min   = min(el_vals);  el_max = max(el_vals)
    el_span  = max(el_max - el_min, 1e-6)
    el_step  = max(1, len(el_vals) // width)
    el_chunks= [el_vals[i:i+el_step] for i in range(0, len(el_vals), el_step)][:width]
    el_line  = ''.join(SPARK[min(8, int((sum(c)/len(c) - el_min) / el_span * 8))]
                       for c in el_chunks)
    print(f"  Recovery elast:  [{el_line}]")
    print(f"  {el_min:.3f}{'─'*(width-8)}{el_max:.3f}")

    # Instability level timeline
    instab_line = ''.join(INSTAB_GLYPH.get(r['instab'], '?') for r in rows)
    if len(instab_line) > width:
        step = max(1, len(instab_line) // width)
        # Worst-case per bucket
        levels = {'STABLE': 0, 'STRAINED': 1, 'CRITICAL': 2}
        glyphs = ['.', 's', 'C']
        instab_line = ''.join(
            glyphs[max(levels.get(r['instab'], 0)
                       for r in rows[i:i+step])]
            for i in range(0, len(rows), step)
        )[:width]
    print(f"  Instab level:    [{instab_line:<{width}}]")
    print(f"  . = STABLE   s = STRAINED   C = CRITICAL")

    # Summary counts
    counts = {}
    for r in rows:
        k = r['instab']
        counts[k] = counts.get(k, 0) + 1
    parts = [f"{k}: {v}" for k, v in sorted(counts.items())]
    print(f"  Counts — {', '.join(parts)}")


# ── Entry point ───────────────────────────────────────────────────────

def main():
    path = sys.argv[1] if len(sys.argv) > 1 else 'trajectory.csv'
    rows = load_csv(path)
    ascii_manifold(rows)
    tension_sparkline(rows)
    print_instability_panel(rows)
    print_timeline(rows)
    print_certificate()


if __name__ == '__main__':
    main()
