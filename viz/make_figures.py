#!/usr/bin/env python3
"""
Generate the three publication-grade figures for stak_psal_rv.tex.

  fig1_architecture.pdf  — pipeline block diagram
  fig2_regime.pdf        — regime phase diagram (boost × delta)
  fig3_trajectory.pdf    — metastable trajectory (4-panel)

Figures are written to docs/paper/figs/.
"""

import os, sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.gridspec as gridspec
import matplotlib.patheffects as pe
from matplotlib.collections import LineCollection
import pandas as pd

BASE = os.path.join(os.path.dirname(__file__), '..')
FIGS = os.path.join(BASE, 'docs', 'paper', 'figs')
os.makedirs(FIGS, exist_ok=True)

# ── Typography and colour palette ──────────────────────────────────────────
plt.rcParams.update({
    'font.family'       : 'DejaVu Serif',
    'font.size'         : 9,
    'axes.titlesize'    : 9,
    'axes.labelsize'    : 9,
    'xtick.labelsize'   : 8,
    'ytick.labelsize'   : 8,
    'figure.dpi'        : 200,
    'text.usetex'       : False,
    'lines.linewidth'   : 1.3,
    'axes.spines.top'   : False,
    'axes.spines.right' : False,
    'legend.fontsize'   : 7.5,
    'legend.framealpha' : 0.85,
})

C_CLOSED  = '#2166ac'   # blue
C_META    = '#d6604d'   # red-orange
C_RUPT    = '#b2182b'   # dark red
C_STABLE  = '#4dac26'   # green (R3)
C_FRAGILE = '#d6604d'   # orange (R2)
C_IMMED   = '#b2182b'   # dark red (R1)
C_NEVER   = '#888888'   # grey (R0)
C_CRIT    = '#e6a817'   # amber (CRITICAL)
BG_CLOSED = '#ddeeff'
BG_META   = '#ffe8d8'
BG_RUPT   = '#ffe0e0'


# ═══════════════════════════════════════════════════════════════════════════
# Python simulation of the OCaml model (exact parameter match)
# ═══════════════════════════════════════════════════════════════════════════

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-np.clip(x, -500, 500)))

def project(W, z):
    return sigmoid(W @ z)

def make_initial_W(rng):
    W = np.zeros((2, 64))
    # Row 0: pressure boundary
    W[0, 0]  = 5.0;  W[0, 1] = 0.40;  W[0, 2] = 0.30
    W[0, 3:16]  = rng.normal(0, 0.05, 13)
    W[0, 16] = 0.80; W[0, 17] = 0.20
    W[0, 18:32] = rng.normal(0, 0.04, 14)
    W[0, 32:48] = rng.normal(0, 0.03, 16)
    W[0, 48] = 0.60; W[0, 49] = 0.15
    W[0, 50:64] = rng.normal(0, 0.04, 14)
    # Row 1: flow/temperature projection
    W[1, 0:6]   = rng.normal(0, 0.02, 6)
    W[1, 16]    = 0.80
    W[1, 17:32] = 0.30 + rng.normal(0, 0.06, 15)
    W[1, 32]    = 2.0;  W[1, 33] = 0.50;  W[1, 34] = 0.30
    W[1, 35:48] = 0.20 + rng.normal(0, 0.08, 13)
    return W

def make_states():
    """Fixed (noise-free anchor) states for clean figure."""
    x = np.full(64, 0.50)
    x[0]  = 0.15; x[16] = 0.20; x[32] = 0.55; x[48] = 0.10
    y = np.full(64, 0.55)
    y[0]  = 0.88; y[16] = 0.72; y[32] = 0.22; y[48] = 0.76
    return x, y

def degrade_multimodal(W, rng, rates=(0.020, 0.012, 0.007, 0.015)):
    W2 = W.copy()
    for j in range(64):
        r = rates[j // 16]
        W2[:, j] = W[:, j] * (1.0 - r) + rng.normal(0, r * 0.20, 2)
    return W2

def enrich(W, x, y, boost):
    gaps  = np.abs(x - y)
    j_str = int(np.argmax(gaps))
    W2 = W.copy()
    W2[0, j_str] += boost
    return W2

EPS_C  = 0.08
EPS_M  = 0.22
DECAY  = 0.92

def simulate(seed, boost, n_steps=200, rates=(0.020, 0.012, 0.007, 0.015)):
    """Run one simulation; return per-step trace dict and event times."""
    rng = np.random.default_rng(seed)
    W = make_initial_W(rng)
    x, y = make_states()

    # State variables
    status      = 'CLOSED'
    T           = 0.0
    T_slope     = 0.0
    sep_ema     = None
    fatigue     = 0.0
    T_entry     = 0.0
    steps_meta  = 0
    T_prev      = 0.0

    # Events
    rcls_t = -1; rupt_t = -1; crit_t = -1

    # Trace
    trace = dict(d=[], T=[], T_slope=[], R=[], E_r=[], F=[], status=[])

    prev_status = 'CLOSED'

    for t in range(n_steps):
        mx = project(W, x)
        my = project(W, y)
        d  = float(np.linalg.norm(mx - my))

        # Tension update
        strain  = max(0.0, EPS_M - d)
        T_new   = T * DECAY + strain * 0.6
        T_slope = 0.25 * (T_new - T) + 0.75 * T_slope
        if sep_ema is None:
            sep_ema = d
        sep_ema = 0.10 * d + 0.90 * sep_ema
        R = 1.0 / (d + 0.001)
        T = T_new

        # State transition
        if d <= EPS_C:
            next_status = 'HARD_RUPTURE'
        elif d <= EPS_M:
            next_status = 'META_REVIEW'
        else:
            next_status = 'CLOSED'

        # Enrichment on CLOSED → META_REVIEW
        if next_status == 'META_REVIEW' and prev_status == 'CLOSED':
            W = enrich(W, x, y, boost)
            T_entry    = T
            steps_meta = 1
        elif next_status == 'META_REVIEW':
            steps_meta += 1
        elif next_status == 'CLOSED' and prev_status == 'META_REVIEW':
            if rcls_t == -1:
                rcls_t = t
            steps_meta = 0
        elif next_status == 'CLOSED':
            steps_meta = 0

        # Recovery elasticity (only valid after 2+ steps in meta)
        E_r = float('nan')
        if next_status == 'META_REVIEW' and steps_meta > 2:
            E_r = (T_entry - T) / steps_meta

        # Fatigue (during META_REVIEW)
        if next_status == 'META_REVIEW':
            dT     = abs(T - T_prev)
            el_neg = max(0.0, -E_r) if not np.isnan(E_r) else 0.0
            rp_ex  = max(0.0, R - 5.0)
            fatigue += dT + el_neg * 0.1 + rp_ex * 0.01

        # CRITICAL check
        if next_status == 'META_REVIEW' and steps_meta > 2 and crit_t == -1:
            if (T_slope > 0.002
                    and R > 10.0
                    and not np.isnan(E_r)
                    and E_r < 0.005):
                crit_t = t

        # Record
        trace['d'].append(d)
        trace['T'].append(T)
        trace['T_slope'].append(T_slope)
        trace['R'].append(R)
        trace['E_r'].append(E_r)
        trace['F'].append(fatigue)
        trace['status'].append(next_status)

        T_prev      = T
        prev_status = next_status

        if next_status == 'HARD_RUPTURE':
            rupt_t = t
            break

        W = degrade_multimodal(W, rng, rates)

    return trace, rcls_t, rupt_t, crit_t


def find_metastable_seed(boost=0.07, n_seeds=60):
    """Find a seed giving 2 META_REVIEW episodes with a clear visible pattern.
    Criteria: ep1 ≥ 10 steps, gap ≥ 15 steps, ep2 in [20, 80] steps."""
    best = None
    for seed in range(1, n_seeds + 1):
        tr, rcls, rupt, crit = simulate(seed, boost)
        if rcls == -1 or rupt == -1 or crit == -1:
            continue
        stat = tr['status']
        # Collect episode boundaries
        eps = []
        in_meta = False
        for i, s in enumerate(stat):
            if s == 'META_REVIEW' and not in_meta:
                ep_start = i; in_meta = True
            elif s != 'META_REVIEW' and in_meta:
                eps.append((ep_start, i - 1)); in_meta = False
        if in_meta:
            eps.append((ep_start, len(stat) - 1))
        if len(eps) < 2:
            continue
        ep1_len = eps[0][1] - eps[0][0]
        gap_len = eps[1][0] - eps[0][1]
        ep2_len = eps[1][1] - eps[1][0]
        lead    = rupt - crit
        if (ep1_len >= 10 and gap_len >= 15
                and 20 <= ep2_len <= 85 and lead >= 8):
            if best is None or ep1_len + gap_len > best[0]:
                best = (ep1_len + gap_len, seed, boost, tr, rcls, rupt, crit)
    if best:
        return best[1], best[2], best[3], best[4], best[5], best[6]
    return None


# ═══════════════════════════════════════════════════════════════════════════
# Figure 1 — Architecture block diagram
# ═══════════════════════════════════════════════════════════════════════════

def fig1_architecture():
    fig, ax = plt.subplots(figsize=(6.0, 2.8))
    ax.set_xlim(0, 10); ax.set_ylim(0, 4.2)
    ax.axis('off')

    def box(cx, cy, w, h, text, fc, ec='#333333', fs=8.0, bold=False):
        p = mpatches.FancyBboxPatch(
            (cx - w/2, cy - h/2), w, h,
            boxstyle='round,pad=0.05',
            facecolor=fc, edgecolor=ec, linewidth=0.8, zorder=2)
        ax.add_patch(p)
        ax.text(cx, cy, text, ha='center', va='center', fontsize=fs,
                fontweight='bold' if bold else 'normal', zorder=3,
                linespacing=1.4)

    def arrow(x1, y1, x2, y2, color='#333333', style='->', lw=0.8, ls='-'):
        ax.annotate('', xy=(x2, y2), xytext=(x1, y1),
            arrowprops=dict(arrowstyle=style, color=color, lw=lw,
                            linestyle=ls), zorder=1)

    # ── Sensor input ──
    box(0.9, 2.1, 1.4, 0.7, '64D\nSensor input', '#ddeeff')
    arrow(1.6, 2.1, 2.2, 2.1)

    # ── Reduction operator ──
    box(3.0, 2.1, 1.5, 0.7,
        '$M_t = \\sigma(W_t z)$\nReduction', '#ddeeff')
    arrow(3.75, 2.1, 4.35, 2.1)

    # ── Degradation (top arrow into W) ──
    arrow(3.0, 3.3, 3.0, 2.55, color=C_META, lw=0.9, ls='dashed')
    ax.text(3.0, 3.5, 'Degradation $\\delta$', ha='center', fontsize=7.5,
            color=C_META, style='italic')

    # ── Enrichment (bottom arrow into W) ──
    arrow(3.0, 0.9, 3.0, 1.75, color=C_STABLE, lw=0.9, ls='dashed')
    ax.text(3.0, 0.65, 'Enrichment $b$', ha='center', fontsize=7.5,
            color=C_STABLE, style='italic')

    # ── Distance ──
    box(5.1, 2.1, 1.3, 0.65,
        '$d_t = \\|M_t(x){-}M_t(y)\\|$\nSeparation', '#ddeeff', fs=7.5)
    arrow(5.75, 2.1, 6.35, 2.1)

    # ── Three-state monitor ──
    mon_fc = '#f5f5f5'
    p = mpatches.FancyBboxPatch((6.35, 0.85), 1.85, 2.5,
        boxstyle='round,pad=0.05', facecolor=mon_fc, edgecolor='#333',
        linewidth=1.0, zorder=2)
    ax.add_patch(p)
    ax.text(7.28, 3.15, 'Three-state\nMonitor', ha='center', fontsize=8.5,
            fontweight='bold', zorder=3)

    for txt, yy, col in [
            ('CLOSED',       2.6, C_CLOSED),
            ('META\\_REVIEW', 2.1, C_META),
            ('HARD\\_RUPTURE', 1.5, C_RUPT),
    ]:
        ax.text(7.28, yy, txt, ha='center', fontsize=8, color=col,
                fontweight='bold', zorder=3)

    # ── Enrichment back-arrow from monitor to W ──
    arrow(6.35, 2.1, 4.35, 1.0, color=C_STABLE, lw=0.9)
    ax.annotate('', xy=(3.25, 1.6), xytext=(4.35, 1.0),
        arrowprops=dict(arrowstyle='->', color=C_STABLE, lw=0.9), zorder=1)

    # ── Certificate output ──
    arrow(8.2, 1.5, 8.85, 1.2)
    box(9.3, 1.0, 1.3, 0.55, 'Rupture\nCertificate', '#ffe0e0', fs=7.5)

    # ── Predictive estimators (below main flow) ──
    box(5.1, 0.85, 1.3, 0.85,
        'Estimators\n$\\dot{T}_t, R_t, E_r, \\bar{d}_t$', '#edf7e8', fs=7.5)
    arrow(5.1, 1.28, 5.1, 1.78)

    # ── CRITICAL label inside monitor ──
    box(7.28, 0.55, 1.55, 0.45, 'CRITICAL signal', '#fff3cd',
        ec=C_CRIT, fs=7.0)
    arrow(5.75, 0.75, 6.5, 0.6)
    arrow(7.28, 0.78, 7.28, 0.88)

    # ── Title ──
    ax.text(0.0, 4.05, '(a) STAK-PSAL runtime monitoring pipeline',
            fontsize=9, fontweight='bold')

    plt.tight_layout(pad=0.3)
    out = os.path.join(FIGS, 'fig1_architecture.pdf')
    plt.savefig(out, bbox_inches='tight')
    plt.close()
    print(f'  Figure 1 → {out}')


# ═══════════════════════════════════════════════════════════════════════════
# Figure 2 — Regime phase diagram
# ═══════════════════════════════════════════════════════════════════════════

def fig2_regime():
    csv = os.path.join(BASE, 'sweep_stochastic.csv')
    if not os.path.exists(csv):
        print('  [skip] sweep_stochastic.csv not found')
        return
    df = pd.read_csv(csv)

    deltas = sorted(df['degrade'].unique())   # 5 values
    boosts = sorted(df['boost'].unique())     # 13 values

    fig, axes = plt.subplots(1, 2, figsize=(6.5, 2.8),
                             gridspec_kw={'wspace': 0.38})

    # ── Panel a: bifurcation curves ──
    ax = axes[0]
    palette = ['#053061', '#2166ac', '#4393c3', '#d6604d', '#b2182b']
    for delta, col in zip(deltas, palette):
        sub  = df[df['degrade'] == delta].groupby('boost')
        frac = sub.apply(lambda g: (g['regime'] == 3).sum() / len(g))
        ax.plot(frac.index, frac.values, color=col, marker='.', ms=4,
                label=f'$\\delta={delta:.3f}$')

    ax.axhline(0.5, color='#999', lw=0.7, ls='--', zorder=0)
    ax.text(0.58, 0.53, '50%', fontsize=7, color='#999', ha='right')

    # b_crit ticks
    bcrit = {0.010: 0.05, 0.015: 0.10, 0.020: 0.15, 0.025: 0.20, 0.030: 0.25}
    for delta, bc in bcrit.items():
        col = palette[list(bcrit.keys()).index(delta)]
        ax.axvline(bc, color=col, lw=0.5, ls=':', alpha=0.5, zorder=0)

    ax.set_xlabel('Boost $b$')
    ax.set_ylabel('Fraction R3 (stable)')
    ax.set_title('(a) Bifurcation by degradation rate', fontsize=9)
    ax.legend(loc='center right', ncol=1)
    ax.set_xlim(-0.01, 0.63)
    ax.set_ylim(-0.04, 1.05)

    # ── Panel b: phase heatmap ──
    ax = axes[1]
    H = np.zeros((len(deltas), len(boosts)))
    for i, delta in enumerate(deltas):
        for j, boost in enumerate(boosts):
            cell = df[(df['degrade'] == delta) & (df['boost'] == boost)]
            if len(cell):
                H[i, j] = (cell['regime'] == 3).sum() / len(cell)

    extent = [min(boosts) - 0.025, max(boosts) + 0.025,
              min(deltas) - 0.0025, max(deltas) + 0.0025]
    im = ax.imshow(H, aspect='auto', origin='lower', extent=extent,
                   cmap='RdYlBu', vmin=0, vmax=1, interpolation='nearest')
    plt.colorbar(im, ax=ax, label='Fraction R3', shrink=0.85)

    # b_crit boundary
    bcs = [bcrit[d] for d in deltas]
    ax.plot(bcs, deltas, color='white', lw=2.5, zorder=3)
    ax.plot(bcs, deltas, color='black', lw=1.0, ls='--', zorder=4,
            label='$b_{\\mathrm{crit}}$')
    ax.legend(loc='upper left', fontsize=7.5)

    ax.set_xlabel('Boost $b$')
    ax.set_ylabel('Degradation $\\delta$')
    ax.set_title('(b) Regime phase diagram', fontsize=9)

    plt.tight_layout(pad=0.4)
    out = os.path.join(FIGS, 'fig2_regime.pdf')
    plt.savefig(out, bbox_inches='tight')
    plt.close()
    print(f'  Figure 2 → {out}')


# ═══════════════════════════════════════════════════════════════════════════
# Figure 3 — Metastable trajectory (4-panel)
# ═══════════════════════════════════════════════════════════════════════════

def status_color(s):
    if s == 'CLOSED':       return BG_CLOSED
    if s == 'META_REVIEW':  return BG_META
    return BG_RUPT

def add_background(ax, statuses, alpha=0.25):
    """Shade background by status."""
    prev_s  = statuses[0]
    seg_start = 0
    for i, s in enumerate(statuses):
        if s != prev_s or i == len(statuses) - 1:
            end = i if s != prev_s else i + 1
            ax.axvspan(seg_start, end - 1,
                       facecolor=status_color(prev_s), alpha=alpha,
                       linewidth=0, zorder=0)
            seg_start = i
            prev_s = s

def fig3_trajectory():
    print('  Searching for a metastable seed...', end=' ', flush=True)
    result = find_metastable_seed(boost=0.07, n_seeds=80)
    if result is None:
        print('not found — trying boost=0.06')
        result = find_metastable_seed(boost=0.06, n_seeds=100)
    if result is None:
        print('  [skip] No suitable seed found for Figure 3')
        return

    seed, boost, tr, rcls_t, rupt_t, crit_t = result
    print(f'seed={seed} boost={boost:.2f} rcls={rcls_t} rupt={rupt_t} crit={crit_t}')

    T    = len(tr['d'])
    ts   = np.arange(T)
    stat = tr['status']

    # Find META_REVIEW episode boundaries
    meta_bands = []
    in_meta    = False
    for i, s in enumerate(stat):
        if s == 'META_REVIEW' and not in_meta:
            meta_start = i; in_meta = True
        elif s != 'META_REVIEW' and in_meta:
            meta_bands.append((meta_start, i - 1)); in_meta = False
    if in_meta:
        meta_bands.append((meta_start, T - 1))

    fig = plt.figure(figsize=(5.5, 5.2))
    gs  = gridspec.GridSpec(4, 1, hspace=0.08,
                            height_ratios=[2.0, 1.2, 1.2, 0.45])
    axes = [fig.add_subplot(gs[i]) for i in range(4)]

    # ── Panel 1: Separation d_t ──
    ax = axes[0]
    add_background(ax, stat)
    ax.plot(ts, tr['d'], color='#333333', lw=1.4, zorder=3)
    ax.axhline(EPS_M, color=C_META,   lw=0.9, ls='--', label=f'$\\varepsilon_m={EPS_M}$')
    ax.axhline(EPS_C, color=C_RUPT,   lw=0.9, ls='--', label=f'$\\varepsilon_c={EPS_C}$')
    if crit_t != -1:
        ax.axvline(crit_t, color=C_CRIT, lw=1.0, ls=':', alpha=0.9, zorder=4)
    if rupt_t != -1:
        ax.axvline(rupt_t, color=C_RUPT, lw=1.2, alpha=0.7, zorder=4)
    ax.set_ylabel('Separation $d_t$')
    ax.set_ylim(bottom=0)
    ax.legend(loc='upper right', ncol=2, fontsize=7)
    ax.set_xticklabels([])

    # Annotate episodes
    for k, (t0, t1) in enumerate(meta_bands):
        mid = (t0 + t1) / 2
        ax.text(mid, EPS_M + 0.01, f'M{k+1}',
                ha='center', fontsize=7, color=C_META, style='italic')

    if rcls_t != -1:
        ax.annotate('reclosure', xy=(rcls_t, tr['d'][rcls_t]),
                    xytext=(rcls_t + 2, tr['d'][rcls_t] + 0.04),
                    arrowprops=dict(arrowstyle='->', lw=0.7, color='#555'),
                    fontsize=7, color='#555')

    # ── Panel 2: Recovery elasticity ──
    ax = axes[1]
    add_background(ax, stat)
    er_vals = np.array([v if not np.isnan(v) else np.nan for v in tr['E_r']])
    ax.plot(ts, er_vals, color='#333333', lw=1.4, zorder=3, label='$E_r^{(t)}$')
    ax.axhline(0.0,   color='#666', lw=0.7, ls='-')
    ax.axhline(0.005, color=C_CRIT, lw=0.7, ls='--', alpha=0.7)
    if crit_t != -1:
        ax.axvline(crit_t, color=C_CRIT, lw=1.0, ls=':', alpha=0.9, zorder=4)
    if rupt_t != -1:
        ax.axvline(rupt_t, color=C_RUPT, lw=1.2, alpha=0.7, zorder=4)
    ax.set_ylabel('Elasticity $E_r$')
    ax.set_xticklabels([])
    # mark sign change
    neg_start = next((i for i, v in enumerate(er_vals)
                      if not np.isnan(v) and v < 0), None)
    if neg_start:
        ax.annotate('$E_r < 0$', xy=(neg_start, er_vals[neg_start]),
                    xytext=(neg_start + 2, -0.002),
                    arrowprops=dict(arrowstyle='->', lw=0.7, color=C_META),
                    fontsize=7, color=C_META)

    # ── Panel 3: Cumulative fatigue ──
    ax = axes[2]
    add_background(ax, stat)
    ax.plot(ts, tr['F'], color='#333333', lw=1.4, zorder=3)
    if crit_t != -1:
        ax.axvline(crit_t, color=C_CRIT, lw=1.0, ls=':', alpha=0.9, zorder=4,
                   label='CRITICAL')
    if rupt_t != -1:
        ax.axvline(rupt_t, color=C_RUPT, lw=1.2, alpha=0.7, zorder=4,
                   label='rupture')
    ax.set_ylabel('Fatigue $\\mathcal{F}_n$')
    ax.set_xticklabels([])
    ax.legend(loc='upper left', fontsize=7)
    # label final fatigue
    ax.text(T - 1, tr['F'][-1] + 0.05,
            f'  $\\mathcal{{F}}_n={tr["F"][-1]:.2f}$',
            fontsize=7, color='#333', va='bottom')

    # ── Panel 4: Status strip ──
    ax = axes[3]
    col_map = {'CLOSED': C_CLOSED, 'META_REVIEW': C_META, 'HARD_RUPTURE': C_RUPT}
    colors = [col_map[s] for s in stat]
    for i, c in enumerate(colors):
        ax.axvspan(i, i + 1, facecolor=c, alpha=0.85, linewidth=0)
    ax.set_ylim(0, 1); ax.set_yticks([])
    ax.set_xlabel('Step $t$')
    ax.set_xlim(0, T)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_visible(False)

    # Legend patches
    patches = [
        mpatches.Patch(color=C_CLOSED, label='Closed'),
        mpatches.Patch(color=C_META,   label='Meta-review'),
        mpatches.Patch(color=C_RUPT,   label='Hard-rupture'),
    ]
    ax.legend(handles=patches, loc='center', ncol=3,
              fontsize=7, framealpha=0.9, borderpad=0.3)

    # Shared x-axis range
    for a in axes:
        a.set_xlim(0, T)

    # Title
    axes[0].set_title(
        f'(c) Metastable trajectory  '
        f'($\\delta_{{\\mathrm{{pressure}}}}=0.020$, $b={boost:.2f}$, seed {seed})',
        fontsize=9)

    plt.savefig(os.path.join(FIGS, 'fig3_trajectory.pdf'), bbox_inches='tight')
    plt.close()
    print(f'  Figure 3 → {os.path.join(FIGS, "fig3_trajectory.pdf")}')


# ═══════════════════════════════════════════════════════════════════════════
# Figure 4 — Blind-spot: statistical normality vs. distinguishability collapse
# ═══════════════════════════════════════════════════════════════════════════

def fig_blindspot():
    """
    Two-panel figure illustrating the core blind-spot.

    Panel (a): M_t(x)[0] and M_t(y)[0] individually — both remain within
               the safe output range; no output monitor alarm fires.
    Panel (b): d_t = ||M_t(x) - M_t(y)|| — collapses through the three
               STAK-PSAL states to hard rupture.

    Uses pure deterministic decay (no noise, no enrichment) for a clean figure.
    """
    rng  = np.random.default_rng(0)
    W    = make_initial_W(rng)
    x, y = make_states()

    mx0_tr, my0_tr, d_tr, stat_tr = [], [], [], []

    for _ in range(200):
        px = project(W, x)
        py = project(W, y)
        d  = float(np.linalg.norm(px - py))
        mx0_tr.append(float(px[0]))
        my0_tr.append(float(py[0]))
        d_tr.append(d)
        if d <= EPS_C:
            stat_tr.append('HARD_RUPTURE')
            break
        stat_tr.append('CLOSED' if d > EPS_M else 'META_REVIEW')
        W = W * (1.0 - 0.020)   # deterministic uniform decay

    T  = len(d_tr)
    ts = np.arange(T)

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6.5, 2.2),
                                    gridspec_kw={'wspace': 0.40})

    # ── Panel (a): individual outputs — appear normal throughout ──────────
    ax1.axhspan(0.1, 0.9, facecolor='#d4edda', alpha=0.38, zorder=0)
    ax1.text(2, 0.915, 'safe output range', fontsize=6.5,
             color='#2d6a2d', style='italic')

    ax1.plot(ts, mx0_tr, color=C_CLOSED, lw=1.4, label='$M_t(x)[0]$  (normal state)')
    ax1.plot(ts, my0_tr, color=C_RUPT,   lw=1.4, label='$M_t(y)[0]$  (critical state)')

    # Annotate convergence at the final step
    t_end = T - 1
    gap = abs(mx0_tr[t_end] - my0_tr[t_end])
    mid_y = (mx0_tr[t_end] + my0_tr[t_end]) / 2
    ax1.annotate(
        f'$t={t_end}$: gap = {gap:.2f}\n(both within range)',
        xy=(t_end, mid_y),
        xytext=(max(5, t_end - 38), mid_y + 0.16),
        arrowprops=dict(arrowstyle='->', lw=0.7, color='#444'),
        fontsize=6.5, color='#444')

    ax1.set_xlabel('Step $t$')
    ax1.set_ylabel('Output value')
    ax1.set_ylim(0.0, 1.06)
    ax1.set_xlim(0, T)
    ax1.set_title('(a) Output monitor: no alarm fires', fontsize=9)
    ax1.legend(loc='lower left', fontsize=7, handlelength=1.2)

    # ── Panel (b): d_t — collapses through state bands ───────────────────
    add_background(ax2, stat_tr)
    ax2.plot(ts, d_tr, color='#111111', lw=1.5, zorder=3)

    ax2.axhline(EPS_M, color=C_META, lw=0.9, ls='--',
                label=f'$\\varepsilon_m={EPS_M}$')
    ax2.axhline(EPS_C, color=C_RUPT, lw=0.9, ls='--',
                label=f'$\\varepsilon_c={EPS_C}$')

    # State region labels
    x_closed = T * 0.20
    x_meta   = T * 0.62
    ax2.text(x_closed, EPS_M + 0.03, 'closed',
             ha='center', fontsize=6.5, color=C_CLOSED, style='italic')
    ax2.text(x_meta,   (EPS_M + EPS_C) / 2 - 0.01, 'meta-review',
             ha='center', fontsize=6.5, color=C_META, style='italic')

    # Annotate rupture
    ax2.annotate(
        f'$d_{{T}} = {d_tr[-1]:.2f}$:\n$M_t(x)\\approx M_t(y)$',
        xy=(T - 1, d_tr[-1]),
        xytext=(max(0, T - 32), 0.16),
        arrowprops=dict(arrowstyle='->', lw=0.7, color=C_RUPT),
        fontsize=6.5, color=C_RUPT)

    ax2.set_xlabel('Step $t$')
    ax2.set_ylabel('$d_t = \\|M_t(x){-}M_t(y)\\|$')
    ax2.set_ylim(bottom=0)
    ax2.set_xlim(0, T)
    ax2.set_title('(b) STAK-PSAL: distinguishability collapses', fontsize=9)
    ax2.legend(loc='upper right', fontsize=7.5)

    out = os.path.join(FIGS, 'fig_blindspot.pdf')
    plt.savefig(out, bbox_inches='tight')
    plt.close()
    print(f'  Figure (blind-spot) → {out}')


# ═══════════════════════════════════════════════════════════════════════════
# Run
# ═══════════════════════════════════════════════════════════════════════════

if __name__ == '__main__':
    print('Generating figures...')
    fig1_architecture()
    fig2_regime()
    fig3_trajectory()
    fig_blindspot()
    print('Done.')
