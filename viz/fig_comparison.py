#!/usr/bin/env python3
"""
Generate fig4_comparison.pdf: R3 vs R2 side-by-side showing that identical
instantaneous status conceals radically different structural state (Fn).

Layout: 2 rows × 2 columns
  Top row:    d_t trajectory with threshold bands
  Bottom row: cumulative fatigue F_n
  Left col:   R3 (stable recovery)     boost = 0.25
  Right col:  R2 (metastable closure)  boost = 0.07, seed chosen for clarity
"""

import os, sys
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

BASE = os.path.join(os.path.dirname(__file__), '..')
FIGS = os.path.join(BASE, 'docs', 'paper', 'figs')
os.makedirs(FIGS, exist_ok=True)

# ── Simulation (mirror of make_figures.py, uniform degradation for clarity) ──

EPS_C = 0.08
EPS_M = 0.22
DELTA = 0.020
DECAY = 0.92

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-np.clip(x, -500, 500)))

def project(W, z):
    return sigmoid(W @ z)

def make_W():
    W = np.zeros((2, 64))
    W[0, 0] = 5.0
    W[1, 1] = 1.0; W[1, 2] = 1.0
    return W

def make_inputs():
    x = np.full(64, 0.50); x[0] = 0.20
    y = np.full(64, 0.50); y[0] = 0.90
    return x, y

def simulate(seed, boost, n_steps=200):
    rng = np.random.default_rng(seed)
    W   = make_W()
    x, y = make_inputs()

    status     = 'CLOSED'
    T          = 0.0; T_slope = 0.0
    sep_ema    = None
    fatigue    = 0.0
    T_entry    = 0.0; steps_meta = 0; T_prev = 0.0
    enriched   = False

    rcls_t = -1; rupt_t = -1; crit_t = -1

    trace = dict(d=[], F=[], status=[], T_slope=[], R=[], E_r=[])
    prev_status = 'CLOSED'

    for t in range(n_steps):
        mx = project(W, x); my = project(W, y)
        d  = float(np.linalg.norm(mx - my))

        strain  = max(0.0, EPS_M - d)
        T_new   = T * DECAY + strain * 0.6
        T_slope = 0.25 * (T_new - T) + 0.75 * T_slope
        if sep_ema is None: sep_ema = d
        sep_ema = 0.10 * d + 0.90 * sep_ema
        R = 1.0 / (d + 0.001)
        T = T_new

        if d <= EPS_C:
            next_status = 'HARD_RUPTURE'
        elif d <= EPS_M:
            next_status = 'META_REVIEW'
        else:
            next_status = 'CLOSED'

        if next_status == 'META_REVIEW' and prev_status == 'CLOSED' and not enriched:
            j_str = int(np.argmax(np.abs(x - y)))
            W[0, j_str] += boost
            enriched   = True
            T_entry    = T; steps_meta = 1
        elif next_status == 'META_REVIEW':
            steps_meta += 1
        elif next_status == 'CLOSED' and prev_status == 'META_REVIEW':
            if rcls_t == -1: rcls_t = t
            steps_meta = 0; enriched = False
        elif next_status == 'CLOSED':
            steps_meta = 0

        E_r = float('nan')
        if next_status == 'META_REVIEW' and steps_meta > 2:
            E_r = (T_entry - T) / steps_meta

        if next_status == 'META_REVIEW':
            dT     = abs(T - T_prev)
            el_neg = max(0.0, -E_r) if not np.isnan(E_r) else 0.0
            rp_ex  = max(0.0, R - 5.0)
            fatigue += dT + el_neg * 0.1 + rp_ex * 0.01

        if next_status == 'META_REVIEW' and steps_meta > 2 and crit_t == -1:
            if T_slope > 0.002 and R > 10.0 and not np.isnan(E_r) and E_r < 0.005:
                crit_t = t

        trace['d'].append(d)
        trace['F'].append(fatigue)
        trace['status'].append(next_status)
        trace['T_slope'].append(T_slope)
        trace['R'].append(R)
        trace['E_r'].append(E_r)

        T_prev = T; prev_status = next_status
        if next_status == 'HARD_RUPTURE':
            rupt_t = t; break

        W = W * (1.0 - DELTA)

    return trace, rcls_t, rupt_t, crit_t


def find_r2_seed(boost=0.07, seeds=range(1, 80)):
    """Find a seed with reclosure then rupture, decent gap, and lead >= 8."""
    best = None
    for s in seeds:
        tr, rcls, rupt, crit = simulate(s, boost)
        if rcls == -1 or rupt == -1 or crit == -1: continue
        gap  = rupt - rcls
        lead = rupt - crit
        if gap >= 15 and lead >= 8:
            score = gap + lead
            if best is None or score > best[0]:
                best = (score, s, tr, rcls, rupt, crit)
    return best[1:] if best else None


# ── Colour / style ────────────────────────────────────────────────────────────

plt.rcParams.update({
    'font.family'       : 'DejaVu Serif',
    'font.size'         : 8.5,
    'axes.titlesize'    : 9,
    'axes.labelsize'    : 8.5,
    'xtick.labelsize'   : 7.5,
    'ytick.labelsize'   : 7.5,
    'figure.dpi'        : 200,
    'text.usetex'       : False,
    'lines.linewidth'   : 1.4,
    'axes.spines.top'   : False,
    'axes.spines.right' : False,
})

C_CLOSED = '#2166ac'   # blue
C_META   = '#d6604d'   # orange-red
C_RUPT   = '#b2182b'   # dark red
C_FN_R3  = '#4dac26'   # green
C_FN_R2  = '#b2182b'   # dark red

STATUS_COLOR = {'CLOSED': C_CLOSED, 'META_REVIEW': C_META, 'HARD_RUPTURE': C_RUPT}

ANNOT = dict(fontsize=7.5, ha='center', va='bottom',
             bbox=dict(boxstyle='round,pad=0.2', fc='white', ec='none', alpha=0.8))


def shade_status(ax, statuses, alpha=0.08):
    """Shade background by status band."""
    n = len(statuses)
    i = 0
    while i < n:
        s = statuses[i]; j = i
        while j < n and statuses[j] == s: j += 1
        c = {'CLOSED': '#ddeeff', 'META_REVIEW': '#ffe8d8',
             'HARD_RUPTURE': '#ffe0e0'}.get(s, 'white')
        ax.axvspan(i, j, color=c, alpha=alpha, linewidth=0)
        i = j


def plot_d(ax, trace, rcls_t, rupt_t, crit_t, title):
    d   = np.array(trace['d'])
    st  = trace['status']
    n   = len(d)
    t   = np.arange(n)

    shade_status(ax, st, alpha=0.12)

    ax.axhline(EPS_M, color='#888', lw=0.8, ls='--')
    ax.axhline(EPS_C, color='#888', lw=0.8, ls=':')
    ax.text(n * 0.01, EPS_M + 0.01, r'$\varepsilon_m$', fontsize=7, color='#555')
    ax.text(n * 0.01, EPS_C + 0.01, r'$\varepsilon_c$', fontsize=7, color='#555')

    # colour d_t by status
    for i in range(n - 1):
        ax.plot(t[i:i+2], d[i:i+2], color=STATUS_COLOR.get(st[i], '#555'), lw=1.4)

    if rcls_t != -1:
        ax.axvline(rcls_t, color='#555', lw=0.8, ls='-.')
        ax.text(rcls_t, ax.get_ylim()[1] * 0.92, 'reclosure', fontsize=6.5,
                ha='center', color='#555')
    if crit_t != -1:
        ax.axvline(crit_t, color=C_RUPT, lw=0.9, ls='--')
        ax.text(crit_t, ax.get_ylim()[1] * 0.72, 'CRITICAL', fontsize=6.5,
                ha='center', color=C_RUPT)
    if rupt_t != -1:
        ax.axvline(rupt_t, color=C_RUPT, lw=1.0, ls='-')

    ax.set_title(title, fontweight='bold')
    ax.set_ylabel(r'separation $d_t$')
    ax.set_ylim(-0.02, 0.80)
    ax.set_xlim(0, n + 1)


def plot_fn(ax, trace, rcls_t, label_color):
    F  = np.array(trace['F'])
    st = trace['status']
    n  = len(F)
    t  = np.arange(n)

    shade_status(ax, st, alpha=0.12)
    ax.plot(t, F, color=label_color, lw=1.5)

    if rcls_t != -1:
        ax.axvline(rcls_t, color='#555', lw=0.8, ls='-.')

    ax.set_xlabel('time step $t$')
    ax.set_ylabel(r'cumulative fatigue $\mathcal{F}_n$')
    ax.set_xlim(0, n + 1)


# ── Run simulations ───────────────────────────────────────────────────────────

print("Finding R2 seed …")
r2 = find_r2_seed(boost=0.07)
if r2 is None:
    print("No suitable R2 seed found; trying boost=0.05")
    r2 = find_r2_seed(boost=0.05)
if r2 is None:
    print("Falling back to seed 3 boost=0.07")
    fallback = simulate(3, 0.07)
    r2 = (3, fallback[0], fallback[1], fallback[2], fallback[3])
r2_seed, r2_tr, r2_rcls, r2_rupt, r2_crit = r2
print(f"  R2: seed={r2_seed}  rcls={r2_rcls}  rupt={r2_rupt}  crit={r2_crit}")

r3_tr, r3_rcls, r3_rupt, r3_crit = simulate(r2_seed, 0.25)
print(f"  R3: seed={r2_seed}  rcls={r3_rcls}  rupt={r3_rupt}  crit={r3_crit}")

# ── Draw figure ───────────────────────────────────────────────────────────────

fig, axes = plt.subplots(2, 2, figsize=(6.5, 4.0),
                         gridspec_kw=dict(hspace=0.42, wspace=0.35))

plot_d(axes[0, 0], r3_tr, r3_rcls, r3_rupt, r3_crit,
       'R3: Stable Recovery  (boost = 0.25)')
plot_d(axes[0, 1], r2_tr, r2_rcls, r2_rupt, r2_crit,
       'R2: Metastable Closure  (boost = 0.07)')

plot_fn(axes[1, 0], r3_tr, r3_rcls, C_FN_R3)
plot_fn(axes[1, 1], r2_tr, r2_rcls, C_FN_R2)

# Align Fn y-axes
fn_max = max(max(r3_tr['F']), max(r2_tr['F'])) * 1.15 + 0.05
for ax in axes[1]:
    ax.set_ylim(-0.05, fn_max)

# Annotate Fn values at reclosure for R3
if r3_rcls != -1:
    fn_r3 = r3_tr['F'][r3_rcls]
    axes[1, 0].annotate(f'$\\mathcal{{F}}_n = {fn_r3:.2f}$',
                        xy=(r3_rcls, fn_r3),
                        xytext=(r3_rcls + len(r3_tr['d']) * 0.12, fn_r3 + fn_max * 0.08),
                        arrowprops=dict(arrowstyle='->', color='#444', lw=0.8),
                        fontsize=7, color='#444')

# Annotate Fn values at reclosure for R2
if r2_rcls != -1:
    fn_r2 = r2_tr['F'][r2_rcls]
    axes[1, 1].annotate(f'$\\mathcal{{F}}_n = {fn_r2:.2f}$',
                        xy=(r2_rcls, fn_r2),
                        xytext=(r2_rcls + len(r2_tr['d']) * 0.12, fn_r2 + fn_max * 0.08),
                        arrowprops=dict(arrowstyle='->', color='#444', lw=0.8),
                        fontsize=7, color='#444')

# Legend patches for status colours
legend_patches = [
    mpatches.Patch(color=C_CLOSED, label=r'\textsc{closed}'),
    mpatches.Patch(color=C_META,   label=r'\textsc{meta\_review}'),
    mpatches.Patch(color=C_RUPT,   label=r'\textsc{hard\_rupture}'),
]
fig.legend(handles=legend_patches, loc='lower center', ncol=3,
           fontsize=7, framealpha=0.9, bbox_to_anchor=(0.5, -0.01))

fig.suptitle(
    'At reclosure: both systems appear identical in $d_t$ and status; '
    r'$\mathcal{F}_n$ separates them.',
    fontsize=8, y=1.01, ha='center')

out = os.path.join(FIGS, 'fig4_comparison.pdf')
fig.savefig(out, bbox_inches='tight')
print(f"Saved → {out}")
