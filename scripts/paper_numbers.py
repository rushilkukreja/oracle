#!/usr/bin/env python3
"""Print every number quoted in the paper's text, computed from the model.

Usage:
    python3 scripts/paper_numbers.py
"""

import statistics as st
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import oracle_model as model  # noqa: E402

# Conversions used in Section 4.4 (EPA, EIA and Encon figures cited in the paper).
T_PER_CAR_YEAR = 4.6
T_PER_HOUSEHOLD_YEAR = 9.2
KG_PER_TREE_YEAR = 21.9
SATELLITE_LIFETIME_YEARS = 5
GLOBAL_MT_PER_YEAR = 37000


def pct(x):
    return f"{100 * x:.1f}%"


def main():
    params, propellant_ef, material_ef, transport_ef, rocket_rows, logistics, constellation_rows = model.load_inputs()
    R = model.rocket_model(params, propellant_ef, material_ef, transport_ef, rocket_rows, logistics)
    C = model.constellation_model(params, R, constellation_rows)
    by_c, by_country = model.uncertainty(C, params["uncertainty_sd_fraction"])
    sd = {row[0]: row[3] for row in by_c + by_country}
    S, CS = model.ROCKET_STAGES, model.CONSTELLATION_STAGES

    print("== 4.1 Emissions per rocket")
    tot = {n: sum(r["amortized"].values()) for n, r in R.items()}
    avg = st.fmean(tot.values())
    print(f"average per launch: {avg / 1000:.0f} t ({avg / 1e6:.2f} kt)")
    for s in S:
        v = st.fmean(r["amortized"][s] for r in R.values())
        print(f"  {s}: {v / 1000:.0f} t ({pct(v / avg)})")
    hi = max(tot, key=tot.get); lo = min(tot, key=tot.get)
    print(f"highest: {hi} {tot[hi] / 1e6:.2f} kt, launch event share {pct(R[hi]['amortized']['Launch Event'] / tot[hi])}, payload {R[hi]['payload']} t")
    print(f"lowest: {lo} {tot[lo] / 1000:.0f} t, payload {R[lo]['payload']} t")
    per_t = {n: tot[n] / R[n]["payload"] / 1000 for n in R}
    print(f"per tonne of payload: highest {max(per_t, key=per_t.get)} {max(per_t.values()):.0f} t; lowest {min(per_t, key=per_t.get)} {min(per_t.values()):.1f} t; mean {st.fmean(per_t.values()):.0f} t")
    for n in ("Falcon-9", "Falcon-Heavy", "New Glenn", "Starship"):
        print(f"  {n}: production share {pct(R[n]['amortized']['Launcher Production'] / tot[n])}, per tonne payload {per_t[n] * 1000:,.0f} kg")
    tr = {n: R[n]["amortized"]["Launcher Transportation"] / tot[n] for n in R}
    print("transport share by rocket: " + ", ".join(f"{n} {pct(v)}" for n, v in sorted(tr.items(), key=lambda x: x[1])))
    print(f"no-launch-event rockets: {[n for n in R if R[n]['amortized']['Launch Event'] == 0]}")
    f9i = sum(R["Falcon-9"]["initial"].values()); f9a = tot["Falcon-9"]
    print(f"Falcon 9: new vehicle {f9i / 1000:,.0f} t -> reuse-life average {f9a / 1000:,.0f} t")
    reu = [n for n in R if R[n]["reusable"]]; non = [n for n in R if not R[n]["reusable"]]
    pr = lambda ns: st.fmean(R[n]["amortized"]["Launcher Production"] for n in ns)
    print(f"production, reusable vs expendable: {pct(1 - pr(reu) / pr(non))} lower")
    print(f"production share, expendable average: {pct(st.fmean(R[n]['amortized']['Launcher Production'] / tot[n] for n in non))}")
    launch_plus_prod = st.fmean(R[n]["amortized"]["Launch Event"] + R[n]["amortized"]["Launcher Production"] for n in R) / avg
    print(f"launch event + launcher production share of average rocket: {pct(launch_plus_prod)}")

    print("\n== 4.2 Emissions per constellation")
    T = {n: sum(c["total_kt"].values()) for n, c in C.items()}
    grand = sum(T.values())
    print(f"highest: {max(T, key=T.get)} {max(T.values()):,.0f} kt; lowest: {min(T, key=T.get)} {min(T.values()):,.1f} kt")
    for s in CS:
        print(f"  {s}: {pct(sum(c['total_kt'][s] for c in C.values()) / grand)} of all")
    for n in ("Starlink (Gen2)", "Kuiper", "Lacuna"):
        print(f"  {n}: " + ", ".join(f"{s} {pct(C[n]['total_kt'][s] / T[n])}" for s in CS))
    prod = {n: C[n]["total_kt"]["Launcher Production"] / T[n] for n in C}
    print(f"production share: min {min(prod, key=prod.get)} {pct(min(prod.values()))}, max {max(prod, key=prod.get)} {pct(max(prod.values()))}")
    le = {n: C[n]["total_kt"]["Launch Event"] / T[n] for n in C}
    print(f"launch event share: min {min(le, key=le.get)} {pct(min(le.values()))}, max {max(le, key=le.get)} {pct(max(le.values()))}")
    pl = {n: sum(c["per_launch_t"].values()) for n, c in C.items()}
    top = sorted(pl, key=pl.get, reverse=True)
    print("per launch: " + ", ".join(f"{n} {pl[n]:,.0f} t" for n in top[:3]) + f"; below 1,500 t: {sum(v < 1500 for v in pl.values())} of {len(pl)}")
    ps_share = {n: C[n]["total_kt"]["Satellite Kr/Xe Propellant"] / T[n] for n in C}
    print(f"satellite propellant share: max {max(ps_share, key=ps_share.get)} {pct(max(ps_share.values()))}")
    print("stand-in satellites per launch: " + ", ".join(f"{n} {C[n]['per_rocket']:.0f}" for n in C if C[n]["stand_in"]))

    print("\n== 4.3 Emissions per user")
    subs = {n: c["subscribers"] for n, c in C.items()}
    print(f"subscribers: {min(subs, key=subs.get)} {min(subs.values()):,.0f} to {max(subs, key=subs.get)} {max(subs.values()):,.0f}; under 7 million: {sum(v < 7e6 for v in subs.values())} of {len(subs)}")
    ps = {n: sum(c["per_subscriber"].values()) for n, c in C.items()}
    mean = st.fmean(ps.values())
    print(f"mean per user: {mean:.0f} (+/- {sd['All constellations (mean)']:.0f}) kg")
    for n in sorted(ps, key=ps.get, reverse=True):
        print(f"  {n}: {ps[n]:,.0f} (+/- {sd[n]:,.0f}) kg, {pct(ps[n] / mean - 1)} vs mean")
    country = {row[0]: row[1] for row in by_country}
    for k in sorted(country, key=country.get, reverse=True):
        print(f"  country {k}: {country[k]:.0f} (+/- {sd[k]:.0f}) kg, {pct(country[k] / mean - 1)} vs mean")

    print("\n== 4.4 Context")
    print(f"average launch {avg / 1000:,.0f} t = {avg / 1000 / T_PER_CAR_YEAR:,.0f} cars/yr = {avg / 1000 / T_PER_HOUSEHOLD_YEAR:,.0f} households/yr = {avg / KG_PER_TREE_YEAR:,.0f} trees/yr")
    st_kt = T["Starlink (Gen2)"]
    print(f"Starlink (Gen2): {st_kt:,.0f} kt = one year of {st_kt * 1000 / T_PER_CAR_YEAR:,.0f} cars")
    annual = mean / SATELLITE_LIFETIME_YEARS
    print(f"per user per year ({SATELLITE_LIFETIME_YEARS}-yr life): {annual:.0f} kg = {pct(annual / 5000)} of U.S. per capita, {pct(annual / 2000)} of global per capita")
    print(f"  vs fiber 20-50 kg/yr: {annual / 50:.1f}-{annual / 20:.1f}x; vs 5G 50-150 kg/yr: {annual / 150:.1f}-{annual / 50:.1f}x")
    print(f"all constellations once: {grand / 1000:.1f} Mt = {grand / 1000 / GLOBAL_MT_PER_YEAR:.3%} of a year of global CO2; per year over {SATELLITE_LIFETIME_YEARS} yr: {grand / 1000 / SATELLITE_LIFETIME_YEARS:.1f} Mt ({grand / 1000 / SATELLITE_LIFETIME_YEARS / GLOBAL_MT_PER_YEAR:.3%})")
    print(f"Cinnamon-937 + Semaphore-C share of all: {pct((T['Cinnamon-937'] + T['Semaphore-C']) / grand)}")

    print("\n== 4.5 Comparison")
    print(f"average launch event: {st.fmean(r['amortized']['Launch Event'] for r in R.values()) / 1000:.0f} t")


if __name__ == "__main__":
    main()
