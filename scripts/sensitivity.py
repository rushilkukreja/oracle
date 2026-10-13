#!/usr/bin/env python3
"""One-at-a-time sensitivity of the headline results (Section 3.11).

Each case changes one assumption from the base case and reruns the model.
Writes results/sensitivity.csv with, for every case, the mean emissions per
rocket launch (t), the total for all constellations (Mt), the mean emissions
per subscriber across constellations (kg) and the launch-event share of the
constellation total.

Usage:
    python3 scripts/sensitivity.py
"""

import copy
import csv
import statistics
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import oracle_model as model  # noqa: E402

CO2_PER_C = 44.009 / 12.011


def liquid_factor(of, carbon):
    """kg CO2 per kg propellant for a bipropellant at oxidizer-to-fuel ratio of."""
    return carbon / (1 + of) * CO2_PER_C


def run(params, factors, rocket_rows, logistics, constellation_rows, **options):
    rockets = model.rocket_model(params, factors, rocket_rows, logistics, options.get("transport_propellant", False))
    planned = copy.deepcopy(constellation_rows)
    if options.get("planned_per_launch"):
        for c in planned.values():
            c["planned_payload"] = 0.0
    cs = model.constellation_model(params, rockets, planned)
    per_launch = statistics.fmean(sum(r["amortized"].values()) for r in rockets.values())
    total = sum(sum(c["total_kg"].values()) for c in cs.values())
    per_sub = statistics.fmean(sum(c["per_subscriber"].values()) for c in cs.values())
    launch_share = sum(c["total_kg"]["Launch Event"] for c in cs.values()) / total
    return per_launch / 1000, total / 1e9, per_sub, launch_share


def main():
    base = model.load_inputs()
    params, factors, rocket_rows, logistics, constellation_rows = base

    def with_params(**changes):
        p = dict(params)
        p.update(changes)
        return (p, factors, rocket_rows, logistics, constellation_rows)

    def with_factor(name, value):
        f = copy.deepcopy(factors)
        f["propellant"][name] = value
        return (params, f, rocket_rows, logistics, constellation_rows)

    def with_reuse(flights):
        log = copy.deepcopy(logistics)
        for row in log.values():
            if row["Flights per reusable stage (R)"].strip():
                row["Flights per reusable stage (R)"] = str(flights)
        return (params, factors, rocket_rows, log, constellation_rows)

    cases = [
        ("Base case", base, {}),
        ("Transport includes propellant mass", base, {"transport_propellant": True}),
        ("Flights per reusable stage R = 10", with_reuse(10), {}),
        ("Flights per reusable stage R = 20", with_reuse(20), {}),
        ("Planned satellites per launch, not payload-scaled", base, {"planned_per_launch": True}),
        ("Kerosene O/F = 2.3", with_factor("Kerosene", liquid_factor(2.3, 0.859)), {}),
        ("Kerosene O/F = 2.7", with_factor("Kerosene", liquid_factor(2.7, 0.859)), {}),
        ("Hypergolic O/F = 1.7", with_factor("Hypergolic", liquid_factor(1.7, 0.400)), {}),
        ("Hypergolic O/F = 2.7", with_factor("Hypergolic", liquid_factor(2.7, 0.400)), {}),
        ("Kr/Xe emission factor x 0.5", with_params(kr_xe_emission_factor=params["kr_xe_emission_factor"] * 0.5), {}),
        ("Kr/Xe emission factor x 2", with_params(kr_xe_emission_factor=params["kr_xe_emission_factor"] * 2), {}),
        ("Subscribers per satellite x 0.5", with_params(subscribers_per_satellite=params["subscribers_per_satellite"] * 0.5), {}),
        ("Subscribers per satellite x 2", with_params(subscribers_per_satellite=params["subscribers_per_satellite"] * 2), {}),
    ]
    path = model.RESULTS / "sensitivity.csv"
    with open(path, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["Case", "Mean per launch (t)", "All constellations (Mt)", "Mean per subscriber (kg)", "Launch event share of total"])
        for label, inputs, options in cases:
            per_launch, total, per_sub, share = run(*inputs, **options)
            writer.writerow([label, f"{per_launch:,.0f}", f"{total:.2f}", f"{per_sub:,.0f}", f"{100 * share:.1f}%"])
            print(f"{label:52s} {per_launch:7,.0f} t {total:7.2f} Mt {per_sub:6,.0f} kg {100 * share:5.1f}%")
    print(f"Wrote {path}")


if __name__ == "__main__":
    main()
