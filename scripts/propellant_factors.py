#!/usr/bin/env python3
"""Propellant emission factors (Section 3.1) from propellant composition.

Each factor is the CO2 formed if all carbon in the propellant is oxidised
to CO2 in the engine and exhaust plume:

    EF = (mass fraction of carbon-bearing component)
         x (carbon mass fraction of that component) x 44.009 / 12.011

Reads data/raw/propellant_composition.csv and writes
data/raw/propellant_emissions_factors.csv (kg CO2 per kg propellant).

Usage:
    python3 scripts/propellant_factors.py
"""

import csv
from pathlib import Path

RAW = Path(__file__).resolve().parent.parent / "data" / "raw"
CO2_PER_C = 44.009 / 12.011


def main():
    with open(RAW / "propellant_composition.csv", newline="") as f:
        rows = list(csv.DictReader(f))
    with open(RAW / "propellant_emissions_factors.csv", "w", encoding="utf-8-sig", newline="") as f:
        f.write("Propellant,Emissions Factor\r\n")
        lines = []
        for r in rows:
            ef = float(r["Component mass fraction of propellant"]) * float(r["Carbon mass fraction of component"]) * CO2_PER_C
            lines.append(f"{r['Propellant']},{ef:.4f}")
            print(f"{r['Propellant']}: {ef:.4f} kg CO2 per kg propellant")
        f.write("\r\n".join(lines))


if __name__ == "__main__":
    main()
