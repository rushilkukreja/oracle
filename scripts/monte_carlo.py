#!/usr/bin/env python3
"""Monte Carlo uncertainty in emissions per subscriber (Section 3.10).

For each constellation, the number of subscribers is drawn from a normal
distribution with mean equal to the estimate and standard deviation equal to
SD_FRACTION x the estimate; non-positive draws are rejected. Total emissions,
subscribers and countries come straight from scripts/oracle_model.py, so the
central estimates match the model exactly. Writes
results/per_subscriber_uncertainty.csv and
results/subscriber_uncertainty_by_country.csv.

Usage:
    python3 scripts/monte_carlo.py
"""

import csv
import random
import statistics
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import oracle_model as model  # noqa: E402

RESULTS = model.RESULTS
SD_FRACTION = 0.10
NUM_SIMULATIONS = 10000
SEED = 42
KG_PER_KT = 1e6


def load():
    """(subscribers, total kt) and country for each constellation, from the model."""
    params, factors, rocket_rows, logistics, constellation_rows = model.load_inputs()
    rockets = model.rocket_model(params, factors, rocket_rows, logistics)
    constellations = model.constellation_model(params, rockets, constellation_rows)
    data = {n: (c["subscribers_exact"], sum(c["total_kg"].values()) / KG_PER_KT) for n, c in constellations.items()}
    return data, {n: c["country"] for n, c in constellations.items()}


def sample_subscribers(subscribers, rng):
    sampled = rng.gauss(subscribers, SD_FRACTION * subscribers)
    while sampled <= 0:
        sampled = rng.gauss(subscribers, SD_FRACTION * subscribers)
    return sampled


def main():
    """Per-subscriber emissions (kg CO2e) and their standard deviation for each
    constellation, for the average across constellations, and for each country
    (total emissions / total subscribers)."""
    rng = random.Random(SEED)
    data, country = load()
    countries = sorted(set(country.values()))
    samples = {key: [] for key in list(data) + ["Average across constellations"] + countries}
    for _ in range(NUM_SIMULATIONS):
        subs = {name: sample_subscribers(s, rng) for name, (s, _) in data.items()}
        ratios = {name: data[name][1] * KG_PER_KT / subs[name] for name in data}
        for name, value in ratios.items():
            samples[name].append(value)
        samples["Average across constellations"].append(statistics.fmean(ratios.values()))
        for k in countries:
            members = [n for n in data if country[n] == k]
            samples[k].append(sum(data[n][1] for n in members) * KG_PER_KT / sum(subs[n] for n in members))

    central = {name: total * KG_PER_KT / s for name, (s, total) in data.items()}
    central["Average across constellations"] = statistics.fmean(central[n] for n in data)
    for k in countries:
        members = [n for n in data if country[n] == k]
        central[k] = sum(data[n][1] for n in members) * KG_PER_KT / sum(data[n][0] for n in members)

    header = ["Emissions_Per_Subscriber (kg)", "Std_Dev_Emissions_Per_Subscriber (kg)"]
    outputs = {
        "per_subscriber_uncertainty.csv": ("Constellation", list(data) + ["Average across constellations"]),
        "subscriber_uncertainty_by_country.csv": ("Country", countries),
    }
    for filename, (label, keys) in outputs.items():
        with open(RESULTS / filename, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow([label] + header)
            for key in keys:
                writer.writerow([key, f"{central[key]:.1f}", f"{statistics.stdev(samples[key]):.1f}"])
        print(f"Wrote {RESULTS / filename}")


if __name__ == "__main__":
    main()
