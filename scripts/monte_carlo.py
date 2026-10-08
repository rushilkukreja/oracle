#!/usr/bin/env python3
"""Monte Carlo uncertainty in emissions per subscriber (Section 3.11).

For each constellation, the number of subscribers is drawn from a normal
distribution with mean equal to the estimate and standard deviation equal to
SD_FRACTION x the estimate; non-positive draws are rejected. Total emissions
come from results/constellation_emissions.csv (kt, summed over all stages),
subscribers from results/subscribers_emissions.csv, and countries from
data/raw/constellation_metadata.csv.

Usage:
    python3 scripts/monte_carlo.py
"""

import csv
import random
import statistics
import sys
from pathlib import Path

RESULTS = Path(__file__).resolve().parent.parent / "results"
SD_FRACTION = 0.10
NUM_SIMULATIONS = 10000
SEED = 42
KG_PER_KT = 1e6


def num(value):
    return float(value.replace(",", ""))


def load():
    with open(RESULTS / "constellation_emissions.csv", newline="") as f:
        emissions = {row["Constellation"]: sum(num(v) for k, v in row.items() if k != "Constellation") for row in csv.DictReader(f)}
    with open(RESULTS / "subscribers_emissions.csv", encoding="utf-8-sig", newline="") as f:
        subscribers = {row["Constellation"]: num(row["Subscribers"]) for row in csv.DictReader(f)}
    return {name: (subscribers[name], emissions[name]) for name in subscribers}


def country_of():
    with open(RESULTS.parent / "data" / "raw" / "constellation_metadata.csv", newline="") as f:
        return {row["Constellation"]: row["Country"] for row in csv.DictReader(f)}


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
    data = load()
    country = country_of()
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

    writer = csv.writer(sys.stdout)
    writer.writerow(["Constellation or country", "Emissions_Per_Subscriber (kg)", "Std_Dev_Emissions_Per_Subscriber (kg)"])
    for key in samples:
        writer.writerow([key, f"{central[key]:.1f}", f"{statistics.stdev(samples[key]):.1f}"])


if __name__ == "__main__":
    main()
