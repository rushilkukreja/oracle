#!/usr/bin/env python3
"""ORACLE life cycle emissions model.

Rebuilds every CSV in results/ from the inputs in data/raw/, following the
equations in Section 3 of Kukreja, Oughton & Linares. The calculation order
and rounding mirror the spreadsheet that produced the published results, so
the output reproduces those files. Standard library only.

Usage:
    python3 scripts/oracle_model.py              # write results/
    python3 scripts/oracle_model.py --out DIR    # write somewhere else
    python3 scripts/oracle_model.py --check      # compare with results/ cell by cell, without writing

Units: rocket outputs are kg CO2e per launch, constellation totals are kt
CO2e, per-launch constellation outputs are t CO2e and per-subscriber outputs
are kg CO2e.
"""

import argparse
import csv
import io
import math
import statistics
from decimal import ROUND_HALF_UP, Decimal
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
RESULTS = ROOT / "results"

ROCKET_STAGES = [
    "Launch Event",
    "Launcher Production",
    "Electronics Production",
    "Launcher Transportation",
    "Electricity Consumption",
]
CONSTELLATION_STAGES = ROCKET_STAGES + ["Satellite Kr/Xe Propellant"]


# ---------------------------------------------------------------- rounding and formatting

def round_half_up(x, digits=0):
    """Spreadsheet-style rounding (0.5 rounds away from zero)."""
    q = Decimal(1).scaleb(-digits)
    return float(Decimal(repr(x)).quantize(q, rounding=ROUND_HALF_UP))


def fmt_int(x):
    return str(int(round_half_up(x)))


def fmt_thousands(x):
    return f"{int(round_half_up(x)):,}"


def fmt_fixed(x, digits):
    return f"{round_half_up(x, digits):.{digits}f}"


def fmt_general(x, digits=None):
    """Spreadsheet 'General' format: no trailing zeros, at most 11 characters."""
    if digits is not None:
        x = round_half_up(x, digits)
    if x == int(x):
        return str(int(x))
    whole = len(str(int(abs(x))))
    decimals = max(0, 11 - whole - 1 - (x < 0))
    text = f"{round_half_up(x, decimals):.{decimals}f}".rstrip("0").rstrip(".")
    return text


def fmt_float(x):
    """Python float text, as written by pandas (577.0, 0.8)."""
    return repr(float(x))


# ---------------------------------------------------------------- inputs

def num(value):
    value = value.strip().replace(",", "")
    return float(value) if value else 0.0


def read_rows(name):
    with open(RAW / name, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def read_keyed(name, key):
    return {row[key].strip(): row for row in read_rows(name)}


def load_inputs():
    params = {r["Parameter"]: float(r["Value"]) for r in read_rows("model_parameters.csv")}
    factors = {
        "propellant": {r["Propellant"]: float(r["Emissions Factor"]) for r in read_rows("propellant_emissions_factors.csv")},
        "material": {r["Material"]: float(r["Emissions Factor"]) for r in read_rows("dry_mass_emissions_factors.csv")},
        "transport": {r["Transportation"]: float(r["Emissions Factor"]) for r in read_rows("transportation_emissions_factors.csv")},
    }
    rockets = read_keyed("rocket_data.csv", "Rocket")
    logistics = read_keyed("rocket_logistics.csv", "Rocket")

    # constellation_parameters.csv is stored transposed: one column per constellation.
    with open(RAW / "constellation_parameters.csv", encoding="utf-8-sig", newline="") as f:
        table = list(csv.reader(f))
    names = table[0][1:]
    fields = {row[0]: row[1:] for row in table[1:]}
    metadata = read_keyed("constellation_metadata.csv", "Constellation")
    constellations = {}
    for i, name in enumerate(names):
        meta = metadata[name]
        constellations[name] = {
            "satellites": num(fields["Number of satellites"][i]),
            "per_rocket": num(fields["Satellites per rocket"][i]),
            "mass": num(fields["Satellite mass (kg)"][i]),
            "country": meta["Country"],
            "rocket": meta["Modeled rocket"],
            "reusable": meta["Reusable launcher"] == "Yes",
            "in_country_means": meta["In country means"] == "Yes",
        }
    return params, factors, rockets, logistics, constellations


# ---------------------------------------------------------------- model

def amortization_factor(log, params):
    """Eq. 5: share of full-vehicle production charged to each launch.

    A = (1 - S1P) + S1P / R + R_A x (R - 1) / R, i.e. the first flight builds the
    whole vehicle and each of the R - 1 reflights rebuilds the expendable stages
    and refurbishes the rest. Expendable rockets have A = 1.
    """
    if not log["Flights per reusable stage (R)"].strip():
        return 1.0
    s1p = float(log["S1P"])
    flights = float(log["Flights per reusable stage (R)"])
    return (1 - s1p) + s1p / flights + params["refurbishment_factor"] * (flights - 1) / flights


def rocket_model(params, factors, rockets, logistics):
    """Per-launch emissions for each rocket (kg CO2e) on a new vehicle, averaged over
    the reuse life, and on a subsequent (reflown) launch."""
    out = {}
    for name, r in rockets.items():
        log = logistics[name]
        dry = float(log["Dry mass (kg)"]) if log["Dry mass (kg)"].strip() else num(r["Dry Mass"])
        structure = dry * params["structure_fraction"]
        material = log["Material"]
        # Eq. 1: propellant combustion.
        launch = sum(num(r[p]) * ef for p, ef in factors["propellant"].items())
        # Eq. 2: structure and electronics production.
        production = structure * factors["material"][material]
        electronics = dry * params["electronics_fraction"] * factors["material"]["Electronics"]
        # Eq. 3: manufacturing electricity, per kg of structural material.
        key = "electricity_" + ("steel" if material == "Steel" else "aluminum_alloy")
        electricity = structure * params[key]
        # Eq. 7: transport of the fuelled vehicle (t) over the route (km).
        mode = log["Transportation"]
        transport = 0.0
        if mode != "None":
            mass_t = (num(r["Dry Mass"]) + num(r["Propellant"])) / 1000
            transport = mass_t * float(log["Distance (km)"]) * factors["transport"][mode]

        share = amortization_factor(log, params)
        reusable = share != 1.0
        s1p = float(log["S1P"]) if reusable else 1.0
        initial = {
            "Launch Event": launch,
            "Launcher Production": production,
            "Electronics Production": electronics,
            "Launcher Transportation": transport,
            "Electricity Consumption": electricity,
        }
        # Eq. 4: production and electricity spread over the reuse life.
        amortized = dict(initial)
        for stage in ("Launcher Production", "Electronics Production", "Electricity Consumption"):
            amortized[stage] = initial[stage] * share
        out[name] = {
            "payload": num(r["Payload Capacity"]),
            "dry_mass": num(r["Dry Mass"]),
            "reusable": reusable,
            "initial": initial,
            "amortized": amortized,
            # A reflight rebuilds the expendable upper stage and refurbishes the rest.
            "subsequent_share": (1 - s1p) + params["refurbishment_factor"],
        }
    return out


def constellation_model(params, rockets, constellations):
    """Launches, totals (kg) and per-subscriber emissions for each constellation."""
    kr_xe = params["kr_xe_fraction"] * params["kr_xe_emission_factor"]
    out = {}
    for name, c in constellations.items():
        per_launch = rockets[c["rocket"]]["amortized"]
        launches = math.ceil(c["satellites"] / c["per_rocket"])
        # Eq. 9: launches needed x per-launch emissions; Eq. 10: Kr/Xe propellant.
        total_kg = {s: launches * per_launch[s] for s in ROCKET_STAGES}
        total_kg["Satellite Kr/Xe Propellant"] = c["satellites"] * c["mass"] * kr_xe
        total_kt = {s: round_half_up(v / 1e6, 1) for s, v in total_kg.items()}
        # Eq. 11: subscribers scale with constellation size.
        subscribers_exact = c["satellites"] * params["subscribers_per_satellite"]
        subscribers = round_half_up(subscribers_exact)
        out[name] = {
            **c,
            "launches": launches,
            "subscribers": subscribers,
            "subscribers_exact": subscribers_exact,
            "per_launch_kg": per_launch,
            "propulsion_per_launch_kg": c["per_rocket"] * c["mass"] * kr_xe,
            "total_kg": total_kg,
            "total_kt": total_kt,
            # Eq. 12: emissions per subscriber.
            "per_subscriber": {s: v / subscribers for s, v in total_kg.items()},
        }
    return out


# ---------------------------------------------------------------- output tables

def per_launch_t(x):
    """Per-launch values in tonnes: whole tonnes, or one decimal below 1 t."""
    return round_half_up(x) if x >= 1 else round_half_up(x, 1)


def build_tables(params, rockets, constellations):
    """Each table: (format, header, rows of already-formatted strings)."""
    rs = list(rockets.items())
    cs = list(constellations.items())
    excel = {"bom": True, "newline": "\r\n", "final_newline": False}
    plain = {"bom": False, "newline": "\n", "final_newline": True}
    tables = {}

    rocket_int = {n: {s: round_half_up(v) for s, v in r["amortized"].items()} for n, r in rs}
    tables["rocket_emissions.csv"] = (plain, ["Rocket"] + ROCKET_STAGES,
        [[n] + [fmt_thousands(rocket_int[n][s]) for s in ROCKET_STAGES] for n, _ in rs])
    tables["adjusted_rocket_emissions.csv"] = (plain, ["Rocket"] + ROCKET_STAGES,
        [[n] + [fmt_thousands(r["amortized"][s] / r["payload"]) for s in ROCKET_STAGES] for n, r in rs])
    tables["size_emissions.csv"] = (excel, ["Rocket", "Dry Mass", "Emissions"],
        [[n.replace("Long March 5", "Long March-5"), fmt_thousands(r["dry_mass"]), fmt_thousands(sum(r["amortized"].values()))] for n, r in rs])

    reuse_rows = []
    for n, r in rs:
        if r["reusable"]:
            initial = {s: round_half_up(v) for s, v in r["initial"].items()}
            subsequent = dict(initial)
            for s in ("Launcher Production", "Electronics Production", "Electricity Consumption"):
                subsequent[s] = initial[s] * r["subsequent_share"]
            reuse_rows.append([n + " Initial"] + [fmt_fixed(initial[s], 2) for s in ROCKET_STAGES])
            reuse_rows.append([n + " Subsequent"] + [fmt_fixed(subsequent[s], 2) for s in ROCKET_STAGES])
    tables["reusability.csv"] = (excel, ["Rocket"] + ROCKET_STAGES, reuse_rows)
    tables["reusability_rockets.csv"] = (excel, ["Rocket"] + ROCKET_STAGES, [
        [label] + [fmt_int(statistics.fmean(r["amortized"][s] for _, r in rs if r["reusable"] == flag)) for s in ROCKET_STAGES]
        for label, flag in (("Reusable", True), ("Non-Reusable", False))])

    tables["constellation_emissions.csv"] = (plain, ["Constellation"] + CONSTELLATION_STAGES,
        [[n] + [fmt_general(c["total_kt"][s]) for s in CONSTELLATION_STAGES] for n, c in cs])
    per_launch_rows = []
    for n, c in cs:
        values = [per_launch_t(round_half_up(c["per_launch_kg"][s]) / 1000) for s in ROCKET_STAGES]
        values.append(per_launch_t(c["propulsion_per_launch_kg"] / 1000))
        per_launch_rows.append([n] + [fmt_float(v) for v in values])
    tables["per_launch_emissions.csv"] = (plain, ["Constellation"] + CONSTELLATION_STAGES, per_launch_rows)
    tables["per_subscriber_emissions.csv"] = (plain, ["Constellation"] + CONSTELLATION_STAGES,
        [[n] + [fmt_int(c["per_subscriber"][s]) for s in CONSTELLATION_STAGES] for n, c in cs])

    total = {n: sum(c["total_kt"].values()) for n, c in cs}
    tables["constellation_size_emissions.csv"] = (excel, ["Constellation", "Size", "Emissions", "", "", "", ""],
        [[n, fmt_thousands(c["satellites"]), fmt_general(total[n], 1), "", "", "", ""] for n, c in cs if c["in_country_means"]])
    tables["subscribers_emissions.csv"] = (excel, ["Constellation", "Subscribers", "Emissions"],
        [[n, fmt_thousands(c["subscribers"]), fmt_thousands(total[n])] for n, c in cs])

    countries = sorted({c["country"] for _, c in cs})
    rows = []
    for k in countries:
        members = [c for _, c in cs if c["country"] == k and c["in_country_means"]]
        if members:
            rows.append([k] + [fmt_general(statistics.fmean(c["total_kt"][s] for c in members), 2) for s in CONSTELLATION_STAGES])
    tables["constellation_emissions_by_country.csv"] = (excel, ["Country"] + CONSTELLATION_STAGES, rows)
    rows = []
    for k in countries:
        members = [c for _, c in cs if c["country"] == k]
        subs = sum(c["subscribers_exact"] for c in members)
        rows.append([k] + [fmt_general(sum(c["total_kt"][s] for c in members) * 1e6 / subs) for s in CONSTELLATION_STAGES])
    tables["subscriber_emissions_by_country.csv"] = (excel, ["Country"] + CONSTELLATION_STAGES, rows)

    groups = {flag: [c for _, c in cs if c["reusable"] == flag] for flag in (True, False)}
    tables["reusability_constellations.csv"] = (excel, ["Reusable"] + CONSTELLATION_STAGES, [
        [label] + [fmt_general(statistics.fmean(c["total_kt"][s] for c in groups[flag])) for s in CONSTELLATION_STAGES]
        for label, flag in (("Reusable", True), ("Non-Reusable", False))])
    rows = []
    for label, flag in (("Non-Reusable", False), ("Reusable", True)):
        subs = sum(c["subscribers_exact"] for c in groups[flag])
        rows.append([label] + [fmt_general(sum(c["total_kt"][s] for c in groups[flag]) * 1e6 / subs, 5) for s in CONSTELLATION_STAGES])
    tables["reusability_per_subscriber.csv"] = (excel, ["Reusable"] + CONSTELLATION_STAGES, rows)
    return tables


def render(fmt, header, rows):
    buffer = io.StringIO()
    csv.writer(buffer, lineterminator=fmt["newline"]).writerows([header] + rows)
    text = buffer.getvalue()
    if not fmt["final_newline"]:
        text = text[: -len(fmt["newline"])]
    return ("﻿" if fmt["bom"] else "") + text


def check(tables, ref_dir):
    """Compare every cell with the existing results; returns the number of differences."""
    differences = 0
    for name, (fmt, header, rows) in tables.items():
        path = ref_dir / name
        produced = render(fmt, header, rows)
        expected = path.read_text(encoding="utf-8", newline="")
        if produced == expected:
            print(f"{name}: identical")
            continue
        ref_rows = list(csv.reader(io.StringIO(expected.lstrip("﻿"))))
        new_rows = list(csv.reader(io.StringIO(produced.lstrip("﻿"))))
        cells = []
        for i, (a, b) in enumerate(zip(new_rows, ref_rows)):
            for j, (x, y) in enumerate(zip(a, b)):
                if x != y:
                    cells.append(f"{b[0]} / {ref_rows[0][j]}: model {x!r} vs results {y!r}")
        if len(new_rows) != len(ref_rows):
            cells.append(f"row count: model {len(new_rows)} vs results {len(ref_rows)}")
        if not cells:
            cells.append("values identical; file formatting differs")
        print(f"{name}: {len(cells)} differences")
        for c in cells:
            print("    " + c)
        differences += len(cells)
    return differences


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--out", type=Path, default=RESULTS, help="output directory (default: results/)")
    parser.add_argument("--check", action="store_true", help="compare with results/ instead of writing")
    args = parser.parse_args()

    params, factors, rocket_rows, logistics, constellation_rows = load_inputs()
    rockets = rocket_model(params, factors, rocket_rows, logistics)
    constellations = constellation_model(params, rockets, constellation_rows)
    tables = build_tables(params, rockets, constellations)

    if args.check:
        raise SystemExit(1 if check(tables, RESULTS) else 0)
    args.out.mkdir(parents=True, exist_ok=True)
    for name, (fmt, header, rows) in tables.items():
        (args.out / name).write_text(render(fmt, header, rows), encoding="utf-8", newline="")
    print(f"Wrote {len(tables)} files to {args.out}")


if __name__ == "__main__":
    main()
