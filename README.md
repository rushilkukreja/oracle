# Open-source Rocket and Constellation Lifecycle Emissions (ORACLE)

Welcome to the Open-source Rocket and Constellation Lifecycle Emissions 
(`oracle`) repository.

## Team
- Rushil Kukreja, Thomas Jefferson High School for Science and Technology.
- Prof. Edward J. Oughton, George Mason University.
- Prof. Richard Linares, Massachusetts Institute of Technology.

## Abstract
The proliferation of satellite megaconstellations in low earth orbit (LEO) represents a significant advancement in global broadband connectivity. However, we urgently need to understand the potential environmental impacts, particularly greenhouse gas (GHG) emissions associated with these constellations. This study addresses a critical gap in modeling current and future GHG emissions by developing a comprehensive open-source life cycle assessment (LCA) methodology, applied to 12 launch vehicles and 24 megaconstellations. Our analysis reveals that propellant combustion during launch events and launcher transportation contribute most significantly to overall GHG emissions, accounting for 85.9% of total constellation emissions. Among the rockets analyzed, reusable vehicles like Falcon-9 and Starship demonstrate 77.6% lower production emissions compared to non-reusable alternatives, highlighting the environmental benefits of reusability in space technology. The findings underscore the importance of launch vehicle and satellite design select to minimize potential environmental impact. The Open-source Rocket and Constellation Lifecycle Emissions (ORACLE) repository is freely available and aims to facilitate further research in this field. This study provides a critical baseline for policymakers and industry stakeholders to develop strategies for reducing the carbon footprint of the space industry, especially satellite megaconstellations.

## Citation
To reference this repository in your work, please cite the corresponding paper:
**Kukreja, R., Oughton, E. J., & Linares, R. (2025).**
*Greenhouse Gas (GHG) Emissions Poised to Rocket: Modeling the Environmental Impact of LEO Satellite Constellations.*
[arXiv:2504.15291](https://arxiv.org/abs/2504.15291)

## How to run
1. `python3 scripts/oracle_model.py` rebuilds every CSV in `results/` from the inputs in `data/raw/` (Python 3, no packages needed).
2. `python3 scripts/monte_carlo.py` runs the subscriber uncertainty analysis and writes `results/per_subscriber_uncertainty.csv` and `results/subscriber_uncertainty_by_country.csv`; `python3 scripts/paper_numbers.py` then prints every number quoted in the paper.
3. `Rscript vis/<script>.R` (one per figure, e.g. `Rscript vis/d_rocket_emissions.R`) regenerates the figures in `vis/figures/` from `results/`.
