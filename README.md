# GOOS Polar Argo figures

Maps and charts of Argo float coverage in the Arctic and Antarctic regions (all countries'
floats), produced for the Global Ocean Observing System (GOOS). 

The final figure is
`output/figures/GOOS_argo_polar_option_B.png`: Antarctic (left) and Arctic (right) panels showing the last five years of tracks of floats active in July 2026 (orange) over historical tracks (blue), with the 60° parallel and the median winter-maximum sea
ice extent (2021–2025).

## Scripts

Run from the repository root, e.g. `Rscript scripts/03_polar_map_panels_north.R`.

| Script | Output |
|---|---|
| `01_fetch_argo_index.R` | Argo GDAC index -> `output/rds/argo_index_clean.rds` |
| `02_sea_ice_extent_lines.R` | median sea ice extent lines -> `output/gis/seaice_extent_lines_{south,north}.rds` |
| `03_polar_map_panels_north.R` | Arctic visualisations -> `output/figures/panels/` |
| `04_polar_map_panels_south.R` | Antarctic visualisations -> `output/figures/panels/` |
| `05_polar_composites.R` | `output/figures/GOOS_argo_polar_option_{A,B}.png` (B = final output) |
| `06_active_vs_deployments.R` | trend charts -> `output/figures/GOOS_argo_{active_vs_deployments,fleet_age_and_replacement}.png`, `output/tables/` |

Run order: `01` -> `02` (only if the sea ice data changes) -> `03` -> `04` -> `05`;
`06` only needs `01`.

## Requirements

R packages: `data.table`, `sf`, `terra`, `ggplot2`, `maptiles`, `magick`, `png`,
`ragg`, `systemfonts`, `rnaturalearth` (+ `rnaturalearthdata`), `scales`, `R.utils`,
`units`. Fonts (Roboto, Roboto Condensed; SIL Open Font Licence) are bundled in
`fonts/`; the GOOS logo is `GOOS_Main_logo_-_white.png`.

## Data

Not tracked in git (`.gitignore`); the scripts download or rebuild them, except ETOPO1:

- Argo Global Profile Index (Coriolis GDAC, `ar_index_global_prof.txt.gz`) -
  downloaded by script 01 into `data/` if missing (updated daily upstream).
- ETOPO1 Ice Surface bathymetry, GeoTIFF (NOAA NCEI) - place it at
  `data/bathymetry/ETOPO1_Ice_bathymetry_geotiff.tif`.
- NOAA/NSIDC Sea Ice Concentration CDR v6 (G02202 v6), daily 25 km - downloaded by
  script 02 into `output/gis/seaice_daily_raw_{south,north}/`.
- CartoDB Dark Matter tiles and Natural Earth coastlines - downloaded and cached in
  `output/gis/` on first run.

Credits:
Data: Argo Global Profile Index (Coriolis), IMOS Australian Ocean Data Network and NOAA/NSIDC; 
Code and visualisation: Fabrice Jaine (Integrated Marine Observing
System, IMOS). 
Polar Argo floats are deployed as part of the international OneArgo
program (https://argo.ucsd.edu/oneargo/).
