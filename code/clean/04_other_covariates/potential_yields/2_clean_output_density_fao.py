import sys

import os
import geopandas as gpd
import rasterio
from rasterstats import zonal_stats
from pathlib import Path

project_root = Path(__file__).resolve().parents[4]
raw_root = project_root / "data" / "raw"
build_root = project_root / "data" / "intermediate" / ".build" / "optional_covariates"
output_path = build_root / "fao" / "output_density_6190.csv"
output_path.parent.mkdir(parents=True, exist_ok=True)
county_path = raw_root / "geography" / "shapefiles" / "2000" / "US_County_Boundaries.shp"
yield_path = raw_root / "optional_covariates" / "fao" / "raw" / "CRUTS32" / "Hist" / "6190"

#####################################
######### load counties ###########
#####################################

counties_shp = gpd.read_file(county_path)


#####################################
######### Get file names ###########
#####################################

hist = os.listdir(yield_path)
hist = [x for x in hist if "Hr0" not in x]

#####################################
######### Zonal stat ###########
#####################################

for lc in hist:
    print(lc, flush = True)
    avg = zonal_stats(
        county_path,
        yield_path / lc,
            stats="mean"
    )
    counties_shp["output_density_" + lc] = avg


counties_shp.to_csv(output_path, index = False)
