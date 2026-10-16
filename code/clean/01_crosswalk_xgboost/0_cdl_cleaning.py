import sys
year =  int(sys.argv[1]) + 2008
print(year, flush = True)


import numpy as np
import rasterio
from rasterio.warp import Resampling
from pathlib import Path

project_root = Path(__file__).resolve().parents[3]
raw_cdl = project_root / "data" / "raw" / "cropland_data_layer"
build_cdl = project_root / "data" / "intermediate" / ".build" / "crosswalk" / "cdl"
build_cdl.mkdir(parents=True, exist_ok=True)

############################
######### Global ###########
############################

crs_meters = 'EPSG:3310' #choose projection
crs_lat_lon = 'EPSG:4326'

dowscale_factor = 1/7 #results in squares of ~210m x 210m ~ 10 acres


#####################################
######### load land cover ###########
#####################################

land_cover = list(range(1, 15)) + \
    list(range(21, 61)) + \
    list(range(66, 78)) + \
    list(range(204, 255))

print("Aggregating...", flush=True)

####################################
## Aggregate land cover classes ####
####################################
source_raster = raw_cdl / f"{year}_30m_cdls" / f"{year}_30m_cdls.tif"
with rasterio.open(source_raster, 'r') as src:
    out_meta = src.meta.copy()
    print(out_meta, flush = True)
    with rasterio.open(build_cdl / f"{year}_30m_cdls_agg_for_prediction.tif", 'w', **out_meta) as dst:
        band = src.read(1)
        new_band = np.zeros(band.shape, dtype='uint8')
        # aggregate 
        new_band = np.where(np.isin(band, land_cover), 1, new_band)#.astype("uint8")
        dst.write(new_band, 1)

print("   Done", flush=True)



print("Resampling...", flush=True)

##################################
############ Resample ############
##################################
with rasterio.open(build_cdl / f"{year}_30m_cdls_agg_for_prediction.tif", 'r') as src:
    # resample data to target shape
    data = src.read(
        out_shape=(
            src.count,
            int(src.height * dowscale_factor),
            int(src.width * dowscale_factor)
        ),
        resampling=Resampling.mode # resample by choosing the mode
    )

    # scale image transform
    transform = src.transform * src.transform.scale(
        (src.width / data.shape[-1]),
        (src.height / data.shape[-2])
    )

    out_meta = src.meta.copy()
    out_meta.update({
        'width':data.shape[-1],
        'height':data.shape[-2],
        'transform':transform
    })

    # write 
    with rasterio.open(build_cdl / f"{year}_210m_cdls_agg_for_prediction.tif", 'w', **out_meta) as dst:
        dst.write(data)

# remove src 
#os.remove(os.path.join(path, "cdl/" + str(year) + "_30m_cdls_agg.tif"))
print("   Done", flush=True)
