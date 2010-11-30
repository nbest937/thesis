#!/bin/bash

set -x

# quell gdal_translate complaints on my Mac

export GISBASE=/Applications/GRASS-6.4.app/Contents/MacOS


# have to use gdal_translate because r.out.gdal in GRASS does not assign an SRS


# Ramankutty's Aglands2000 data

AGLANDS='cropland pasture'

for map in $AGLANDS; do
    gdal_translate -of Rasterlite -a_srs EPSG:4326 \
        ~/mnt/see/grass/global/cusa/cellhd/$map \
        RASTERLITE:cusa.sqlite,table=ag_$map -co WIPE=YES
done

# ~/mnt/see/grass/global/aglands/cellhd/$map \  # complete data set


# aggregated MLCT according to A_s = 0.0 or 0.5

# crop and open only
MLCT=$( find -E ~/mnt/see/grass/global/cusa/cellhd \
    -regex '^.*/2001_(crop|open)_As0[05]_5min$' \
    | sort)

# all covers but mosaic
MLCT=$( find -E ~/mnt/see/grass/global/cusa/cellhd \
    -regex '^.*/2001_[^m].*_As0[05]_5min$' \
    | sort)

MLCT=$( find -E ~/mnt/see/grass/global/cusa/cellhd \
    -regex '^.*/2001_mosaic_As0[05]_5min$' \
    | sort)

for map in $MLCT; do
    new=$(echo $map \
        | perl -ne '/(2001_\w+_As0[05])/; print "mlct_$1"')
    gdal_translate -of Rasterlite -a_srs EPSG:4326 $map \
        RASTERLITE:cusa.sqlite,table=$new -co WIPE=YES
done


# aggregated NLCD 2001

NLCD=$( find -E ~/mnt/see/grass/global/nlcd/cellhd \
    -regex '^.*/nlcd_[a-z]+_5min$' \
    -not -name 'nlcd_other_5min' \
    -not -name 'nlcd_pasture_5min'
    | sort)

for map in $NLCD; do
    gdal_translate -of Rasterlite -a_srs EPSG:4326 $map \
        RASTERLITE:cusa.sqlite,table=$( basename $map _5min) -co WIPE=YES
done

# our "Aglands-Complete" hybrid of Aglands2000 and MLCT(A_s = 0.5)

AGC=$( find -E ~/mnt/see/grass/global/cusa/cellhd \
    -regex '^.*/agc_[a-z]+$' \
    -not -name 'agc_total' \
    | sort)

for map in $AGC; do
    gdal_translate -of Rasterlite -a_srs EPSG:4326 $map \
        RASTERLITE:cusa.sqlite,table=$( basename $map) -co WIPE=YES
done


