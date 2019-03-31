# Fire forecasting for South East National Forest in Australia

Basically we are going to forecast the _active fire_ by 8-Day, using two fire products, precipitation and air temperature data.


### Datasets:
- [Temperature data from reanalysis product (at daily scale resolution)](https://www.esrl.noaa.gov/psd/forecasts/reforecast2/download.html)

- [Precipitation data from reanalysis product (at daily resolution)](https://www.esrl.noaa.gov/psd/forecasts/reforecast2/download.html)
- [MODIS EVI product (at 16-Day resolution)](https://e4ftl01.cr.usgs.gov/MOLT/MOD13A2.006/)
- [MODIS/Terra Thermal Anomalies & Fire data product (at 8-Day resolution)](https://e4ftl01.cr.usgs.gov/MOLT/MOD14A2.006/)
- [VIIRS I Band 750 m Active Fire Product NRT (at 8-Day resolution)](https://e4ftl01.cr.usgs.gov/VIIRS/VNP14A1.001/)

### Workflow:

1. Run 01.x codes in sequence, get target data downloaded to the group's GEO shared directory.
 - the starting date is Feb 2nd, 2019
 - the study area is H29 V12 in MODIS tile
 - data visualization for GEFs data will be done in R; for VIIRS and MODIS data will be done with qgis or bash

## Contact Information
- Vivien Chen: [vchen2@bu.edu]
- Yingtong Zhang: [zhangyt@bu.edu]
- Qianning Qin: [qnqin@bu.edu]
- Tess McCabe: [tmccabe@bu.edu]
- Shijuan Chen: [shijuan@bu.edu]
