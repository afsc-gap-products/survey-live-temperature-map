# GAP Survey Temperature Maps

<!-- badges: start -->
<!-- badges: end -->

> Code is still in development. 

## Developers

**Emily Markowitz** (Emily.Markowitz AT noaa.gov)

Research Fisheries Biologist

**Liz Dawson** (Liz.Dawson AT noaa.gov)

Fisheries Biologist

**Caitlin Allen Akselrud** (caitlin.allen_akselrud AT noaa.gov)

Research Fisheries Biologist


Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195


## Use

These scripts create daily survey station daily temperature and anomaly plots as the ships work their way through the Bering Sea. These ships are conducting NOAA Fisheries' Alaska Fisheries Science Center's fisheries independent surveys in the Eastern Bering Sea. Scripts pull temperatures from google drive, entered by FPCs at sea, create daily maps and composite gifs, and then push the maps to google drive for the communications team. These plots are displayed on the AFSC website: https://www.fisheries.noaa.gov/alaska/science-data/near-real-time-temperatures-bering-sea-bottom-trawl-survey

How to set up the task scheduler:  https://docs.google.com/document/d/1pwBmR6AqgnvUx_AiWYQxtYxIRjWMfdd5EPWwFvpI3Ug/edit

Where the files will be saved to:  https://drive.google.com/drive/u/2/folders/1BSMOHWQO_oWxF6AmOFI6sudiSbFLvToq


## Final combined gif for the NEBS survey

This was created using [this free online gif maker](https://gifmaker.me/)

![NEBS 2021 Survey](./test/final_2021_nebs_daily_400ms.gif)

## Example files

> Note: Values are made up. 


### Blank, Grid-only Plot

![Daily Temperatrues](./test/_grid.png)

### Daily Plot

![Daily Temperatrues](./test/2021-06-04_daily.png)


### Anomaly Plot

![Anomaly Temperatrues](./test/2021-06-04_anom.png)


### GIF of maps as they progressed over time

![GIF of Daily Temperatrues](./test/2021-06-04_daily.gif)

