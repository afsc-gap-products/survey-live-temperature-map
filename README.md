<!-- README.md is generated from README.Rmd. Please edit that file -->

# [Near Real-Time Survey Progress and Temperature Maps](https://github.com/afsc-gap-products/survey-live-temperature-map) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

*This code is always in development. Find code used for final products
of this code in
[releases](paste0(https://github.com/afsc-gap-products/survey-live-temperature-map,%20%22/releases%22)).*

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

**Chris Anderson** (Christopher.Anderson AT noaa.gov;
[@ChrisAnderson-NOAA](https://github.com/ChrisAnderson-NOAA))

And previously,

**Caitlin Allen Akselrud** (caitlin.allen_akselrud AT noaa.gov;
[@CaitlinAkselrud-NOAA](https://github.com/CaitlinAkselrud-NOAA))

**Liz Dawson** (Liz.Dawson AT noaa.gov;
[@liz-dawson-NOAA](https://github.com/liz-dawson-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98115

# Table of contents

> - [*Purpose*](#purpose)
> - [*Notes*](#notes)
> - [*Plot Examples*](#plot-examples)
>   - [*Final stacked gifs*](#final-stacked-gifs)
>   - [*Blank, Grid-only Plot*](#blank,-grid-only-plot)
>   - [*Mean Plot*](#mean-plot)
>   - [*Anomaly Plot*](#anomaly-plot)
> - [*Relevant publications*](#relevant-publications)
> - [*Suggestions and Comments*](#suggestions-and-comments)
>   - [*R Version Metadata*](#r-version-metadata)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

# Purpose

These scripts create daily survey station daily temperature and anomaly
plots as the ships work their way through the Bering Sea. These ships
are conducting NOAA Fisheries’ Alaska Fisheries Science Center’s
fisheries independent surveys in the Eastern Bering Sea. Scripts pull
temperatures from google drive, entered by FPCs at sea, create daily
maps and composite gifs, and then push the maps to google drive for the
communications team. These plots are displayed on the AFSC website

- [**Progress and temperature maps landing
  page**](https://www.fisheries.noaa.gov/alaska/science-data/bottom-trawl-survey-temperature-and-progress-maps)
- [2024 Eastern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/science-data/near-real-time-temperatures-eastern-bering-sea-bottom-trawl-survey-2024)
- [2024 Aleutian Islands Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/climate/near-real-time-temperatures-aleutian-islands-bottom-trawl-survey-2024)
- [2023 Eastern and Northern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/science-data/near-real-time-temperatures-bering-sea-bottom-trawl-survey-2023)
- [2023 Gulf of Alaska Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/science-data/near-real-time-temperatures-gulf-alaska-bottom-trawl-survey-2023)
- [2023 Temperature Anomalies and Cold Pool Estimates from Bering Sea
  Bottom Trawl
  Surveys](https://www.fisheries.noaa.gov/alaska/science-data/temperature-anomalies-and-cold-pool-estimates-bering-sea-bottom-trawl-surveys-2023)
- [2022 Eastern and Northern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/climate/near-real-time-temperatures-bering-sea-bottom-trawl-surveys-2022)
- [2022 Aleutian Islands Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/climate/near-real-time-temperatures-aleutian-islands-bottom-trawl-survey-2022)
- [2021 Eastern and Northern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/alaska/science-data/near-real-time-temperatures-bering-sea-bottom-trawl-survey)
- [2019 Eastern and Northern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/feature-story/2019-southeastern-bering-sea-shelf-bottom-trawl-survey-gets-underway)
- [2018 Eastern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/resource/document/2018-eastern-bering-sea-continental-shelf-and-northern-bering-sea-trawl-surveys)
- [2017 Eastern and Northern Bering Sea Bottom Trawl
  Survey](https://www.fisheries.noaa.gov/resource/document/2017-eastern-bering-sea-continental-shelf-and-northern-bering-sea-bottom-trawl)

# Notes

- [How to set up the task
  scheduler](https://docs.google.com/document/d/1pwBmR6AqgnvUx_AiWYQxtYxIRjWMfdd5EPWwFvpI3Ug/edit)
- Files are saved to our internal dev FTP server and google drive.
- Troubleshooting: if the task scheduler fails to run the code, but you
  can run the script in R or Rstudio, you may need to update Pandoc. If
  you are on a NOAA machine, ask IT to install the .msi file for you.
  Close and reopen everything and try again.

# Plot Examples

Find more plot examples
[here](https://github.com/afsc-gap-products/survey-live-temperature-map/tree/main/test).

## Final stacked gifs

![NOAA Fisheries AFSC Groundfish Assessment Program conducted the
Aleutian Islands bottom trawl survey. The near real-time ocean bottom
temperatures depicted were collected June 7-June 21 On June 21, stations
237-36 (52.52°N, -172.26°W; \>3.5–4°C), 236-36 (52.52°N, -172.34°W;
\>4–4.5°C), and 238-37 (52.57°N, -172.18°W; \>3.5–4°C) were surveyed by
the F/V Ocean Explorer. No stations were surveyed by the F/V Alaska
Provider. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_ai.gif)
, ![NOAA Fisheries AFSC Groundfish Assessment Program conducted the Gulf
of Alaska bottom trawl survey. The near real-time ocean bottom
temperatures depicted were collected June 12-June 13 On June 13,
stations 56-49 (54.33°N, -165.37°W; ≤3°C) and 25-18 (53.05°N, -167.95°W;
≤3°C) were surveyed by the F/V Alaska Provider and stations 50-33
(53.66°N, -165.88°W; \>3–4°C) and 52-40 (53.95°N, -165.71°W; \>4–5°C)
were surveyed by the F/V Ocean Explorer. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_goa.gif)
, ![NOAA Fisheries AFSC Groundfish Assessment Program conducted the
eastern Bering Sea and northern Bering Sea bottom trawl surveys. The
near real-time ocean bottom temperatures depicted were collected May
29-August 20 This is the last day of the survey. On August 20, stations
R-18 (60.67°N, -168.69°W; \>8°C), R-01 (60.67°N, -168.01°W; \>8°C), and
R-02 (60.67°N, -167.32°W; \>8°C) were surveyed by the F/V Alaska Knight.
No stations were surveyed by the F/V Vesteraalen. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_bs.gif)
, ![NOAA Fisheries AFSC Groundfish Assessment Program conducted the
eastern Bering Sea bottom trawl survey. The near real-time ocean bottom
temperatures depicted were collected June 2-June 21 On June 21, stations
K-04 (58.33°N, -165.88°W; \>2–3°C), L-04 (58.67°N, -165.89°W; \>2–3°C),
and M-04 (59°N, -165.9°W; \>3–4°C) were surveyed by the F/V Alaska
Knight and stations K-05 (58.33°N, -165.24°W; \>2–3°C), K-06 (58.33°N,
-164.61°W; \>2–3°C), and L-05 (58.67°N, -165.25°W; \>2–3°C) were
surveyed by the F/V Northwest Explorer. There are 8 stations planned for
June 22 Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_ebs.gif)

## Blank, Grid-only Plot

![The Aleutian Islands bottom trawl survey. This survey covers the
Central Aleutians, Eastern Aleutians, Southern Bering Sea, and Western
Aleutians districts. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_ai.png)
, ![The Gulf of Alaska bottom trawl survey. This survey covers the
Shumagin, Chirikof, Kodiak, Yakutat, and Southeastern regions. Credit:
NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_goa.png)
, ![The grid of designated stations in the eastern Bering Sea and
northern Bering Sea bottom trawl survey areas as well as the 50m, 100m,
and 200m bathymetric boundaries. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_bs.png)
, ![Map of the eastern Bering Sea 2024 survey station grid and the 50 m,
100 m, and 200 m bathymetry lines. Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_ebs.png)

## Mean Plot

![The timeseries mean bottom temperatures from the NOAA Fisheries
eastern Bering Sea (1982-2021; 39 years) and northern Bering Sea
(2010-2021; 4 years) bottom trawl surveys. These data are publicly
accessible on Fisheries One Stop Shop data platform
(<https://www.fisheries.noaa.gov/foss>). Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_mean_bs.png)
, ![The time series mean bottom temperatures from the NOAA Fisheries
eastern Bering Sea (1982-2023; 41 years) bottom trawl survey. These data
are publicly accessible on Fisheries One Stop Shop data platform
(<https://www.fisheries.noaa.gov/foss>) and the coldpool R package
(<https://github.com/afsc-gap-products/coldpool>). Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_mean_ebs.png)
, ![The time series mean bottom temperatures from the NOAA Fisheries
eastern Bering Sea (1982-2023; 41 years) bottom trawl survey. These data
are publicly accessible on Fisheries One Stop Shop data platform
(<https://www.fisheries.noaa.gov/foss>) and the coldpool R package
(<https://github.com/afsc-gap-products/coldpool>). Credit: NOAA
Fisheries](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_mean_cb_ebs.png)

## Anomaly Plot

<figure>
<img
src="https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_anom_bs.png"
alt="The 2022 near real-time ocean bottom temperature anomaly in the NOAA Fisheries AFSC Groundfish Assessment Program’s eastern Bering Sea and northern Bering Sea bottom trawl surveys. The timeseries mean bottom temperatures from the eastern Bering Sea (1982-2021; 39 years) and northern Bering Sea (2010-2021; 4 years) bottom trawl surveys are compared to their respective 2022 surveys (May 30-August 20). These data are publicly accessible on Fisheries One Stop Shop data platform (https://www.fisheries.noaa.gov/foss). Credit: NOAA Fisheries" />
<figcaption aria-hidden="true">The 2022 near real-time ocean bottom
temperature anomaly in the NOAA Fisheries AFSC Groundfish Assessment
Program’s eastern Bering Sea and northern Bering Sea bottom trawl
surveys. The timeseries mean bottom temperatures from the eastern Bering
Sea (1982-2021; 39 years) and northern Bering Sea (2010-2021; 4 years)
bottom trawl surveys are compared to their respective 2022 surveys (May
30-August 20). These data are publicly accessible on Fisheries One Stop
Shop data platform (<a href="https://www.fisheries.noaa.gov/foss"
class="uri">https://www.fisheries.noaa.gov/foss</a>). Credit: NOAA
Fisheries</figcaption>
</figure>

# Relevant publications

**Learn more about these surveys and ocean temperatures around Alaska**
(Hoff, 2016; Markowitz et al., 2023, 2023; Rohan et al., 2022; Von
Szalay et al., 2023; Von Szalay and Raring, 2018)

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-RN979" class="csl-entry">

Hoff, G. R. (2016). *Results of the 2016 eastern Bering Sea upper
continental slope survey of groundfishes and invertebrate resources*
(NOAA Tech. Memo. NOAA-AFSC-339). U.S. Dep. Commer.
<https://doi.org/10.7289/V5/TM-AFSC-339>

</div>

<div id="ref-2022NEBS2023" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Anderson, A. B., Rohan, S. K.,
Charriere, N. E., Prohaska, B. K., and Stevenson, D. E. (2023). *Results
of the 2022 eastern and northern Bering Sea continental shelf bottom
trawl survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-469; p. 213). U.S. Dep. Commer.
<https://doi.org/10.25923/rt50-th19>

</div>

<div id="ref-RohanColdPool" class="csl-entry">

Rohan, S., Barnett, L., and Charriere, N. (2022). *Evaluating approaches
to estimating mean temperatures and cold pool area from AFSC bottom
trawl surveys of the eastern Bering Sea* (NOAA Tech. Memo.
NMFS-AFSC-456; p. 42). U.S. Dep. Commer.
<https://doi.org/10.25923/1wwh-q418>

</div>

<div id="ref-GOA2018" class="csl-entry">

Von Szalay, P. G., and Raring, N. W. (2018). *Data report: 2017
<span class="nocase">Gulf of Alaska</span> bottom trawl survey* (NOAA
Tech. Memo. NMFS-AFSC-374). U.S. Dep. Commer.
<https://doi.org/10.7289/V5/TM-AFSC-374>

</div>

<div id="ref-AI2022" class="csl-entry">

Von Szalay, P. G., Raring, N. W., Siple, M. C., Dowlin, A. N., Riggle,
B. C., and Laman, E. A. and. (2023). *Data report: 2022 Aleutian Islands
bottom trawl survey* (AFSC Processed Rep. 2023-07; p. 230). U.S. Dep.
Commer. <https://doi.org/10.25923/85cy-g225>

</div>

</div>

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/afsc-gap-products/survey-live-temperature-map/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/afsc-gap-products/survey-live-temperature-map/issues).

## R Version Metadata

    FALSE R version 4.4.0 (2024-04-24 ucrt)
    FALSE Platform: x86_64-w64-mingw32/x64
    FALSE Running under: Windows 10 x64 (build 19045)
    FALSE 
    FALSE Matrix products: default
    FALSE 
    FALSE 
    FALSE locale:
    FALSE [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
    FALSE [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    FALSE 
    FALSE time zone: America/Los_Angeles
    FALSE tzcode source: internal
    FALSE 
    FALSE attached base packages:
    FALSE [1] stats     graphics  grDevices utils     datasets  methods   base     
    FALSE 
    FALSE other attached packages:
    FALSE  [1] RCurl_1.98-1.14   RODBC_1.3-23      tinytex_0.51      quarto_1.4        readxl_1.4.3      here_1.0.1        stringr_1.5.1     googledrive_2.1.1
    FALSE  [9] tidyr_1.3.1       rmarkdown_2.27    readr_2.1.5       viridis_0.6.5     viridisLite_0.4.2 janitor_2.2.0     qpdf_1.3.3        magick_2.8.3     
    FALSE [17] cowplot_1.1.3     ggspatial_1.1.9   akgfmaps_3.5.3    terra_1.7-78      stars_0.6-5       abind_1.4-5       sf_1.0-16         gstat_2.1-1      
    FALSE [25] ggplot2_3.5.1     classInt_0.4-10   devtools_2.4.5    usethis_2.2.3     glue_1.7.0        dplyr_1.1.4       magrittr_2.0.3   
    FALSE 
    FALSE loaded via a namespace (and not attached):
    FALSE  [1] DBI_1.2.3          bitops_1.0-7       gridExtra_2.3      remotes_2.5.0      rlang_1.1.4        snakecase_0.11.1   e1071_1.7-14      
    FALSE  [8] compiler_4.4.0     vctrs_0.6.5        profvis_0.3.8      crayon_1.5.2       pkgconfig_2.0.3    fastmap_1.2.0      ellipsis_0.3.2    
    FALSE [15] utf8_1.2.4         promises_1.3.0     sessioninfo_1.2.2  tzdb_0.4.0         ps_1.7.6           bit_4.0.5          purrr_1.0.2       
    FALSE [22] xfun_0.44          cachem_1.1.0       jsonlite_1.8.8     later_1.3.2        parallel_4.4.0     R6_2.5.1           stringi_1.8.4     
    FALSE [29] pkgload_1.3.4      cellranger_1.1.0   lubridate_1.9.3    Rcpp_1.0.12        knitr_1.47         zoo_1.8-12         readtext_0.91     
    FALSE [36] FNN_1.1.4          timechange_0.3.0   httpuv_1.6.15      tidyselect_1.2.1   rstudioapi_0.16.0  yaml_2.3.8         codetools_0.2-20  
    FALSE [43] miniUI_0.1.1.1     processx_3.8.4     curl_5.2.1         pkgbuild_1.4.4     lattice_0.22-6     tibble_3.2.1       intervals_0.15.4  
    FALSE [50] shiny_1.8.1.1      withr_3.0.0        askpass_1.2.0      evaluate_0.24.0    units_0.8-5        proxy_0.4-27       urlchecker_1.0.1  
    FALSE [57] xts_0.14.0         pillar_1.9.0       KernSmooth_2.23-22 generics_0.1.3     vroom_1.6.5        rprojroot_2.0.4    sp_2.1-4          
    FALSE [64] spacetime_1.3-1    hms_1.1.3          munsell_0.5.1      scales_1.3.0       xtable_1.8-4       class_7.3-22       tools_4.4.0       
    FALSE [71] data.table_1.15.4  fs_1.6.4           grid_4.4.0         colorspace_2.1-0   cli_3.6.2          rappdirs_0.3.3     fansi_1.0.6       
    FALSE [78] gargle_1.5.2       gtable_0.3.5       digest_0.6.35      htmlwidgets_1.6.4  memoise_2.0.1      htmltools_0.5.8.1  lifecycle_1.0.4   
    FALSE [85] httr_1.4.7         mime_0.12          bit64_4.0.5        openssl_2.2.0

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
