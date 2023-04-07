<!-- README.md is generated from README.Rmd. Please edit that file -->

# [Near Real-Time Survey Progress and Temperature Maps](https://github.com/afsc-gap-products/survey-live-temperature-map) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

*This code is always in development. Find code used for final products
of this code in
[releases](paste0(https://github.com/afsc-gap-products/survey-live-temperature-map,%20%22/releases%22)).*

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

**Liz Dawson** (Liz.Dawson AT noaa.gov;
[@liz-dawson-NOAA](https://github.com/liz-dawson-NOAA))

**Chris Anderson** (Christopher.Anderson AT noaa.gov;
[@ChrisAnderson-NOAA](https://github.com/ChrisAnderson-NOAA))

And previously, **Caitlin Allen Akselrud** (caitlin.allen_akselrud AT
noaa.gov;
[@CaitlinAkselrud-NOAA](https://github.com/CaitlinAkselrud-NOAA))

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
eastern Bering Sea and northern Bering Sea bottom trawl surveys. The
near real-time ocean bottom temperatures depicted were collected May
29-August 20 This is the last day of the survey. On August 20, stations
R-18 (60.67°N, -168.69°W; \>8°C), R-01 (60.67°N, -168.01°W; \>8°C), and
R-02 (60.67°N, -167.32°W; \>8°C) were surveyed by the F/V Alaska Knight.
No stations were surveyed by the F/V Vesteraalen. Credit: NOAA
Fisheries](./examples/current_daily_bs.gif) , ![NOAA Fisheries AFSC
Groundfish Assessment Program conducted the Aleutian Islands bottom
trawl survey. The near real-time ocean bottom temperatures depicted were
collected June 10-August 13 On August 13, stations 162-16 (51.65°N,
-177.89°W; \>5.5–6°C) and 184-16 (51.66°N, -176.25°W; \>5–5.5°C) were
surveyed by the F/V Alaska Provider. No stations were surveyed by the
F/V Ocean Explorer. Allocated stations that have not yet been sampled
are shown as gray dots. Credit: NOAA
Fisheries](./examples/current_daily_ai.gif)

## Blank, Grid-only Plot

![The grid of designated stations in the eastern Bering Sea and northern
Bering Sea bottom trawl survey areas as well as the 50m, 100m, and 200m
bathymetric boundaries. Credit: NOAA
Fisheries](./examples/current_grid_bs.png) , ![The Gulf of Alaska bottom
trawl survey. This survey covers the Central Aleutians, Eastern
Aleutians, Southern Bering Sea, and Western Aleutians regions. Credit:
NOAA Fisheries](./examples/current_grid_ai.png)

## Mean Plot

![The timeseries mean bottom temperatures from the NOAA Fisheries
eastern Bering Sea (1982-2021; 39 years) and northern Bering Sea
(2010-2021; 4 years) bottom trawl surveys. These data are publicly
accessible on Fisheries One Stop Shop data platform
(<https://www.fisheries.noaa.gov/foss>). Credit: NOAA
Fisheries](./examples/current_mean_bs.png)

## Anomaly Plot

![The 2022 near real-time ocean bottom temperature anomaly in the NOAA
Fisheries AFSC Groundfish Assessment Program’s eastern Bering Sea and
northern Bering Sea bottom trawl surveys. The timeseries mean bottom
temperatures from the eastern Bering Sea (1982-2021; 39 years) and
northern Bering Sea (2010-2021; 4 years) bottom trawl surveys are
compared to their respective 2022 surveys (May 30-August 20). These data
are publicly accessible on Fisheries One Stop Shop data platform
(<https://www.fisheries.noaa.gov/foss>). Credit: NOAA
Fisheries](./examples/current_anom_bs.png)

# Relevant publications

**Learn more about these surveys and ocean temperatures around Alaska**
(Markowitz, Dawson, Charriere, Prohaska, Rohan, Stevenson, et al.,
2022b, 2022a, In review; Markowitz, Dawson, Charriere, Prohaska, Rohan,
Haehn, et al., 2022; Rohan et al., 2022; Von Szalay and Raring, 2018,
2020)

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-2018EBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Haehn, R. A., Stevenson, D. E., and Britt, L. L. (2022).
*Results of the 2018 eastern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-F/SPO-450; p. 183). U.S. Dep. Commer.
<https://doi.org/10.25923/m4pw-t510>

</div>

<div id="ref-2019NEBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Stevenson, D. E., and Britt, L. L. (2022a). *Results of
the 2019 eastern and northern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-F/SPO-451; p. 225). U.S. Dep. Commer.
<https://doi.org/10.25923/d641-xb21>

</div>

<div id="ref-2021NEBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Stevenson, D. E., and Britt, L. L. (2022b). *Results of
the 2021 eastern and northern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-F/SPO-452; p. 227). U.S. Dep. Commer.
<https://doi.org/10.25923/g1ny-y360>

</div>

<div id="ref-2022NEBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Stevenson, D. E., and Britt, L. L. (In review). *Results
of the 2022 eastern and northern Bering Sea continental shelf bottom
trawl survey of groundfish and invertebrate fauna* \[NOAA Tech. Memo.\].
U.S. Dep. Commer.

</div>

<div id="ref-RohanColdPool" class="csl-entry">

Rohan, S., Barnett, L., and Charriere, N. (2022). *Evaluating approaches
to estimating mean temperatures and cold pool area from AFSC bottom
trawl surveys of the eastern Bering Sea* (NOAA Tech. Memo.
NMFS-AFSC-456; p. 42). U.S. Dep. Commer.
<https://doi.org/10.25923/1wwh-q418>

</div>

<div id="ref-GOA2018" class="csl-entry">

Von Szalay, P. G., and Raring, N. W. (2018). *Data report: 2017 gulf of
alaska bottom trawl survey* (NOAA Tech. Memo. NMFS-AFSC-374). U.S. Dep.
Commer.
<https://apps-afsc.fisheries.noaa.gov/Publications/AFSC-TM/NOAA-TM-AFSC-374.pdf>

</div>

<div id="ref-AI2018" class="csl-entry">

Von Szalay, P. G., and Raring, N. W. (2020). *Data report: 2018 aleutian
islands bottom trawl survey* (NOAA Tech. Memo. NMFS-AFSC-409). U.S. Dep.
Commer. <https://repository.library.noaa.gov/view/noaa/26367>

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

    FALSE R version 4.2.3 (2023-03-15 ucrt)
    FALSE Platform: x86_64-w64-mingw32/x64 (64-bit)
    FALSE Running under: Windows 10 x64 (build 19045)
    FALSE 
    FALSE Matrix products: default
    FALSE 
    FALSE locale:
    FALSE [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
    FALSE [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
    FALSE [5] LC_TIME=English_United States.utf8    
    FALSE 
    FALSE attached base packages:
    FALSE [1] stats     graphics  grDevices utils     datasets  methods   base     
    FALSE 
    FALSE other attached packages:
    FALSE [1] glue_1.6.2     dplyr_1.1.1    magrittr_2.0.3
    FALSE 
    FALSE loaded via a namespace (and not attached):
    FALSE  [1] rstudioapi_0.14   knitr_1.42        tidyselect_1.2.0  R6_2.5.1          rlang_1.1.0       fastmap_1.1.1    
    FALSE  [7] fansi_1.0.4       httr_1.4.5        tools_4.2.3       data.table_1.14.8 xfun_0.38         utf8_1.2.3       
    FALSE [13] cli_3.6.1         htmltools_0.5.5   yaml_2.3.7        digest_0.6.31     tibble_3.2.1      gargle_1.3.0     
    FALSE [19] lifecycle_1.0.3   purrr_1.0.1       vctrs_0.6.1       fs_1.6.1          evaluate_0.20     rmarkdown_2.20   
    FALSE [25] stringi_1.7.12    compiler_4.2.3    pillar_1.9.0      generics_0.1.3    readtext_0.81     googledrive_2.1.0
    FALSE [31] pkgconfig_2.0.3

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
