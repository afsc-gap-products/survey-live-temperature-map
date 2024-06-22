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

![*NOAA* *Fisheries* *AFSC* *Groundfish* *Assessment* *Program*
*conducted* *the* *Aleutian* *Islands* *bottom* *trawl* *survey.* *The*
*near* *real-time* *ocean* *bottom* *temperatures* *depicted* *were*
*collected* *June* *7-June* *21* *On* *June* *21,* *stations* *237-36*
*(52.52°N,* *-172.26°W;* *\>3.5–4°C),* *236-36* *(52.52°N,* *-172.34°W;*
*\>4–4.5°C),* *and* *238-37* *(52.57°N,* *-172.18°W;* *\>3.5–4°C)*
*were* *surveyed* *by* *the* *F/V* *Ocean* *Explorer.* *No* *stations*
*were* *surveyed* *by* *the* *F/V* *Alaska* *Provider.* *Credit:* *NOAA*
*Fisheries*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_ai.gif)
, ![*NOAA* *Fisheries* *scientists* *are* *providing* *information* *on*
*ocean* *temperatures* *observed* *during* *the* *Gulf* *of* *Alaska*
*Bottom* *Trawl* *Survey* *in* *2023* *Credit:* *NOAA*
*Fisheries.*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_goa.gif)
, ![*Map* *of* *the* *ocean* *bottom* *temperatures* *from* *the* *2023*
*eastern* *and* *northern* *Bering* *Sea* *bottom* *trawl* *surveys*
*aboard* *the* *F/V* *Northwest* *Explorer* *and* *F/V* *Alaska*
*Knight.* *Updated* *colors:* *This* *year* *in* *an* *effort* *to*
*make* *the* *heatmaps* *more* *accessible* *to* *a* *broader*
*audience* *a* *new* *color* *scheme* *was* *used* *for* *our*
*heatmaps.* *However,* *given* *feedback* *we* *have* *received* *we*
*are* *reverting* *back* *to* *the* *colors* *used* *in* *previous*
*years.* *Credit:* *NOAA*
*Fisheries*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_bs.gif)
, ![*NOAA* *Fisheries* *AFSC* *Groundfish* *Assessment* *Program*
*conducted* *the* *eastern* *Bering* *Sea* *bottom* *trawl* *survey.*
*The* *near* *real-time* *ocean* *bottom* *temperatures* *depicted*
*were* *collected* *June* *2-June* *21* *On* *June* *21,* *stations*
*K-04* *(58.33°N,* *-165.88°W;* *\>2–3°C),* *L-04* *(58.67°N,*
*-165.89°W;* *\>2–3°C),* *and* *M-04* *(59°N,* *-165.9°W;* *\>3–4°C)*
*were* *surveyed* *by* *the* *F/V* *Alaska* *Knight* *and* *stations*
*K-05* *(58.33°N,* *-165.24°W;* *\>2–3°C),* *K-06* *(58.33°N,*
*-164.61°W;* *\>2–3°C),* *and* *L-05* *(58.67°N,* *-165.25°W;*
*\>2–3°C)* *were* *surveyed* *by* *the* *F/V* *Northwest* *Explorer.*
*There* *are* *8* *stations* *planned* *for* *June* *22* *Credit:*
*NOAA*
*Fisheries*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_daily_ebs.gif)

## Blank, Grid-only Plot

![*The* *Aleutian* *Islands* *bottom* *trawl* *survey.* *This* *survey*
*covers* *the* *Central* *Aleutians,* *Eastern* *Aleutians,* *Southern*
*Bering* *Sea,* *and* *Western* *Aleutians* *districts.* *Credit:*
*NOAA*
*Fisheries*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_ai.png)
, ![*The* *Gulf* *of* *Alaska* *bottom* *trawl* *survey* *region.*
*This* *survey* *covers* *the* *Shumagin,* *Chirikof,* *Kodiak,*
*Yakutat,* *and* *Southeastern* *subregions.* *Credit:* *NOAA*
*Fisheries.*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_goa.png)
, ![*Map* *of* *the* *eastern* *Bering* *Sea* *and* *northern* *Bering*
*Sea* *2023* *survey* *stations* *with* *the* *50m,* *100m,* *and*
*200m* *bathymetry* *lines.* *Credit:* *NOAA*
*Fisheries.*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_bs.png)
, ![*Map* *of* *the* *eastern* *Bering* *Sea* *2024* *survey* *station*
*grid* *and* *the* *50* *m,* *100* *m,* *and* *200* *m* *bathymetry*
*lines.* *Credit:* *NOAA*
*Fisheries*](https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_grid_ebs.png)

## Mean Plot

<figure>
<img
src="https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_mean_bs.png"
alt="Mean bottom temperatures in the NOAA Fisheries eastern (1982-2022; 40 years) and northern Bering Sea (2010-2022; 5 years) Bottom Trawl Surveys. Credit: NOAA Fisheries." />
<figcaption aria-hidden="true"><em>Mean</em> <em>bottom</em>
<em>temperatures</em> <em>in</em> <em>the</em> <em>NOAA</em>
<em>Fisheries</em> <em>eastern</em> <em>(1982-2022;</em> <em>40</em>
<em>years)</em> <em>and</em> <em>northern</em> <em>Bering</em>
<em>Sea</em> <em>(2010-2022;</em> <em>5</em> <em>years)</em>
<em>Bottom</em> <em>Trawl</em> <em>Surveys.</em> <em>Credit:</em>
<em>NOAA</em> <em>Fisheries.</em></figcaption>
</figure>

## Anomaly Plot

<figure>
<img
src="https://github.com/afsc-gap-products/survey-live-temperature-map/blob/main/examples/current_anom_bs.png"
alt="Ocean bottom temperature anomaly of the mean bottom temperatures from the eastern (1982-2022; 40 years) and northern Bering Sea (2010-2022; 5 years) bottom trawl surveys compared to their respective 2023 survey stations. Credit: NOAA Fisheries" />
<figcaption aria-hidden="true"><em>Ocean</em> <em>bottom</em>
<em>temperature</em> <em>anomaly</em> <em>of</em> <em>the</em>
<em>mean</em> <em>bottom</em> <em>temperatures</em> <em>from</em>
<em>the</em> <em>eastern</em> <em>(1982-2022;</em> <em>40</em>
<em>years)</em> <em>and</em> <em>northern</em> <em>Bering</em>
<em>Sea</em> <em>(2010-2022;</em> <em>5</em> <em>years)</em>
<em>bottom</em> <em>trawl</em> <em>surveys</em> <em>compared</em>
<em>to</em> <em>their</em> <em>respective</em> <em>2023</em>
<em>survey</em> <em>stations.</em> <em>Credit:</em> <em>NOAA</em>
<em>Fisheries</em></figcaption>
</figure>

# Relevant publications

**Learn more about these surveys and ocean temperatures around Alaska**
(Hoff, 2016; Markowitz et al., 2023, 2023; Rohan et al., 2022; Von
Szalay et al., 2023; Von Szalay and Raring, 2018).

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
