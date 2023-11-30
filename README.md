README
================
Beñat Yañez
30/11/2023

------------------------------------------------------------------------

This is the code and data used for an Ecological Impact Assessment
Report.This is an assessment for the Professional Skills for Ecology and
Evolution course in the Ecology, Ecology, Evolution and Biodiversity MSc
at Edinburgh University.

------------------------------------------------------------------------

## Goals for this report

-   #### Using previously gathered data and our own data identify any protected species or those in the Scottish biodiversity list in the sites

-   #### Calculate the water quality of two sampled streams in the plots

-   #### Calculate diversity patterns from the data obtained

------------------------------------------------------------------------

Data obtained from a week long field trip in Arran and a Desk Based
Survey.

------------------------------------------------------------------------

The purpose of each code is:

BatDetection:

-   From the bats data containing the output of Batdetect2 analysis of
    Audio moth audio files; select the calls with a class probability
    higher than 0.71
-   Save these calls, the name of the species and the recording event
    they are associated with

DiversityBirds:

-   Have the bird species names from the Transect walks and Point counts
    changed to a harmonized taxonomy using rgbif
-   Calculate alpha diversity from presence data for each event and for
    both sites
-   Calculate beta diversity between sites using betapart
-   Make a Rank Abundance Distribution graph for each site using ggplot2
-   Calculate Simpsons diversity index from abundance data

DiversityTerrestrialInvertebrate:

-   Use the Terrestrial Invertebrate occurrence data
-   Calculate alpha diversity at a family level from presence data for
    each event and for both sites
-   Calculate beta diversity at family level between sites using
    betapart
-   Make a Rank Abundance Distribution graph for each site using ggplot2
-   Calculate Simpsons diversity index from abundance data

ProtectedSpeciesAtSite

-   Load the NBN data for each site and the both the Protected Species
    and Biodiversity lists
-   Change the names so the taxonomy is harmonized using rgbif and save
    a table with the harmonized names for the protected species and
    biodiversity list
-   Identify species from the NBN Atlas that are either protected or in
    the Biodiversity list and save the results

ProtectedSpeciesFromSampling

-   Load the harmonized Protected Species and Biodiversity lists and the
    all the sampled data which constains species identifications
-   Harmonize the names of these species using rgbif
-   Identify species that are either protected or in the Biodiversity
    list and save the results

WaterQuality

-   Load the sampled aquatic invertebrate data and the family/water
    quality score sheet
-   Calculate the Average Score per Taxon for each stream in the sites.
