
---

# **Dataset for the paper:** "*Environmental constraints can explain clutch size differences between urban and forest blue tits: insights from an egg removal experiment*"

This dataset contains all the data that was collected and analysed for a project investigating whether urban and forest blue tit populations respond differently to an egg removal experiment.

------------------------------------------------------------------------

Mark D. Pitt, Pablo Capilla-Lasheras, Norah S.S. Alhowiti, Claire J. Branston, Eugenio Carlon, Jelle J. Boonekamp, Davide M. Dominoni. **Environmental constraints can explain clutch size differences between urban and forest blue tits: insights from an egg removal experiment**. *bioRxiv*. <DOI:10.1101/2023.01.05.522710>

------------------------------------------------------------------------

For any further information, please contact: Mark Pitt, email: [markdavidpitt\@gmail.com](mailto:markdavidpitt@gmail.com)

## Description of data:

The data from this project is contained in a single excel document consisting of four sheets.

------------------------------------------------------------------------

### Sheet 1: ALL BROODS

This sheet contains information on the clutch ID's, which analyses the clutch was included in, the clutch sizes, brood sizes, number of fledglings, egg laying dates, and hatching dates for all the nests included in the project.

#### Key:

-   Column A: **analysis_total_number_eggs**- Dictates whether a clutch was included in the analysis investigating the number of eggs laid in both treatment groups in urban and forest habitats.

-   Column B: **analysis_total_number_eggs_reason** - For clutches excluded from the analysis investigating the number of eggs laid, the explanation behind this removal is given in this column.

-   Column C: **analysis_egg_volume**- Dictates whether a clutch was included in the analysis investigating egg volume in both treatment groups in urban and forest habitats.

-   Column D: **analysis_egg_volume_reason** - For clutches excluded from the analysis investigating egg volume, the explanation behind this removal is given in this column.

-   Column E: **analysis_nestling_survival** - Dictates whether a brood was included in the nestling survival analysis for both treatment groups in urban and forest habitats.

-   Column F: **analysis_nestling_survival_reason** - For broods excluded from the analysis investigating nestling survival, the explanation behind this removal is given in this column.

-   Column G: **EXPERIMENTAL_GROUP** - The treatment group nests were assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column H: **CROSS_FOSTER_GROUP** - Dictates to which habitats nestlings were cross-fostered. Either *BETWEEN* or *WITHIN* or *NOT CROSS-FOSTERED*. Nestlings that were cross-fostered between habitats were urban-forest, while within habitat cross-fosters could be urban-urban or forest-forest.

-   Column I: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column J: **LOCATION** - The location that the clutch was located. Either *KELVINGROVE* (the urban habitat) or *SCENE* (the forest habitat).

-   Column K: **HABITAT**- The habitat that the clutch was located. Either *urban* (Kelvingrove Park) or *forest* (SCENE). 

-   Column L: **PAIRED_NESTBOX** - The nestbox ID of the nest to where chicks from the brood were to be cross-fostered.

-   Column M: **julian_1st_egg** - The date when the first egg was recorded in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column N: **1st_EGG_LAYING_DATE** - The date (dd/mm/yy) when the first egg was recorded in a clutch (dd/mm/yy). Converted to Julian days (see above).

-   Column O: **EGGS_FIRST_VISIT** - The number of eggs recorded in the nest on the first egg laying check.

-   Column P: **julian_hatch_date** - The date when hatching was first detected in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column Q: **HATCH_DATE** - The date (dd/mm/yy) when hatching was first detected in a clutch. Converted to Julian days (see above).

-   Column R: **IN_NEST_CLUTCH_SIZE** - The final number of eggs that the female incubated IN the nest. For control treatment nests, this is equal to the number of eggs they laid. For experimental birds, this is equal to the number of eggs they laid - 4, as four eggs were removed from the clutch to manipulate laying effort.

-   Column S: **NUMBER_EGGS_LAID** - The total number of eggs laid by a female.

-   Column T: **EGGS_IN_NEST_DAY3_PRE_CF_MANIPULATION** - The number of eggs in the nest three days after the first chick hatched (day 2).

-   Column U: **NUMBER_HATCHED_EGGS** - The number of eggs that hatched in the nest.

-   Column V: **BROODSIZE_DAY3_PREMANIPULATION** - The number of chicks in the nest before cross-fostering took place.

-   Column W: **DATE_CROSSFOSTERING_MANIPULATION -** The date (dd/mm/yy) cross-fostering occurred.

-   Column X: **TIME_CF_MANIPULATION** - The time at which cross-fostering occurred (hh:mm).

-   Column Y: **BROODSIZE_DAY3_POSTMANIPULATION** - The number of chicks present in the nest after the cross-fostering occurred.

-   Column Z: **BROODSIZE_DAY7** - The number of chicks in the nest on day seven (six days after the first chick had hatched).

-   Column AA: **BROODSIZE_DAY13** - The number of chicks in the nest on day thirteen (twelve days after the first chick had hatched).

-   Column AB: **NUMBER_FLEDGLINGS** - The number of chicks that successfully fledged from the nest.

------------------------------------------------------------------------

### Sheet 2: Egg Volumes

This sheet provides further details on each egg that was recorded in the clutch, including when it was laid and all the egg volume measurements (egg volume was measured three times using three separate images of each egg).

#### Key:

-   Column A: **EXPERIMENTAL_GROUP** - The treatment group nests were assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column B: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column C: **LOCATION** - The location that the clutch was located. Either *Kelvingrove_park* (the urban habitat) or *SCENE* (the forest habitat).

-  Column D: **HABITAT**- The habitat that the clutch was located. Either *urban* (Kelvingrove Park) or *forest* (SCENE). 

-   Column E: **Number_of_eggs_present** - The number of eggs already present in the nest when a new egg was recorded in the nestbox.

-   Column F **EARLIEST_LAYING_ORDER** - The earliest possible position in laying order for each egg.

-   Column G: **LATEST_LAYING_ORDER** - The latest possible position in laying order for each egg.

-   Column H: **EGG_LABEL** - The label given to each egg. Reflects the eggs position in the laying sequence. When two eggs were recorded on the same day, given either an A or B (e.g., egg 3A and 3B would represent eggs 3 and 4 in the laying sequence, but as they were found on the same day the laying order cannot be discerned).

-   Column I: **julian_earliest_lay_date** - The earliest possible laying date for each egg in the clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column J: **EARLIEST_LAY_DATE** - The earliest possible laying date (dd/mm/yyyy) for each egg in the clutch. Converted to Julian date (see above).

-   Column K: **julian_latest_lay_date** - The latest possible laying date for each egg in the clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column L: **LATEST_LAY_DATE** - The latest possible laying date (dd/mm/yyyy) for each egg in the clutch. Converted to Julian date (see above).

-   Column M: **accurate** - Whether we obtained accurate information on the laying order positions and laying dates for each egg. Either *1* (where information on laying dates were accurate), or *2* (where there was uncertainty regarding the lay date). Only eggs assigned a value of *1* were included in the analysis.

-   Column N: **volume -** The volume (mm^3^) measurement of the egg obtained from each photograph.

-   Column O: **photo_id** - The unique identifier given to each photograph.

-   Column P: **no_eggs_photo** - The number of eggs included in each photograph.

-   Column Q: **EGG_REMOVED?** - Whether the egg was experimentally removed from the nest. Either *Y* (yes) or *N* (no).

-   Column R: **weight_g** - The mass (g) of the experimentally removed egg.

-   Column T: **rep** - Reflects the measurement number for each egg. Egg measurements were taken from three separate images, so this column reflects measurements from images *1*,*2* and *3* of each egg. Used in the repeatability analysis to determine how repeatable our measurements were for each egg.

-   Column U: **Group** - The egg laying group eggs were assigned to. Either *1* (eggs one to three in the laying sequence), or *2* (eggs four and beyond in the laying sequence).

------------------------------------------------------------------------
### Sheet 3: ALL NESTLINGS

This sheet provides further details on each individual nestling, including the unique BTO ring numbers they were assigned, as well as information on individual mortality.

#### Key:

-   Column A: **RING_NUMBER** - The unique BTO ring number assigned to each individual nestling.

-   Column B: **julian_hatch** - The date when hatching was first detected in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column C: **hatch_date** - The date (dd/mm/yy) when hatching was first detected in a clutch. Converted to Julian days (see above).

-   Column D: **site_hatch** - The location that the nestling hatched in. Either *SCENE* (the forest habitat) or *Kelvingrove_Park* (the urban habitat).

-   Column E: **habitat_hatch** - The habitat that the nestling hatched in. Either *urban* (Kelvingrove Park) or *forest* (SCENE). 

-   Column F: **nestbox_hatch** - The unique number identifier assigned to the nestbox the chick hatched in. Each nestbox number represents a unique clutch ID.

-   Column G: **site_rearing** - The location that the nestling was reared in (after cross-fostering). Either *SCENE* (the forest habitat) or *Kelvingrove_Park* (the urban habitat).

-   Column H: **habitat_rearing** - The habitat that the nestling was reared in (after cross-fostering). Either *urban* (Kelvingrove Park) or *forest* (SCENE). 

-   Column I: **nestbox_rearing** - The unique number identifier assigned to the nestbox the chick was reared in (after cross-fostering occurred). Each nestbox number represents a unique clutch ID.

-   Column J: **julian_fate** - The date when the nestlings fate (fledged or dead) was known. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column K: **date_fate** - The date (dd/mm/yyyy) when the nestlings fate (fledged or dead) was known. Converted to Julian days (see above).

-   Column L: **fate** - The fate of the nestling. Either *FLEDGED* or *DEAD*.

------------------------------------------------------------------------

### Sheet 4: ALL NESTLING WEIGHTS

This sheet provides further details on each individual nestlings mass throughout the experimental period.

#### Key:

-   Column A: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column B: **LOCATION** - The habitat that the clutch was located. Either *Kelvingrove_park* (the urban habitat) or *SCENE* (the forest habitat). Note that some nestlings from GCU (another urban site) were included as part of the cross-fosterings, but these were excluded from all of the analyses presented in the current project.

-   Column C: **HABITAT**- The habitat that the clutch was located. Either *urban* (Kelvingrove Park) or *forest* (SCENE). 

-   Column D: **RING_NUMBER** - The unique BTO ring number assigned to each individual nestling.

-   Column E: **exp_period** - The period of the experiment where individual biometrics were taken. Either *pre-manipulation* (before cross-fostering occurred) or *post-manipulation* (after cross-fostering occurred).

-   Column F: **julian_date** - The date that mass, wing, and tarsus measurements were obtained from each nestling. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column G: **DATE** - The date (dd/mm/yyyy) that mass, wing, and tarsus measurements were obtained from each nestling. Converted to Julian days (see above).

-   Column H: **TIME** - The time when measurements were taken from each nestling (hh:mm).

-   Column I: **AGE** - The nestlings age when measurements were taken. Nestlings were measured on days two, six, and twelve after hatching.

-   Column J: **WEIGHT** - The body mass (g) of the nestling.

-   Column K: **time_category** - The time that nestlings were measured, but as a categorical variable. Either *morning* (before 12:00 hrs), or *afternoon* (after 12:00 hrs).
