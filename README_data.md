---
---
---

# **Dataset for the paper:** "*Environmental constraints can explain clutch size differences between urban and forest blue tits: insights from an egg removal experiment*"

This dataset contains all the data that was collected and analysed for a project investigating whether urban and forest blue tit populations respond differently to an egg removal experiment.

------------------------------------------------------------------------

Mark D. Pitt, Pablo Capilla-Lasheras, Norah S.S. Alhowiti, Claire J. Branston, Eugenio Carlon, Jelle J. Boonekamp, Davide M. Dominoni. **Environmental constraints can explain clutch size differences between urban and forest blue tits: insights from an egg removal experiment**. *bioRxiv*. <DOI:10.1101/2023.01.05.522710>

------------------------------------------------------------------------

For any further information, please contact: Mark Pitt, email: [markdavidpitt\@gmail.com](mailto:markdavidpitt@gmail.com){.email}

## [Description of data:]{.underline}

The data from this project is contained in a single excel document consisting of seven sheets.

------------------------------------------------------------------------

### [Sheet 1: ALL BROODS]{.underline}

This sheet contains information on the clutch sizes, brood sizes, number of fledglings, egg laying dates, and hatching dates for all the nests included in the project.

#### Key:

-   Column A: **ACTIVE**- Dictates whether a female completed egg laying. If clutch completed (1), if clutch abandoned (0).

-   Column B: **EXPERIMENTAL_GROUP** - The treatment group nests were assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column C: **CROSS_FOSTER_GROUP** - Dictates to which habitats nestlings were cross-fostered. Either *BETWEEN* or *WITHIN* or *NOT CROSS-FOSTERED*. Nestlings that were cross-fostered between habitats were urban-forest, while within habitat cross-fosters could be urban-urban or forest-forest.

-   Column D: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column E: **Provisioning_beh_recorded** - Whether footage on parental provisioning behaviour was obtained from the nest. Nests had either 2,1 or 0 bouts of footage.

-   Column F: **Mother_ID** - the unique BTO ring number assigned to the mother caught from a nestbox.

-   Column G: **FATHER_ID** - the unique BTO ring number assigned to the father caught from a nestbox.

-   Column H: **LOCATION** - The habitat that the clutch was located. Either *KELVINGROVE* (the urban habitat) or *SCENE* (the forest habitat).

-   Column J: **PAIRED_NESTBOX** - The nestbox ID of the nest to where chicks from the brood were to be cross-fostered.

-   Column K: **julian_1st_egg** - The date when the first egg was recorded in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column L: **1st_EGG_LAYING_DATE** - The date (dd/mm/yy) when the first egg was recorded in a clutch (dd/mm/yy). Converted to Julian days (see above).

-   Column M: **EGGS_FIRST_VISIT** - The number of eggs recorded in the nest on the first egg laying check.

-   Column N: **julian_hatch_date** - The date when hatching was first detected in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column O: **HATCH_DATE** - The date (dd/mm/yy) when hatching was first detected in a clutch. Converted to Julian days (see above).

-   Column P: **IN_NEST_CLUTCH_SIZE** - The final number of eggs that the female incubated IN the nest. For control treatment nests, this is equal to the number of eggs they laid. For experimental birds, this is equal to the number of eggs they laid - 4, as four eggs were removed from the clutch to manipulate laying effort.

-   Column Q: **NUMBER_EGGS_LAID** - The total number of eggs laid by a female.

-   Column R: **EGGS_DAY3_PREMANIPULATION** - The number of eggs in the nest two days after the first chick hatched (day 3).

-   Column S: **NUMBER_HATCHED_EGGS** - The number of eggs that hatched in the nest.

-   Column T: **BROODSIZE_DAY3_PREMANIPULATION** - The number of chicks in the nest before cross-fostering took place.

-   Column U: **DATE_CROSSFOSTERING_MANIPULATION -** The date (dd/mm/yy) cross-fostering occurred.

-   Column V: **TIME_CF_MANIPULATION** - The time at which cross-fostering occurred (hh:mm).

-   Column W: **BROODSIZE_DAY3_POSTMANIPULATION** - The number of chicks present in the nest after the cross-fostering occurred.

-   Column X: **BROODSIZE_DAY7** - The number of chicks in the nest on day seven (six days after the first chick had hatched).

-   Column Y: **BROODSIZE_DAY13** - The number of chicks in the nest on day thirteen (twelve days after the first chick had hatched).

-   Column Z: **NUMBER_FLEDGLINGS** - The number of chicks that successfully fledged from the nest.

------------------------------------------------------------------------

### [Sheet 2: ALL PAIRS]{.underline}

This sheet contains further information on the pairs of nests that were included in the cross-fostering manipulation, including more details on how long the manipulation lasted etc.,.

#### Key:

-   Column A: **NESTBOX1_EXPERIMENTAL_GROUP** - The treatment group that the first nestbox in a cross-foster pair was assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column B: **NESTBOX1_NUMBER** - The unique number identifier assigned to the first nestbox in the cross-foster pair. Each nestbox number represents a unique clutch ID.

-   Column C: **NESTBOX2_EXPERIMENTAL_GROUP** - The treatment group that the second nestbox in a cross-foster pair was assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column D: **NESTBOX2_NUMBER** -The unique number identifier assigned to the second nestbox in the cross-foster pair. Each nestbox number represents a unique clutch ID.

-   Column E: **CROSS_FOSTER_GROUP** - Dictates to which habitats nestlings were cross-fostered. Either *BETWEEN* or *WITHIN*. Nestlings that were cross-fostered between habitats were urban-forest, while within habitat cross-fosters could be urban-urban or forest-forest.

-   Column F: **NESTBOX1_LOCATION** - The habitat that the first nest in a cross-foster pair was located. Either *Kelvingrove_Park* (the urban habitat) or *SCENE* (the forest habitat).

-   Column G: **NESTBOX2_LOCATION** - The habitat that the second nest in a cross-foster pair was located. Either *Kelvingrove_Park* (the urban habitat) or *SCENE* (the forest habitat).

-   Column H: **HATCH_DATE_MATCH** - The date (dd/mm/yy) that clutches were matched for the cross-fostering. Pairs of nests that were cross-fostered had to have the same hatch date.

-   Column I: **NESTBOX1_CLUTCH_SIZE** - The clutch size of the first nest in a cross-foster pair.

-   Column J: **NESTBOX2_CLUTCH_SIZE** - The clutch size of the second nest in a cross-foster pair.

-   Column K: **NESTBOX1_BROODSIZE_DAY3** - The brood size of the first nest in a cross-foster pair two days after the first chick had hatched.

-   Column L: **NESTBOX2_BROODSIZE_DAY3** - The brood size of the second nest in a cross-foster pair two days after the first chick had hatched.

-   Column M: **DATE_MANIPULATION** - The date (dd/mm/yy) the cross-fostering manipulation occurred.

-   Column N: **TIME_MANIPULATION_START** - The time the cross-fostering manipulation started (hh:mm).

-   Column O: **TIME_MANIPULATION_COMPLETED** - The time the cross-fostering manipulation was completed (hh:mm).

------------------------------------------------------------------------

### [Sheet 3: ALL EGGS]{.underline}

Note that information in this sheet is replicated in sheet 7 (Egg volumes) - with this data being used in the project. Please see sheet 7 for details on what each column contains. This sheet functioned as a place to store information on when each egg was laid.

------------------------------------------------------------------------

### [Sheet 4: ALL NESTLINGS]{.underline}

This sheet provides further details on each individual nestling, including the unique BTO ring numbers they were assigned, as well as information on individual mortality.

#### Key:

-   Column A: **RING_NUMBER** - The unique BTO ring number assigned to each individual nestling.

-   Column B: **COLOUR_COMBINATION [LEFT FLANK, RIGHT FLANK]** - The colour combinations nestlings were given BEFORE they could be ringed. Nestlings were marked with non-toxic water-based marker pens. Used to establish individual identities in the brood.

-   Column C: **julian_hatch** - The date when hatching was first detected in a clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column D: **hatch_date** - The date (dd/mm/yy) when hatching was first detected in a clutch. Converted to Julian days (see above).

-   Column E: **site_hatch** - The habitat that the nestling hatched in. Either *SCENE* (the forest habitat) or *Kelvingrove_Park* (the urban habitat).

-   Column F: **nestbox_hatch** - The unique number identifier assigned to the nestbox the chick hatched in. Each nestbox number represents a unique clutch ID.

-   Column_G: **site_rearing** - The habitat that the nestling was reared in (after cross-fostering). Either *SCENE* (the forest habitat) or *Kelvingrove_Park* (the urban habitat).

-   Column H: **nestbox_rearing** - The unique number identifier assigned to the nestbox the chick was reared in (after cross-fostering occurred). Each nestbox number represents a unique clutch ID.

-   Column I: **day_manipulation** - The date (dd/mm/yy) the cross-fostering manipulation occurred.

-   Column J: **julian_fate** - The date when the nestlings fate (fledged or dead) was known. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column K: **date_fate** - The date (dd/mm/yy) when the nestlings fate (fledged or dead) was known. Converted to Julian days (see above).

-   Column L: **fate** - The fate of the nestling. Either *FLEDGED* or *DEAD*.

-   Column S: **STATUS** - The fate of the nestling represented numerically. Either 0 (fledged) or DEAD (1). Not used in the analysis.

-   Column S: **EXIT** - The number of days to when a chick exited the experiment. For fledged chicks this was \>21 days after the chick hatched in the brood. For dead nestlings, this was the number of days after hatching the dead nestling was discovered. Not used in the analysis.

------------------------------------------------------------------------

### [Sheet 5: ALL NESTLING WEIGHTS]{.underline}

This sheet provides further details on each individual nestlings mass throughout the experimental period, as well as information on their wing and tarsus lengths.

#### Key:

-   Column A: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column B: **LOCATION** - The habitat that the clutch was located. Either *Kelvingrove_park* (the urban habitat) or *SCENE* (the forest habitat). Note that some nestlings from GCU (another urban site) were included as part of the cross-fosterings, but these were excluded from all of the analyses presented in the current project.

-   Column C: **COLOUR_COMBINATION [LEFT FLANK, RIGHT FLANK]** - The colour combinations nestlings were given BEFORE they could be ringed. Nestlings were marked with non-toxic water-based marker pens. Used to establish individual identities in the brood.

-   Column D: **RING_NUMBER** - The unique BTO ring number assigned to each individual nestling.

-   Column E: **exp_period** - The period of the experiment where individual biometrics were taken. Either *pre-manipulation* (before cross-fostering occurred) or *post-manipulation* (after cross-fostering occurred).

-   Column F: **julian_date** - The date that mass, wing, and tarsus measurements were obtained from each nestling. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column G: **DATE** - The date (dd/mm/yy) that mass, wing, and tarsus measurements were obtained from each nestling. Converted to Julian days (see above).

-   Column H: **TIME** - The time when measurements were taken from each nestling (hh:mm).

-   Column I: **AGE** - The nestlings age when measurements were taken. Nestlings were measured on days two, six, and twelve after hatching.

-   Column J: **WEIGHT** - The body mass (g) of the nestling.

-   Column K: **TARSUS** - The tarsus length (mm) of the nestling.

-   Column L: **WING** - The wing length (mm) of the nestling.

-   Column N: **BLOOD_ID** - The sample number for the blood sample taken from each nestling, Recorded as part of a separate project.

-   Column O: **SAMPLE_ID** - The sample number for the faecal sample taken from each nestling. Recorded as part of a separate project.

-   Column P: **time_category** - The time that nestlings were measured, but as a categorical variable. Either *morning* (before 12:00 hrs), or *afternoon* (after 12:00 hrs).

------------------------------------------------------------------------

### [Sheet 6: ADULTS]{.underline}

This sheet provides further details on the parents of each brood.

#### Key:

-   Column A: **RING_NUMBER** - The unique BTO ring number assigned to each individual adult.

-   Column B: **COLOUR_RING_COMBINATION** - If adults were colour ringed, this column had details on the colour ring combinations.

-   Column C: **NESTBOX** - The nestbox number that the adult was caught at.

-   Column D: **LOCATION** - Where the adult was caught. Either *Kelvingrove_park* (the urban habitat) or *SCENE* (the forest habitat).

-   Column E: **DATE**- The date (dd/mm/yy) when the adult was caught.

-   Column F: **TIME**- The time (hh:mm) when the adult was caught.

-   Column G: **AGE** - The age of the adult. Either a first year breeding bird (5) or older than one year (6).

-   Column H: **SEX** - The sex of the parent. Either *F* (female) or *M* (male).

------------------------------------------------------------------------

### [Sheet 7: Egg Volumes]{.underline}

This sheet provides further details on each egg that was recorded in the clutch, including when it was laid, its length, and width. Also includes all the egg volume measurements (egg volume was measured three times using three separate images of each egg).

#### Key:

-   Column A: **EXPERIMENTAL_GROUP** - The treatment group nests were assigned. Either *EXPERIMENTAL* or *CONTROL*. Experimental nests had four eggs removed from the nest, control clutches had no eggs removed.

-   Column B: **NESTBOX** - The unique number identifier assigned to each nestbox. Each nestbox number represents a unique clutch ID.

-   Column C: **LOCATION** - The habitat that the clutch was located. Either *Kelvingrove_park* (the urban habitat) or *SCENE* (the forest habitat).

-   Column D: **Number_of_eggs_present** - The number of eggs already present in the nest when a new egg was recorded in the nestbox.

-   Column E: **EARLIEST_LAYING_ORDER** - The earliest possible position in laying order for each egg.

-   Column F: **LATEST_LAYING_ORDER** - The latest possible position in laying order for each egg.

-   Column G: **EGG_LABEL** - The label given to each egg. Reflects the eggs position in the laying sequence. When two eggs were recorded on the same day, given either an A or B (e.g., egg 3A and 3B would represent eggs 3 and 4 in the laying sequence, but as they were found on the same day the laying order cannot be discerned).

-   Column H: **julian_early** - The earliest possible laying date for each egg in the clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column I: **EARLIEST_LAY_DATE** - The earliest possible laying date (dd/mm/yy) for each egg in the clutch. Converted to Julian date (see above).

-   Column J: **julian_latest** - The latest possible laying date for each egg in the clutch. In Julian days (represented as the number of days since 01/01/2022), which is needed for the analysis.

-   Column K: **LATEST_LAY_DATE** - The latest possible laying date (dd/mm/yy) for each egg in the clutch. Converted to Julian date (see above).

-   Column L: **accurate** - Whether we obtained accurate information on the laying order positions and laying dates for each egg. Either *1* (where information on laying dates were accurate), or *2* (where there was uncertainty regarding the lay date). Only eggs assigned a value of *1* were included in the analysis.

-   Column M: **complete?** - Whether a female completed the clutch. Either *Y* (yes) or *N* (no).

-   Column N: **eggs_photographed** - The number of eggs that were photographed in the clutch.

-   Column O: **width** - The width (mm) measurement of the egg obtained from each photograph

-   Column P: **length** - The length (mm) measurement of the egg obtained from each photograph.

-   Column Q: **volume -** The volume (mm^3^) measurement of the egg obtained from each photograph.

-   Column R: **volume1** - The volume (cm^3^) measurement of the egg obtained from each photograph. Not included in the analysis.

-   Column S: **photo_id** - The unique identifier given to each photograph.

-   Column T: **no_eggs_photo** - The number of eggs included in each photograph.

-   Column U: **EGG_REMOVED?** - Whether the egg was experimentally removed from the nest. Either *Y* (yes) or *N* (no).

-   Column V: **weight_g** - The mass (g) of the experimentally removed egg.

-   Column W: **yolk_weight_g** - The mass (g) of the egg yolk from the experimentally removed egg.

-   Column Y: **rep** - Reflects the measurement number for each egg. Egg measurements were taken from three separate images, so this column reflects measurements from images *1*,*2* and *3* of each egg. Used in the repeatability analysis to determine how repeatable our measurements were for each egg.

-   Column Z: **Group** - The egg laying group eggs were assigned to. Either *1* (eggs one to three in the laying sequence), or *2* (eggs four and beyond in the laying sequence).
