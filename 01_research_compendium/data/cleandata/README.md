# Access Data
Because the data from the Survey of Health, Ageing, and Retirement
in Europe (SHARE) are only available after registration, unfortunatley, I am
not able to share them on github. 

**If you are not registered yet**, to access the data, please follow these steps:
- visit: [https://share-eric.eu/data/](https://share-eric.eu/data/)
- go to "Become a User" and follow the steps given on this page
- it can take some time (a few days/weeks) before the registration is complete

**If you are already registered**, to access the data, please follow these steps:
- visit: [https://releases.sharedataportal.eu/users/login](https://releases.sharedataportal.eu/users/login) and log in
- here you see the data files for all waves, in this study only waves 2, 4, 5, 6 and 7 are used, so these are the only ones that need to be downloaded
- Note: the SPSS data are used, for example: sharew1_rel9-0-0_ALL_datasets_spss.zip

**After getting access to the full data**, select only those files that are needed in this study.
An example of which data files are relevant and how they should be placed in the folder structure can be found below:

##### For wave 1
- 01_research_compendium/data/rawdata/wave1/sharew1_rel9-0-0_br.sav
- 01_research_compendium/data/rawdata/wave1/sharew1_rel9-0-0_dn.sav
- 01_research_compendium/data/rawdata/wave1/sharew1_rel9-0-0_gv_health.sav
- 01_research_compendium/data/rawdata/wave1/sharew1_rel9-0-0_ph.sav

##### For wave 2 (repeat for 4, 5, 6 and 7)
- 01_research_compendium/data/rawdata/wave2/sharew2_rel9-0-0_br.sav
- 01_research_compendium/data/rawdata/wave2/sharew2_rel9-0-0_dn.sav
- 01_research_compendium/data/rawdata/wave2/sharew2_rel9-0-0_gv_health.sav
- 01_research_compendium/data/rawdata/wave2/sharew2_rel9-0-0_ph.sav
- 01_research_compendium/data/rawdata/wave2/sharew2_rel9-0-0_xt.sav
- *Note: waves 2 - 7 contain one dataset more than wave 1, as these contain information about death since last interview*

# Data Structure and Variables

In this study, waves 1-7 of the SHARE are used.

The focus is on the following variables:

# Outcomes

-   ph006d1: Heart attack: ever diagnosed
-   ph006d4: Stroke: ever diagnosed
-   ph009_1: Age heart attack or other heart problems
-   ph009_4: Age stroke or cerebral vascular disease
-   ph067_1, ph067_2, ph072_1, ph072_2: Heart attack / stroke since
    last interview
-   xt011\_: Main cause of death heart attack / stroke

# Exposure

-   dn014\_ Marital status (married & living together)

# Adjustment Sets\

*Age* 

- dn002\_: month of birth - dn003\_: year of birth

*Sex* 

- dn042\_: Male or female

*Country* 

- country: country identifier

*Education* 

- dn010\_: highest degree obtained (education)

*For smoking it's a bit unclear:* 

- br001\_: Ever smoked daily 
- br002\_: Smoke at present time 
- br003\_: How many years smoked 
- br004\_: Age stopped smoking 
- br005\_: Type of smoke (cigarettes/pipes etc) 
- br006\_ - br008\_: Average amount of cigs / day

*Note: given that not all variables were present in all waves, I decided to*
*include only br002\_ (smoke at present time).*

*Physical Activity (from health dataset)* 
- phactiv: physical inactivity

## Extra Variables Needed for Exclusion Criteria
*Subjects married before 18*
- dn015\_: Year of marriage (when living together)

*Change in exposure*
- dn018\_: Since when divorced
- dn019\_: Since when widowed 

For more information about the variables, visit:
<https://www.share-datadocutool.org/study-units/view/1>