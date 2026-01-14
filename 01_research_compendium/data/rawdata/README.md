# Data Structure and Variables

In this study, waves 1-7 of the Survey of Health, Ageing, and Retirement
in Europe are used.

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

<https://www.share-datadocutool.org/study-units/view/1>