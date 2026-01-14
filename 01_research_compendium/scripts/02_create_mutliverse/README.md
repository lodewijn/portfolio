# Setting up the Multiverse

## Decision Space
First, the 01_ files contain the setting up of the decision space.

**Outcome Variable**
The outcome variable in the original paper by Kowall et al. (2025) consisted
of combinations of four different conceptual options, where two of the variables
in the data set were combined into one concept:

-   ph006d1 / ph006d4: Heart attack/stroke ever diagnosed
-   ph009_1 / ph009_4: Age heart attack or stroke
-   ph072_1, ph072_2: Heart attack / stroke since last interview
-   xt011\_: Main cause of death heart attack / stroke

The first code chunk first creates those conceptual options, and then makes all 
possible combinations of those options (ranging from 1 option to all 4).

**Adjustment Set**
Because of computational and time limits, I decided to only include variables 
in the adjustment set that were used by at least four analysis teams. The second
code chunk creates a list of all different combinations of covariates
(including) the empty set.

**Decision Space**
In the third code chunk, a data frame containing all possible combinations of 
all decisions is created using `expand.grid`

*Note: for more information about the variables see REAME.md in the data folder.*
---

- DS: Data Set
