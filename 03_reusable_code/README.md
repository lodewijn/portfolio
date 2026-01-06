# Markup languages and reproducible programming in statistics

Deliverable 3: Reusable `R` code wrapped in an `R` package or `Shiny` app.

- If `R` package, link the package website and your development repository (on GitHub).
- If `Shiny app`, link the app on a `Shiny` server and your development repository (on GitHub).

See course manual for requirements.

# My Contribution

Because I wanted multiple tabs, but wanted to make sure each tab worked properly, 
first I created two separate shiny apps.
These can be found under `decisionspace.R` and `decisionsensitivity.R`. 
Then, when everything was working, I combined the two into one: `combinedShiny.R`.

This shiny app aims to show how multiverse analysis works, 
by setting up the decision space, where all possible analytical paths are mapped, 
and then calculating the sensitivity of each decision based on the distributions 
of the outcomes of each universe.