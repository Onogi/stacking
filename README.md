# Stacking

An R package for ensemble learning stacking. This package is based on caret, and learners implemented in caret can be used as base and meta learners. Please see Nukui and Onogi (2023) (https://doi.org/10.1101/2023.06.06.543970) for the details of the package. R scripts for examples illustrated in the article are provided in /RscriptsExamples.

The latest version is 0.2.1.

Novel features: with the latet version, users can choose either of cross-validation or random sampling to train base learners. In addition, users can add the original features (X) to the explanatory variables when training the meta learner.
