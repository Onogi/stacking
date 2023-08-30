# Stacking

An R package for ensemble learning stacking. This package is based on caret, and learners implemented in caret can be used as base and meta learners. Please see Nukui and Onogi (2023) (https://doi.org/10.1101/2023.06.06.543970) for the details of the package. R scripts for examples illustrated in the article are provided in /RscriptsExamples.

The latest version is 0.1.2.

Fixed bugs: the previous version (ver. 0.1.0) uses 'glmnet' as the meta learner irrespective of the choice with the argument. Please use the latest version to use other learners as the meta learner.

Novel features: the latet version introduce 'TrainEachFold' in training of meta learner. When TrainEachFold is TRUE, the meta learner is trained at each cross-valudation fold (the cross-validation is conducted using base learners to produce explanatory variables for the meta model). When TrainEachFold is FALSE (default), the meta learner is trained by pooling all values predicted by the base models of each fold.
