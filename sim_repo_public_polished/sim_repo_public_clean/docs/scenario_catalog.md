# Scenario catalog

Each scenario is defined by these dimensions:

- `outcome_type`: `continuous` or `binary`
- `relationship`: `linear` or `nonlinear`
- `distribution`: `mvnorm` or `mvt`
- `missing_mechanism`: `none`, `MAR`, or `MNAR`
- `missing_target`: `complete`, `exposure`, `outcome`, or `exposure_outcome`
- `method`: `complete`, `available`, `mean`, `median`, `knn`, `mice`, `amelia`

Example scenario name:

`binary_nonlinear_mvnorm_MAR_exposure_outcome_knn`
