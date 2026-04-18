# Contributing

Thank you for your interest in improving this repository.

## General expectations

- Keep functions modular and scenario-agnostic whenever possible.
- Preserve the standardized simulation output structure.
- Keep post-processing focused on variable importance and variable selection.
- Avoid introducing model-comparison metrics that are not comparable across all supported model classes.

## Before opening a pull request

1. Run a small scenario locally with reduced `nsim`.
2. Confirm that a saved `.rds` result can still be analyzed with `scripts/analyze_saved_result.R`.
3. Document any package-version assumptions that affect model object structure.
4. Update `README.md` if the public workflow changes.

## Style

- Use clear function names.
- Prefer explicit argument names.
- Add comments only where they improve reproducibility or interpretation.
