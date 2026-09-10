# Independent review of the all-predictor extension

Review date: 10 September 2026. This note reviews the proposed extension after the dietary analysis was completed. It is not a prospective protocol for the earlier analyses. No new model was fitted for this review, and no existing analysis, result, or manuscript was modified.

The defensible extension is a comparison of **four specified model families in two fixed information scenarios**, with individual and joint-block held-out permutation importance as the main descriptive focus. The scenarios contain 38 predictors without the six oral symptoms and all 44 predictors with those symptoms. Reuse the six existing spline-logistic, random-forest, and histogram-gradient-boosting prediction sets. Add one bounded multilayer-perceptron (MLP) development procedure per scenario. Complete boosting importance without changing its existing predictions or hyperparameters.

## Evidence already available

All six existing variants include the same 5,036 dentate participants and 2,689 observed cases. Survey-weighted results from `outputs/dietary/tables/model_performance.csv` are:

| Family | Predictors | AUC (95% conditional interval) | Brier score | Calibration slope |
|---|---:|---:|---:|---:|
| Spline logistic | 38 | 0.618 (0.590–0.647) | 0.238261 | 0.979 |
| Random forest | 38 | 0.614 (0.585–0.642) | 0.237505 | 1.082 |
| Histogram boosting | 38 | 0.612 (0.583–0.639) | 0.239079 | 0.781 |
| Spline logistic | 44 | 0.630 (0.601–0.659) | 0.235996 | 0.907 |
| Random forest | 44 | 0.637 (0.605–0.666) | 0.234062 | 1.200 |
| Histogram boosting | 44 | 0.635 (0.605–0.664) | 0.234748 | 0.844 |

These values establish modest discrimination and meaningful differences between discrimination and probability calibration; they do not establish a universally superior family. Uncertainty is conditional on the fitted models and fixed validation partitions, rather than covering model redevelopment or a new population.

The existing individual-permutation file contains 4,100 rows: 38 or 44 raw predictors × five outer folds × five repeats for each spline and random-forest scenario. The grouped file contains 350 rows for the corresponding three or four disjoint blocks. Their summaries contain 164 individual and 14 block rows. **Neither boosting scenario has permutation importance yet.** Sources are `permutation_importance.csv`, `grouped_permutation_importance.csv`, `importance_stability.csv`, and `grouped_importance_stability.csv` in the same directory.

## Minimum fair extension

1. Preserve the original eligible cohort, participant identifiers, 5 outer/3 inner PSU-disjoint folds, outcome coding, and analysis weights. Validate joins by identifier and equality of outcome, weight, stratum, PSU, and fold; never assume positional correspondence between independently loaded files. Keep the full oral-examination design for the existing bootstrap procedure. The raw class split is approximately 53.4%/46.6%, providing no imbalance-based rationale for synthetic resampling.
2. Reuse cached predictions and importance for the four spline/RF variants, and cached predictions for both boosting variants. Their caches contain probabilities rather than fitted estimators. Reconstruct only the ten required boosting outer fits using the original selected parameters, original preprocessing and weights, and seed `20260910 + 1000 * outer_fold`. Require agreement with archived outer-fold predictions within a declared tight tolerance before calculating importance. A mismatch is a provenance failure requiring investigation, not permission to replace the baseline silently. Do not repeat tuning of the old families.
3. Develop the two MLP variants with a small grid fixed before examining their outer predictions. Two two-layer architectures crossed with three declared regularization/training-budget regimes is a bounded six-candidate procedure. Choose candidates exclusively by pooled, survey-weighted inner-validation log loss, using the existing inner splits and deterministic seeds. Do not select seeds, add architectures, or change the stopping budget because of outer AUC. Coupling regularization and epoch budgets is a restricted candidate set, not a factorial test of either component; report it as such.
4. Keep beverage log1p transformations, categorical handling, numeric missingness indicators, and training-only imputation. Add MLP-specific numeric standardization learned inside every training fold: the existing tree preprocessing does not scale numeric terms. Preserve income as categories and do not convert category labels to artificial continuous scores. Retain the primary uncapped beverage values; the already completed positive-p99 sensitivities need not be duplicated.
5. Normalize fitting weights to mean one within each training set, pass them to MLP fitting, and evaluate with the same survey-weighted metrics as the baselines. Record the actual training budget, iterations, final loss, convergence warnings, finite probabilities, and finite coefficients. Fixed-epoch training can be deliberate regularization; reaching its limit must not be falsely labelled optimization convergence. Conversely, a poorly optimized MLP does not justify a general conclusion against neural networks.

Local inspection confirmed scikit-learn **1.9.0** and `MLPClassifier.fit(..., sample_weight=None)`. Its code weights the data loss and gradients and divides the penalty by the sum of fitting weights. The live official documentation, currently 1.9.1, confirms weight support since 1.7. Automatic `early_stopping=True` uses an internal stratified row split and validation accuracy; it does not accept the survey PSU grouping. Keep it disabled. Training-loss stopping or a declared fixed epoch budget avoids introducing that incompatible split. [Official MLP API](https://scikit-learn.org/stable/modules/generated/sklearn.neural_network.MLPClassifier.html)

## Importance and uncertainty

For all eight variants, use the same five held-out marginal permutations per raw variable per outer fold, with common donor permutations where the scenarios contain the same variable. Keep recipients' outcomes and survey weights fixed. Jointly permute each intact block: context 17, intake 15, label-related behaviours 6, and oral symptoms 6 when present. Reuse the previous block definitions; normalize the exported `intake`/`diet` naming explicitly.

Report Brier increase as the main importance quantity and AUC drop as complementary. Show negative values, weighted mean effects, and rank variation across folds. Fold ranges and repeat variation are descriptive stability measures, **not 95% sampling intervals**. Cross-model agreement is informative, but do not pool rankings into an unexplained universal score or call them percentages of explained caries. Separate permutations of frequency and derived beverage volume can break their dependence; intact intake-block permutations preserve within-block combinations. Both quantify model reliance under a defined perturbation, not a unique causal contribution or the gain from refitting without a variable.

Assess prediction using Brier score, log loss, AUC, calibration intercept and slope, and calibration plots. Prespecify paired comparisons: MLP versus the existing spline reference within each scenario, and the 44-versus-38 information contrast within each family; comparisons with RF/boosting can remain secondary. Reuse the 1,000 full-design PSU bootstrap draws for aligned conditional intervals. Do not fit a calibrator on pooled outer predictions and then report those same recalibrated predictions as independently validated.

The 44-predictor scenario includes concurrent oral symptoms, and both scenarios retain examined remaining teeth. Their target is prevalent-caries classification, not future caries or a fully remote screening tool. Exclude decayed-tooth count, DMFT components defining the outcome, and other direct outcome-derived inputs. Existing missingness summaries and the complete 44-variable descriptive profile can be reused.

## Position on neural networks

A two-layer MLP is a reasonable bounded additional family; it is not an exhaustive evaluation of deep learning. Grinsztajn, Oyallon, and Varoquaux compared 45 tabular datasets and found strong tree-model performance in medium-sized samples. Their controlled benchmark excluded missing data and balanced classes, so it is not direct evidence for this complex survey. [NeurIPS 2022 original paper](https://proceedings.neurips.cc/paper_files/paper/2022/file/0378c7692da36807bdec87ab043cdadc-Paper-Datasets_and_Benchmarks.pdf)

McElfresh and colleagues compared 19 methods across 176 datasets. Neural methods performed best for some datasets, but tuning and dataset characteristics often mattered more than the family label; trees were particularly competitive with irregular feature distributions. This supports testing the MLP under a declared budget, without presuming higher AUC or interpreting a single MLP result as proof for or against all neural approaches. [NeurIPS 2023 original paper](https://papers.neurips.cc/paper_files/paper/2023/file/f06d5ebd4ff40b40dd97e30cee632123-Paper-Datasets_and_Benchmarks.pdf)

Freeze the extension specification and its provenance before new fits. Present it as a new exploratory question prompted after the earlier results, while retaining the original dietary and Andrea analyses unchanged.
