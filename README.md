# Predictor importance for prevalent dental caries in ENS 2016–2017

The current proposal examines which available variables inform classification of prevalent caries, returning to the earlier manuscript sequence of participant profile, model comparison and predictor importance. It evaluates 44 constructs in 5,036 dentate participants, including 2,689 cases. A complementary 38-construct scenario excludes concurrent oral-status indicators. The analysis is exploratory and was reframed after inspecting previous results.

## Current report and results

- [Executed English report](docs/all_predictors.html) and [Python Quarto source](docs/all_predictors.qmd).
- [Extension plan fixed before the neural fits](docs/all-predictor-analysis-plan.md) and [independent methods review with primary references](docs/all-predictor-methods-review.md).
- [Aggregate results](outputs/all_predictors/tables) and [nine composite figures](outputs/all_predictors/figures), each exported as PNG, SVG and PDF.

The 44 constructs comprise 17 context variables, 15 dietary-consumption indicators, six label behaviours and six concurrent oral-status indicators. Water and questionnaire-derived soda/juice volumes remain included. The complete models retain all 44 constructs; top-variable plots are descriptive displays, not feature screening. All signed scores and ranks are supplied in the tables.

Four model families are compared in each scenario: penalized spline logistic regression, random forest, histogram gradient boosting and a regularized multilayer perceptron (MLP). Six classical prediction sets are preserved exactly from the preceding analysis. Boosting outer fits are reconstructed only to obtain missing importance, with predictions required to reproduce the original held-out values. The two new MLP variants use a fixed six-candidate search over two hidden-layer architectures and regularization/epoch settings. All share the same five outer and three inner PSU-disjoint partitions, survey-weighted fitting/tuning/evaluation and training-only preprocessing. No synthetic balancing is used.

## Findings and limits

With all 44 constructs, the classical-model AUCs are 0.630 for spline logistic, 0.637 for random forest and 0.635 for boosting. Age, self-rated oral health, remaining teeth and reason for dental consultation are repeatedly among the leading classical-model predictors, with different rankings across families. These results describe model reliance and modest discrimination; they do not establish causal determinants or clinical utility.

The regularized MLP obtained AUC 0.488 in both information scenarios and made nearly constant predictions within each outer fold. Its variable ranks, positive-fold fractions and permutation AUC changes are numerically uninformative and are not interpreted. Raw values remain available. A reporting-only rule marks a model as near zero in the rank figure when every absolute mean Brier importance is below 1e-8; this rule was adopted after observing the neural predictions, without changing any fitted model or search setting. Cross-model interpretation uses separately reported agreement across the three classical families. This result concerns the bounded MLP search, not all deep-learning architectures.

Individual and joint-block importance use held-out permutation with five repeats per outer fold. Fold ranges describe stability rather than sampling confidence intervals. Performance intervals and paired model comparisons use 1,000 shared stratified PSU-bootstrap draws conditional on trained models and fixed partitions; they do not represent external validation or the uncertainty of repeating all development. Both scenarios contain examined remaining-teeth information and dental-care variables, while the complete scenario also contains symptoms that may reflect existing disease. Neither scenario predicts future caries incidence.

## Reproduce the extension

Use the pinned environment in requirements-lock.txt and privately authorized data/data.sav. Native MLP sample weighting requires scikit-learn 1.7 or later; the recorded analysis used 1.9.0. The original private dietary results must exist. If reproducing from a fresh checkout with no private caches, first execute scripts/run_dietary.py --stage all --jobs 3 --bootstrap 1000 to reconstruct them from the same source release.

```sh
.venv/bin/python scripts/run_all_predictor_models.py --jobs 3
.venv/bin/python scripts/summarize_all_predictors.py --bootstrap 1000
.venv/bin/python scripts/render_all_predictors.py
.venv/bin/python -m pytest -q
```

Authenticated private caches allow the new model run to resume. The summary and report verify the exact declared fitting and evaluation source hashes and the fixed extension plan. New outputs are written under outputs/all_predictors and outputs/private/all_predictors. Individual records, predictions, fold assignments and caches remain excluded from Git. The reviewed local Word package can be rebuilt using scripts/package_all_predictors.py once its editorial sources and document QA are complete.

## Preserved earlier work

The [dietary-increment proposal](docs/dietary_incremental.html), its [guide](DIETARY_ANALYSIS.md), the [earlier general reanalysis](docs/caries_python.html), original R sources, [historical coding audit](docs/historical-analysis-audit.md), and [migration audit](docs/r-to-python-migration.md) remain available. The earlier Andrea correction stays separate under manuscript_drafts/andrea_revision_separate. New English manuscript, supplement and reporting maps are local author-review artifacts under manuscript_drafts/all_predictor_proposal. No journal submission or public preprint deposit is performed.

The working examination weight is Fexp_F1F2p_Corr, equal to Fexp_F2p_Corr in the supplied release; official examination-phase documentation still requires confirmation. Beverage volumes are reported glasses, not grams of sugar, and extreme values warrant source confirmation. Direct measures of brushing, dentifrice/fluoride exposure, plaque and xerostomia were not identified in the available questionnaire metadata. Authorship, affiliations, declarations and data-access wording require author review before submission.
