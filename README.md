# Dietary information and prevalent dental caries in ENS 2016–2017

The current proposal asks how much dietary questionnaire information improves classification of prevalent cavitated caries beyond demographic, dental, socioeconomic, health and dental-care information. It is a new, dataset-informed analysis of 5,036 dentate participants, including 2,689 with caries. It does not predict future lesions or estimate causal dietary effects.

## Current independent proposal

- [Executable English report](docs/dietary_incremental.html) and [Python Quarto source](docs/dietary_incremental.qmd).
- [Analysis plan fixed before the new model fits](docs/dietary-incremental-analysis-plan.md), with the primary comparison and all exploratory extensions.
- [Verified reference register](docs/dietary-manuscript-references.json).
- [New aggregate tables](outputs/dietary/tables) and [new composite figures](outputs/dietary/figures).

The expanded data contain 44 predictor constructs: 17 context variables, 15 direct dietary indicators, six label behaviours and six concurrent oral-status indicators. The direct dietary block includes questionnaire-derived soda and juice volumes in addition to consumption frequency. Symptoms are kept in a distinct information scenario because they may reflect existing disease. Labels are not treated as direct measures of food intake.

Seven information sets are evaluated with penalized spline logistic regression, random forest and histogram gradient boosting, yielding 21 main variants. Two additional spline variants examine beverage extremes using positive-training-value percentile caps. All models use the same five outer and three inner PSU-disjoint partitions, survey-weighted training and evaluation, and training-only statistical preprocessing. No outcome balancing is used.

The primary estimand is the spline Brier-score gain from adding direct diet to expanded context. Paired AUC and log-loss gains, dietary increments conditional on oral symptoms, label-behaviour increments and whole-block/individual permutation scores are also reported. Positive refitted performance gains uniformly indicate improvement; permutation Brier increases instead measure deterioration after perturbation. Intervals use 1,000 shared stratified PSU-bootstrap draws conditional on fitted models and partitions. They are not external validation or uncertainty for the entire redevelopment process.

## Main finding

Adding the 15 dietary indicators did not demonstrate incremental predictive value in any of the three model families. In the primary spline comparison, the Brier score increased from 0.23349 to 0.23788: the prespecified gain was −0.00439 (conditional 95% interval −0.00716 to −0.00162), a 1.88% increase in error. AUC decreased from 0.63691 to 0.62251. Expanded context and concurrent oral-status information contributed more than the measured diet block. This finding concerns the available questionnaire measurements and the specified validation procedure; it does not contradict a biological role of diet in caries.

## Run the new analysis

Use the pinned analysis environment in requirements-lock.txt. Place the privately held ENS SPSS release at data/data.sav; participant records are not distributed in this repository.

```sh
.venv/bin/python scripts/run_dietary.py --stage all --jobs 3 --bootstrap 1000
.venv/bin/python scripts/render_dietary.py
.venv/bin/python -m pytest -q
```

The prepare, models and summarize stages may be run separately. Validated private fold caches allow interrupted model runs to resume. The summarize stage refreshes all new descriptive tables and checks training provenance before evaluation. The renderer refuses incomplete or stale aggregate runs. Individual records, fold assignments, probabilities and caches remain under ignored private paths.

Core modules are expanded_data.py for questionnaire reconstruction, dietary_models.py for nested validation, dietary_results.py for aggregate evaluation and dietary_figures.py for the figures. The new analysis writes only outputs/dietary and outputs/private/dietary; it does not replace historical results.

## Earlier analysis and Andrea revision

The [earlier 27-predictor reanalysis](docs/caries_python.html), [historical coding audit](docs/historical-analysis-audit.md), [R-to-Python migration audit](docs/r-to-python-migration.md), original R sources and saved reference outputs remain available. The prior guide is preserved in [LEGACY_ANALYSIS.md](LEGACY_ANALYSIS.md).

The earlier Andrea manuscript correction is a separate local editorial deliverable. The new manuscript and supplement are created in manuscript_drafts/dietary_incremental_proposal, and a preserved copy of the previous correction is in manuscript_drafts/andrea_revision_separate. These local manuscripts and private source materials are excluded from Git. No journal submission or public preprint deposit is made by this workflow.

## Interpretation and outstanding source confirmation

The working examination-phase weight is Fexp_F1F2p_Corr, equal to Fexp_F2p_Corr in this release; confirmation against the official ENS phase-selection manual remains outstanding. Available beverage volumes are reported standard glasses, not grams of sugar. Direct measures of brushing, dentifrice/fluoride exposure, plaque and xerostomia were not identified in the supplied questionnaire metadata. Missing or coarse exposure measurements limit interpretation of dietary increments. Small or absent gains do not establish a lack of biological dietary importance.
