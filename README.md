# Predictor contributions to prevalent dental caries classification in Chile

This repository contains a reproducible Python reanalysis of how 27 demographic, dental and dietary predictors contribute to classification of prevalent cavitated caries in the Chilean National Health Survey 2016–2017. The English report presents all predictor contributions and their stability across held-out survey clusters in random forest and spline logistic regression, with overall performance as context. Water was an emerging finding of interest and is retained as an exploratory follow-up, not the original primary objective. Quarto generates the report from completed Python results.

This is the original [ENS-dental-caries repository](https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries), with the R reference audited at commit [`77c107feb56d3f825cd4ad26354b672f8bfebe38`](https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries/tree/77c107feb56d3f825cd4ad26354b672f8bfebe38). The original water-and-caries source, `docs/index.qmd`, and its saved HTML are preserved. The saved HTML contains the manuscript's reported AUC values; identifying those outputs is distinct from rerunning the R analysis.

The original Stata file, `23.07.26_base_ens_5520_R_enviar.dta`, was recovered from Andrea Correa's email of 31 July 2023 and reconciled with the supplied SPSS release by survey identifier. All 5,520 Stata participants match the SPSS oral-examination sample; the caries outcome, decayed- and remaining-teeth counts, decoded education and indigenous identification agree. Twenty-five of the 27 predictor constructs agree after label decoding. The two beverage-frequency differences reflect the original conversion of monthly frequency using four weeks (28 days), compared with 30 days in the primary Python analysis. A separate association sensitivity retains the original conversion.

This is a **Python reanalysis with verified original data and an audited R reference**. It deliberately changes the original model development and evaluation methods rather than claiming identical `caret` results. The source audit also confirms that the R water expression converts labelled values to factor indices; the Python analysis retains the physical reported glasses/day values. The supplementary SPSS release supplies survey weights, PSUs and additional water-context variables absent from the original Stata file. File hashes and aggregate reconciliation checks are recorded with each run.

## Read the report

- `docs/caries_python.html`: standalone English report generated from a completed Python run.
- `docs/caries_python.qmd`: Python-executed Quarto source.
- `docs/caries-analysis-plan.md`: analysis decisions, the internal-grid amendment and the dated correction of reporting focus.
- `docs/methodological-sources.md`: source verification and outstanding survey-documentation questions.
- `docs/r-to-python-migration.md`: comparison of the original caries R workflow and the Python reanalysis, including verified source reconciliation and intentional methodological changes.
- `docs/index.qmd` and `docs/index.html`: preserved original R source and saved reference results.
- `outputs/tables/`: aggregate results and reproducibility manifests.
- `outputs/tables/importance_stability.csv`: all 27 raw-variable contributions and fold stability in both model families.
- `outputs/tables/grouped_importance_stability.csv` and `predictor_contributions_manifest.json`: joint contributions of conceptual blocks and checks against the existing models.
- `outputs/tables/source_reconciliation.json`, `source_comparison.csv` and `legacy_water_coding.csv`: source hashes, agreement checks and the original water-index mapping.
- `outputs/figures/`: publication figures in PNG at 300 dpi and SVG.

## Reproduce

Requires Python 3.12 or newer and Quarto with Jupyter support. The verified local run used Python 3.14 and Quarto 1.9.35. Place the supplementary SPSS release at `data/data.sav` and the recovered Stata file at `data/original/23.07.26_base_ens_5520_R_enviar.dta`; both are excluded from Git. The pipeline requires the original file for source reconciliation. The commands below reproduce the verified-source Python reanalysis; they do not rerun the original R model fits.

```bash
python3 -m venv .venv
.venv/bin/python -m pip install -r requirements-lock.txt
.venv/bin/python -m pip install -e .
.venv/bin/python -m pytest -q
.venv/bin/python scripts/run_caries.py --jobs 3 --bootstrap 1000
.venv/bin/python scripts/render_caries.py
```

`requirements-lock.txt` records the verified macOS environment. On another platform, install the declared dependencies with `pip install -e '.[report,test]'` and preserve a fresh environment manifest. Quarto is installed separately.

The pipeline also supports `--stage prepare`, `--stage models`, and `--stage summarize`. The summary stage includes joint predictor-block permutations using the selected random-forest and spline models; refitted held-out probabilities must reproduce the saved predictions within numerical tolerance. Nested model/fold fits resume from a cache only if input fields, feature specifications, software versions, analysis settings and model source match. Summarization rejects stale model results. A failed or unfinished run must not be rendered as a completed report.

## Scientific scope

The primary predictive cohort includes all 5,036 dentate participants with observed outcomes; predictor imputation is learned within training folds. The reconciled education-and-water-complete manuscript subset contains 4,994 participants and 2,664 caries cases. It is retained as a sensitivity evaluation of existing held-out predictions, rather than a separately developed complete-case model. The report distinguishes concurrent classification, adjusted associations and future prognosis. It does not infer a preventive effect of water from variable importance.

The primary presentation includes all individual permutation importances, rank stability across folds, and joint permutation of sociodemographic, remaining-teeth, food-intake, label-related, beverage and cooking-fat blocks. A block is permuted jointly; its importance is neither the sum of its individual variables nor a performance change after redeveloping a model without it. Contributions depend on the fitted classifier and correlated predictors, and have no causal or directional interpretation.

Survey-weighted descriptions, shared PSU-disjoint nested validation, probability calibration and paired model comparisons provide the evaluation context. Existing water follow-ups and design-based association sensitivities remain secondary. The reporting focus was corrected on 10 September 2026 after model results were available; joint block analyses are a documented exploratory addition. The corrected F1/F2 weight is a working choice pending official phase-selection confirmation, and singleton-stratum conventions differ between analytic variance estimation and conditional bootstrap intervals.

## Data boundaries

Participant records, recovered source attachments, fold assignments, out-of-fold probabilities and caches remain under ignored `data/` and `outputs/private/` directories. The HTML embeds aggregate figures and tables. Only aggregate outputs belong in version control. Email addresses, message contents and private storage locations are not needed in the public provenance record. Do not load a pickle supplied by an untrusted party; the pipeline's private pickle is produced locally by its own model stage.

## Code map

| Module | Responsibility |
| --- | --- |
| `src/ens_analysis/data.py` | Metadata-aware variable coding, eligibility, missingness and source audit |
| `src/ens_analysis/survey.py` | Stratified PSU linearization and domain-aware GLM inference |
| `src/ens_analysis/models.py` | Fold-contained preprocessing, grouped tuning/calibration and model comparisons |
| `src/ens_analysis/evaluation.py` | Held-out metrics, conditional cluster bootstrap and paired comparisons |
| `src/ens_analysis/inference.py` | Water associations, adjusted curves and sensitivity analyses |
| `src/ens_analysis/figures.py` | Aggregate scientific figures |
| `scripts/run_caries.py` | Reproducible orchestration and result manifests |
| `scripts/render_caries.py` | Verified-environment report rendering |
