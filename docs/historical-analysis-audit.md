# Historical water coding and random-forest figure audit

Audit date: 10 September 2026. This note updates the historical account using newly recovered source files, written analytical correspondence, the original importance spreadsheet, RStudio caches and the Word chart. It does not change the completed Python models, their results or their validation. Private correspondence, participant records and model caches are not distributed with this note. Historical requests are evidence of earlier decisions, not instructions to execute additional analyses.

## What is directly established

The 11 values in the original Word importance chart match the first 11 rows of the recovered `rf_importance.xlsx` exactly, including their order. The largest values are age, 100; remaining teeth, 77.9155633166657; and water, 53.981911139722. Water was therefore genuinely third in that saved ranking. These are rescaled model-importance scores, not percentages of disease explained or evidence of a protective effect.

The spreadsheet contains **90 encoded terms representing 27 original predictors**: seven single numeric columns and 83 indicator columns from 20 categorical predictors. All seven numeric columns occupy the first seven positions. Water occurs once as `die11`; `edad_cat` is absent. The plot displays seven numeric terms and four individual category indicators, rather than importance calculated once for each whole original predictor. Its caption says ten variables, but the chart contains eleven bars.

One label is mistranslated: the ninth plotted term is `die5al_menos_una_vez_por_semana`, meaning **legumes at least once per week**. The original chart instead says “less than once per week”. The current full-predictor figures do not reuse that category-specific label.

## Recovered chronology

| Date in 2024 | Documented evidence | Limited conclusion |
|---|---|---|
| March and 10 September | Earlier analytical discussions refer to Gini importance. | This describes the earlier stage, not the final agreed terminology. |
| 24 September | The water field is identified as categorical with too many indicator terms; conversion to numeric is requested and accepted. Duplicate categorical and continuous age representations are also identified. | The historical water representation did change; more than one feature decision changed during this revision. |
| 25 September | Updated analyses and tables are reported; remaining teeth are discussed and retained as a predictor. | These are concurrent development decisions, not a controlled water-only comparison. |
| 2 October | Mean decrease in accuracy is proposed as the methodological description and the exact importance spreadsheet is shared. | This identifies the later intended metric and the saved numerical result. It does not alone establish its computational implementation. |
| 26 October | Local Git commit `b0b24eaf86af0f258dc5c194cdf155ac879c66cd` includes the identical spreadsheet and an export-enabled QMD. | The export follows the second `varImp(rf_model)` call. This strengthens the source-to-output link. |
| 15 November | The later discussion explicitly confirms mean decrease in accuracy and rejects Gini as the intended description of the figure. | The earlier account that described the historical agreement simply as Gini is superseded by this fuller chronology. |
| 18 November | Shared preprocessing code converts labelled values to factors, then uses `as.numeric(droplevels(die11))`. | This produces retained factor-level indices, not the original physical quantities. |
| 22 November | Water bands written as `<3`, `3–6` and `6>` are proposed for an interaction with residential context. The response rejects the interaction and requests separate analyses for proposed fluoridation groups. | Bands were proposed, with ambiguous endpoint notation, but no implementation or output was found. They were proposed after the October chart data. The proposed contextual grouping is not a measured individual fluoride exposure. |

## Intended metric versus implementation evidence

The final saved QMD uses `caret::train(method="rf")` without `importance=TRUE`, followed by `varImp(rf_model)`. Inspection of the archived **caret 6.0-94** adapter confirms that it does not enable permutation importance automatically. Under the documented randomForest defaults, classification fitting with `importance=FALSE` still supplies Mean Decrease Gini. The caret fallback converts the available column to `Overall`; `varImp.train` then rescales by subtracting the minimum and dividing by the resulting maximum, producing a 0–100 scale. [Archived caret package](https://cran.r-project.org/src/contrib/Archive/caret/caret_6.0-94.tar.gz), [randomForest fitting documentation](https://search.r-project.org/CRAN/refmans/randomForest/html/randomForest.html).

A recovered `varImp.train` object identifies model `rf`, has one `Overall` column and contains the same 90 scores as the spreadsheet. This is strong evidence consistent with **rescaled Gini importance under the preserved code and package defaults**, despite the later intention and figure description referring to accuracy. However, the fitted `rf_model` and its raw importance matrix were not recovered from the inspected caches or Git history. The exact runtime call cannot therefore be certified, and an undocumented execution change cannot be ruled out.

Earlier standalone scripts do set `importance=TRUE`, but fit a different object, `classifier_RF`, using a different feature representation and workflow. Those calls do not establish the metric used for the matched caret export. Permutation accuracy importance and impurity-based importance are distinct measures. [randomForest importance documentation](https://search.r-project.org/CRAN/refmans/randomForest/html/importance.html).

## Why the revised analysis does not reproduce water's original prominence

The historical graph and the revised graph answer different questions. The former ranks rescaled fitted-model scores for encoded terms. The revised analysis permutes each of 27 whole original predictors in held-out survey clusters and measures the unscaled change in weighted Brier score, with fold stability. Sample selection, weighting, validation, preprocessing, tuning and software also differ.

Impurity importance can favour numeric or high-cardinality features; splitting categorical predictors into separate indicators also changes the unit being ranked. These are plausible contributors to the original pattern, **not a demonstrated decomposition of why water ranked third in that particular forest**. Summing category scores would not recover the revised whole-predictor permutation measure. [Official illustration of impurity and permutation importance](https://scikit-learn.org/stable/auto_examples/inspection/plot_permutation_importance.html).

The completed controlled sensitivity provides a narrower empirical result: the current random forest gives effectively identical probabilities for physical glasses/day and the exact historical level indices (maximum absolute difference 3.33 × 10⁻¹⁶). Thus that index conversion alone does not explain the difference in water prominence in the current fitted forest. This is not an exact historical refit or proof that coding never matters. The nominal encoding comparison and spline comparisons remain in the report and Table S12; none of the four paired AUC intervals excludes zero, without establishing equivalence.

## Provenance and limits

- The newly recovered Stata attachment has SHA-256 `754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966`, identical to the original input already reconciled in the reanalysis.
- The spreadsheet shared in October, the recovered local copy and the version in local commit `b0b24ea` have SHA-256 `6bae69fc8343861e1d10ca38d2aeac7e4651e9ff1f4da1353265a3cdf3cf04ec`.
- The final recovered QMD is text-identical to the retained original after line-ending normalization. The second QMD adds Excel export, without a new water transformation or importance setting.
- The caret 6.0-94 archive inspected has SHA-256 `2715e83ca260bb739cd926a55b0d2da1e3f6308b17b56862466e738d930d29a8`.
- The audit covered written correspondence, relevant screenshots, R/QMD sources, histories, source buffers, 18 serialized cache artifacts and seven local Git commits. Audio and video attachments were not transcribed. Absence of a fitted object or implemented subgroup analysis is a finding within that inspected scope.

The updated author package includes a Spanish explanatory addendum. The principal results, five main composite figures and three main tables remain unchanged. A subsequent implementation adds a matched historical-coding comparison, Table S13 and Figure S9; see the completed report for its numerical results. Water remains visible as a secondary predictor without an established independent predictive gain or causal interpretation.

## Follow-up implementation

The complete historical encoding is implemented in Python, including water level indices, fruit/vegetable days as factors, weekly beverage frequencies using four-week months and the original treatment contrasts. The 4,994 by 90 Python matrix was checked against an independent R preprocessing export; the corresponding R data frame also matched the recovered 2024 cache. Two random forests are newly fitted in these same complete cases, using the inherited five PSU folds, survey weights and previously selected outer-training parameters. This representation-package sensitivity adds whole-predictor and encoded-column importance comparisons without claiming to recover the missing historical fitted forest. The earlier water-only sensitivity remains a distinct analysis.
