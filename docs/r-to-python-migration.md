# Original R analysis to Python: migration map and verification status

Status as of 10 September 2026: **the original caries repository and Andrea Correa's attachment have been identified and the datasets compared by participant ID. The Python work is a revised reanalysis, not an exact numerical reproduction of the historical R results.** Source agreement is now documented; remaining method differences, including the original water encoding and beverage conversion, remain explicit.

## 1. Source identity and current limits

The original repository is [AmaruSimonAgueroJimenez/ENS-dental-caries](https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries), inspected at commit `77c107feb56d3f825cd4ad26354b672f8bfebe38`, dated 24 September 2024. Its `docs/index.qmd` concerns sociodemographic/dietary predictors of caries and names Andrea Correa Ramírez and Amaru Simón Agüero Jiménez. This is distinct from the sibling `ENS` project, whose existing R report concerns metabolic syndrome and oral health.

The Python analysis was initially developed in the sibling project while the original caries source was unavailable, then transferred into this repository. The metabolic-syndrome R report is not the historical source of this caries migration.

| Item | Status | What has and has not been established |
|---|---|---|
| Original caries repository and R report | Identified and read | Commit and source contents verified locally. |
| Historical rendered report | Identified and read | Its stored numerical results correspond to the manuscript's approximately 0.623 RF and 0.590 logistic AUCs. This is inspection of a saved artifact, not a new R execution. |
| Andrea's source-file identity | Identified | A 31 July 2023 email was located with attachment name `23.07.26_base_ens_5520_R_enviar.dta`. Both exploratory R scripts reference that exact filename. |
| Original DTA contents and checksum | Retrieved and verified | `data/original/23.07.26_base_ens_5520_R_enviar.dta`, SHA-256 `754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966`; 5,520 unique participant IDs. |
| Current Python input | Verified as the working SAV only | `data/data.sav`, SHA-256 `9873b421d446190070bfff6495321fb152485c47457692936dbcd18c542eb89a`. |
| DTA versus SAV agreement | Verified with specified transformation differences | DTA IDs exactly match the 5,520 examined SAV participants. Outcomes and 25 of 27 harmonized predictor constructs agree; monthly beverage conversion differs. Row order differs, and the R water-factor transformation is a further implementation difference. |
| Python results | Computed for the reconciled SAV representation under a revised method | This does not establish exact numerical reproduction of the original R models; an additional association sensitivity uses the original beverage conversion. |

The historical Quarto report reads `data/data.dta` rather than the attachment's original filename. The two exploratory scripts identify the recovered attachment directly. The exact bytes of the historical renamed `data/data.dta` are not independently available; the recovered attachment, aligned participant records and stored-result checkpoints provide the documented reconstruction source rather than an unsupported filename-equivalence assertion.

### Completed source comparison

The recovered DTA has exactly the same ID set as the SAV examination phase, but **a different row order**. Outcomes, upper/lower decay and remaining-tooth counts agree after matching by ID. Both yield 5,036 dentate participants and the same 4,994-person education/water-complete cohort with 2,664 caries cases.

The original `nedu` uses 0 = high, 1 = intermediate and 2 = low; `3 - nedu` matches the SAV's MINSAL 1/2/3 categories. Original derived `indigena` 0/1 agrees with the SAV collapse of indigenous categories 1–9 versus category 10. The original raw `c6` instead uses 0 for no indigenous affiliation; its raw code must not be fed unchanged to the SAV category map.

All 27 constructs were compared in common physical/category units with harmonized structural skips. **Twenty-five agree**, including physical water glasses/day. The two beverage-frequency differences are fully explained by month conversion: the original weekly fields use monthly quantity divided by four, equivalent to **quantity/28 times per day**; Python's primary convention uses **quantity/30**. The difference affects 520 dentate participants for soda and 286 for juice, with a maximum absolute difference of approximately 0.071429 times/day. Original quantity/unit fields themselves agree. Thus these discrepancies are a documented convention difference, not different survey responses.

The source-reconciliation module exports aggregate comparisons and the original water coding map; retain these alongside the run manifest. All participant-level matching and the original health-data attachment remain private.

Original source fingerprints:

| File at commit `77c107f` | SHA-256 |
|---|---|
| `docs/index.qmd` | `3b27041247da0f279f8a04d2f8cbd13038d42fdd8663335c401929668a03d856` |
| `docs/ENScaries (2).R` | `a2bdfe697f455c95f156aa0cdeade47e1e60b0773cf9535ff4341c61aa2a0228` |
| `docs/RF con componentes principales.R` | `44e4a99c2a27f6bbe4618544362f3232d324b4c03de86c7169c73a34e8ea2dce` |

## 2. Which R workflow corresponds to the manuscript?

The clearest manuscript-level reference is [`docs/index.qmd` at the identified commit](https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries/blob/77c107feb56d3f825cd4ad26354b672f8bfebe38/docs/index.qmd). Its principal steps are:

1. Read the DTA and select one outcome plus 27 predictors: 19 dietary constructs, seven sociodemographic variables, and remaining teeth (lines 43 and 59–88).
2. Code `cariesnt` as caries-free/caries, convert labeled fields to factors, exclude `remanentes <= 0`, replace selected dietary missing values, convert water, and drop all remaining incomplete rows (89–100).
3. Make a single outcome-stratified 80/20 participant split using `set.seed(123)` and `createDataPartition` (140–143).
4. Train five algorithms within the 80% subset using `caret::train`: GLM logistic regression, RF, radial SVM, rpart decision tree and KNN. The training control is 10-fold cross-validation and the selection metric is ROC AUC (146–162).
5. Evaluate the resulting fitted models on the reserved 20% test set (164–176 and 199–217).
6. Train again on the same 80% subset using `trainControl(method="boot", number=100)` and evaluate on the **same** reserved test set (362–420).
7. Display logistic coefficients and `caret::varImp(rf_model)` from the final fitted model in each workflow (332–354 and 555–574).

Therefore the reported “CV” and “bootstrap” AUCs are **test-set AUCs after two different training-resampling procedures**. They are not averages of outer-fold AUCs, bootstrap distributions of the test AUC, or two independent validation samples. Agreement between them does not provide independent evidence of generalization.

The other two R files are exploratory analyses, including conditional trees, alternative covariate sets, macrozones/age groups, 90/10 splits, KNN and component-based models. They must not be silently combined into the manuscript workflow. They contain state-dependent/inconsistent object references and subsetting operations that need a separate execution audit if those analyses are to be recovered. For example, the component script creates `training.ids2` but references `training.ids`, computes `cp2` but uses `cp`, and later uses `subset(dat3, split = "TRUE")` / `split = "FALSE"` rather than a row-selection expression based on `split2`. The exploratory KNN sections also scale training and test data independently. None of these operations is carried into the new primary pipeline.

## 3. Exact variable correspondence to the current reconstruction

This is a **source-field and harmonization map**. Participant-level comparison has verified the relationships below, with the beverage and R-water-transformation differences described separately. Matching harmonized constructs does not mean the final R and Python model matrices are identical.

| Original R field | Python field | Current SAV source / relationship to verify |
|---|---|---|
| `cariesnt` | `caries` | Reconstructed as `m5p5 + m5p8 > 0`, preserving missing outcome counts; agrees with original derived `cariesnt`. |
| `Edad` | `age` | `Edad`; aligned values agree. |
| `sexo` | `sex` | `Sexo`, recoded to Male/Female. |
| `zona` | `rural` | `Zona`, recoded to Urban/Rural. |
| `nedu` | `education` | Original 0 high / 1 intermediate / 2 low maps exactly to `NEDU1_MINSAL_1 = 3 - nedu`. |
| `region` | `region` | `Region`; preserve the source code-to-region mapping, including its 2016 boundaries. |
| `indigena` | `indigenous` | Original 0/1 agrees with the SAV collapse of `c6`: 1–9 Yes, 10 No. |
| `c1` | `born_chile` | `c1`, Born in Chile Yes/No. |
| `remanentes` | `teeth_remaining` | `m5p3 + m5p6`; aligned totals agree with the original derived field. |
| `die1a` | `fish` | Fish-consumption category from `die1a`. |
| `die2` | `dairy_frequency` | Dairy-consumption category from `die2`. |
| `die3` | `dairy_type` | `die3`, with a no-dairy category only when `die2` indicates never consuming dairy. |
| `die4` | `wholegrain` | Wholegrain-consumption category from `die4`. |
| `die5` | `legumes` | Legume-consumption category from `die5`. |
| `die6` | `fruit_days` | Numeric days/week from `die6`. |
| `die7` | `fruit_portions` | Numeric portions; structural zero only if `die6 == 0`. |
| `die8` | `vegetable_days` | Numeric days/week from `die8`. |
| `die9` | `vegetable_portions` | Numeric portions; structural zero only if `die8 == 0`. |
| `die10_a` | `label_ingredients` | Frequency category from `die10_a`. |
| `die10_b` | `label_nutrition` | Frequency category from `die10_b`. |
| `die10_c` | `label_warnings` | Frequency category from `die10_c`. |
| `die10_d` | `label_health` | Frequency category from `die10_d`; included in the canonical QMD even where exploratory formulas omit it. |
| `die10_e` | `label_brand` | Frequency category from `die10_e`. |
| `die10_f` | `label_discounts` | Frequency category from `die10_f`. |
| `die11` | `water` | Raw quantitative glasses/day, not factor indices; see the scale warning below. |
| `die14` | `oil` | Cooking oil/fat category from `die14`. |
| `frecsembebida` | `soda_frequency` | Original weekly frequency divided by seven agrees with daily/weekly reports; monthly reports use quantity/28, versus quantity/30 in the primary reconstruction. |
| `frecsemjugo` | `juice_frequency` | Same verified conversion difference for juice; both versions are put in **times/day** before sensitivity analysis. |

Python additionally retains `IdEncuesta` as an internal identifier, `Estrato`, `Conglomerado`, and the supplied F1/F2 expansion weight. These are not predictors. The exploratory extension adds household water supply (`as29`), recent rural/well/spring water use (`m8p13`) and recency of dental attendance (`sb2`); these are not part of the original 27-predictor comparison.

For beverage conversion, Python uses day = 1, week = 7 days and month = 30 days, and explicitly codes recorded nonconsumption as zero. The additional `Original 28-day beverage conversion` association model uses the original weekly values divided by seven, matched by participant ID. The seven existing association models and predictive-model inputs retain their declared 30-day convention; this sensitivity is not a covert change of the primary analysis.

### Water scale and missing values require special attention

The original QMD converts labeled variables to factors and then executes `die11 = as.numeric(droplevels(die11))`. On a factor, this returns **level indices**, not the numerical quantities represented by its labels. The recovered DTA confirms numerical value labels and unused levels. After excluding edentulous participants and before the final complete-case removal, the observed mapping is:

| Reported glasses/day | R factor-derived numeric value |
|---|---|
| 0 through 16 | 1 through 17, respectively |
| 18 | 18 |
| 20 | 19 |
| 24 | 20 |
| 25 | 21 |
| 28 | 22 |
| 30 | 23 |
| 32 | 24 |
| 40 | 25 |

This is a confirmed change of scale. A coefficient for the encoded R field is not automatically an effect per additional physical glass/day. Physical water responses themselves agree exactly between DTA and SAV.

Python retains numerical water values and treats the questionnaire's unquantified “other value” response as missing. This is a deliberate measurement choice, not a claim of byte-for-byte compatibility. A monotone recoding can leave some tree splits equivalent while changing linear, distance-based or spline analyses; its impact cannot be inferred only from matching sample counts.

The R source turns **all** missing `die3` into no dairy and **all** missing `die7`/`die9` into zero. Python makes these replacements only for documented nonconsumers using the preceding question. **These alternative rules do not change observations in the verified source files:** in the full SAV, the 508 missing dairy-type responses, 424 missing fruit portions and 156 missing vegetable portions are exactly the corresponding nonconsumer groups; there are no additional item-missing responses in these fields. The DTA subset agrees. The conditional Python rules provide a safeguard for future inputs, not a demonstrated numerical correction in this dataset.

## 4. Methods that intentionally differ

| Component | Original canonical R workflow | Current Python workflow and consequence |
|---|---|---|
| Input | `data/data.dta`, containing derived fields | The SAV representation has now been reconciled by ID with Andrea's recovered DTA, with specified transformation differences. |
| Target population | Dentate participants after preprocessing and complete-case deletion | Predictive cohort: 5,036 dentate participants with known outcomes; the verified original 4,994-person complete subset is identified separately. |
| Missing predictors | Selected blanket replacements, then global `drop_na()` before splitting | Structural replacements only when supported by skip logic; numeric median imputation, missing indicators and categorical missing category learned within training folds. Outcome is never imputed. |
| Categorical representation | Labeled-to-factor conversion followed by the caret formula interface | Explicit categorical maps and training-fold one-hot encoding with unknown-category handling. Factor coding/model matrices must be checked separately for numerical reproduction. |
| Validation unit | One participant-level 80/20 split; no PSU grouping is supplied | Five shared outer folds grouped by PSU and three grouped inner folds. Each eligible participant receives one out-of-fold probability per model. |
| Resampling indices | No explicit fixed `index` list supplied to `trainControl` | Shared prespecified outer and inner splits across models. A single R seed is not an exact cross-language split specification. |
| Model selection | ROC AUC; engine/tuning defaults not pinned by an explicit grid | Survey-weighted inner-fold log loss using documented candidate grids. This changes the model-selection objective. |
| Survey design | No survey weights, strata or PSU identifiers enter the selected modeling frame | Weighted fitting/evaluation, original strata and PSUs retained for uncertainty. KNN remains an explicitly unweighted fitting benchmark. |
| Logistic prediction | Unpenalized `glm` | L2-regularized sklearn logistic regression, plus a separate spline-logistic alternative. Different estimators; not an identical coefficient fit. |
| Random forest | caret RF with package defaults and no explicit tree count/grid in the QMD | sklearn RF with 200 trees and tuning over leaf size and feature subsampling. Engine and regularization differ. |
| SVM | caret `svmRadial`, probability output handled by that engine | sklearn radial SVC with training-only grouped out-of-fold probability calibration. Do not assume the same calibration or scale defaults. |
| Decision tree | caret/rpart | sklearn decision tree with explicit depth/leaf-size grid; not the same pruning algorithm. |
| KNN | caret KNN with its historical defaults | Distance-weighted sklearn KNN, explicitly scaled in training folds and tuned over 25/75/150/300 neighbors. |
| Added algorithms/feature sets | Five full-feature algorithms | Seven algorithms plus six planned reduced/extended feature comparisons; 13 fitted model specifications in total, plus a derived prevalence-only benchmark. |
| Evaluation | Predictions on the same reserved 20% after CV-based or bootstrap-based training | Pooled outer out-of-fold probabilities from the shared PSU partitions; no separately reserved 20% test set. |
| Decision threshold | Class predictions supplied by the caret estimator | Explicit probability threshold 0.5 for confusion metrics; no threshold chosen on evaluation outcomes. |
| Uncertainty | `pROC::ci.auc` on the test set and simple Wald proportion intervals | Shared 1,000-replicate rescaled stratified PSU bootstrap for AUC/Brier/log-loss and paired differences. Fits and fold assignments remain fixed: intervals are conditional, not full redevelopment intervals. Singleton PSU fixed in these metric replicates. |
| Importance | `caret::varImp` on the final RF, with defaults | Group each original predictor and permute it in held-out folds; report weighted Brier increases/AUC decreases and stability over five repeats per fold. Scores are not on the original importance scale. |
| Direction/association | Ordinary logistic coefficients of the training-set full-feature GLM | Separate survey-adjusted association models with flexible age/teeth/drink adjustment, water spline, 6-versus-2-glass OR and sensitivities. These do not establish causal protection. |
| Association uncertainty | Ordinary model-based Wald intervals | PSU score-sandwich covariance with design t degrees of freedom; explicit singleton-average adjustment. Curve intervals condition on the observed covariate composition. |
| Calibration | Not evaluated in the canonical QMD | Out-of-fold calibration intercept/slope, reliability curves, Brier score and log loss; nonidentifiable calibration fits are flagged. |
| Reproducibility | Package installation at render time, no pinned environment or saved split identities in the QMD | Pinned environment, source/data hashes, deterministic settings, split checks, cache fingerprints and stage-aware manifests. Private records/predictions stay outside version control. |

These changes mean that differences between the historical and current AUCs cannot be interpreted as proof that Python performs better or worse than R. Input verification, the estimand, validation design, weights, preprocessing and tuning must be separated before interpreting a numerical difference.

The working F1/F2 weight equals the corrected F2 weight in the current SAV. Examination-phase coverage supports it as a working choice, but definitive confirmation against the official phase-selection documentation remains outstanding. The existence of weights in a new analysis does not retrospectively turn the original results into weighted estimates.

## 5. Confirmed reporting/implementation issues in the original QMD

1. **ROC plot coordinates are mislabeled.** Lines 220–230 use a sequence called FPR as the input to `coords(..., input="specificity")`, then plot it on an axis labeled `1 - Specificity`. The plotted horizontal coordinate is specificity, unless transformed. The separately computed numeric `auc(roc_curve)` is not invalidated by this plotting mistake. The bootstrap plotting section repeats the same construction.
2. **The standard-metric CIs use the wrong common denominator.** Lines 245–271 use the full test-set `n` for sensitivity, specificity and precision as well as accuracy. Sensitivity's denominator is actual positives, specificity's is actual negatives, and PPV's is predicted positives. They are not all full-test-size binomial proportions; complex-survey uncertainty would require further design treatment.
3. **Text definitions are incorrect even where the software metric is correctly extracted.** Lines 121–125 describe sensitivity/specificity denominators inaccurately and define precision as overall accuracy. The code at 256–259 extracts sensitivity, specificity, precision and accuracy separately. Do not equate the prose mistake with a demonstrated error in all point estimates.
4. **Resampling and test evaluation are easy to confuse.** Both result sections evaluate the same test participants; the bootstrap training control does not create bootstrap CIs for the final test metrics. This distinction explains why, for example, ordinary logistic performance can be identical in the two sections.
5. **The RF importance definition is insufficiently specified.** The QMD calls `varImp(rf_model)` without documenting the historical caret/randomForest version or explicitly defining the underlying importance calculation and scaling. The RF training call does not explicitly request `importance=TRUE`. Inspect the fitted object or historical engine implementation before labeling every displayed score “mean decrease in accuracy”; this audit does not assert an unverified default importance type. The new held-out Brier permutation importance is deliberately a different quantity.
6. **Class direction must be explicit when reproducing resampling summaries.** The original factor puts `libre_de_caries` first, while final confusion matrices explicitly specify positive = `caries`. Verify event conventions of the historical `twoClassSummary` output. Reversing both outcome and its complementary probability leaves binary AUC unchanged; this is not, by itself, evidence of an erroneous AUC.

The stored HTML reports RF AUC `0.6227129` (CI `0.5882290–0.6571968`) and logistic AUC `0.5901005` (CI `0.5549954–0.6252056`) in its first section. Its displayed logistic coefficient for encoded `die11` has OR `1.0025649`, CI `0.9809868–1.0246177`, p `0.8175015`. These are useful historical checkpoints, not verified per-glass effects or newly reproduced estimates. An RF importance ranking does not override the need to establish the direction and uncertainty of an association.

## 6. Python module map and remaining verification work

| Original component | Python location | Verification boundary |
|---|---|---|
| DTA loading, field selection, recodes and complete cases | `src/ens_analysis/data.py` and the source-reconciliation module | Retain the documented ID-based comparison and fail on unexplained disagreement; preserve original filename/checksum. |
| caret training, resampling and prediction | `src/ens_analysis/models.py` | Current implementation is the revised nested analysis. A historical-compatibility run must explicitly freeze the original cohort, transforms, split and engine settings. |
| Test ROC/confusion metrics and comparisons | `src/ens_analysis/evaluation.py` | Event = caries, correct denominators, unmodified ranking probabilities, explicit conditional paired CIs. |
| Descriptive and association inference | `src/ens_analysis/survey.py`, `src/ens_analysis/inference.py` | New design-based analyses; distinct from the original training-set GLM. |
| Figures/tables/rendering | `src/ens_analysis/figures.py`, `docs/caries_python.qmd` | Preserve `docs/index.qmd`/historical HTML as original artifacts; label rebuilt outputs and method changes. |
| Run orchestration and provenance | `scripts/run_caries.py` | Invalidate previous completion when starting any stage; regenerate current associations when summarizing; record source and input hashes. |

Completed: the identified attachment has been retrieved and hashed; IDs, outcome definitions, cohort membership, category mappings and all 27 harmonized constructs have been compared; the water-factor mapping and structural-missingness rules have been recovered. Keep these aggregate checks and the identified email provenance with the analysis.

Before claiming **exact numerical reproduction** of the historical model results:

1. Preserve a historical-compatibility preprocessing path, including factor-index water, original beverage conversion, complete cases and DTA row order. Label the revised physical-water analysis separately.
2. Recover the actual original test assignments and package/model settings where possible. Reusing `seed=123` in Python does not reproduce R's random partition, and sorting the same participants differently also changes a seed-based split. If runtime state or versions are missing, state the resulting limit.
3. Compare historical-compatibility outputs with the stored HTML checkpoints. The current grouped, weighted, regularized Python analysis is not such a compatibility run, even though its underlying survey records have been verified.
4. Keep revised and compatibility outputs in distinct namespaces, and report the original-conversion sensitivity alongside the unchanged primary analysis. Retain source-validation and method-status fields in each new manifest.

Current conclusion: **correct repository and source attachment recovered; survey records reconciled; two beverage-conversion differences and the R water-factor encoding documented; revised Python analysis implemented. Exact historical numerical reproduction has not been claimed or established.**
