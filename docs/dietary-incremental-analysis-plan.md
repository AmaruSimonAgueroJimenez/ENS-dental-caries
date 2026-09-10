# Incremental contribution of dietary information to prevalent caries classification

**Analysis specification frozen on 10 September 2026, before fitting the dedicated expanded models.** This is a new, dataset-informed analysis: the cohort, questionnaire distributions and results of earlier analyses were already known when this question was formulated. It is not a prospectively preregistered study. Any subsequent analytical change must be dated, justified and distinguished from this specification. The corrected Andrea Correa manuscript and its historical-method comparison remain separate, unchanged deliverables.

## Question, scope and primary estimand

Among dentate ENS 2016–2017 participants, how much does the available dietary questionnaire improve classification of concurrent, clinically observed cavitated caries beyond demographic, dental, socioeconomic, health and care-access information? A second question asks how much dietary information adds when current oral symptoms and self-rated oral health are also available. Food-label behaviours are evaluated separately from direct consumption.

The **primary comparison** is penalized spline logistic regression using context plus direct diet (C17+D15) versus the same model family using context alone (C17). The **primary metric is Brier gain**, defined as Brier(C17) minus Brier(C17+D15), calculated from paired out-of-fold probabilities and survey weights. Positive values indicate improvement. Complementary measures are AUC(C17+D15) minus AUC(C17) and log-loss(C17) minus log-loss(C17+D15). This positive-is-better convention applies to every refitted comparison; performance tables retain raw metric values. Absolute gains, paired 95% intervals and the performance of both comparators will be reported, without selecting the primary algorithm after inspecting the new results.

The target is concurrent classification, not prediction of future onset, an effect of changing diet, or a validated clinical decision rule. No minimum clinically useful gain is asserted without an external decision context. An interval containing zero is not proof of no contribution or equivalence. A small conditional gain also does not show that diet is etiologically unimportant: measurements may be coarse, correlated with context, or affected by current disease.

## Population, outcome and source provenance

Use the existing reconciled ENS cohort: 6,233 survey records, 5,520 complete oral examinations, 484 edentulous exclusions and **5,036 dentate participants, including 2,689 with caries**. Caries is at least one clinically observed decayed tooth, reconstructed from upper- and lower-jaw counts. Retain participants with missing predictors and impute inside training folds. Do not replace this cohort with the historical education-and-water-complete subset of 4,994.

The supplied SPSS release contains the expanded questionnaire variables. Its SHA-256 is `9873b421d446190070bfff6495321fb152485c47457692936dbcd18c542eb89a`. The verified original Stata examination file has SHA-256 `754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966`. Existing identifier-based reconciliation establishes matching examination participants and outcomes; the new variable dictionary must additionally retain each expanded field's source code, response labels, derivation and missingness. Public outputs contain aggregates and reproducible definitions, excluding participant records and individual probabilities.

## Predictor blocks and fixed model sets

Counts below refer to original predictor constructs, not the larger encoded design matrices. No predictor is screened using univariable P values, observed model importance or the new outer-test performance.

| Block | Constructs | Definition |
| --- | ---: | --- |
| B8: basic demographic/dental information | 8 | Age, sex, region, urban/rural residence, education, indigenous identification, born in Chile, remaining teeth. |
| C17: expanded context | 17 | B8 plus income band, health insurance, smoking status, self-reported diabetes, last dental visit, reason for that visit, dental-care access/barriers, household water source, and recent well/spring/rural water consumption. |
| S6: concurrent oral status | 6 | Self-rated oral health; oral pain; and impacts on speech, eating, daily activities and social interaction. |
| D15: direct dietary consumption | 15 | Fish, dairy frequency, dairy type, wholegrain, legumes, fruit days, fruit portions, vegetable days, vegetable portions, water, soda frequency, juice frequency, cooking oil/fat, soda glasses/day, juice glasses/day. |
| L6: food-label behaviours | 6 | Consideration/checking of ingredients, nutritional information, warnings, health claims, brand and discounts. |

The exact C17 fields are `age`, `sex`, `region`, `rural`, `education`, `indigenous`, `born_chile`, `teeth_remaining`, `income_band`, `health_insurance`, `smoking_status`, `diabetes_reported`, `dental_visit`, `dental_visit_reason`, `dental_access`, `household_water_source`, `rural_water_consumption`. S6 fields are `oral_health_selfrating`, `oral_pain`, `oral_speech`, `oral_eating`, `oral_daily_activities`, `oral_social`. D15 comprises the 13 original consumption constructs after removing all six `label_*` variables, plus `soda_glasses_daily` and `juice_glasses_daily`.

| Set | Predictor count | Role |
| --- | ---: | --- |
| B8 | 8 | Basic reference for the increment from expanded context. |
| C17 | 17 | Primary reference. |
| C17+D15 | 32 | Primary dietary extension. |
| C17+D15+L6 | 38 | Additional label-behaviour information. |
| C17+S6 | 23 | Symptom-aware reference. |
| C17+S6+D15 | 38 | Dietary extension conditional on concurrent oral status. |
| C17+S6+D15+L6 | 44 | Full symptom-aware set. |

Each set is fitted using spline logistic regression, random forest and histogram gradient boosting: **21 main variants**. Two additional spline sensitivity variants yield **23 fitted variants in total**. None is a previously fitted historical model relabelled as a new result.

Decayed-tooth counts, outcome components and DMFT are excluded from predictors. Remaining teeth describes the available dentition rather than deterministically defining current decay. Oral symptoms and self-rating are concurrent information that may be consequences of disease; they are not automatically a programming leak, but they change the intended information scenario. This is why S6 is separated from the primary comparison. C17 also contains potentially disease-responsive attendance information and is not a causal adjustment set. It already requires a tooth count, so this analysis cannot establish a wholly remote questionnaire-only screening tool. Reported diabetes is not laboratory-confirmed diabetes; laboratory-specific expansion weights are not invoked.

## Coding, missingness and extreme values

Keep meaningful questionnaire categories; income remains a band, without invented midpoints. Use categorical missing-response levels and one-hot encoding with unknown categories handled explicitly. Numeric predictors use training-fold medians and one missingness indicator per numeric construct. Category vocabularies, medians, scaling, spline knots and any sensitivity cutoff are learned only from the relevant training partition, repeated within every inner split and outer refit. Resolve structural nonconsumption only when the preceding response establishes it; do not turn ambiguous skips or unknown responses into zero.

Soda and juice frequencies are converted to occasions/day with seven days/week and 30 days/month. Each new volume is frequency/day multiplied by reported standard glasses/occasion; report **standard glasses/day**, not grams of sugar or millilitres without a verified conversion. The two volumes and the two frequencies are intentionally retained together, despite dependence. They are questionnaire-derived indicators, not nutrient estimates or independent measurements. Water remains reported glasses/day; neither quantity nor household supply measures individual fluoride exposure.

Apply `log1p` to four nonnegative variables in all main models: soda frequency, juice frequency, soda glasses/day and juice glasses/day. Keep valid zeros. No primary tail clipping is applied. The spline model retains cubic splines with three training-derived quantile knots for age, remaining teeth and water; other numeric inputs, including the transformed beverages, enter as continuous linear terms. These choices are fixed from prior data inspection rather than selected from the new performance results.

The two sensitivity models are spline C17+D15 and spline C17+S6+D15. For each of the four beverage variables, estimate the 99th percentile **among strictly positive training values**, cap higher values using that training cutoff, preserve zeros, and then apply `log1p` and the remaining preprocessing. Re-estimate cutoffs within inner training folds and outer-training refits; do not calculate them globally. Retune these variants within their own inner folds using the same fixed grid. Report paired gains over their matching references and paired differences from the corresponding uncapped variant. Any insufficient-positive-value fallback must be deterministic and recorded, not chosen using the outcome.

## Validation, training and frozen search space

Use the same five outer and three inner PSU-grouped, outcome-stratified folds across sets and algorithms, with seed **20260910**. Inherit the established outer assignments for the unchanged cohort; save and check inner assignments. No PSU may occur in both training and validation within a split. Every participant must receive exactly one held-out probability per variant. All preprocessing is inside the fitted pipeline; no imputation, knot selection, winsorization, rescaling or tuning uses held-out outcomes or predictor distributions.

All three families support survey-weighted fitting. Normalize training weights to mean one and select each candidate by survey-weighted inner-validation log loss on the same participants. Use the established fixed candidate grid below. Earlier grid development was informed by earlier analyses and is not described as independent prespecification; it is frozen for this new analysis. Retain all selected parameters, scores, diagnostics and boundary selections. A boundary optimum alone does not authorize expansion after seeing new outer performance.

| Family | Fixed candidates and settings |
| --- | --- |
| Penalized spline logistic | C = 0.0001, 0.001, 0.01, 0.1, 1, 10; L2 penalty; standardized continuous/spline terms; maximum 3,000 iterations. |
| Random forest | 200 trees; minimum leaf size = 5, 20, 50, 100 crossed with maximum features = square root or 0.7. |
| Histogram gradient boosting | Maximum leaves = 3, 7, 15 crossed with L2 regularization = 1, 10, 100; 150 iterations; early stopping disabled. |

Do not rebalance the outcome, choose a classification threshold using outer-test outcomes, replace failed fits silently, or select a final primary model from the best outer AUC. Convergence failures must stop the affected run with diagnostics. Cache validity must depend on cohort, feature set, code, configuration and folds; resumed fits must reproduce the same analysis.

## Evaluation, comparison hierarchy and uncertainty

Report survey-weighted Brier score, AUC and log loss, followed by calibration intercept and slope and descriptive calibration curves. Clip probabilities only for logarithms at 10^-7 and 1−10^-7, preserving unmodified probabilities for AUC and Brier score. Unweighted metrics are secondary diagnostics. If fixed 0.5 sensitivity, specificity, predictive values and accuracy are provided, label that threshold descriptive rather than clinically optimized.

The planned comparison hierarchy is:

1. **Primary:** spline C17+D15 versus C17, led by Brier gain.
2. **Dietary robustness across model families:** the same contrast in random forest and boosting.
3. **Diet beyond symptoms:** C17+S6+D15 versus C17+S6 in all three families.
4. **Additional label information:** C17+D15+L6 versus C17+D15, and C17+S6+D15+L6 versus C17+S6+D15, in all three families.
5. **Expanded context:** C17 versus B8 in all three families. The change from C17 to C17+S6 may be described separately as a concurrent-status contrast.
6. **Tail sensitivity:** the two capped spline variants as specified above.

Use **1,000 shared stratified PSU-bootstrap replicates** for paired 95% intervals, retaining the established rescaled survey-bootstrap implementation and full positive-weight design before analytical subsetting. The working expansion weight is `Fexp_F1F2p_Corr`, numerically equal to `Fexp_F2p_Corr` in this release; the original manual has not yet confirmed the choice. Preserve the supplied strata and PSU identifiers. Singleton strata remain fixed in this bootstrap; any descriptive Taylor-linearized estimates using an average-stratum singleton convention must disclose the different convention. Do not treat observed singleton membership as proof of certainty sampling.

These intervals are conditional on the fitted models, fixed folds, preprocessing and selected parameters; they do not include full redevelopment, repeated search or external-population uncertainty. Report how many replicates are valid. Secondary comparisons and importance analyses are exploratory, with no multiplicity-adjusted confirmatory claims. Do not interpret a selected significant secondary contrast as replacing the primary finding. Numerical discrimination, calibration and their uncertainty all matter; no new clinical utility or deployment claim is planned.

## Individual and joint contributions

Evaluate held-out permutation contributions in spline and random forest for C17+D15+L6 and C17+S6+D15+L6: four fitted-model/settings combinations. Permute original variables before preprocessing, with five repeats per outer fold and shared donor permutations for reproducible comparisons. All encoded columns and a construct's missingness indicator move together. Report weighted ΔBrier = perturbed minus original Brier and AUC decrease = original minus perturbed AUC; positive values indicate a contribution to the fitted model. Combine fold means using held-out weight totals and retain minimum/maximum fold means, fold ranks and stability. Fold ranges are not confidence intervals; negative values must remain visible.

Also permute the disjoint C17, D15 and L6 blocks jointly, adding S6 in the symptom-aware model. Use one shared row permutation for all variables in a block. Joint block importance is not the sum of individual importance and is not the same estimand as refitting with and without diet. Blocks differ in size. The strongly related beverage-frequency and volume variables particularly limit an isolated-variable attribution; whole-D15 permutation and the refitted dietary increment provide complementary information. These analyses describe model reliance, not causal contributions, protective effects, percentages of disease explained, or independent dietary effects.

## New manuscript and display allocation

Proposed title: **Does dietary information improve classification of prevalent dental caries? A survey-weighted machine-learning study in Chile.** A shorter alternative is **The incremental contribution of diet to prevalent dental caries classification in Chile**. Target approximately 3,500 main-text words and a concise structured abstract. The main narrative is the dietary increment, not a search for an algorithm with the largest AUC or a single favourable exposure.

Prepare a completely new Word proposal and supplement using only the new expanded-analysis outputs. Preserve the familiar Correa visual language through blue horizontal contribution bars, compact grouped comparisons, consistent model colours, white backgrounds and readable panel labels. No old performance table, historical-coding figure or previous importance ranking supplies a result in this new paper.

| Main item | Planned content |
| --- | --- |
| Table 1 | No caries / Caries / Total participant profile, with context, oral-status and key dietary quantities; unweighted counts, weighted summaries, observed denominators and missingness. Complete 44-construct distributions go in the new supplement. |
| Table 2 | All 21 main set/family combinations, with raw AUC, Brier and log-loss estimates and intervals, grouped by information set. |
| Table 3 | Prespecified primary and secondary increments: Brier gain, AUC gain and log-loss gain, paired intervals and explicitly marked comparison hierarchy. |
| Figure 1: four panels | Performance across the seven information sets and three families: AUC, Brier, log loss and calibration slope, using the new results only. |
| Figure 2: six panels | Dietary increment in three metrics, without and with S6: three metrics × two reference scenarios; all three model families, common positive-is-better convention and paired intervals. |
| Figure 3: four panels | All 15 direct dietary variables' signed held-out contributions, with and without S6, separately for spline and random forest; water is displayed in its declared questionnaire position. |
| Figure 4: four panels | Joint block contributions for spline and random forest: ΔBrier and AUC decrease, with the available C17/D15/L6/S6 blocks, block counts and fold ranges. |
| Figure 5: six panels | ROC and calibration of the primary spline comparisons in both information scenarios (four panels), then two panels for paired Brier/AUC tail-sensitivity contrasts. |

Figures may be condensed for legibility without changing analyses or selectively removing unfavourable results. Assemble each at no more than 180 × 223 mm, with four to six panels, and verify the final Word and separate exports visually. Embed the three editable tables and five composite figures in the author-review Word; retain separate editable tables and figure files for journal submission preparation.

Create a new supplement with: participant/design flow; the full source dictionary and derivations; all 44-construct distributions and missingness; fixed grids and selected parameters; fold and convergence checks; all 23 variant performances; calibration and descriptive threshold metrics; all paired contrasts; complete individual and block contributions with ranks; and beverage-tail diagnostics/cutoffs. Number its tables and figures afresh. The historical R audit and corrected Andrea document remain separately available, not recycled supplementary results. Add current cross-sectional STROBE and TRIPOD+AI reporting maps with honest item coverage.

## Journal fit and verified sources

**Caries Research, Research Article**, is a reasonable proposed target because the question concerns dietary measurement and caries epidemiology; acceptance is not implied. Its [current author guidelines](https://karger.com/cre/pages/guidelines), checked on 10 September 2026, support concise English research reports with sequential square-bracket references, a running title of at most 80 characters and 3–5 keywords. The suggested Research Article scale is about 5,000 words and 4–8 combined tables/figures; the proposed five figures and three tables fit that guidance. Supply separate editable unshaded Word tables, assembled figure files and legends; publication-ready supplements should preferably be below 10 MB. A 250-word structured abstract is a drafting target, not a verified limit from the currently accessible page. Ethics, consent, funding, contributions, conflicts, data access and AI assistance require accurate declarations. No previous author's approval or ethics coverage is inferred for this new proposal. See also the publisher's [technical instructions](https://karger.com/pages/technical-instructions-to-publish-a-paper) and [editorial policies](https://karger.com/pages/publication-ethics).

The following primary publications or author/publisher sources support the rationale and reporting approach. They do not predetermine this analysis's results:

- **Dietary amount and frequency:** Bernabé E, Vehkalahti MM, Sheiham A, Lundqvist A, Suominen AL. *The Shape of the Dose-Response Relationship between Sugars and Caries in Adults.* J Dent Res. 2016;95:167–172. [Publisher abstract and metadata](https://journals.sagepub.com/doi/10.1177/0022034515616572), DOI 10.1177/0022034515616572. An adult longitudinal study supports distinguishing amount from frequency; its DMFT outcome and validated intake measures differ from this concurrent ENS classifier. The accessible abstract, not subscription full text, was checked.
- **Dietary measurement in a national survey:** Alosaimi N, Bernabé E. *Amount and Frequency of Added Sugars Intake and Their Associations with Dental Caries in United States Adults.* Int J Environ Res Public Health. 2022;19:4511. [Publisher full text](https://mdpi-res.com/d_attachment/ijerph/ijerph-19-04511/article_deploy/ijerph-19-04511.pdf), DOI 10.3390/ijerph19084511. This original NHANES analysis distinguishes intake quantity and frequency definitions. ENS beverage glasses are not equivalent to its grams of added sugar.
- **Diet-containing ML models:** Ogwo C, Brown G, Warren J, Caplan D, Levy S. *Predicting dental caries outcomes in young adults using machine learning approach.* BMC Oral Health. 2024;24:529. [Publisher full text](https://link.springer.com/article/10.1186/s12903-024-04294-7), DOI 10.1186/s12903-024-04294-7. This original longitudinal study uses dietary and other predictors with nested resampling. Its temporal design and cumulative caries outcome are different; its performance is not a direct benchmark. No numerical accuracy claim is imported.
- **Current caries-method guidance:** Uribe SE, Carrasco-Labra A, Schwendicke F, Maldupa I. *Prognostic Clinical Predictive Models for Dental Caries Using Artificial Intelligence: Methodological Considerations.* Caries Res. Published online 16 July 2026. [Publisher article](https://karger.com/cre/article/doi/10.1159/000553309/952744/Prognostic-Clinical-Predictive-Models-for-Dental), DOI 10.1159/000553309. The publisher's article and online-first listing distinguish this prognostic guidance from our concurrent classification question. It is a methodological review, not original validation evidence.
- **Reporting:** Collins GS, Moons KGM, Dhiman P, Riley RD, Beam AL, Van Calster B, et al. *TRIPOD+AI statement: updated guidance for reporting clinical prediction models that use regression or machine learning methods.* BMJ. 2024;385:e078378. [Original statement](https://www.bmj.com/content/385/bmj-2023-078378), DOI 10.1136/bmj-2023-078378; [official expanded checklist](https://www.bmj.com/content/suppl/2024/04/25/bmj-2023-078378.DC8/colg078378.wt2.pdf). Bibliographic details and statement summary were verified through the original BMJ PDF/indexed record; automated retrieval of some BMJ URLs was intermittent. Apply its reporting items without implying that checklist completion establishes model validity.

The existing [ENS methodological-source audit](methodological-sources.md) records the oral-examination source, verified data provenance and the unresolved official weighting instructions. Those source facts can be cited in the new work; its earlier model estimates are not new study findings.

## Completion criteria and amendments

Before writing Results, require a complete new run manifest, all 23 expected variants or explicit failure diagnostics, source/configuration hashes, one out-of-fold probability per eligible person and variant, zero PSU overlap, successful convergence checks, matched paired denominators, valid bootstrap counts and the full contribution tables. Verify metric-direction conventions, identical primary participants across models and preservation of valid zeros. Report actual cohort or variable discrepancies before interpreting results.

Results and conclusions remain unwritten until those outputs exist. Record later deviations here with date, reason and whether new outcomes or performance had been inspected. Changes made because the new dietary gain is disappointing or favourable must be called exploratory; they cannot silently replace the primary comparison.

**Amendment log:** No post-fit amendment at initial freeze. The p99 sensitivity uses strictly positive training values, as clarified before fitting; all other valid zeros are preserved. Before any new fits, the display order was finalized as performance first and dietary gains second: five main figures with 4, 6, 4, 4 and 6 panels. This presentation change does not alter estimands or model selection.
