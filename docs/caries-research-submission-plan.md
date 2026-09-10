# Caries Research submission proposal

Checked against official publisher sources on **10 September 2026**. This is a preparation plan, not a submission or an acceptance prediction.

## Fit and proposed contribution

**Caries Research, Research Article** is a defensible first target. The manuscript addresses population caries epidemiology and the information available from demographic, dental and dietary measurements. Its contribution is a transparent comparison of all 27 predictors and six conceptual blocks across two model families, with cluster-respecting validation, calibration and stability assessment. It should not be pitched as a high-performing clinical tool or evidence that water prevents caries. Modest discrimination, retrospective amendments and the absence of external validation will matter to reviewers. This fit assessment is an editorial judgement, informed by the journal's scope and its recent [methodological review of caries predictive models](https://doi.org/10.1159/000553309).

## Checked requirements

The [journal guidelines](https://karger.com/cre/pages/guidelines) describe Research Articles as typically **5,000 words with 4–8 tables and figures**, explicitly as guidance. English is required. Supply a completed cross-sectional STROBE checklist as cited supplementary material. Use an unnumbered heading structure, a running title of at most 80 characters and 3–5 keywords. Cite references sequentially in square brackets, with a numbered Vancouver-style bibliography listing up to six authors before “et al.” Supplementary files should be publication-ready, clearly named, cited in the text and preferably below 10 MB. Essential findings belong in the main article.

The guideline page does **not state an abstract word limit or compulsory abstract headings**. Its linked [Research Article template](https://karger.com/DocumentLibrary/research-article.docx) could not be retrieved during this check. The proposal uses Introduction, Methods, Results and Conclusion and stays below 250 words as a conservative drafting choice; verify the template/submission form before submission. The page's separate 250-word plain-language-summary guidance must not be misreported as an abstract limit.

[Technical instructions](https://karger.com/pages/technical-instructions-to-publish-a-paper) require separate editable Word tables and individual figure files, with all panels of a figure assembled together. Use automatic page and line numbering. Provide editable vector text for flowcharts and statistical plots; check legibility at final size, 57–180 mm wide and no more than 223 mm high. Supply at least 300 dpi where raster export is used, RGB colour and bold lowercase panel labels. Figure legends belong at the manuscript end. The repository provides SVG/PDF and 300-dpi PNG versions; the final journal layout still needs visual inspection.

[Publication ethics and editorial policies](https://karger.com/pages/publication-ethics) require transparent ethics/consent, conflicts, funding and author-contribution statements, and a data-availability statement describing access restrictions where applicable. Generative-AI assistance must be disclosed in Methods, including its use and verification; authors retain responsibility. Confirm these declarations with the authors. No automated check establishes human approval or ethical coverage.

## Proposed package

The approximately 3,000–3,800-word main text will retain the scientific results needed to evaluate the contribution:

- Table 1: cohort and weighted prevalence by demographic characteristics.
- Table 2: seven algorithms using the same 27 predictors, with AUC, Brier score and log loss. Calibration and fixed-threshold metrics are in supplementary Table S4.
- Table 3: paired algorithm and global feature-set comparisons, with conditional confidence intervals.
- Figure 1: original-style grouped model-comparison bars, with classification metrics, AUC, Brier score and log loss in four panels and shared conditional PSU-bootstrap intervals.
- Figure 2: blue horizontal bars for the ten leading predictors in each model and two corresponding fold-rank panels; complete results for all 27 remain in Figure S7.
- Figure 3: ROC, calibration, fold AUC and weighted versus unweighted AUC.
- Figure 4: joint predictor-family contributions and paired refitted comparisons.
- Figure 5: full/subset performance and demographic subgroup summaries, with four descriptive panels.

The author-review Word embeds all five four-panel figures, as requested, preserving the original Correa manuscript’s graphical language. Separate figure exports and legends are also retained for submission preparation. The original eight analytical displays form supplementary Figures S1–S8, including the participant flow and complete 27-predictor results. Five figures plus three tables match the upper end of the journal’s suggested combined range.

One supplementary document will include the full variable dictionary, source reconciliation, grids and fold diagnostics, all 13 fitted variants plus the prevalence baseline, unweighted results, calibration/ROC plots, subgroup and subset evaluations, full permutation tables, and the complete exploratory water analyses. Water is a result-motivated secondary question. Include STROBE and TRIPOD+AI reporting checklists as identifiable supplementary items, with accurate locations and genuinely unresolved items marked for author review. This allocation is a proposal rather than a journal-imposed arrangement.

## Author review before submission

Keep these checks in a separate author note, rather than filling the manuscript with administrative placeholders:

- Confirm the examination-stage expansion weight against the original ENS manual and clarify the singleton-stratum convention. The current F1/F2 weight is a documented working choice, numerically identical to F2 in the supplied release.
- Complete affiliations b, c and d, the corresponding email, author name spelling/identifiers and current contributions. Preserve supplied affiliations a and e without guessing missing institutions.
- Verify that CEISH approval **068-2022**, consent wording and permission for this updated secondary analysis apply. These details come from the supplied Correa draft, not an independently inspected approval letter.
- Confirm the supplied ANID doctoral grant **2019–21190278**, funder-role and conflict statements, and update contributions for the Python reanalysis.
- Confirm source-data access terms and the generative-AI disclosure. Public release of code and aggregate results has already been authorized and the reanalysis branch published; no renewed authorization is requested. Participant records and individual predictions are excluded from the public repository. The manuscript proposal remains local.
- Verify the current abstract template and submission-form requirements; approve the final text, tables and figure files. Identify any previous public preprint or related submission accurately.

The [verified reference register](manuscript-references.json), [analysis plan](caries-analysis-plan.md), [migration audit](r-to-python-migration.md) and [completed report](caries_python.qmd) support this package. The thirteen scientific references were checked against primary publications or authoritative bibliographic records; each register entry states the scope of verification.
