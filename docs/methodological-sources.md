# ENS 2016-2017: methodological source and coding audit

Verified on 10 September 2026. This note distinguishes published evidence, observations from the supplied dataset, and recommendations that still require the original ENS user manual. It does not certify a survey weight merely from its name.

## Original analysis and input provenance

The original water-and-caries analysis is in [AmaruSimonAgueroJimenez/ENS-dental-caries](https://github.com/AmaruSimonAgueroJimenez/ENS-dental-caries/tree/77c107feb56d3f825cd4ad26354b672f8bfebe38), audited at commit `77c107feb56d3f825cd4ad26354b672f8bfebe38`. Its preserved `docs/index.qmd` is the R analysis reference, and the saved `docs/index.html` contains AUC values matching the manuscript. This confirms the source relationship; it does not show that the R analysis has been freshly rerun.

The R program reads `data/data.dta`. The attachment `23.07.26_base_ens_5520_R_enviar.dta` was recovered from Andrea Correa's email dated 31 July 2023 and inspected. Its SHA-256 is `754c2d32bafd002b861221d9d468f74a703de4ea9dd6c492cf848f042f3b0966`. The supplementary `data/data.sav` SHA-256 is `9873b421d446190070bfff6495321fb152485c47457692936dbcd18c542eb89a`. The Stata original remains unmodified. The public provenance record does not require email addresses or message contents.

The original Stata file has 5,520 unique survey identifiers, all matching the SPSS oral-examination records. Their row order differs, so reconciliation joins by identifier. The original `cariesnt`, decayed-teeth and remaining-teeth variables agree with the Python reconstruction. Education codes differ numerically (Stata: 0 high, 1 intermediate, 2 low) but agree after decoding. The derived indigenous indicator also agrees despite a changed raw code for no indigenous identification. The reconciled dentate cohort has 5,036 participants; its education-and-water-complete subset has 4,994, including 2,664 caries cases.

Twenty-five of the 27 predictor constructs agree after decoding labels and accounting for source numeric precision. The two differences, soda and juice frequency, have an identified derivation: the original `frecsembebida` and `frecsemjugo` use four weeks per month. Converting their weekly values to daily values therefore uses 28 days/month; the primary Python reconstruction uses 30. An additional adjusted-water analysis retains the original conversion. The original raw quantities and frequency-unit codes agree between source files.

The Stata water metadata confirms that `die11` is labelled numeric. The R pipeline converts it to a factor and then applies `as.numeric(droplevels(die11))`, returning category indices rather than the reported glasses/day. Ordering is retained, but cardinal spacing changes when observed values have gaps. The Python analysis uses the reported numeric values. The full mapping is in [legacy_water_coding.csv](../outputs/tables/legacy_water_coding.csv); source hashes and checks are in [source_reconciliation.json](../outputs/tables/source_reconciliation.json) and [source_comparison.csv](../outputs/tables/source_comparison.csv).

The supplementary SPSS release supplies survey weights, PSUs and extended water-context fields absent from the Stata file. This verified-source reanalysis intentionally improves the original workflow; it does not claim exact reproduction of `caret` fits or historical performance. See the [R-to-Python migration audit](r-to-python-migration.md) for the complete comparison. The remaining documentation gaps below concern official survey instructions and measurement definitions, not recovery of the original analysis input.

## Published evidence on the oral examination

[Margozzini et al. (2020), *Number of Remaining Teeth and Its Association with Educational Level in Chilean Adults*](https://pmc.ncbi.nlm.nih.gov/articles/PMC7479467/) ([DOI](https://doi.org/10.1155/2020/8848190)) describes ENS as a cross-sectional, stratified, multistage cluster survey. Trained nurses examined cavitated caries, prostheses, and tooth counts during the second home visit. The study used 5,473 participants from the 5,520-person oral-examination stage and reported analysis with SPSS Complex Samples. It does **not** identify the weight variable in its methods. Its sample size should therefore not be imposed on a new analysis with different eligibility or missing-data rules.

## Official documents located, but not retrieved

The following official addresses are cited by published research or the ENS teaching material. The Ministry of Health server returned HTTP 403 during this audit. Their content was **not** read, and the links alone do not establish the correct weight.

- [Calculation of expansion factors](https://epi.minsal.cl/wp-content/uploads/2018/06/Informe-c%C3%A1lculo-de-factores-de-expansi%C3%B3n-ENS-2016-2017.pdf).
- [Sampling design](https://epi.minsal.cl/wp-content/uploads/2018/05/DISE%C3%91O-MUESTRAL-ENS-2016-2017.pdf).
- [F2 questionnaire](https://epi.minsal.cl/wp-content/uploads/2018/02/ENS-2016-17-CuestionarioF2.DEPTO_.EPIDEMIOLOG%C3%8DA.pdf).
- [Oral-health report](https://epi.minsal.cl/wp-content/uploads/2021/03/Informe_Salud_Bucal_ENS_2016_17.pdf).
- [ENS database documentation landing page](https://epi.minsal.cl/bases-de-datos/).

The archived teaching material identifies a user manual updated on 14 February 2018 and F1-F4 codebooks. Obtain that manual and preserve the exact version before declaring that a particular weight follows official instructions.

## Accessible technical source

[Alvaro Passi-Solar, 2020 ENS complex-survey workshop](https://rstudio-pubs-static.s3.amazonaws.com/666227_f822f21ebae44f219a23764b448be668.html) is an authored technical teaching source, **not an official MINSAL manual**. Its section “CHEQUEAR FACTOR DE EXPANSION” reproduces a table imported from `ENS2017_FactoresExp.xlsx`. Relevant mappings are:

| Laboratory variable | F1/laboratory column | F2/laboratory column |
| --- | --- | --- |
| Glucose, HbA1c | `Fexp_EX1p_Corr` | `Fexp_F1F2EX1p_Corr` |
| HDL, triglycerides, total cholesterol | `Fexp_EX2p_Corr` | `Fexp_F1F2EX2p_Corr` |
| TSH, free T4 | `Fexp_EX3p_Corr` | `Fexp_F1F2EX3p_Corr` |

The workshop uses `Conglomerado` and `Estrato`, constructs the survey design before analytical subsetting, and identifies stratum 22 as a singleton. Its example uses a certainty-PSU setting; this example does not establish that all singleton strata in every subset are certainty selections.

The laboratory mappings above describe the scope of the source material. Laboratory-stage weights are not used for the present water-and-caries analysis, which has no laboratory component.

## Direct checks of the supplied SPSS file

These findings were computed from `data/data.sav`, not copied from the publications:

| Item | Observed value |
| --- | --- |
| Total records | 6,233 |
| Nonmissing `Fexp_F1p_Corr` | 6,233 |
| Nonmissing `Fexp_F2p_Corr` | 5,520 |
| Nonmissing `Fexp_F1F2p_Corr` | 5,520 |
| Equality of F2 and F1F2 weights | Identical in every row, including missingness |
| Sum of F1 weights | 14,518,968.989 |
| Sum of F2 / F1F2 weights | 14,518,968.979 |
| Nonmissing design identifiers | All 6,233 records |
| Distinct strata / clusters | 30 / 1,077 |
| Clusters spanning multiple strata | 0 |
| Stratum 22 clusters in the full file | 1 |

The labels of the F1, F2, F1F2, and EX1 corrected weights describe poststratification over 120 cells and absolute truncation at the 99.5th percentile. EX2 and EX3 labels describe 14 cells. Preserve these supplied weights; do not truncate them again by default.

`Estrato` is not `Region * 10 + Zona` in this file. The `Region` labels use a different order from the geographic numbering embedded in `Estrato`. Keep the supplied design identifiers and read the region value labels rather than substituting administrative region numbers.

### Oral and water variables verified in file metadata

| Variable | Meaning | Coding / analytical implication |
| --- | --- | --- |
| `die11` | Reported glasses of water per day; refers to card 22 | Zero is valid. The label does not establish glass volume, water source, fluoride exposure, or substitution for sweetened drinks. |
| `die12_cantidad`, `die12_unidad` | Frequency of sweetened soft drinks during the previous month | Units: 1 = times/day; 2 = times/week; 3 = times/month; 4 = no consumption. |
| `die12e` | Standard glasses of sweetened soft drinks per occasion | Combine frequency and quantity only after explicitly defining unit conversions and skip handling. |
| `die13_cantidad`, `die13_unidad`, `die13e` | Sweetened juice frequency and standard glasses per occasion | Frequency units have the same four codes as soft drinks. |
| `as29` | Household water source | Public supply has three separate codes (1-3); other categories include well, natural surface source, tanker, and other. This is not measured fluoride concentration. |
| `as30` | Household water distribution | Tap inside dwelling; tap outside dwelling but on the site; carried water. |
| `m8p13` | Well, spring, or rural drinking-water use for drinking/cooking during the preceding week | 1 = yes; 2 = no. This differs from usual daily water quantity. |
| `m5p3`, `m5p6` | Remaining upper and lower teeth | Each labelled 0-16. Total edentulism requires both valid counts to equal zero. |
| `m5p5`, `m5p8` | Decayed upper and lower teeth | Each labelled 0-16. These are current decayed-tooth counts, not DMFT. |
| `m5p1` | Removable prosthesis use | Upper, lower, both, or none; not a substitute for tooth counts. |

All four oral tooth-count variables had valid nonnegative values in 5,520 records. There were 484 people with zero remaining teeth in both arches. These counts are checks of this file, not final study eligibility counts.

Water values observed were 0-40 glasses/day, with five missing records. The metadata also defines code 51 as “other value, specify”; no such record occurred here. It must not be interpreted as 51 glasses in another release. The SPSS missing-value metadata includes values at or below -5555 for this item. Missing codes must be converted to missing values before numeric modelling. No verified primary document retrieved here established the volume of card 22; report glasses/day until that is resolved.

## Recommendations, conditional on the final research question

1. For an F1 exposure paired with the F2 oral examination, `Fexp_F1F2p_Corr` is the defensible **working candidate**, because both stages are required. It is numerically identical to `Fexp_F2p_Corr` in this supplied file. Do not describe this choice as officially confirmed until the original manual is obtained. The original R model calls omit survey fitting weights; an F1-weight analysis must not be described as reproducing the original weighting strategy.
2. Represent the supplied strata and clusters in variance estimation. Weighted regression with ordinary independent-observation standard errors does not reproduce the complex-survey analysis described by Margozzini et al. Document how singleton strata are treated. Do not assert certainty selection solely because one PSU was observed.
3. Establish the design-eligible sample first, then define analytical domains such as dentate participants, age restrictions, or complete outcomes. Retain zero contributions from out-of-domain records when the variance method requires them. Do not silently discard clusters before domain variance estimation.
4. Report exclusions sequentially and distinguish oral-examination participation, total edentulism, exposure missingness and covariate missingness. The 2020 publication's 5,473 and the supplied file's 5,520 oral records are compatible with different analytical restrictions. Preserve the file hashes and identifier-based reconciliation already completed here, rather than relying only on matching final counts.
5. Use association language for concurrent water intake and existing cavitated caries. A water-source covariate can be a useful sensitivity adjustment, but neither daily glass count nor public-supply status establishes individual fluoride dose or a causal preventive effect.

The exact official weight instructions, card 22 volume, questionnaire skip rules for beverage questions, and certainty status of stratum 22 remain documentation gaps. They should be disclosed, not filled with assumptions presented as verified facts.
