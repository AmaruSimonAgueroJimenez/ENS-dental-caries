"""Publication-oriented figures generated from shareable summary tables only.

All figures are written as 300-dpi PNG and editable SVG. Captions, source table
names, and uncertainty definitions accompany them in figure_metadata.json.
"""

from __future__ import annotations

import json
import textwrap
from pathlib import Path
from typing import Any

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import FuncFormatter, MaxNLocator, PercentFormatter
import numpy as np
import pandas as pd


FULL_MODELS = [
    "logistic", "spline_logistic", "random_forest", "hist_gradient_boosting",
    "svm", "decision_tree", "knn",
]
MODEL_LABELS = {
    "logistic": "Logistic regression", "spline_logistic": "Spline logistic regression",
    "random_forest": "Random forest", "hist_gradient_boosting": "Gradient boosting",
    "svm": "Support vector machine", "decision_tree": "Decision tree",
    "knn": "Nearest neighbours", "spline_base": "Spline: base variables",
    "spline_no_water": "Spline: without water", "rf_no_water": "Forest: without water",
    "rf_base": "Forest: base variables", "spline_extended": "Spline: extended context",
    "spline_no_teeth": "Spline: without remaining teeth",
}
FEATURE_LABELS = {
    "age": "Age", "sex": "Sex", "region": "Region", "rural": "Rural residence",
    "education": "Education", "indigenous": "Indigenous ancestry", "born_chile": "Born in Chile",
    "teeth_remaining": "Remaining teeth", "fish": "Fish / seafood", "dairy_frequency": "Dairy frequency",
    "dairy_type": "Dairy type", "wholegrain": "Wholegrain frequency", "legumes": "Legume frequency",
    "fruit_days": "Fruit days / week", "fruit_portions": "Fruit portions",
    "vegetable_days": "Vegetable days / week", "vegetable_portions": "Vegetable portions",
    "label_ingredients": "Ingredients on labels", "label_nutrition": "Nutrition on labels",
    "label_warnings": "Warnings on labels", "label_health": "Health claims on labels",
    "label_brand": "Brand on labels", "label_discounts": "Discounts on labels",
    "water": "Water intake", "soda_frequency": "Sugared soda frequency",
    "juice_frequency": "Sugared juice frequency", "oil": "Cooking oil / fat",
}
ASSOCIATION_LABELS = {
    "Primary linear water": "Primary adjustment",
    "Additional water context and dental attendance": "Water context + dental attendance",
    "Age at least 18": "Participants aged ≥18 years",
    "Exclude water above 99th percentile": "Exclude water above 99th percentile",
    "Without remaining teeth": "Omit remaining teeth",
    "Modified Poisson prevalence ratio": "Modified Poisson",
    "Original 28-day beverage conversion": "Original beverage conversion",
}
TEAL, ORANGE, INK, GREY = "#14616B", "#CC7028", "#243740", "#77858A"
COLORS = ["#14616B", "#CC7028", "#5576A1", "#6E8B54", "#9674A4", "#9A6357", "#5C9291"]
LINESTYLES = ["-", "-", "--", "--", "-.", ":", ":"]
STYLE = {
    "font.family": "sans-serif", "font.sans-serif": ["DejaVu Sans", "Arial"],
    "font.size": 10, "axes.titlesize": 11, "axes.titleweight": "bold",
    "axes.labelsize": 10, "xtick.labelsize": 9, "ytick.labelsize": 9,
    "legend.fontsize": 9, "text.color": INK, "axes.labelcolor": INK,
    "xtick.color": INK, "ytick.color": INK, "axes.edgecolor": "#A5AFB2",
    "axes.spines.top": False, "axes.spines.right": False,
    "figure.facecolor": "white", "axes.facecolor": "white",
    "savefig.facecolor": "white", "svg.fonttype": "none",
    "pdf.fonttype": 42, "axes.axisbelow": True,
}


def _read(directory: Path, name: str, required: list[str]) -> pd.DataFrame | None:
    path = directory / name
    if not path.exists() or path.stat().st_size == 0:
        return None
    try:
        table = pd.read_csv(path)
    except pd.errors.EmptyDataError:
        return None
    missing = sorted(set(required) - set(table.columns))
    if missing:
        raise ValueError(f"Figure source {name} is missing columns: {missing}")
    return table if len(table) else None


def _weighted(table: pd.DataFrame) -> pd.DataFrame:
    if "weighting" not in table:
        return table.copy()
    mask = table.weighting.str.lower().str.contains("survey", na=False)
    return table.loc[mask].copy() if mask.any() else table.copy()


def _wrap(value: str, width: int = 29) -> str:
    return "\n".join(textwrap.wrap(str(value), width=width, break_long_words=False))


def _interval(ax, estimate, low, high, y, color=TEAL, marker="o", hollow=False):
    """Draw endpoints directly; a bootstrap CI need not contain the point estimate."""
    if np.isfinite(low) and np.isfinite(high):
        ax.hlines(y, low, high, color=color, lw=1.5, zorder=2)
        ax.vlines([low, high], y - .045, y + .045, color=color, lw=1.1)
    if np.isfinite(estimate):
        ax.scatter(estimate, y, s=34, marker=marker, facecolor="white" if hollow else color,
                   edgecolor=color, linewidth=1.3, zorder=3)


def _grid(ax, axis="x"):
    ax.grid(axis=axis, color="#E6EAEC", lw=.7)
    ax.tick_params(length=3)


def _save(fig, name: str, directory: Path, metadata: dict, caption: str, sources: list[str]):
    paths = []
    for extension in ["png", "svg"]:
        path = directory / f"{name}.{extension}"
        fig.savefig(path, dpi=300, bbox_inches="tight", pad_inches=.12)
        paths.append(path)
    plt.close(fig)
    metadata["figures"].append({
        "name": name, "files": [p.name for p in paths], "caption": caption,
        "source_tables": sources, "png_dpi": 300,
    })
    return paths


def _performance(table, directory, metadata, labels, models):
    selected = table.loc[table.model.isin(models)]
    primary = _weighted(selected).drop_duplicates("model").sort_values("auc", ascending=False)
    if primary.empty:
        return []
    fig, axes = plt.subplots(1, 2, figsize=(10.5, 4.6), sharey=True,
                             gridspec_kw={"width_ratios": [1.2, 1]})
    y = np.arange(len(primary))
    unweighted = selected.loc[selected.get("weighting", pd.Series("", index=selected.index)).eq("Unweighted")]
    for panel, (metric, title) in enumerate([("auc", "A  Discrimination"), ("brier", "B  Prediction error")]):
        ax = axes[panel]
        for i, row in enumerate(primary.to_dict("records")):
            _interval(ax, row[metric], row.get(metric + "_low", np.nan), row.get(metric + "_high", np.nan), i - .10)
            comparison = unweighted.loc[unweighted.model.eq(row["model"])]
            if len(comparison):
                _interval(ax, float(comparison.iloc[0][metric]), np.nan, np.nan, i + .13, color=GREY, hollow=True)
        ax.set_title(title, loc="left", pad=12)
        _grid(ax)
        if metric == "auc":
            ax.axvline(.5, color=GREY, lw=1, ls=":")
            ax.set_xlim(min(.45, float(primary.auc.min()) - .03), 1)
            ax.set_xlabel("Area under the ROC curve (higher is better)")
        else:
            # A dot plot can use a clearly labelled local scale: retain every
            # interval endpoint and both sets of points, with rounded margins.
            bounds = np.r_[primary.brier.to_numpy(float),
                           primary.get("brier_low", primary.brier).to_numpy(float),
                           primary.get("brier_high", primary.brier).to_numpy(float),
                           unweighted.brier.to_numpy(float)]
            bounds = bounds[np.isfinite(bounds)]
            span = max(.01, float(np.ptp(bounds)))
            lower = max(0, np.floor((bounds.min() - span * .12) / .005) * .005)
            upper = min(1, np.ceil((bounds.max() + span * .12) / .005) * .005)
            ax.set_xlim(lower, upper)
            ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
            ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{value:.3f}"))
            ax.set_xlabel("Brier score (lower is better)")
    axes[0].set_yticks(y, [labels.get(m, m) for m in primary.model])
    axes[0].set_ylim(len(primary) - .5, -.5)
    handles = [Line2D([], [], color=TEAL, marker="o", lw=1.5, label="Survey weighted, with 95% CI")]
    if len(unweighted):
        handles.append(Line2D([], [], color=GREY, marker="o", mfc="white", lw=0, label="Unweighted point estimate"))
    fig.legend(handles=handles, loc="lower center", ncol=2, frameon=False, bbox_to_anchor=(.53, -.025))
    fig.subplots_adjust(left=.24, right=.985, wspace=.18, bottom=.20, top=.9)
    return _save(fig, "performance_dot", directory, metadata,
                 "Out-of-fold discrimination and Brier scores for the seven full-feature algorithms. "
                 "Survey-weighted points have 95% conditional stratified PSU-bootstrap intervals; "
                 "trained models and validation splits are held fixed. Open points are unweighted estimates. "
                 "AUC=0.5 denotes chance discrimination. The Brier-score dot plot uses an explicitly labelled "
                 "local scale covering all intervals and point estimates. Models are ordered by weighted AUC.",
                 ["model_performance.csv"])


def _calibration_roc(calibration, roc, directory, metadata, labels, models):
    if calibration is None and roc is None:
        return []
    fig, axes = plt.subplots(1, 2, figsize=(10.5, 5.6))
    sources, handles = [], []
    for ax, title in zip(axes, ["A  Calibration", "B  Receiver operating characteristic"], strict=True):
        ax.plot([0, 1], [0, 1], color="#B0B8BB", lw=1, ls="--", zorder=0)
        ax.set(xlim=(0, 1), ylim=(0, 1))
        ax.set_aspect("equal", adjustable="box")
        ax.set_title(title, loc="left", pad=12)
        _grid(ax, "both")
    for i, model in enumerate(models):
        color, linestyle = COLORS[i % len(COLORS)], LINESTYLES[i % len(LINESTYLES)]
        present = False
        if calibration is not None:
            block = _weighted(calibration).loc[lambda d: d.model.eq(model)].sort_values("predicted")
            if len(block):
                axes[0].plot(block.predicted, block.observed, marker="o", markersize=3.3,
                             color=color, ls=linestyle, lw=1.4, alpha=.95)
                present = True
        if roc is not None:
            block = _weighted(roc).loc[lambda d: d.model.eq(model)].sort_values(["fpr", "tpr"])
            if len(block):
                axes[1].plot(block.fpr, block.tpr, color=color, ls=linestyle, lw=1.35, alpha=.95)
                present = True
        if present:
            handles.append(Line2D([], [], color=color, ls=linestyle, lw=1.6, label=labels.get(model, model)))
    if calibration is not None:
        sources.append("calibration.csv")
    if roc is not None:
        sources.append("roc.csv")
    axes[0].set(xlabel="Mean predicted probability", ylabel="Observed caries prevalence")
    axes[1].set(xlabel="False-positive rate (1 − specificity)", ylabel="True-positive rate (sensitivity)")
    fig.legend(handles=handles, loc="lower center", ncol=3, frameon=False, bbox_to_anchor=(.5, -.012))
    fig.subplots_adjust(left=.08, right=.985, top=.91, bottom=.22, wspace=.24)
    return _save(fig, "calibration_roc", directory, metadata,
                 "Survey-weighted calibration and ROC curves from out-of-fold predictions for the full-feature "
                 "algorithms. Calibration points represent quantile-based groups of predicted probability; "
                 "the diagonal indicates perfect calibration. The ROC diagonal indicates chance discrimination. "
                 "Binned calibration curves are descriptive and have no confidence intervals.", sources)


def _effect_panel(ax, table, measure: str, title: str):
    rows = table.loc[table.measure.str.lower().eq(measure.lower())].copy()
    rows = rows.loc[rows.estimate.gt(0) & rows.ci_low.gt(0) & rows.ci_high.gt(0)]
    ax.set_title(title, loc="left", pad=12)
    ax.axvline(1, color=GREY, lw=1, ls=":")
    ax.set_xscale("log")
    _grid(ax)
    if rows.empty:
        ax.text(.5, .5, "No estimate available", ha="center", va="center", transform=ax.transAxes)
        ax.set_yticks([])
        return
    for i, row in enumerate(rows.to_dict("records")):
        _interval(ax, row["estimate"], row["ci_low"], row["ci_high"], i, color=ORANGE)
    rowlabels = [_wrap(ASSOCIATION_LABELS.get(row.model, row.model), 32) + f"\n(n={int(row.n):,})"
                 for row in rows.itertuples()]
    ax.set_yticks(np.arange(len(rows)), rowlabels)
    ax.set_ylim(len(rows) - .55, -.55)
    lower, upper = min(.95, rows.ci_low.min() * .97), max(1.05, rows.ci_high.max() * 1.03)
    ax.set_xlim(lower, upper)
    # Evenly spaced *label values* avoid near-duplicate labels created by adding
    # 1.0 to a geometric grid. Positions remain logarithmic on the actual axis.
    if .90 <= lower and upper <= 1.10:
        ticks = np.arange(.90, 1.10001, .025)
    else:
        ticks = MaxNLocator(nbins=5, steps=[1, 2, 2.5, 5, 10]).tick_values(lower, upper)
        ticks = ticks[~np.isclose(ticks, 1, atol=max(1e-8, (upper - lower) * .06))]
        ticks = np.r_[ticks, 1.0]
    ticks = np.sort(ticks[(ticks >= lower - 1e-10) & (ticks <= upper + 1e-10)])
    ax.set_xticks(ticks)
    precision = max(3, int(np.ceil(-np.log10(np.diff(ticks).min()))) + 1) if len(ticks) > 1 else 3
    ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{value:.{precision}f}"))
    ax.minorticks_off()
    ax.set_xlabel(f"{measure} per additional glass/day (log scale)")


def _water(curve, effects, directory, metadata):
    if curve is None and effects is None:
        return []
    has_pr = effects is not None and effects.measure.str.lower().eq("prevalence ratio").any()
    fig = plt.figure(figsize=(12.5, 8.0 if has_pr else 5.4))
    grid = fig.add_gridspec(2 if has_pr else 1, 2, width_ratios=[1.25, 1.0],
                           height_ratios=[4.8, 1.5] if has_pr else [1], wspace=.85, hspace=.75)
    ax = fig.add_subplot(grid[:, 0])
    ax.set_title("A  Adjusted prevalence across water intake", loc="left", pad=12)
    sources = []
    if curve is not None:
        curve = curve.sort_values("water")
        ax.fill_between(curve.water.to_numpy(float), curve.ci_low.to_numpy(float), curve.ci_high.to_numpy(float),
                        color=ORANGE, alpha=.17, linewidth=0)
        ax.plot(curve.water, curve.adjusted_prevalence, color=ORANGE, lw=2)
        ax.set_xlim(curve.water.min(), curve.water.max())
        sources.append("water_curve.csv")
    ax.set(xlabel="Reported water intake (glasses/day)", ylabel="Adjusted caries prevalence", ylim=(0, 1))
    ax.yaxis.set_major_formatter(PercentFormatter(1))
    ax.xaxis.set_major_locator(MaxNLocator(integer=True, nbins=7))
    _grid(ax, "both")
    if effects is not None:
        _effect_panel(fig.add_subplot(grid[0, 1]), effects, "Odds ratio", "B  Linear-water sensitivity analyses")
        sources.append("water_effects.csv")
        if has_pr:
            _effect_panel(fig.add_subplot(grid[1, 1]), effects, "Prevalence ratio", "C  Prevalence-ratio sensitivity")
    fig.subplots_adjust(left=.07, right=.985, top=.93, bottom=.12)
    return _save(fig, "water_association", directory, metadata,
                 "Adjusted cross-sectional association of reported water intake with cavitated caries. "
                 "Panel A is survey-weighted marginal standardization of the water-spline model, with 95% "
                 "pointwise delta-method confidence intervals; the displayed exposure grid is restricted to "
                 "the range supplied in water_curve.csv. Panels B and C show linear-water sensitivity estimates "
                 "per additional reported glass/day and survey-design 95% intervals. Odds ratios and prevalence "
                 "ratios are displayed separately on logarithmic axes; the vertical line denotes no association. "
                 "These estimates do not establish causal protection.", sources)


def _incremental(table, directory, metadata):
    rows = table.loc[table.metric.str.lower().eq("auc")].copy()
    if rows.empty:
        return []
    fig, ax = plt.subplots(figsize=(10.5, max(3.5, len(rows) * .7 + 1.7)))
    for i, row in enumerate(rows.to_dict("records")):
        _interval(ax, row["difference"], row["ci_low"], row["ci_high"], i,
                  color=ORANGE if "water" in row["comparison"].lower() else TEAL)
    ax.set_yticks(np.arange(len(rows)), [_wrap(value, 41) for value in rows.comparison])
    ax.set_ylim(len(rows) - .5, -.5)
    ax.axvline(0, color=GREY, lw=1, ls=":")
    bound = max(.01, float(np.nanmax(np.abs(rows[["difference", "ci_low", "ci_high"]].to_numpy()))) * 1.14)
    ax.set_xlim(-bound, bound)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{value:+.3f}" if value else "0"))
    ax.set_xlabel("Difference in AUC (first model minus reference)")
    ax.set_title("Incremental predictive value", loc="left", pad=13)
    _grid(ax)
    fig.subplots_adjust(left=.40, right=.975, top=.87, bottom=.18)
    return _save(fig, "incremental_water", directory, metadata,
                 "Paired differences in survey-weighted out-of-fold AUC, with 95% conditional stratified "
                 "PSU-bootstrap intervals. Each contrast subtracts the reference model from the first model. "
                 "Positive differences favour the first model. Orange identifies contrasts mentioning water; "
                 "other planned comparisons are teal. Validation participants and bootstrap draws are shared "
                 "within each pair; models and the validation split remain fixed.", ["paired_comparisons.csv"])


def _importance(table, directory, metadata, labels):
    needed = {"model", "fold", "feature", "brier_increase"}
    if not needed.issubset(table):
        raise ValueError("permutation_importance.csv must retain raw model, fold, feature, brier_increase columns")
    fold_rows = []
    for (model, feature, fold), block in table.groupby(["model", "feature", "fold"], sort=False):
        fold_rows.append({"model": model, "feature": feature, "fold": fold,
                          "increase": block.brier_increase.mean(),
                          "weight": block.test_weight_sum.iloc[0] if "test_weight_sum" in block else block.n_test.iloc[0] if "n_test" in block else 1})
    aggregates = []
    for (model, feature), block in pd.DataFrame(fold_rows).groupby(["model", "feature"], sort=False):
        aggregates.append({"model": model, "feature": feature,
                           "mean": float(np.average(block.increase, weights=block.weight)),
                           "low": block.increase.min(), "high": block.increase.max()})
    summary = pd.DataFrame(aggregates)
    models = summary.model.drop_duplicates().tolist()
    if not models:
        return []
    top = summary.groupby("feature")["mean"].max().sort_values(ascending=False).head(10).index.tolist()
    if "water" in summary.feature.values and "water" not in top:
        top = top[:9] + ["water"]
    fig, axes = plt.subplots(1, len(models), figsize=(6 + 4 * len(models), 5.6), sharey=True, squeeze=False)
    for i, model in enumerate(models):
        ax = axes[0, i]
        rows = summary.loc[summary.model.eq(model)].set_index("feature")
        for j, feature in enumerate(top):
            if feature in rows.index:
                row = rows.loc[feature]
                _interval(ax, row["mean"], row.low, row.high, j, color=ORANGE if feature == "water" else TEAL)
        ax.axvline(0, color=GREY, lw=1, ls=":")
        ax.set_title(labels.get(model, model), loc="left", pad=12)
        ax.set_xlabel("Increase in held-out Brier score")
        ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
        _grid(ax)
    axes[0, 0].set_yticks(np.arange(len(top)), [FEATURE_LABELS.get(feature, feature) for feature in top])
    axes[0, 0].set_ylim(len(top) - .5, -.5)
    fig.subplots_adjust(left=.20, right=.98, top=.9, bottom=.13, wspace=.20)
    return _save(fig, "permutation_importance", directory, metadata,
                 "Permutation importance of raw held-out predictors. Points are mean increases in survey-weighted "
                 "Brier score after permutation, averaged within each fold over repeats and then across folds using "
                 "test-set expansion-weight totals. Whiskers show the minimum to maximum fold means; they are "
                 "stability ranges, not confidence intervals. The displayed variables are the ten with the largest "
                 "maximum mean importance across models, retaining water when necessary. Orange identifies water. "
                 "Importance has no direction or causal interpretation and can be shared among correlated predictors.",
                 ["permutation_importance.csv"])


def _prevalence(table, directory, metadata):
    specs = [("age", "A  Age", "Age group (years)"),
             ("education", "B  Education", "Education level"),
             ("water", "C  Water intake", "Reported glasses/day")]
    fig, axes = plt.subplots(1, 3, figsize=(12.5, 4.2), sharey=True)
    available = False
    for ax, (variable, title, xlabel) in zip(axes, specs, strict=True):
        block = table.loc[table.variable.eq(variable)].copy()
        if variable == "education":
            block["_order"] = block.level.map({"Low": 0, "Intermediate": 1, "High": 2})
            block = block.sort_values("_order")
        color = ORANGE if variable == "water" else TEAL
        for i, row in enumerate(block.to_dict("records")):
            ax.vlines(i, row["ci_low"], row["ci_high"], color=color, lw=1.4)
            ax.hlines([row["ci_low"], row["ci_high"]], i - .055, i + .055, color=color, lw=1.1)
            ax.scatter(i, row["estimate"], color=color, s=33, zorder=3)
            available = True
        ax.set_xticks(np.arange(len(block)), [f"{row.level}\n(n={int(row.n):,})" for row in block.itertuples()])
        if variable == "water":
            ax.tick_params(axis="x", labelsize=8)
        ax.set_title(title, loc="left", pad=12)
        ax.set(xlabel=xlabel, ylim=(0, 1))
        _grid(ax, "y")
    axes[0].set_ylabel("Caries prevalence")
    axes[0].yaxis.set_major_formatter(PercentFormatter(1))
    fig.subplots_adjust(left=.07, right=.99, top=.87, bottom=.23, wspace=.16)
    if not available:
        plt.close(fig)
        return []
    return _save(fig, "descriptive_prevalence", directory, metadata,
                 "Survey-weighted caries prevalence by age, education, and reported water intake, with 95% "
                 "survey-design confidence intervals. Parentheses give unweighted group sample sizes. "
                 "These are unadjusted descriptive comparisons among dentate participants; group differences "
                 "must not be interpreted as causal effects.", ["descriptive_prevalence.csv"])


def make_figures(
    tables_dir: str | Path,
    figures_dir: str | Path,
    private_oof_path: str | Path | None = None,
    model_labels: dict[str, str] | None = None,
    top_models: list[str] | None = None,
) -> list[Path]:
    """Render all available summary tables and return generated image paths.

    ``private_oof_path`` is retained as an integration-compatible optional argument;
    it is intentionally never read. ROC/calibration data must be summary CSVs.
    Missing source tables are recorded in metadata; malformed existing tables fail.
    ``top_models`` explicitly selects algorithms, defaulting to all seven full models.
    """
    source, destination = Path(tables_dir), Path(figures_dir)
    destination.mkdir(parents=True, exist_ok=True)
    labels, models = {**MODEL_LABELS, **(model_labels or {})}, top_models or FULL_MODELS
    specifications = {
        "model_performance.csv": ["model", "auc", "brier"],
        "calibration.csv": ["model", "predicted", "observed"],
        "roc.csv": ["model", "fpr", "tpr"],
        "water_curve.csv": ["water", "adjusted_prevalence", "ci_low", "ci_high"],
        "water_effects.csv": ["model", "measure", "estimate", "ci_low", "ci_high", "n"],
        "paired_comparisons.csv": ["comparison", "metric", "difference", "ci_low", "ci_high"],
        "permutation_importance.csv": ["model", "fold", "feature", "brier_increase"],
        "descriptive_prevalence.csv": ["variable", "level", "n", "estimate", "ci_low", "ci_high"],
    }
    tables = {name: _read(source, name, required) for name, required in specifications.items()}
    metadata: dict[str, Any] = {
        "figures": [], "language": "English", "private_predictions_read": False,
        "algorithms_displayed": models, "missing_or_empty_source_tables": [name for name, table in tables.items() if table is None],
        "uncertainty_note": "See each caption: bootstrap CIs, survey-design CIs, and fold stability ranges are distinct.",
        "style": {"background": "white", "primary_colour": TEAL, "water_accent": ORANGE,
                  "font": "DejaVu Sans", "raster_dpi": 300, "vector_format": "SVG"},
    }
    paths: list[Path] = []
    with plt.rc_context(STYLE):
        if tables["model_performance.csv"] is not None:
            paths += _performance(tables["model_performance.csv"], destination, metadata, labels, models)
        paths += _calibration_roc(tables["calibration.csv"], tables["roc.csv"], destination, metadata, labels, models)
        paths += _water(tables["water_curve.csv"], tables["water_effects.csv"], destination, metadata)
        if tables["paired_comparisons.csv"] is not None:
            paths += _incremental(tables["paired_comparisons.csv"], destination, metadata)
        if tables["permutation_importance.csv"] is not None:
            paths += _importance(tables["permutation_importance.csv"], destination, metadata, labels)
        if tables["descriptive_prevalence.csv"] is not None:
            paths += _prevalence(tables["descriptive_prevalence.csv"], destination, metadata)
    (destination / "figure_metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
    return paths
