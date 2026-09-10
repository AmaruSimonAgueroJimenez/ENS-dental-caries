"""Historical-reference and matched-coding panels from aggregate outputs only."""
from pathlib import Path
import hashlib

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from .figures import FEATURE_LABELS, _save, _wrap


def make_historical_figure(source, destination, metadata):
    source, destination = Path(source), Path(destination)
    names = ["historical_saved_importance.csv", "historical_importance27_summary.csv",
             "historical_mdi27_summary.csv"]
    saved, importance, mdi = [pd.read_csv(source / name) for name in names]
    variants = ["rf_complete_modern", "rf_complete_historical"]
    if set(importance.model) != set(variants) or any(
        set(importance.loc[importance.model.eq(model), "feature"]) != set(FEATURE_LABELS)
        for model in variants
    ):
        raise ValueError("Historical comparison figure requires both complete 27-predictor profiles")
    blue, orange = "#377EB8", "#A85116"
    with plt.rc_context({"font.size": 7, "axes.titlesize": 8, "axes.labelsize": 7,
                         "xtick.labelsize": 6.5, "ytick.labelsize": 6.5,
                         "legend.fontsize": 6.5}):
        fig, axes = plt.subplots(2, 2, figsize=(180 / 25.4, 230 / 25.4),
                                 gridspec_kw={"height_ratios": [0.85, 1.3]},
                                 layout="constrained")
        fig.get_layout_engine().set(w_pad=0.025, h_pad=0.055, wspace=0.025, hspace=0.05)
        ax = axes[0, 0]
        top = saved.sort_values("rank").head(11)
        ax.barh(np.arange(11), top.score, color=blue, height=.68)
        ax.set_yticks(np.arange(11), [_wrap(v, 22) for v in top.display_label])
        ax.invert_yaxis()
        ax.set(xlim=(0, 108), xlabel="Recovered score (0–100)",
               title="a  Original saved scores")
        for i, value in enumerate(top.score):
            ax.text(value + 1, i, f"{value:.1f}", va="center", fontsize=6)
        for tick in ax.get_yticklabels():
            if "Water" in tick.get_text(): tick.set_fontweight("bold")

        ax = axes[0, 1]
        for model, color, marker, label in zip(
            variants, [orange, blue], ["o", "s"], ["Modern coding", "Historical coding"], strict=True
        ):
            a = importance.loc[importance.model.eq(model)].set_index("feature")
            b = mdi.loc[mdi.model.eq(model)].set_index("feature")
            mdi_rank = b.mdi_mean.rank(ascending=False, method="min")
            permutation_rank = a.brier_increase.rank(ascending=False, method="min")
            ax.scatter(mdi_rank, permutation_rank.reindex(mdi_rank.index), marker=marker,
                       s=17, edgecolor=color, facecolor="white" if marker == "o" else color,
                       linewidth=.7, alpha=.85, label=label)
            x, y = float(mdi_rank.water), float(permutation_rank.water)
            ax.scatter([x], [y], marker=marker, s=48, facecolor="none", edgecolor="black", linewidth=.9)
            ax.annotate("Water", (x, y), xytext=(5, 5) if model == variants[0] else (-4, -9),
                        ha="left" if model == variants[0] else "right",
                        textcoords="offset points", fontsize=6.5, color=color)
        ax.plot([1, 27], [1, 27], color="#89939A", ls=":", lw=.7)
        ax.set(xlim=(.3, 28), ylim=(28, .3), xticks=[1, 7, 14, 21, 27], yticks=[1, 7, 14, 21, 27],
               xlabel="Rank of summed impurity importance", ylabel="Rank of held-out permutation importance",
               title="b  Whole-predictor ranks")
        ax.legend(loc="upper right", frameon=True, facecolor="white", edgecolor="none", framealpha=.95)

        order = importance.loc[importance.model.eq(variants[1])].sort_values(
            ["brier_increase", "feature"], ascending=[False, True]).feature.tolist()
        bounds = importance[["minimum_fold_brier_increase", "maximum_fold_brier_increase"]].to_numpy()
        lo, hi = min(0., float(bounds.min())), max(0., float(bounds.max()))
        pad = max((hi - lo) * .1, .0003)
        for ax, model, panel, label in zip(axes[1], variants, "cd", ["Modern coding", "Historical coding"], strict=True):
            rows = importance.loc[importance.model.eq(model)].set_index("feature").loc[order]
            y = np.arange(len(rows))
            ax.barh(y, rows.brier_increase, height=.65, color=blue, alpha=.9)
            ax.hlines(y, rows.minimum_fold_brier_increase, rows.maximum_fold_brier_increase,
                      color="#35434C", linewidth=.8)
            ax.scatter(rows.brier_increase, y, color="#152C3A", s=6, zorder=3)
            ax.axvline(0, color="#48545C", lw=.7)
            ax.set_yticks(y, [FEATURE_LABELS[name] for name in order])
            for tick, feature in zip(ax.get_yticklabels(), order, strict=True):
                if feature == "water": tick.set_fontweight("bold")
            ax.set(ylim=(len(rows) - .3, -.7), xlim=(lo-pad, hi+pad),
                   xlabel="Increase in held-out Brier score", title=f"{panel}  {label}: 27 predictors")
            ax.ticklabel_format(axis="x", style="plain", useOffset=False)
            ax.locator_params(axis="x", nbins=4)
        for ax in axes.flat:
            ax.spines[["top", "right"]].set_visible(False)
            ax.grid(axis="x", color="#E5E9ED", linewidth=.5)
            ax.set_axisbelow(True)
        caption = (
            "Historical coding and the unit of predictor importance. "
            "a, the 11 recovered original chart scores, with the legume category corrected to at least once/week; "
            "these saved scores are a separate historical result, not an exact refit. "
            "b, ranks of summed impurity importance versus whole-predictor held-out permutation importance in each new forest variant; "
            "smaller ranks indicate larger contributions. Summing impurity scores is not permutation importance. "
            "c–d, all 27 mean permutation contributions, with a common order and axis; whiskers are the five-fold minimum–maximum, not confidence intervals. "
            "Both variants are newly trained in the same 4,994 complete cases with the same inherited PSU-disjoint folds, "
            "weights and per-fold parameters selected in the principal forest's outer training data; no retuning occurs. "
            "Historical coding jointly changes water indices, fruit/vegetable-day factors, beverage units and categorical contrasts. "
            "Results evaluate this representation package, not each component separately or the original caret workflow."
        )
        paths = _save(fig, "historical_coding_comparison", destination, metadata, caption, names,
                      preserve_size=True, alt_text="Four panels connect the original eleven-term chart to whole-predictor impurity ranks and all twenty-seven held-out contributions under two matched coding schemes. Water is explicitly labelled; signed contributions and fold ranges are retained.")
        metadata["source_table_sha256"].update({name: hashlib.sha256((source/name).read_bytes()).hexdigest() for name in names})
        return paths
