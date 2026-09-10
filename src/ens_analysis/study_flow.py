"""Editable, publication-size cohort and validation schematic from aggregates."""
from __future__ import annotations

import csv
import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, Rectangle


SOURCES = ["data_audit.json", "sample_flow.csv", "model_manifest.json"]
TEAL = "#14616B"


def make_study_flow(tables_dir, figures_dir):
    """Write study_flow PNG/SVG/PDF and return one figure-metadata entry.

    All three aggregate sources are required. Incompatible counts fail before
    drawing; callers supporting partial figure fixtures may check their presence.
    Coordinates are millimetres, with custom orthogonal connectors and a nested
    training panel. Every label remains vector text in SVG and PDF.
    """
    tables, destination = Path(tables_dir), Path(figures_dir)
    audit = json.loads((tables / SOURCES[0]).read_text())
    with (tables / SOURCES[1]).open(newline="") as handle:
        rows = list(csv.DictReader(handle))
    if len({row["stage"] for row in rows}) != len(rows):
        raise ValueError("Duplicate study-flow stages")
    flow = {row["stage"]: int(row["n"]) for row in rows}
    if flow != audit["sample_flow"]:
        raise ValueError("Study-flow counts disagree between audit and table")
    model = json.loads((tables / SOURCES[2]).read_text())
    missing = (flow["dentate_missing_education"] + flow["dentate_missing_water"]
               - flow["dentate_missing_both_education_water"])
    checks = [
        flow["all_participants"] == flow["dental_examination"] + flow["no_complete_dental_examination"],
        flow["dental_examination"] == flow["dentate_eligible"] + flow["edentulous"],
        flow["dentate_eligible"] == flow["paper_complete"] + missing,
        flow["paper_complete"] == flow["paper_cases"] + flow["paper_non_cases"],
        flow["dentate_eligible"] == flow["dentate_cases"] + flow["dentate_non_cases"],
        flow["dentate_outcome_missing"] == 0,
        model["n_participants"] == flow["dentate_eligible"],
        {"random_forest", "spline_logistic"}.issubset(model["models"]),
        all(value >= 0 for value in flow.values()),
    ]
    if not all(checks):
        raise ValueError("Incompatible cohort partitions or model cohort in study flow")
    n = lambda key: f'{flow[key]:,}'
    variants = len(model["models"])
    outer, inner = (int(model["settings"][key]) for key in ["outer_folds", "inner_folds"])
    destination.mkdir(parents=True, exist_ok=True)
    style = {"font.family": "sans-serif", "font.sans-serif": ["Arial", "DejaVu Sans"],
             "font.size": 7.5, "text.color": "black", "svg.fonttype": "none",
             "pdf.fonttype": 42, "figure.facecolor": "white", "savefig.facecolor": "white"}
    with plt.rc_context(style):
        fig = plt.figure(figsize=(180 / 25.4, 155 / 25.4))
        ax = fig.add_axes([0, 0, 1, 1], xlim=(0, 180), ylim=(0, 155))
        ax.set_axis_off()
        boxes = []

        def box(x, y, width, height, label, *, emphasis=False, dashed=False, size=7.5):
            patch = Rectangle((x, y), width, height, linewidth=.85,
                              edgecolor=TEAL if emphasis else "black",
                              facecolor="#EEF6F5" if emphasis else "white",
                              linestyle=(0, (3, 2)) if dashed else "solid", zorder=2)
            ax.add_patch(patch)
            text = ax.text(x + width / 2, y + height / 2, label,
                           ha="center", va="center", fontsize=size, linespacing=1.3, zorder=3)
            boxes.append((patch, text))

        def arrow(start, end):
            ax.add_patch(FancyArrowPatch(start, end, arrowstyle="-|>", mutation_scale=7,
                                        linewidth=.8, color="black", shrinkA=.4, shrinkB=.5,
                                        zorder=1))

        ax.text(5, 147, "A  Study cohort", fontsize=9, fontweight="bold")
        ax.text(96, 147, "B  Model development and evaluation", fontsize=9, fontweight="bold")
        ax.plot([90, 90], [7, 142], color="#B8CDCF", linewidth=.6)

        box(5, 128, 49, 14, f"ENS 2016–2017 release\nn = {n('all_participants')}")
        box(5, 98, 49, 16, f"Complete dental examination\nn = {n('dental_examination')}")
        arrow((29.5, 128), (29.5, 114))
        arrow((29.5, 121), (60, 121))
        box(60, 112, 25, 18, f"No complete\ndental examination\nn = {n('no_complete_dental_examination')}", size=7)
        box(5, 64, 49, 19, f"Primary modelling cohort\nDentate participants\nn = {n('dentate_eligible')}", emphasis=True)
        arrow((29.5, 98), (29.5, 83))
        arrow((29.5, 90.5), (60, 90.5))
        box(60, 82, 25, 17, f"Edentulous\nn = {n('edentulous')}")
        box(5, 27, 80, 30,
            f"Original-manuscript reference subset\nn = {n('paper_complete')}; caries cases = {n('paper_cases')}\n"
            f"Observed education and water\n{missing:,} others remain in the modelling cohort", dashed=True)
        ax.plot([54, 87, 87, 85], [73.5, 73.5, 42, 42], color="black",
                linewidth=.8, linestyle=(0, (3, 2)), zorder=1)
        ax.text(5, 16, "Dashed connector: overlapping reference subset,\nnot a further modelling exclusion.",
                ha="left", va="center", fontsize=7.2, linespacing=1.35)

        box(96, 128, 79, 14, f"Same {n('dentate_eligible')} participants\n{variants} model variants", emphasis=True)
        arrow((135.5, 128), (135.5, 121))
        box(96, 108, 79, 13, f"{outer} outer folds grouped by PSU\nHold out one fold at a time")
        arrow((135.5, 108), (135.5, 101))
        ax.add_patch(Rectangle((96, 70), 79, 31, edgecolor=TEAL,
                               facecolor="#F6FAFA", linewidth=.85, zorder=1))
        ax.text(135.5, 97, "Within each outer training set", fontsize=7.7,
                fontweight="bold", ha="center", va="center")
        box(101, 74, 69, 18,
            f"{inner} inner folds grouped by PSU\nFit imputation and preprocessing\nTune by survey-weighted log loss")
        arrow((135.5, 70), (135.5, 64))
        box(96, 50, 79, 14, "Refit selected pipeline on outer training data\nPredict the untouched outer evaluation fold", size=7.4)
        arrow((135.5, 50), (135.5, 44))
        box(96, 31, 79, 13,
            f"{n('dentate_eligible')} out-of-fold probabilities\nfor each of {variants} model variants", emphasis=True)
        arrow((135.5, 31), (135.5, 25))
        box(96, 7, 79, 18,
            f"Weighted performance: all {variants} variants\nIndividual and grouped permutation:\nrandom forest and spline logistic")

        # Check every boxed label at the final physical publication dimensions.
        fig.canvas.draw()
        renderer = fig.canvas.get_renderer()
        for patch, text in boxes:
            bounds, content = patch.get_window_extent(renderer), text.get_window_extent(renderer)
            padding = 1.1 * fig.dpi / 25.4
            if content.width > bounds.width - 2 * padding or content.height > bounds.height - 2 * padding:
                plt.close(fig)
                raise ValueError(f"Study-flow text does not fit its box: {text.get_text()!r}")
        paths = [destination / f"study_flow.{extension}" for extension in ["png", "svg", "pdf"]]
        for path in paths:
            fig.savefig(path, dpi=300, bbox_inches=None)
        plt.close(fig)

    caption = (
        "Study cohort and nested validation. Panel A distinguishes the primary dentate modelling cohort "
        "from the overlapping original-manuscript reference subset, which requires observed education "
        "and water. Participants outside that reference subset remain eligible for modelling. Panel B "
        "shows training-only preprocessing and tuning with PSU-disjoint folds. Each participant receives "
        "one out-of-fold probability per fitted model variant. Survey-weighted performance is evaluated "
        "for all variants; individual and joint-group permutation analyses use random forest and spline "
        "logistic regression. ENS, Chilean National Health Survey; PSU, primary sampling unit."
    )
    return {
        "name": "study_flow", "files": [path.name for path in paths],
        "caption": caption, "source_tables": SOURCES,
        "alt_text": (
            f"Two-panel flow. {n('all_participants')} survey participants include {n('dental_examination')} "
            f"with complete dental examinations and {n('no_complete_dental_examination')} without. "
            f"After excluding {n('edentulous')} edentulous participants, {n('dentate_eligible')} enter modelling. "
            f"The overlapping manuscript subset contains {n('paper_complete')}, including {n('paper_cases')} "
            f"caries cases. Modelling retains the {missing} with missing education or water. "
            f"Nested {outer}-outer/{inner}-inner-fold PSU-grouped validation yields one held-out probability "
            f"per participant for each of {variants} model variants. Permutation evaluates two model families."
        ),
        "canvas_width_mm": 180, "canvas_height_mm": 155, "png_dpi": 300,
        "physical_canvas_preserved": True,
        "svg_text": "Editable text; requires Arial or the specified fallback font",
        "pdf_text": "Embedded TrueType fonts (Type 42)",
    }
