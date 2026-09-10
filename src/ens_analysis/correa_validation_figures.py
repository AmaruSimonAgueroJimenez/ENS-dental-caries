"""Four-panel validation and sensitivity figures from aggregate tables only."""
from __future__ import annotations

from hashlib import sha256
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np
import pandas as pd


MODELS = ["decision_tree", "knn", "logistic", "random_forest", "svm",
          "spline_logistic", "hist_gradient_boosting"]
SENSITIVITY_MODELS = ["logistic", "spline_logistic", "random_forest"]
COLOURS = dict(zip(MODELS, ["#1F77B4", "#2CA02C", "#D62728", "#9467BD",
                            "#FF7F0E", "#168A8A", "#7F7F7F"], strict=True))
LABELS = dict(zip(MODELS, ["Decision tree", "Nearest neighbours", "Logistic regression",
                           "Random forest", "Support vector machine", "Spline logistic",
                           "Gradient boosting"], strict=True))
SHORT = dict(zip(MODELS, ["Tree", "KNN", "LR", "RF", "SVM", "Spline", "Boost"], strict=True))
MARKERS = dict(zip(MODELS, ["o", "s", "^", "D", "v", "P", "X"], strict=True))
LINES = dict(zip(MODELS, ["-", "--", "-.", ":", "-", "--", "-."], strict=True))
STYLE = {
    "font.family":"sans-serif", "font.sans-serif":["Arial", "DejaVu Sans"],
    "font.size":8, "axes.titlesize":9, "axes.titleweight":"bold",
    "axes.labelsize":8, "xtick.labelsize":7, "ytick.labelsize":7,
    "legend.fontsize":7, "text.color":"black", "axes.labelcolor":"black",
    "xtick.color":"black", "ytick.color":"black", "axes.edgecolor":"#777777",
    "axes.spines.top":False, "axes.spines.right":False, "axes.linewidth":.6,
    "figure.facecolor":"white", "axes.facecolor":"white", "savefig.facecolor":"white",
    "svg.fonttype":"none", "pdf.fonttype":42, "axes.axisbelow":True,
}
SUBGROUPS = [
    ("sex", "Female", "Sex: Female"), ("sex", "Male", "Sex: Male"),
    ("age_group", "15–24", "Age: 15–24"), ("age_group", "25–44", "Age: 25–44"),
    ("age_group", "45–64", "Age: 45–64"), ("age_group", "65+", "Age: 65+"),
    ("rural", "Urban", "Residence: Urban"), ("rural", "Rural", "Residence: Rural"),
    ("education", "Low", "Education: Low"),
    ("education", "Intermediate", "Education: Intermediate"),
    ("education", "High", "Education: High"),
]


def _read(source, name, required):
    path = source / name
    if not path.exists():
        return None
    table = pd.read_csv(path)
    missing = set(required) - set(table)
    if missing:
        raise ValueError(f"{name} lacks required figure columns: {sorted(missing)}")
    if table.empty:
        return None
    return table


def _complete(table, models, keys, name):
    selected = table.loc[table.model.isin(models)].copy()
    if set(selected.model) != set(models):
        raise ValueError(f"{name} must contain every displayed model")
    if selected.duplicated(keys).any():
        raise ValueError(f"{name} has duplicate displayed records for {keys}")
    return selected


def _grid(ax, axis="both"):
    ax.grid(axis=axis, color="#E5E5E5", linewidth=.55)
    ax.tick_params(length=2.5, width=.6, pad=3)


def _title(ax, title):
    ax.set_title(title, loc="left", pad=7)


def _legend(fig, models, *, location=(.5,.035), columns=3):
    handles = [Line2D([0],[0],color=COLOURS[m],marker=MARKERS[m],
                      linestyle=LINES[m],linewidth=1,markersize=3.4,label=LABELS[m])
               for m in models]
    fig.legend(handles=handles,loc="lower center",bbox_to_anchor=location,
               ncol=columns,frameon=False,handlelength=2.2,columnspacing=1.2,
               labelspacing=.6)


def _save_and_record(fig, name, destination, metadata, caption, sources, alt_text):
    # Imported at use time; figures.make_figures can import this module locally.
    from .figures import _save

    return _save(fig,name,destination,metadata,caption,sources,
                 preserve_size=True,alt_text=alt_text)


def _validation(tables, destination, metadata):
    roc = _complete(tables["roc.csv"],MODELS,["model","fpr","tpr"],"ROC table")
    calibration = _complete(tables["calibration.csv"],MODELS,["model","predicted"],"Calibration table")
    folds = _complete(tables["outer_fold_performance.csv"],MODELS,["model","fold"],"Fold table")
    performance = _complete(tables["model_performance.csv"],MODELS,["model","weighting"],"Performance table")
    if performance.n.nunique()!=1:
        raise ValueError("All displayed validation models must share the same participant count")
    cohort_n=int(performance.n.iloc[0])
    fold_ids = sorted(folds.fold.unique())
    for model in MODELS:
        if sorted(folds.loc[folds.model.eq(model),"fold"]) != fold_ids:
            raise ValueError("Every displayed model must share the same held-out folds")
    fig = plt.figure(figsize=(180/25.4,210/25.4))
    a = fig.add_axes([.10,.57,.38,.35])
    b = fig.add_axes([.60,.57,.37,.35])
    c = fig.add_axes([.10,.18,.38,.28])
    d = fig.add_axes([.60,.18,.37,.28])
    for model in MODELS:
        data = roc.loc[roc.model.eq(model)]
        a.plot(data.fpr,data.tpr,color=COLOURS[model],linestyle=LINES[model],linewidth=1,
               marker=MARKERS[model],markersize=2.5,markevery=max(1,len(data)//7))
        data = calibration.loc[calibration.model.eq(model)].sort_values("predicted")
        b.plot(data.predicted,data.observed,color=COLOURS[model],linestyle=LINES[model],
               marker=MARKERS[model],markersize=3,linewidth=.85)
        data = folds.loc[folds.model.eq(model)].sort_values("fold")
        c.plot(data.fold,data.auc,color=COLOURS[model],linestyle=LINES[model],
               marker=MARKERS[model],markersize=3.5,linewidth=1)
    for ax in [a,b]:
        ax.plot([0,1],[0,1],color="black",linewidth=.7,linestyle=(0,(3,3)),zorder=0)
        ax.set(xlim=(0,1),ylim=(0,1),xticks=np.linspace(0,1,6),yticks=np.linspace(0,1,6))
        _grid(ax)
    a.set(xlabel="False-positive rate",ylabel="True-positive rate")
    b.set(xlabel="Mean predicted probability",ylabel="Observed caries prevalence")
    _title(a,"a  ROC curves")
    _title(b,"b  Calibration")
    c.axhline(.5,color="black",linewidth=.7,linestyle=(0,(3,3)))
    c.set(xlabel="Held-out fold",ylabel="AUC",xticks=fold_ids,xlim=(min(fold_ids)-.2,max(fold_ids)+.2))
    # This point-and-line panel uses an explicit local scale, never a bar baseline.
    c.set_ylim(min(.45,np.floor(folds.auc.min()*20)/20),
               max(.70,np.ceil(folds.auc.max()*20)/20))
    _grid(c)
    _title(c,"c  Variation across folds")
    x = np.arange(len(MODELS))
    for offset,weighting,hatch in [(-.19,"Survey weighted",None),(.19,"Unweighted","///")]:
        data = performance.loc[performance.weighting.eq(weighting)].set_index("model").reindex(MODELS)
        if data.auc.isna().any():
            raise ValueError(f"Missing AUC for {weighting} comparison")
        d.bar(x+offset,data.auc,width=.36,color=[COLOURS[m] for m in MODELS],
              edgecolor="black",linewidth=.35,hatch=hatch)
    d.axhline(.5,color="black",linewidth=.7,linestyle=(0,(3,3)))
    d.set(xlabel="Model",ylabel="AUC",xticks=x,xticklabels=[SHORT[m] for m in MODELS],ylim=(0,1))
    d.tick_params(axis="x",labelrotation=45)
    d.legend(handles=[Patch(facecolor="white",edgecolor="black",label="Survey weighted"),
                      Patch(facecolor="white",edgecolor="black",hatch="///",label="Unweighted")],
             loc="upper left",frameon=False,handlelength=1.4,borderaxespad=.4)
    _grid(d,"y")
    _title(d,"d  Weighting comparison")
    _legend(fig,MODELS)
    sources=["roc.csv","calibration.csv","outer_fold_performance.csv","model_performance.csv"]
    caption=(f"Validation of the seven full-feature algorithms using fixed out-of-fold predictions for "
             f"{cohort_n:,} dentate participants. (a) Survey-weighted ROC curves. (b) Survey-weighted observed "
             "prevalence versus mean predicted probability within model-specific quantile bins; lines "
             "join bin summaries and are not fitted calibration functions. The diagonal denotes chance "
             "discrimination in a and perfect calibration in b. (c) Survey-weighted AUC in each of the "
             "five held-out folds; the explicitly labelled local AUC scale applies only to this point-and-line "
             "panel. These are descriptive fold estimates, not uncertainty intervals. (d) Pooled survey-weighted "
             "and unweighted AUC point estimates on a zero-baseline bar scale. Horizontal dashed lines denote "
             "AUC=0.5. No confidence intervals or tests of differences are displayed in this figure. "
             "LR, logistic regression; RF, random forest; KNN, nearest neighbours; SVM, support vector machine.")
    return _save_and_record(fig,"correa_validation",destination,metadata,caption,sources,
        "Four panels compare seven models: survey-weighted ROC curves and calibration, AUC across five "
        "validation folds, and pooled weighted versus unweighted AUC. All values are descriptive estimates.")


def _sensitivity(tables, destination, metadata):
    performance = _complete(tables["model_performance.csv"].loc[lambda x:x.weighting.eq("Survey weighted")],
                            SENSITIVITY_MODELS,["model"],"Primary cohort table").set_index("model")
    subset = _complete(tables["prediction_subset_sensitivity.csv"].loc[
        lambda x:x.subset.eq("Exact manuscript subset")],SENSITIVITY_MODELS,["model"],
        "Original manuscript subset table").set_index("model")
    subgroup = _complete(tables["subgroup_performance.csv"],SENSITIVITY_MODELS,
                         ["model","variable","level"],"Subgroup table")
    fig = plt.figure(figsize=(180/25.4,210/25.4))
    a=fig.add_axes([.10,.64,.38,.29]); b=fig.add_axes([.60,.64,.37,.29])
    c=fig.add_axes([.255,.14,.29,.40]); d=fig.add_axes([.635,.14,.335,.40],sharey=c)
    x=np.arange(len(SENSITIVITY_MODELS))
    for ax,metric,title,limit in [(a,"auc","a  Cohort sensitivity: AUC",1),
                                 (b,"brier","b  Cohort sensitivity: Brier",.30)]:
        for offset,data,hatch in [(-.19,performance,None),(.19,subset,"///")]:
            values=data.loc[SENSITIVITY_MODELS,metric]
            ax.bar(x+offset,values,width=.36,color=[COLOURS[m] for m in SENSITIVITY_MODELS],
                   edgecolor="black",linewidth=.35,hatch=hatch)
        ax.set(xticks=x,xticklabels=["Logistic\nregression","Spline\nlogistic","Random\nforest"],
               ylabel="AUC" if metric=="auc" else "Brier score",ylim=(0,limit))
        _grid(ax,"y"); _title(ax,title)
    a.axhline(.5,color="black",linestyle=(0,(3,3)),linewidth=.7)
    n_primary=int(performance.n.iloc[0]); n_subset=int(subset.n.iloc[0])
    a.legend(handles=[Patch(facecolor="white",edgecolor="black",label=f"Primary cohort (n={n_primary:,})"),
                      Patch(facecolor="white",edgecolor="black",hatch="///",label=f"Original subset (n={n_subset:,})")],
             loc="upper left",frameon=False,handlelength=1.3,borderaxespad=.35)
    y=np.arange(len(SUBGROUPS),dtype=float)
    for j,model in enumerate(SENSITIVITY_MODELS):
        model_data=subgroup.loc[subgroup.model.eq(model)].set_index(["variable","level"])
        keys=pd.MultiIndex.from_tuples([(v,l) for v,l,_ in SUBGROUPS],names=["variable","level"])
        values=model_data.reindex(keys)
        if values[["auc","brier"]].isna().any().any():
            raise ValueError("Subgroup figure requires all eleven descriptive subgroup estimates")
        for ax,metric in [(c,"auc"),(d,"brier")]:
            position=y+(j-1)*.24
            ax.barh(position,values[metric],height=.22,color=COLOURS[model],edgecolor="white",linewidth=.2)
            ax.plot(values[metric],position,linestyle="none",marker=MARKERS[model],
                    markersize=2.5,color=COLOURS[model],markeredgecolor="black",markeredgewidth=.25)
    c.set(yticks=y,yticklabels=[label for _,_,label in SUBGROUPS],xlim=(0,1),xlabel="AUC")
    d.set(xlim=(0,.30),xlabel="Brier score")
    c.set_ylim(len(SUBGROUPS)-.5,-.5)
    d.tick_params(axis="y",labelleft=False)
    c.axvline(.5,color="black",linewidth=.7,linestyle=(0,(3,3)))
    for ax in [c,d]:
        ax.set_xticks(np.linspace(0,1,5) if ax is c else [0,.1,.2,.3])
        _grid(ax,"x")
        for boundary in [1.5,5.5,7.5]:
            ax.axhline(boundary,color="#BBBBBB",linewidth=.5)
    _title(c,"c  AUC by subgroup"); _title(d,"d  Brier score by subgroup")
    _legend(fig,SENSITIVITY_MODELS,location=(.5,.035),columns=3)
    sources=["model_performance.csv","prediction_subset_sensitivity.csv","subgroup_performance.csv"]
    caption=(f"Descriptive sensitivity checks for linear logistic regression, spline logistic regression "
             f"and random forest. (a,b) Survey-weighted AUC and Brier scores in the primary dentate cohort "
             f"(n={n_primary:,}) and its overlapping original-manuscript subset with observed education and "
             f"water intake (n={n_subset:,}). The same held-out predictions are restricted to the subset; "
             "models are not refitted or retuned, and these are not independent cohorts or a reproduction "
             "of the original R model estimates. (c,d) Survey-weighted AUC and Brier scores within sex, age, "
             "residence and education groups, in the same row order in both panels. These three models "
             "are the prespecified subgroup-reporting models. All bars start at zero; the dashed line "
             "denotes AUC=0.5. All panels show point estimates without confidence intervals. No subgroup "
             "heterogeneity test or claim of differential performance is implied; overlapping subgroup "
             "partitions and unequal sample sizes must be considered. Higher AUC and lower Brier scores "
             "are favourable. Age is in years; subgroup sample sizes are available in subgroup_performance.csv.")
    return _save_and_record(fig,"correa_sensitivity",destination,metadata,caption,sources,
        "Four descriptive panels compare AUC and Brier scores for logistic regression, spline logistic and "
        "random forest in the full dentate cohort, the original complete subset and eleven demographic subgroups.")


def make_correa_validation_figures(source, destination, metadata):
    """Append two four-panel figures and provenance; return their PNG/SVG/PDF paths.

    Only aggregate CSVs are read. Missing input groups skip their corresponding
    figure, while malformed or incomplete existing input tables fail explicitly.
    """
    source,destination=Path(source),Path(destination)
    destination.mkdir(parents=True,exist_ok=True)
    requirements={
        "roc.csv":["model","fpr","tpr"],
        "calibration.csv":["model","predicted","observed"],
        "outer_fold_performance.csv":["model","fold","auc","test_weight_sum"],
        "model_performance.csv":["model","weighting","n","auc","brier"],
        "prediction_subset_sensitivity.csv":["model","subset","n","auc","brier"],
        "subgroup_performance.csv":["model","variable","level","n","auc","brier"],
    }
    tables={name:_read(source,name,columns) for name,columns in requirements.items()}
    metadata.setdefault("figures",[])
    metadata.setdefault("source_table_sha256",{}).update({
        name:sha256((source/name).read_bytes()).hexdigest() for name,table in tables.items() if table is not None})
    metadata.setdefault("missing_or_empty_source_tables",[])
    for name,table in tables.items():
        if table is None and name not in metadata["missing_or_empty_source_tables"]:
            metadata["missing_or_empty_source_tables"].append(name)
    paths=[]
    with plt.rc_context(STYLE):
        if all(tables[name] is not None for name in ["roc.csv","calibration.csv",
                                                    "outer_fold_performance.csv","model_performance.csv"]):
            paths+=_validation(tables,destination,metadata)
        if all(tables[name] is not None for name in ["model_performance.csv",
                                                    "prediction_subset_sensitivity.csv","subgroup_performance.csv"]):
            paths+=_sensitivity(tables,destination,metadata)
    return paths
