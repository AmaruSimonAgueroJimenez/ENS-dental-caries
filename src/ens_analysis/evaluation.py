"""Evaluation of held-out probabilities, never of apparent training performance."""
from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.optimize import minimize, brentq
from scipy.special import expit, logit
from sklearn.metrics import roc_auc_score, average_precision_score


def probability_metrics(y, probability, weights=None, calibration=True):
    y, p = np.asarray(y, float), np.asarray(probability, float)
    w = np.ones(len(y)) if weights is None else np.asarray(weights, float)
    good = np.isfinite(y) & np.isfinite(p) & np.isfinite(w) & (w > 0)
    y, p, w = y[good], p[good], w[good]
    if len(y) == 0 or not np.isin(y, [0, 1]).all():
        raise ValueError("Binary observed outcomes and nonempty valid evaluation data required")
    if ((p<0)|(p>1)).any():
        raise ValueError("Predicted probabilities must lie between zero and one")
    pc=np.clip(p,1e-7,1-1e-7)
    w = w / w.sum()
    positive = p >= 0.5
    tp, tn = np.sum(w * y * positive), np.sum(w * (1-y) * ~positive)
    fp, fn = np.sum(w * (1-y) * positive), np.sum(w * y * ~positive)
    ratio = lambda a, b: float(a / b) if b > 0 else np.nan
    result = {
        "n": len(y), "events": int(y.sum()), "prevalence": float(w @ y),
        "auc": float(roc_auc_score(y, p, sample_weight=w)) if len(np.unique(y)) == 2 else np.nan,
        "average_precision": float(average_precision_score(y, p, sample_weight=w)),
        "brier": float(w @ (y-p)**2),
        "log_loss": float(-w @ (y*np.log(pc)+(1-y)*np.log1p(-pc))),
        "sensitivity": ratio(tp, tp+fn), "specificity": ratio(tn, tn+fp),
        "ppv": ratio(tp, tp+fp), "npv": ratio(tn, tn+fn),
        "accuracy": float(tp+tn),
        "balanced_accuracy": (ratio(tp, tp+fn)+ratio(tn, tn+fp))/2,
        "mean_prediction": float(w @ p),
    }
    if calibration:
        lp = logit(pc)
        result.update(calibration_intercept=np.nan,calibration_slope=np.nan,
                      joint_calibration_intercept=np.nan,calibration_status='Not identifiable')
        if len(np.unique(y)) < 2:
            return result
        result['calibration_intercept']=float(brentq(lambda b: w@(expit(b+lp)-y),-50.,50.))
        if np.ptp(lp) < 1e-10:
            result['calibration_status']='Slope not identifiable for constant predictions'
            return result
        X=np.column_stack([np.ones(len(y)),lp])
        def loss(par):
            eta = X@par
            return float(w @ (np.logaddexp(0, eta)-y*eta))
        def gradient(par):
            return X.T@(w*(expit(X@par)-y))
        def hessian(par):
            q=expit(X@par)
            return X.T@((w*q*(1-q))[:,None]*X)
        fit = minimize(loss, [0., 1.], jac=gradient,hess=hessian,
                       method="trust-exact",options={'gtol':1e-9,'maxiter':100})
        if ((fit.success or np.linalg.norm(gradient(fit.x))<1e-7)
                and np.max(np.abs(fit.x))<50
                and np.linalg.eigvalsh(hessian(fit.x)).min()>1e-10):
            result.update(calibration_slope=float(fit.x[1]),
                          joint_calibration_intercept=float(fit.x[0]),
                          calibration_status='Estimated')
        else:
            result['calibration_status']='No stable finite calibration fit'
    return result


def replicate_multipliers(design, evaluation, replicates=1000, seed=20260910):
    """Rao–Wu-style rescaled PSU bootstrap within original strata.

    The complete examination-phase design, including out-of-domain PSUs, is used.
    Draw m-1 PSUs with replacement, and rescale by m/(m-1). A singleton PSU is
    kept fixed: its unidentified contribution is NOT estimated by this bootstrap.
    Model fits and the chosen CV split remain fixed; these are conditional CIs.
    """
    rng = np.random.default_rng(seed)
    units = design[["stratum", "psu"]].drop_duplicates().reset_index(drop=True)
    keys = pd.MultiIndex.from_frame(units)
    indices = keys.get_indexer(pd.MultiIndex.from_frame(evaluation[["stratum", "psu"]]))
    if (indices < 0).any():
        raise ValueError("Evaluation PSU missing from full survey design")
    draws = np.ones((replicates, len(units)), dtype=np.float32)
    for _, block in units.groupby("stratum", sort=False):
        ix = block.index.to_numpy()
        m = len(ix)
        if m > 1:
            counts = rng.multinomial(m-1, np.full(m, 1/m), size=replicates)
            draws[:, ix] = counts * m/(m-1)
    return draws[:, indices]


def evaluate_oof(oof, design, replicates=1000, seed=20260910):
    """Return aggregate metrics and paired conditional bootstrap differences."""
    index = ["row_index", "participant_id", "psu", "stratum", "y", "weight", "fold"]
    wide = oof.pivot(index=index, columns="model", values="p").reset_index()
    if wide.drop(columns=index).isna().any().any():
        raise ValueError("All models must evaluate the same participants")
    models = [c for c in wide if c not in index]
    mult = replicate_multipliers(design, wide, replicates, seed)
    y, w = wide.y.to_numpy(), wide.weight.to_numpy()
    metrics, bootstrap = [], {}
    for name in models:
        p = wide[name].to_numpy()
        point = probability_metrics(y, p, w)
        rows = []
        for draw in mult:
            wb = w*draw
            wn = wb / wb.sum()
            rows.append([roc_auc_score(y, p, sample_weight=wb),
                         float(wn @ (y-p)**2),
                         float(-wn @ (y*np.log(np.clip(p,1e-7,1-1e-7)) +
                                      (1-y)*np.log1p(-np.clip(p,1e-7,1-1e-7))))])
        bootstrap[name] = np.asarray(rows)
        for j, metric in enumerate(["auc", "brier", "log_loss"]):
            point[metric+"_low"], point[metric+"_high"] = np.quantile(bootstrap[name][:, j], [.025, .975])
        metrics.append({"model":name, "weighting":"Survey weighted", **point})
        metrics.append({"model":name, "weighting":"Unweighted", **probability_metrics(y, p)})
    return pd.DataFrame(metrics), bootstrap, wide


def paired_differences(bootstrap, point_metrics, pairs):
    points = point_metrics.query("weighting == 'Survey weighted'").set_index("model")
    records = []
    for larger, smaller, label in pairs:
        if larger not in bootstrap or smaller not in bootstrap:
            continue
        difference = bootstrap[larger]-bootstrap[smaller]
        for j, metric in enumerate(["auc", "brier", "log_loss"]):
            low, high = np.quantile(difference[:,j], [.025,.975])
            records.append({"comparison":label,"model":larger,"reference":smaller,
                            "metric":metric,"difference":float(points.loc[larger,metric]-points.loc[smaller,metric]),
                            "ci_low":low,"ci_high":high})
    return pd.DataFrame(records)


def calibration_bins(y, p, w, bins=10):
    data = pd.DataFrame({"y":y,"p":p,"w":w})
    data["bin"] = pd.qcut(data.p, bins, duplicates="drop")
    records=[]
    for label, block in data.groupby("bin", observed=True):
        ww=block.w.to_numpy()
        records.append({"bin":str(label),"n":len(block),
                        "predicted":np.average(block.p,weights=ww),
                        "observed":np.average(block.y,weights=ww)})
    return pd.DataFrame(records)
