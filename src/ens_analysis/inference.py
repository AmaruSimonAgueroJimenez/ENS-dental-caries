"""Design-based descriptive and association analyses, separate from prediction."""
from __future__ import annotations

import warnings
import re
import numpy as np
import pandas as pd
from patsy import build_design_matrices
from scipy.special import expit, logit
from scipy.stats import t

from .survey import survey_mean, survey_glm


CURVE_CI_METHOD = 'delta_method_beta_only_conditional_on_fixed_observed_covariate_composition'


def water_spline_contrast(result, analytical, reference=2.0, comparison=6.0):
    """Conditional odds ratio at identical covariates, using the fitted spline.

    The current additive models have no water interactions, so the difference
    in model-matrix rows is identical for every covariate vector. This is an
    adjusted odds association, not a ratio of standardized prevalences or a
    causal effect. Require observations at both specified water values.
    """
    if result['family'] != 'binomial' or not result['valid_inference']:
        raise ValueError('The water odds contrast requires a valid binomial fit.')
    if not reference < comparison:
        raise ValueError('The comparison value must exceed the reference value.')
    n_reference = int(analytical.water.eq(reference).sum())
    n_comparison = int(analytical.water.eq(comparison).sum())
    record = {
        'model': 'Water spline', 'measure': 'Conditional odds ratio',
        'contrast': f'{comparison:g} versus {reference:g} reported glasses/day',
        'reference_water': reference, 'comparison_water': comparison,
        'n': len(analytical), 'n_at_reference': n_reference,
        'n_at_comparison': n_comparison,
        'n_water_2': int(analytical.water.eq(2).sum()),
        'n_water_6': int(analytical.water.eq(6).sum()),
        'df': result['df'], 'confidence': result['confidence'],
        'supported': n_reference > 0 and n_comparison > 0,
        'method': 'same_covariates_spline_log_odds_contrast_with_design_covariance',
        'estimate': np.nan, 'ci_low': np.nan, 'ci_high': np.nan,
        'log_odds_ratio': np.nan, 'se_log_odds_ratio': np.nan, 'p_value': np.nan,
    }
    if not record['supported']:
        return record
    at_reference = analytical.copy(); at_reference['water'] = reference
    at_comparison = analytical.copy(); at_comparison['water'] = comparison
    info = result['design_info']
    X0 = np.asarray(build_design_matrices([info], at_reference)[0])
    X1 = np.asarray(build_design_matrices([info], at_comparison)[0])
    differences = X1 - X0
    if not np.allclose(differences, differences[0], rtol=1e-10, atol=1e-12):
        raise ValueError('A single conditional water OR requires an additive model without water interactions.')
    delta = differences[0]
    beta = result['coefficients'].coef.to_numpy()
    log_or = float(delta @ beta)
    variance = float(delta @ np.asarray(result['covariance']) @ delta)
    if not np.isfinite(variance) or variance < -1e-10:
        raise ValueError('The water contrast has an invalid design-based variance.')
    se = float(np.sqrt(max(0, variance)))
    critical = t.ppf((1 + result['confidence']) / 2, result['df'])
    record.update(
        estimate=float(np.exp(log_or)),
        ci_low=float(np.exp(log_or - critical * se)),
        ci_high=float(np.exp(log_or + critical * se)),
        log_odds_ratio=log_or, se_log_odds_ratio=se,
        p_value=float(2 * t.sf(abs(log_or / se), result['df'])) if se > 0 else np.nan,
    )
    return record


def descriptive_tables(frame):
    design=frame.loc[frame.weight.notna() & (frame.weight>0)].copy()
    eligible=design.eligible & design.caries.notna()
    records=[]
    groups=[('Overall','All',eligible)]
    for var in ['sex','rural','education','household_water_source']:
        for val in sorted(design.loc[eligible,var].dropna().unique()):
            groups.append((var,str(val),eligible & design[var].eq(val)))
    ages=pd.cut(design.age,[14,24,44,64,np.inf],labels=['15–24','25–44','45–64','65+'])
    for val in ages.cat.categories:
        groups.append(('age',str(val),eligible & ages.eq(val)))
    watergroup=pd.cut(design.water,[-.1,0,2,4,6,10,np.inf],
                      labels=['0','>0–2','>2–4','>4–6','>6–10','>10'])
    for val in watergroup.cat.categories:
        groups.append(('water',str(val),eligible & watergroup.eq(val)))
    for var,level,mask in groups:
        if mask.sum()==0: continue
        with warnings.catch_warnings():
            warnings.simplefilter('ignore', UserWarning)
            est=survey_mean(design.caries,design.weight,design.stratum,design.psu,domain=mask)
        records.append({'variable':var,'level':level,'n':int(mask.sum()),
                        'events':int(design.loc[mask,'caries'].sum()),
                        'unweighted_prevalence':float(design.loc[mask,'caries'].mean()),
                        **{k:est[k] for k in ['estimate','se','ci_low','ci_high','df']}})
    numeric=[]
    for var in ['age','teeth_remaining','decayed_teeth','water','soda_frequency','juice_frequency']:
        with warnings.catch_warnings():
            warnings.simplefilter('ignore',UserWarning)
            est=survey_mean(design[var],design.weight,design.stratum,design.psu,domain=eligible)
        numeric.append({'variable':var,**{k:est[k] for k in ['estimate','se','ci_low','ci_high','df','n']},
                        'median_unweighted':design.loc[eligible,var].median(),
                        'minimum':design.loc[eligible,var].min(),'maximum':design.loc[eligible,var].max()})
    return pd.DataFrame(records),pd.DataFrame(numeric)


def run_associations(frame, legacy_frequencies=None):
    """Adjusted association models with complete-case inference and survey scores.

    Optionally append a sensitivity using original-DTA beverage frequencies.
    ``legacy_frequencies`` must contain unique participant_id, soda_frequency
    and juice_frequency columns, with frequencies already in times/day (the
    original weekly fields divided by seven). Matching is by identifier, never
    row order. The seven primary models and their input frame remain unchanged.
    """
    design=frame.loc[frame.weight.notna() & (frame.weight>0)].copy()
    social='C(sex) + C(region) + C(rural) + C(education) + C(indigenous) + C(born_chile)'
    core='bs(age, df=4, degree=3) + bs(teeth_remaining, df=4, degree=3) + '+social
    drinks='bs(soda_frequency, df=3, degree=3) + bs(juice_frequency, df=3, degree=3)'
    extended='C(household_water_source) + C(rural_water_consumption) + C(dental_visit)'
    high_water=design.loc[design.eligible,'water'].quantile(.99)
    specs=[
        ('Primary linear water','caries ~ water + '+core+' + '+drinks,'binomial',design.eligible),
        ('Water spline','caries ~ bs(water, df=4, degree=3) + '+core+' + '+drinks,'binomial',design.eligible),
        ('Additional water context and dental attendance','caries ~ water + '+core+' + '+drinks+' + '+extended,'binomial',design.eligible),
        ('Age at least 18','caries ~ water + '+core+' + '+drinks,'binomial',design.eligible & design.age.ge(18)),
        ('Exclude water above 99th percentile','caries ~ water + '+core+' + '+drinks,'binomial',design.eligible & design.water.le(high_water)),
        ('Without remaining teeth','caries ~ water + bs(age, df=4, degree=3) + '+social+' + '+drinks,'binomial',design.eligible),
        ('Modified Poisson prevalence ratio','caries ~ water + '+core+' + '+drinks,'poisson',design.eligible),
    ]
    specs=[(*spec,design) for spec in specs]
    if legacy_frequencies is not None:
        fields=['participant_id','soda_frequency','juice_frequency']
        missing=set(fields)-set(legacy_frequencies.columns)
        if missing:
            raise ValueError(f'Original beverage frequencies lack required columns: {sorted(missing)}')
        legacy=legacy_frequencies.loc[:,fields].copy()
        if legacy.participant_id.isna().any() or legacy.participant_id.duplicated().any():
            raise ValueError('Original beverage frequencies require unique nonmissing participant IDs')
        if design.participant_id.isna().any() or design.participant_id.duplicated().any():
            raise ValueError('Examination design requires unique nonmissing participant IDs')
        if not design.participant_id.isin(legacy.participant_id).all():
            raise ValueError('Original beverage frequencies do not cover every examination-design participant')
        indexed=legacy.set_index('participant_id')
        legacy_design=design.copy()
        for feature in ['soda_frequency','juice_frequency']:
            values=pd.to_numeric(indexed[feature],errors='raise')
            observed=values.dropna().to_numpy(dtype=float)
            if not np.isfinite(observed).all() or (observed<0).any():
                raise ValueError('Original beverage frequencies must be finite nonnegative times/day or missing')
            legacy_design[feature]=legacy_design.participant_id.map(values)
        specs.append(('Original 28-day beverage conversion',
                      'caries ~ water + '+core+' + '+drinks,'binomial',
                      legacy_design.eligible,legacy_design))
    coefficients=[]; water_effects=[]; water_contrasts=[]; diagnostics=[]; curve=[]
    for name,formula,family,domain,model_design in specs:
        # Stateful Patsy splines must never see missing raw values: otherwise
        # their estimated knots become NaN before Patsy's row removal occurs.
        required=[col for col in model_design if re.search(r'\b'+re.escape(col)+r'\b',formula)]
        complete_domain=domain & model_design[required].notna().all(axis=1)
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter('always')
            result=survey_glm(formula,model_design,'weight','stratum','psu',family=family,domain=complete_domain)
        table=result['coefficients'].copy()
        table['model']=name; table['family']=family
        coefficients.append(table)
        diagnostics.append({'model':name,'n':result['n'],'df':result['df'],
                            'rank':result['rank'],'parameters':len(table),'converged':bool(result['converged']),
                            'predictions_above_one':int((result['predicted']>1).sum()),
                            'formula':formula,'warnings':' | '.join(sorted({str(a.message) for a in caught}))})
        if not result['valid_inference']:
            raise RuntimeError(f'Association model has invalid inference: {name}; rank/convergence/separation must be resolved')
        if 'water' in table.term.values:
            row=table.set_index('term').loc['water']
            water_effects.append({'model':name,'n':result['n'],'measure':'Prevalence ratio' if family=='poisson' else 'Odds ratio',
                                 'estimate':np.exp(row.coef),'ci_low':np.exp(row.ci_low),
                                 'ci_high':np.exp(row.ci_high),'p_value':row.p_value,
                                 'contrast':'One additional reported glass/day'})
        if name=='Water spline':
            info=result['design_info']
            mask=result['analysis_mask']
            analytical=model_design.loc[mask].copy()
            water_contrasts.append(water_spline_contrast(result, analytical))
            weights=analytical.weight.to_numpy();weights=weights/weights.sum()
            beta=table.coef.to_numpy(); covariance=np.asarray(result['covariance'])
            critical=t.ppf(.975,result['df'])
            max_grid=min(12.,float(analytical.water.quantile(.99)))
            for value in np.arange(0,max_grid+.001,.5):
                at=analytical.copy();at['water']=value
                X=np.asarray(build_design_matrices([info],at)[0])
                pr=expit(X@beta)
                mean=float(weights@pr)
                grad=(weights*pr*(1-pr))@X
                se=float(np.sqrt(max(0,grad@covariance@grad)))
                se_logit=se/(mean*(1-mean))
                curve.append({'water':value,'adjusted_prevalence':mean,
                              'ci_low':expit(logit(mean)-critical*se_logit),
                              'ci_high':expit(logit(mean)+critical*se_logit),
                              'n':len(analytical)})
    return {'coefficients':pd.concat(coefficients,ignore_index=True),
            'water_effects':pd.DataFrame(water_effects),
            'water_contrasts':pd.DataFrame(water_contrasts),
            'diagnostics':pd.DataFrame(diagnostics),'water_curve':pd.DataFrame(curve),
            'water_curve_ci_method':CURVE_CI_METHOD,
            'tail_cutoff':float(high_water)}
