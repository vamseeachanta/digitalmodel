# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run a bounded reliability comparison or rebuild its retained report."""
import argparse
import hashlib
import html
import json
import math
import time
from datetime import datetime, timezone
from pathlib import Path

from scipy.stats import norm

from digitalmodel.structural.fatigue.fatigue_reliability import (
    failure_probability, monte_carlo_fatigue, sensitivity_analysis,
)
from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve

DATA = Path(__file__).resolve().parent


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def now():
    return datetime.now(timezone.utc).isoformat()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + '\n')


def form(ot, function, distribution, threshold):
    event = ot.ThresholdEvent(
        ot.CompositeRandomVector(function, ot.RandomVector(distribution)),
        ot.Less(), threshold,
    )
    solver = ot.Cobyla()
    solver.setStartingPoint(distribution.getMean())
    solver.setMaximumCallsNumber(400)
    for name in ('Absolute', 'Relative', 'Residual', 'Constraint'):
        getattr(solver, 'setMaximum' + name + 'Error')(1e-10)
    algorithm = ot.FORM(solver, event)
    algorithm.run()
    result = algorithm.getResult()
    return result.getEventProbability(), result.getGeneralisedReliabilityIndex()


def compute(criteria, record):
    import openturns as ot

    if ot.__version__ != criteria['openturns_version']:
        raise ValueError('OpenTURNS version differs from the declared pin')
    ot.TBB.SetThreadsNumber(criteria['processes'])
    ot.RandomGenerator.SetSeed(criteria['seed'])
    start = time.perf_counter()
    beam = ot.SymbolicFunction(['E', 'F', 'L', 'I'], ['-F*L^3/(3*E*I)'])
    pf, beta = form(ot, beam, ot.Normal([50.0, 1.0, 10.0, 5.0], [1.0]*4,
                                     ot.IdentityMatrix(4)), -3.0)
    record['published'] = {'pf': pf, 'beta': beta,
                           'wall_seconds': time.perf_counter() - start}
    basis = criteria['fatigue']
    curve = PowerLawSNCurve(name='Benchmark', A=basis['A'], m=basis['m'], fatigue_limit=0.0)
    variance_s = math.log1p(basis['cov_stress']**2)
    variance_a = math.log1p(basis['cov_sn']**2)
    means = [math.log(basis['stress_range']) - variance_s/2,
             math.log(basis['A']) - variance_a/2]
    distribution = ot.Normal(means, [math.sqrt(variance_s), math.sqrt(variance_a)],
                             ot.IdentityMatrix(2))
    function = ot.SymbolicFunction(['s', 'a'], [f'a-{basis["m"]}*s'])
    record['fatigue'] = []
    for life in basis['design_lives']:
        reference_beta = (means[1] - basis['m']*means[0] - math.log(life)) / math.sqrt(
            variance_a + basis['m']**2*variance_s)
        row = {'design_life': life, 'reference_beta': reference_beta,
               'reference_pf': float(norm.cdf(-reference_beta))}
        start = time.perf_counter()
        native = failure_probability(curve, basis['stress_range'], life,
                                     basis['cov_stress'], basis['cov_sn'])
        row.update(native_pf=native['pf'], native_beta=native['beta'],
                   native_form_seconds=time.perf_counter()-start)
        start = time.perf_counter()
        pf, beta = form(ot, function, distribution, math.log(life))
        row.update(openturns_pf=pf, openturns_beta=beta,
                   openturns_form_seconds=time.perf_counter()-start)
        start = time.perf_counter()
        mc = monte_carlo_fatigue(curve, basis['stress_range'], life,
                                basis['cov_stress'], basis['cov_sn'],
                                n_samples=criteria['samples'], seed=criteria['seed'])
        row.update(native_mc_pf=mc['pf'], native_mc_seconds=time.perf_counter()-start)
        start = time.perf_counter()
        ot.RandomGenerator.SetSeed(criteria['seed'])
        samples = function(distribution.getSample(criteria['samples']))
        count = sum(value[0] < math.log(life) for value in samples)
        row.update(openturns_mc_pf=count/criteria['samples'],
                   openturns_mc_seconds=time.perf_counter()-start)
        record['fatigue'].append(row)
    start = time.perf_counter()
    native = sensitivity_analysis(curve, basis['stress_range'], basis['design_lives'][0],
                                  basis['cov_stress'], basis['cov_sn'],
                                  n_samples=criteria['samples'], seed=criteria['seed'])
    native_seconds = time.perf_counter()-start
    start = time.perf_counter()
    ot.RandomGenerator.SetSeed(criteria['seed'])
    inputs = ot.SobolIndicesExperiment(distribution, criteria['samples'], False).generate()
    sobol = ot.JansenSensitivityAlgorithm(inputs, function(inputs), criteria['samples'])
    indices = list(sobol.getFirstOrderIndices())
    total = variance_a + basis['m']**2*variance_s
    record['sobol'] = {
        'reference': {'stress': basis['m']**2*variance_s/total, 'sn': variance_a/total},
        'native': native['first_order'],
        'openturns': dict(zip(('stress', 'sn'), indices)),
        'native_seconds': native_seconds,
        'openturns_seconds': time.perf_counter()-start,
        'evaluations_per_method': native['n_evaluations'],
    }
    record['openturns_version'] = ot.__version__
    return record


def validate(result, criteria):
    errors = []
    tolerance = criteria['tolerances']
    basis = criteria['fatigue']
    variance_s = math.log1p(basis['cov_stress']**2)
    variance_a = math.log1p(basis['cov_sn']**2)
    variance = variance_a + basis['m']**2*variance_s
    for group in (result['published'], *result['fatigue']):
        if any(not math.isfinite(value) for value in group.values()):
            errors.append('nonfinite result')
    for method in ('reference', 'native', 'openturns'):
        if any(not math.isfinite(value) for value in result['sobol'][method].values()):
            errors.append('nonfinite sensitivity')
    if result['openturns_version'] != criteria['openturns_version']:
        errors.append('OpenTURNS version')
    for key in ('pf', 'beta'):
        if abs(result['published'][key] - criteria['published'][key]) > tolerance[
                'published_' + key + '_absolute']:
            errors.append('published ' + key)
    if [row['design_life'] for row in result['fatigue']] != criteria['fatigue']['design_lives']:
        errors.append('fatigue case coverage')
    for row in result['fatigue']:
        beta = (math.log(basis['A']) - variance_a/2
                - basis['m']*(math.log(basis['stress_range'])-variance_s/2)
                - math.log(row['design_life'])) / math.sqrt(variance)
        for key, expected in (('beta', beta), ('pf', float(norm.cdf(-beta)))):
            if abs(row['reference_'+key] - expected) > tolerance['fatigue_'+key+'_absolute']:
                errors.append('altered exact reference ' + key)
        for method in ('native', 'openturns'):
            for key in ('pf', 'beta'):
                if abs(row[method+'_'+key] - row['reference_'+key]) > tolerance[
                        'fatigue_'+key+'_absolute']:
                    errors.append(f'{method} {key} at {row["design_life"]}')
            if abs(row[method+'_mc_pf'] - row['reference_pf']) > tolerance['sampling_pf_absolute']:
                errors.append(f'{method} sampling at {row["design_life"]}')
            for variable in ('stress', 'sn'):
                expected = (basis['m']**2*variance_s if variable == 'stress' else variance_a)/variance
                if abs(result['sobol']['reference'][variable] - expected) > tolerance['sobol_absolute']:
                    errors.append('altered sensitivity reference ' + variable)
                if abs(result['sobol'][method][variable] - result['sobol']['reference'][variable]) > tolerance['sobol_absolute']:
                    errors.append(f'{method} Sobol {variable}')
    return errors


def number(value):
    return f'{value:.4g}'


def difference(value, reference):
    return f'{100*(value-reference)/reference:.1f}%'


def render(result, criteria):
    esc = html.escape
    rows = ''.join(
        '<tr>' + ''.join(f'<td>{number(row[key])}</td>' for key in (
            'design_life', 'reference_pf', 'native_pf', 'openturns_pf',
            'native_mc_pf', 'openturns_mc_pf')) + '</tr>'
        for row in result['fatigue'])
    comparisons = ''.join(
        f'<li>At {number(row["design_life"])} cycles, native analytic and OpenTURNS FORM '
        f'probability differences from the exact reference are '
        f'{difference(row["native_pf"], row["reference_pf"])} and '
        f'{difference(row["openturns_pf"], row["reference_pf"])}; sampling differences are '
        f'{difference(row["native_mc_pf"], row["reference_pf"])} and '
        f'{difference(row["openturns_mc_pf"], row["reference_pf"])} respectively.</li>'
        for row in result['fatigue'])
    costs = ''.join(
        f'<tr><td>{number(row["design_life"])}</td>' + ''.join(
            f'<td>{number(row[key])}</td>' for key in (
                'native_form_seconds', 'openturns_form_seconds',
                'native_mc_seconds', 'openturns_mc_seconds')) + '</tr>'
        for row in result['fatigue'])
    sobol = result['sobol']
    sensitivities = ''.join(
        f'<tr><td>{key}</td><td>{number(sobol["reference"][key])}</td>'
        f'<td>{number(sobol["native"][key])}</td><td>{number(sobol["openturns"][key])}</td>'
        f'<td>{difference(sobol["native"][key], sobol["reference"][key])}</td>'
        f'<td>{difference(sobol["openturns"][key], sobol["reference"][key])}</td></tr>'
        for key in ('stress', 'sn'))
    revisions = ''.join('<tr>'+''.join(f'<td>{esc(item[key])}</td>' for key in (
        'revision', 'date', 'change'))+'</tr>' for item in criteria['revision_history'])
    beam = result['published']
    t = criteria['tolerances']
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Reliability program benchmarking analysis</title>
<style>body{{font:16px/1.5 system-ui;color:#182f40;margin:auto;max-width:1100px;padding:24px}}table{{border-collapse:collapse;width:100%;margin:20px 0}}th,td{{border:1px solid #aab;padding:8px;text-align:left}}caption{{caption-side:bottom;text-align:left}}h2{{margin-top:32px}}@media print{{body{{font-size:10pt}}tr{{break-inside:avoid}}}}</style>
</head><body>
<h1>Reliability program benchmarking analysis</h1><p>Internal method comparison</p>
<h2>Summary</h2>
<p>Retain the existing analytic fatigue calculation for independent lognormal stress and S-N intercept uncertainty with fixed slope. Use OpenTURNS as an optional comparison and as a candidate for separately validated nonlinear models. Neither method establishes an accept or re-rate decision without an inspection basis and a governing reliability target; those inputs are absent from this benchmark.</p>
<h2>Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>{revisions}<caption>Revision history.</caption></table>
<h2>1. Introduction</h2><p>The analysis compares probability, reliability index, sensitivity and measured computation cost. The reference set contains a published nonlinear FORM calculation and exact linear-in-log-space fatigue cases.</p>
<h2>2. Design basis</h2>
<h3>2.1 Asset and geometry</h3><p>No inspected asset is represented. The nonlinear numerical reference is a beam deflection model.</p>
<h3>2.2 Materials</h3><p>The fatigue curve is a synthetic repository fixture, not a qualified material curve.</p>
<h3>2.3 Loads</h3><p>Mean stress range is {number(criteria['fatigue']['stress_range'])} MPa; the design lives in Table 1 are cycles, not calendar years.</p>
<h3>2.4 Environment</h3><p>Environmental loading is not included because the reference model is algebraic.</p>
<h3>2.5 Criteria</h3><p>Absolute probability and index tolerances for analytic comparisons are {number(t['fatigue_pf_absolute'])} and {number(t['fatigue_beta_absolute'])}. Sampling probability tolerance is {number(t['sampling_pf_absolute'])}; first-order sensitivity tolerance is {number(t['sobol_absolute'])}. Published FORM tolerances are {number(t['published_pf_absolute'])} for probability and {number(t['published_beta_absolute'])} for index. These numerical agreement criteria are not asset acceptance criteria.</p>
<h3>2.6 Method</h3><p>See Analysis methodology.</p>
<h3>2.7 Assumptions</h3><p>Stress and intercept are independent lognormal variables, each with coefficient of variation {number(criteria['fatigue']['cov_stress'])}. The power-law slope is fixed at {number(criteria['fatigue']['m'])}; intercept is {number(criteria['fatigue']['A'])} in cycles multiplied by MPa raised to that slope. No uncertainty in slope, correlation or rainflow history is included.</p>
<h2>3. Analysis methodology</h2><p>The existing NumPy/SciPy routine evaluates the exact normal margin in log-space. OpenTURNS {esc(result['openturns_version'])} applies FORM to the same margin. Exact reference probabilities are computed independently from the stated normal mean and variance. Fixed-seed direct sampling and Jansen first-order sensitivity are also compared. The native routine is not applied to the nonlinear beam because its interface only represents power-law fatigue.</p>
<h2>4. Results</h2>
<table><tr><th>Life (cycles)</th><th>Exact probability (1)</th><th>Native analytic (1)</th><th>OpenTURNS FORM (1)</th><th>Native sampling (1)</th><th>OpenTURNS sampling (1)</th></tr>{rows}<caption>Table 1. Failure probabilities for the repository fatigue model.</caption></table>
<ul>{comparisons}</ul>
<p>The nonlinear FORM probability is {number(beam['pf'])}, compared with the published {number(criteria['published']['pf'])}, a difference of {difference(beam['pf'], criteria['published']['pf'])}. The reliability index is {number(beam['beta'])}, compared with the published {number(criteria['published']['beta'])}, a difference of {difference(beam['beta'], criteria['published']['beta'])}. This comparison reproduces a FORM approximation; it does not establish the exact nonlinear failure probability.</p>
<table><tr><th>Input</th><th>Exact index (1)</th><th>Native (1)</th><th>OpenTURNS (1)</th><th>Native difference (%)</th><th>OpenTURNS difference (%)</th></tr>{sensitivities}<caption>Table 2. First-order sensitivity and percentage differences from the exact variance partition.</caption></table>
<h2>5. Computation cost</h2><p>Each sampling run uses {criteria['samples']} samples with seed {criteria['seed']}, in {criteria['processes']} process. Each sensitivity method evaluates {sobol['evaluations_per_method']} model points. Costs below are single observed wall times, not performance guarantees. An eight-core allocation does not imply an eightfold reduction because this implementation runs serially.</p>
<table><tr><th>Life (cycles)</th><th>Native analytic (s)</th><th>OpenTURNS FORM (s)</th><th>Native sampling (s)</th><th>OpenTURNS sampling (s)</th></tr>{costs}<caption>Table 3. Measured costs at the time of this run; installation and report rendering are excluded.</caption></table>
<p>Native sensitivity took {number(sobol['native_seconds'])} s and OpenTURNS sensitivity took {number(sobol['openturns_seconds'])} s. The nonlinear FORM run took {number(beam['wall_seconds'])} s. Total benchmark execution took {number(result['total_wall_seconds'])} s; the projected cost for the same serial implementation within an eight-core allocation is this measured time, without parallel speedup.</p>
<p>The sampling timings include different work: the native method computes fatigue-life percentiles, while the OpenTURNS method evaluates the margin and counts events through a Python loop. These observations compare the implemented workflows and do not isolate library throughput.</p>
<h2>6. Recommendation and decision routing</h2><p>The exact analytic fatigue calculation avoids sampling error under the tested assumptions and remains the recommended default for that model. Direct sampling provides a distribution check but the tested sample count does not qualify rare-event tail estimates. OpenTURNS provides a nonlinear FORM alternative, demonstrated against its published reference; an asset-specific model needs separate validation.</p>
<p>Accept or re-rate requires a governing target, an inspection-derived model and qualified load and material data. Measure more where those inputs or their uncertainty are missing. Escalate when variable slope, correlated inputs, nonlinear damage or rare-event probability changes the model. The present benchmark establishes numerical agreement only and supplies no asset disposition.</p>
<h2>7. Limitations</h2><p>Synthetic fatigue inputs, fixed slope and independence limit the model; see Design basis. The nonlinear reference is a FORM result, not an exact probability; see Results. One seed and one timing observation do not establish sampling coverage or transferable speed; see Computation cost. No inspection or target reliability is provided, so acceptance and re-rating remain unestablished; see Recommendation.</p>
<h2>8. Conclusions</h2><p>All retained results meet their declared numerical agreement criteria. The comparison supports retaining the analytic fatigue model and using OpenTURNS optionally where separately qualified model requirements justify it.</p>
<h2>9. References</h2><ul>
<li><a href="{esc(criteria['published']['source'])}">OpenTURNS v1.26 published expected output</a>: event probability and generalized reliability index lines.</li>
<li><a href="{esc(criteria['published']['model_source'])}">OpenTURNS v1.26 reference model</a>: independent normal beam inputs and nonlinear deflection function.</li>
<li>Repository fatigue basis: {esc(criteria['fatigue']['basis'])}.</li>
<li>OpenTURNS {esc(result['openturns_version'])}; native fatigue implementation: src/digitalmodel/structural/fatigue/fatigue_reliability.py. Exact normal probabilities use SciPy.</li>
</ul>
<h2>10. Appendices</h2><p>The adjacent criteria, results and manifest retain unrounded values, execution times and content digests. The report uses four significant figures for quantities and one decimal place for percentage comparisons; validation uses unrounded values.</p>
</body></html>
'''


def build_report():
    criteria = json.loads((DATA / 'criteria.json').read_text())
    result = json.loads((DATA / 'results.json').read_text())
    if errors := validate(result, criteria):
        raise ValueError('Benchmark disagreement: ' + ', '.join(errors))
    if result['criteria_sha256'] != digest(DATA / 'criteria.json'):
        raise ValueError('Criteria digest mismatch')
    (DATA / 'report.html').write_text(render(result, criteria))
    write_json(DATA / 'manifest.json', {
        'criteria_sha256': result['criteria_sha256'],
        'criteria_recorded_at': result['criteria_recorded_at'],
        'started_at': result['started_at'],
        'sha256': {name: digest(DATA / name) for name in (
            'build.py', 'criteria.json', 'results.json', 'report.html',
            'sources.json', 'run-record.jsonl')},
        'retained': 'Compact metrics and model basis; no sample arrays retained.',
        'intended_use': 'Numerical method comparison; no inspected asset disposition.',
    })


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run', action='store_true')
    parser.add_argument('--log', type=Path)
    args = parser.parse_args()
    if args.run:
        if (DATA / 'results.json').exists():
            raise ValueError('Retained results already exist; rebuild without --run')
        if args.log is None:
            parser.error('--run requires an execution log')
        criteria = json.loads((DATA / 'criteria.json').read_text())
        record = {'criteria_sha256': digest(DATA / 'criteria.json'),
                  'criteria_recorded_at': now()}
        with args.log.open('a') as log:
            log.write(json.dumps(record) + '\n')
            log.flush()
        record['started_at'] = now()
        start = time.perf_counter()
        result = compute(criteria, record)
        result['total_wall_seconds'] = time.perf_counter()-start
        result['completed_at'] = now()
        write_json(DATA / 'results.json', result)
        with args.log.open('a') as log:
            log.write(json.dumps({'started_at': result['started_at'],
                                  'completed_at': result['completed_at']}) + '\n')
    build_report()


if __name__ == '__main__':
    main()
