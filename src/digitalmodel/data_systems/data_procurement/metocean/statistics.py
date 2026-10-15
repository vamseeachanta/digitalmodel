# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Optional in-memory metocean statistics, without retrieval or output files.

Install ``digitalmodel[metocean]``. All engine imports are lazy; the existing
``MetoceanClient.calculate_extremes`` remains independent of this extra.

API mapping to metocean-stats 1.2.0:
    extremes: stats.return_levels_annual_max_uncertainty (AM) or
        stats.return_levels_pot_uncertainty (POT). Periods are years. AM uses
        1/T annual exceedance probability; POT uses the engine's mean number
        of declustered peaks per calendar year, including partial edge years.
        Intervals are the engine's 1000-resample nonparametric percentile
        bootstrap, conditional on the event rate. They differ from a
        parametric bootstrap and Poisson AM convention used by some callers.
    joint_hs_tp: CMA.JointProbabilityModel with a fitted Weibull Hs marginal
        and conditional lognormal Tp, linear log-mean and constant log-spread.
        This is an explicit statistical model choice, with no standard preset
        or licensed coefficients. Hs is in metres and Tp in seconds.
    environmental_contour: CMA.contours.get_contour, IFORM or ISORM. Return
        period is years and state duration is hours. Coordinates are Hs, Tp.
    weather_windows: stats.weather_window_length_MultipleVariables. Supports
        complete hourly series and up to three strict-below thresholds. Returns
        mean, percentiles and maximum of wait plus operation duration in DAYS.
        The engine censors after the last qualifying start; these are neither
        complete-run lengths nor probabilities of an operable start.

Callers qualify record completeness, station metadata and model suitability.
No measured-record report, plotting, design qualification or release is done.
"""

from threading import Lock

import numpy as np
import pandas as pd

_RANDOM_LOCK = Lock()
_MISSING = "metocean-stats is required; install digitalmodel[metocean]"


def _require_engine():
    """Load the optional package without disguising missing transitive modules."""
    try:
        import metocean_stats
    except ModuleNotFoundError as error:
        if error.name == "metocean_stats":
            raise ImportError(_MISSING) from error
        raise
    return metocean_stats


def _frame(data, columns):
    """Reject ambiguous times and missing values instead of silently dropping."""
    if not isinstance(data, pd.DataFrame) or not isinstance(data.index, pd.DatetimeIndex):
        raise ValueError("a DataFrame with a DatetimeIndex is required")
    if len(data) < 2 or data.index.hasnans or not data.index.is_unique or not data.index.is_monotonic_increasing:
        raise ValueError("at least two unique increasing timestamps are required")
    if not data.columns.is_unique or any(column not in data for column in columns):
        raise ValueError("unique columns containing every requested variable are required")
    result = data.loc[:, columns].astype(float).copy()
    if not np.isfinite(result.to_numpy()).all():
        raise ValueError("finite observations are required")
    return result


def extremes(data, variable, periods, *, method="AM", distribution="GUM",
             confidence=0.95, threshold=None, separation="48h", seed=0):
    """Compute engine return levels and bootstrap intervals, or raise.

    AM supports GUM and GEV; POT supports EXP, GP and Weibull_2P and requires
    an explicit threshold. No automatic threshold or period substitution is
    used. NumPy's legacy random state is restored even on an engine failure;
    concurrent use of that global generator outside this adapter is unsupported.
    """
    _require_engine()
    from metocean_stats import stats as engine

    frame = _frame(data, [variable])
    periods = np.asarray(periods, dtype=float)
    if periods.ndim != 1 or not len(periods) or not np.isfinite(periods).all() or np.any(periods <= 1) or np.any(np.diff(periods) <= 0):
        raise ValueError("finite increasing return periods greater than one are required")
    if not np.isfinite(confidence) or not 0 < confidence < 1:
        raise ValueError("confidence must be between zero and one")
    if method not in ("AM", "POT"):
        raise ValueError("method must be AM or POT")
    choices = {"AM": ("GUM", "GEV"), "POT": ("EXP", "GP", "Weibull_2P")}
    if distribution not in choices[method]:
        raise ValueError("distribution is unsupported for the selected method")
    kwargs = dict(data=frame, var=variable, dist=distribution, periods=periods, uncertainty=confidence)
    if method == "AM":
        sample = frame.groupby(frame.index.year)[variable].max().to_numpy()
        function = engine.return_levels_annual_max_uncertainty
    else:
        if threshold is None or not np.isfinite(threshold):
            raise ValueError("a finite explicit POT threshold is required")
        if pd.Timedelta(separation) <= pd.Timedelta(0):
            raise ValueError("separation must be positive")
        # The same extractor used by the engine validates the independent sample.
        sample = engine.extreme.get_extremes(frame[variable], method="POT", threshold=threshold, r=separation).to_numpy()
        kwargs.update(threshold=threshold, r=separation)
        function = engine.return_levels_pot_uncertainty
    if len(sample) < 3 or np.ptp(sample) <= 0:
        raise ValueError("at least three nonconstant annual maxima or POT peaks are required")
    with _RANDOM_LOCK:
        state = np.random.get_state()
        try:
            np.random.seed(seed)
            result = function(**kwargs)
        finally:
            np.random.set_state(state)
    columns = ["return_levels", "ci_lower_rl", "ci_upper_rl"]
    if not np.isfinite(result[columns].to_numpy()).all():
        raise ValueError("engine produced non-finite return levels or intervals")
    if np.any(result.ci_lower_rl > result.ci_upper_rl) or np.any(np.diff(result.return_levels) < 0):
        raise ValueError("engine produced unordered levels or interval bounds")
    result.attrs.update(engine="metocean-stats", bootstrap_samples=1000,
                        confidence=confidence, seed=seed, period_units="years")
    if method == "POT":
        # Upstream's r attribute is hard-coded; preserve the actual input here.
        result.attrs.update(r=separation, separation=separation)
    return result


def joint_hs_tp(data, hs="hs", tp="tp", *, intervals=4, min_points=10):
    """Fit an explicit conditional model and return the engine's model object."""
    _require_engine()
    from metocean_stats.CMA import JointProbabilityModel
    import virocon

    frame = _frame(data, [hs, tp])
    if hs == tp or (frame.to_numpy() <= 0).any() or (frame.nunique() < 2).any():
        raise ValueError("distinct positive nonconstant Hs and Tp are required")
    if not isinstance(intervals, int) or intervals < 3 or not isinstance(min_points, int) or min_points < 3:
        raise ValueError("at least three intervals and three points per interval are required")

    def linear(x, a, b):
        return a + b * x

    def constant(x, c):
        return np.zeros_like(x) + c

    descriptions = [
        {"distribution": virocon.WeibullDistribution(f_gamma=0),
         "intervals": virocon.NumberOfIntervalsSlicer(intervals, reference="mean", min_n_points=min_points)},
        {"distribution": virocon.LogNormalDistribution(), "conditional_on": 0,
         "parameters": {"mu": virocon.DependenceFunction(linear),
                        "sigma": virocon.DependenceFunction(constant, bounds=[(np.finfo(float).eps, None)])}},
    ]
    semantics = {"names": ["Significant wave height", "Peak wave period"],
                 "symbols": ["Hs", "Tp"], "units": ["m", "s"]}
    model = JointProbabilityModel(model=None, dist_descriptions=descriptions,
                                 fit_descriptions=[{"method": "mle"}, {"method": "mle"}],
                                 semantics=semantics)
    model.fit(frame, var1=hs, var2=tp)
    density = model.pdf(frame.to_numpy())
    if not np.isfinite(density).all() or (density < 0).any():
        raise ValueError("engine produced an invalid joint density")
    return model


def environmental_contour(model, return_period, *, state_duration_hours,
                          method="IFORM", n_points=180):
    """Return Hs-Tp coordinates computed by the engine, without plotting."""
    _require_engine()
    from metocean_stats.CMA.contours import get_contour

    if not np.isfinite(return_period) or return_period <= 1:
        raise ValueError("return period must be finite and greater than one year")
    if not np.isfinite(state_duration_hours) or state_duration_hours <= 0:
        raise ValueError("state duration must be finite and positive")
    if method not in ("IFORM", "ISORM") or not isinstance(n_points, int) or n_points < 4:
        raise ValueError("IFORM or ISORM and at least four points are required")
    if getattr(model, "n_dim", None) != 2:
        raise ValueError("a fitted two-dimensional Hs-Tp model is required")
    contour = get_contour(model, return_period=return_period,
                          state_duration=state_duration_hours, method=method, n_samples=n_points)
    coordinates = np.asarray(contour.coordinates)
    if coordinates.shape != (n_points, 2) or not np.isfinite(coordinates).all():
        raise ValueError("engine produced invalid contour coordinates")
    result = pd.DataFrame(coordinates, columns=["hs", "tp"])
    result.attrs.update(method=method, return_period_years=return_period,
                        state_duration_hours=state_duration_hours)
    return result


def weather_windows(data, thresholds, operation_hours):
    """Return engine wait-plus-operation statistics in days for hourly data.

    Equality to a threshold is inoperable. Gaps, fractional durations and cases
    without a qualifying start raise; no window probability is substituted.
    """
    _require_engine()
    from metocean_stats.stats import weather_window_length_MultipleVariables

    if not 1 <= len(thresholds) <= 3 or not np.isfinite(list(thresholds.values())).all():
        raise ValueError("one to three finite thresholds are required")
    frame = _frame(data, list(thresholds))
    if not (frame.index[1:] - frame.index[:-1] == pd.Timedelta(hours=1)).all():
        raise ValueError("complete regular hourly observations are required")
    if not np.isfinite(operation_hours) or operation_hours < 1 or operation_hours != int(operation_hours) or operation_hours > len(frame):
        raise ValueError("operation duration must be an integer number of recorded hours")
    operable = frame.lt(pd.Series(thresholds)).all(axis=1)
    if not operable.rolling(int(operation_hours)).sum().eq(operation_hours).any():
        raise ValueError("no qualifying weather window")
    values = weather_window_length_MultipleVariables(
        frame, list(thresholds), list(thresholds.values()), operation_hours, 1
    )
    if not np.isfinite(values).all():
        raise ValueError("engine produced non-finite weather-window statistics")
    return dict(zip(["mean_days", "p10_days", "p50_days", "p90_days", "p95_days", "max_days"], values))
