"""Least-squares profile estimators on associated synthetic returns.

Line selections: NOAA Field Procedures Manual (2020), section 1.5.6.2,
pp. 26-27. Equations are derived below from geometry.py's explicit model.
No routine reads truth_xyz_m or injected offsets. Earlier biases must have
been removed in the documented order. Results are signed injected biases,
not signed inverse corrections. No precision or field acceptance is implied.
"""

from math import atan, degrees, fsum

from .geometry import Swath


def _fit(xs, ys):
    """OLS y=intercept+slope*x, from minimising sum((y-a-b*x)**2).

    Least-squares criterion: OpenStax, Introductory Statistics 2e (2023),
    section 12.3. The normal equations give b=sum((x-xbar)(y-ybar))/sum((x-xbar)**2).
    This is an algebraic derivation, with no fitted empirical coefficient.
    """
    if len(xs) < 2:
        raise ValueError("profile requires at least two samples")
    mx, my = fsum(xs) / len(xs), fsum(ys) / len(ys)
    variance = fsum((x - mx) ** 2 for x in xs)
    if variance == 0:
        raise ValueError("profile has no geometric spread")
    slope = fsum((x - mx) * (y - my) for x, y in zip(xs, ys)) / variance
    return my - slope * mx, slope


def _pair(a: Swath, b: Swath, *, reciprocal: bool):
    if a.seabed != b.seabed:
        raise ValueError("line pair must describe the same seabed and target")
    if (a.line.direction != b.line.direction) != reciprocal:
        raise ValueError(
            "reciprocal lines required" if reciprocal else "same direction required"
        )
    if reciprocal and a.line.speed_m_s != b.line.speed_m_s:
        raise ValueError("reciprocal lines require the same speed")
    if a.origins_m[0][2] != b.origins_m[0][2]:
        raise ValueError("line pair requires the same sensor depth")


def _same_track(a: Swath, b: Swath):
    if a.origins_m[0][1] != b.origins_m[0][1]:
        raise ValueError("coincident sensor tracks required")
    lo = max(min(p[0] for p in s.origins_m) for s in (a, b))
    hi = min(max(p[0] for p in s.origins_m) for s in (a, b))
    if lo >= hi:
        raise ValueError("line tracks must overlap along track")


def _nadir_profile(swath: Swath):
    if swath.seabed.slope_x == 0:
        raise ValueError("along-track seabed slope required")
    points = [
        p
        for i, (p, beam) in enumerate(zip(swath.xyz_m, swath.beam_offsets_m))
        if beam == 0 and i != swath.feature_index
    ]
    if not points:
        raise ValueError("nadir beam required")
    # Fit X against sensor-relative observed depth, excluding the point target.
    zs = [p[2] - swath.origins_m[0][2] for p in points]
    return zs, _fit(zs, [p[0] for p in points])


def _profile_shift(a: Swath, b: Swath):
    za, (aa, ba) = _nadir_profile(a)
    zb, (ab, bb) = _nadir_profile(b)
    lo, hi = max(min(za), min(zb)), min(max(za), max(zb))
    if lo >= hi:
        raise ValueError("nadir profiles must overlap in observed depth")
    depth = (lo + hi) / 2
    if depth <= 0:
        raise ValueError("profile depth below sensor must be positive")
    return (aa - ab) + (ba - bb) * depth, depth


def estimate_roll(a: Swath, b: Swath) -> float:
    """Roll [deg] from reciprocal flat-bottom cross-track depth mismatch.

    After pitch correction, z=b*cos(yaw)*sin(r)+D*cos(r),
    y_local=b*cos(yaw)*cos(r)-D*sin(r). Eliminating b gives
    dz/dY=direction*tan(r). Thus m_a-m_b=(dir_a-dir_b)*tan(r).
    OLS uses all returns; a flat plane and cross-track spread are required.
    """
    _pair(a, b, reciprocal=True)
    _same_track(a, b)
    if a.seabed.slope_x != 0 or a.seabed.slope_y != 0 or a.seabed.feature is not None:
        raise ValueError("flat feature-free seabed required for roll")
    slopes = [_fit([p[1] for p in s.xyz_m], [p[2] for p in s.xyz_m])[1] for s in (a, b)]
    return degrees(
        atan((slopes[0] - slopes[1]) / (a.line.direction - b.line.direction))
    )


def estimate_pitch(a: Swath, b: Swath) -> float:
    """Pitch [deg] from reciprocal nadir profiles after latency correction.

    For plane z_true=d0+s*X_true, nadir x_local=D_true*cos(r)*sin(p),
    D_obs=D_true*cos(r)*cos(p). Therefore X_obs = X_true + dir*D_obs*tan(p).
    At equal D_obs, X_true is equal in reciprocal runs, so
    dx=(dir_a-dir_b)*D_obs*tan(p), or dx=2*D_obs*tan(p) for + then -.
    This is exact in the synthetic model; using true depth instead of observed
    depth gives the usual small-angle approximation. The factor 2 is the
    difference of reciprocal direction signs, not a standard coefficient.
    """
    _pair(a, b, reciprocal=True)
    _same_track(a, b)
    shift, depth = _profile_shift(a, b)
    return degrees(atan(shift / ((a.line.direction - b.line.direction) * depth)))


def estimate_latency(a: Swath, b: Swath) -> float:
    """Latency [s] from coincident same-heading nadir profiles at two speeds.

    X_obs=X_geometric+dir*v*tau. Constant angular offsets make the same
    geometric profile at both speeds. At equal observed depth,
    dx=dir*(v_a-v_b)*tau; therefore tau=dx/(dir*(v_a-v_b)).
    This determines the injected position timestamp bias under this convention.
    NOAA FPM (2020), 1.5.6.2 treats timing as a gross diagnostic, not an annual
    calibration requirement; no universal field procedure is claimed here.
    """
    _pair(a, b, reciprocal=False)
    _same_track(a, b)
    speed_difference = a.line.direction * (a.line.speed_m_s - b.line.speed_m_s)
    if speed_difference == 0:
        raise ValueError("different speeds required for latency")
    shift, _ = _profile_shift(a, b)
    return shift / speed_difference


def estimate_yaw(a: Swath, b: Swath) -> float:
    """Yaw [deg] from the same associated point target on parallel outer beams.

    After latency/pitch/roll correction, x_local=-b*sin(yaw),
    y_local=b*cos(yaw). Two returns of the same target have equal true X,
    so dx_local=-delta_b*sin(yaw) and dy_local=delta_b*cos(yaw).
    yaw=atan(-dx_local/dy_local). The across-track distance is the difference
    of OBSERVED sensor-relative offsets, not ground track spacing; using track
    spacing instead would require asin. Association is supplied by the synthetic
    point target, not inferred by matching arbitrary depth peaks.
    """
    _pair(a, b, reciprocal=False)
    if a.line.speed_m_s != b.line.speed_m_s:
        raise ValueError("same speed required for yaw")
    if a.origins_m[0][1] == b.origins_m[0][1]:
        raise ValueError("adjacent parallel tracks required for yaw")
    if a.feature_index is None or b.feature_index is None:
        raise ValueError("same associated feature must be sampled on both lines")
    pa, pb = a.xyz_m[a.feature_index], b.xyz_m[b.feature_index]
    oa, ob = a.origins_m[a.feature_index], b.origins_m[b.feature_index]
    # Target must lie in the overlap, on opposite sides of the two tracks.
    ya, yb = pa[1] - oa[1], pb[1] - ob[1]
    if ya * yb >= 0:
        raise ValueError("feature must lie between the parallel sensor tracks")
    return degrees(atan(-(pa[0] - pb[0]) / (ya - yb)))
