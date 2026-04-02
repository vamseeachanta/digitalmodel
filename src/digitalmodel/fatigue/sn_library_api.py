     1|"""
     2|S-N Curve Library API — Programmatic Access to 221 Curves
     3|==========================================================
     4|
     5|High-level API wrapping :mod:`sn_library` to provide a clean, typed interface
     6|for listing, querying, and computing with the full S-N curve catalogue.
     7|
     8|This module exposes:
     9|
    10|- :func:`list_curves` — list/filter the 221-curve catalogue
    11|- :func:`get_curve` — look up one curve by ``curve_id``
    12|- :func:`calculate_endurance` — allowable cycles for a stress range
    13|- :func:`compare_curves` — multi-curve comparison data (plot-ready)
    14|
    15|All models are Pydantic v2 for type safety and serialisation.
    16|
    17|Issue: #1676 (P0 — fatigue expansion)
    18|
    19|References
    20|----------
    21|- DNV-RP-C203 (2021), Fatigue Design of Offshore Steel Structures
    22|- BS 7608:2014, Fatigue design and assessment of steel structures
    23|- API RP 2A-WSD (2014), Section 5
    24|"""
    25|
    26|from typing import List, Optional, Union
    27|
    28|import numpy as np
    29|from pydantic import BaseModel, Field
    30|
    31|from .sn_library import (
    32|    SNCurveRecord,
    33|    get_catalog,
    34|    get_library_curve,
    35|    search_curves as _search_curves,
    36|)
    37|
    38|
    39|# ---------------------------------------------------------------------------
    40|# Pydantic API models
    41|# ---------------------------------------------------------------------------
    42|
    43|class CurveInfo(BaseModel):
    44|    """Lightweight summary of an S-N curve for listing/filtering.
    45|
    46|    Attributes
    47|    ----------
    48|    curve_id : str
    49|        Unique identifier (e.g. ``"DNV-RP-C203:D:air"``).
    50|    standard : str
    51|        Source standard name.
    52|    weld_class : str
    53|        Detail category or FAT class label.
    54|    environment : str
    55|        ``"air"``, ``"seawater_cp"``, or ``"free_corrosion"``.
    56|    m1 : float
    57|        Primary (high-stress) slope.
    58|    endurance_limit : float | None
    59|        CAFL in MPa, or None if single-slope.
    60|    """
    61|
    62|    curve_id: str
    63|    standard: str
    64|    weld_class: str
    65|    environment: str
    66|    m1: float
    67|    endurance_limit: Optional[float] = None
    68|
    69|
    70|class SNCurve(BaseModel):
    71|    """Full S-N curve parameters for engineering calculations.
    72|
    73|    Attributes
    74|    ----------
    75|    curve_id : str
    76|        Unique identifier.
    77|    standard : str
    78|        Source standard name.
    79|    weld_class : str
    80|        Detail category / FAT class.
    81|    environment : str
    82|        Exposure environment.
    83|    m1 : float
    84|        Negative inverse slope, first segment.
    85|    log_a1 : float
    86|        log10(intercept) for first segment.
    87|    m2 : float | None
    88|        Slope of second segment (beyond knee). None for single-slope.
    89|    log_a2 : float | None
    90|        Intercept of second segment.
    91|    knee_point : float
    92|        Cycle count at slope change (N_D).
    93|    endurance_limit : float | None
    94|        CAFL in MPa at the knee.
    95|    thickness_ref : float
    96|        Reference thickness for correction (mm).
    97|    thickness_exponent : float
    98|        Thickness correction exponent k.
    99|    note : str
   100|        Free-text description.
   101|    """
   102|
   103|    curve_id: str
   104|    standard: str
   105|    weld_class: str = ""
   106|    environment: str = "air"
   107|    m1: float
   108|    log_a1: float
   109|    m2: Optional[float] = None
   110|    log_a2: Optional[float] = None
   111|    knee_point: float = 1e7
   112|    endurance_limit: Optional[float] = None
   113|    thickness_ref: float = 25.0
   114|    thickness_exponent: float = 0.25
   115|    note: str = ""
   116|
   117|
   118|class ComparisonData(BaseModel):
   119|    """Multi-curve comparison data, ready for plotting.
   120|
   121|    Attributes
   122|    ----------
   123|    curve_names : list[str]
   124|        Curve IDs in order.
   125|    stress_ranges : object
   126|        The stress range array (stored as list for serialisation).
   127|    endurance_cycles : list
   128|        List of endurance-cycle arrays, one per curve.
   129|    """
   130|
   131|    curve_names: List[str]
   132|    stress_ranges: List[float] = Field(default_factory=list)
   133|    endurance_cycles: List  # list of np.ndarray — serialised as lists
   134|
   135|    class Config:
   136|        arbitrary_types_allowed = True
   137|
   138|
   139|# ---------------------------------------------------------------------------
   140|# Internal helpers
   141|# ---------------------------------------------------------------------------
   142|
   143|def _record_to_curve_info(rec: SNCurveRecord) -> CurveInfo:
   144|    """Convert an SNCurveRecord to a CurveInfo summary."""
   145|    return CurveInfo(
   146|        curve_id=rec.curve_id,
   147|        standard=rec.standard,
   148|        weld_class=rec.curve_class,
   149|        environment=rec.environment,
   150|        m1=rec.m1,
   151|        endurance_limit=rec.endurance_limit,
   152|    )
   153|
   154|
   155|def _record_to_sncurve(rec: SNCurveRecord) -> SNCurve:
   156|    """Convert an SNCurveRecord to a full SNCurve model."""
   157|    return SNCurve(
   158|        curve_id=rec.curve_id,
   159|        standard=rec.standard,
   160|        weld_class=rec.curve_class,
   161|        environment=rec.environment,
   162|        m1=rec.m1,
   163|        log_a1=rec.log_a1,
   164|        m2=rec.m2,
   165|        log_a2=rec.log_a2,
   166|        knee_point=rec.n_transition,
   167|        endurance_limit=rec.endurance_limit,
   168|        thickness_ref=rec.thickness_ref,
   169|        thickness_exponent=rec.thickness_exponent,
   170|        note=rec.note,
   171|    )
   172|
   173|
   174|# ---------------------------------------------------------------------------
   175|# Public API
   176|# ---------------------------------------------------------------------------
   177|
   178|def list_curves(
   179|    standard: Optional[str] = None,
   180|    weld_class: Optional[str] = None,
   181|) -> List[CurveInfo]:
   182|    """List available S-N curves, optionally filtered.
   183|
   184|    Parameters
   185|    ----------
   186|    standard : str, optional
   187|        Filter by standard name (e.g. ``"DNV-RP-C203"``).
   188|    weld_class : str, optional
   189|        Filter by weld/detail class (e.g. ``"D"``, ``"F2"``).
   190|
   191|    Returns
   192|    -------
   193|    list[CurveInfo]
   194|        Lightweight summaries of matching curves.
   195|
   196|    Examples
   197|    --------
   198|    >>> len(list_curves())  # all 221 curves
   199|    221
   200|    >>> len(list_curves(standard="BS 7608"))  # 20 BS 7608 curves
   201|    20
   202|    """
   203|    records = _search_curves(standard=standard, curve_class=weld_class)
   204|    return [_record_to_curve_info(r) for r in records]
   205|
   206|
   207|def get_curve(curve_id: str) -> SNCurve:
   208|    """Look up a single S-N curve by its unique ID.
   209|
   210|    Parameters
   211|    ----------
   212|    curve_id : str
   213|        Unique identifier, e.g. ``"DNV-RP-C203:D:air"``.
   214|
   215|    Returns
   216|    -------
   217|    SNCurve
   218|        Full curve parameters.
   219|
   220|    Raises
   221|    ------
   222|    KeyError
   223|        If the curve_id is not in the library.
   224|
   225|    Examples
   226|    --------
   227|    >>> c = get_curve("DNV-RP-C203:D:air")
   228|    >>> c.m1
   229|    3.0
   230|    """
   231|    rec = get_library_curve(curve_id)
   232|    return _record_to_sncurve(rec)
   233|
   234|
   235|def calculate_endurance(
   236|    curve: SNCurve,
   237|    stress_range: Union[float, np.ndarray],
   238|) -> Union[float, np.ndarray]:
   239|    """Calculate allowable cycles N for a given stress range.
   240|
   241|    Uses bi-linear log-log model::
   242|
   243|        if S > S_D:  N = 10^(log_a1) · S^(-m1)
   244|        else:        N = 10^(log_a2) · S^(-m2)   (if m2 defined)
   245|
   246|    For single-slope curves, one slope is used throughout.
   247|
   248|    Parameters
   249|    ----------
   250|    curve : SNCurve
   251|        S-N curve parameters.
   252|    stress_range : float or np.ndarray
   253|        Applied stress range(s) in MPa.
   254|
   255|    Returns
   256|    -------
   257|    float or np.ndarray
   258|        Allowable number of cycles. ``inf`` for zero/negative stress.
   259|
   260|    Examples
   261|    --------
   262|    >>> c = get_curve("DNV-RP-C203:D:air")
   263|    >>> calculate_endurance(c, 100.0)  # ≈ 1.46e6
   264|    """
   265|    S = np.asarray(stress_range, dtype=float)
   266|    scalar = S.ndim == 0
   267|    S = np.atleast_1d(S)
   268|
   269|    N = np.full_like(S, np.inf, dtype=float)
   270|    mask_pos = S > 0
   271|
   272|    if curve.endurance_limit is not None and curve.m2 is not None and curve.log_a2 is not None:
   273|        high = mask_pos & (S >= curve.endurance_limit)
   274|        low = mask_pos & (S < curve.endurance_limit)
   275|        N[high] = 10 ** curve.log_a1 * S[high] ** (-curve.m1)
   276|        N[low] = 10 ** curve.log_a2 * S[low] ** (-curve.m2)
   277|    else:
   278|        N[mask_pos] = 10 ** curve.log_a1 * S[mask_pos] ** (-curve.m1)
   279|
   280|    return float(N[0]) if scalar else N
   281|
   282|
   283|def compare_curves(
   284|    names: List[str],
   285|    stress_ranges: np.ndarray,
   286|) -> ComparisonData:
   287|    """Generate comparison data for multiple S-N curves.
   288|
   289|    Parameters
   290|    ----------
   291|    names : list[str]
   292|        Curve IDs to compare.
   293|    stress_ranges : np.ndarray
   294|        Array of stress range values (MPa) for the comparison.
   295|
   296|    Returns
   297|    -------
   298|    ComparisonData
   299|        Contains curve names, shared stress ranges, and per-curve
   300|        endurance cycle arrays.
   301|
   302|    Raises
   303|    ------
   304|    KeyError
   305|        If any curve name is not found.
   306|
   307|    Examples
   308|    --------
   309|    >>> data = compare_curves(
   310|    ...     ["DNV-RP-C203:D:air", "DNV-RP-C203:E:air"],
   311|    ...     np.linspace(10, 300, 50),
   312|    ... )
   313|    >>> len(data.endurance_cycles)  # one array per curve
   314|    2
   315|    """
   316|    stress_ranges = np.asarray(stress_ranges, dtype=float)
   317|    curves = [get_curve(name) for name in names]
   318|    endurance_arrays = [calculate_endurance(c, stress_ranges) for c in curves]
   319|
   320|    return ComparisonData(
   321|        curve_names=list(names),
   322|        stress_ranges=stress_ranges.tolist(),
   323|        endurance_cycles=endurance_arrays,
   324|    )
   325|