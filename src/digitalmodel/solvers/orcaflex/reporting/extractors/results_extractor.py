"""
Extractor for analysis results from OrcaFlex models.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

import numpy as np
from ..models.results import StaticResultsData, DynamicResultsData, EnvelopeData, TimeSeriesData


def extract_static_results(line: 'ofx.OrcaFlexLineObject') -> StaticResultsData:
    """Extracts equilibrium results."""
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    arc_lengths = list(line.NodeArclengths)
    
    # End tensions
    end_tensions = {
        "End A": line.StaticResult("Effective Tension", ofx.oeArcLength(arc_lengths[0])),
        "End B": line.StaticResult("Effective Tension", ofx.oeArcLength(arc_lengths[-1]))
    }
    
    # Static profiles
    te_s = [line.StaticResult("Effective Tension", ofx.oeArcLength(s)) for s in arc_lengths]
    bm_s = [line.StaticResult("Bend Moment", ofx.oeArcLength(s)) for s in arc_lengths]

    return StaticResultsData(
        end_tensions_kn=end_tensions,
        tension_profile={"arc_length": arc_lengths, "tension": te_s},
        bm_profile={"arc_length": arc_lengths, "bm": bm_s}
    )


def extract_dynamic_results(line: 'ofx.OrcaFlexLineObject', period: int = None) -> DynamicResultsData:
    """Extracts dynamic envelopes and key time histories using vectorized calls."""
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")
    
    if period is None:
        period = ofx.pnDynamic

    model = line.model
    arc_lengths = list(line.NodeArclengths)
    
    # Vectorized RangeGraph extraction
    rg_te = line.RangeGraph("Effective Tension", period)
    te_max = list(rg_te.Max)
    te_min = list(rg_te.Min)
    
    rg_bm = line.RangeGraph("Bend Moment", period)
    bm_max = list(rg_bm.Max)
    bm_min = list(rg_bm.Min)
    
    # Envelopes
    envelopes = [
        EnvelopeData(id="te_envelope", label="Effective Tension", 
                     arc_length=arc_lengths, max_values=te_max, min_values=te_min, units="kN"),
        EnvelopeData(id="bm_envelope", label="Bending Moment", 
                     arc_length=arc_lengths, max_values=bm_max, min_values=bm_min, units="kN·m")
    ]
    
    # Find governing location for tension
    governing_idx = int(np.argmax(te_max))
    governing_s = arc_lengths[governing_idx]
    
    # Extract time history at governing location
    times = list(model.SampleTimes(period))
    te_ts = list(line.TimeHistory("Effective Tension", period, objectExtra=ofx.oeArcLength(governing_s)))
    
    time_series = [
        TimeSeriesData(id="te_governing", label=f"Te @ {governing_s:.1f}m", 
                       t=times, values=te_ts, units="kN", arc_length_m=governing_s)
    ]

    return DynamicResultsData(
        ramp_end_time_s=model.general.RampDuration,
        time_series=time_series,
        envelopes=envelopes
    )
