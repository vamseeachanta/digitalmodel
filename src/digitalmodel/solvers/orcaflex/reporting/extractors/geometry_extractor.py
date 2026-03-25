"""
Extractor for structural geometry from OrcaFlex models.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

from ..models.geometry import GeometryData, LineProfileData, KeyPointData


def extract_geometry(line: 'ofx.OrcaFlexLineObject') -> GeometryData:
    """Extracts static geometry and profiles from an OrcaFlex line."""
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    model = line.model
    arc_lengths = list(line.NodeArclengths)
    
    # Extract static shape
    x_s = [line.StaticResult("X", ofx.oeArcLength(s)) for s in arc_lengths]
    y_s = [line.StaticResult("Y", ofx.oeArcLength(s)) for s in arc_lengths]
    z_s = [line.StaticResult("Z", ofx.oeArcLength(s)) for s in arc_lengths]
    
    # Identify basic key points
    key_points = [
        KeyPointData(label="End A", arc_length_m=arc_lengths[0], x=x_s[0], y=y_s[0], z=z_s[0]),
        KeyPointData(label="End B", arc_length_m=arc_lengths[-1], x=x_s[-1], y=y_s[-1], z=z_s[-1]),
    ]
    
    # Attempt to find TDP (min Z point on laid section if any)
    # This is a simplification
    tdp_idx = z_s.index(min(z_s))
    if 0 < tdp_idx < len(z_s) - 1:
        key_points.append(KeyPointData(
            label="Static TDP", 
            arc_length_m=arc_lengths[tdp_idx], 
            x=x_s[tdp_idx], y=y_s[tdp_idx], z=z_s[tdp_idx]
        ))

    return GeometryData(
        water_depth_m=model.environment.WaterDepth,
        line_profile=LineProfileData(arc_length=arc_lengths, x=x_s, y=y_s, z=z_s),
        key_points=key_points
    )
