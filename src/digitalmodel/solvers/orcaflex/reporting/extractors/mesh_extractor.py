"""
Extractor for mesh discretization from OrcaFlex models.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

import numpy as np
from ..models.mesh import MeshData, SegmentData, MeshQualityData


def extract_mesh(line: 'ofx.OrcaFlexLineObject') -> MeshData:
    """Extracts segment lengths and computes mesh quality ratios."""
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    segs = line.NumberOfSegments
    seg_lengths = [line.SegmentLength[i] for i in range(segs)]
    
    # Adjacent ratios
    adj_ratios = []
    for i in range(segs - 1):
        ratio = max(seg_lengths[i], seg_lengths[i+1]) / min(seg_lengths[i], seg_lengths[i+1])
        adj_ratios.append(ratio)
    
    worst_ratio = max(adj_ratios) if adj_ratios else 1.0
    worst_idx = adj_ratios.index(worst_ratio) if adj_ratios else 0
    
    # Cumulative arc length for segments
    arc_lens_seg = [sum(seg_lengths[:i]) for i in range(segs)]

    return MeshData(
        total_segment_count=segs,
        segments=[SegmentData(arc_length_m=arc_lens_seg[i], length_m=seg_lengths[i]) for i in range(segs)],
        quality=MeshQualityData(
            max_adjacent_ratio=worst_ratio,
            worst_ratio_arc_length_m=arc_lens_seg[worst_idx],
            verdict="PASS" if worst_ratio < 3.0 else "WARNING",
            adjacent_ratios=adj_ratios
        )
    )
