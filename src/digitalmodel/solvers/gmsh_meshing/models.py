#!/usr/bin/env python3
"""
ABOUTME: Data models for GMSH meshing including mesh quality metrics, element types,
and mesh statistics for finite element analysis.
"""

import numpy as np
from dataclasses import dataclass, field
from typing import Optional, List, Dict
from enum import Enum


class GeometryType(Enum):
    """Supported geometry file types"""
    STEP = "step"
    IGES = "iges"
    STL = "stl"
    BREP = "brep"
    GEO = "geo"


class ElementType(Enum):
    """Finite element types"""
    LINE = "line"  # 1D
    TRIANGLE = "triangle"  # 2D
    QUADRILATERAL = "quadrilateral"  # 2D
    TETRAHEDRON = "tetrahedron"  # 3D
    HEXAHEDRON = "hexahedron"  # 3D
    PRISM = "prism"  # 3D
    PYRAMID = "pyramid"  # 3D


class MeshAlgorithm(Enum):
    """GMSH meshing algorithms"""
    MESHADAPT = "meshadapt"
    AUTOMATIC = "automatic"
    DELAUNAY = "delaunay"
    FRONTAL = "frontal"
    FRONTAL_DELAUNAY = "frontal_delaunay"  # Default, best quality
    PACKED_PARALLELOGRAMS = "packed_parallelograms"


@dataclass
class MeshQuality:
    """
    Mesh quality metrics for finite element analysis

    Quality thresholds (from GMSH best practices):
    - Jacobian: > 0.3 (good), < 0.1 (critical)
    - Aspect Ratio: < 5.0 (good), > 10.0 (critical)
    - Skewness: < 0.7 (good), > 0.9 (critical)
    """
    min_jacobian: float
    max_jacobian: float
    mean_jacobian: float

    min_aspect_ratio: float
    max_aspect_ratio: float
    mean_aspect_ratio: float

    min_skewness: float
    max_skewness: float
    mean_skewness: float

    n_poor_jacobian: int = 0  # Elements with Jacobian < 0.3
    n_poor_aspect: int = 0    # Elements with AR > 5.0
    n_poor_skewness: int = 0  # Elements with skewness > 0.7

    @property
    def is_acceptable(self) -> bool:
        """Check if mesh meets minimum quality standards"""
        return (
            self.min_jacobian > 0.1 and
            self.max_aspect_ratio < 10.0 and
            self.max_skewness < 0.9
        )

    @property
    def is_good(self) -> bool:
        """Check if mesh meets good quality standards"""
        return (
            self.min_jacobian > 0.3 and
            self.max_aspect_ratio < 5.0 and
            self.max_skewness < 0.7
        )

    @property
    def quality_score(self) -> float:
        """Overall quality score (0-100)"""
        # Weighted average of metrics
        jacobian_score = min(100, self.min_jacobian / 0.3 * 100)
        aspect_score = max(0, (1 - self.max_aspect_ratio / 10.0) * 100)
        skewness_score = max(0, (1 - self.max_skewness) * 100)

        return (jacobian_score + aspect_score + skewness_score) / 3.0

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'jacobian': {
                'min': float(self.min_jacobian),
                'max': float(self.max_jacobian),
                'mean': float(self.mean_jacobian),
                'poor_count': self.n_poor_jacobian,
            },
            'aspect_ratio': {
                'min': float(self.min_aspect_ratio),
                'max': float(self.max_aspect_ratio),
                'mean': float(self.mean_aspect_ratio),
                'poor_count': self.n_poor_aspect,
            },
            'skewness': {
                'min': float(self.min_skewness),
                'max': float(self.max_skewness),
                'mean': float(self.mean_skewness),
                'poor_count': self.n_poor_skewness,
            },
            'quality_score': float(self.quality_score),
            'is_acceptable': self.is_acceptable,
            'is_good': self.is_good,
        }


@dataclass
class MeshStatistics:
    """Complete mesh statistics"""
    n_nodes: int
    n_elements: int
    element_type: str

    # Dimension info
    dimension: int  # 1, 2, or 3

    # Size metrics
    min_element_size: float
    max_element_size: float
    mean_element_size: float

    # Volume/area (for 3D/2D)
    total_volume: Optional[float] = None
    total_area: Optional[float] = None

    # Quality
    quality: Optional[MeshQuality] = None

    @property
    def elements_per_node(self) -> float:
        """Average elements per node"""
        return self.n_elements / self.n_nodes if self.n_nodes > 0 else 0

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        result = {
            'nodes': self.n_nodes,
            'elements': self.n_elements,
            'element_type': self.element_type,
            'dimension': self.dimension,
            'element_size': {
                'min': float(self.min_element_size),
                'max': float(self.max_element_size),
                'mean': float(self.mean_element_size),
            },
            'elements_per_node': float(self.elements_per_node),
        }

        if self.total_volume is not None:
            result['total_volume'] = float(self.total_volume)

        if self.total_area is not None:
            result['total_area'] = float(self.total_area)

        if self.quality is not None:
            result['quality'] = self.quality.to_dict()

        return result
