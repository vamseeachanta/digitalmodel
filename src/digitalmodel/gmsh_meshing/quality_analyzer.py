#!/usr/bin/env python3
"""
ABOUTME: Mesh quality analysis for finite element meshes including Jacobian,
aspect ratio, and skewness calculations.
"""

import numpy as np
from typing import Dict, List
from .models import MeshQuality, MeshStatistics


class MeshQualityAnalyzer:
    """
    Analyze finite element mesh quality

    Computes standard quality metrics:
    - Jacobian (element distortion)
    - Aspect Ratio (element shape)
    - Skewness (element skew)
    """

    def __init__(self):
        """Initialize quality analyzer"""
        pass

    def analyze_tetrahedral_mesh(
        self,
        nodes: np.ndarray,
        elements: np.ndarray
    ) -> MeshQuality:
        """
        Analyze tetrahedral mesh quality

        Args:
            nodes: N×3 array of node coordinates
            elements: M×4 array of element connectivity (0-based)

        Returns:
            MeshQuality object with computed metrics
        """
        n_elements = len(elements)

        jacobians = np.zeros(n_elements)
        aspect_ratios = np.zeros(n_elements)
        skewnesses = np.zeros(n_elements)

        for i, elem in enumerate(elements):
            # Get element nodes
            pts = nodes[elem]

            # Compute quality metrics
            jacobians[i] = self._tetrahedron_jacobian(pts)
            aspect_ratios[i] = self._tetrahedron_aspect_ratio(pts)
            skewnesses[i] = self._tetrahedron_skewness(pts)

        # Count poor quality elements
        n_poor_jacobian = np.sum(jacobians < 0.3)
        n_poor_aspect = np.sum(aspect_ratios > 5.0)
        n_poor_skewness = np.sum(skewnesses > 0.7)

        return MeshQuality(
            min_jacobian=float(np.min(jacobians)),
            max_jacobian=float(np.max(jacobians)),
            mean_jacobian=float(np.mean(jacobians)),
            min_aspect_ratio=float(np.min(aspect_ratios)),
            max_aspect_ratio=float(np.max(aspect_ratios)),
            mean_aspect_ratio=float(np.mean(aspect_ratios)),
            min_skewness=float(np.min(skewnesses)),
            max_skewness=float(np.max(skewnesses)),
            mean_skewness=float(np.mean(skewnesses)),
            n_poor_jacobian=int(n_poor_jacobian),
            n_poor_aspect=int(n_poor_aspect),
            n_poor_skewness=int(n_poor_skewness),
        )

    def compute_mesh_statistics(
        self,
        nodes: np.ndarray,
        elements: Dict,
        element_type: str = "tetrahedron"
    ) -> MeshStatistics:
        """
        Compute complete mesh statistics

        Args:
            nodes: N×3 array of node coordinates
            elements: Dictionary with element connectivity
            element_type: Element type string

        Returns:
            MeshStatistics object
        """
        n_nodes = len(nodes)

        # Get element data
        if isinstance(elements, dict):
            # From GMSH format
            elem_data = list(elements.values())[0]  # Take first element type
            elem_conn = elem_data['connectivity']
            dimension = elem_data['dimension']
        else:
            # Assume direct array
            elem_conn = elements
            dimension = 3

        n_elements = len(elem_conn)

        # Compute element sizes
        element_sizes = self._compute_element_sizes(nodes, elem_conn)

        min_size = float(np.min(element_sizes))
        max_size = float(np.max(element_sizes))
        mean_size = float(np.mean(element_sizes))

        # Compute quality if tetrahedra
        quality = None
        if element_type == "tetrahedron" and elem_conn.shape[1] == 4:
            quality = self.analyze_tetrahedral_mesh(nodes, elem_conn)

        return MeshStatistics(
            n_nodes=n_nodes,
            n_elements=n_elements,
            element_type=element_type,
            dimension=dimension,
            min_element_size=min_size,
            max_element_size=max_size,
            mean_element_size=mean_size,
            quality=quality,
        )

    def _tetrahedron_jacobian(self, pts: np.ndarray) -> float:
        """
        Compute Jacobian for tetrahedral element

        Jacobian = det(J) / |J|_Frobenius
        where J is the Jacobian matrix

        Args:
            pts: 4×3 array of tetrahedron vertices

        Returns:
            Normalized Jacobian (0-1, higher is better)
        """
        # Edges from first vertex
        e1 = pts[1] - pts[0]
        e2 = pts[2] - pts[0]
        e3 = pts[3] - pts[0]

        # Jacobian matrix
        J = np.column_stack([e1, e2, e3])

        # Determinant
        det_J = np.linalg.det(J)

        # Frobenius norm
        norm_J = np.linalg.norm(J, 'fro')

        if norm_J < 1e-12:
            return 0.0

        # Normalized Jacobian
        jacobian = det_J / norm_J**3

        return float(jacobian)

    def _tetrahedron_aspect_ratio(self, pts: np.ndarray) -> float:
        """
        Compute aspect ratio for tetrahedral element

        AR = max_edge_length / min_altitude

        Args:
            pts: 4×3 array of tetrahedron vertices

        Returns:
            Aspect ratio (1.0 is ideal, higher is worse)
        """
        # Compute all edge lengths
        edges = []
        for i in range(4):
            for j in range(i+1, 4):
                edge_length = np.linalg.norm(pts[i] - pts[j])
                edges.append(edge_length)

        max_edge = max(edges)

        # Compute volume
        v = pts[1] - pts[0]
        u = pts[2] - pts[0]
        w = pts[3] - pts[0]
        volume = abs(np.dot(v, np.cross(u, w))) / 6.0

        if volume < 1e-12:
            return 999.0

        # Minimum altitude = 3 * volume / base_area
        # Approximate using volume and max edge
        min_altitude = 3 * volume / (max_edge**2)

        aspect_ratio = max_edge / min_altitude if min_altitude > 1e-12 else 999.0

        return float(aspect_ratio)

    def _tetrahedron_skewness(self, pts: np.ndarray) -> float:
        """
        Compute skewness for tetrahedral element

        Skewness measures deviation from ideal shape

        Args:
            pts: 4×3 array of tetrahedron vertices

        Returns:
            Skewness (0 is ideal, 1 is degenerate)
        """
        # Compute all face areas
        areas = []
        for i in range(4):
            # Face opposite to vertex i
            face_pts = pts[[j for j in range(4) if j != i]]

            v1 = face_pts[1] - face_pts[0]
            v2 = face_pts[2] - face_pts[0]

            area = 0.5 * np.linalg.norm(np.cross(v1, v2))
            areas.append(area)

        # Skewness from area variation
        if max(areas) < 1e-12:
            return 1.0

        skewness = 1.0 - (min(areas) / max(areas))

        return float(skewness)

    def _compute_element_sizes(
        self,
        nodes: np.ndarray,
        elements: np.ndarray
    ) -> np.ndarray:
        """
        Compute characteristic size for each element

        Args:
            nodes: Node coordinates
            elements: Element connectivity

        Returns:
            Array of element sizes
        """
        n_elements = len(elements)
        sizes = np.zeros(n_elements)

        for i, elem in enumerate(elements):
            pts = nodes[elem]

            # Characteristic size = max edge length
            max_edge = 0.0
            for j in range(len(elem)):
                for k in range(j+1, len(elem)):
                    edge_length = np.linalg.norm(pts[j] - pts[k])
                    max_edge = max(max_edge, edge_length)

            sizes[i] = max_edge

        return sizes
