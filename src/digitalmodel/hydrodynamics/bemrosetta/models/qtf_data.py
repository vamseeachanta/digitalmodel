"""
QTF (Quadratic Transfer Function) Data Model

Data model for storing second-order wave force coefficients.
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any

import numpy as np
from numpy.typing import NDArray


class QTFType(Enum):
    """Type of QTF component."""

    SUM_FREQUENCY = "sum"  # Second-order sum-frequency forces
    DIFFERENCE_FREQUENCY = "difference"  # Second-order difference-frequency forces
    MEAN_DRIFT = "mean_drift"  # Mean drift forces (diagonal of difference QTF)


@dataclass
class QTFComponent:
    """Single QTF component for one DOF and heading combination.

    Attributes:
        dof: Degree of freedom index (0-5 for surge, sway, heave, roll, pitch, yaw).
        heading: Wave heading in degrees.
        qtf_type: Type of QTF (sum or difference frequency).
        frequencies_1: First set of frequencies in rad/s.
        frequencies_2: Second set of frequencies in rad/s.
        amplitude: QTF amplitude matrix [n_freq1 x n_freq2].
        phase: QTF phase matrix in radians [n_freq1 x n_freq2].
    """

    dof: int
    heading: float
    qtf_type: QTFType
    frequencies_1: NDArray[np.float64]
    frequencies_2: NDArray[np.float64]
    amplitude: NDArray[np.float64]
    phase: NDArray[np.float64]

    def __post_init__(self):
        """Validate array dimensions."""
        n1 = len(self.frequencies_1)
        n2 = len(self.frequencies_2)

        if self.amplitude.shape != (n1, n2):
            raise ValueError(
                f"Amplitude shape {self.amplitude.shape} does not match "
                f"frequency dimensions ({n1}, {n2})"
            )

        if self.phase.shape != (n1, n2):
            raise ValueError(
                f"Phase shape {self.phase.shape} does not match "
                f"frequency dimensions ({n1}, {n2})"
            )

    @property
    def complex_qtf(self) -> NDArray[np.complex128]:
        """Get QTF as complex array (amplitude * exp(i*phase))."""
        return self.amplitude * np.exp(1j * self.phase)

    def get_mean_drift(self) -> NDArray[np.float64]:
        """Extract mean drift forces from diagonal.

        Only valid for difference-frequency QTF.

        Returns:
            Mean drift force at each frequency.
        """
        if self.qtf_type != QTFType.DIFFERENCE_FREQUENCY:
            raise ValueError("Mean drift only available for difference-frequency QTF")

        return np.diag(self.amplitude)


@dataclass
class QTFData:
    """Container for complete QTF data set.

    Attributes:
        qtf_type: Type of QTF data (sum or difference frequency).
        body_name: Name of the body.
        components: List of QTF components for each DOF/heading.
        frequencies: Unique frequencies used in the analysis.
        headings: Wave headings in degrees.
        dof_names: Names of degrees of freedom.
        metadata: Additional metadata.
    """

    qtf_type: QTFType
    body_name: str = ""
    components: list[QTFComponent] = field(default_factory=list)
    frequencies: NDArray[np.float64] = field(
        default_factory=lambda: np.array([], dtype=np.float64)
    )
    headings: NDArray[np.float64] = field(
        default_factory=lambda: np.array([], dtype=np.float64)
    )
    dof_names: list[str] = field(
        default_factory=lambda: ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    )
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def n_frequencies(self) -> int:
        """Number of frequencies in the QTF."""
        return len(self.frequencies)

    @property
    def n_headings(self) -> int:
        """Number of headings."""
        return len(self.headings)

    @property
    def n_dof(self) -> int:
        """Number of degrees of freedom (typically 6)."""
        return len(self.dof_names)

    def get_component(self, dof: int, heading: float) -> QTFComponent | None:
        """Get QTF component for specific DOF and heading.

        Args:
            dof: Degree of freedom index (0-5).
            heading: Wave heading in degrees.

        Returns:
            QTFComponent if found, None otherwise.
        """
        for comp in self.components:
            if comp.dof == dof and np.isclose(comp.heading, heading, atol=0.1):
                return comp
        return None

    def get_mean_drift_forces(self) -> dict[int, dict[float, NDArray[np.float64]]]:
        """Extract mean drift forces for all DOFs and headings.

        Returns:
            Nested dict: {dof: {heading: mean_drift_array}}.
        """
        if self.qtf_type != QTFType.DIFFERENCE_FREQUENCY:
            raise ValueError("Mean drift only available for difference-frequency QTF")

        result: dict[int, dict[float, NDArray[np.float64]]] = {}
        for comp in self.components:
            if comp.dof not in result:
                result[comp.dof] = {}
            result[comp.dof][comp.heading] = comp.get_mean_drift()

        return result

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization.

        Returns:
            Dictionary representation (arrays as lists).
        """
        return {
            "qtf_type": self.qtf_type.value,
            "body_name": self.body_name,
            "frequencies": self.frequencies.tolist(),
            "headings": self.headings.tolist(),
            "dof_names": self.dof_names,
            "metadata": self.metadata,
            "components": [
                {
                    "dof": comp.dof,
                    "heading": comp.heading,
                    "frequencies_1": comp.frequencies_1.tolist(),
                    "frequencies_2": comp.frequencies_2.tolist(),
                    "amplitude": comp.amplitude.tolist(),
                    "phase": comp.phase.tolist(),
                }
                for comp in self.components
            ],
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "QTFData":
        """Create from dictionary representation.

        Args:
            data: Dictionary with QTF data.

        Returns:
            QTFData instance.
        """
        qtf_type = QTFType(data["qtf_type"])

        components = [
            QTFComponent(
                dof=c["dof"],
                heading=c["heading"],
                qtf_type=qtf_type,
                frequencies_1=np.array(c["frequencies_1"], dtype=np.float64),
                frequencies_2=np.array(c["frequencies_2"], dtype=np.float64),
                amplitude=np.array(c["amplitude"], dtype=np.float64),
                phase=np.array(c["phase"], dtype=np.float64),
            )
            for c in data.get("components", [])
        ]

        return cls(
            qtf_type=qtf_type,
            body_name=data.get("body_name", ""),
            components=components,
            frequencies=np.array(data.get("frequencies", []), dtype=np.float64),
            headings=np.array(data.get("headings", []), dtype=np.float64),
            dof_names=data.get(
                "dof_names", ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
            ),
            metadata=data.get("metadata", {}),
        )
