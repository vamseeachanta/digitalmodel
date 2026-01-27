"""
Pipeline Installation Schematic Visualization Module.

Creates plan and elevation view schematics for OrcaFlex pipeline installation
models with boundary condition markups.

Supports two modes:
1. YAML-only mode: Generates Plotly schematics from YAML model data
2. OrcaFlex API mode: Captures actual screenshots from OrcaFlex and combines
   with boundary condition overlays
"""

from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import base64
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import yaml
from loguru import logger

# Optional OrcaFlex API import
try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    logger.debug("OrcFxAPI not available - API screenshot mode disabled")


class BoundaryConditionType(Enum):
    """Types of boundary conditions in pipeline installation models."""

    FIXED_SEABED = "fixed_seabed"  # End A - all DOF constrained
    FREE_6DOF = "free_6dof"  # End B - 6D Buoy connection
    WINCH_TENSION = "winch_tension"  # Tension controlled
    FIXED_SUPPORT = "fixed_support"  # Tug supports
    VERTICAL_RESTRAINT = "vertical_restraint"  # Roller support
    DISTRIBUTED_BUOYANCY = "distributed_buoyancy"  # Buoyancy modules


# Color scheme for boundary conditions
BC_COLORS = {
    BoundaryConditionType.FIXED_SEABED: "#8B4513",  # Brown
    BoundaryConditionType.FREE_6DOF: "#1E90FF",  # Blue
    BoundaryConditionType.WINCH_TENSION: "#FF4500",  # Orange
    BoundaryConditionType.FIXED_SUPPORT: "#228B22",  # Green
    BoundaryConditionType.VERTICAL_RESTRAINT: "#9932CC",  # Purple
    BoundaryConditionType.DISTRIBUTED_BUOYANCY: "#FFD700",  # Gold
}


@dataclass
class BoundaryCondition:
    """Represents a boundary condition with its location and properties."""

    name: str
    bc_type: BoundaryConditionType
    position: Tuple[float, float, float]
    properties: Dict = field(default_factory=dict)

    @property
    def x(self) -> float:
        return self.position[0]

    @property
    def y(self) -> float:
        return self.position[1]

    @property
    def z(self) -> float:
        return self.position[2]


@dataclass
class PipelineData:
    """Parsed pipeline model data."""

    # Pipeline path coordinates
    path_x: np.ndarray
    path_y: np.ndarray
    path_z: np.ndarray

    # Boundary conditions
    boundary_conditions: List[BoundaryCondition]

    # Model metadata
    model_name: str = ""
    water_depth: float = 0.0
    pipeline_length: float = 0.0


class PipelineSchematicGenerator:
    """Generate plan and elevation view schematics with boundary conditions."""

    def __init__(self, yaml_file: str, output_dir: Optional[str] = None):
        """
        Initialize the schematic generator.

        Args:
            yaml_file: Path to the OrcaFlex YAML model file.
            output_dir: Optional output directory for generated files.
        """
        self.yaml_file = Path(yaml_file)
        self.output_dir = Path(output_dir) if output_dir else self.yaml_file.parent
        self.output_dir.mkdir(parents=True, exist_ok=True)

        self._model_data: Optional[PipelineData] = None
        self._raw_yaml: Optional[Dict] = None

    def parse_model(self) -> PipelineData:
        """
        Parse the YAML model file and extract pipeline and BC data.

        Returns:
            PipelineData object with parsed information.
        """
        if self._model_data is not None:
            return self._model_data

        # Load YAML
        with open(self.yaml_file, "r", encoding="utf-8") as f:
            self._raw_yaml = yaml.safe_load(f)

        # Load base file if present and merge data
        self._load_base_file()

        # Parse pipeline path
        path_x, path_y, path_z = self._parse_pipeline_path()

        # Parse boundary conditions
        boundary_conditions = self._parse_boundary_conditions()

        # Get metadata
        model_name = self.yaml_file.stem
        water_depth = self._get_water_depth()
        pipeline_length = self._calculate_pipeline_length(path_x, path_y, path_z)

        self._model_data = PipelineData(
            path_x=path_x,
            path_y=path_y,
            path_z=path_z,
            boundary_conditions=boundary_conditions,
            model_name=model_name,
            water_depth=water_depth,
            pipeline_length=pipeline_length,
        )

        return self._model_data

    def _load_base_file(self) -> None:
        """Load and merge base file data if BaseFile is specified."""
        base_file_name = self._raw_yaml.get("BaseFile")
        if not base_file_name:
            return

        # Resolve base file path relative to main file
        base_file_path = self.yaml_file.parent / base_file_name
        if not base_file_path.exists():
            # Try without extension changes
            return

        try:
            with open(base_file_path, "r", encoding="utf-8") as f:
                base_yaml = yaml.safe_load(f)

            # Merge base file data into main YAML
            # Base file data is overridden by main file where conflicts exist
            self._merge_yaml(base_yaml, self._raw_yaml)

        except Exception as e:
            print(f"Warning: Could not load base file {base_file_path}: {e}")

    def _merge_yaml(self, base: Dict, overlay: Dict) -> None:
        """Merge base YAML data with overlay, modifying base in place."""
        for key, value in base.items():
            if key not in overlay:
                overlay[key] = value
            elif key == "6DBuoys":
                # Special handling for 6DBuoys which can be list or dict
                self._merge_6dbuoys(value, overlay)
            elif key == "Lines":
                # Special handling for Lines which can be list or dict
                self._merge_lines(value, overlay)
            elif isinstance(value, dict) and isinstance(overlay.get(key), dict):
                self._merge_yaml(value, overlay[key])
            elif isinstance(value, list) and isinstance(overlay.get(key), list):
                # For other lists, add unique items from base
                for item in value:
                    if item not in overlay[key]:
                        overlay[key].append(item)

    def _merge_lines(self, base_lines, overlay: Dict) -> None:
        """Merge Lines from base file into overlay, handling list/dict formats."""
        overlay_lines = overlay.get("Lines", {})

        # Convert base list format to dict if needed
        base_dict = {}
        if isinstance(base_lines, list):
            for line in base_lines:
                if isinstance(line, dict) and "Name" in line:
                    name = line["Name"]
                    base_dict[name] = line
        elif isinstance(base_lines, dict):
            base_dict = base_lines

        # Ensure overlay Lines is a dict
        if not isinstance(overlay_lines, dict):
            overlay["Lines"] = {}
            overlay_lines = overlay["Lines"]

        # Merge each line from base into overlay
        for name, line_data in base_dict.items():
            if name not in overlay_lines:
                overlay_lines[name] = line_data
            elif isinstance(line_data, dict) and isinstance(overlay_lines.get(name), dict):
                # Merge line properties (preserves overlay values, adds missing from base)
                for prop_key, prop_value in line_data.items():
                    if prop_key not in overlay_lines[name]:
                        overlay_lines[name][prop_key] = prop_value

    def _merge_6dbuoys(self, base_buoys, overlay: Dict) -> None:
        """Merge 6DBuoys from base file into overlay, handling list/dict formats."""
        # Convert base to list of buoy dicts
        base_list = []
        if isinstance(base_buoys, list):
            base_list = base_buoys
        elif isinstance(base_buoys, dict):
            for name, buoy_data in base_buoys.items():
                if isinstance(buoy_data, dict):
                    buoy_copy = buoy_data.copy()
                    if "Name" not in buoy_copy:
                        buoy_copy["Name"] = name
                    base_list.append(buoy_copy)

        # Get existing overlay buoy names
        overlay_buoys = overlay.get("6DBuoys", {})
        overlay_names = set()

        if isinstance(overlay_buoys, dict):
            overlay_names = set(overlay_buoys.keys())
            for name, buoy_data in overlay_buoys.items():
                if isinstance(buoy_data, dict) and "Name" in buoy_data:
                    overlay_names.add(buoy_data["Name"])
        elif isinstance(overlay_buoys, list):
            for item in overlay_buoys:
                if isinstance(item, dict):
                    overlay_names.add(item.get("Name", ""))

        # Add base buoys that aren't in overlay
        for buoy in base_list:
            if isinstance(buoy, dict):
                name = buoy.get("Name", "")
                if name and name not in overlay_names:
                    # Convert overlay to list format if needed
                    if isinstance(overlay.get("6DBuoys"), dict):
                        overlay["6DBuoys"][name] = buoy
                    elif isinstance(overlay.get("6DBuoys"), list):
                        overlay["6DBuoys"].append(buoy)
                    else:
                        overlay["6DBuoys"] = [buoy]

    def _parse_pipeline_path(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Extract pipeline trajectory from YAML."""
        lines = self._raw_yaml.get("Lines", {})
        pipeline = lines.get("pipeline", {})

        # Get the starting shape data
        shape_key = "StartingShapeX, StartingShapeY, StartingShapeZ, StartingShapeAzm, StartingShapeDec, StartingShapeGamma"
        shape_data = pipeline.get(shape_key, [])

        if not shape_data:
            # Try alternative format
            shape_data = pipeline.get("StartingShape", [])

        if not shape_data:
            raise ValueError("No pipeline path data found in YAML")

        # Extract X, Y, Z coordinates
        coords = np.array(shape_data)
        path_x = coords[:, 0]
        path_y = coords[:, 1]
        path_z = coords[:, 2]

        return path_x, path_y, path_z

    def _parse_boundary_conditions(self) -> List[BoundaryCondition]:
        """Extract boundary conditions from the model."""
        bcs = []

        # 1. End A - Fixed seabed (first point of pipeline)
        lines = self._raw_yaml.get("Lines", {})
        pipeline = lines.get("pipeline", {})
        shape_key = "StartingShapeX, StartingShapeY, StartingShapeZ, StartingShapeAzm, StartingShapeDec, StartingShapeGamma"
        shape_data = pipeline.get(shape_key, [])

        if shape_data:
            first_point = shape_data[0]
            bcs.append(
                BoundaryCondition(
                    name="End A - Fixed",
                    bc_type=BoundaryConditionType.FIXED_SEABED,
                    position=(first_point[0], first_point[1], first_point[2]),
                    properties={"constraint": "All 6 DOF fixed"},
                )
            )

        # 2. 6D Buoys (including vessel connection at End B)
        buoys_6d = self._raw_yaml.get("6DBuoys", {})
        buoy_list = []
        if isinstance(buoys_6d, dict):
            for key, value in buoys_6d.items():
                if isinstance(value, dict):
                    # Check for position in either format
                    if "InitialX" in value or "InitialPosition" in value:
                        buoy_copy = value.copy()
                        if "Name" not in buoy_copy:
                            buoy_copy["Name"] = key
                        buoy_list.append(buoy_copy)
        elif isinstance(buoys_6d, list):
            buoy_list = buoys_6d

        for buoy in buoy_list:
            if isinstance(buoy, dict):
                name = buoy.get("Name", "")
                connection = buoy.get("Connection", "Free")

                # Get position
                if "InitialX" in buoy:
                    pos = (
                        buoy.get("InitialX", 0),
                        buoy.get("InitialY", 0),
                        buoy.get("InitialZ", 0),
                    )
                elif "InitialPosition" in buoy:
                    pos = tuple(buoy.get("InitialPosition", [0, 0, 0]))
                else:
                    continue

                # Determine BC type based on name, connection, and properties
                name_lower = name.lower()
                volume = buoy.get("Volume", 0)
                buoy_type = buoy.get("BuoyType", "").lower()

                # Buoyancy modules: connected to pipeline with volume > 0
                if (connection.lower() == "pipeline" or "pipeline" in connection.lower()) and volume > 0:
                    bcs.append(
                        BoundaryCondition(
                            name=f"Buoyancy - {name}",
                            bc_type=BoundaryConditionType.DISTRIBUTED_BUOYANCY,
                            position=pos,
                            properties={
                                "connection": connection,
                                "volume": volume,
                                "connection_z": buoy.get("ConnectionzRelativeTo", ""),
                            },
                        )
                    )
                # 6D Buoy vessel connection (free floating)
                elif "buoy" in name_lower and connection != "Fixed":
                    bcs.append(
                        BoundaryCondition(
                            name=f"End B - {name}",
                            bc_type=BoundaryConditionType.FREE_6DOF,
                            position=pos,
                            properties={"connection": connection},
                        )
                    )
                # Roller supports
                elif "roller" in name_lower:
                    bcs.append(
                        BoundaryCondition(
                            name=name,
                            bc_type=BoundaryConditionType.VERTICAL_RESTRAINT,
                            position=pos,
                            properties={"connection": connection},
                        )
                    )
                # Tug supports
                elif "tug" in name_lower:
                    bcs.append(
                        BoundaryCondition(
                            name=name,
                            bc_type=BoundaryConditionType.FIXED_SUPPORT,
                            position=pos,
                            properties={"connection": connection},
                        )
                    )

        # 3. Winches
        winches = self._raw_yaml.get("Winches", {})
        if isinstance(winches, dict):
            for key, value in winches.items():
                if isinstance(value, dict) and key != "New" and key != "Deleted":
                    name = value.get("Name", key)
                    # Winch position - typically at anchor or vessel
                    # For now, estimate based on pipeline end
                    if shape_data:
                        last_point = shape_data[-1]
                        # Winch anchor is typically beyond end B
                        winch_pos = (5000, 0, 2)  # Default position
                        bcs.append(
                            BoundaryCondition(
                                name=f"Winch - {name}",
                                bc_type=BoundaryConditionType.WINCH_TENSION,
                                position=winch_pos,
                                properties={"winch_type": value.get("WinchType", "Simple")},
                            )
                        )

        # 4. Pipeline attachments (buoyancy modules)
        attachments = pipeline.get("Attachments", [])
        if attachments and shape_data:
            # Calculate arc lengths along pipeline for interpolation
            coords = np.array(shape_data)
            path_x, path_y, path_z = coords[:, 0], coords[:, 1], coords[:, 2]
            arc_lengths = self._calculate_arc_lengths(path_x, path_y, path_z)

            # Group attachments by type and collect arc length info
            bm_attachments = [a for a in attachments if a.get("AttachmentType") == "BM"]

            if bm_attachments:
                # Get arc length range for buoyancy modules
                arc_positions = []
                for att in bm_attachments:
                    att_pos = att.get("AttachmentPosition", [0, 0, 0])
                    if len(att_pos) >= 3:
                        arc_len = att_pos[2]  # Arc length is 3rd element
                        arc_positions.append(arc_len)

                if arc_positions:
                    min_arc = min(arc_positions)
                    max_arc = max(arc_positions)
                    count = len(arc_positions)

                    # Get positions at start, middle, and end of buoyancy region
                    key_arcs = [min_arc, (min_arc + max_arc) / 2, max_arc]
                    for i, arc in enumerate(key_arcs):
                        pos = self._interpolate_position(arc, arc_lengths, path_x, path_y, path_z)
                        label = ["Start", "Middle", "End"][i]
                        bcs.append(
                            BoundaryCondition(
                                name=f"Buoyancy Zone {label}",
                                bc_type=BoundaryConditionType.DISTRIBUTED_BUOYANCY,
                                position=pos,
                                properties={
                                    "total_count": count,
                                    "arc_start": min_arc,
                                    "arc_end": max_arc,
                                    "zone": label,
                                },
                            )
                        )

        return bcs

    def _calculate_arc_lengths(
        self, x: np.ndarray, y: np.ndarray, z: np.ndarray
    ) -> np.ndarray:
        """Calculate cumulative arc length along pipeline path."""
        dx = np.diff(x)
        dy = np.diff(y)
        dz = np.diff(z)
        segment_lengths = np.sqrt(dx**2 + dy**2 + dz**2)
        arc_lengths = np.zeros(len(x))
        arc_lengths[1:] = np.cumsum(segment_lengths)
        return arc_lengths

    def _interpolate_position(
        self,
        arc_length: float,
        arc_lengths: np.ndarray,
        x: np.ndarray,
        y: np.ndarray,
        z: np.ndarray,
    ) -> Tuple[float, float, float]:
        """Interpolate position along pipeline at given arc length."""
        # Find the segment containing this arc length
        idx = np.searchsorted(arc_lengths, arc_length)
        if idx == 0:
            return (float(x[0]), float(y[0]), float(z[0]))
        if idx >= len(arc_lengths):
            return (float(x[-1]), float(y[-1]), float(z[-1]))

        # Linear interpolation within segment
        t = (arc_length - arc_lengths[idx - 1]) / (arc_lengths[idx] - arc_lengths[idx - 1])
        px = x[idx - 1] + t * (x[idx] - x[idx - 1])
        py = y[idx - 1] + t * (y[idx] - y[idx - 1])
        pz = z[idx - 1] + t * (z[idx] - z[idx - 1])
        return (float(px), float(py), float(pz))

    def _get_water_depth(self) -> float:
        """Get water depth from environment settings."""
        env = self._raw_yaml.get("Environment", {})
        return abs(env.get("SeabedOriginDepth", 0))

    def _calculate_pipeline_length(
        self, x: np.ndarray, y: np.ndarray, z: np.ndarray
    ) -> float:
        """Calculate total pipeline length from coordinates."""
        dx = np.diff(x)
        dy = np.diff(y)
        dz = np.diff(z)
        lengths = np.sqrt(dx**2 + dy**2 + dz**2)
        return float(np.sum(lengths))

    def create_plan_view(self) -> go.Figure:
        """
        Generate the XY plane (top-down) schematic.

        Returns:
            Plotly Figure object with the plan view.
        """
        data = self.parse_model()

        fig = go.Figure()

        # 1. Draw pipeline path
        fig.add_trace(
            go.Scatter(
                x=data.path_x,
                y=data.path_y,
                mode="lines",
                name="Pipeline",
                line=dict(color="#2C3E50", width=4),
                hovertemplate="X: %{x:.1f}m<br>Y: %{y:.1f}m<extra>Pipeline</extra>",
            )
        )

        # 2. Add boundary condition markers
        for bc in data.boundary_conditions:
            marker_size = self._get_marker_size(bc.bc_type)
            marker_symbol = self._get_marker_symbol(bc.bc_type)
            color = BC_COLORS.get(bc.bc_type, "#333333")

            fig.add_trace(
                go.Scatter(
                    x=[bc.x],
                    y=[bc.y],
                    mode="markers+text",
                    name=bc.name,
                    marker=dict(
                        size=marker_size,
                        symbol=marker_symbol,
                        color=color,
                        line=dict(color="black", width=1),
                    ),
                    text=[bc.name],
                    textposition="top center",
                    textfont=dict(size=10),
                    hovertemplate=(
                        f"<b>{bc.name}</b><br>"
                        f"Type: {bc.bc_type.value}<br>"
                        f"X: {bc.x:.1f}m<br>"
                        f"Y: {bc.y:.1f}m<br>"
                        f"Z: {bc.z:.1f}m"
                        "<extra></extra>"
                    ),
                )
            )

        # 3. Add scale bar
        self._add_scale_bar(fig, data.path_x, data.path_y)

        # Layout settings
        fig.update_layout(
            title=dict(
                text=f"Plan View (XY Plane) - {data.model_name}",
                font=dict(size=18, color="#2C3E50"),
            ),
            xaxis=dict(
                title="Global X (m)",
                scaleanchor="y",
                scaleratio=1,
                showgrid=True,
                gridcolor="lightgray",
            ),
            yaxis=dict(
                title="Global Y (m)",
                showgrid=True,
                gridcolor="lightgray",
            ),
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1,
            ),
            template="plotly_white",
            height=600,
            margin=dict(l=60, r=60, t=80, b=60),
        )

        return fig

    def create_elevation_view(self) -> go.Figure:
        """
        Generate the XZ plane (side) schematic.

        Returns:
            Plotly Figure object with the elevation view.
        """
        data = self.parse_model()

        fig = go.Figure()

        # 1. Draw water surface
        x_range = [min(data.path_x) - 100, max(data.path_x) + 200]
        fig.add_trace(
            go.Scatter(
                x=x_range,
                y=[0, 0],
                mode="lines",
                name="Water Surface",
                line=dict(color="#00BFFF", width=2, dash="dash"),
            )
        )

        # 2. Draw seabed
        seabed_z = -data.water_depth
        fig.add_trace(
            go.Scatter(
                x=x_range,
                y=[seabed_z, seabed_z],
                mode="lines",
                name="Seabed",
                line=dict(color="#8B4513", width=3),
                fill="tozeroy",
                fillcolor="rgba(139, 69, 19, 0.1)",
            )
        )

        # 3. Draw pipeline profile
        fig.add_trace(
            go.Scatter(
                x=data.path_x,
                y=data.path_z,
                mode="lines",
                name="Pipeline",
                line=dict(color="#2C3E50", width=4),
                hovertemplate="X: %{x:.1f}m<br>Z: %{y:.1f}m<extra>Pipeline</extra>",
            )
        )

        # 4. Add boundary condition markers
        for bc in data.boundary_conditions:
            marker_size = self._get_marker_size(bc.bc_type)
            marker_symbol = self._get_marker_symbol(bc.bc_type)
            color = BC_COLORS.get(bc.bc_type, "#333333")

            fig.add_trace(
                go.Scatter(
                    x=[bc.x],
                    y=[bc.z],
                    mode="markers+text",
                    name=bc.name,
                    marker=dict(
                        size=marker_size,
                        symbol=marker_symbol,
                        color=color,
                        line=dict(color="black", width=1),
                    ),
                    text=[bc.name],
                    textposition="top center",
                    textfont=dict(size=10),
                    hovertemplate=(
                        f"<b>{bc.name}</b><br>"
                        f"Type: {bc.bc_type.value}<br>"
                        f"X: {bc.x:.1f}m<br>"
                        f"Z: {bc.z:.1f}m"
                        "<extra></extra>"
                    ),
                    showlegend=False,  # Already shown in plan view legend
                )
            )

        # 5. Add annotations for water depth
        fig.add_annotation(
            x=x_range[0] + 50,
            y=seabed_z / 2,
            text=f"Water Depth: {data.water_depth:.1f}m",
            showarrow=False,
            font=dict(size=12, color="#00BFFF"),
        )

        # Layout settings
        fig.update_layout(
            title=dict(
                text=f"Elevation View (XZ Plane) - {data.model_name}",
                font=dict(size=18, color="#2C3E50"),
            ),
            xaxis=dict(
                title="Global X (m)",
                showgrid=True,
                gridcolor="lightgray",
            ),
            yaxis=dict(
                title="Global Z (m)",
                showgrid=True,
                gridcolor="lightgray",
            ),
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1,
            ),
            template="plotly_white",
            height=500,
            margin=dict(l=60, r=60, t=80, b=60),
        )

        return fig

    def _get_marker_symbol(self, bc_type: BoundaryConditionType) -> str:
        """Get the Plotly marker symbol for a boundary condition type."""
        symbols = {
            BoundaryConditionType.FIXED_SEABED: "triangle-up",
            BoundaryConditionType.FREE_6DOF: "circle",
            BoundaryConditionType.WINCH_TENSION: "diamond",
            BoundaryConditionType.FIXED_SUPPORT: "square",
            BoundaryConditionType.VERTICAL_RESTRAINT: "triangle-down",
            BoundaryConditionType.DISTRIBUTED_BUOYANCY: "star",
        }
        return symbols.get(bc_type, "circle")

    def _get_marker_size(self, bc_type: BoundaryConditionType) -> int:
        """Get the marker size for a boundary condition type."""
        sizes = {
            BoundaryConditionType.FIXED_SEABED: 20,
            BoundaryConditionType.FREE_6DOF: 18,
            BoundaryConditionType.WINCH_TENSION: 16,
            BoundaryConditionType.FIXED_SUPPORT: 14,
            BoundaryConditionType.VERTICAL_RESTRAINT: 16,
            BoundaryConditionType.DISTRIBUTED_BUOYANCY: 12,
        }
        return sizes.get(bc_type, 12)

    def _add_scale_bar(
        self, fig: go.Figure, x_data: np.ndarray, y_data: np.ndarray
    ) -> None:
        """Add a scale bar to the figure."""
        x_range = max(x_data) - min(x_data)

        # Determine appropriate scale bar length
        if x_range > 4000:
            scale_length = 1000
        elif x_range > 1000:
            scale_length = 500
        elif x_range > 200:
            scale_length = 100
        else:
            scale_length = 50

        # Position scale bar at bottom left
        scale_x_start = min(x_data) + x_range * 0.02
        scale_y = min(y_data) - abs(max(y_data) - min(y_data)) * 0.1

        fig.add_shape(
            type="line",
            x0=scale_x_start,
            y0=scale_y,
            x1=scale_x_start + scale_length,
            y1=scale_y,
            line=dict(color="black", width=3),
        )

        fig.add_annotation(
            x=scale_x_start + scale_length / 2,
            y=scale_y,
            text=f"{scale_length}m",
            showarrow=False,
            yshift=-15,
            font=dict(size=12),
        )

    def create_boundary_condition_legend(self) -> go.Figure:
        """Create a standalone legend figure explaining boundary conditions."""
        fig = go.Figure()

        bc_types = list(BoundaryConditionType)
        descriptions = {
            BoundaryConditionType.FIXED_SEABED: "Fixed seabed connection (all 6 DOF constrained)",
            BoundaryConditionType.FREE_6DOF: "6D Buoy connection (6 DOF free, vessel motion)",
            BoundaryConditionType.WINCH_TENSION: "Winch system (tension controlled)",
            BoundaryConditionType.FIXED_SUPPORT: "Fixed support (Tug support positions)",
            BoundaryConditionType.VERTICAL_RESTRAINT: "Roller support (vertical restraint only)",
            BoundaryConditionType.DISTRIBUTED_BUOYANCY: "Buoyancy modules (distributed upward force)",
        }

        for i, bc_type in enumerate(bc_types):
            y_pos = len(bc_types) - i

            fig.add_trace(
                go.Scatter(
                    x=[0.5],
                    y=[y_pos],
                    mode="markers",
                    marker=dict(
                        size=20,
                        symbol=self._get_marker_symbol(bc_type),
                        color=BC_COLORS[bc_type],
                        line=dict(color="black", width=1),
                    ),
                    showlegend=False,
                )
            )

            fig.add_annotation(
                x=1,
                y=y_pos,
                text=f"<b>{bc_type.value.replace('_', ' ').title()}</b>: {descriptions[bc_type]}",
                showarrow=False,
                xanchor="left",
                font=dict(size=12),
            )

        fig.update_layout(
            title="Boundary Condition Legend",
            xaxis=dict(visible=False, range=[0, 10]),
            yaxis=dict(visible=False, range=[0, len(bc_types) + 1]),
            height=300,
            margin=dict(l=20, r=20, t=50, b=20),
            template="plotly_white",
        )

        return fig

    def generate_html_report(
        self, output_filename: Optional[str] = None
    ) -> Path:
        """
        Generate a combined HTML report with both views.

        Args:
            output_filename: Optional custom filename for the report.

        Returns:
            Path to the generated HTML file.
        """
        data = self.parse_model()

        # Create figures
        plan_view = self.create_plan_view()
        elevation_view = self.create_elevation_view()
        legend_fig = self.create_boundary_condition_legend()

        # Generate HTML
        output_file = output_filename or f"{data.model_name}_schematic_report.html"
        output_path = self.output_dir / output_file

        html_content = self._generate_html_content(
            plan_view, elevation_view, legend_fig, data
        )

        with open(output_path, "w", encoding="utf-8") as f:
            f.write(html_content)

        print(f"Report generated: {output_path}")
        return output_path

    def _generate_html_content(
        self,
        plan_view: go.Figure,
        elevation_view: go.Figure,
        legend_fig: go.Figure,
        data: PipelineData,
    ) -> str:
        """Generate the HTML content for the report."""
        from datetime import datetime

        plan_json = plan_view.to_json()
        elevation_json = elevation_view.to_json()
        legend_json = legend_fig.to_json()

        # Count boundary conditions by type
        bc_counts = {}
        for bc in data.boundary_conditions:
            bc_type_name = bc.bc_type.value.replace("_", " ").title()
            bc_counts[bc_type_name] = bc_counts.get(bc_type_name, 0) + 1

        bc_summary_html = "".join(
            f'<div class="stat-card"><div class="stat-label">{name}</div>'
            f'<div class="stat-value">{count}</div></div>'
            for name, count in bc_counts.items()
        )

        return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Pipeline Schematic - {data.model_name}</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background-color: #f5f7fa;
            padding: 20px;
            color: #2d3748;
        }}
        .container {{ max-width: 1600px; margin: 0 auto; }}
        .header {{
            background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
            color: white;
            padding: 40px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .header h1 {{ font-size: 2.2em; margin-bottom: 15px; }}
        .header p {{ font-size: 1.1em; opacity: 0.95; margin: 5px 0; }}
        .stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 15px;
            margin-bottom: 25px;
        }}
        .stat-card {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
            text-align: center;
        }}
        .stat-label {{ font-size: 0.85em; color: #718096; margin-bottom: 5px; text-transform: uppercase; }}
        .stat-value {{ font-size: 1.8em; font-weight: 700; color: #2C3E50; }}
        .stat-unit {{ font-size: 0.5em; color: #a0aec0; }}
        .plot-container {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
        }}
        .plot-container h2 {{
            font-size: 1.3em;
            margin-bottom: 15px;
            color: #2d3748;
            border-bottom: 2px solid #3498DB;
            padding-bottom: 10px;
        }}
        .plot {{ width: 100%; }}
        .two-col {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        @media (max-width: 1200px) {{ .two-col {{ grid-template-columns: 1fr; }} }}
        .footer {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            margin-top: 25px;
            text-align: center;
            color: #718096;
            font-size: 0.9em;
        }}
        .bc-section {{ margin-top: 20px; }}
        .bc-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Pipeline Installation Schematic</h1>
            <p><strong>Model:</strong> {data.model_name}</p>
            <p><strong>Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <p><strong>Pipeline Length:</strong> {data.pipeline_length:.1f} m</p>
            <p><strong>Water Depth:</strong> {data.water_depth:.1f} m</p>
        </div>

        <div class="stats">
            <div class="stat-card">
                <div class="stat-label">Pipeline Length</div>
                <div class="stat-value">{data.pipeline_length:.0f}<span class="stat-unit">m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Water Depth</div>
                <div class="stat-value">{data.water_depth:.1f}<span class="stat-unit">m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Boundary Conditions</div>
                <div class="stat-value">{len(data.boundary_conditions)}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Path Points</div>
                <div class="stat-value">{len(data.path_x)}</div>
            </div>
        </div>

        <div class="bc-section">
            <h3 style="margin-bottom: 15px; color: #2C3E50;">Boundary Conditions Summary</h3>
            <div class="bc-grid">{bc_summary_html}</div>
        </div>

        <div class="plot-container" style="margin-top: 25px;">
            <h2>Plan View (XY Plane - Top Down)</h2>
            <div id="plan-view" class="plot"></div>
        </div>

        <div class="plot-container">
            <h2>Elevation View (XZ Plane - Side Profile)</h2>
            <div id="elevation-view" class="plot"></div>
        </div>

        <div class="plot-container">
            <h2>Boundary Condition Legend</h2>
            <div id="legend" class="plot"></div>
        </div>

        <div class="footer">
            <p>Pipeline Schematic Visualization | OrcaFlex Model Analysis</p>
            <p>Interactive: Hover for details, click and drag to zoom, double-click to reset</p>
        </div>
    </div>

    <script>
        var planData = {plan_json};
        Plotly.newPlot('plan-view', planData.data, planData.layout, {{responsive: true}});

        var elevationData = {elevation_json};
        Plotly.newPlot('elevation-view', elevationData.data, elevationData.layout, {{responsive: true}});

        var legendData = {legend_json};
        Plotly.newPlot('legend', legendData.data, legendData.layout, {{responsive: true}});
    </script>
</body>
</html>"""

    def save_individual_views(self) -> Tuple[Path, Path]:
        """
        Save plan and elevation views as separate HTML files.

        Returns:
            Tuple of paths to the plan view and elevation view files.
        """
        data = self.parse_model()

        plan_view = self.create_plan_view()
        elevation_view = self.create_elevation_view()

        plan_path = self.output_dir / f"{data.model_name}_plan_view.html"
        elevation_path = self.output_dir / f"{data.model_name}_elevation_view.html"

        plan_view.write_html(str(plan_path))
        elevation_view.write_html(str(elevation_path))

        print(f"Plan view saved: {plan_path}")
        print(f"Elevation view saved: {elevation_path}")

        return plan_path, elevation_path

    # =========================================================================
    # OrcaFlex API Screenshot Methods
    # =========================================================================

    def capture_orcaflex_screenshots(
        self,
        sim_file: Optional[str] = None,
        view_params: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Path]:
        """
        Capture plan and elevation screenshots using OrcaFlex API.

        Args:
            sim_file: Path to .sim or .dat file. If None, uses YAML file path
                      with .sim extension.
            view_params: Optional custom view parameters. If None, uses defaults.

        Returns:
            Dict with 'plan' and 'elevation' keys containing image paths.

        Raises:
            RuntimeError: If OrcaFlex API is not available.
        """
        if not ORCAFLEX_AVAILABLE:
            raise RuntimeError(
                "OrcFxAPI not available. Install OrcaFlex or run on licensed machine."
            )

        # Determine simulation file path
        if sim_file is None:
            sim_file = str(self.yaml_file.with_suffix(".sim"))
            if not Path(sim_file).exists():
                sim_file = str(self.yaml_file.with_suffix(".dat"))

        if not Path(sim_file).exists():
            raise FileNotFoundError(f"Simulation file not found: {sim_file}")

        logger.info(f"Loading OrcaFlex model: {sim_file}")
        model = OrcFxAPI.Model(sim_file)

        # Get view parameters
        params = view_params or self._get_default_view_params(model)

        screenshots = {}

        # Capture plan view (XY - top down)
        if "plan" in params:
            plan_path = self.output_dir / f"{self.yaml_file.stem}_orcaflex_plan.jpg"
            self._capture_view(model, params["plan"], plan_path)
            screenshots["plan"] = plan_path
            logger.info(f"Plan view saved: {plan_path}")

        # Capture elevation view (XZ - side)
        if "elevation" in params:
            elev_path = self.output_dir / f"{self.yaml_file.stem}_orcaflex_elevation.jpg"
            self._capture_view(model, params["elevation"], elev_path)
            screenshots["elevation"] = elev_path
            logger.info(f"Elevation view saved: {elev_path}")

        return screenshots

    def _get_default_view_params(self, model) -> Dict[str, Dict]:
        """
        Get default view parameters for plan and elevation views.

        Calculates appropriate view center and size based on model extents.
        """
        data = self.parse_model()

        # Calculate model extents
        x_min, x_max = float(np.min(data.path_x)), float(np.max(data.path_x))
        y_min, y_max = float(np.min(data.path_y)), float(np.max(data.path_y))
        z_min, z_max = float(np.min(data.path_z)), float(np.max(data.path_z))

        x_center = (x_min + x_max) / 2
        y_center = (y_min + y_max) / 2
        z_center = (z_min + z_max) / 2

        x_range = x_max - x_min
        y_range = max(y_max - y_min, 100)  # Minimum 100m for visibility
        z_range = max(z_max - z_min, 50)

        # View size with padding
        plan_size = max(x_range, y_range) * 1.2
        elev_size = max(x_range, z_range) * 1.2

        return {
            "plan": {
                "ViewCentre": [x_center, y_center, 0],
                "ViewSize": plan_size,
                "ViewAzimuth": 270,  # Looking down from above
                "ViewElevation": 90,  # Top-down view
                "ViewGamma": 0,
                "GraphicsMode": "WireFrame",
                "FileWidth": 1920,
                "FileHeight": 1080,
                "BackgroundColour": 16777215,  # White
                "DrawSeaSurface": False,
                "DrawSeabed": True,
            },
            "elevation": {
                "ViewCentre": [x_center, 0, z_center],
                "ViewSize": elev_size,
                "ViewAzimuth": 270,  # Looking from +Y toward -Y
                "ViewElevation": 0,  # Side view
                "ViewGamma": 0,
                "GraphicsMode": "WireFrame",
                "FileWidth": 1920,
                "FileHeight": 600,
                "BackgroundColour": 16777215,  # White
                "DrawSeaSurface": True,
                "DrawSeabed": True,
            },
        }

    def _capture_view(
        self,
        model,
        params: Dict[str, Any],
        output_path: Path,
    ) -> None:
        """Capture a single view using OrcaFlex API."""
        viewparams = model.defaultViewParameters

        # Apply view parameters
        for key, value in params.items():
            try:
                if key == "ViewCentre":
                    for i, v in enumerate(value):
                        viewparams.ViewCentre[i] = v
                elif key == "RelativeToObject" and value:
                    viewparams.RelativeToObject = model[value]
                elif hasattr(viewparams, key):
                    setattr(viewparams, key, value)
            except Exception as e:
                logger.warning(f"Could not set view parameter {key}: {e}")

        # Save the view
        model.SaveModelView(str(output_path), viewparams)

    def generate_combined_report(
        self,
        sim_file: Optional[str] = None,
        include_api_screenshots: bool = True,
        view_params: Optional[Dict[str, Any]] = None,
        output_filename: Optional[str] = None,
    ) -> Path:
        """
        Generate HTML report with OrcaFlex API screenshots and Plotly schematics.

        This combines:
        1. OrcaFlex API screenshots (actual model visualization)
        2. Plotly boundary condition schematics (interactive with BC markups)

        Args:
            sim_file: Path to .sim or .dat file for API screenshots.
            include_api_screenshots: Whether to capture OrcaFlex API screenshots.
            view_params: Custom view parameters for API screenshots.
            output_filename: Optional custom filename for the report.

        Returns:
            Path to the generated HTML file.
        """
        data = self.parse_model()

        # Capture OrcaFlex screenshots if requested and available
        screenshots = {}
        if include_api_screenshots and ORCAFLEX_AVAILABLE:
            try:
                screenshots = self.capture_orcaflex_screenshots(sim_file, view_params)
            except Exception as e:
                logger.warning(f"Could not capture OrcaFlex screenshots: {e}")

        # Create Plotly figures
        plan_view = self.create_plan_view()
        elevation_view = self.create_elevation_view()
        legend_fig = self.create_boundary_condition_legend()

        # Generate HTML
        output_file = output_filename or f"{data.model_name}_combined_report.html"
        output_path = self.output_dir / output_file

        html_content = self._generate_combined_html_content(
            plan_view, elevation_view, legend_fig, data, screenshots
        )

        with open(output_path, "w", encoding="utf-8") as f:
            f.write(html_content)

        logger.info(f"Combined report generated: {output_path}")
        return output_path

    def _generate_combined_html_content(
        self,
        plan_view: go.Figure,
        elevation_view: go.Figure,
        legend_fig: go.Figure,
        data: PipelineData,
        screenshots: Dict[str, Path],
    ) -> str:
        """Generate HTML content with both API screenshots and Plotly schematics."""
        from datetime import datetime

        plan_json = plan_view.to_json()
        elevation_json = elevation_view.to_json()
        legend_json = legend_fig.to_json()

        # Count boundary conditions by type
        bc_counts = {}
        for bc in data.boundary_conditions:
            bc_type_name = bc.bc_type.value.replace("_", " ").title()
            bc_counts[bc_type_name] = bc_counts.get(bc_type_name, 0) + 1

        bc_summary_html = "".join(
            f'<div class="stat-card"><div class="stat-label">{name}</div>'
            f'<div class="stat-value">{count}</div></div>'
            for name, count in bc_counts.items()
        )

        # Encode screenshots as base64 for embedding
        screenshot_html = ""
        if screenshots:
            screenshot_html = '<div class="screenshot-section">'
            screenshot_html += '<h2 style="margin-bottom: 20px; color: #2C3E50;">OrcaFlex Model Views</h2>'
            screenshot_html += '<div class="screenshot-grid">'

            for view_name, img_path in screenshots.items():
                if img_path.exists():
                    with open(img_path, "rb") as img_file:
                        img_base64 = base64.b64encode(img_file.read()).decode("utf-8")
                    view_title = view_name.replace("_", " ").title()
                    screenshot_html += f'''
                    <div class="screenshot-card">
                        <h3>{view_title} View (OrcaFlex)</h3>
                        <img src="data:image/jpeg;base64,{img_base64}" alt="{view_title}" />
                    </div>'''

            screenshot_html += '</div></div>'

        return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Pipeline Schematic - {data.model_name}</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background-color: #f5f7fa;
            padding: 20px;
            color: #2d3748;
        }}
        .container {{ max-width: 1800px; margin: 0 auto; }}
        .header {{
            background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
            color: white;
            padding: 40px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .header h1 {{ font-size: 2.2em; margin-bottom: 15px; }}
        .header p {{ font-size: 1.1em; opacity: 0.95; margin: 5px 0; }}
        .stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 15px;
            margin-bottom: 25px;
        }}
        .stat-card {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
            text-align: center;
        }}
        .stat-label {{ font-size: 0.85em; color: #718096; margin-bottom: 5px; text-transform: uppercase; }}
        .stat-value {{ font-size: 1.8em; font-weight: 700; color: #2C3E50; }}
        .stat-unit {{ font-size: 0.5em; color: #a0aec0; }}
        .plot-container {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
        }}
        .plot-container h2 {{
            font-size: 1.3em;
            margin-bottom: 15px;
            color: #2d3748;
            border-bottom: 2px solid #3498DB;
            padding-bottom: 10px;
        }}
        .plot {{ width: 100%; }}
        .screenshot-section {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
        }}
        .screenshot-grid {{
            display: grid;
            grid-template-columns: 1fr;
            gap: 20px;
        }}
        .screenshot-card {{
            border: 1px solid #e2e8f0;
            border-radius: 8px;
            overflow: hidden;
        }}
        .screenshot-card h3 {{
            background: #f7fafc;
            padding: 12px 15px;
            font-size: 1.1em;
            color: #2d3748;
            border-bottom: 1px solid #e2e8f0;
        }}
        .screenshot-card img {{
            width: 100%;
            display: block;
        }}
        .two-col {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        @media (max-width: 1200px) {{ .two-col {{ grid-template-columns: 1fr; }} }}
        .footer {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            margin-top: 25px;
            text-align: center;
            color: #718096;
            font-size: 0.9em;
        }}
        .bc-section {{ margin-top: 20px; }}
        .bc-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px; }}
        .section-divider {{
            border-top: 3px solid #3498DB;
            margin: 30px 0;
            padding-top: 20px;
        }}
        .section-title {{
            font-size: 1.5em;
            color: #2C3E50;
            margin-bottom: 20px;
            display: flex;
            align-items: center;
            gap: 10px;
        }}
        .section-title::before {{
            content: "";
            width: 4px;
            height: 24px;
            background: #3498DB;
            border-radius: 2px;
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Pipeline Installation Schematic</h1>
            <p><strong>Model:</strong> {data.model_name}</p>
            <p><strong>Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <p><strong>Pipeline Length:</strong> {data.pipeline_length:.1f} m</p>
            <p><strong>Water Depth:</strong> {data.water_depth:.1f} m</p>
        </div>

        <div class="stats">
            <div class="stat-card">
                <div class="stat-label">Pipeline Length</div>
                <div class="stat-value">{data.pipeline_length:.0f}<span class="stat-unit">m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Water Depth</div>
                <div class="stat-value">{data.water_depth:.1f}<span class="stat-unit">m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Boundary Conditions</div>
                <div class="stat-value">{len(data.boundary_conditions)}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Path Points</div>
                <div class="stat-value">{len(data.path_x)}</div>
            </div>
        </div>

        <div class="bc-section">
            <h3 style="margin-bottom: 15px; color: #2C3E50;">Boundary Conditions Summary</h3>
            <div class="bc-grid">{bc_summary_html}</div>
        </div>

        {screenshot_html}

        <div class="section-divider">
            <div class="section-title">Interactive Schematic Views</div>
        </div>

        <div class="plot-container">
            <h2>Plan View (XY Plane - Top Down) with Boundary Conditions</h2>
            <div id="plan-view" class="plot"></div>
        </div>

        <div class="plot-container">
            <h2>Elevation View (XZ Plane - Side Profile) with Boundary Conditions</h2>
            <div id="elevation-view" class="plot"></div>
        </div>

        <div class="plot-container">
            <h2>Boundary Condition Legend</h2>
            <div id="legend" class="plot"></div>
        </div>

        <div class="footer">
            <p>Pipeline Schematic Visualization | OrcaFlex Model Analysis</p>
            <p>Interactive: Hover for details, click and drag to zoom, double-click to reset</p>
        </div>
    </div>

    <script>
        var planData = {plan_json};
        Plotly.newPlot('plan-view', planData.data, planData.layout, {{responsive: true}});

        var elevationData = {elevation_json};
        Plotly.newPlot('elevation-view', elevationData.data, elevationData.layout, {{responsive: true}});

        var legendData = {legend_json};
        Plotly.newPlot('legend', legendData.data, legendData.layout, {{responsive: true}});
    </script>
</body>
</html>"""


# =============================================================================
# Standalone OrcaFlex View Capture Utility
# =============================================================================

class OrcaFlexViewCapture:
    """
    Utility class for capturing OrcaFlex model views.

    This can be used independently of PipelineSchematicGenerator for
    capturing screenshots from any OrcaFlex model.
    """

    # Default view configurations
    DEFAULT_VIEWS = {
        "plan": {
            "ViewAzimuth": 270,
            "ViewElevation": 90,
            "ViewGamma": 0,
            "GraphicsMode": "WireFrame",
            "FileWidth": 1920,
            "FileHeight": 1080,
            "BackgroundColour": 16777215,
            "DrawSeaSurface": False,
            "DrawSeabed": True,
        },
        "elevation": {
            "ViewAzimuth": 270,
            "ViewElevation": 0,
            "ViewGamma": 0,
            "GraphicsMode": "WireFrame",
            "FileWidth": 1920,
            "FileHeight": 600,
            "BackgroundColour": 16777215,
            "DrawSeaSurface": True,
            "DrawSeabed": True,
        },
        "isometric": {
            "ViewAzimuth": 315,
            "ViewElevation": 30,
            "ViewGamma": 0,
            "GraphicsMode": "WireFrame",
            "FileWidth": 1920,
            "FileHeight": 1080,
            "BackgroundColour": 16777215,
            "DrawSeaSurface": True,
            "DrawSeabed": True,
        },
    }

    def __init__(self, model_file: str, output_dir: Optional[str] = None):
        """
        Initialize the view capture utility.

        Args:
            model_file: Path to OrcaFlex model file (.dat, .sim, .yml).
            output_dir: Output directory for captured images.
        """
        if not ORCAFLEX_AVAILABLE:
            raise RuntimeError("OrcFxAPI not available")

        self.model_file = Path(model_file)
        self.output_dir = Path(output_dir) if output_dir else self.model_file.parent
        self.output_dir.mkdir(parents=True, exist_ok=True)

        self._model = None

    def load_model(self, calculate_statics: bool = True) -> None:
        """Load the OrcaFlex model."""
        logger.info(f"Loading model: {self.model_file}")
        self._model = OrcFxAPI.Model(str(self.model_file))

        if calculate_statics:
            logger.info("Calculating statics...")
            self._model.CalculateStatics()

    def capture_view(
        self,
        view_name: str,
        view_params: Optional[Dict[str, Any]] = None,
        output_filename: Optional[str] = None,
    ) -> Path:
        """
        Capture a single view.

        Args:
            view_name: Name of the view (used in filename and for default params).
            view_params: Custom view parameters (merged with defaults).
            output_filename: Custom output filename.

        Returns:
            Path to the captured image.
        """
        if self._model is None:
            self.load_model()

        # Get default params and merge with custom
        params = self.DEFAULT_VIEWS.get(view_name, {}).copy()
        if view_params:
            params.update(view_params)

        # Set up view parameters
        viewparams = self._model.defaultViewParameters
        for key, value in params.items():
            try:
                if key == "ViewCentre":
                    for i, v in enumerate(value):
                        viewparams.ViewCentre[i] = v
                elif key == "RelativeToObject" and value:
                    viewparams.RelativeToObject = self._model[value]
                elif hasattr(viewparams, key):
                    setattr(viewparams, key, value)
            except Exception as e:
                logger.warning(f"Could not set {key}: {e}")

        # Generate output path
        filename = output_filename or f"{self.model_file.stem}_{view_name}.jpg"
        output_path = self.output_dir / filename

        # Capture and save
        self._model.SaveModelView(str(output_path), viewparams)
        logger.info(f"View captured: {output_path}")

        return output_path

    def capture_all_views(
        self,
        views: Optional[List[str]] = None,
        custom_params: Optional[Dict[str, Dict]] = None,
    ) -> Dict[str, Path]:
        """
        Capture multiple views.

        Args:
            views: List of view names to capture. Defaults to ["plan", "elevation"].
            custom_params: Dict of view_name -> custom parameters.

        Returns:
            Dict mapping view names to output paths.
        """
        if views is None:
            views = ["plan", "elevation"]

        custom_params = custom_params or {}
        results = {}

        for view_name in views:
            params = custom_params.get(view_name)
            results[view_name] = self.capture_view(view_name, params)

        return results

    def set_visualization_settings(
        self,
        hide_items: Optional[List[str]] = None,
        sea_surface_style: str = "Clear",
    ) -> None:
        """
        Configure model visualization settings.

        Args:
            hide_items: List of object names to hide.
            sea_surface_style: Pen style for sea surface.
        """
        if self._model is None:
            self.load_model(calculate_statics=False)

        env = self._model["Environment"]
        env.SeaSurfacePenStyle = sea_surface_style

        if hide_items:
            all_objects = [str(obj) for obj in self._model.objects]
            for item in hide_items:
                if item in all_objects:
                    self._model[item].Hidden = "Yes"
                    logger.debug(f"Hidden: {item}")
