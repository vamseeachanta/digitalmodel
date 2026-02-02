"""Workflow orchestration for AQWA viscous damping determination."""

from __future__ import annotations

import glob
import logging
import shutil
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Union

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import plotly.io as pio
import yaml

from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_damping import AqwaDamping
from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_raos import AqwaRAOs
from digitalmodel.hydrodynamics.aqwa.aqwa_dat_files import AqwaDATFiles, DATInspectionResult
from digitalmodel.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser


LOGGER = logging.getLogger(__name__)
DEFAULT_RESULTS_DIR = Path(
    "specs/modules/aqwa/ship-analysis/results"
)


class ViscousDampingDetermination(AqwaRAOs):
    """Determine additional viscous damping leveraging existing AQWA modules."""

    def __init__(self) -> None:
        super().__init__()
        self.dat_parser = AqwaDATFiles()
        self.lis_parser = AQWAEnhancedParser()
        self.damping_analyzer = AqwaDamping()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run(
        self,
        config_source: Union[str, Path, Dict[str, Any]],
        results_directory: Optional[Union[str, Path]] = None,
    ) -> Dict[str, Any]:
        """Execute viscous damping determination workflow."""

        config = self._load_config(config_source)
        analysis_root = Path(config["context"]["analysis_source"]).resolve()

        output_dir = Path(results_directory) if results_directory else DEFAULT_RESULTS_DIR
        output_dir.mkdir(parents=True, exist_ok=True)

        master_results: List[Dict[str, Any]] = []
        for case_cfg in config.get("cases", []):
            dataset = self._prepare_case_dataset(case_cfg, analysis_root, output_dir)
            case_results = self._process_case(dataset, config)
            master_results.append(case_results)
            self._generate_case_reports(case_results, output_dir)

        self._aggregate_case_summaries(master_results, output_dir)

        return {"results": master_results, "output_directory": output_dir}

    # ------------------------------------------------------------------
    # Case preparation helpers
    # ------------------------------------------------------------------

    def _load_config(self, source: Union[str, Path, Dict[str, Any]]) -> Dict[str, Any]:
        if isinstance(source, dict):
            config = source
        else:
            config_path = Path(source)
            with config_path.open("r", encoding="utf-8") as stream:
                config = yaml.safe_load(stream)

        if "context" not in config or "analysis_source" not in config["context"]:
            raise ValueError("Configuration must define context.analysis_source")
        return config

    def _prepare_case_dataset(
        self,
        case_cfg: Dict[str, Any],
        analysis_root: Path,
        output_dir: Path,
    ) -> Dict[str, Any]:
        case_id = case_cfg["id"]
        case_dir = output_dir / case_id
        inputs_dir = case_dir / "inputs"
        inputs_dir.mkdir(parents=True, exist_ok=True)

        file_pairs = self._resolve_file_pairs(analysis_root, case_cfg.get("files", []))
        manifest: List[Dict[str, Any]] = []

        for dat_path, lis_path in file_pairs:
            copied_dat = self._copy_to_scratch(dat_path, inputs_dir)
            copied_lis = self._copy_to_scratch(lis_path, inputs_dir)

            record = {
                "id": Path(dat_path).stem,
                "dat": copied_dat,
                "lis": copied_lis,
            }
            record.update(self._infer_record_metadata(Path(dat_path).stem))
            manifest.append(record)

        return {
            "id": case_id,
            "description": case_cfg.get("description", ""),
            "case_dir": case_dir,
            "scratch": inputs_dir,
            "manifest": manifest,
        }

    def _resolve_file_pairs(
        self, analysis_root: Path, patterns: Sequence[str]
    ) -> List[Tuple[Path, Path]]:
        pairs: Dict[str, Dict[str, Path]] = {}

        for pattern in patterns:
            for expanded_pattern in self._expand_brace_pattern(pattern):
                absolute_pattern = analysis_root / expanded_pattern
                for file_path in glob.glob(str(absolute_pattern)):
                    path = Path(file_path)
                    extension = path.suffix.upper().lstrip(".")
                    if extension not in {"DAT", "LIS"}:
                        continue
                    base_key = str(path.with_suffix(""))
                    pairs.setdefault(base_key, {})[extension] = path

        missing_pairs = [key for key, value in pairs.items() if {"DAT", "LIS"} - set(value)]
        if missing_pairs:
            raise FileNotFoundError(
                f"Missing paired DAT/LIS files for: {', '.join(missing_pairs)}"
            )

        return [
            (pair_info["DAT"], pair_info["LIS"])
            for pair_info in pairs.values()
        ]

    def _expand_brace_pattern(self, pattern: str) -> List[str]:
        if "{" not in pattern:
            return [pattern]

        start = pattern.index("{")
        end = pattern.index("}")
        options = pattern[start + 1 : end].split(",")
        results = []
        for option in options:
            results.extend(
                self._expand_brace_pattern(
                    pattern[:start] + option + pattern[end + 1 :]
                )
            )
        return results

    def _copy_to_scratch(self, path: Path, scratch_dir: Path) -> Path:
        if not path.exists():
            raise FileNotFoundError(f"Input file not found: {path}")
        target = scratch_dir / path.name
        shutil.copy2(path, target)
        return target

    def _infer_record_metadata(self, stem: str) -> Dict[str, Any]:
        tokens = stem.split("_")
        vessel = tokens[0] if tokens else stem
        draft = next((token for token in tokens if token.upper() in {"MWL", "HWL", "LWL"}), "")
        return {"vessel": vessel, "draft": draft}

    # ------------------------------------------------------------------
    # Case processing
    # ------------------------------------------------------------------

    def _process_case(
        self,
        dataset: Dict[str, Any],
        config: Dict[str, Any],
    ) -> Dict[str, Any]:
        case_results: Dict[str, Any] = {
            "case_id": dataset["id"],
            "description": dataset.get("description", ""),
            "records": [],
        }

        hydro_cfg = config.get("hydrostatics", {})
        inertia_defaults = hydro_cfg.get("inertia", {})
        restoring_defaults = hydro_cfg.get("restoring", {})
        density = float(config.get("context", {}).get("fluid_density", 1025.0))

        for record in dataset["manifest"]:
            dat_path: Path = record["dat"]
            lis_path: Path = record["lis"]

            inspection = self.dat_parser.inspect_dat_file(str(dat_path))
            self.dat_parser.validate_ldrg_option(str(dat_path))
            self.dat_parser.validate_cog_element(str(dat_path))

            lis_basic = self.lis_parser.parse_lis_file(str(lis_path))
            frequencies = lis_basic["frequencies"]

            radiation_section = self.lis_parser.extract_damping_matrices(str(lis_path))
            added_mass_section = self.lis_parser.extract_added_mass_matrices(str(lis_path))
            added_mass_inf = self.lis_parser.extract_added_mass_at_infinite_frequency(str(lis_path))
            drag_table = self.lis_parser.extract_drag_table(str(lis_path))
            external_raw = self.lis_parser.extract_external_damping(str(lis_path))
            percent_critical = self.lis_parser.extract_percent_critical_damping(str(lis_path))
            restoring_section = self.lis_parser.extract_restoring_coefficients(str(lis_path))

            self._validate_frequency_alignment(frequencies, radiation_section.get("frequencies", np.array([])))
            self._validate_frequency_alignment(frequencies, added_mass_section.get("frequencies", np.array([])))

            radiation_components = self._extract_dof_diagonals(radiation_section)
            added_mass_frequency = self._extract_dof_diagonals(added_mass_section, prefix="A")
            added_mass_infinite = added_mass_inf.get("diagonal", {})
            restoring_from_lis = self._extract_restoring_diagonal(restoring_section)

            drag_properties = self._build_drag_properties(drag_table, inspection)
            viscous_components = self.damping_analyzer.derive_viscous_damping_statistical(
                frequencies,
                lis_basic["raos"],
                drag_properties,
                fluid_density=density,
            )

            external_damping = self._broadcast_external_damping(
                external_raw,
                len(frequencies),
            )

            combined = self.damping_analyzer.combine_damping(
                frequencies,
                radiation_components,
                viscous_components,
                external_damping,
            )

            inertia = dict(inspection.inertia) if inspection.inertia else {}
            if not inertia:
                inertia = self._resolve_hydro_property(inertia_defaults, record, required=True)
            elif inspection.mass and "I33" not in inertia:
                inertia["I33"] = inspection.mass

            restoring = restoring_from_lis or dict(inspection.restoring)
            if not restoring:
                restoring = self._resolve_hydro_property(restoring_defaults, record, required=True)

            ratios_inf = self.damping_analyzer.compute_critical_damping_ratios(
                frequencies,
                combined["total"],
                inertia,
                restoring,
                added_mass_infinite=added_mass_infinite,
                added_mass_frequency=added_mass_frequency,
                use_frequency_dependent=False,
            )

            ratios_freq = self.damping_analyzer.compute_critical_damping_ratios(
                frequencies,
                combined["total"],
                inertia,
                restoring,
                added_mass_infinite=added_mass_infinite,
                added_mass_frequency=added_mass_frequency,
                use_frequency_dependent=True,
            )

            record_result = {
                "record_id": record["id"],
                "vessel": record.get("vessel"),
                "draft": record.get("draft"),
                "frequencies": frequencies,
                "inspection": inspection,
                "radiation": radiation_components,
                "viscous": viscous_components,
                "external": external_damping,
                "combined": combined,
                "critical": {
                    "infinite_frequency": ratios_inf,
                    "frequency_dependent": ratios_freq,
                },
                "added_mass_frequency": added_mass_frequency,
                "added_mass_infinite": added_mass_infinite,
                "percent_critical": percent_critical,
                "drag_table": drag_table,
                "headings": lis_basic.get("headings"),
                "inertia": inertia,
                "restoring": restoring,
            }

            case_results["records"].append(record_result)

        return case_results

    def _resolve_hydro_property(
        self,
        property_cfg: Dict[str, Any],
        record: Dict[str, Any],
        required: bool = False,
    ) -> Dict[str, float]:
        if not property_cfg:
            if required:
                raise ValueError(
                    "Hydrostatic properties must be provided under config['hydrostatics']"
                )
            return {}

        record_key = record.get("id")
        vessel_key = record.get("vessel")

        if record_key and record_key in property_cfg:
            return dict(property_cfg[record_key])
        if vessel_key and vessel_key in property_cfg:
            return dict(property_cfg[vessel_key])
        return dict(property_cfg)

    def _validate_frequency_alignment(self, base: np.ndarray, other: np.ndarray) -> None:
        if other.size == 0:
            return
        if base.shape != other.shape or not np.allclose(base, other, rtol=1e-5, atol=1e-8):
            raise ValueError("Frequency grids between DAT/LIS data are inconsistent")

    def _extract_dof_diagonals(
        self,
        section: Dict[str, Any],
        prefix: str = "B",
    ) -> Dict[str, np.ndarray]:
        matrices = section.get("matrices")
        if matrices is None or matrices.size == 0:
            return {f"{prefix}33": np.array([]), f"{prefix}44": np.array([]), f"{prefix}55": np.array([])}

        matrices = np.asarray(matrices, dtype=float)
        return {
            f"{prefix}33": matrices[:, 2, 2],
            f"{prefix}44": matrices[:, 3, 3],
            f"{prefix}55": matrices[:, 4, 4],
        }

    def _build_drag_properties(
        self, drag_entries: List[Dict[str, Any]], inspection: DATInspectionResult
    ) -> Dict[str, List[Dict[str, float]]]:
        properties = {"heave": [], "roll": [], "pitch": []}
        cog = inspection.deck_one.cog_coordinates or (0.0, 0.0, 0.0)

        for entry in drag_entries:
            area = float(entry.get("area") or 0.0)
            cd_normal = float(entry.get("cd_normal") or 0.0)
            centroid = entry.get("centroid") or (0.0, 0.0, 0.0)
            lever_x = centroid[0] - cog[0]
            lever_y = centroid[1] - cog[1]

            properties["heave"].append({"cd": cd_normal, "area": area, "lever_arm": 0.0})
            properties["roll"].append({"cd": cd_normal, "area": area, "lever_arm": lever_y})
            properties["pitch"].append({"cd": cd_normal, "area": area, "lever_arm": lever_x})

        return properties

    def _broadcast_external_damping(
        self, external_raw: Dict[str, Any], length: int
    ) -> Dict[str, np.ndarray]:
        output: Dict[str, np.ndarray] = {}
        for key in ("B33", "B44", "B55"):
            values = np.asarray(external_raw.get(key, np.array([])), dtype=float)
            output[key] = self._broadcast_to_length(values, length)
        return output

    def _broadcast_to_length(self, values: np.ndarray, length: int) -> np.ndarray:
        if values.size == 0:
            return np.zeros(length)
        if values.size == length:
            return values
        if values.size == 1:
            return np.repeat(values, length)
        raise ValueError("Cannot broadcast external damping array to match frequency grid")

    def _extract_restoring_diagonal(self, restoring_section: Dict[str, Any]) -> Dict[str, float]:
        diagonal = restoring_section.get("diagonal", {}) if restoring_section else {}
        if not diagonal:
            return {}
        mapped = {}
        mapping = {
            "C33": "C33",
            "C44": "C44",
            "C55": "C55",
        }
        for target, source in mapping.items():
            if source in diagonal:
                try:
                    mapped[target] = float(diagonal[source])
                except (TypeError, ValueError):
                    continue
        return mapped

    # ------------------------------------------------------------------
    # Reporting
    # ------------------------------------------------------------------

    def _generate_case_reports(self, case_results: Dict[str, Any], results_dir: Path) -> None:
        case_id = case_results["case_id"]
        case_dir = results_dir / case_id
        case_dir.mkdir(parents=True, exist_ok=True)
        data_dir = case_dir / "data"
        data_dir.mkdir(exist_ok=True)

        figures: List[go.Figure] = []
        damping_tables: List[pd.DataFrame] = []

        for record in case_results["records"]:
            df_damping = self._build_damping_dataframe(record)
            df_percentages = self._build_percentage_dataframe(record)
            df_critical = self._build_critical_dataframe(record)

            df_damping.to_csv(data_dir / f"{record['record_id']}_damping.csv", index=False)
            df_percentages.to_csv(data_dir / f"{record['record_id']}_percentages.csv", index=False)
            df_critical.to_csv(data_dir / f"{record['record_id']}_critical.csv", index=False)

            damping_tables.append(df_damping)

        figures.extend(self._build_damping_vs_frequency_figures(case_results))
        figures.append(self._build_percentage_bar_chart(case_results))
        figures.append(self._build_critical_ratio_chart(case_results))
        figures.append(self._build_sensitivity_chart(case_results))

        html_output = case_dir / f"{case_id}_viscous_damping.html"
        html_output.write_text(self._compose_html_page(case_id, figures, damping_tables), encoding="utf-8")

    def _build_damping_dataframe(self, record: Dict[str, Any]) -> pd.DataFrame:
        freq = record["frequencies"]
        components = record["combined"]["components"]
        total = record["combined"]["total"]

        return pd.DataFrame(
            {
                "frequency": freq,
                "B33_radiation": components["radiation"].get("B33", np.zeros_like(freq)),
                "B33_viscous": components["viscous"].get("B33", np.zeros_like(freq)),
                "B33_external": components["external"].get("B33", np.zeros_like(freq)),
                "B33_total": total.get("B33", np.zeros_like(freq)),
                "B44_radiation": components["radiation"].get("B44", np.zeros_like(freq)),
                "B44_viscous": components["viscous"].get("B44", np.zeros_like(freq)),
                "B44_external": components["external"].get("B44", np.zeros_like(freq)),
                "B44_total": total.get("B44", np.zeros_like(freq)),
                "B55_radiation": components["radiation"].get("B55", np.zeros_like(freq)),
                "B55_viscous": components["viscous"].get("B55", np.zeros_like(freq)),
                "B55_external": components["external"].get("B55", np.zeros_like(freq)),
                "B55_total": total.get("B55", np.zeros_like(freq)),
            }
        )

    def _build_percentage_dataframe(self, record: Dict[str, Any]) -> pd.DataFrame:
        freq = record["frequencies"]
        percentages = record["combined"]["percentages"]

        return pd.DataFrame(
            {
                "frequency": freq,
                "B33_radiation_pct": percentages["B33"]["radiation"],
                "B33_viscous_pct": percentages["B33"]["viscous"],
                "B33_external_pct": percentages["B33"]["external"],
                "B44_radiation_pct": percentages["B44"]["radiation"],
                "B44_viscous_pct": percentages["B44"]["viscous"],
                "B44_external_pct": percentages["B44"]["external"],
                "B55_radiation_pct": percentages["B55"]["radiation"],
                "B55_viscous_pct": percentages["B55"]["viscous"],
                "B55_external_pct": percentages["B55"]["external"],
            }
        )

    def _build_critical_dataframe(self, record: Dict[str, Any]) -> pd.DataFrame:
        freq = record["frequencies"]
        ratios_inf = record["critical"]["infinite_frequency"]
        ratios_freq = record["critical"]["frequency_dependent"]

        return pd.DataFrame(
            {
                "frequency": freq,
                "zeta_heave_inf": ratios_inf["heave"]["critical_damping_ratio"],
                "zeta_roll_inf": ratios_inf["roll"]["critical_damping_ratio"],
                "zeta_pitch_inf": ratios_inf["pitch"]["critical_damping_ratio"],
                "zeta_heave_freq": ratios_freq["heave"]["critical_damping_ratio"],
                "zeta_roll_freq": ratios_freq["roll"]["critical_damping_ratio"],
                "zeta_pitch_freq": ratios_freq["pitch"]["critical_damping_ratio"],
            }
        )

    def _build_damping_vs_frequency_figures(self, case_results: Dict[str, Any]) -> List[go.Figure]:
        figures: List[go.Figure] = []
        for idx, (dof_key, title) in enumerate(
            [("B33", "Heave"), ("B44", "Roll"), ("B55", "Pitch")]
        ):
            fig = go.Figure()
            for record in case_results["records"]:
                freq = record["frequencies"]
                components = record["combined"]["components"]
                total = record["combined"]["total"]
                record_name = record["record_id"]
                fig.add_trace(
                    go.Scatter(
                        x=freq,
                        y=components["radiation"].get(dof_key, np.zeros_like(freq)),
                        mode="lines",
                        name=f"{record_name} radiation",
                    )
                )
                fig.add_trace(
                    go.Scatter(
                        x=freq,
                        y=components["viscous"].get(dof_key, np.zeros_like(freq)),
                        mode="lines",
                        name=f"{record_name} viscous",
                    )
                )
                fig.add_trace(
                    go.Scatter(
                        x=freq,
                        y=total.get(dof_key, np.zeros_like(freq)),
                        mode="lines",
                        name=f"{record_name} total",
                    )
                )

            fig.update_layout(
                title=f"{title} damping versus frequency",
                xaxis_title="Frequency (rad/s)",
                yaxis_title="Damping",
            )
            figures.append(fig)

        return figures

    def _build_percentage_bar_chart(self, case_results: Dict[str, Any]) -> go.Figure:
        fig = go.Figure()
        record_names = [record["record_id"] for record in case_results["records"]]
        for source in ("radiation", "viscous", "external"):
            values = [
                float(np.nanmean(record["combined"]["percentages"]["B33"][source]))
                for record in case_results["records"]
            ]
            fig.add_trace(go.Bar(x=record_names, y=values, name=source.capitalize()))

        fig.update_layout(
            title="Heave damping contribution breakdown",
            barmode="stack",
            yaxis_title="Percentage (%)",
        )
        return fig

    def _build_critical_ratio_chart(self, case_results: Dict[str, Any]) -> go.Figure:
        fig = go.Figure()
        record_names = [record["record_id"] for record in case_results["records"]]
        for dof in ("heave", "roll", "pitch"):
            ratios = [
                self._characteristic_ratio(record["critical"]["infinite_frequency"], dof)
                for record in case_results["records"]
            ]
            fig.add_trace(go.Bar(x=record_names, y=ratios, name=dof.capitalize()))

        fig.update_layout(
            title="Critical damping ratios (A_inf method)",
            barmode="group",
            yaxis_title="Critical damping ratio ζ",
        )
        return fig

    def _build_sensitivity_chart(self, case_results: Dict[str, Any]) -> go.Figure:
        fig = go.Figure()
        for record in case_results["records"]:
            record_name = record["record_id"]
            for dof in ("heave", "roll", "pitch"):
                inf_ratio = self._characteristic_ratio(record["critical"]["infinite_frequency"], dof)
                freq_ratio = self._characteristic_ratio(record["critical"]["frequency_dependent"], dof)
                fig.add_trace(
                    go.Bar(
                        x=[f"{record_name} {dof}"],
                        y=[inf_ratio],
                        name="A_inf",
                    )
                )
                fig.add_trace(
                    go.Bar(
                        x=[f"{record_name} {dof}"],
                        y=[freq_ratio],
                        name="A(ω)",
                    )
                )

        fig.update_layout(
            title="Critical damping sensitivity (A(ω) vs A_inf)",
            barmode="group",
            xaxis_title="Configuration / DOF",
            yaxis_title="Critical damping ratio ζ",
        )
        return fig

    def _characteristic_ratio(self, ratios: Dict[str, Any], dof: str) -> float:
        values = ratios[dof]["critical_damping_ratio"]
        return float(np.nanmax(values)) if values.size else 0.0

    def _compose_html_page(
        self,
        case_id: str,
        figures: Sequence[go.Figure],
        tables: Sequence[pd.DataFrame],
    ) -> str:
        html_sections: List[str] = []
        for idx, fig in enumerate(figures):
            include_js = "cdn" if idx == 0 else False
            html_sections.append(
                pio.to_html(fig, include_plotlyjs=include_js, full_html=False)
            )

        for df in tables:
            html_sections.append(df.to_html(index=False, classes="table table-striped"))

        return """<html><head><meta charset='utf-8'><title>{title}</title></head><body>{body}</body></html>""".format(
            title=f"Viscous damping report - {case_id}",
            body="\n".join(html_sections),
        )

    def _aggregate_case_summaries(
        self, master_results: Sequence[Dict[str, Any]], results_dir: Path
    ) -> None:
        summary_records: List[Dict[str, Any]] = []
        for case in master_results:
            for record in case["records"]:
                summary_records.append(
                    {
                        "case_id": case["case_id"],
                        "record_id": record["record_id"],
                        "vessel": record.get("vessel"),
                        "draft": record.get("draft"),
                        "max_zeta_heave": self._characteristic_ratio(record["critical"]["infinite_frequency"], "heave"),
                        "max_zeta_roll": self._characteristic_ratio(record["critical"]["infinite_frequency"], "roll"),
                        "max_zeta_pitch": self._characteristic_ratio(record["critical"]["infinite_frequency"], "pitch"),
                    }
                )

        if summary_records:
            df_summary = pd.DataFrame(summary_records)
            df_summary.to_csv(results_dir / "viscous_damping_master_summary.csv", index=False)
