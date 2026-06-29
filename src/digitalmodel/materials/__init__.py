# ABOUTME: Canonical material-grade matrix — single source of truth for steel
# ABOUTME: grade strengths (API 5L / ISO 3183, IACS UR W11, EN 10025-2).
"""Canonical material-grade matrix for digitalmodel.

One reconciled record per grade (see :class:`MaterialGrade`) replaces the eight-
plus scattered strength dicts that previously disagreed on X52 SMYS (358 / 359 /
360 MPa) and mixed psi / MPa / Pa units.  Strengths are stored in **MPa**; psi
helpers and back-compat adapters are provided for existing consumers.

Standards covered:
  * API 5L / ISO 3183 line pipe — A25..X120 (PSL1 and PSL2).
  * IACS UR W11 hull structural steel — A/B/D/E, AH/DH/EH/FH 32/36/40.
  * EN 10025-2 structural steel — S275/S355/S420.
"""
from .grades import (
    MaterialGrade,
    GRADES,
    get,
    all_grades,
    by_standard,
    smys_mpa,
    smts_mpa,
    smys_psi,
    smts_psi,
    smys_kn_m2,
    smts_kn_m2,
    smys_pa,
    smts_pa,
    legacy_smys_psi_dict,
    legacy_smts_psi_dict,
    legacy_smys_table_mpa,
    legacy_pipe_db_grades_mpa,
    legacy_pipe_db_grades_pa,
    legacy_riser_pipe_grades_mpa,
    legacy_pipeline_material_library_mpa,
    MPA_PER_PSI,
    PSI_PER_MPA,
)
from .line_pipe import (
    LinePipe,
    line_pipe,
    NPS_OD_IN,
    SCHEDULE_WT_IN,
    list_nps,
    od_for_nps,
    available_schedules,
    wt_for_schedule,
)

__all__ = [
    "MaterialGrade",
    "GRADES",
    "get",
    "all_grades",
    "by_standard",
    "smys_mpa",
    "smts_mpa",
    "smys_psi",
    "smts_psi",
    "smys_kn_m2",
    "smts_kn_m2",
    "smys_pa",
    "smts_pa",
    "legacy_smys_psi_dict",
    "legacy_smts_psi_dict",
    "legacy_smys_table_mpa",
    "legacy_pipe_db_grades_mpa",
    "legacy_pipe_db_grades_pa",
    "legacy_riser_pipe_grades_mpa",
    "legacy_pipeline_material_library_mpa",
    "MPA_PER_PSI",
    "PSI_PER_MPA",
    # line pipe
    "LinePipe",
    "line_pipe",
    "NPS_OD_IN",
    "SCHEDULE_WT_IN",
    "list_nps",
    "od_for_nps",
    "available_schedules",
    "wt_for_schedule",
]
