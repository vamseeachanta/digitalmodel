"""
ABOUTME: OrcaFlex template library — lookup canonical spec.yml/Jinja2 templates by structure type.
ABOUTME: Returns template content or path for all structure types in the OrcaFlex agent template set.
"""

from pathlib import Path
from jinja2 import Environment, FileSystemLoader, select_autoescape

_TEMPLATES_DIR = Path(__file__).parent / "templates" / "base_files"

STRUCTURE_TYPES: dict[str, str] = {
    "general": "01_general.yml.j2",
    "environment": "02_environment.yml.j2",
    "vessel": "04_vessel_wrapper.yml.j2",
    "vessel_instance": "05_vessel_inst_wrapper.yml.j2",
    "line_types": "06_line_types.yml.j2",
    "lines": "07_lines_wrapper.yml.j2",
    "buoys": "08_buoys_wrapper.yml.j2",
    "groups": "09_groups.yml.j2",
}

ALIASES: dict[str, str] = {
    "riser": "lines",
    "mooring": "lines",
    "hull": "vessel",
    "catenary": "lines",
    "tether": "lines",
}


def _resolve_structure_type(structure_type: str) -> str:
    """Resolve aliases to canonical structure type keys.

    Args:
        structure_type: Raw structure type or alias string.

    Returns:
        Canonical structure type key.

    Raises:
        ValueError: When the structure_type is not recognised.
    """
    normalized = structure_type.lower().strip()
    if normalized in STRUCTURE_TYPES:
        return normalized
    if normalized in ALIASES:
        return ALIASES[normalized]
    known = sorted(list(STRUCTURE_TYPES.keys()) + list(ALIASES.keys()))
    raise ValueError(
        f"Unknown structure_type '{structure_type}'. "
        f"Supported types: {known}"
    )


def get_orcaflex_template(
    structure_type: str,
    render: bool = False,
    context: dict | None = None,
) -> dict:
    """Return template metadata and content for a given OrcaFlex structure type.

    Args:
        structure_type: Structure type key or alias (e.g. "general", "riser",
            "vessel", "mooring").
        render: When True, attempt to render the Jinja2 template with
            ``context``.  If ``context`` is None or empty the rendered field
            will be None.
        context: Jinja2 template variables used when ``render=True``.

    Returns:
        Dictionary with keys:
            - ``structure_type`` (str): Resolved canonical type name.
            - ``template_file`` (str): Template filename.
            - ``template_path`` (str): Absolute path to the template file.
            - ``content`` (str): Raw Jinja2 template content.
            - ``rendered`` (str | None): Rendered output when requested;
              otherwise None.

    Raises:
        ValueError: When ``structure_type`` is not recognised.
        FileNotFoundError: When the mapped template file is missing on disk.
    """
    resolved = _resolve_structure_type(structure_type)
    template_file = STRUCTURE_TYPES[resolved]
    template_path = _TEMPLATES_DIR / template_file

    if not template_path.exists():
        raise FileNotFoundError(
            f"Template file not found: {template_path}"
        )

    content = template_path.read_text(encoding="utf-8")

    rendered: str | None = None
    if render and context:
        env = Environment(
            loader=FileSystemLoader(str(_TEMPLATES_DIR)),
            autoescape=select_autoescape(),
            trim_blocks=True,
            lstrip_blocks=True,
        )
        tmpl = env.get_template(template_file)
        rendered = tmpl.render(**context)

    return {
        "structure_type": resolved,
        "template_file": template_file,
        "template_path": str(template_path.resolve()),
        "content": content,
        "rendered": rendered,
    }


def list_structure_types() -> list[str]:
    """Return all supported structure types including aliases.

    Returns:
        Sorted list of canonical type names followed by alias names.
    """
    return sorted(STRUCTURE_TYPES.keys()) + sorted(ALIASES.keys())


def get_all_templates() -> list[dict]:
    """Return metadata for all canonical templates (without content).

    Returns:
        List of dicts, each containing ``structure_type``, ``template_file``,
        and ``template_path`` — one entry per canonical type.
    """
    result = []
    for structure_type, template_file in STRUCTURE_TYPES.items():
        template_path = _TEMPLATES_DIR / template_file
        result.append(
            {
                "structure_type": structure_type,
                "template_file": template_file,
                "template_path": str(template_path.resolve()),
            }
        )
    return result
