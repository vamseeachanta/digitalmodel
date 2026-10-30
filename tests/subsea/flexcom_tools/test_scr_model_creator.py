"""Independent analytical and rendering checks; no solver is imported.

Basis: Catenary Curve (2019), equations (1), (3), (4), (9), (10).
All dimensions and forces below are synthetic inputs, not design defaults.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.subsea.flexcom_tools.scr_model_creator import (
    Offset,
    SCRModelConfig,
    build_model_context,
    generate_flexcom_model,
    qualification_status,
)


@pytest.fixture
def config():
    """Hand case: H/w=2 m; h=3 m; s=sqrt(3*(3+4))=sqrt(21) m."""
    return SCRModelConfig(
        analysis_type="storm",
        height_m=3.0,
        horizontal_tension_n=20.0,
        submerged_weight_n_m=10.0,
        element_count=2,
        offsets=(Offset("mean", 0.0, 0.0), Offset("far", 3.0, -4.0)),
        template_sets={
            mode: {
                "mdl": "{{ analysis_type }}\n{% for n in nodes %}"
                "{{ n.id }} {{ n.x_m }} {{ n.z_m }}\n{% endfor %}",
                "inf": "H={{ horizontal_tension_n }} T={{ top_tension_n }}\n",
                "off": "{% for o in offsets %}{{ o.name }} "
                "{{ o.x_m }} {{ o.y_m }}\n{% endfor %}",
            }
            for mode in ("storm", "fatigue", "viv")
        },
        template_source="Synthetic rendering fixture; not native solver syntax",
        parameters={"diameter_m": 0.2},  # Synthetic supplied input.
    )


def test_catenary_hand_calculation(config):
    """a=2; y_top=a+h=5; s²=5²-2²=21; x=2 ln((sqrt(21)+5)/2).

    T=w*(a+h)=50 N; V=w*s=10 sqrt(21) N; H=20 N.
    These independently rearrange reference equations (1), (3), (4), (9), (10).
    """
    context = build_model_context(config)
    assert context["catenary_parameter_m"] == pytest.approx(2)
    assert context["suspended_length_m"] == pytest.approx(math.sqrt(21))
    assert context["horizontal_span_m"] == pytest.approx(
        2 * math.log((math.sqrt(21) + 5) / 2)
    )
    assert context["top_tension_n"] == pytest.approx(50)
    assert context["top_vertical_tension_n"] == pytest.approx(10 * math.sqrt(21))
    assert context["nodes"][0] == {
        "id": 1,
        "s_m": 0,
        "x_m": 0,
        "y_m": 0,
        "z_m": 0,
    }
    assert context["nodes"][-1]["z_m"] == pytest.approx(3)


def test_equal_arc_mesh_and_connectivity(config):
    """Midpoint: s=sqrt(21)/2; z=sqrt(4+21/4)-2=sqrt(37)/2-2."""
    context = build_model_context(config)
    nodes = context["nodes"]
    assert len(nodes) == 3
    assert nodes[1]["s_m"] == pytest.approx(math.sqrt(21) / 2)
    assert nodes[1]["z_m"] == pytest.approx(math.sqrt(37) / 2 - 2)
    assert context["elements"] == [
        {"id": 1, "start": 1, "end": 2, "arc_length_m": math.sqrt(21) / 2},
        {"id": 2, "start": 2, "end": 3, "arc_length_m": math.sqrt(21) / 2},
    ]
    for left, right in zip(nodes, nodes[1:]):
        chord = math.hypot(right["x_m"] - left["x_m"], right["z_m"] - left["z_m"])
        assert chord < right["s_m"] - left["s_m"]


def test_offsets_are_boundary_coordinates_not_re_equilibrated_geometry(config):
    """Translation: (x_top,0)+(3,-4)=(x_top+3,-4); magnitude=5 by 3-4-5."""
    context = build_model_context(config)
    assert context["offsets"][0]["x_m"] == context["nodes"][-1]["x_m"]
    far = context["offsets"][1]
    assert far["x_m"] == pytest.approx(context["horizontal_span_m"] + 3)
    assert far["y_m"] == -4
    assert far["z_m"] == 3
    assert far["magnitude_m"] == 5
    assert context["nodes"][-1]["y_m"] == 0


@pytest.mark.parametrize("mode", ["storm", "fatigue", "viv"])
def test_mode_specific_deterministic_bundle(config, mode):
    """The expected strings are literal template expansion, not solver outputs."""
    cfg = replace(config, analysis_type=mode)
    result = generate_flexcom_model(cfg)
    assert set(result) == {"mdl", "inf", "off"}
    assert result["mdl"].startswith(f"{mode}\n1 0.0 0.0\n")
    assert result["inf"] == "H=20.0 T=50.0\n"
    assert result == generate_flexcom_model(cfg)


def test_actual_selected_template_is_used(config):
    templates = {
        mode: dict(config.template_sets[mode], inf=f"selected {mode}")
        for mode in ("storm", "fatigue", "viv")
    }
    for mode in templates:
        cfg = replace(config, analysis_type=mode, template_sets=templates)
        assert generate_flexcom_model(cfg)["inf"] == f"selected {mode}"


def test_mode_does_not_fall_back_to_storm(config):
    cfg = replace(
        config,
        analysis_type="viv",
        template_sets={"storm": config.template_sets["storm"]},
    )
    with pytest.raises(ValueError, match="template"):
        generate_flexcom_model(cfg)


@pytest.mark.parametrize(
    "field", ["height_m", "horizontal_tension_n", "submerged_weight_n_m"]
)
@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf, True, "3"])
def test_reject_invalid_physical_inputs(config, field, value):
    with pytest.raises(ValueError, match=field):
        build_model_context(replace(config, **{field: value}))


@pytest.mark.parametrize("value", [0, -1, 1.5, True, "2"])
def test_reject_invalid_element_count(config, value):
    with pytest.raises(ValueError, match="element_count"):
        build_model_context(replace(config, element_count=value))


def test_one_element_has_two_endpoints(config):
    assert len(build_model_context(replace(config, element_count=1))["nodes"]) == 2


@pytest.mark.parametrize("mode", ["static", "Storm", "", None])
def test_reject_unknown_mode(config, mode):
    with pytest.raises(ValueError, match="analysis_type"):
        build_model_context(replace(config, analysis_type=mode))


@pytest.mark.parametrize(
    "offsets",
    [
        (),
        (Offset("same", 0, 0), Offset("same", 1, 1)),
        (Offset("bad", math.inf, 0),),
        (Offset("", 0, 0),),
        (Offset("bad\nname", 0, 0),),
    ],
)
def test_reject_invalid_offsets(config, offsets):
    with pytest.raises(ValueError, match="offset"):
        build_model_context(replace(config, offsets=offsets))


@pytest.mark.parametrize("key", ["mdl", "inf", "off"])
def test_every_file_is_required(config, key):
    templates = dict(config.template_sets["storm"])
    del templates[key]
    with pytest.raises(ValueError, match="mdl.*inf.*off"):
        generate_flexcom_model(replace(config, template_sets={"storm": templates}))


def test_undefined_template_value_fails_closed(config):
    from jinja2 import UndefinedError

    templates = dict(config.template_sets["storm"], inf="{{ missing_coefficient }}")
    with pytest.raises(UndefinedError):
        generate_flexcom_model(replace(config, template_sets={"storm": templates}))


@pytest.mark.parametrize("text", ["", "  \n", "{% if false %}unused{% endif %}"])
def test_empty_output_is_rejected(config, text):
    templates = dict(config.template_sets["storm"], inf=text)
    with pytest.raises(ValueError, match="empty"):
        generate_flexcom_model(replace(config, template_sets={"storm": templates}))


def test_template_provenance_required(config):
    with pytest.raises(ValueError, match="template_source"):
        generate_flexcom_model(replace(config, template_source=""))


def test_explicit_parameters_available_without_shadowing_geometry(config):
    templates = dict(config.template_sets["storm"], inf="{{ parameters.diameter_m }}")
    assert (
        generate_flexcom_model(replace(config, template_sets={"storm": templates}))[
            "inf"
        ]
        == "0.2"
    )


def test_missing_evidence_never_becomes_pass(config):
    status = qualification_status()
    assert status
    assert set(status.values()) == {"NOT_EVALUATED"}
    assert {
        "workbook_extraction",
        "archive_equivalence",
        "native_model_validity",
        "storm_response",
        "fatigue_damage",
        "viv_response",
        "wall_tension",
    } == set(status)
    assert build_model_context(config)["qualification"] == status


def test_derived_overflow_rejected(config):
    with pytest.raises(ValueError, match="range"):
        build_model_context(replace(config, height_m=1e308))


def test_shallow_geometry_remains_resolved(config):
    """h=1e-12, a=2; s²=h(h+4); rationalized z retains h without cancellation."""
    context = build_model_context(replace(config, height_m=1e-12))
    assert context["nodes"][-1]["z_m"] == pytest.approx(1e-12, rel=1e-12, abs=0)


@pytest.mark.parametrize(
    "parameters",
    [
        None,
        {"value": math.nan},
        {"value": math.inf},
        {"value": True},
        {"value": []},
        {1: 2},
        {"": 1},
    ],
)
def test_invalid_explicit_parameters_are_rejected(config, parameters):
    with pytest.raises(ValueError, match="parameters"):
        generate_flexcom_model(replace(config, parameters=parameters))


@pytest.mark.parametrize("template_sets", [None, [], {"storm": None}])
def test_invalid_template_containers_are_rejected(config, template_sets):
    with pytest.raises(ValueError, match="template"):
        generate_flexcom_model(replace(config, template_sets=template_sets))


def test_template_cannot_mutate_the_mesh(config):
    from jinja2.exceptions import SecurityError

    templates = dict(config.template_sets["storm"], inf="{{ nodes.clear() }}")
    with pytest.raises(SecurityError):
        generate_flexcom_model(replace(config, template_sets={"storm": templates}))


def test_unrepresentable_numeric_input_raises_validation_error(config):
    with pytest.raises(ValueError, match="height_m"):
        build_model_context(replace(config, height_m=10**400))


def test_expected_example_is_independently_checked():
    """Expected s²=21, x=2 ln((sqrt(21)+5)/2), T=50 from the hand worksheet."""
    import runpy
    from pathlib import Path

    example = (
        Path(__file__).resolve().parents[3]
        / "examples"
        / "subsea"
        / "scr_model_preparation.py"
    )
    result = runpy.run_path(str(example))["worked_example"]()
    assert result["answer"]["suspended_length_m"] == pytest.approx(math.sqrt(21))
    assert result["answer"]["horizontal_span_m"] == pytest.approx(
        2 * math.log((math.sqrt(21) + 5) / 2)
    )
    assert result["answer"]["top_tension_n"] == pytest.approx(50)
    assert result["files"]["inf"] == "PREPARATION ONLY: storm; H_eff=20.0; T_eff=50.0\n"
    assert set(result["qualification"].values()) == {"NOT_EVALUATED"}


def test_mesh_allocation_limit_is_enforced(config):
    with pytest.raises(ValueError, match="element_count"):
        build_model_context(replace(config, element_count=10_001))


def test_mesh_allocation_limit_is_inclusive(config):
    context = build_model_context(replace(config, element_count=10_000))
    assert len(context["elements"]) == 10_000
    assert len(context["nodes"]) == 10_001


@pytest.mark.parametrize("extra_dx", [0.0, -1.0])
def test_reject_hangoff_at_or_behind_touchdown(config, extra_dx):
    span = build_model_context(config)["horizontal_span_m"]
    offsets = (Offset("crossed", -span + extra_dx, 0.0),)
    with pytest.raises(ValueError, match="touchdown"):
        build_model_context(replace(config, offsets=offsets))


def test_hangoff_ahead_of_touchdown_is_allowed(config):
    span = build_model_context(config)["horizontal_span_m"]
    offsets = (Offset("near", -span / 2, 0.0),)
    context = build_model_context(replace(config, offsets=offsets))
    assert context["offsets"][0]["x_m"] == pytest.approx(span / 2)


@pytest.mark.parametrize("control", ["\r", "\n", "\x00"])
def test_reject_control_characters_in_parameter_text(config, control):
    parameters = {"value": f"first{control}injected"}
    with pytest.raises(ValueError, match="parameters.*single-line"):
        generate_flexcom_model(replace(config, parameters=parameters))


def test_single_line_parameter_text_is_preserved(config):
    templates = dict(config.template_sets["storm"], inf="{{ parameters.value }}")
    cfg = replace(
        config,
        parameters={"value": "explicit text"},
        template_sets={"storm": templates},
    )
    assert generate_flexcom_model(cfg)["inf"] == "explicit text"
