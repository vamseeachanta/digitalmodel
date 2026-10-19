"""Synthetic SCR hand calculation and template expansion, without solver syntax.

Run with the source package on PYTHONPATH. Output goes to stdout only.
See docs/domains/subsea/scr-model-preparation.html for assumptions and sources.
All numeric inputs are illustrative, not material properties or design limits.
"""

import json

from digitalmodel.subsea.flexcom_tools.scr_model_creator import (
    Offset,
    SCRModelConfig,
    build_model_context,
    generate_flexcom_model,
    qualification_status,
)


def _teaching_templates() -> dict[str, str]:
    return {
        "mdl": "PREPARATION ONLY: equal-arc coordinates\n"
        "{% for n in nodes %}{{ n.id }} {{ '%.1f'|format(n.s_m) }} "
        "{{ '%.1f'|format(n.x_m) }} {{ '%.1f'|format(n.z_m) }}\n{% endfor %}",
        "inf": "PREPARATION ONLY: {{ analysis_type }}; "
        "H={{ horizontal_tension_n }}; T={{ top_tension_n }}\n",
        "off": "PREPARATION ONLY: hang-off boundary coordinates\n"
        "{% for o in offsets %}{{ o.name }} {{ '%.1f'|format(o.x_m) }} "
        "{{ '%.1f'|format(o.y_m) }} {{ '%.1f'|format(o.z_m) }}\n{% endfor %}",
    }


def worked_example() -> dict:
    """Return inputs, the method, analytical answers and non-native sample text."""
    # Synthetic hand case: Catenary Curve (2019), eqs. (1), (3), (4), (9), (10).
    # h=3, H=20, w=10 gives a=2, s=sqrt(21), x=2*ln((sqrt(21)+5)/2), T=50.
    config = SCRModelConfig(
        analysis_type="storm",
        height_m=3.0,
        horizontal_tension_n=20.0,
        submerged_weight_n_m=10.0,
        element_count=2,
        offsets=(Offset("mean", 0.0, 0.0), Offset("far", 3.0, -4.0)),
        template_sets={"storm": _teaching_templates()},
        template_source="Synthetic teaching templates, revision 1; not native syntax",
        parameters={},
    )
    context = build_model_context(config)
    return {
        "inputs": {
            "height_m": config.height_m,
            "horizontal_tension_n": config.horizontal_tension_n,
            "submerged_weight_n_m": config.submerged_weight_n_m,
            "element_count": config.element_count,
        },
        "method": "Uniform inextensible catenary; equal arc mesh; Cartesian offsets; explicit templates",
        "answer": {
            key: context[key]
            for key in (
                "catenary_parameter_m",
                "suspended_length_m",
                "horizontal_span_m",
                "top_tension_n",
            )
        },
        "qualification": qualification_status(),
        "files": generate_flexcom_model(config),
    }


if __name__ == "__main__":
    # Presentation rounds numbers only; analytical answers above retain precision.
    result = worked_example()
    result["answer"] = {key: round(value, 1) for key, value in result["answer"].items()}
    print(json.dumps(result, indent=2))
