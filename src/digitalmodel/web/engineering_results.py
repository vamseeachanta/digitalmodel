# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Render validated scalar engineering results within a Flask context."""

from flask import render_template_string

from digitalmodel.specs.engineering_result import EngineeringResult

_RESULT_TEMPLATE = """<dl class="engineering-result">
<dt>{{ result.quantity }}</dt>
<dd><span class="value">{{ result.value }}</span> <span class="units">{{ result.units }}</span></dd>
<dt>Source</dt><dd>{{ result.source }}</dd>
</dl>"""


def render_engineering_result(result: EngineeringResult) -> str:
    """Render the declared value and units without conversion or rounding.

    Flask autoescaping applies to all metadata. A Flask application context is
    required. This fragment adds no route and makes no design-acceptance claim.
    """
    return render_template_string(_RESULT_TEMPLATE, result=result)
