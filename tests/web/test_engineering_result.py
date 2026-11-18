# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Engineering values survive HTML rendering in their declared units."""

from html.parser import HTMLParser

from flask import Flask

from digitalmodel.specs.engineering_result import EngineeringResult
from digitalmodel.web.engineering_results import render_engineering_result


class ResultText(HTMLParser):
    def __init__(self):
        super().__init__()
        self.parts = []

    def handle_data(self, data):
        self.parts.append(data)


def test_rendered_pressure_equals_hand_value():
    """OpenStax UP1 (2016), 14.1, eq. 14.4: 1000*10*2.5 = 25000 Pa.

    Inputs are synthetic rho [kg/m³], g [m/s²], h [m]. The expected text
    is independently hand calculated; no rounding or unit conversion occurs.
    """
    result = EngineeringResult(
        quantity="gauge pressure",
        value=1000.0 * 10.0 * 2.5,
        units="Pa",
        source="OpenStax UP1 (2016), 14.1, eq. 14.4",
    )
    with Flask(__name__).app_context():
        rendered = render_engineering_result(result)
    parsed = ResultText()
    parsed.feed(rendered)
    assert "25000.0" in parsed.parts
    assert "Pa" in parsed.parts
    assert result.source in parsed.parts


def test_result_metadata_is_html_escaped():
    result = EngineeringResult(
        quantity="<script>bad()</script>",
        value=0.0,
        units="<b>Pa</b>",
        source="<img src=x onerror=bad()>",
    )
    with Flask(__name__).app_context():
        rendered = render_engineering_result(result)
    assert "<script>" not in rendered
    assert "<img" not in rendered
    assert "&lt;b&gt;Pa&lt;/b&gt;" in rendered
