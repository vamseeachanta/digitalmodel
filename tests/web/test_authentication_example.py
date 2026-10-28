# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Re-run the documented synthetic request calculation."""

from http import HTTPStatus

from scripts.web.authentication_example import worked_example


def test_documented_example():
    """Independent oracle: equal bytes accept; adding x makes them unequal.

    Both routes define HTTPStatus.OK for success and HTTPStatus.FORBIDDEN for
    refusal. The old exception was measured against Flask-HTTPAuth 4.8.1;
    Python 3.11 hmac.compare_digest documents its ASCII-only str restriction.
    No expected value is calculated by the verifier under test.
    """
    result = worked_example()
    assert result["library"] == "Flask-HTTPAuth 4.8.1"
    for label in ("application", "template"):
        assert result["old"][label] == (
            "TypeError: comparing strings with non-ASCII characters is not supported"
        )
        assert result["new"][label] == {
            "equal": HTTPStatus.OK, "unequal": HTTPStatus.FORBIDDEN,
        }
