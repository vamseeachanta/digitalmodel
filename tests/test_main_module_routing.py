"""Tests for `python -m digitalmodel <subcommand>` routing (#713).

The module entry point must dispatch known console-script CLIs while leaving
the engine input-yml contract untouched: an existing file path as argv[1]
always goes to engine(), and unknown non-file arguments keep today's engine
behavior.
"""

from __future__ import annotations

import sys
from unittest.mock import patch

import pytest

from digitalmodel.__main__ import _resolve_cli, main


class TestResolveCli:
    def test_known_console_script_resolves(self):
        entry_point = _resolve_cli("diffraction")
        assert entry_point is not None
        assert entry_point.value == "digitalmodel.hydrodynamics.diffraction.cli:cli"

    def test_unknown_name_returns_none(self):
        assert _resolve_cli("definitely-not-a-cli") is None

    def test_own_entry_point_is_not_dispatched(self):
        assert _resolve_cli("digital_model") is None
        assert _resolve_cli("digitalmodel") is None


class TestMainRouting:
    def test_known_subcommand_dispatches(self):
        with patch.object(sys, "argv", ["digitalmodel", "diffraction", "--help"]):
            with patch("digitalmodel.__main__.engine") as mock_engine:
                with pytest.raises(SystemExit) as exc_info:
                    main()
        assert exc_info.value.code == 0
        mock_engine.assert_not_called()

    def test_existing_file_goes_to_engine(self, tmp_path):
        input_file = tmp_path / "diffraction"  # file named like a CLI still wins
        input_file.write_text("basename: test\n")
        with patch.object(sys, "argv", ["digitalmodel", str(input_file)]):
            with patch("digitalmodel.__main__.engine") as mock_engine:
                main()
        mock_engine.assert_called_once()

    def test_unknown_non_file_arg_keeps_engine_contract(self):
        with patch.object(sys, "argv", ["digitalmodel", "no-such-thing.yml"]):
            with patch("digitalmodel.__main__.engine") as mock_engine:
                main()
        mock_engine.assert_called_once()

    def test_no_args_goes_to_engine(self):
        with patch.object(sys, "argv", ["digitalmodel"]):
            with patch("digitalmodel.__main__.engine") as mock_engine:
                main()
        mock_engine.assert_called_once()
