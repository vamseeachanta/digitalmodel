"""Exercise the script entry point without loading blueprints or opening a socket.

Expected settings follow Flask 3.1 Flask.run API: explicit debug=False disables
both debugger and reloader; the documented local-only address is 127.0.0.1.
https://flask.palletsprojects.com/en/3.1.x/api/#flask.Flask.run
"""
import ast
from pathlib import Path

import pytest
from flask import Flask

APP = (Path(__file__).resolve().parents[2]
       / "src/digitalmodel/web/digitaltwinfeed/app.py")


@pytest.mark.parametrize("environment_debug", ["0", "1"])
def test_script_launch_is_local_without_debugger(environment_debug, monkeypatch):
    """Explicit settings must override both environment and app configuration."""
    monkeypatch.setenv("FLASK_DEBUG", environment_debug)
    monkeypatch.setenv("FLASK_SKIP_DOTENV", "1")
    monkeypatch.delenv("FLASK_RUN_FROM_CLI", raising=False)
    app = Flask("launch-test")
    app.config["SERVER_NAME"] = "0.0.0.0"
    launches = []
    monkeypatch.setattr(
        "werkzeug.serving.run_simple",
        lambda host, port, application, **options: launches.append(
            (host, application, options)
        ),
    )
    tree = ast.parse(APP.read_text(encoding="utf-8"))
    guard = next(node for node in tree.body if isinstance(node, ast.If)
                 and ast.unparse(node.test) == "__name__ == '__main__'")
    # Run only the shipped entry point; optional blueprint imports stay outside.
    exec(compile(ast.Module(body=[guard], type_ignores=[]), "<web-launch>", "exec"),
         {"__name__": "__main__", "app": app})
    [(host, application, options)] = launches
    assert host == "127.0.0.1"
    assert application is app
    assert app.debug is False
    assert options["use_debugger"] is False
    assert options["use_reloader"] is False
