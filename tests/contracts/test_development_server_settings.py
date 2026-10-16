"""Exercise production launch calls without opening sockets or loading data."""

import argparse
import ast
import asyncio
import os
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
SERVERS = {
    "browser": "solvers/orcaflex/browser/orcaflex_data_server.py",
    "dashboard": "workflows/mcp_server/orcawave/client/serve_dashboard.py",
    "monitor": "workflows/mcp_server/orcawave/core/websocket_monitor.py",
}


def production_function(server, name, namespace):
    """Load the actual launch node, avoiding optional import side effects."""
    path = ROOT / "src/digitalmodel" / SERVERS[server]
    tree = ast.parse(path.read_text(encoding="utf-8"))
    nodes = [
        node for node in ast.walk(tree)
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name == name
    ]
    assert len(nodes) == 1, f"Expected one production {name} function"
    module = ast.Module(body=nodes, type_ignores=[])
    exec(compile(module, SERVERS[server], "exec"), namespace)
    return namespace[name]


def launch(server, initial_debug=False, **settings):
    """Replace network launch objects and return the arguments they received."""
    if server == "browser":
        app = SimpleNamespace(run=Mock())
        function = production_function(server, "run_server", {"app": app})
        function(**settings)
        return app.run.call_args.kwargs
    if server == "dashboard":
        runner = SimpleNamespace(setup=AsyncMock())
        site = SimpleNamespace(start=AsyncMock())
        web = SimpleNamespace(AppRunner=Mock(return_value=runner),
                              TCPSite=Mock(return_value=site))
        instance = SimpleNamespace(app=object(), port=8080,
                                   mcp_host="localhost", mcp_port=3100)
        function = production_function(server, "start_server",
                                       {"web": web, "asyncio": asyncio})

        async def invoke():
            asyncio.get_running_loop().set_debug(initial_debug)
            await function(instance, **settings)
            return asyncio.get_running_loop().get_debug()

        debug = asyncio.run(invoke())
        return {"host": web.TCPSite.call_args.args[1], "debug": debug}
    config = Mock()
    server_double = SimpleNamespace(serve=AsyncMock())
    uvicorn = SimpleNamespace(Config=config,
                             Server=Mock(return_value=server_double))
    instance = SimpleNamespace(app=object(), port=8765)
    function = production_function(server, "start_server",
                                   {"uvicorn": uvicorn, "logger": Mock()})
    asyncio.run(function(instance, **settings))
    options = config.call_args.kwargs
    return {"host": options["host"], "debug": options["log_level"] == "debug"}


@pytest.mark.parametrize("server", SERVERS)
def test_default_host_is_loopback(server):
    assert launch(server)["host"] == "127.0.0.1"


@pytest.mark.parametrize("server", SERVERS)
@pytest.mark.parametrize("host", ["192.0.2.10", "0.0.0.0"])
def test_host_requires_explicit_selection(server, host):
    assert launch(server, host=host)["host"] == host


@pytest.mark.parametrize("server", SERVERS)
def test_debug_is_off_by_default(server):
    assert launch(server)["debug"] is False


@pytest.mark.parametrize("server", SERVERS)
def test_debug_requires_explicit_selection(server):
    assert launch(server, debug=True)["debug"] is True
    assert launch(server, debug=False)["debug"] is False


@pytest.mark.parametrize("arguments,host,debug", [
    ([], "127.0.0.1", False),
    (["--host", "192.0.2.10", "--debug"], "192.0.2.10", True),
])
def test_browser_command_line_selects_host_and_debug(monkeypatch, arguments, host, debug):
    path = ROOT / "src/digitalmodel" / SERVERS["browser"]
    tree = ast.parse(path.read_text(encoding="utf-8"))
    main = next(node for node in tree.body if isinstance(node, ast.If)
                and ast.unparse(node.test) == "__name__ == '__main__'")
    run_server = Mock()
    app = SimpleNamespace(run=Mock())
    monkeypatch.setattr("sys.argv", ["server", *arguments])
    namespace = {"__name__": "__main__", "app": app, "BASE_DIR": ".",
                 "run_server": run_server}
    exec(compile(ast.Module(body=[main], type_ignores=[]),
                 SERVERS["browser"], "exec"), namespace)
    run_server.assert_called_once_with(host=host, debug=debug)
    app.run.assert_not_called()


@pytest.mark.parametrize("arguments,host,debug", [
    ([], "127.0.0.1", False),
    (["--host", "192.0.2.10", "--debug"], "192.0.2.10", True),
])
def test_dashboard_command_line_selects_host_and_debug(monkeypatch, arguments, host, debug):
    instance = SimpleNamespace(start_server=AsyncMock())
    constructor = Mock(return_value=instance)
    event = SimpleNamespace(wait=AsyncMock())
    async_api = SimpleNamespace(Event=Mock(return_value=event))
    main = production_function("dashboard", "main", {
        "argparse": argparse, "DashboardServer": constructor, "asyncio": async_api,
    })
    monkeypatch.setattr("sys.argv", ["server", *arguments])
    asyncio.run(main())
    instance.start_server.assert_awaited_once_with(host=host, debug=debug)


@pytest.mark.parametrize("settings", [{}, {"debug": False}])
def test_dashboard_preserves_existing_loop_debug(settings):
    assert launch("dashboard", initial_debug=True, **settings)["debug"] is True


@pytest.mark.parametrize("host", ["192.0.2.10", "0.0.0.0"])
def test_browser_warns_when_debug_is_exposed(host):
    with pytest.warns(RuntimeWarning, match="interactive debugger"):
        options = launch("browser", host=host, debug=True)
    assert options["host"] == host
    assert options["debug"] is True


@pytest.mark.parametrize("config,environment,expected", [
    ({}, None, "127.0.0.1"),
    ({"websocket_host": "192.0.2.10"}, None, "192.0.2.10"),
    ({}, "0.0.0.0", "0.0.0.0"),
    ({"websocket_host": "127.0.0.1"}, "0.0.0.0", "127.0.0.1"),
])
def test_integrated_monitor_binding(monkeypatch, config, environment, expected):
    path = ROOT / "src/digitalmodel/workflows/mcp_server/orcawave/core/integrated_server.py"
    tree = ast.parse(path.read_text(encoding="utf-8"))
    start = next(node for node in ast.walk(tree)
                 if isinstance(node, ast.AsyncFunctionDef) and node.name == "start")
    namespace = {"asyncio": asyncio, "os": os, "logger": Mock()}
    exec(compile(ast.Module(body=[start], type_ignores=[]), path.name, "exec"), namespace)
    monkeypatch.delenv("ORCAWAVE_WEBSOCKET_HOST", raising=False)
    if environment is not None:
        monkeypatch.setenv("ORCAWAVE_WEBSOCKET_HOST", environment)
    monitor = SimpleNamespace(start_server=AsyncMock())
    instance = SimpleNamespace(config=config, websocket_monitor=monitor, api=None)

    async def invoke():
        await namespace["start"](instance)
        await instance.websocket_task

    asyncio.run(invoke())
    monitor.start_server.assert_awaited_once_with(host=expected)


def test_container_bindings_are_explicit():
    path = ROOT / "src/digitalmodel/workflows/mcp_server/orcawave/docker-compose.yml"
    services = yaml.safe_load(path.read_text(encoding="utf-8"))["services"]
    assert "ORCAWAVE_WEBSOCKET_HOST=0.0.0.0" in services["orcawave-mcp"]["environment"]
    command = services["dashboard"]["command"]
    assert command[command.index("--host") + 1] == "0.0.0.0"
    assert any(value.endswith("serve_dashboard.py") for value in command)
