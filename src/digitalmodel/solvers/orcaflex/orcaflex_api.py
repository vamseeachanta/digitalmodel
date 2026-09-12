"""The single import gate for the OrcaFlex Python binding.

Why this module exists
----------------------
``OrcFxAPI`` loads its DLL at import time. The path it loads is decided by
``OrcFxAPIConfig``, which reads, in order: a path previously handed to
``setLibPath()``, the ``_OrcFxAPIlib`` environment variable, and failing both the
registry key ``HKLM\\Software\\Orcina\\OrcaFlex\\Installation Directory`` -- which
names the *highest-numbered* installed OrcaFlex. An OrcaFlex upgrade therefore
changes the solver under every campaign, silently, with nothing recorded.

``setLibPath()`` only has an effect while ``OrcFxAPI`` is still unimported, and
import state is process-global. A helper function cannot fix this on its own: a
single module-scope ``import OrcFxAPI`` anywhere on the import path of the
calling program settles the question before the helper is ever reached. So this
module owns the import instead:

* it never imports ``OrcFxAPI`` at module scope;
* :func:`configure` refuses to run once ``OrcFxAPI`` is in ``sys.modules``, and
  names the module that got there first;
* :func:`api` is the only sanctioned way for the package to reach the binding;
* :func:`record` returns what was actually resolved, for the run manifest.

Modules that want the old ``import OrcFxAPI`` spelling without the module-scope
import use :func:`lazy_api`, which returns a proxy that resolves on first
attribute access.

Issue: https://github.com/vamseeachanta/workspace-hub/issues/3838
"""

from __future__ import annotations

import importlib
import importlib.util
import os
import sys
import threading
from typing import Any, Dict, Optional

__all__ = [
    "OrcaFlexApiError",
    "OrcFxAPIAlreadyImportedError",
    "OrcaFlexVersionUnavailableError",
    "api",
    "available",
    "configure",
    "installed_versions",
    "is_configured",
    "lazy_api",
    "record",
]

_MODULE = "OrcFxAPI"
_CONFIG_MODULE = "OrcFxAPIConfig"

#: Registry location that enumerates the installed OrcaFlex versions. Read under
#: the 32-bit view, matching what the vendor's own ``OrcFxAPIConfig`` does.
_REGISTRY_SUBKEY = r"Software\Orcina\OrcaFlex"
_INSTALL_DIR_KEY = "Installation Directory"
_INSTALL_DIR_VALUE = "Normal"

_lock = threading.RLock()
_record: Optional[Dict[str, Any]] = None


class OrcaFlexApiError(RuntimeError):
    """Base class for failures of the OrcaFlex import gate."""


class OrcFxAPIAlreadyImportedError(OrcaFlexApiError):
    """``configure()`` was called after ``OrcFxAPI`` had already been imported.

    ``setLibPath()`` can no longer take effect, so the version in force is
    whatever the binding picked on its own. The message names the importer that
    got there first.
    """


class OrcaFlexVersionUnavailableError(OrcaFlexApiError):
    """A specific OrcaFlex version was requested and is not installed."""


# ---------------------------------------------------------------------------
# Version discovery
# ---------------------------------------------------------------------------


def _dll_relative_path() -> str:
    is64bit = sys.maxsize > 2**32
    return os.path.join("OrcFxAPI", "Win64" if is64bit else "Win32", "OrcFxAPI.dll")


def installed_versions() -> Dict[str, str]:
    """Map each installed OrcaFlex version to its ``OrcFxAPI.dll``.

    Returns an empty mapping off Windows, or where the Orcina registry key is
    absent. Entries are reported whether or not the DLL is present on disk;
    :func:`_resolve_version` is what checks for the file.
    """
    try:
        import winreg  # noqa: PLC0415 -- Windows-only, imported on use
    except ImportError:
        return {}

    versions: Dict[str, str] = {}
    try:
        with winreg.OpenKey(
            winreg.HKEY_LOCAL_MACHINE,
            _REGISTRY_SUBKEY,
            0,
            winreg.KEY_READ | winreg.KEY_WOW64_32KEY,
        ) as root:
            index = 0
            while True:
                try:
                    name = winreg.EnumKey(root, index)
                except OSError:
                    break
                index += 1
                if name == _INSTALL_DIR_KEY:
                    # The unversioned sibling key -- this is the "highest
                    # installed version" default the defect is about, not a
                    # version in its own right.
                    continue
                try:
                    with winreg.OpenKey(
                        root,
                        f"{name}\\{_INSTALL_DIR_KEY}",
                        0,
                        winreg.KEY_READ | winreg.KEY_WOW64_32KEY,
                    ) as key:
                        directory = winreg.QueryValueEx(key, _INSTALL_DIR_VALUE)[0]
                except OSError:
                    continue
                versions[name] = os.path.join(str(directory), _dll_relative_path())
    except OSError:
        return {}
    return versions


def _resolve_version(requested: str) -> str:
    versions = installed_versions()
    candidates = {key: value for key, value in versions.items()}
    path = candidates.get(requested)
    if path is None:
        # Accept "11.6c" against an installed "11.6": the registry keys the
        # major.minor line, the DLL carries the build letter.
        prefix_matches = sorted(
            key for key in candidates if requested.startswith(key)
        )
        if len(prefix_matches) == 1:
            path = candidates[prefix_matches[0]]
    if path is None:
        raise OrcaFlexVersionUnavailableError(
            f"OrcaFlex version {requested!r} is not installed on this host. "
            f"Installed versions: {sorted(versions) or 'none found in the registry'}."
        )
    if not os.path.isfile(path):
        raise OrcaFlexVersionUnavailableError(
            f"OrcaFlex version {requested!r} is registered but its library is "
            f"missing: {path}"
        )
    return os.path.abspath(path)


# ---------------------------------------------------------------------------
# Import ordering
# ---------------------------------------------------------------------------


def _earlier_importers() -> list:
    """Name the loaded modules still holding a reference to ``OrcFxAPI``.

    This is a best-effort attribution: it reports every module that bound the
    binding as an attribute, which in practice is every module that imported it.
    A module that imported it and then deleted the name is not found, and the
    caller says so rather than claiming the list is complete.
    """
    binding = sys.modules.get(_MODULE)
    if binding is None:
        return []
    holders = []
    for name, module in list(sys.modules.items()):
        if module is None or module is binding or name == __name__:
            continue
        namespace = getattr(module, "__dict__", None)
        if not isinstance(namespace, dict):
            continue
        try:
            found = any(value is binding for value in list(namespace.values()))
        except Exception:  # pragma: no cover -- a hostile __dict__ proxy
            continue
        if found:
            holders.append(name)
    return sorted(holders)


def available() -> bool:
    """Whether ``OrcFxAPI`` can be imported, WITHOUT importing it.

    ``find_spec`` locates the module without executing it, so asking this
    question does not itself decide which library gets loaded.
    """
    if _MODULE in sys.modules:
        return True
    try:
        return importlib.util.find_spec(_MODULE) is not None
    except (ImportError, ValueError):  # pragma: no cover -- broken sys.path entry
        return False


def is_configured() -> bool:
    return _record is not None


def configure(
    requested_version: Optional[str] = None,
    *,
    lib_path: Optional[str] = None,
) -> Dict[str, Any]:
    """Select the OrcaFlex library for this process and record what was resolved.

    Args:
        requested_version: An OrcaFlex version as the Orcina registry spells it,
            e.g. ``"11.6"``. ``None`` selects nothing and records whatever the
            binding resolves on its own.
        lib_path: An explicit ``OrcFxAPI.dll`` path, overriding version lookup.

    Returns:
        The run record: ``requested``, ``resolved_lib_path``, ``resolved_version``.

    Raises:
        OrcFxAPIAlreadyImportedError: ``OrcFxAPI`` is already imported, so
            ``setLibPath()`` can no longer take effect.
        OrcaFlexVersionUnavailableError: the requested version is not installed.
    """
    global _record

    with _lock:
        if _MODULE in sys.modules:
            holders = _earlier_importers()
            attribution = (
                ", ".join(holders)
                if holders
                else "an importer that did not keep the name bound"
            )
            raise OrcFxAPIAlreadyImportedError(
                f"{_MODULE} was already imported, so the library path is already "
                f"decided and setLibPath() can no longer take effect. Imported "
                f"by: {attribution}. Call configure() before anything imports "
                f"{_MODULE}, and reach the binding through "
                f"{__name__}.api() instead of importing it directly."
            )

        selected: Optional[str] = None
        if lib_path is not None:
            selected = os.path.abspath(str(lib_path))
            if not os.path.isfile(selected):
                raise OrcaFlexVersionUnavailableError(
                    f"the requested OrcaFlex library does not exist: {selected}"
                )
        elif requested_version is not None:
            selected = _resolve_version(str(requested_version))

        if selected is not None:
            config = importlib.import_module(_CONFIG_MODULE)
            config.setLibPath(selected)

        resolved: Dict[str, Any] = {
            "requested": requested_version,
            "resolved_lib_path": selected,
            "resolved_version": None,
        }

        try:
            binding = importlib.import_module(_MODULE)
        except Exception as exc:
            # Recorded rather than raised: a manifest on a host without the
            # binding should say so, and api() re-raises for callers that need
            # the solver. The failure is never reported as a resolved version.
            resolved["import_error"] = f"{type(exc).__name__}: {exc}"
            _record = resolved
            return dict(resolved)

        resolved["resolved_lib_path"] = _current_lib_path() or selected
        resolved["resolved_version"] = _dll_version(binding)
        _record = resolved
        return dict(resolved)


def _current_lib_path() -> Optional[str]:
    try:
        config = importlib.import_module(_CONFIG_MODULE)
        return str(config.getLibPath())
    except Exception:
        return None


def _dll_version(binding) -> Optional[str]:
    """The version the loaded library reports.

    ``DLLVersion()`` reads the loaded DLL and claims no licence seat; only
    constructing a ``Model`` does.
    """
    for attribute in ("DLLVersion", "__version__"):
        value = getattr(binding, attribute, None)
        if value is None:
            continue
        try:
            return str(value() if callable(value) else value)
        except Exception:
            continue
    return None


def api():
    """Return the ``OrcFxAPI`` module, configuring the gate first if needed.

    Every access to the binding inside this package goes through here, so the
    selection made by :func:`configure` is the one in force.
    """
    with _lock:
        if _record is None:
            configure(None)
        return importlib.import_module(_MODULE)


def record() -> Dict[str, Any]:
    """The resolved solver record, for the run manifest.

    Configures with no requested version if nothing has configured yet, so a
    manifest written by a caller that never called :func:`configure` still
    carries the library path and version that were actually used.
    """
    with _lock:
        if _record is None:
            configure(None)
        return dict(_record or {})


class _LazyOrcFxAPI:
    """A stand-in for the ``OrcFxAPI`` module that resolves on first use.

    Lets a module keep writing ``OrcFxAPI.Model(...)`` at its call sites while
    the import itself is deferred past :func:`configure`. Attribute access
    delegates to :func:`api`, so the first touch is what triggers the import.
    """

    __slots__ = ()

    def __getattr__(self, name: str):
        if name.startswith("__") and name.endswith("__"):
            raise AttributeError(name)
        return getattr(api(), name)

    def __repr__(self) -> str:
        if _MODULE in sys.modules:
            return f"<lazy {_MODULE} (loaded)>"
        return f"<lazy {_MODULE} (not yet imported)>"

    def __bool__(self) -> bool:
        return available()


_LAZY = _LazyOrcFxAPI()


def lazy_api() -> _LazyOrcFxAPI:
    """The lazy ``OrcFxAPI`` stand-in; a singleton, so identity comparisons hold."""
    return _LAZY
