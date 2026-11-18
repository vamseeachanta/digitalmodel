# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Isolated real-executor startup regression; no solver interface is loaded."""

import importlib.util
import multiprocessing
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path


spec = importlib.util.spec_from_file_location("probe_geometry", sys.argv[1])
geometry = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = geometry
spec.loader.exec_module(geometry)

started = []
parsed = []
attempts = 0


class ControlledProcess(multiprocessing.get_context(sys.argv[3]).Process):
    """Inject an error before a selected start while retaining real workers."""

    def start(self):
        global attempts
        attempts += 1
        if attempts == int(sys.argv[4]):
            (Path(sys.argv[2]) / "release").touch()
            raise OSError("synthetic partial worker startup failure")
        super().start()
        started.append(self)


class SyntheticValidator(geometry.GeometryValidator):
    """Return dispatch tokens while checking cleanup before fallback work."""

    def validate_file(self, path, kind):
        if multiprocessing.current_process().name == "MainProcess":
            assert started, "no real worker was started"
            assert all(not worker.is_alive() for worker in started), "worker leaked"
            assert all(worker.exitcode is not None for worker in started), "worker not joined"
            parsed.append(kind)
        else:
            # Prevent an idle worker from suppressing a later attempted start.
            deadline = time.monotonic() + 10
            while not (self.geometry_path / "release").exists():
                if time.monotonic() > deadline:
                    raise AssertionError("worker release timed out")
                time.sleep(0.01)
        return kind


def main():
    """Check failed startup cleanup or normal parallel completion."""
    directory = Path(sys.argv[2])
    context = multiprocessing.get_context(sys.argv[3])
    failed_start = int(sys.argv[4])
    context.Process = ControlledProcess

    def pool(**kwargs):
        if not failed_start:
            # Bound the probe's process count including start-method helpers.
            kwargs["max_workers"] = 2
        return ProcessPoolExecutor(mp_context=context, **kwargs)

    geometry.ProcessPoolExecutor = pool
    if not failed_start:
        (directory / "release").touch()
    try:
        validator = SyntheticValidator(str(directory))
        validator.results["stale"] = object()
        results = validator.validate_all()
        expected = ["stl_ascii", "stl_binary", "obj"]
        assert sorted(results.values()) == sorted(expected)
        assert "stale" not in results
        assert parsed == (expected if failed_start else [])
        if failed_start:
            assert attempts == failed_start
            assert len(started) == failed_start - 1
        assert started
        assert all(not worker.is_alive() for worker in started)
        assert all(worker.exitcode is not None for worker in started)
        assert not multiprocessing.active_children()
    finally:
        # A failing implementation must not leave the regression's workers alive.
        (directory / "release").touch()
        for worker in started:
            if worker.is_alive():
                worker.terminate()
        for worker in started:
            worker.join(timeout=5)
            if worker.is_alive():
                worker.kill()
                worker.join(timeout=5)


if __name__ == "__main__":
    main()
