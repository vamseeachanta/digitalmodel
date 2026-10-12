"""Machine environment capture and CPU-load sampling for the baseline pack.

Records what makes two timings comparable (CPU, cores, memory, OS) and nothing
that identifies the deployment: the hostname is deliberately absent; the
operator supplies a logical machine label instead.
"""

from __future__ import annotations

import os
import platform
import re
import subprocess
import sys
import time
from pathlib import Path


def _cpu_model() -> str:
    if sys.platform == "win32":
        try:
            import winreg

            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 r"HARDWARE\DESCRIPTION\System\CentralProcessor\0")
            return winreg.QueryValueEx(key, "ProcessorNameString")[0].strip()
        except OSError:
            pass
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        m = re.search(r"^model name\s*:\s*(.+)$", cpuinfo.read_text(), flags=re.M)
        if m:
            return m.group(1).strip()
    return platform.processor() or "unknown"


def _physical_cores() -> int | None:
    try:
        import psutil

        return psutil.cpu_count(logical=False)
    except Exception:
        pass
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        pairs = set(re.findall(r"physical id\s*:\s*(\d+)[\s\S]*?core id\s*:\s*(\d+)",
                               cpuinfo.read_text()))
        return len(pairs) or None
    if sys.platform == "win32":
        try:
            out = subprocess.run(
                ["powershell", "-NoProfile", "-Command",
                 "(Get-CimInstance Win32_Processor | Measure-Object NumberOfCores -Sum).Sum"],
                capture_output=True, text=True, timeout=30)
            return int(out.stdout.strip())
        except Exception:
            return None
    return None


def _ram_gb() -> float | None:
    if sys.platform == "win32":
        import ctypes

        class MemoryStatus(ctypes.Structure):
            _fields_ = [("dwLength", ctypes.c_ulong), ("dwMemoryLoad", ctypes.c_ulong),
                        ("ullTotalPhys", ctypes.c_ulonglong),
                        ("ullAvailPhys", ctypes.c_ulonglong),
                        ("ullTotalPageFile", ctypes.c_ulonglong),
                        ("ullAvailPageFile", ctypes.c_ulonglong),
                        ("ullTotalVirtual", ctypes.c_ulonglong),
                        ("ullAvailVirtual", ctypes.c_ulonglong),
                        ("ullAvailExtendedVirtual", ctypes.c_ulonglong)]

        status = MemoryStatus()
        status.dwLength = ctypes.sizeof(MemoryStatus)
        if ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(status)):
            return round(status.ullTotalPhys / 2**30, 1)
        return None
    meminfo = Path("/proc/meminfo")
    if meminfo.exists():
        m = re.search(r"^MemTotal:\s+(\d+) kB", meminfo.read_text(), flags=re.M)
        if m:
            return round(int(m.group(1)) / 2**20, 1)
    return None


def machine_environment() -> dict:
    return {
        # platform.version() can carry the kernel builder's user@host on Linux
        "os": f"{platform.system()} {platform.release()}",
        "cpu_model": _cpu_model(),
        "logical_cores": os.cpu_count() or 1,
        "physical_cores": _physical_cores(),
        "ram_gb": _ram_gb(),
        "python": platform.python_version(),
    }


# --------------------------------------------------------------- CPU load


def _proc_stat() -> list[tuple[int, int]]:
    rows = []
    for line in Path("/proc/stat").read_text().splitlines():
        if re.match(r"cpu\d+ ", line):
            values = [int(v) for v in line.split()[1:]]
            idle = values[3] + (values[4] if len(values) > 4 else 0)
            rows.append((sum(values), idle))
    return rows


def sample_cpu_load(interval: float = 2.0) -> dict | None:
    """Average and busiest-core CPU utilisation over ``interval`` seconds.

    The busiest core matters: one saturated core on a 64-core host still reads
    as ~2 % on the average, yet it distorts a single-threaded timing.
    Returns None where no sampler is available (recorded, not blocking).
    """
    try:
        import psutil

        per_core = psutil.cpu_percent(interval=interval, percpu=True)
        return {"avg_pct": round(sum(per_core) / len(per_core), 1),
                "max_core_pct": round(max(per_core), 1)}
    except Exception:
        pass
    if Path("/proc/stat").exists():
        before = _proc_stat()
        time.sleep(interval)
        after = _proc_stat()
        busy = []
        for (t0, i0), (t1, i1) in zip(before, after):
            dt = t1 - t0
            busy.append(100.0 * (dt - (i1 - i0)) / dt if dt else 0.0)
        return {"avg_pct": round(sum(busy) / len(busy), 1),
                "max_core_pct": round(max(busy), 1)}
    if sys.platform == "win32":
        samples = max(1, int(round(interval)))
        command = (
            "$s = Get-Counter '\\Processor(*)\\% Processor Time' -SampleInterval 1 "
            f"-MaxSamples {samples}; $c = $s.CounterSamples | "
            "Where-Object InstanceName -ne '_total' | Group-Object InstanceName | "
            "ForEach-Object { ($_.Group | Measure-Object CookedValue -Average).Average }; "
            "'{0} {1}' -f ($c | Measure-Object -Average).Average, "
            "($c | Measure-Object -Maximum).Maximum"
        )
        try:
            out = subprocess.run(["powershell", "-NoProfile", "-Command", command],
                                 capture_output=True, text=True, timeout=60 + samples)
            avg, top = (float(v) for v in out.stdout.split()[:2])
            return {"avg_pct": round(min(avg, 100.0), 1),
                    "max_core_pct": round(min(top, 100.0), 1)}
        except Exception:
            return None
    return None
