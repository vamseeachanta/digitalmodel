"""Win32 Job Object ownership; no breakaway and kill on final handle close.

API basis: Microsoft AssignProcessToJobObject, SetInformationJobObject,
JOBOBJECT_EXTENDED_LIMIT_INFORMATION and QueryInformationJobObject. ctypes
layouts use Windows fixed-width integers even when inspected on another OS.
"""
import ctypes as c
import os

DWORD, HANDLE, SIZE = c.c_uint32, c.c_void_p, c.c_size_t


class BasicLimit(c.Structure):
    _fields_ = [("process_time", c.c_int64), ("job_time", c.c_int64),
                ("flags", DWORD), ("min_working", SIZE), ("max_working", SIZE),
                ("active_limit", DWORD), ("affinity", SIZE),
                ("priority", DWORD), ("scheduling", DWORD)]


class IoCounters(c.Structure):
    _fields_ = [(name, c.c_uint64) for name in
                ("read_ops", "write_ops", "other_ops", "read_bytes", "write_bytes", "other_bytes")]


class ExtendedLimit(c.Structure):
    _fields_ = [("basic", BasicLimit), ("io", IoCounters),
                ("process_memory", SIZE), ("job_memory", SIZE),
                ("peak_process", SIZE), ("peak_job", SIZE)]


class Accounting(c.Structure):
    _fields_ = [(name, c.c_int64) for name in
                ("user", "kernel", "period_user", "period_kernel")] + [
                (name, DWORD) for name in ("faults", "total", "active", "terminated")]


def _kernel():
    if os.name != "nt":
        raise OSError("Windows job API requires Windows")
    api = c.WinDLL("kernel32", use_last_error=True)
    signatures = {
        "CreateJobObjectW": ([c.c_void_p, c.c_wchar_p], HANDLE),
        "SetInformationJobObject": ([HANDLE, c.c_int, c.c_void_p, DWORD], c.c_int),
        "AssignProcessToJobObject": ([HANDLE, HANDLE], c.c_int),
        "QueryInformationJobObject": ([HANDLE, c.c_int, c.c_void_p, DWORD, c.c_void_p], c.c_int),
        "TerminateJobObject": ([HANDLE, c.c_uint], c.c_int),
        "CloseHandle": ([HANDLE], c.c_int),
    }
    for name, (args, result) in signatures.items():
        function = getattr(api, name)
        function.argtypes, function.restype = args, result
    return api


class WindowsJob:
    def __init__(self, api=None):
        self.api = api if api is not None else _kernel()
        self.handle = self.api.CreateJobObjectW(None, None)
        self._check(self.handle, "CreateJobObjectW")
        limits = ExtendedLimit()
        limits.basic.flags = 0x2000  # KILL_ON_JOB_CLOSE; no BREAKAWAY flags
        try:
            self._check(self.api.SetInformationJobObject(
                self.handle, 9, c.byref(limits), c.sizeof(limits)), "SetInformationJobObject")
        except OSError:
            self.api.CloseHandle(self.handle)
            self.handle = None
            raise

    @staticmethod
    def _check(result, operation):
        if not result:
            code = c.get_last_error() if os.name == "nt" else 0
            raise OSError(code, f"{operation} failed; winerror={code}")

    def assign(self, process_handle):
        self._check(self.api.AssignProcessToJobObject(self.handle, process_handle),
                    "AssignProcessToJobObject")

    def active(self):
        data = Accounting()
        self._check(self.api.QueryInformationJobObject(
            self.handle, 1, c.byref(data), c.sizeof(data), None), "QueryInformationJobObject")
        return int(data.active)

    def terminate(self):
        self._check(self.api.TerminateJobObject(self.handle, 1), "TerminateJobObject")

    def close(self):
        if self.handle is not None:
            self._check(self.api.CloseHandle(self.handle), "CloseHandle")
            self.handle = None
