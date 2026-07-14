"""Descriptor-confined MPI case inspection and dictionary mutation."""

import os
import re

from digitalmodel.workflows.openfoam_batch_descriptor_io import (
    read_regular_file,
    replace_regular_file,
)
from digitalmodel.workflows.openfoam_batch_legacy_layout import DECOMPOSE_PAR_DICT

_DIR_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW


def _case_fd(work_fd: int, case: str) -> int:
    return os.open(case, _DIR_FLAGS, dir_fd=work_fd)


def has_processor_dirs(work_fd: int, case: str) -> bool:
    try:
        case_fd = _case_fd(work_fd, case)
    except FileNotFoundError:
        return False
    try:
        return any(re.fullmatch(r"processor[0-9]+", name) for name in os.listdir(case_fd))
    finally:
        os.close(case_fd)


def set_start_from_latest_time(work_fd: int, case: str) -> None:
    case_fd = _case_fd(work_fd, case)
    try:
        system_fd = os.open("system", _DIR_FLAGS, dir_fd=case_fd)
        try:
            data, _ = read_regular_file(system_fd, "controlDict")
            text = data.decode()
            patched = re.sub(r"startFrom\s+\w+\s*;", "startFrom       latestTime;", text)
            if patched == text and "startFrom" not in text:
                patched += "\nstartFrom       latestTime;\n"
            replace_regular_file(system_fd, "controlDict", patched.encode())
        finally:
            os.close(system_fd)
    finally:
        os.close(case_fd)


def write_decompose_par_dict(work_fd: int, case: str, workers: int) -> None:
    case_fd = _case_fd(work_fd, case)
    try:
        system_fd = os.open("system", _DIR_FLAGS, dir_fd=case_fd)
        try:
            data = DECOMPOSE_PAR_DICT.format(n=workers).encode()
            replace_regular_file(system_fd, "decomposeParDict", data)
        finally:
            os.close(system_fd)
    finally:
        os.close(case_fd)
