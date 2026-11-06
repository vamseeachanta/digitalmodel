# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Lifecycle regressions for issue #1925, using text files and handshakes.

Expected delivery is calculated directly: one submitted event produces one
callback on the loop that started the monitor. Zero stability delay disables
wall-clock waiting. The deadline below is a harness failure bound, not an
ordering mechanism; every ordering assertion uses a future, event or join.
"""

import asyncio
import threading
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.orcaflex.results_analysis import file_monitor as fm


DEADLINE_SECONDS = 5  # Harness safety budget for an event that never arrives.


async def checkpoint():
    """Yield through the ready queue without a timing-dependent sleep."""
    loop = asyncio.get_running_loop()
    ready = loop.create_future()
    loop.call_soon(ready.set_result, None)
    await ready


class ObserverStub(threading.Thread):
    """Keep observer setup deterministic; delivery uses a real thread below."""

    def __init__(self):
        super().__init__()
        self.stopped = threading.Event()

    def schedule(self, handler, directory, recursive):
        self.handler = handler

    def run(self):
        self.stopped.wait()

    def stop(self):
        self.stopped.set()


@pytest.fixture
def monitor(tmp_path, monkeypatch):
    monkeypatch.setattr(fm, "WATCHDOG_AVAILABLE", True)
    monkeypatch.setattr(fm, "Observer", ObserverStub)
    return fm.FileMonitor(
        tmp_path,
        fm.FileMonitorConfig(
            min_file_size=0,  # Empty text files are valid fixtures.
            stable_time_seconds=0,  # No wall-clock stabilization in these tests.
        ),
    )


def test_start_keeps_consumer_alive(monitor, monkeypatch):
    async def scenario():
        loop = asyncio.get_running_loop()
        delivered = loop.create_future()

        async def callback(event):
            delivered.set_result(event)

        async def initial_scan():
            await checkpoint()

        monitor.event_callback = callback
        monkeypatch.setattr(monitor, "_initialize_existing_files", initial_scan)
        try:
            assert await monitor.start()
            assert not monitor._event_processor_task.done()
            path = monitor.watch_directory / "sample.txt"
            path.write_text("sample", encoding="utf-8")
            monitor._handle_file_event(fm.EventType.CREATED, path)
            event = await asyncio.wait_for(delivered, DEADLINE_SECONDS)
            assert event.file_path == path
            assert event.event_type is fm.EventType.CREATED
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_observer_thread_event_reaches_consumer(monitor, monkeypatch):
    async def scenario():
        loop = asyncio.get_running_loop()
        delivered = loop.create_future()
        callback_threads = []

        async def callback(event):
            delivered.set_result((event, asyncio.get_running_loop(), threading.get_ident()))

        async def initial_scan():
            # Isolate observer delivery from the separate startup race.
            return None

        monitor.event_callback = callback
        monkeypatch.setattr(monitor, "_initialize_existing_files", initial_scan)
        path = monitor.watch_directory / "sample.txt"
        path.write_text("sample", encoding="utf-8")

        def notify():
            callback_threads.append(threading.get_ident())
            monitor.observer.handler.on_created(
                SimpleNamespace(is_directory=False, src_path=str(path))
            )

        try:
            assert await monitor.start()
            worker = threading.Thread(target=notify)
            worker.start()
            worker.join()
            assert not worker.is_alive()
            event, callback_loop, callback_thread = await asyncio.wait_for(
                delivered, DEADLINE_SECONDS
            )
            assert event.file_path == path
            assert callback_loop is loop
            assert monitor._loop is loop
            assert callback_thread == threading.get_ident()
            assert callback_threads == [worker.ident]
            assert worker.ident != callback_thread
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def assert_stopped(monitor, tasks=(), observer=None, executor=None):
    """Assert owned resources finished before asyncio.run can clean them up."""
    assert not monitor.is_running
    assert monitor._loop is None
    assert monitor._event_processor_task is None
    assert monitor._polling_task is None
    assert not monitor._rescan_tasks
    assert monitor.observer is None
    assert monitor.executor is None
    assert all(task.done() for task in tasks)
    if observer is not None:
        assert not observer.is_alive()
    if executor is not None:
        assert all(not thread.is_alive() for thread in executor._threads)
    assert asyncio.all_tasks() == {asyncio.current_task()}


def test_stop_and_restart_owns_tasks_and_executor(monitor):
    async def scenario():
        path = monitor.watch_directory / "sample.txt"
        path.write_text("sample", encoding="utf-8")
        delivered = asyncio.Queue()

        async def callback(event):
            await delivered.put(event)

        monitor.event_callback = callback
        try:
            assert await monitor.start()
            consumer = monitor._event_processor_task
            observer = monitor.observer
            executor = monitor.executor
            assert await monitor.start()
            assert monitor._event_processor_task is consumer
            assert monitor.observer is observer
            monitor._handle_file_event(fm.EventType.CREATED, path)
            first = await asyncio.wait_for(delivered.get(), DEADLINE_SECONDS)
            assert first.file_path == path
            assert first.checksum  # Exercise the real I/O executor.
            await monitor.stop()
            assert_stopped(monitor, (consumer,), observer, executor)
            await monitor.stop()
            assert_stopped(monitor)

            assert await monitor.start()
            assert monitor._event_processor_task is not consumer
            assert monitor.executor is not executor
            consumer = monitor._event_processor_task
            observer = monitor.observer
            executor = monitor.executor
            monitor._handle_file_event(fm.EventType.CREATED, path)
            restarted = await asyncio.wait_for(delivered.get(), DEADLINE_SECONDS)
            assert restarted.file_path == path
            assert restarted.checksum  # Restart must use a live I/O executor.
            assert delivered.empty()
            await monitor.stop()
            assert_stopped(monitor, (consumer,), observer, executor)
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


@pytest.mark.parametrize("failure_stage", ["scan", "schedule", "start", "started"])
def test_failed_start_cleans_up_and_allows_restart(monitor, monkeypatch, failure_stage):
    async def scenario():
        owned = {}
        original_scan = monitor._initialize_existing_files

        async def scan():
            owned["consumer"] = monitor._event_processor_task
            owned["executor"] = monitor.executor
            await checkpoint()
            if failure_stage == "scan":
                raise OSError("synthetic scan failure")
            await original_scan()

        class FailingObserver(ObserverStub):
            def __init__(self):
                super().__init__()
                owned["observer"] = self

            def schedule(self, *args, **kwargs):
                if failure_stage == "schedule":
                    raise OSError("synthetic schedule failure")
                super().schedule(*args, **kwargs)

            def start(self):
                if failure_stage == "start":
                    raise OSError("synthetic observer failure")
                super().start()
                raise OSError("synthetic failure after thread start")

        monkeypatch.setattr(monitor, "_initialize_existing_files", scan)
        monkeypatch.setattr(fm, "Observer", FailingObserver)
        try:
            assert not await monitor.start()
            assert_stopped(
                monitor, (owned["consumer"],), owned.get("observer"), owned["executor"]
            )
            monkeypatch.setattr(monitor, "_initialize_existing_files", original_scan)
            monkeypatch.setattr(fm, "Observer", ObserverStub)
            assert await monitor.start()
            await monitor.stop()
            assert_stopped(monitor)
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_cancelled_start_awaits_cleanup(monitor, monkeypatch):
    async def scenario():
        entered = asyncio.Event()
        blocked = asyncio.Event()

        async def scan():
            entered.set()
            await blocked.wait()

        monkeypatch.setattr(monitor, "_initialize_existing_files", scan)
        starting = asyncio.create_task(monitor.start())
        try:
            await asyncio.wait_for(entered.wait(), DEADLINE_SECONDS)
            consumer = monitor._event_processor_task
            executor = monitor.executor
            starting.cancel()
            with pytest.raises(asyncio.CancelledError):
                await starting
            assert_stopped(monitor, (consumer,), executor=executor)
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_concurrent_starts_share_one_consumer(monitor, monkeypatch):
    async def scenario():
        entered = asyncio.Event()
        release = asyncio.Event()
        original_scan = monitor._initialize_existing_files

        async def scan():
            entered.set()
            await release.wait()
            await original_scan()

        monkeypatch.setattr(monitor, "_initialize_existing_files", scan)
        first = asyncio.create_task(monitor.start())
        try:
            await asyncio.wait_for(entered.wait(), DEADLINE_SECONDS)
            consumer = monitor._event_processor_task
            second = asyncio.create_task(monitor.start())
            release.set()
            assert await asyncio.wait_for(first, DEADLINE_SECONDS)
            assert await asyncio.wait_for(second, DEADLINE_SECONDS)
            assert monitor._event_processor_task is consumer
        finally:
            release.set()
            await monitor.stop()
        assert_stopped(monitor, (consumer,))

    asyncio.run(scenario(), debug=True)


def test_stop_during_start_waits_for_initialization(monitor, monkeypatch):
    async def scenario():
        entered = asyncio.Event()
        release = asyncio.Event()

        async def scan():
            entered.set()
            await release.wait()

        monkeypatch.setattr(monitor, "_initialize_existing_files", scan)
        starting = asyncio.create_task(monitor.start())
        await asyncio.wait_for(entered.wait(), DEADLINE_SECONDS)
        stopping = asyncio.create_task(monitor.stop())
        release.set()
        assert await asyncio.wait_for(starting, DEADLINE_SECONDS)
        await asyncio.wait_for(stopping, DEADLINE_SECONDS)
        assert_stopped(monitor)

    asyncio.run(scenario(), debug=True)


def test_polling_fallback_delivers_changes_and_stops(monitor, monkeypatch):
    async def scenario():
        monkeypatch.setattr(fm, "WATCHDOG_AVAILABLE", False)
        # The real poller is paced by Events instead of a clock.
        monitor.config.enable_checksums = False
        scanned = asyncio.Queue()
        next_scan = asyncio.Event()
        delivered = asyncio.Queue()

        async def pace_scan(delay):
            if delay == monitor.config.polling_interval_seconds:
                await scanned.put(None)
                await next_scan.wait()
                next_scan.clear()
            else:
                await checkpoint()

        async def callback(event):
            await delivered.put((event.event_type, event.file_path))

        async def poll_once():
            next_scan.set()
            await asyncio.wait_for(scanned.get(), DEADLINE_SECONDS)
            return await asyncio.wait_for(delivered.get(), DEADLINE_SECONDS)

        monkeypatch.setattr(fm.asyncio, "sleep", pace_scan)
        monitor.event_callback = callback
        try:
            assert await monitor.start()
            consumer = monitor._event_processor_task
            poller = monitor._polling_task
            executor = monitor.executor
            await asyncio.wait_for(scanned.get(), DEADLINE_SECONDS)
            path = monitor.watch_directory / "sample.txt"
            path.write_text("sample", encoding="utf-8")
            assert await poll_once() == (fm.EventType.CREATED, path)
            path.write_text("longer sample", encoding="utf-8")
            assert await poll_once() == (fm.EventType.MODIFIED, path)
            path.unlink()  # Remove only this test's temporary text fixture.
            assert await poll_once() == (fm.EventType.DELETED, path)
            await monitor.stop()
            assert_stopped(monitor, (consumer, poller), executor=executor)
            assert await monitor.start()
            poller = monitor._polling_task
            await asyncio.wait_for(scanned.get(), DEADLINE_SECONDS)
            await monitor.stop()
            assert_stopped(monitor, (poller,))
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_stale_observer_delivery_is_dropped_on_restart(monitor):
    async def scenario():
        received = []

        async def callback(event):
            received.append(event)

        monitor.event_callback = callback
        path = monitor.watch_directory / "sample.txt"
        try:
            assert await monitor.start()
            # Schedule delivery, then stop before the scheduled handle runs.
            monitor._handle_file_event(fm.EventType.DELETED, path)
            await monitor.stop()
            assert await monitor.start()
            await checkpoint()
            assert monitor.event_queue.empty()
            assert received == []
        finally:
            await monitor.stop()
        monitor._handle_file_event(fm.EventType.DELETED, path)
        await checkpoint()
        assert monitor.event_queue.empty()
        assert_stopped(monitor)

    asyncio.run(scenario(), debug=True)


def test_stop_cancels_owned_rescans(monitor, monkeypatch):
    async def scenario():
        entered = asyncio.Event()
        blocked = asyncio.Event()
        try:
            assert await monitor.start()

            async def rescan():
                entered.set()
                await blocked.wait()

            monkeypatch.setattr(monitor, "_initialize_existing_files", rescan)
            monitor.force_rescan()
            await asyncio.wait_for(entered.wait(), DEADLINE_SECONDS)
            tasks = tuple(monitor._rescan_tasks)
            assert tasks
            await monitor.stop()
            assert_stopped(monitor, tasks)
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_initial_scan_error_is_a_failed_start(monitor, monkeypatch):
    async def scenario():
        def fail_scan(*args, **kwargs):
            raise OSError("synthetic filesystem failure")

        monkeypatch.setattr(type(monitor.watch_directory), "rglob", fail_scan)
        assert not await monitor.start()
        assert_stopped(monitor)

    asyncio.run(scenario(), debug=True)


def test_callback_can_stop_its_consumer(monitor):
    async def scenario():
        delivered = asyncio.get_running_loop().create_future()

        async def callback(event):
            await monitor.stop()
            delivered.set_result(event)

        monitor.event_callback = callback
        try:
            assert await monitor.start()
            consumer = monitor._event_processor_task
            monitor._handle_file_event(
                fm.EventType.DELETED, monitor.watch_directory / "sample.txt"
            )
            await asyncio.wait_for(delivered, DEADLINE_SECONDS)
            assert_stopped(monitor, (consumer,))
        finally:
            await monitor.stop()

    asyncio.run(scenario(), debug=True)


def test_closed_loop_delivery_creates_no_coroutine(monitor):
    class ClosedLoop:
        def call_soon_threadsafe(self, *args):
            raise RuntimeError("Event loop is closed")

    async def scenario():
        try:
            assert await monitor.start()
            monitor._loop = ClosedLoop()
            monitor._handle_file_event(
                fm.EventType.CREATED, monitor.watch_directory / "sample.txt"
            )
            monitor.force_rescan()
            await checkpoint()
            assert monitor.event_queue.empty()
        finally:
            await monitor.stop()
        assert_stopped(monitor)

    asyncio.run(scenario(), debug=True)


def test_failed_rescan_exception_is_retrieved(monitor, monkeypatch):
    async def scenario():
        completed = asyncio.Event()

        async def fail_scan():
            completed.set()
            raise OSError("synthetic rescan failure")

        try:
            assert await monitor.start()
            monkeypatch.setattr(monitor, "_initialize_existing_files", fail_scan)
            monitor.force_rescan()
            await asyncio.wait_for(completed.wait(), DEADLINE_SECONDS)
            await checkpoint()
            assert not monitor._rescan_tasks
        finally:
            await monitor.stop()
        assert_stopped(monitor)

    asyncio.run(scenario(), debug=True)
