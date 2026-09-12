"""Model-state gating for completed OrcaFlex solves.

Why this module exists
----------------------
An unstable OrcaFlex simulation does not raise. ``RunSimulation`` returns
``stOK`` and leaves the model in ``SimulationStoppedUnstable``; a caller that
handles only exceptions therefore saves a diverged run and reports success. The
state is the only signal that separates a converged run from a diverged one, so
every path that completes a solve has to read it.

Reading it correctly matters as much as reading it. ``OrcFxAPI.ModelState`` is an
``IntEnum``, and from Python 3.11 ``str()`` of an ``IntEnum`` member is its
integer value -- ``str(ModelState.SimulationStopped)`` is ``'4'``, not
``'SimulationStopped'``. A whitelist compared against ``str(model.state)``
matches nothing at all and rejects every run, converged or not.
:func:`state_name` resolves the member name instead.

Indeterminate states pass through
---------------------------------
The gate raises on a state it recognises as wrong. It stays silent on a state it
cannot read -- a test double, a mock, an object with no ``state``. The purpose is
to reject a diverged solver run, not to reject a caller whose model is a stand-in.

Issue: https://github.com/vamseeachanta/workspace-hub/issues/3838
"""

from __future__ import annotations

from typing import Any, Iterable, Optional

__all__ = [
    "KNOWN_STATES",
    "SIMULATION_COMPLETE_STATES",
    "STATICS_COMPLETE_STATES",
    "UNSTABLE_STATES",
    "SimulationStateError",
    "check_simulation",
    "check_state",
    "check_statics",
    "state_name",
]

#: Every member ``OrcFxAPI.ModelState`` defines. A state outside this set cannot
#: be judged, so it is treated as indeterminate rather than as a pass --
#: except for the ``UNDEFINED_`` members the vendor enum fabricates for values it
#: does not recognise, which are a genuine solver answer and are rejected.
KNOWN_STATES = frozenset(
    {
        "Reset",
        "CalculatingStatics",
        "InStaticState",
        "RunningSimulation",
        "SimulationStopped",
        "SimulationStoppedUnstable",
    }
)

#: The state an unstable run lands in while its call returns success.
UNSTABLE_STATES = frozenset({"SimulationStoppedUnstable"})

#: States in which a static solve has completed and its results may be read.
#: ``SimulationStopped`` is included because a completed dynamic run also
#: carries valid static results.
STATICS_COMPLETE_STATES = frozenset({"InStaticState", "SimulationStopped"})

#: The only state in which a dynamic simulation has completed normally.
SIMULATION_COMPLETE_STATES = frozenset({"SimulationStopped"})

_UNDEFINED_PREFIX = "UNDEFINED_"


class SimulationStateError(RuntimeError):
    """A solve completed in a model state its results must not be read from.

    Attributes:
        state: The resolved model-state name.
        allowed: The states the caller was prepared to accept.
        context: What was being run, for the operator reading the message.
    """

    def __init__(self, state: str, allowed: Iterable[str], context: str = ""):
        self.state = state
        self.allowed = sorted(allowed)
        self.context = context
        where = f" for {context}" if context else ""
        if state in UNSTABLE_STATES:
            detail = (
                "the simulation went unstable; OrcaFlex reports success from the "
                "run call itself, so this state is the only signal"
            )
        elif state.startswith(_UNDEFINED_PREFIX):
            detail = "the solver reported a model state this build does not define"
        else:
            detail = "the solve did not complete"
        super().__init__(
            f"OrcaFlex run{where} finished in state {state!r}: {detail}. "
            f"Accepted states: {self.allowed}."
        )


def state_name(model: Any) -> Optional[str]:
    """The model's state as a member NAME, or ``None`` if it cannot be resolved.

    ``str(state)`` is deliberately the last resort: on an ``IntEnum`` it returns
    the integer value, which is what made the pre-existing name whitelists dead
    code.
    """
    state = getattr(model, "state", None)
    if state is None:
        return None

    for attribute in ("name", "_name_"):
        value = getattr(state, attribute, None)
        if isinstance(value, str) and value:
            return value

    if isinstance(state, str):
        return state

    try:
        text = str(state)
    except Exception:  # pragma: no cover -- a stand-in with a hostile __str__
        return None
    if text in KNOWN_STATES or text.startswith(_UNDEFINED_PREFIX):
        return text
    # Anything else -- an integer, a repr, a mock -- is not a name.
    return None


def check_state(
    model: Any,
    allowed: Iterable[str],
    *,
    context: str = "",
) -> Optional[str]:
    """Raise unless the model's state is acceptable.

    Args:
        model: The model whose state to read.
        allowed: The state names that are acceptable here.
        context: What was being run, quoted back in the error message.

    Returns:
        The resolved state name, or ``None`` when the state could not be read.

    Raises:
        SimulationStateError: The state resolved to a known OrcaFlex state that
            is not in ``allowed``, or to an undefined vendor state.
    """
    name = state_name(model)
    if name is None:
        return None
    allowed = frozenset(allowed)
    if name in allowed:
        return name
    if name in KNOWN_STATES or name.startswith(_UNDEFINED_PREFIX):
        raise SimulationStateError(name, allowed, context)
    return name


def check_statics(model: Any, *, context: str = "") -> Optional[str]:
    """Raise unless a static solve completed. See :func:`check_state`."""
    return check_state(model, STATICS_COMPLETE_STATES, context=context)


def check_simulation(model: Any, *, context: str = "") -> Optional[str]:
    """Raise unless a dynamic simulation completed. See :func:`check_state`."""
    return check_state(model, SIMULATION_COMPLETE_STATES, context=context)
