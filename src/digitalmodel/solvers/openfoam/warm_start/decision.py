"""Expected-value launch decision from section 8.3 of the warm-start design."""
from __future__ import annotations

from dataclasses import dataclass

PRIORS = {"geometry": .40, "speed": .25, "potential": .15, "analytic": .15}


@dataclass(frozen=True)
class Decision:
    hop: str
    n_cold: int
    n_abort: int
    checkpoint: int
    f_prior: float
    successes: int
    failures: int
    n_warm_est: int
    saving: int
    probability: float
    ev: int
    margin: int
    decision: str

    def block(self, target="-", source="-", level="default") -> str:
        return (
            f"warm_start plan  target={target} hop={self.hop} source={source} level={level}\n"
            f"  N_cold={self.n_cold} N_abort={self.n_abort} checkpoint={self.checkpoint} "
            f"f_prior={self.f_prior:.2f} record: ok={self.successes} fail={self.failures}\n"
            f"  N_warm_est={self.n_warm_est} S={self.saving} p={self.probability:.3f} "
            f"EV={self.ev} margin={self.margin} -> "
            f"{'WARM' if self.decision.startswith('WARM') else 'COLD'}"
        )


def decide(hop: str, n_cold: int, checkpoint: int, hops: list[dict], *,
           n_abort: int | None = None, margin_fraction: float = .10,
           calibrate: bool = False) -> Decision:
    f = PRIORS[hop]
    successes = [h for h in hops if h.get("outcome") == "WARM_OK" and
                 int(h.get("iterations", n_cold + 1)) <= .75 * n_cold]
    failures = [h for h in hops if h.get("outcome") in {"WARM_ABORTED", "WARM_FAILED_CAP"}]
    abort = n_abort if n_abort is not None else int((n_cold / 3) // checkpoint * checkpoint)
    estimate = round((2 * (1 - f) * n_cold + sum(int(h["iterations"]) for h in successes)) /
                     (2 + len(successes)))
    saving = n_cold - estimate
    probability = (2 + len(successes)) / (4 + len(successes) + len(failures))
    ev = round(probability * saving - (1 - probability) * abort)
    margin = round(margin_fraction * n_cold)
    decision = "WARM" if ev >= margin else "COLD_BY_EV"
    if calibrate:
        if any(h.get("decision") == "WARM_CALIBRATION" for h in hops):
            raise ValueError(f"calibration already used for {hop}")
        decision = "WARM_CALIBRATION"
    return Decision(hop, n_cold, abort, checkpoint, f, len(successes), len(failures),
                    estimate, saving, probability, ev, margin, decision)
