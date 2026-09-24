"""One closed finite synthetic observation policy; no live-runtime authority."""
from types import MappingProxyType
PROFILE=MappingProxyType({'watchdog':30,'readiness_timeout':5,'trigger_budget':10,'cleanup_timeout':2,'margin':5})
TIMEOUT_SECONDS=1
