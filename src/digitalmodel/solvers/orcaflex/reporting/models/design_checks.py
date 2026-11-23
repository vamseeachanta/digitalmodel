from pydantic import BaseModel, Field, model_validator
from typing import List, Optional, Dict


class UtilizationData(BaseModel):
    """Result of a single engineering design check."""
    name: str
    value: Optional[float] = None
    allowable: Optional[float] = None
    uc: float = Field(..., description="Utilization Ratio (value / allowable)")
    pass_fail: Optional[bool] = None
    derived_pass_fail: Optional[bool] = None
    location_arc_m: Optional[float] = None
    load_case: Optional[str] = None

    @model_validator(mode='after')
    def set_pass_fail(self) -> 'UtilizationData':
        self.derived_pass_fail = (self.uc <= 1.0)
        if self.pass_fail is None:
            self.pass_fail = self.derived_pass_fail
        return self

    @property
    def has_conflict(self) -> bool:
        """Determines if the provided pass_fail contradicts the derived value."""
        if self.derived_pass_fail is None:
            return False
        return self.pass_fail != self.derived_pass_fail


class DesignCheckData(BaseModel):
    """Collection of design check results."""
    code: str = Field(..., description="Design code(s) used, e.g., API RP 2RD")
    checks: List[UtilizationData] = Field(default_factory=list)
    safety_factors: Optional[Dict[str, float]] = None
    
    @property
    def overall_pass(self) -> Optional[bool]:
        """Determines if all checks pass. Returns None if no checks are present."""
        if not self.checks:
            return None
        return all(check.pass_fail for check in self.checks)
