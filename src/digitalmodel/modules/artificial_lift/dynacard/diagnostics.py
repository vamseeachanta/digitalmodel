import numpy as np
from .models import CardData, AnalysisResults

class PumpDiagnostics:
    """
    AI-driven troubleshooting engine for Sucker Rod Pumps.
    Classifies card patterns into standard pump failure modes.
    """
    
    FAILURE_MODES = {
        "NORMAL": "Well is operating within normal parameters.",
        "GAS_INTERFERENCE": "Compression of gas in the pump barrel detected. Consider increasing intake pressure.",
        "FLUID_POUND": "Incomplete pump fillage. Pump speed may be too high for current inflow.",
        "PUMP_TAGGING": "Mechanical contact between plunger and standing valve or top of pump.",
        "TUBING_MOVEMENT": "Unanchored tubing causing excessive stroke loss.",
        "VALVE_LEAK": "Fluid bypassing through traveling or standing valves."
    }

    @staticmethod
    def classify_card(downhole_card: CardData) -> str:
        """
        Pattern recognition for pump failure modes.
        """
        pos = np.array(downhole_card.position)
        load = np.array(downhole_card.load)
        
        # 1. Detection: Pump Tagging (Extreme loads at stroke ends)
        if np.max(load) > 38000:
            return "PUMP_TAGGING"
            
        # 2. Detection: Gas Interference vs Fluid Pound
        # We look at the downstroke (where position is decreasing)
        mid_point = len(pos) // 2
        downstroke_load = load[mid_point:]
        
        # Fluid Pound usually has a very sharp drop in load mid-stroke
        load_diff = np.diff(downstroke_load)
        if np.max(np.abs(load_diff)) > 5000:
            return "FLUID_POUND"
            
        # Gas Interference has a gradual concave decrease
        if np.min(load) < 2000:
            return "GAS_INTERFERENCE"
            
        # 3. Detection: Valve Leaks (Sloping sides)
        # Placeholder for area-based integration check
            
        return "NORMAL"

    def generate_troubleshooting_report(self, results: AnalysisResults):
        """
        Generates a natural language troubleshooting report.
        """
        mode = self.classify_card(results.downhole_card)
        results.diagnostic_message = f"Classification: {mode}. {self.FAILURE_MODES[mode]}"
        
        if results.buckling_detected:
            results.diagnostic_message += " WARNING: Mechanical buckling detected in rod string."
            
        return results.diagnostic_message
