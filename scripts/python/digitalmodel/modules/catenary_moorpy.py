## NOT WORKING

from moorpy.helpers import CatenaryError, dsolve2

class CatenaryCalculator:
    def __init__(self, end1, end2, length, weight_per_unit_length, bending_stiffness):
        """
        Initialize the CatenaryCalculator with the required parameters.
        
        Parameters:
        - end1: tuple (x1, z1) -> Coordinates of the first end.
        - end2: tuple (x2, z2) -> Coordinates of the second end.
        - length: float -> Catenary length (unstretched length of the line) [m].
        - weight_per_unit_length: float -> Weight of the line per unit length [N/m].
        - bending_stiffness: float -> Bending stiffness of the line [N*m^2].
        """
        self.x1, self.z1 = end1
        self.x2, self.z2 = end2
        self.length = length
        self.weight_per_unit_length = weight_per_unit_length
        self.bending_stiffness = bending_stiffness
        self.EA = bending_stiffness  # Placeholder for extensional stiffness
        
    def calculate(self):
        """
        Calculate the tensions and other properties using the MoorPy catenary function.
        
        Returns:
        - result: dict -> Contains tensions, stiffness matrices, and other properties.
        """
        # Calculate horizontal and vertical distances
        XF = self.x2 - self.x1
        ZF = self.z2 - self.z1
        
        try:
            # Call the MoorPy catenary function
            result = dsolve2.catenary(
                XF=XF,
                ZF=ZF,
                L=self.length,
                EA=self.EA,
                W=self.weight_per_unit_length,
                CB=0,
                HF0=0,
                VF0=0,
                Tol=1e-6,
                nNodes=20,
                MaxIter=100,
                plots=0
            )
            return result
        
        except CatenaryError as e:
            print(f"Error during catenary calculation: {e.message}")
            return None

# Example usage
end1 = (0, 0)
end2 = (100, -50)
length = 120
weight_per_unit_length = 10
bending_stiffness = 1e6

catenary = CatenaryCalculator(end1, end2, length, weight_per_unit_length, bending_stiffness)
result = catenary.calculate()
if result:
    print("Catenary calculation successful:", result)
else:
    print("Catenary calculation failed")