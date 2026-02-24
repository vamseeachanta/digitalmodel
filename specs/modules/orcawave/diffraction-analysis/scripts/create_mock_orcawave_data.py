#!/usr/bin/env python
"""
Create mock OrcaWave output data for testing the processing pipeline.
This generates realistic-looking hydrodynamic data based on typical tug vessel characteristics.
"""

import numpy as np
import pandas as pd
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

def create_mock_data(output_dir: Path):
    """Generate mock OrcaWave output files."""
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Define frequency range (rad/s)
    periods = np.array([3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 25])
    frequencies = 2 * np.pi / periods
    n_freq = len(frequencies)
    
    # Define headings
    headings = np.array([0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180])
    
    # Vessel properties (typical tug)
    mass = 400000  # kg
    length = 30  # m
    beam = 10  # m
    draft = 3.5  # m
    
    logger.info("Creating mock added mass data...")
    # Create added mass matrix (6x6 for each frequency)
    added_mass_data = []
    for i, freq in enumerate(frequencies):
        row = {'Frequency': freq}
        
        # Diagonal terms (simplified models)
        row['A11'] = mass * 0.3 * (1 + 0.5 / (1 + freq))  # Surge
        row['A22'] = mass * 0.8 * (1 + 0.7 / (1 + freq))  # Sway
        row['A33'] = mass * 1.0 * (1 + 0.9 / (1 + freq))  # Heave
        row['A44'] = mass * length**2 * 0.01 * (1 + 0.3 / (1 + freq))  # Roll
        row['A55'] = mass * length**2 * 0.05 * (1 + 0.4 / (1 + freq))  # Pitch
        row['A66'] = mass * length**2 * 0.05 * (1 + 0.3 / (1 + freq))  # Yaw
        
        # Off-diagonal terms (symmetric)
        row['A12'] = row['A21'] = 0
        row['A13'] = row['A31'] = 0
        row['A14'] = row['A41'] = 0
        row['A15'] = row['A51'] = mass * length * 0.02 * np.sin(freq)
        row['A16'] = row['A61'] = 0
        row['A23'] = row['A32'] = 0
        row['A24'] = row['A42'] = mass * beam * 0.1 * np.cos(freq)
        row['A25'] = row['A52'] = 0
        row['A26'] = row['A62'] = mass * length * 0.01 * np.sin(freq)
        row['A34'] = row['A43'] = 0
        row['A35'] = row['A53'] = mass * length * 0.03 * np.cos(freq)
        row['A36'] = row['A63'] = 0
        row['A45'] = row['A54'] = 0
        row['A46'] = row['A64'] = 0
        row['A56'] = row['A65'] = 0
        
        added_mass_data.append(row)
    
    added_mass_df = pd.DataFrame(added_mass_data)
    added_mass_df.to_csv(output_dir / 'sea_cypress_added_mass.csv', index=False)
    
    logger.info("Creating mock damping data...")
    # Create damping matrix (positive semi-definite)
    damping_data = []
    for i, freq in enumerate(frequencies):
        row = {'Frequency': freq}
        
        # Diagonal terms (always positive)
        damping_factor = freq / (1 + freq**2)
        row['B11'] = mass * 0.1 * damping_factor  # Surge
        row['B22'] = mass * 0.3 * damping_factor  # Sway
        row['B33'] = mass * 0.5 * damping_factor  # Heave
        row['B44'] = mass * length**2 * 0.005 * damping_factor  # Roll
        row['B55'] = mass * length**2 * 0.01 * damping_factor  # Pitch
        row['B66'] = mass * length**2 * 0.008 * damping_factor  # Yaw
        
        # Off-diagonal terms (symmetric, smaller)
        for j in range(1, 7):
            for k in range(j+1, 7):
                value = 0.05 * np.sqrt(row[f'B{j}{j}'] * row[f'B{k}{k}']) * np.sin(freq * (k-j))
                row[f'B{j}{k}'] = row[f'B{k}{j}'] = value
        
        damping_data.append(row)
    
    damping_df = pd.DataFrame(damping_data)
    damping_df.to_csv(output_dir / 'sea_cypress_damping.csv', index=False)
    
    logger.info("Creating mock excitation force data...")
    # Create excitation forces
    excitation_data = []
    for heading in headings:
        for i, (freq, period) in enumerate(zip(frequencies, periods)):
            row = {
                'Frequency': freq,
                'Period': period,
                'Heading': heading
            }
            
            # Wave encounter angle effect
            encounter_factor = np.cos(np.deg2rad(heading))
            beam_factor = np.sin(np.deg2rad(heading))
            
            # Force amplitudes (per unit wave amplitude)
            wave_force_factor = 1000 * np.exp(-0.5 * ((freq - 0.8) / 0.3)**2)
            
            row['Fx'] = wave_force_factor * encounter_factor  # Surge force
            row['Fy'] = wave_force_factor * beam_factor * 0.8  # Sway force
            row['Fz'] = wave_force_factor * 1.2  # Heave force
            row['Mx'] = wave_force_factor * beam * beam_factor * 0.5  # Roll moment
            row['My'] = wave_force_factor * length * encounter_factor * 0.3  # Pitch moment
            row['Mz'] = wave_force_factor * length * beam_factor * 0.1  # Yaw moment
            
            # Phase angles
            row['Fx_phase'] = -freq * 10 * encounter_factor
            row['Fy_phase'] = -freq * 10 * beam_factor
            row['Fz_phase'] = -freq * 8
            row['Mx_phase'] = -freq * 12 * beam_factor
            row['My_phase'] = -freq * 11 * encounter_factor
            row['Mz_phase'] = -freq * 9 * beam_factor
            
            excitation_data.append(row)
    
    excitation_df = pd.DataFrame(excitation_data)
    excitation_df.to_csv(output_dir / 'sea_cypress_excitation.csv', index=False)
    
    logger.info("Creating mock RAO data...")
    # Create RAOs (Response Amplitude Operators)
    rao_sheets = {}
    dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
    
    for heading in headings:
        for dof_idx, dof in enumerate(dof_names):
            sheet_name = f'{dof}_{heading}deg'
            rao_data = []
            
            for i, (freq, period) in enumerate(zip(frequencies, periods)):
                # RAO amplitude calculation (simplified)
                natural_period = [12, 10, 8, 6, 9, 11][dof_idx]  # Natural periods
                resonance_factor = 1 / (1 + ((period - natural_period) / 3)**2)
                
                # Direction dependence
                if dof in ['Surge', 'Pitch']:
                    dir_factor = abs(np.cos(np.deg2rad(heading)))
                elif dof in ['Sway', 'Roll', 'Yaw']:
                    dir_factor = abs(np.sin(np.deg2rad(heading)))
                else:  # Heave
                    dir_factor = 1.0
                
                # Calculate RAO
                if dof in ['Surge', 'Sway', 'Heave']:
                    # Translational RAOs (m/m)
                    amplitude = (0.2 + 0.8 * resonance_factor) * dir_factor
                    if dof == 'Heave' and period > 15:
                        amplitude = min(amplitude, 1.0)  # Heave approaches 1.0 at low freq
                else:
                    # Rotational RAOs (deg/m)
                    amplitude = (1 + 5 * resonance_factor) * dir_factor
                
                # Phase angle
                phase = -freq * 20 * (1 + 0.5 * np.random.randn())
                
                rao_data.append({
                    'Period': period,
                    'Frequency': freq,
                    'Amplitude': amplitude,
                    'Phase': phase
                })
            
            rao_sheets[sheet_name] = pd.DataFrame(rao_data)
    
    # Save RAOs to Excel
    with pd.ExcelWriter(output_dir / 'sea_cypress_RAOs.xlsx') as writer:
        for sheet_name, df in rao_sheets.items():
            df.to_excel(writer, sheet_name=sheet_name[:31], index=False)  # Excel limit: 31 chars
    
    logger.info("Creating mock QTF data...")
    # Create simplified QTF data (mean drift only)
    qtf_data = []
    for heading in headings:
        for i, freq1 in enumerate(frequencies[:10]):  # Subset for QTF
            for j, freq2 in enumerate(frequencies[:10]):
                row = {
                    'Freq1': freq1,
                    'Freq2': freq2,
                    'Heading': heading,
                    'Fx': 100 * np.exp(-0.1 * (freq1 + freq2)) * np.cos(np.deg2rad(heading)),
                    'Fy': 80 * np.exp(-0.1 * (freq1 + freq2)) * np.sin(np.deg2rad(heading)),
                    'Fz': 50 * np.exp(-0.1 * (freq1 + freq2)),
                    'Mx': 200 * np.exp(-0.1 * (freq1 + freq2)) * np.sin(np.deg2rad(heading)),
                    'My': 150 * np.exp(-0.1 * (freq1 + freq2)) * np.cos(np.deg2rad(heading)),
                    'Mz': 100 * np.exp(-0.1 * (freq1 + freq2)) * np.sin(np.deg2rad(heading))
                }
                qtf_data.append(row)
    
    qtf_df = pd.DataFrame(qtf_data)
    qtf_df.to_csv(output_dir / 'sea_cypress_QTF_sum.csv', index=False)
    
    # Create mean drift coefficients
    mean_drift_data = []
    for heading in headings:
        for i, (freq, period) in enumerate(zip(frequencies, periods)):
            row = {
                'Frequency': freq,
                'Period': period,
                'Heading': heading,
                'Fx_drift': 50 * np.exp(-0.2 * freq) * np.cos(np.deg2rad(heading))**2,
                'Fy_drift': 40 * np.exp(-0.2 * freq) * np.sin(np.deg2rad(heading))**2,
                'Mz_drift': 30 * np.exp(-0.2 * freq) * np.sin(2 * np.deg2rad(heading))
            }
            mean_drift_data.append(row)
    
    mean_drift_df = pd.DataFrame(mean_drift_data)
    mean_drift_df.to_csv(output_dir / 'sea_cypress_mean_drift.csv', index=False)
    
    logger.info(f"Mock data created successfully in {output_dir}")
    
    return True


if __name__ == "__main__":
    import sys
    output_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path('../outputs')
    create_mock_data(output_dir)