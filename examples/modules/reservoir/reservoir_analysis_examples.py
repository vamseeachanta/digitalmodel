#!/usr/bin/env python3
"""Reservoir Analysis Examples.

This module demonstrates the usage of the digitalmodel.reservoir package
for various reservoir engineering calculations and analysis workflows.

Examples include:
- Basic reservoir property calculations
- Well log analysis and interpretation
- Material balance calculations
- Production forecasting
- Stratigraphic correlation
- Reservoir characterization workflows
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime, timedelta
from typing import Dict, List, Any

# Import reservoir modules
from digitalmodel.marine_ops.reservoir import (
    ReservoirProperties,
    RockProperties, 
    FluidProperties,
    PVTProperties,
    create_sandstone_reservoir,
    create_carbonate_reservoir,
    ReservoirModel,
    ReservoirAnalysis,
    LogAnalysis,
    StratigraphicAnalysis,
    WellData,
    ProductionData,
    LogCurve,
    StratigraphicLayer
)


def example_basic_reservoir_properties():
    """Example 1: Basic reservoir property calculations."""
    print("\n=== Example 1: Basic Reservoir Property Calculations ===")
    
    # Create rock properties
    rock = RockProperties(
        porosity=0.22,
        permeability=150,  # mD
        thickness=85,      # ft
        net_to_gross=0.8,
        compressibility=4e-6  # 1/psi
    )
    
    # Create fluid properties
    fluids = FluidProperties(
        oil_density=32,  # °API
        gas_density=0.7,  # specific gravity
        oil_viscosity=1.2,  # cp
        oil_formation_volume_factor=1.25,
        solution_gor=500  # scf/STB
    )
    
    # Create PVT properties
    pvt = PVTProperties(
        pressure=3500,     # psi
        temperature=180,   # °F
        initial_pressure=4200,
        bubble_point=3200
    )
    
    # Create reservoir properties
    reservoir = ReservoirProperties(
        rock=rock,
        fluids=fluids,
        pvt=pvt,
        area=640,  # acres (1 section)
        water_saturation=0.25
    )
    
    # Calculate reserves
    print(f"Reservoir Area: {reservoir.area:,.0f} acres")
    print(f"Net Thickness: {reservoir.rock.thickness:.1f} ft")
    print(f"Porosity: {reservoir.rock.porosity:.1%}")
    print(f"Permeability: {reservoir.rock.permeability:.0f} mD")
    print(f"Water Saturation: {reservoir.water_saturation:.1%}")
    print(f"Oil Saturation: {reservoir.oil_saturation:.1%}")
    
    # Reserve calculations
    pore_volume = reservoir.calculate_pore_volume()
    hc_pore_volume = reservoir.calculate_hydrocarbon_pore_volume()
    ooip = reservoir.calculate_ooip()
    
    print(f"\nReserve Calculations:")
    print(f"Pore Volume: {pore_volume:,.0f} acre-ft")
    print(f"Hydrocarbon Pore Volume: {hc_pore_volume:,.0f} acre-ft")
    print(f"OOIP: {ooip:,.0f} STB")
    
    return reservoir


def example_quick_reservoir_setup():
    """Example 2: Quick reservoir setup using helper functions."""
    print("\n=== Example 2: Quick Reservoir Setup ===")
    
    # Create sandstone reservoir
    sandstone = create_sandstone_reservoir(
        porosity=0.18,
        permeability=75,
        water_saturation=0.35,
        area=320,  # acres
        thickness=60,  # ft
        pressure=2800,
        temperature=160,
        oil_api=28
    )
    
    print(f"Sandstone Reservoir:")
    print(f"OOIP: {sandstone.calculate_ooip():,.0f} STB")
    
    # Create carbonate reservoir
    carbonate = create_carbonate_reservoir(
        porosity=0.15,
        permeability=25,
        water_saturation=0.20,
        area=480,  # acres
        thickness=45,  # ft
        pressure=3200,
        temperature=190,
        oil_api=35
    )
    
    print(f"\nCarbonate Reservoir:")
    print(f"OOIP: {carbonate.calculate_ooip():,.0f} STB")
    
    return sandstone, carbonate


def example_well_log_analysis():
    """Example 3: Well log analysis and interpretation."""
    print("\n=== Example 3: Well Log Analysis ===")
    
    # Create synthetic log data
    depths = np.arange(2000, 2100, 0.5)  # 2000-2100 ft, 0.5 ft sampling
    n_points = len(depths)
    
    # Generate synthetic curves with realistic values
    np.random.seed(42)  # For reproducible results
    
    # Gamma Ray: background + thin shale layers
    gr_base = 45 + 15 * np.sin(depths / 20) + 5 * np.random.normal(0, 1, n_points)
    shale_layers = ((depths > 2030) & (depths < 2035)) | ((depths > 2065) & (depths < 2070))
    gr_values = np.where(shale_layers, gr_base + 60, gr_base)
    gr_values = np.clip(gr_values, 15, 150)
    
    # Resistivity: higher in clean zones, lower in shales
    rt_base = 20 * np.exp((depths - 2000) / 100) + 5 * np.random.normal(0, 1, n_points)
    rt_values = np.where(shale_layers, rt_base * 0.3, rt_base)
    rt_values = np.clip(rt_values, 1, 200)
    
    # Density: varies with lithology and porosity
    rhob_values = 2.4 + 0.15 * np.sin(depths / 15) + 0.05 * np.random.normal(0, 1, n_points)
    rhob_values = np.where(shale_layers, rhob_values - 0.2, rhob_values)
    rhob_values = np.clip(rhob_values, 2.0, 2.8)
    
    # Neutron: higher in shales and high porosity zones
    nphi_base = 0.15 + 0.05 * np.sin(depths / 12) + 0.02 * np.random.normal(0, 1, n_points)
    nphi_values = np.where(shale_layers, nphi_base + 0.15, nphi_base)
    nphi_values = np.clip(nphi_values, 0.05, 0.45)
    
    # Create log curves
    gr_curve = LogCurve('GR', depths, gr_values, 'API')
    rt_curve = LogCurve('RT', depths, rt_values, 'ohm-m')
    rhob_curve = LogCurve('RHOB', depths, rhob_values, 'g/cm3')
    nphi_curve = LogCurve('NPHI', depths, nphi_values, 'fraction')
    
    # Create log analysis
    log_analysis = LogAnalysis('WELL-001')
    log_analysis.add_curve(gr_curve)
    log_analysis.add_curve(rt_curve)
    log_analysis.add_curve(rhob_curve)
    log_analysis.add_curve(nphi_curve)
    
    # Add formation tops
    formation1 = StratigraphicLayer('Formation A', 2000, 2040, 'sandstone')
    formation2 = StratigraphicLayer('Formation B', 2040, 2080, 'limestone')
    formation3 = StratigraphicLayer('Formation C', 2080, 2100, 'sandstone')
    
    log_analysis.add_formation(formation1)
    log_analysis.add_formation(formation2)
    log_analysis.add_formation(formation3)
    
    # Perform calculations
    try:
        # Calculate facies
        facies = log_analysis.calculate_facies_from_logs(n_clusters=4)
        print(f"Calculated {len(facies)} facies data points")
        
        # Calculate porosity
        porosity = log_analysis.calculate_porosity_from_logs()
        print(f"Average porosity: {np.mean(porosity[porosity > 0]):.1%}")
        
        # Calculate water saturation
        sw = log_analysis.calculate_water_saturation(rw=0.08)
        valid_sw = sw[sw != -999.25]
        print(f"Average water saturation: {np.mean(valid_sw):.1%}")
        
        # Calculate net pay
        net_pay = log_analysis.calculate_net_pay(porosity_cutoff=0.12, sw_cutoff=0.6)
        print(f"\nNet Pay Analysis:")
        print(f"Gross thickness: {net_pay['gross_thickness']:.1f} ft")
        print(f"Net thickness: {net_pay['net_thickness']:.1f} ft")
        print(f"Net-to-gross: {net_pay['net_to_gross']:.1%}")
        print(f"Average porosity (net): {net_pay['average_porosity']:.1%}")
        print(f"Average Sw (net): {net_pay['average_water_saturation']:.1%}")
        
    except Exception as e:
        print(f"Error in log analysis: {e}")
    
    return log_analysis


def example_material_balance():
    """Example 4: Material balance calculations."""
    print("\n=== Example 4: Material Balance Analysis ===")
    
    # Create reservoir properties
    reservoir = create_sandstone_reservoir(
        porosity=0.20,
        permeability=100,
        water_saturation=0.30,
        area=500,
        thickness=75,
        pressure=3000,
        temperature=180
    )
    
    # Create reservoir model
    model = ReservoirModel(reservoir, "Example Field")
    
    # Create synthetic production data
    start_date = datetime(2020, 1, 1)
    production_data = []
    
    for i in range(365):  # One year of production
        date = start_date + timedelta(days=i)
        
        # Declining production rate
        oil_rate = 150 * np.exp(-i / 500)  # STB/day
        gas_rate = oil_rate * 800  # Mscf/day (800 GOR)
        water_rate = oil_rate * 0.1 * (1 + i / 365)  # Increasing water cut
        
        prod_data = ProductionData(
            well_id='PRODUCER-001',
            date=date,
            oil_rate=oil_rate,
            gas_rate=gas_rate,
            water_rate=water_rate
        )
        production_data.append(prod_data)
    
    # Add production data to material balance
    model.material_balance.add_production_data(production_data)
    
    # Add pressure data
    initial_pressure = reservoir.pvt.initial_pressure
    current_pressure = 2700  # After one year
    
    model.material_balance.add_pressure_data(start_date, initial_pressure)
    model.material_balance.add_pressure_data(start_date + timedelta(days=365), current_pressure)
    
    # Perform material balance
    try:
        mb_results = model.material_balance.tank_material_balance(current_pressure)
        
        print(f"Material Balance Results:")
        print(f"Pressure drop: {mb_results['pressure_drop']:.0f} psi")
        print(f"Cumulative oil production: {mb_results['cumulative_oil']:,.0f} STB")
        print(f"Cumulative gas production: {mb_results['cumulative_gas']:,.0f} Mscf")
        
        if mb_results['ooip_estimate']:
            print(f"OOIP estimate from MB: {mb_results['ooip_estimate']:,.0f} STB")
            original_ooip = reservoir.calculate_ooip()
            print(f"Original OOIP calculation: {original_ooip:,.0f} STB")
            print(f"Recovery factor: {mb_results['cumulative_oil'] / original_ooip:.1%}")
        
    except Exception as e:
        print(f"Error in material balance: {e}")
    
    return model


def example_production_forecasting():
    """Example 5: Production forecasting using decline curves."""
    print("\n=== Example 5: Production Forecasting ===")
    
    # Create synthetic historical production data
    start_date = datetime(2020, 1, 1)
    production_data = []
    
    for i in range(730):  # Two years of history
        date = start_date + timedelta(days=i)
        
        # Hyperbolic decline
        qi = 200  # Initial rate
        D = 0.15 / 365  # Decline rate per day
        b = 0.6  # Hyperbolic exponent
        
        oil_rate = qi / (1 + b * D * i)**(1/b)
        
        prod_data = ProductionData(
            well_id='FORECAST-WELL',
            date=date,
            oil_rate=oil_rate
        )
        production_data.append(prod_data)
    
    # Create forecast model
    from digitalmodel.marine_ops.reservoir.modeling import ProductionForecast
    forecast = ProductionForecast()
    forecast.add_production_data(production_data)
    
    try:
        # Perform decline analysis
        decline_params = forecast.arps_decline_analysis(decline_type='hyperbolic')
        
        print(f"Decline Curve Analysis:")
        print(f"Initial rate: {decline_params['initial_rate']:.1f} STB/day")
        print(f"Decline rate: {decline_params['decline_rate']*365:.2f} per year")
        print(f"Hyperbolic exponent: {decline_params['hyperbolic_exponent']:.2f}")
        print(f"R-squared: {decline_params['r_squared']:.3f}")
        
        # Generate forecast
        forecast_results = forecast.forecast_production(forecast_days=3650)  # 10 years
        
        print(f"\nForecast Results:")
        print(f"EUR (10 years): {forecast_results['eur_stb']:,.0f} STB")
        print(f"Final rate: {forecast_results['oil_rate_stb_day'][-1]:.1f} STB/day")
        
        # Plot results (if matplotlib available)
        try:
            plt.figure(figsize=(12, 6))
            
            # Historical data
            hist_days = [(p.date - start_date).days for p in production_data]
            hist_rates = [p.oil_rate for p in production_data]
            
            plt.subplot(1, 2, 1)
            plt.plot(hist_days, hist_rates, 'bo-', label='Historical', markersize=2)
            plt.plot(forecast_results['time_days'], forecast_results['oil_rate_stb_day'], 'r-', label='Forecast')
            plt.xlabel('Days')
            plt.ylabel('Oil Rate (STB/day)')
            plt.title('Production Forecast')
            plt.legend()
            plt.grid(True)
            
            plt.subplot(1, 2, 2)
            plt.plot(forecast_results['time_days'], forecast_results['cumulative_oil_stb'])
            plt.xlabel('Days')
            plt.ylabel('Cumulative Oil (STB)')
            plt.title('Cumulative Production Forecast')
            plt.grid(True)
            
            plt.tight_layout()
            plt.savefig('/mnt/github/github/digitalmodel/examples/production_forecast.png', dpi=150)
            print(f"Plot saved to: examples/production_forecast.png")
            
        except Exception as e:
            print(f"Could not create plot: {e}")
        
    except Exception as e:
        print(f"Error in forecasting: {e}")
    
    return forecast


def example_stratigraphic_correlation():
    """Example 6: Multi-well stratigraphic correlation."""
    print("\n=== Example 6: Stratigraphic Correlation ===")
    
    # Create multiple wells with log data
    strat_analysis = StratigraphicAnalysis()
    
    well_ids = ['WELL-A', 'WELL-B', 'WELL-C']
    
    for i, well_id in enumerate(well_ids):
        # Create synthetic log data for each well
        base_depth = 2000 + i * 50  # Slightly different depths
        depths = np.arange(base_depth, base_depth + 100, 0.5)
        n_points = len(depths)
        
        np.random.seed(42 + i)  # Different seed for each well
        
        # Create curves with structural dip
        gr_values = 50 + 20 * np.sin(depths / 25) + 10 * np.random.normal(0, 1, n_points)
        rt_values = 15 + 10 * np.exp((depths - base_depth) / 200) + 3 * np.random.normal(0, 1, n_points)
        rhob_values = 2.3 + 0.2 * np.sin(depths / 20) + 0.05 * np.random.normal(0, 1, n_points)
        nphi_values = 0.18 + 0.08 * np.sin(depths / 15) + 0.02 * np.random.normal(0, 1, n_points)
        
        # Clip values to realistic ranges
        gr_values = np.clip(gr_values, 20, 120)
        rt_values = np.clip(rt_values, 2, 100)
        rhob_values = np.clip(rhob_values, 2.0, 2.7)
        nphi_values = np.clip(nphi_values, 0.05, 0.35)
        
        # Create log analysis
        log_analysis = LogAnalysis(well_id)
        log_analysis.add_curve(LogCurve('GR', depths, gr_values, 'API'))
        log_analysis.add_curve(LogCurve('RT', depths, rt_values, 'ohm-m'))
        log_analysis.add_curve(LogCurve('RHOB', depths, rhob_values, 'g/cm3'))
        log_analysis.add_curve(LogCurve('NPHI', depths, nphi_values, 'fraction'))
        
        # Add correlation markers (adjusted for structural dip)
        marker_top = base_depth + 20 + i * 5  # Dipping structure
        marker_base = marker_top + 30
        
        marker = StratigraphicLayer(
            name='Main_Reservoir',
            top_depth=marker_top,
            base_depth=marker_base,
            lithology='sandstone'
        )
        
        log_analysis.add_formation(marker)
        strat_analysis.add_well_analysis(well_id, log_analysis)
        strat_analysis.add_correlation_marker(well_id, marker)
    
    # Set well order for cross-section
    strat_analysis.set_well_order(well_ids)
    
    try:
        # Create flattened section
        flattened_data = strat_analysis.create_flattened_section(datum_marker='Main_Reservoir')
        
        print(f"Created stratigraphic correlation with {len(well_ids)} wells")
        print(f"Flattened to datum: Main_Reservoir")
        
        for well_id in well_ids:
            if well_id in flattened_data:
                datum_depth = flattened_data[well_id]['datum_depth']
                print(f"{well_id}: datum at {datum_depth:.1f} ft")
        
        # Plot stratigraphic section
        fig = strat_analysis.plot_stratigraphic_section(
            figsize=(15, 8),
            track_names=['GR', 'RT', 'RHOB_NPHI'],
            show_facies=True,
            flatten=True,
            datum_marker='Main_Reservoir'
        )
        
        plt.savefig('/mnt/github/github/digitalmodel/examples/stratigraphic_correlation.png', dpi=150, bbox_inches='tight')
        print(f"Stratigraphic correlation plot saved to: examples/stratigraphic_correlation.png")
        
    except Exception as e:
        print(f"Error in stratigraphic correlation: {e}")
    
    return strat_analysis


def example_integrated_reservoir_model():
    """Example 7: Integrated reservoir modeling workflow."""
    print("\n=== Example 7: Integrated Reservoir Modeling ===")
    
    # Create base reservoir properties
    reservoir = create_sandstone_reservoir(
        porosity=0.19,
        permeability=85,
        water_saturation=0.32,
        area=750,
        thickness=68,
        pressure=2950,
        temperature=175,
        oil_api=31
    )
    
    # Create integrated reservoir model
    model = ReservoirModel(reservoir, "Integrated Example Field")
    
    # Add wells
    wells_data = [
        {'id': 'PROD-001', 'x': 1000, 'y': 1000, 'type': 'producer'},
        {'id': 'PROD-002', 'x': 3000, 'y': 1000, 'type': 'producer'},
        {'id': 'PROD-003', 'x': 2000, 'y': 3000, 'type': 'producer'},
        {'id': 'INJ-001', 'x': 2000, 'y': 2000, 'type': 'injector'}
    ]
    
    for well_info in wells_data:
        well_data = WellData(
            well_id=well_info['id'],
            x_coord=well_info['x'],
            y_coord=well_info['y'],
            measured_depth=2500,
            well_type=well_info['type']
        )
        model.add_well(well_data)
    
    # Run reservoir simulation
    try:
        sim_results = model.simulation.run_depletion_simulation(
            production_rate=100,  # STB/day per well
            simulation_days=1825  # 5 years
        )
        
        print(f"Simulation Results:")
        print(f"Initial pressure: {sim_results['pressure_psi'][0]:.0f} psi")
        print(f"Final pressure: {sim_results['final_pressure']:.0f} psi")
        print(f"Total production: {sim_results['cumulative_production_stb'][-1]:,.0f} STB")
        print(f"Recovery factor: {sim_results['recovery_factor']:.1%}")
        
        # Plot simulation results
        try:
            plt.figure(figsize=(12, 8))
            
            plt.subplot(2, 2, 1)
            plt.plot(sim_results['time_days'], sim_results['pressure_psi'])
            plt.xlabel('Time (days)')
            plt.ylabel('Pressure (psi)')
            plt.title('Reservoir Pressure Decline')
            plt.grid(True)
            
            plt.subplot(2, 2, 2)
            plt.plot(sim_results['time_days'], sim_results['production_rate_stb_day'])
            plt.xlabel('Time (days)')
            plt.ylabel('Production Rate (STB/day)')
            plt.title('Production Rate')
            plt.grid(True)
            
            plt.subplot(2, 2, 3)
            plt.plot(sim_results['time_days'], sim_results['cumulative_production_stb'])
            plt.xlabel('Time (days)')
            plt.ylabel('Cumulative Production (STB)')
            plt.title('Cumulative Production')
            plt.grid(True)
            
            plt.subplot(2, 2, 4)
            recovery_factor = np.array(sim_results['cumulative_production_stb']) / reservoir.calculate_ooip()
            plt.plot(sim_results['time_days'], recovery_factor * 100)
            plt.xlabel('Time (days)')
            plt.ylabel('Recovery Factor (%)')
            plt.title('Recovery Factor')
            plt.grid(True)
            
            plt.tight_layout()
            plt.savefig('/mnt/github/github/digitalmodel/examples/reservoir_simulation.png', dpi=150)
            print(f"Simulation plot saved to: examples/reservoir_simulation.png")
            
        except Exception as e:
            print(f"Could not create simulation plot: {e}")
    
    except Exception as e:
        print(f"Error in simulation: {e}")
    
    # Run integrated analysis
    try:
        analysis_results = model.run_integrated_analysis()
        
        print(f"\nIntegrated Analysis Results:")
        print(f"Model: {analysis_results['model_name']}")
        print(f"OOIP: {analysis_results['reservoir_properties']['ooip']:,.0f} STB")
        print(f"Pore Volume: {analysis_results['reservoir_properties']['pore_volume']:,.0f} acre-ft")
        print(f"Total wells: {analysis_results['wells']['total_wells']}")
        print(f"Producers: {analysis_results['wells']['producers']}")
        print(f"Injectors: {analysis_results['wells']['injectors']}")
        
        # Export summary
        summary = model.export_summary()
        print(f"\nModel Summary:")
        print(f"Created: {summary['model_info']['created'][:10]}")
        print(f"Porosity: {summary['reservoir_properties']['porosity']:.1%}")
        print(f"Permeability: {summary['reservoir_properties']['permeability']:.0f} mD")
        
    except Exception as e:
        print(f"Error in integrated analysis: {e}")
    
    return model


def main():
    """Run all reservoir analysis examples."""
    print("Reservoir Analysis Examples")
    print("===========================\n")
    print("This script demonstrates the capabilities of the digitalmodel.reservoir package.")
    
    try:
        # Run examples
        reservoir = example_basic_reservoir_properties()
        sandstone, carbonate = example_quick_reservoir_setup()
        log_analysis = example_well_log_analysis()
        model = example_material_balance()
        forecast = example_production_forecasting()
        strat_analysis = example_stratigraphic_correlation()
        integrated_model = example_integrated_reservoir_model()
        
        print("\n=== All Examples Completed Successfully ===")
        print("\nGenerated outputs:")
        print("- examples/production_forecast.png")
        print("- examples/stratigraphic_correlation.png")
        print("- examples/reservoir_simulation.png")
        
    except Exception as e:
        print(f"\nError running examples: {e}")
        print("\nThis may be due to missing dependencies or import issues.")
        print("Make sure the digitalmodel.reservoir package is properly installed.")


if __name__ == "__main__":
    main()
