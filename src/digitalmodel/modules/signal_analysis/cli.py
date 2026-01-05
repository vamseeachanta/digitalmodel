#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for signal analysis providing rainflow counting,
FFT/spectral analysis, and signal processing utilities.
"""

import click
import json
import sys
import numpy as np
import pandas as pd
from pathlib import Path

from . import __version__, RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor


@click.group()
@click.version_option(version=__version__, prog_name="signal-analysis")
def cli():
    """Signal Analysis Tools - Rainflow, FFT, and signal processing"""
    pass


@cli.command('rainflow')
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--column', '-c', type=str, default='0', help='Column name or index')
@click.option('--method', type=click.Choice(['astm', 'rainflow']), default='astm', help='Counting method')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
@click.option('--bins', type=int, default=64, help='Number of bins for histogram')
def rainflow_cmd(input_file, column, method, output, bins):
    """Perform rainflow cycle counting on signal data"""

    try:
        # Load data
        if input_file.endswith('.csv'):
            df = pd.read_csv(input_file)
            if column.isdigit():
                signal = df.iloc[:, int(column)].values
            else:
                signal = df[column].values
        elif input_file.endswith('.txt'):
            signal = np.loadtxt(input_file)
        else:
            click.echo(f"Error: Unsupported file format. Use .csv or .txt", err=True)
            sys.exit(1)

        # Perform rainflow counting
        counter = RainflowCounter(method=method)
        cycles = counter.count_cycles(signal, extract_info=True)

        # Get statistics
        stats = counter.get_statistics(cycles)

        # Output results
        click.echo("\n=== Rainflow Cycle Counting Results ===\n")
        click.echo(f"Method:        {method.upper()}")
        click.echo(f"Total Cycles:  {stats['total_cycles']:.1f}")
        click.echo(f"Max Range:     {stats['max_range']:.2f}")
        click.echo(f"Mean Range:    {stats['mean_range']:.2f}")
        click.echo(f"Std Range:     {stats['std_range']:.2f}")

        if output:
            output_data = {
                'method': method,
                'statistics': {
                    'total_cycles': float(stats['total_cycles']),
                    'max_range': float(stats['max_range']),
                    'mean_range': float(stats['mean_range']),
                    'std_range': float(stats['std_range']),
                },
                'cycles': [
                    {
                        'range': float(c.get('range', c.get('Range', 0))),
                        'mean': float(c.get('mean', c.get('Mean', 0))),
                        'count': float(c.get('count', c.get('Count', 1))),
                    }
                    for c in cycles[:100]  # Limit to first 100 for JSON size
                ]
            }
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('fft')
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--column', '-c', type=str, default='0', help='Column name or index')
@click.option('--sampling-rate', '-fs', type=float, required=True, help='Sampling rate (Hz)')
@click.option('--method', type=click.Choice(['fft', 'welch']), default='welch', help='Analysis method')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
@click.option('--n-peaks', type=int, default=5, help='Number of peaks to identify')
def fft_cmd(input_file, column, sampling_rate, method, output, n_peaks):
    """Perform FFT/spectral analysis on signal data"""

    try:
        # Load data
        if input_file.endswith('.csv'):
            df = pd.read_csv(input_file)
            if column.isdigit():
                signal = df.iloc[:, int(column)].values
            else:
                signal = df[column].values
        elif input_file.endswith('.txt'):
            signal = np.loadtxt(input_file)
        else:
            click.echo(f"Error: Unsupported file format. Use .csv or .txt", err=True)
            sys.exit(1)

        # Perform spectral analysis
        analyzer = SpectralAnalyzer(sampling_rate=sampling_rate, method=method)
        spectrum = analyzer.compute_spectrum(signal)

        # Find peaks
        peaks = analyzer.find_peaks(spectrum, n_peaks=n_peaks)

        # Output results
        click.echo("\n=== FFT/Spectral Analysis Results ===\n")
        click.echo(f"Method:         {method.upper()}")
        click.echo(f"Sampling Rate:  {sampling_rate} Hz")
        click.echo(f"Signal Length:  {len(signal)} samples")
        click.echo(f"\nDominant Frequencies:")
        for i, peak in enumerate(peaks[:n_peaks], 1):
            click.echo(f"  {i}. {peak['frequency']:.4f} Hz (amplitude: {peak['amplitude']:.2e})")

        if output:
            output_data = {
                'method': method,
                'sampling_rate': sampling_rate,
                'signal_length': len(signal),
                'peaks': [
                    {
                        'frequency': float(p['frequency']),
                        'amplitude': float(p['amplitude']),
                    }
                    for p in peaks
                ],
                'spectrum': {
                    'frequencies': spectrum['frequencies'][:500].tolist(),
                    'amplitudes': spectrum['amplitudes'][:500].tolist(),
                }
            }
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('psd')
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--column', '-c', type=str, default='0', help='Column name or index')
@click.option('--sampling-rate', '-fs', type=float, required=True, help='Sampling rate (Hz)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def psd_cmd(input_file, column, sampling_rate, output):
    """Compute Power Spectral Density of signal"""

    try:
        # Load data
        if input_file.endswith('.csv'):
            df = pd.read_csv(input_file)
            if column.isdigit():
                signal = df.iloc[:, int(column)].values
            else:
                signal = df[column].values
        elif input_file.endswith('.txt'):
            signal = np.loadtxt(input_file)
        else:
            click.echo(f"Error: Unsupported file format", err=True)
            sys.exit(1)

        # Compute PSD using Welch method
        analyzer = SpectralAnalyzer(sampling_rate=sampling_rate, method='welch')
        psd = analyzer.compute_psd(signal)

        # Calculate statistics
        total_power = np.trapz(psd['psd'], psd['frequencies'])
        peak_freq = psd['frequencies'][np.argmax(psd['psd'])]
        peak_psd = np.max(psd['psd'])

        # Output results
        click.echo("\n=== Power Spectral Density Results ===\n")
        click.echo(f"Sampling Rate:  {sampling_rate} Hz")
        click.echo(f"Total Power:    {total_power:.2e}")
        click.echo(f"Peak Frequency: {peak_freq:.4f} Hz")
        click.echo(f"Peak PSD:       {peak_psd:.2e}")

        if output:
            output_data = {
                'sampling_rate': sampling_rate,
                'total_power': float(total_power),
                'peak_frequency': float(peak_freq),
                'peak_psd': float(peak_psd),
                'psd': {
                    'frequencies': psd['frequencies'][:500].tolist(),
                    'psd': psd['psd'][:500].tolist(),
                }
            }
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('filter')
@click.argument('input_file', type=click.Path(exists=True))
@click.argument('output_file', type=click.Path())
@click.option('--column', '-c', type=str, default='0', help='Column name or index')
@click.option('--sampling-rate', '-fs', type=float, required=True, help='Sampling rate (Hz)')
@click.option('--filter-type', type=click.Choice(['lowpass', 'highpass', 'bandpass']), default='lowpass')
@click.option('--cutoff', type=float, required=True, help='Cutoff frequency (Hz)')
@click.option('--order', type=int, default=4, help='Filter order')
def filter_cmd(input_file, output_file, column, sampling_rate, filter_type, cutoff, order):
    """Apply frequency domain filter to signal"""

    try:
        # Load data
        if input_file.endswith('.csv'):
            df = pd.read_csv(input_file)
            if column.isdigit():
                signal = df.iloc[:, int(column)].values
            else:
                signal = df[column].values
        elif input_file.endswith('.txt'):
            signal = np.loadtxt(input_file)
        else:
            click.echo(f"Error: Unsupported file format", err=True)
            sys.exit(1)

        # Apply filter
        processor = TimeSeriesProcessor(sampling_rate=sampling_rate)
        filtered = processor.filter_signal(
            signal,
            filter_type=filter_type,
            cutoff_freq=cutoff,
            order=order
        )

        # Save filtered signal
        if output_file.endswith('.csv'):
            pd.DataFrame({'filtered': filtered}).to_csv(output_file, index=False)
        else:
            np.savetxt(output_file, filtered)

        click.echo(f"\n=== Signal Filtering Complete ===\n")
        click.echo(f"Filter Type:    {filter_type}")
        click.echo(f"Cutoff Freq:    {cutoff} Hz")
        click.echo(f"Filter Order:   {order}")
        click.echo(f"Filtered signal saved to: {output_file}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point"""
    cli()


if __name__ == '__main__':
    main()
