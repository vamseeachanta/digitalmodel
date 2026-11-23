"""Common temperature derating factor calculations for pipe capacity."""

from digitalmodel.units import Q_


def temperatureDerating(data):
    _mpa_to_psi = Q_(1, 'MPa').to('psi').magnitude
    if data["Code"] == "Other":
        if data["temperature"] > 50 and data["temperature"] <= 100:
            data["S"] = (
                data["S"] - (25 - 0) / (100 - 50) * (data["temperature"] - 50) * _mpa_to_psi
            )
            data["U"] = (
                data["U"] - (25 - 0) / (100 - 50) * (data["temperature"] - 50) * _mpa_to_psi
            )
        elif data["temperature"] > 100:
            data["S"] = (
                data["S"]
                - (25 + (68 - 25) / (200 - 100) * (data["temperature"] - 100)) * _mpa_to_psi
            )
            data["U"] = (
                data["U"]
                - (25 + (68 - 25) / (200 - 100) * (data["temperature"] - 100)) * _mpa_to_psi
            )

        return data
    if data["Code"] == "ASME B31.4":
        data["TemperatureDerating"] = 1
        return data

    if data["Code"] == "ASME B31.8":
        data["TemperatureDerating"] = 1
        return data
