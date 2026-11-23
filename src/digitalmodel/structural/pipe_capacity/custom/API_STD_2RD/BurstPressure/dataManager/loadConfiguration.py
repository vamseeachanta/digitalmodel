"""Configuration loading for API STD 2RD burst pressure analysis."""

from dataManager.ConfigurationManager import ConfigurationManager


def loadConfiguration(file):
    configuration_manager = ConfigurationManager(file)
    configurationParameters = configuration_manager.get_configured_values()
    return configurationParameters
