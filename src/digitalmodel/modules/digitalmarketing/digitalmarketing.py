"""Digital Marketing router class."""

import logging


class DigitalMarketing:
    """Router for digital marketing analysis tools."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def router(self, cfg: dict) -> dict:
        """Route to appropriate digital marketing analysis.

        Args:
            cfg: Configuration dictionary with calculation settings.

        Returns:
            Updated configuration dictionary with results.

        Raises:
            ValueError: If calculation name is not implemented.
        """
        self.logger.debug("Digital marketing analysis ... BEGIN")
        calculation_name = cfg.get("calculation", {}).get("name", "")

        if calculation_name == "seo_analysis":
            from digitalmodel.modules.digitalmarketing.seo.seo_analysis import SEOAnalysis
            seo_analysis = SEOAnalysis()
            cfg = seo_analysis.run(cfg)
        else:
            raise ValueError(f"Calculation '{calculation_name}' not implemented")

        self.logger.debug("Digital marketing analysis ... END")
        return cfg
