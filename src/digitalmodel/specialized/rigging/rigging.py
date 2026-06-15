import json
from pathlib import Path

from assetutilities.common.data import ReadFromExcel

from digitalmodel.specialized.rigging.rigging_components import Slings
from digitalmodel.specialized.rigging.rigging_components import Shackles

read_excel = ReadFromExcel()

slings = Slings()
shackles = Shackles()


class Rigging:
    def __init__(self):
        pass

    def get_rigging_groups(self, cfg=None):
        self.cfg = cfg
        rigging_groups = self.cfg.rigging["groups"]
        resolved_groups = []
        for rigging_group in rigging_groups:
            resolved_groups.append(self.get_rigging_group(rigging_group))
        # Store the resolved per-element vendor records back into the config
        # so the engine emits a meaningful, deterministic output (previously
        # the lookups ran and their results were discarded).
        self.cfg["rigging"]["resolved_groups"] = resolved_groups
        summary_path = self._write_summary(resolved_groups)
        if summary_path is not None:
            self.cfg["rigging"]["summary_json"] = summary_path
        # Return the cfg so the engine's `cfg_base = ...router/get_rigging_groups`
        # assignment doesn't null the config (was returning None).
        return self.cfg

    def get_rigging_group(self, rigging_group=None):
        rigging_elements = rigging_group["elements"]
        resolved_elements = []
        for rigging in rigging_elements:
            resolved = {
                "category": rigging.get("category"),
                "subcategory": rigging.get("subcategory"),
                "part_number": rigging.get("part_number"),
            }
            if rigging["category"] == "sling":
                sling, sling_model = self.get_sling(rigging)
                resolved["catalog_record"] = sling
                resolved["model"] = sling_model
            elif rigging["category"] == "shackle":
                resolved["catalog_record"] = self.get_shackle(rigging)
                resolved["model"] = {}
            resolved_elements.append(resolved)
        return {
            "label": rigging_group.get("label"),
            "elements": resolved_elements,
        }

    def get_sling(self, cfg=None):
        # ``Slings.get_sling`` returns a (sling, sling_model) tuple; surface
        # both so the resolved group carries the full vendor record plus the
        # normalised model.
        sling, sling_model = slings.get_sling(cfg)
        return sling, sling_model

    def get_shackle(self, cfg=None):
        return shackles.get_shackle(cfg)

    def _write_summary(self, resolved_groups):
        # Mirror the durable-workflow output convention: write a deterministic
        # JSON summary of the resolved vendor catalog entries to
        # ``results/rigging/`` resolved relative to the input file's directory.
        config_dir = self.cfg.get("_config_dir_path")
        if not config_dir:
            return None
        output_dir = Path(config_dir) / "results" / "rigging"
        output_dir.mkdir(parents=True, exist_ok=True)
        summary_path = output_dir / "rigging_summary.json"
        summary = {"groups": resolved_groups}
        with summary_path.open("w", encoding="utf-8") as stream:
            json.dump(summary, stream, indent=2, sort_keys=True)
            stream.write("\n")
        return str(summary_path)
