"""Patch local file:// channel URLs in conda-meta to use conda-forge.

The local channel used to build r-coursekata embeds a file:// URL in the
package metadata. At runtime, xeus-r tries to contact all channels from
package metadata, which fails for local paths. This replaces the local
URL with conda-forge so the warning is suppressed.
"""

import json
from pathlib import Path

CONDA_FORGE = "https://conda.anaconda.org/conda-forge/"
CONDA_META = Path(".pixi/envs/default/conda-meta")


def main():
    for p in CONDA_META.glob("r-coursekata-*.json"):
        meta = json.loads(p.read_text())
        if meta.get("channel", "").startswith("file://"):
            meta["channel"] = CONDA_FORGE
            p.write_text(json.dumps(meta, indent=2) + "\n")
            print(f"Patched {p.name}: channel → conda-forge")


if __name__ == "__main__":
    main()
