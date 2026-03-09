"""Resolve the wasm R version and write a rattler-build variant config.

Reads the r-base version that the wasm environment uses (from app/pixi.lock)
so that the host build environment can be pinned to the same version. Falls
back to querying the emscripten-forge-dev channel if no lock file exists yet.
"""

import json
import re
import sys
import urllib.request
from pathlib import Path

JUPYTERLITE_DIR = Path(__file__).resolve().parent.parent
APP_LOCK_PATH = JUPYTERLITE_DIR / "app" / "pixi.lock"
VARIANT_PATH = JUPYTERLITE_DIR / ".r-version.yaml"

REPODATA_URL = (
    "https://repo.prefix.dev/emscripten-forge-dev"
    "/emscripten-wasm32/repodata.json"
)


def version_key(v):
    """Parse a version string into a tuple of ints for comparison."""
    return tuple(int(x) for x in re.findall(r"\d+", v))


def from_lock_file():
    """Extract the r-base version for emscripten-wasm32 from app/pixi.lock."""
    if not APP_LOCK_PATH.exists():
        return None

    text = APP_LOCK_PATH.read_text()

    # pixi.lock is YAML; r-base URLs look like:
    #   .../emscripten-wasm32/r-base-4.5.1-h8aa216e_0.tar.bz2
    #   .../emscripten-wasm32/r-base-4.5.1-h8aa216e_0.conda
    matches = re.findall(
        r"emscripten-wasm32/r-base-([\d.]+)-", text
    )
    if not matches:
        return None

    return max(set(matches), key=version_key)


def from_repodata():
    """Fetch the r-base version pinned by the latest xeus-r on emscripten-forge-dev."""
    print("Fetching r-base version from emscripten-forge-dev...", file=sys.stderr)
    req = urllib.request.Request(REPODATA_URL)
    req.add_header("User-Agent", "resolve-r-version/1.0")

    with urllib.request.urlopen(req, timeout=30) as resp:
        data = json.loads(resp.read())

    # Find the latest xeus-r and extract its r-base pin, since compiled
    # packages constrain which r-base version the solver can actually use.
    xeus_r_pkgs = []
    for section in ("packages", "packages.conda"):
        for pkg_info in data.get(section, {}).values():
            if pkg_info["name"] == "xeus-r":
                xeus_r_pkgs.append(pkg_info)

    if not xeus_r_pkgs:
        print("ERROR: No xeus-r found in emscripten-forge-dev", file=sys.stderr)
        sys.exit(1)

    latest = max(xeus_r_pkgs, key=lambda p: version_key(p["version"]))
    for dep in latest.get("depends", []):
        match = re.match(r"r-base\s*==\s*([\d.]+)", dep)
        if match:
            return match.group(1)

    print("ERROR: xeus-r has no r-base pin", file=sys.stderr)
    sys.exit(1)


def write_if_changed(path, content):
    if path.exists() and path.read_text() == content:
        return False
    path.write_text(content)
    return True


def main():
    version = from_lock_file()
    source = "app/pixi.lock"

    if version is None:
        version = from_repodata()
        source = "emscripten-forge-dev"

    content = f'r_base: ["{version}"]\n'
    if write_if_changed(VARIANT_PATH, content):
        print(f"Updated .r-version.yaml (r-base {version} from {source})")
    else:
        print(f"r-base {version} (from {source}, unchanged)")


if __name__ == "__main__":
    main()
