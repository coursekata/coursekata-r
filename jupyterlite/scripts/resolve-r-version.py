"""Resolve the wasm R version and write a rattler-build variant config.

Reads the r-base version that the wasm environment uses (from app/pixi.lock)
so that the host build environment can be pinned to the same version. On a
fresh build, resolves xeus-r first to account for its R and ABI constraints.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

JUPYTERLITE_DIR = Path(__file__).resolve().parent.parent
APP_LOCK_PATH = JUPYTERLITE_DIR / "app" / "pixi.lock"
VARIANT_PATH = JUPYTERLITE_DIR / ".r-version.yaml"

WASM_CHANNEL = "https://repo.prefix.dev/emscripten-forge-4x"


def version_key(v):
    """Parse a version string into a tuple of ints for comparison."""
    return tuple(int(x) for x in re.findall(r"\d+", v))


def from_lock_file(lock_path=APP_LOCK_PATH):
    """Extract the r-base version for emscripten-wasm32 from app/pixi.lock."""
    if not lock_path.exists():
        return None

    text = lock_path.read_text()

    # pixi.lock is YAML; r-base URLs look like:
    #   .../emscripten-wasm32/r-base-4.5.1-h8aa216e_0.tar.bz2
    #   .../emscripten-wasm32/r-base-4.5.1-h8aa216e_0.conda
    matches = re.findall(
        r"emscripten-wasm32/r-base-([\d.]+)-", text
    )
    if not matches:
        return None

    return max(set(matches), key=version_key)


def from_kernel_solve():
    """Resolve the kernel without requiring the not-yet-built local package."""
    print("Resolving xeus-r's R version...", file=sys.stderr)
    with tempfile.TemporaryDirectory(prefix="coursekata-kernel-") as directory:
        manifest = Path(directory) / "pixi.toml"
        manifest.write_text(
            '[workspace]\nname = "coursekata-kernel"\n'
            f'channels = ["{WASM_CHANNEL}", "conda-forge"]\n'
            'platforms = ["emscripten-wasm32"]\n'
            '[dependencies]\nxeus-r = "*"\n'
        )
        env = os.environ.copy()
        env.pop("PIXI_PROJECT_MANIFEST", None)
        subprocess.run(
            ["pixi", "lock", "--manifest-path", str(manifest)], check=True, env=env
        )
        version = from_lock_file(Path(directory) / "pixi.lock")
    if version is None:
        raise RuntimeError("xeus-r dependency resolution did not select r-base")
    return version


def write_if_changed(path, content):
    if path.exists() and path.read_text() == content:
        return False
    path.write_text(content)
    return True


def main():
    version = from_lock_file()
    source = "app/pixi.lock"

    if version is None:
        version = from_kernel_solve()
        source = "xeus-r dependency resolution"

    content = f'r_base: ["{version}"]\n'
    if write_if_changed(VARIANT_PATH, content):
        print(f"Updated .r-version.yaml (r-base {version} from {source})")
    else:
        print(f"r-base {version} (from {source}, unchanged)")


if __name__ == "__main__":
    main()
