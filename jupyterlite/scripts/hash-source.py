"""Hash R package source files for pixi task caching.

TODO: pixi 0.63.2 panics on task inputs outside the project root (../R/, etc.).
If a future pixi version fixes this, replace .source-hash with direct inputs in
pixi.toml and remove this script. See: file_hashes.rs StripPrefixError.
"""

import hashlib
from pathlib import Path

JUPYTERLITE_DIR = Path(__file__).resolve().parent.parent
REPO_ROOT = JUPYTERLITE_DIR.parent
SOURCE_HASH_PATH = JUPYTERLITE_DIR / ".source-hash"

SOURCE_PATHS = [
    REPO_ROOT / "R",
    REPO_ROOT / "data",
    REPO_ROOT / "DESCRIPTION",
    REPO_ROOT / "NAMESPACE",
]


def hash_source():
    h = hashlib.sha256()
    for source_path in SOURCE_PATHS:
        if source_path.is_file():
            h.update(source_path.read_bytes())
        elif source_path.is_dir():
            for f in sorted(source_path.rglob("*")):
                if f.is_file():
                    h.update(str(f.relative_to(REPO_ROOT)).encode())
                    h.update(f.read_bytes())
    return h.hexdigest()


def main():
    source_hash = hash_source()
    if SOURCE_HASH_PATH.exists() and SOURCE_HASH_PATH.read_text().strip() == source_hash:
        return
    SOURCE_HASH_PATH.write_text(source_hash + "\n")
    print(f"Updated .source-hash ({source_hash[:12]}...)")


if __name__ == "__main__":
    main()
