"""Run with: python -m unittest discover -s scripts."""

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "resolve_r_version", Path(__file__).with_name("resolve-r-version.py")
)
resolver = importlib.util.module_from_spec(spec)
spec.loader.exec_module(resolver)


class ResolveRVersionTests(unittest.TestCase):
    def test_reads_only_wasm_r_version(self):
        with tempfile.TemporaryDirectory() as directory:
            lock = Path(directory) / "pixi.lock"
            self.assertIsNone(resolver.from_lock_file(lock))
            for extension in ("conda", "tar.bz2"):
                lock.write_text(
                    f"- conda: https://example.org/osx-arm64/r-base-4.6.1-build.{extension}\n"
                    f"- conda: https://example.org/emscripten-wasm32/r-base-4.5.3-build.{extension}\n"
                )
                self.assertEqual(resolver.from_lock_file(lock), "4.5.3")

    def test_fresh_build_uses_kernel_solver_without_outer_manifest(self):
        def solve(command, check, env):
            self.assertEqual(command[:3], ["pixi", "lock", "--manifest-path"])
            self.assertTrue(check)
            self.assertNotIn("PIXI_PROJECT_MANIFEST", env)
            manifest = Path(command[3])
            self.assertIn(resolver.WASM_CHANNEL, manifest.read_text())
            self.assertIn('xeus-r = "*"', manifest.read_text())
            manifest.with_suffix(".lock").write_text(
                "- conda: https://example.org/emscripten-wasm32/r-base-4.5.3-build.conda\n"
            )

        with patch.dict(os.environ, {"PIXI_PROJECT_MANIFEST": "outer/pixi.toml"}):
            with patch.object(resolver.subprocess, "run", side_effect=solve):
                self.assertEqual(resolver.from_kernel_solve(), "4.5.3")

    def test_missing_r_version_is_an_error(self):
        with patch.object(resolver.subprocess, "run"):
            with self.assertRaisesRegex(RuntimeError, "did not select r-base"):
                resolver.from_kernel_solve()


if __name__ == "__main__":
    unittest.main()
