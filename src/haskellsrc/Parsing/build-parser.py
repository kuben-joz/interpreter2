import subprocess
import pathlib
from glob import glob
from pathlib import Path
import re
import os
import sys

old_cwd = Path.cwd()
os.chdir(Path(__file__).parent)
cwd = Path.cwd()
assert cwd == Path(__file__).parent
assert cwd.name == "Parsing"
assert cwd.parent.name == "src"

args = sys.argv
to_remove = (
    ["Makefile"]
    + glob("*.y")
    + glob("*.x")
    + glob("*.hs")
    + glob("*.txt")
    + glob("*.info")
    + glob("*.bak")
    + glob("*.hi")
    + glob("*.o")
)
for f in to_remove:
    if Path(f).exists():
        (cwd / f).unlink()

if len(args) > 1 and args[1] == "clean":
    exit(0)

grammar = glob("*.cf")[0]

subprocess.run(["bnfc", "--functor", "-m", grammar])

subprocess.run(["make"])

subprocess.run(["make", "clean"])

to_remove = ["ErrM.hs", "Makefile"] + glob("*.y") + glob("*.x") + glob("Test*")

for f in to_remove:
    (cwd / f).unlink()

modules = glob("*.hs")
module_names = [Path(m).stem for m in modules]

reg1 = re.compile(r"module (\w+.*\n)")
reg2 = re.compile(r"(import (?:qualified )?)(\w+)")

for m in modules:
    with open(m) as f:
        lines = f.readlines()
    with open(m, "w") as f:
        for line in lines:
            mat = reg1.match(line)
            if mat is not None:
                f.write(f"module Parsing.{mat[1]}")
            else:
                mat = reg2.match(line)
                if mat is not None and mat[2] in module_names:
                    f.write(f"{mat[1]}Parsing.{mat[2]} as {mat[2]}\n")
                else:
                    f.write(line)
