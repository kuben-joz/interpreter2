import argparse
from pathlib import Path
from glob import glob
import subprocess
import sys


def main():
    path = Path(sys.argv[1])
    files_code = { Path(f).stem : f  for f in glob(str(path / "*.lat"))}
    files_in = { Path(f).stem : f  for f in glob(str(path / "*.input"))}
    files_out = { Path(f).stem : f  for f in glob(str(path / "*.out"))}
    i = 0
    for base, full in files_code.items():
        print(base)
        i += 1
        if base in files_in:
            continue
        if base not in files_out:
            true_out = []
        else:
            with open(files_out[base], 'r') as f:
                true_out = f.readlines()

        p = subprocess.run(
            ["./latc", full],
            capture_output=True,
            text=True,
        )
        my_res = p.stdout.splitlines()
        for my, true in zip(my_res, true_out):
            if my != true:
                print(f"!!!!!!!!!!!!!!!fail on {base}!!!!!!!!!!!!!!!!!!!!!")
                break
    



if __name__ == "__main__":
    main()
