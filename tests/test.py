import subprocess
import difflib
import sys
import glob
import pathlib

LONE_PATH = "../build/lone"

def test_cmd(cmd, out):
    process = subprocess.run(cmd, capture_output=True)
    output = process.stdout.replace(b"\r\n", b"\n")
    with open(out, "rb") as f:
        source = f.read()

    if output != source:
        diff = difflib.unified_diff(
            source.decode("utf-8").splitlines(),
            output.decode("utf-8").splitlines(),
            lineterm=""
        )

        print(f"TEST FAILED: {out}")
        print("\n".join(list(diff)))

def record_cmd(cmd, out):
    process = subprocess.run(cmd, capture_output=True)
    output = process.stdout.replace(b"\r\n", b"\n")
    with open(out, "wb") as f:
        f.write(output)

def main():
    program, *args = sys.argv
    record = False
    if args:
        subcommand, *args = args
        if subcommand == "test":
            pass
        elif subcommand == "rec":
            record = True
        else:
            print(f"Usage: {program} [test|rec] [dirs]", file=sys.stderr)

    func = record_cmd if record else test_cmd
    dirs = ["ast"]
    if args:
        dirs = args

    for d in dirs:
        for p in pathlib.Path(d).glob(f"*.ln"):
            out = p.with_suffix(".out")
            if not out.exists():
                print(f"Warning: Ignoring {p}, no output file")
                continue
            cmd = [LONE_PATH, str(p)]
            func(cmd, str(out))

if __name__ == "__main__":
    main()
