#!/usr/bin/env python3
"""Block pipes / inline execution into python in Claude Code Bash calls."""
import json
import re
import sys

data = json.load(sys.stdin)

if data.get("tool_name") != "Bash":
    sys.exit(0)

command = data.get("tool_input", {}).get("command", "")

# (regex, human-readable label)
patterns = [
    (r'\bpython3?\s+-c\b',       'python -c "..." inline string'),
    (r'\|\s*python3?\b',         'pipe into python'),
    (r'\bpython3?\s*<<-?\s*\w',  'heredoc into python'),
    (r'\bpython3?\s+<\(',        'process substitution into python'),
    (r'\bpython3?\s+<\s',        'file redirect into python'),
    (r'\bpython3?\s+-\s*$',      'python reading from stdin (-)'),
]

for pattern, label in patterns:
    if re.search(pattern, command):
        print(
            f"Blocked: {label} detected.\n"
            f"Project rule: never pipe or inline-execute Python. "
            f"Write a .py file, then run `python3 path/to/file.py`.",
            file=sys.stderr,
        )
        sys.exit(2)  # exit 2 = block + show stderr to Claude

sys.exit(0)
