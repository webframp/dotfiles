#!/usr/bin/env python3
"""Block shelling out to non-swamp tools to parse/slice command output.

Same spirit as no-python-pipes.py: in this repo, swamp model methods and CEL
queries (`swamp data query '<CEL>'`) return exactly the fields you need. Piping
`--json` through jq/grep/awk/head/... is the anti-pattern we're stamping out.
Ask the model or the query for the shape you want; don't post-process in the shell.
"""
import json
import re
import sys

data = json.load(sys.stdin)

if data.get("tool_name") != "Bash":
    sys.exit(0)

command = data.get("tool_input", {}).get("command", "")

CEL_HINT = (
    "Use swamp instead: `swamp data query '<CEL predicate>' --json` returns only "
    "matching resources, and a model method returns just the fields it defines. "
    "If the shape you want isn't available, EXTEND the model with a method that "
    "returns it (per the repo's Anti-Bypass rule) rather than slicing output in "
    "the shell."
)

# 1. Structured-data processors whose sole purpose is parsing output. Blocked
#    outright — there is no non-parsing use of these here.
#    Matches at a command position: start of line, or after | ; & ( or newline.
structured_parsers = [
    (r'(?:^|[|;&(\n])\s*(?:[\w./-]*/)?jq\b', 'jq (JSON processor)'),
    (r'(?:^|[|;&(\n])\s*(?:[\w./-]*/)?yq\b', 'yq (YAML/JSON processor)'),
]

for pattern, label in structured_parsers:
    if re.search(pattern, command):
        print(
            f"Blocked: {label} detected.\n"
            f"Project rule: don't parse output with {label}. {CEL_HINT}",
            file=sys.stderr,
        )
        sys.exit(2)

# 2. Piping swamp output into any text slicer / parser. The whole point of a
#    swamp method or CEL query is that it returns just what you need.
if re.search(r'\bswamp\b', command):
    pipe_targets = (
        r'jq|yq|grep|egrep|fgrep|rg|ag|sed|awk|gawk|cut|head|tail|'
        r'sort|uniq|tr|wc|column|paste|fold|fmt|xargs|fx|'
        r'perl|python3?|node|ruby'
    )
    m = re.search(rf'\|\s*(?:[\w./-]*/)?(?P<tool>{pipe_targets})\b', command)
    if m:
        tool = m.group('tool')
        print(
            f"Blocked: piping swamp output into `{tool}`.\n"
            f"Project rule: no shelling swamp output through text tools. {CEL_HINT}",
            file=sys.stderr,
        )
        sys.exit(2)

sys.exit(0)
