#!/usr/bin/env python3
"""Stop hook: bounce a final message that defers work without asking the user.

Scope reduction is the user's decision. If the last assistant message contains
deferral language, block the stop once and make the agent either do the work or
ask the user explicitly. `stop_hook_active` prevents a loop: the second attempt
always goes through, so a genuine ask-the-user message is never trapped.
"""
import json
import re
import sys

data = json.load(sys.stdin)

if data.get("stop_hook_active"):
    sys.exit(0)


def last_assistant_text(d):
    msg = d.get("last_assistant_message")
    if isinstance(msg, str) and msg:
        return msg
    path = d.get("transcript_path")
    if not path:
        return ""
    last = ""
    try:
        with open(path) as f:
            for line in f:
                try:
                    entry = json.loads(line)
                except ValueError:
                    continue
                m = entry.get("message") or {}
                if m.get("role") != "assistant":
                    continue
                content = m.get("content")
                if isinstance(content, str):
                    last = content
                elif isinstance(content, list):
                    texts = [
                        b.get("text", "")
                        for b in content
                        if isinstance(b, dict) and b.get("type") == "text"
                    ]
                    if texts:
                        last = "\n".join(texts)
    except OSError:
        return ""
    return last


text = last_assistant_text(data)
if not text:
    sys.exit(0)

# Phrases agents use to turn skipped work into a "conclusion".
PATTERNS = [
    r"\bfollow[- ]?ups?\b",
    r"\bdefer(?:red|ring)?\b",
    r"\bout of scope\b",
    r"\b(?:can|could|should) (?:be (?:done|addressed|handled|fixed)|wait|come) (?:later|in a (?:follow|separate|future))",
    r"\bin a (?:separate|future|later|subsequent) (?:pr|mr|change|commit|pass)\b",
    r"\bnot worth (?:it|the)\b",
    r"\bleav(?:e|ing) (?:this|that|it) (?:as|for)\b",
    r"\buntil the \w+ (?:can|handles?|supports?)\b",
    r"\bpre-?existing (?:issue|limitation|bug|problem)\b",
    r"\bknown limitation\b",
    r"\bexclud(?:e|ed|ing) (?:it|this|these|the)\b[^.\n]{0,60}\buntil\b",
]

hits = [p for p in PATTERNS if re.search(p, text, re.IGNORECASE)]
if not hits:
    sys.exit(0)

print(json.dumps({
    "decision": "block",
    "reason": (
        "Your final message contains deferral language (e.g. follow-up, "
        "deferred, out of scope, pre-existing, until X supports it). Scope "
        "reduction is the user's decision. For each item you deferred: if the "
        "work is in scope and feasible, do it now. If you believe it should "
        "be deferred, do not conclude that yourself: state what the full fix "
        "involves, its cost, and your recommendation, and ASK the user. If "
        "the user already approved the deferral earlier in this conversation, "
        "say so explicitly and finish."
    ),
}))
