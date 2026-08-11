// ABOUTME: Custom pi footer mirroring the claude-code-status statusline —
// ABOUTME: project/branch, model, context bar, swamp badge, and extension-status line.

import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

const DEFAULT_CONTEXT_WINDOW = 128_000;

// Context bar color/fill is pinned to this fixed budget regardless of the
// model's actual context window (which can be 200k or 1M) — staying under
// 200k tokens in a single session is the ceiling worth watching, mirroring
// claude-code-status's rationale. The label still shows the real model window.
const CTX_BUDGET = 200_000;

// Same 256-color palette as claude-code-status's `which-claude-code`-style
// session arrow, so parallel sessions (tmux panes/windows) are easy to tell
// apart at a glance.
const SESSION_PALETTE = [39, 45, 51, 82, 118, 147, 159, 171, 183, 203, 208, 214, 220, 135, 213, 48, 75, 105, 165, 198];

const SWAMP_TTL_MS = 20_000;
const SWAMP_MARKER = ".swamp.yaml";
const SWAMP_MAX_WALK = 10;

function projectLabel(cwd: string): string {
	const home = homedir();
	const collapsed = home && cwd.startsWith(home) ? `~${cwd.slice(home.length)}` : cwd;
	const parts = collapsed.split("/").filter(Boolean);
	if (parts.length <= 2) return collapsed || ".";
	return parts.slice(-2).join("/");
}

function fmtTokens(n: number): string {
	return n >= 1000 ? `${(n / 1000).toFixed(0)}k` : `${n}`;
}

// Same thresholds as claude-code-status: green <50%, yellow <80%, red >=80%.
function colorForRatio(ratio: number): "success" | "warning" | "error" {
	if (ratio >= 0.8) return "error";
	if (ratio >= 0.5) return "warning";
	return "success";
}

function bar(theme: any, ratio: number | null, width = 8): string {
	if (ratio === null) {
		return theme.fg("dim", "░".repeat(width));
	}
	const clamped = Math.max(0, Math.min(1, ratio));
	const filled = Math.round(clamped * width);
	const empty = width - filled;
	const color = colorForRatio(clamped);
	return theme.fg(color, "█".repeat(filled)) + theme.fg("dim", "░".repeat(empty));
}

// Raw ANSI 256-color escape, bypassing the theme's semantic palette — needed
// for the session-hash arrow color, which picks from an arbitrary palette
// rather than a themed color name. pi-tui's width/truncation helpers strip
// any well-formed SGR sequence, so this is safe to mix with theme.fg output.
function fg256(code: number, text: string): string {
	return `\x1b[38;5;${code}m${text}\x1b[0m`;
}

// Deterministic string hash (FNV-1a) for picking a stable session color —
// doesn't need to match claude-code-status's cksum bit-for-bit, just needs
// to be stable per session ID and spread well across the palette.
function hashString(s: string): number {
	let h = 0x811c9dc5;
	for (let i = 0; i < s.length; i++) {
		h ^= s.charCodeAt(i);
		h = Math.imul(h, 0x01000193);
	}
	return h >>> 0;
}

function sessionColorCode(sessionId: string): number {
	const idx = hashString(sessionId) % SESSION_PALETTE.length;
	return SESSION_PALETTE[idx];
}

function sanitizeStatusText(text: string): string {
	return text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
}

function findSwampRoot(cwd: string): string | null {
	let dir = cwd;
	for (let i = 0; i < SWAMP_MAX_WALK; i++) {
		if (existsSync(join(dir, SWAMP_MARKER))) return dir;
		const parent = dirname(dir);
		if (parent === dir) break;
		dir = parent;
	}
	return null;
}

interface SwampRun {
	workflowName?: string;
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		const sessionId = ctx.sessionManager.getSessionId();
		const sessionColor = sessionColorCode(sessionId);
		const swampRoot = findSwampRoot(ctx.cwd);

		// Detached HEAD: footerData.getGitBranch() only reports the literal
		// "detached", but claude-code-status shows the short SHA instead.
		// Resolving it needs a subprocess, so it's cached and refreshed only
		// on branch change — never synchronously inside render().
		let branchLabel: string | null = null;

		function resolveBranchLabel(rawBranch: string | null, tui: { requestRender(): void }) {
			if (rawBranch !== "detached") {
				branchLabel = rawBranch;
				return;
			}
			pi.exec("git", ["rev-parse", "--short", "HEAD"], { cwd: ctx.cwd, timeout: 2000 })
				.then((res) => {
					branchLabel = res.code === 0 ? res.stdout.trim() || "detached" : "detached";
					tui.requestRender();
				})
				.catch(() => {
					branchLabel = "detached";
					tui.requestRender();
				});
		}

		// Active swamp workflow summary, cached per swamp root with a short TTL
		// and refreshed in the background — `swamp run history --active` takes
		// ~1s, far too slow to shell out to on every render.
		let swampActiveSummary = "";
		let swampLastFetch = 0;
		let swampFetchInFlight = false;

		function maybeRefreshSwampActive(tui: { requestRender(): void }) {
			if (!swampRoot || swampFetchInFlight) return;
			if (Date.now() - swampLastFetch < SWAMP_TTL_MS) return;
			swampFetchInFlight = true;
			pi.exec("swamp", ["run", "history", "--active", "--json"], { cwd: swampRoot, timeout: 5000 })
				.then((res) => {
					if (res.code !== 0) {
						swampActiveSummary = "";
						return;
					}
					const data = JSON.parse(res.stdout) as { runs?: SwampRun[] };
					const runs = Array.isArray(data.runs) ? data.runs : [];
					if (runs.length === 0) {
						swampActiveSummary = "";
						return;
					}
					const names = runs.map((r) => r.workflowName).filter((n): n is string => Boolean(n));
					const shown = names.slice(0, 2).join(", ");
					const extra = runs.length - 2;
					swampActiveSummary = `${runs.length} running: ${shown}${extra > 0 ? `, +${extra} more` : ""}`;
				})
				.catch(() => {
					// Leave previous summary in place on failure — a transient
					// swamp/timeout error shouldn't blank out the last-known state.
				})
				.finally(() => {
					swampFetchInFlight = false;
					swampLastFetch = Date.now();
					tui.requestRender();
				});
		}

		ctx.ui.setFooter((tui, theme, footerData) => {
			const unsub = footerData.onBranchChange(() => {
				resolveBranchLabel(footerData.getGitBranch(), tui);
				tui.requestRender();
			});
			resolveBranchLabel(footerData.getGitBranch(), tui);

			return {
				dispose: unsub,
				invalidate() {},
				render(width: number): string[] {
					maybeRefreshSwampActive(tui);

					const usage = ctx.getContextUsage();
					const used = usage?.tokens ?? null;
					const contextWindow = usage?.contextWindow ?? ctx.model?.contextWindow ?? DEFAULT_CONTEXT_WINDOW;
					const ratio = used === null ? null : used / CTX_BUDGET;
					const exceeds = used !== null && used >= CTX_BUDGET;

					const arrow = fg256(sessionColor, "➜");
					const branchStr = branchLabel ? theme.fg("dim", ` (${branchLabel})`) : "";
					const swampBadge = swampRoot ? " 🐸" : "";
					const proj = `${arrow} ${projectLabel(ctx.cwd)}${branchStr}${swampBadge}`;

					const model = ctx.model?.id ? theme.fg("dim", ` [${ctx.model.id}]`) : "";

					const usedLabel = used === null ? "?" : fmtTokens(used);
					const warn = exceeds ? " ⚠" : "";
					const ctxLabel =
						theme.fg("dim", `[ctx: `) +
						bar(theme, ratio) +
						theme.fg("dim", ` ${usedLabel}/${fmtTokens(contextWindow)}${warn}]`);

					const line1Raw = `${proj}${model} ${ctxLabel}`;
					const line1 = truncateToWidth(line1Raw, width);
					const line1Pad = " ".repeat(Math.max(0, width - visibleWidth(line1)));
					const lines = [line1 + line1Pad];

					// Line 2: any extension-set status text (e.g. a plan-mode or
					// vim-mode extension calling ctx.ui.setStatus()) — the same
					// generic mechanism the built-in footer uses, so this line
					// reflects real state instead of a fabricated permission mode.
					const extensionStatuses = footerData.getExtensionStatuses();
					if (extensionStatuses.size > 0) {
						const statusLine = Array.from(extensionStatuses.entries())
							.sort(([a], [b]) => a.localeCompare(b))
							.map(([, text]) => sanitizeStatusText(text))
							.join(" ");
						lines.push(truncateToWidth(statusLine, width, theme.fg("dim", "...")));
					}

					// Line 3: active swamp workflows, only when there are any.
					if (swampActiveSummary) {
						lines.push(truncateToWidth(theme.fg("dim", `🐸 ${swampActiveSummary}`), width, theme.fg("dim", "...")));
					}

					return lines;
				},
			};
		});
	});
}
