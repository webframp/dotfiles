// ABOUTME: Custom pi footer mirroring the claude-code-status statusline —
// ABOUTME: project/branch, model, and a context-usage bar with the same colors.

import { homedir } from "node:os";
import type { AssistantMessage } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

const DEFAULT_CONTEXT_WINDOW = 128_000;

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
function colorForRatio(theme: any, ratio: number): "success" | "warning" | "error" {
	if (ratio >= 0.8) return "error";
	if (ratio >= 0.5) return "warning";
	return "success";
}

function bar(theme: any, ratio: number, width = 8): string {
	const clamped = Math.max(0, Math.min(1, ratio));
	const filled = Math.round(clamped * width);
	const empty = width - filled;
	const color = colorForRatio(theme, clamped);
	return theme.fg(color, "█".repeat(filled)) + theme.fg("dim", "░".repeat(empty));
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		ctx.ui.setFooter((tui, theme, footerData) => {
			const unsub = footerData.onBranchChange(() => tui.requestRender());

			return {
				dispose: unsub,
				invalidate() {},
				render(width: number): string[] {
					let input = 0;
					let output = 0;
					for (const e of ctx.sessionManager.getBranch()) {
						if (e.type === "message" && e.message.role === "assistant") {
							const m = e.message as AssistantMessage;
							input += m.usage.input;
							output += m.usage.output;
						}
					}

					const contextWindow = (ctx.model as any)?.contextWindow ?? DEFAULT_CONTEXT_WINDOW;
					const used = input + output;
					const ratio = used / contextWindow;

					const branch = footerData.getGitBranch();
					const branchStr = branch ? theme.fg("dim", ` (${branch})`) : "";
					const proj = theme.fg("accent", "➜") + " " + projectLabel(ctx.cwd) + branchStr;

					const model = ctx.model?.id ? theme.fg("dim", ` [${ctx.model.id}]`) : "";

					const ctxLabel = theme.fg("dim", `[ctx: `) + bar(theme, ratio) + theme.fg("dim", ` ${fmtTokens(used)}/${fmtTokens(contextWindow)}]`);

					const left = truncateToWidth(`${proj}${model} ${ctxLabel}`, width);
					const pad = " ".repeat(Math.max(0, width - visibleWidth(left)));
					return [left + pad];
				},
			};
		});
	});
}
