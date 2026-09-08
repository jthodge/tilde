/**
 * Plan Mode Extension
 *
 * Read-only exploration mode. Plan mode narrows the LLM-visible tool
 * set to a snapshot of read/search tools. Tool-set management is
 * strictly authoritative — plan mode never re-enables tools that Pi
 * did not have active at the moment plan mode was entered.
 *
 * Boundaries:
 *
 * - Entry snapshots `pi.getActiveTools()` (the caller-visible active
 *   set at the moment of entry). Plan-mode's allowlist is intersected
 *   against that active set, NOT against `pi.getAllTools()`. This
 *   respects startup restrictions (`--tools ...`, `--no-builtin-tools`)
 *   and any earlier extension's `setActiveTools` decisions.
 * - Exit restores exactly the entry snapshot filtered by the current
 *   registered-tool set. Newly-registered tools that appeared while
 *   plan mode was active are added ONLY when they are also currently
 *   active (documented, narrow exception for dynamic-tool extensions
 *   such as loaders that add tools via `setActiveTools`).
 * - `tool_call` blocks EVERY tool outside the plan-mode-active set
 *   while plan mode is on. This includes bash/edit/write, custom
 *   extension tools, and dynamically-loaded tools. No assumption is
 *   made that extension tools are read-only.
 * - Session-replacement events (`new`, `resume`, `fork`) reset all
 *   in-memory state; the target session's persisted `plan-mode` entry
 *   is authoritative. Off-branch entries are ignored — state is read
 *   from `sessionManager.getBranch()`.
 * - Legacy persisted state without a `preplanTools` snapshot never
 *   re-enables a hardcoded default; the current `pi.getActiveTools()`
 *   is treated as the snapshot instead.
 * - `--plan` at startup takes an immediate snapshot and persists it
 *   during `session_start` so reload/resume cannot lose it.
 *
 * Plan mode is not a sandbox. It constrains Pi's LLM tool surface; it
 * does not restrict any process, filesystem, or network access that
 * runs outside Pi.
 *
 * Features:
 * - `/plan` command and Alt+P shortcut toggle plan mode
 * - Extracts numbered plan steps from the assistant `Plan:` section
 * - `[DONE:n]` markers close steps during execution
 * - Progress widget while executing a plan
 */

import type { AgentMessage } from "@earendil-works/pi-agent-core";
import type { AssistantMessage, TextContent } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Key } from "@earendil-works/pi-tui";

// Tools plan mode is willing to expose, when Pi has them registered
// AND they were active at plan-mode entry.
const READ_ONLY_TOOL_ALLOWLIST = ["read", "grep", "find", "ls"];

// Type guard for assistant messages
function isAssistantMessage(m: AgentMessage): m is AssistantMessage {
	return m.role === "assistant" && Array.isArray(m.content);
}

// Extract text content from an assistant message
function getTextContent(message: AssistantMessage): string {
	return message.content
		.filter((block): block is TextContent => block.type === "text")
		.map((block) => block.text)
		.join("\n");
}

interface TodoItem {
	step: number;
	text: string;
	completed: boolean;
}

function cleanStepText(text: string): string {
	let cleaned = text
		.replace(/\*{1,2}([^*]+)\*{1,2}/g, "$1")
		.replace(/`([^`]+)`/g, "$1")
		.replace(
			/^(Use|Run|Execute|Create|Write|Read|Check|Verify|Update|Modify|Add|Remove|Delete|Install)\s+(the\s+)?/i,
			"",
		)
		.replace(/\s+/g, " ")
		.trim();

	if (cleaned.length > 0) {
		cleaned = cleaned.charAt(0).toUpperCase() + cleaned.slice(1);
	}
	if (cleaned.length > 50) {
		cleaned = `${cleaned.slice(0, 47)}...`;
	}
	return cleaned;
}

function extractTodoItems(message: string): TodoItem[] {
	const items: TodoItem[] = [];
	const headerMatch = message.match(/\*{0,2}Plan:\*{0,2}\s*\n/i);
	if (!headerMatch) return items;

	const planSection = message.slice(message.indexOf(headerMatch[0]) + headerMatch[0].length);
	const numberedPattern = /^\s*(\d+)[.)]\s+\*{0,2}([^*\n]+)/gm;

	for (const match of planSection.matchAll(numberedPattern)) {
		const text = match[2]
			.trim()
			.replace(/\*{1,2}$/, "")
			.trim();
		if (text.length > 5 && !text.startsWith("`") && !text.startsWith("/") && !text.startsWith("-")) {
			const cleaned = cleanStepText(text);
			if (cleaned.length > 3) {
				items.push({ step: items.length + 1, text: cleaned, completed: false });
			}
		}
	}
	return items;
}

function extractDoneSteps(message: string): number[] {
	const steps: number[] = [];
	for (const match of message.matchAll(/\[DONE:(\d+)\]/gi)) {
		const step = Number(match[1]);
		if (Number.isFinite(step)) steps.push(step);
	}
	return steps;
}

function markCompletedSteps(text: string, items: TodoItem[]): number {
	const doneSteps = extractDoneSteps(text);
	for (const step of doneSteps) {
		const item = items.find((t) => t.step === step);
		if (item) item.completed = true;
	}
	return doneSteps.length;
}

/**
 * Compute the plan-mode tool set as the intersection of the allowlist
 * with the caller-visible currently-active tools. Preserves the
 * allowlist ordering.
 *
 * The input MUST be the currently-active tool set, not the
 * registered-tool set, so that plan mode does not re-enable tools
 * disabled by startup restrictions or by other extensions.
 */
export function planModeToolsFor(active: readonly string[]): string[] {
	const set = new Set(active);
	return READ_ONLY_TOOL_ALLOWLIST.filter((name) => set.has(name));
}

// ============================================================================
// Main extension
// ============================================================================

export default function planModeExtension(pi: ExtensionAPI): void {
	let planModeEnabled = false;
	let executionMode = false;
	let todoItems: TodoItem[] = [];
	// Tools that were caller-visible active when plan mode was entered.
	// undefined means plan mode is off (no snapshot to restore).
	let preplanTools: string[] | undefined;
	// Registered-tool snapshot captured at entry, so we can detect
	// "newly registered while plan mode was active" without depending
	// on any Pi internal state.
	let preplanRegistered = new Set<string>();

	pi.registerFlag("plan", {
		description: "Start in plan mode (read-only exploration)",
		type: "boolean",
		default: false,
	});

	function updateStatus(ctx: ExtensionContext): void {
		if (executionMode && todoItems.length > 0) {
			const completed = todoItems.filter((t) => t.completed).length;
			ctx.ui.setStatus("plan-mode", ctx.ui.theme.fg("accent", `📋 ${completed}/${todoItems.length}`));
		} else if (planModeEnabled) {
			ctx.ui.setStatus("plan-mode", ctx.ui.theme.fg("warning", "⏸ plan"));
		} else {
			ctx.ui.setStatus("plan-mode", undefined);
		}

		if (executionMode && todoItems.length > 0) {
			const lines = todoItems.map((item) => {
				if (item.completed) {
					return (
						ctx.ui.theme.fg("success", "☑ ") + ctx.ui.theme.fg("muted", ctx.ui.theme.strikethrough(item.text))
					);
				}
				return `${ctx.ui.theme.fg("muted", "☐ ")}${item.text}`;
			});
			ctx.ui.setWidget("plan-todos", lines);
		} else {
			ctx.ui.setWidget("plan-todos", undefined);
		}
	}

	/**
	 * Restore the pre-plan tool set, filtered by what is still
	 * registered. Tools registered while plan mode was active are added
	 * ONLY when they are still active at exit time (dynamic-tool
	 * extensions that added them intentionally).
	 */
	function restorePreplanTools(): string[] {
		const registeredNames = new Set(pi.getAllTools().map((t) => t.name));
		const currentActive = new Set(pi.getActiveTools());
		const restored: string[] = [];
		const seen = new Set<string>();

		for (const name of preplanTools ?? []) {
			if (registeredNames.has(name) && !seen.has(name)) {
				restored.push(name);
				seen.add(name);
			}
		}

		// Documented exception: tools registered while plan mode was
		// active AND currently active at exit are preserved. This is
		// the ONLY reason plan-mode exit would surface a tool not in
		// the entry snapshot; it never re-enables anything that was
		// off at entry.
		for (const name of currentActive) {
			if (!preplanRegistered.has(name) && registeredNames.has(name) && !seen.has(name)) {
				restored.push(name);
				seen.add(name);
			}
		}

		pi.setActiveTools(restored);
		return restored;
	}

	function enterPlanMode(ctx: ExtensionContext): string[] {
		// Snapshot the caller-visible active tools. This includes only
		// what Pi currently exposes; plan mode never widens beyond it.
		preplanTools = [...pi.getActiveTools()];
		preplanRegistered = new Set(pi.getAllTools().map((t) => t.name));
		const planTools = planModeToolsFor(preplanTools);
		pi.setActiveTools(planTools);
		if (ctx.hasUI) {
			const list = planTools.length ? planTools.join(", ") : "(none available)";
			ctx.ui.notify(`Plan mode enabled. Read-only tools: ${list}`);
		}
		return planTools;
	}

	function exitPlanMode(ctx: ExtensionContext): string[] {
		const restored = restorePreplanTools();
		preplanTools = undefined;
		preplanRegistered = new Set<string>();
		if (ctx.hasUI) {
			ctx.ui.notify(`Plan mode disabled. Restored ${restored.length} tool(s).`);
		}
		return restored;
	}

	function togglePlanMode(ctx: ExtensionContext): void {
		planModeEnabled = !planModeEnabled;
		executionMode = false;
		todoItems = [];

		if (planModeEnabled) {
			enterPlanMode(ctx);
		} else {
			exitPlanMode(ctx);
		}
		updateStatus(ctx);
		persistState();
	}

	function persistState(): void {
		pi.appendEntry("plan-mode", {
			enabled: planModeEnabled,
			todos: todoItems,
			executing: executionMode,
			preplanTools,
		});
	}

	pi.registerCommand("plan", {
		description: "Toggle plan mode (read-only exploration)",
		handler: async (_args, ctx) => togglePlanMode(ctx),
	});

	pi.registerCommand("todos", {
		description: "Show current plan todo list",
		handler: async (_args, ctx) => {
			if (todoItems.length === 0) {
				ctx.ui.notify("No todos. Create a plan first with /plan", "info");
				return;
			}
			const list = todoItems.map((item, i) => `${i + 1}. ${item.completed ? "✓" : "○"} ${item.text}`).join("\n");
			ctx.ui.notify(`Plan Progress:\n${list}`, "info");
		},
	});

	pi.registerShortcut(Key.alt("p"), {
		description: "Toggle plan mode",
		handler: async (ctx) => togglePlanMode(ctx),
	});

	// While plan mode is on, block every tool that is not in the
	// plan-mode-active set. This covers bash/edit/write and any
	// dynamic/extension tool that gets registered or activated
	// mid-turn. Custom tools are NOT assumed to be read-only.
	pi.on("tool_call", async (event) => {
		if (!planModeEnabled) return;
		const allowlist = new Set(planModeToolsFor(preplanTools ?? pi.getActiveTools()));
		if (allowlist.has(event.toolName)) return;
		return {
			block: true,
			reason: `Plan mode: '${event.toolName}' is disabled. Use /plan to exit plan mode.`,
		};
	});

	// Filter out stale plan mode context when not in plan mode
	pi.on("context", async (event) => {
		if (planModeEnabled) return;
		return {
			messages: event.messages.filter((m) => {
				const msg = m as AgentMessage & { customType?: string };
				if (msg.customType === "plan-mode-context") return false;
				if (msg.role !== "user") return true;
				const content = msg.content;
				if (typeof content === "string") {
					return !content.includes("[PLAN MODE ACTIVE]");
				}
				if (Array.isArray(content)) {
					return !content.some(
						(c) => c.type === "text" && (c as TextContent).text?.includes("[PLAN MODE ACTIVE]"),
					);
				}
				return true;
			}),
		};
	});

	pi.on("before_agent_start", async () => {
		if (planModeEnabled) {
			const allowed = new Set(planModeToolsFor(preplanTools ?? []));
			const planTools = pi.getActiveTools().filter((name) => allowed.has(name));
			const list = planTools.length ? planTools.join(", ") : "(none)";
			return {
				message: {
					customType: "plan-mode-context",
					content: `[PLAN MODE ACTIVE]
You are in plan mode - a read-only exploration mode for safe code analysis.

Restrictions:
- Available tools: ${list}
- Every other tool is disabled, including bash, edit, write, and any
  extension or dynamic tool. Plan mode narrows Pi's tool surface but
  is not a sandbox.

Explore the code, then propose a detailed numbered plan under a "Plan:"
header:

Plan:
1. First step description
2. Second step description
...

Do NOT attempt to make changes - just describe what you would do.`,
					display: false,
				},
			};
		}

		if (executionMode && todoItems.length > 0) {
			const remaining = todoItems.filter((t) => !t.completed);
			const todoList = remaining.map((t) => `${t.step}. ${t.text}`).join("\n");
			// Deliberately no "full tool access" claim here. Execution
			// restores whatever tools were active pre-plan (filtered
			// against still-registered), so the surface may still be
			// narrower than a default startup.
			return {
				message: {
					customType: "plan-execution-context",
					content: `[EXECUTING PLAN]

The plan-mode restriction is lifted. Available tools follow the
session's normal configuration (startup restrictions, extensions, and
any tools loaded during planning).

Remaining steps:
${todoList}

Execute each step in order. After completing a step, include a
[DONE:n] tag in your response.`,
					display: false,
				},
			};
		}
	});

	pi.on("turn_end", async (event, ctx) => {
		if (!executionMode || todoItems.length === 0) return;
		if (!isAssistantMessage(event.message)) return;
		const text = getTextContent(event.message);
		if (markCompletedSteps(text, todoItems) > 0) {
			updateStatus(ctx);
		}
		persistState();
	});

	pi.on("agent_end", async (event, ctx) => {
		if (executionMode && todoItems.length > 0) {
			if (todoItems.every((t) => t.completed)) {
				const completedList = todoItems.map((t) => `~~${t.text}~~`).join("\n");
				pi.sendMessage(
					{ customType: "plan-complete", content: `**Plan Complete!** ✓\n\n${completedList}`, display: true },
					{ triggerTurn: false },
				);
				executionMode = false;
				todoItems = [];
				updateStatus(ctx);
				persistState();
			}
			return;
		}

		if (!planModeEnabled || !ctx.hasUI) return;

		const lastAssistant = [...event.messages].reverse().find(isAssistantMessage);
		if (lastAssistant) {
			const extracted = extractTodoItems(getTextContent(lastAssistant));
			if (extracted.length > 0) {
				todoItems = extracted;
			}
		}

		if (todoItems.length > 0) {
			const todoListText = todoItems.map((t, i) => `${i + 1}. ☐ ${t.text}`).join("\n");
			pi.sendMessage(
				{
					customType: "plan-todo-list",
					content: `**Plan Steps (${todoItems.length}):**\n\n${todoListText}`,
					display: true,
				},
				{ triggerTurn: false },
			);
		}

		const choice = await ctx.ui.select("Plan mode - what next?", [
			todoItems.length > 0 ? "Execute the plan (track progress)" : "Execute the plan",
			"Stay in plan mode",
			"Refine the plan",
		]);

		if (choice?.startsWith("Execute")) {
			planModeEnabled = false;
			executionMode = todoItems.length > 0;
			// Restore the caller-visible pre-plan tool set (filtered
			// against still-registered), plus intentionally added
			// dynamic tools. Never widens beyond what was active at
			// entry.
			exitPlanMode(ctx);
			updateStatus(ctx);
			persistState();

			const execMessage =
				todoItems.length > 0
					? `Execute the plan. Start with: ${todoItems[0].text}`
					: "Execute the plan you just created.";
			pi.sendMessage(
				{ customType: "plan-mode-execute", content: execMessage, display: true },
				{ triggerTurn: true },
			);
		} else if (choice === "Refine the plan") {
			const refinement = await ctx.ui.editor("Refine the plan:", "");
			if (refinement?.trim()) {
				pi.sendUserMessage(refinement.trim());
			}
		}
	});

	// Reset in-memory state on any session-replacement flow. `new` and
	// `resume` and `fork` each rebind extensions; per Pi docs the new
	// extension instance receives `session_start` with the appropriate
	// reason. We still explicitly zero state here rather than trust any
	// carry-over from a previous instance.
	pi.on("session_shutdown", async () => {
		// Do not carry this extension's temporary restriction into a replacement session.
		if (planModeEnabled) restorePreplanTools();
		planModeEnabled = false;
		executionMode = false;
		todoItems = [];
		preplanTools = undefined;
		preplanRegistered = new Set<string>();
	});

	async function restoreSession(event: { reason: string }, ctx: ExtensionContext): Promise<void> {
		// Fresh state for the newly bound session; any leftover
		// in-memory state from before rebind must not leak into the
		// target session's tool surface.
		planModeEnabled = false;
		executionMode = false;
		todoItems = [];
		preplanTools = undefined;
		preplanRegistered = new Set<string>();

		const startedInPlan = pi.getFlag("plan") === true;
		const isFreshSession = event.reason === "new" || event.reason === "fork";

		// Read persisted state from the ACTIVE BRANCH only. `getEntries`
		// includes off-branch state which should not restore stale plan
		// mode. `getBranch()` returns the current active-branch path.
		const branchEntries = ctx.sessionManager.getBranch();
		const planModeEntry = branchEntries
			.filter(
				(e: { type: string; customType?: string }) =>
					e.type === "custom" && e.customType === "plan-mode",
			)
			.pop() as
			| {
					data?: {
						enabled?: boolean;
						todos?: TodoItem[];
						executing?: boolean;
						preplanTools?: string[];
					};
			  }
			| undefined;

		// Fresh sessions (new/fork) never restore plan state from any
		// other session's branch. Only "resume" and "reload" carry
		// prior state.
		const shouldRestore = !isFreshSession && planModeEntry?.data !== undefined;

		if (shouldRestore && planModeEntry?.data) {
			planModeEnabled = planModeEntry.data.enabled ?? false;
			todoItems = planModeEntry.data.todos ?? [];
			executionMode = planModeEntry.data.executing ?? false;
			// Startup configuration is authoritative, not an older, possibly wider snapshot.
			preplanTools = [...pi.getActiveTools()];
		}

		if (startedInPlan && event.reason !== "reload" && event.reason !== "tree") {
			// CLI --plan wins over persisted state on initial entry, not on reload. Take a snapshot
			// immediately from the current active-tools set and
			// persist it before we could ever be reloaded.
			planModeEnabled = true;
			executionMode = false;
			todoItems = [];
			preplanTools = [...pi.getActiveTools()];
			preplanRegistered = new Set(pi.getAllTools().map((t) => t.name));
			pi.setActiveTools(planModeToolsFor(preplanTools));
			// Persist immediately so a reload cannot lose the snapshot.
			persistState();
			updateStatus(ctx);
			return;
		}

		// On resume: rebuild completion state from the active branch
		// after the last plan-mode-execute marker.
		if (shouldRestore && executionMode && todoItems.length > 0) {
			let executeIndex = -1;
			for (let i = branchEntries.length - 1; i >= 0; i--) {
				const entry = branchEntries[i] as { type: string; customType?: string };
				if (entry.customType === "plan-mode-execute") {
					executeIndex = i;
					break;
				}
			}
			const messages: AssistantMessage[] = [];
			for (let i = executeIndex + 1; i < branchEntries.length; i++) {
				const entry = branchEntries[i];
				if (
					entry.type === "message" &&
					"message" in entry &&
					isAssistantMessage(entry.message as AgentMessage)
				) {
					messages.push(entry.message as AssistantMessage);
				}
			}
			const allText = messages.map(getTextContent).join("\n");
			markCompletedSteps(allText, todoItems);
		}

		if (planModeEnabled) {
			// Legacy state without preplanTools: never fall back to a
			// hardcoded default. Use the currently-active set (which
			// already reflects startup restrictions) as the snapshot.
			// This deliberately treats the current active set as the
			// authoritative pre-plan surface — the alternative (a
			// hardcoded fallback) would silently widen the surface
			// after a legacy resume.
			if (!preplanTools) {
				preplanTools = [...pi.getActiveTools()];
			}
			preplanRegistered = new Set(pi.getAllTools().map((t) => t.name));
			pi.setActiveTools(planModeToolsFor(preplanTools));
		}
		if (planModeEnabled) persistState();
		updateStatus(ctx);
	}
	pi.on("session_start", restoreSession);
	pi.on("session_tree", async (_event, ctx) => {
		if (planModeEnabled) restorePreplanTools();
		await restoreSession({ reason: "tree" }, ctx);
	});
}
