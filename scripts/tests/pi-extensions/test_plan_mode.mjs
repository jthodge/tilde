import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { createExtensionHarness, loadExtensionSource, REPO_ROOT } from "./harness.mjs";

const PLAN_MODE_PATH = join(REPO_ROOT, "pi", ".pi", "agent", "extensions", "plan-mode.ts");

let planModeModule;

async function makeHarness(opts) {
    const h = createExtensionHarness(opts);
    planModeModule.default(h.pi);
    return h;
}

test("load plan-mode.ts via node:module.stripTypeScriptTypes", async () => {
    planModeModule = await loadExtensionSource(PLAN_MODE_PATH);
    assert.equal(typeof planModeModule.default, "function");
    assert.equal(typeof planModeModule.planModeToolsFor, "function");
});

test("entry intersects allowlist with ACTIVE tools, not all-registered", async () => {
    // Startup restriction: only `read` and `bash` are active, though
    // `grep`/`find`/`ls` are registered. Plan mode must NOT enable
    // grep/find/ls just because they are registered.
    const h = await makeHarness({
        initialActive: ["read", "bash"],
        initialAll: ["read", "bash", "edit", "write", "grep", "find", "ls"],
    });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    assert.deepEqual(h.state.active, ["read"],
        "plan mode narrows against active, not registered");
});

test("exit restores exact pre-plan snapshot, filtered by still-registered", async () => {
    const h = await makeHarness({
        initialActive: ["read", "bash", "edit", "write", "custom_ext"],
        initialAll: ["read", "bash", "edit", "write", "grep", "find", "ls", "custom_ext"],
    });
    await h.fire("session_start", { reason: "startup" });
    const preplan = h.state.active.slice().sort();
    await h.runCommand("plan");
    assert.deepEqual(h.state.active, ["read"],
        "only read is both allowlisted and pre-plan-active");
    await h.runCommand("plan");
    assert.deepEqual(h.state.active.slice().sort(), preplan,
        "exit must restore exactly the pre-plan snapshot");
    // Explicitly assert grep/find/ls stay OFF — the union-with-active
    // bug used to re-enable them.
    for (const t of ["grep", "find", "ls"]) {
        assert.ok(!h.state.active.includes(t),
            `${t} must remain disabled after plan-mode roundtrip`);
    }
});

test("tool_call blocks EVERY tool outside allowlist while plan mode is on", async () => {
    const h = await makeHarness({
        initialActive: ["read", "bash", "edit", "write", "grep", "custom_ext"],
    });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    // In allowlist AND active: allowed.
    const [readDecision] = await h.fire("tool_call", { toolName: "read", input: { path: "x" } });
    assert.equal(readDecision, undefined);
    const [grepDecision] = await h.fire("tool_call", { toolName: "grep", input: {} });
    assert.equal(grepDecision, undefined);
    // Outside allowlist: blocked. Includes bash/edit/write AND any
    // custom/dynamic extension tool. Plan mode does not assume
    // custom tools are read-only.
    for (const t of ["bash", "edit", "write", "custom_ext", "unseen_dynamic_tool"]) {
        const [d] = await h.fire("tool_call", { toolName: t, input: {} });
        assert.ok(d?.block, `expected ${t} blocked in plan mode`);
        assert.match(d.reason, /Plan mode/);
    }
});

test("tool_call is a no-op when plan mode is off", async () => {
    const h = await makeHarness();
    await h.fire("session_start", { reason: "startup" });
    const [d] = await h.fire("tool_call", { toolName: "bash", input: { command: "echo" } });
    assert.equal(d, undefined);
});

test("dynamic tool registered during plan is blocked until exit, then surfaced", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    // A loader extension registers a sibling that is NOT read-only.
    h.pi.registerTool({ name: "late_writer" });
    // While plan mode is on, it is blocked (defense-in-depth).
    const [decision] = await h.fire("tool_call", { toolName: "late_writer", input: {} });
    assert.ok(decision?.block, "dynamic tool blocked while plan mode active");
    // Exit: newly-added AND currently-active tool surfaces via the
    // documented dynamic-tool exception; pre-plan tools also restored.
    await h.runCommand("plan");
    assert.ok(h.state.active.includes("late_writer"),
        "intentionally-added dynamic tool restored after exit");
    assert.ok(h.state.active.includes("bash"), "pre-plan bash restored");
});

test("latent (registered but never activated) tools stay inactive across exit", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    // Simulate a loader-registered tool that stays inactive (Pi's
    // dynamic-tool pattern for search-based loaders).
    h.addLatentTool("latent_tool");
    await h.runCommand("plan");
    assert.ok(!h.state.active.includes("latent_tool"),
        "latent tool must not be silently activated on exit");
});

test("tool removed while plan is active is dropped from restore", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "custom_ext"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    h.removeTool("custom_ext");
    await h.runCommand("plan");
    assert.ok(!h.state.active.includes("custom_ext"),
        "unregistered tool must not be restored");
    assert.ok(h.state.active.includes("read"));
    assert.ok(h.state.active.includes("bash"));
});

test("repeated toggles do not accumulate or lose tools", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "edit", "write"] });
    await h.fire("session_start", { reason: "startup" });
    const baseline = h.state.active.slice().sort();
    for (let i = 0; i < 5; i++) {
        await h.runCommand("plan");
        await h.runCommand("plan");
    }
    assert.deepEqual(h.state.active.slice().sort(), baseline);
});

test("session_start with reason=new resets in-memory state", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    assert.deepEqual(h.state.active, ["read"]);
    // Simulate Pi tearing the runtime down and rebinding to a fresh
    // session (Pi always creates a new active-tools baseline for the
    // new session). We reset the harness's active list to model that.
    await h.fire("session_shutdown", { reason: "new" });
    h.pi.setActiveTools(["read", "bash"]);
    // Seed a stale plan-mode entry on the new session's branch. Even
    // if the persistence came along, reason="new" must ignore it.
    h.seedEntry({
        type: "custom",
        customType: "plan-mode",
        data: { enabled: true, todos: [], executing: false, preplanTools: ["read"] },
    });
    await h.fire("session_start", { reason: "new" });
    assert.deepEqual(h.state.active.sort(), ["bash", "read"],
        "fresh session must not restore prior plan mode");
    // And a subsequent /plan takes a fresh snapshot (not the stale one).
    await h.runCommand("plan");
    await h.runCommand("plan");
    assert.ok(h.state.active.includes("bash"),
        "post-new snapshot must include the new session's tools");
});

test("session_start reads from active branch, ignoring off-branch entries", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "edit"] });
    // Seed an off-branch entry that would (incorrectly) enable plan
    // mode if the extension used getEntries() instead of getBranch().
    h.seedEntry(
        {
            type: "custom",
            customType: "plan-mode",
            data: { enabled: true, todos: [], executing: false, preplanTools: ["read", "bash", "edit"] },
        },
        { offBranch: true },
    );
    await h.fire("session_start", { reason: "resume" });
    assert.deepEqual(h.state.active.sort(), ["bash", "edit", "read"],
        "off-branch plan-mode entry must not restrict tools");
});

test("--plan CLI flag persists snapshot immediately on session_start", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "custom_ext"] });
    h.setFlag("plan", true);
    await h.fire("session_start", { reason: "startup" });
    assert.deepEqual(h.state.active, ["read"],
        "startup --plan narrows to read-only allowlist restricted to active");
    // A plan-mode entry with the snapshot must be persisted BEFORE
    // any reload could drop it.
    const persisted = h.state.entries.filter(
        (e) => e.type === "custom" && e.customType === "plan-mode",
    );
    assert.ok(persisted.length >= 1, "plan-mode entry must be persisted immediately");
    const latest = persisted[persisted.length - 1];
    assert.equal(latest.data.enabled, true);
    assert.deepEqual(latest.data.preplanTools.sort(), ["bash", "custom_ext", "read"]);
});

test("resume restores plan mode narrowed to snapshot ∩ allowlist", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "grep", "custom_ext"] });
    h.seedEntry({
        type: "custom",
        customType: "plan-mode",
        data: {
            enabled: true,
            todos: [],
            executing: false,
            preplanTools: ["read", "bash", "grep", "custom_ext"],
        },
    });
    await h.fire("session_start", { reason: "resume" });
    assert.deepEqual(h.state.active.sort(), ["grep", "read"],
        "resume narrows to allowlist ∩ snapshot");
    // Toggle off — the custom_ext + bash from the snapshot come back.
    await h.runCommand("plan");
    assert.ok(h.state.active.includes("custom_ext"));
    assert.ok(h.state.active.includes("bash"));
});

test("resume respects startup tool restrictions (does not re-enable filtered tools)", async () => {
    // Persisted snapshot from a prior run when bash was active. On
    // resume Pi has been started with `--tools read` (bash not in
    // active OR in registered). Plan mode must not re-materialize
    // bash on exit.
    const h = await makeHarness({
        initialActive: ["read"],
        initialAll: ["read", "bash", "grep"], // registered does not mean active
    });
    h.seedEntry({
        type: "custom",
        customType: "plan-mode",
        data: {
            enabled: true,
            todos: [],
            executing: false,
            preplanTools: ["read", "bash"],
        },
    });
    await h.fire("session_start", { reason: "resume" });
    assert.deepEqual(h.state.active, ["read"]);
    await h.runCommand("plan");
    assert.ok(!h.state.active.includes("bash"),
        "bash must not be restored when startup filtered it out");
    assert.ok(h.state.active.includes("read"));
});

test("reload restores normal tools before re-entering plan mode", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "custom"] });
    h.setFlag("plan", true);
    await h.fire("session_start", { reason: "startup" });
    await h.fire("session_shutdown", { reason: "reload" });
    assert.deepEqual(h.state.active, ["read", "bash", "custom"]);
    await h.fire("session_start", { reason: "reload" });
    assert.deepEqual(h.state.active, ["read"]);
    await h.runCommand("plan");
    assert.deepEqual(h.state.active, ["read", "bash", "custom"]);
    await h.fire("session_shutdown", { reason: "reload" });
    await h.fire("session_start", { reason: "reload" });
    assert.deepEqual(h.state.active, ["read", "bash", "custom"], "reload must not reapply the CLI flag");
});

test("tree navigation clears off-branch restrictions without losing tools", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash", "custom"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    h.state.branch.length = 0;
    await h.fire("session_tree", {});
    assert.deepEqual(h.state.active, ["read", "bash", "custom"]);
    const [decision] = await h.fire("tool_call", { toolName: "bash", input: {} });
    assert.equal(decision, undefined);
});

test("new dynamic tools survive an immediate toggle without a turn event", async () => {
    const h = await makeHarness({ initialActive: ["read", "bash"] });
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    h.activateTool("new_tool");
    await h.runCommand("plan");
    assert.deepEqual(h.state.active, ["read", "bash", "new_tool"]);
});

test("legacy persisted state without preplanTools falls back to current active, never a hardcoded set", async () => {
    const h = await makeHarness({ initialActive: ["read"] });
    h.seedEntry({
        type: "custom",
        customType: "plan-mode",
        data: { enabled: true, todos: [], executing: false /* no preplanTools */ },
    });
    await h.fire("session_start", { reason: "resume" });
    assert.deepEqual(h.state.active, ["read"], "narrows to allowlist ∩ current active");
    await h.runCommand("plan");
    // No bash/edit/write silently re-enabled from a hardcoded fallback.
    assert.deepEqual(h.state.active, ["read"]);
});

test("execution message does NOT claim full tool access", async () => {
    const h = await makeHarness({ initialActive: ["read", "grep"] });
    h.ctx.hasUI = true;
    await h.fire("session_start", { reason: "startup" });
    await h.runCommand("plan");
    h.setSelectResponse("Execute the plan (track progress)");
    // Drive the extension through its plan-completion path: an
    // assistant message with a numbered Plan block, then agent_end
    // in plan mode. The extension parses todos, prompts, and on
    // "Execute" flips to executionMode and calls exitPlanMode.
    await h.fire("agent_end", {
        messages: [
            {
                role: "assistant",
                content: [
                    {
                        type: "text",
                        text: "Plan:\n1. Do the first thing here\n2. Do the second thing now\n",
                    },
                ],
            },
        ],
    });
    const results = await h.fire("before_agent_start", {});
    const msg = results.find((r) => r?.message?.customType === "plan-execution-context");
    assert.ok(msg, "expected execution-context message once executionMode is on");
    assert.doesNotMatch(msg.message.content, /full tool access/i,
        "execution context must not claim full tool access");
});

test("planModeToolsFor takes an ACTIVE list; empty active means empty result", () => {
    const { planModeToolsFor } = planModeModule;
    assert.deepEqual(planModeToolsFor(["read", "bash", "grep"]).sort(), ["grep", "read"]);
    assert.deepEqual(planModeToolsFor([]), []);
    assert.deepEqual(planModeToolsFor(["bash", "edit"]), []);
});

test("plan-mode source has no stale references or hardcoded write sets", () => {
    const source = readFileSync(PLAN_MODE_PATH, "utf8");
    assert.doesNotMatch(source, /questionnaire/i);
    assert.doesNotMatch(source, /brave-search/i);
    assert.doesNotMatch(source, /DESTRUCTIVE_PATTERNS|SAFE_PATTERNS/);
    assert.doesNotMatch(source, /NORMAL_MODE_TOOLS\s*=/);
    // Guard against a regression that intersected the allowlist with
    // pi.getAllTools() at entry, which would re-enable tools disabled
    // by startup restrictions.
    assert.doesNotMatch(
        source,
        /planModeToolsFor\s*\(\s*pi\s*\.\s*getAllTools/,
        "plan-mode must NOT intersect the allowlist with getAllTools at entry",
    );
});
