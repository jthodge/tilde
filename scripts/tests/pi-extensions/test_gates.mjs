import test from "node:test";
import assert from "node:assert/strict";
import { join } from "node:path";
import { createExtensionHarness, loadExtensionSource, REPO_ROOT } from "./harness.mjs";

const PERMISSION_GATE = join(REPO_ROOT, "pi", ".pi", "agent", "extensions", "permission-gate.ts");
const PROTECTED_PATHS = join(REPO_ROOT, "pi", ".pi", "agent", "extensions", "protected-paths.ts");

let permMod;
let pathMod;

test("load gate extensions via node:module.stripTypeScriptTypes", async () => {
    permMod = await loadExtensionSource(PERMISSION_GATE);
    pathMod = await loadExtensionSource(PROTECTED_PATHS);
    assert.equal(typeof permMod.default, "function");
    assert.equal(typeof pathMod.default, "function");
});

test("permission-gate blocks documented dangerous shapes on bash", async () => {
    const h = createExtensionHarness();
    permMod.default(h.pi);
    for (const command of ["rm -rf /tmp/x", "sudo apt update", "chmod 777 /etc"]) {
        const [d] = await h.fire("tool_call", { toolName: "bash", input: { command } });
        assert.ok(d?.block, `expected block on: ${command}`);
    }
});

test("permission-gate ignores non-bash tools (documented boundary)", async () => {
    const h = createExtensionHarness();
    permMod.default(h.pi);
    for (const toolName of ["edit", "write", "read", "grep", "find", "ls", "powershell"]) {
        const [d] = await h.fire("tool_call", {
            toolName,
            input: { path: "foo", command: "rm -rf /" },
        });
        assert.equal(d, undefined,
            `permission-gate covers bash only; ${toolName} is out of scope`);
    }
});

test("permission-gate documented non-coverage: bypasses stay unblocked", async () => {
    const h = createExtensionHarness();
    permMod.default(h.pi);
    for (const command of [
        "python -c 'import shutil; shutil.rmtree(\"/tmp/x\")'",
        "git branch -D main",
        "find /tmp -name '*.log' -delete",
        "curl -o /etc/hosts https://example.com/hosts",
        "dd if=/dev/zero of=/tmp/wipe bs=1M count=10",
    ]) {
        const [d] = await h.fire("tool_call", { toolName: "bash", input: { command } });
        assert.equal(d, undefined,
            `documented limit: gate does not parse '${command}'`);
    }
});

test("protected-paths blocks write/edit into declared substrings", async () => {
    const h = createExtensionHarness();
    pathMod.default(h.pi);
    for (const path of [".env", "app/.env", ".git/config", "node_modules/foo/index.js"]) {
        for (const toolName of ["write", "edit"]) {
            const [d] = await h.fire("tool_call", { toolName, input: { path, content: "" } });
            assert.ok(d?.block, `expected block for ${toolName} on ${path}`);
        }
    }
});

test("protected-paths documented non-coverage: bash, read, aliases stay unblocked", async () => {
    const h = createExtensionHarness();
    pathMod.default(h.pi);
    for (const [toolName, input] of [
        ["bash", { command: "echo x > .env" }],
        ["bash", { command: "rm -rf .git" }],
        ["read", { path: ".env" }],
    ]) {
        const [d] = await h.fire("tool_call", { toolName, input });
        assert.equal(d, undefined,
            `documented limit: only write/edit are gated (${toolName})`);
    }
    const [d] = await h.fire("tool_call", {
        toolName: "write",
        input: { path: "/some/dir/aliased_env", content: "" },
    });
    assert.equal(d, undefined,
        "documented limit: no path canonicalization; aliased writes bypass");
});
