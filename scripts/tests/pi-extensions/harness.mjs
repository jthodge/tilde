// Test harness for Pi extensions.
//
// Loads the *actual* extension source from pi/.pi/agent/extensions
// using node:module.stripTypeScriptTypes (Node >= 22.13). The stripped
// JavaScript is imported via a data: URL. Only `@earendil-works/pi-tui`
// is stubbed at runtime (see loader.mjs); every other pi-* import in
// the extension sources is `import type` and is erased by the type
// stripper.
//
// If node:module.stripTypeScriptTypes is unavailable we exit with a
// clear MISSING message so the runner fails truthfully instead of
// silently skipping.

import { readFileSync } from "node:fs";
import { dirname, resolve as pathResolve } from "node:path";
import { fileURLToPath } from "node:url";
import * as nodeModule from "node:module";
const { register, stripTypeScriptTypes } = nodeModule;

const HERE = dirname(fileURLToPath(import.meta.url));
export const REPO_ROOT = pathResolve(HERE, "..", "..", "..");

if (typeof stripTypeScriptTypes !== "function") {
    console.error(
        "MISSING: node:module.stripTypeScriptTypes. Node >= 22.13 is required to run pi-extension regression tests. Current runtime lacks the built-in type stripper.",
    );
    process.exit(2);
}

// Register the pi-tui stub resolver. Safe to call once per process.
register("./loader.mjs", import.meta.url);

/**
 * Load and evaluate an extension source file. Reads the actual .ts
 * source, strips types with the built-in stripper, and imports the
 * resulting JavaScript as a data: URL. The default export (the
 * extension factory) is returned along with any named exports.
 */
export async function loadExtensionSource(tsPath) {
    const source = readFileSync(tsPath, "utf8");
    const stripped = stripTypeScriptTypes(source, { mode: "strip" });
    const dataUrl = `data:text/javascript;base64,${Buffer.from(stripped).toString("base64")}`;
    return import(dataUrl);
}

/**
 * Build a minimal ExtensionAPI compatible with plan-mode.ts and the
 * gate extensions. Only models the surface these extensions actually
 * touch.
 *
 * initialActive: caller-visible active tools (defaults to a small
 *   subset).
 * initialAll: registered-tool set. Defaults to initialActive; specify
 *   a superset to exercise the case where some registered tools are
 *   disabled at startup (e.g. `pi --tools read`).
 */
export function createExtensionHarness({ initialActive = ["read"], initialAll } = {}) {
    const allTools = new Map();
    for (const name of initialAll ?? initialActive) allTools.set(name, { name });
    for (const name of initialActive) if (!allTools.has(name)) allTools.set(name, { name });
    let active = [...initialActive];
    const listeners = new Map();
    const commands = new Map();
    const shortcuts = new Map();
    const flags = new Map();
    const flagValues = new Map();
    // `entries` mirrors Pi's session storage; `branch` is the active
    // branch path (getBranch). The extension must read state from the
    // branch, not from all entries.
    const entries = [];
    const branch = [];
    const notifications = [];
    const statuses = new Map();
    const widgets = new Map();
    const sentMessages = [];
    const uiCalls = [];

    let selectResponse;
    let editorResponse;

    const theme = { fg: (_s, t) => t, bold: (t) => t, strikethrough: (t) => t };

    const ctx = {
        cwd: REPO_ROOT,
        hasUI: false,
        mode: "print",
        sessionManager: {
            getEntries: () => entries.slice(),
            getBranch: () => branch.slice(),
        },
        ui: {
            theme,
            notify: (msg, level = "info") => notifications.push({ msg, level }),
            setStatus: (id, v) => (v === undefined ? statuses.delete(id) : statuses.set(id, v)),
            setWidget: (id, v) => (v === undefined ? widgets.delete(id) : widgets.set(id, v)),
            select: async (title, options) => {
                uiCalls.push({ kind: "select", title, options });
                return selectResponse;
            },
            editor: async (title, initial) => {
                uiCalls.push({ kind: "editor", title, initial });
                return editorResponse;
            },
            confirm: async () => true,
            input: async () => "",
        },
    };

    const pi = {
        on(event, handler) {
            if (!listeners.has(event)) listeners.set(event, []);
            listeners.get(event).push(handler);
        },
        registerCommand(name, options) {
            commands.set(name, options);
        },
        registerShortcut(key, options) {
            shortcuts.set(key, options);
        },
        registerFlag(name, options) {
            flags.set(name, options);
        },
        getFlag(name) {
            return flagValues.get(name);
        },
        registerTool(def) {
            allTools.set(def.name, def);
            if (!active.includes(def.name)) active.push(def.name);
        },
        getActiveTools() {
            return active.slice();
        },
        getAllTools() {
            return Array.from(allTools.values());
        },
        setActiveTools(names) {
            active = names.slice();
        },
        appendEntry(customType, data) {
            const entry = { type: "custom", customType, data };
            entries.push(entry);
            branch.push(entry);
        },
        sendMessage(msg) {
            sentMessages.push({ kind: "custom", msg });
        },
        sendUserMessage(msg) {
            sentMessages.push({ kind: "user", msg });
        },
    };

    return {
        pi,
        ctx,
        state: {
            get active() { return active; },
            get allTools() { return Array.from(allTools.keys()); },
            get commands() { return commands; },
            get shortcuts() { return shortcuts; },
            get flags() { return flags; },
            get entries() { return entries; },
            get branch() { return branch; },
            get notifications() { return notifications; },
            get statuses() { return statuses; },
            get widgets() { return widgets; },
            get sentMessages() { return sentMessages; },
            get uiCalls() { return uiCalls; },
        },
        setFlag(name, value) {
            flagValues.set(name, value);
        },
        setSelectResponse(v) {
            selectResponse = v;
        },
        setEditorResponse(v) {
            editorResponse = v;
        },
        // Simulate a tool registered by some other extension that is
        // NOT active by default (e.g. a search-tool loader that adds
        // siblings inactively).
        addLatentTool(name) {
            allTools.set(name, { name });
        },
        // Simulate the user or another extension activating a tool
        // (e.g. `pi.setActiveTools([...active, newTool])`).
        activateTool(name) {
            if (!allTools.has(name)) allTools.set(name, { name });
            if (!active.includes(name)) active.push(name);
        },
        removeTool(name) {
            allTools.delete(name);
            active = active.filter((n) => n !== name);
        },
        async fire(event, payload) {
            const handlers = listeners.get(event) ?? [];
            const results = [];
            for (const handler of handlers) results.push(await handler(payload, ctx));
            return results;
        },
        async runCommand(name, args = "") {
            const command = commands.get(name);
            if (!command) throw new Error(`Command not registered: ${name}`);
            return command.handler(args, ctx);
        },
        async runShortcut(key) {
            const shortcut = shortcuts.get(key);
            if (!shortcut) throw new Error(`Shortcut not registered: ${key}`);
            return shortcut.handler(ctx);
        },
        // Seed a persisted custom entry. Adds it to both the raw
        // entry log and the active branch by default. Pass
        // { offBranch: true } to seed something that only shows up in
        // getEntries() and NOT in getBranch(), to prove the extension
        // reads from the branch.
        seedEntry(entry, { offBranch = false } = {}) {
            entries.push(entry);
            if (!offBranch) branch.push(entry);
        },
    };
}
