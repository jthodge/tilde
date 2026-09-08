// ESM loader hook: only pi-tui gets a tiny runtime stub. Every other
// `@earendil-works/pi-*` import is `import type` in the actual
// extension sources and is erased by node:module.stripTypeScriptTypes,
// Unexpected runtime dependencies fail rather than being silently mocked.

const PI_TUI_STUB =
    "export const Key = { alt: (k) => `alt+${k}`, ctrl: (k) => `ctrl+${k}` };\n";

function toDataUrl(source) {
    return `data:text/javascript;base64,${Buffer.from(source).toString("base64")}`;
}

export async function resolve(specifier, context, nextResolve) {
    if (specifier === "@earendil-works/pi-tui") {
        return { url: toDataUrl(PI_TUI_STUB), shortCircuit: true, format: "module" };
    }
    if (specifier.startsWith("@earendil-works/pi-")) {
        throw new Error(`Unexpected Pi runtime import in extension test: ${specifier}`);
    }
    return nextResolve(specifier, context);
}
