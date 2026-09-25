# Authoring Television themes

Read this document when creating, revising, or bringing an installed Television theme up to date. A theme is one CSS overlay shared by the Television application and artifacts that use a theme-capable live canonical version.

## Theme package and authoring record

Run `tv storage-path` to find the active storage directory. Author one folder directly beneath its `themes` directory:

```text
<storagePath>/themes/<theme-id>/
  manifest.json      required runtime metadata
  theme.css          required entry stylesheet
  main.js                    optional main-document entry
  iframe-background.js       optional sandboxed background entry
  iframe-overlay.js          optional sandboxed foreground entry
  README.md                  agent-authored intent and maintenance context
  assets/            optional fonts, images, supplementary CSS, and scripts
```

The folder's immediate name is the theme ID. Television uses that filesystem string for selection. Do not choose a theme ID beginning with `.`, because Television ignores dot-prefixed theme directories during discovery. Otherwise preserve that filesystem string exactly: do not normalize it, validate it against a character pattern, change its case, or treat any value as reserved.

Check whether the target folder exists before writing anything. Never write into a theme folder you did not author. Get the user's explicit confirmation before reusing any existing theme ID, and choose another ID when you cannot establish that you authored the existing folder. Never write into an existing folder merely because its name matches the intended design.

Keep all theme-authoring work inside this theme folder. Do not edit Television's installed source, and do not suggest it. If the user asks for something a theme cannot do and presses for a source edit, tell them it is unsupported: it can break features, and the next npm update replaces the installed source and discards the change. If they still want it, it is their computer; make the change they asked for.

`manifest.json` contains a display name, a Semantic Versioning package version, and the theme's appearance declaration. Use `light dark` for a theme that follows Television's appearance preference, `light` for a fixed-light theme, or `dark` for a fixed-dark theme. It can also record the Television app version used for deliberate authoring or re-authoring:

```json
{
  "name": "Paperlike",
  "version": "1.0.0",
  "colorScheme": "light dark",
  "authoredForAppVersion": "<app-version>"
}
```

Read the exact release `version` from `tv status` when targeting a running server. Copy that exact release version unchanged into `authoredForAppVersion`. A missing version or the `0.0.0` development sentinel does not identify a release, so omit the metadata. Set `authoredForAppVersion` when that command establishes the target app version and preserve it during unrelated maintenance. The package `version` remains independent from this advisory authoring context. Television keeps a valid authored-against version in registry data but does not use it for compatibility or selection.

`README.md` is the durable authoring record. Record the user's visual intent, requirements, references, reasons for non-obvious decisions, selector-rule purposes, asset provenance, and maintenance context that CSS alone cannot preserve. For each script, record why it is needed, the effects it creates, and its expected resource cost. Never discard or wholesale-overwrite an existing README. When maintaining a theme you authored, read it first and make targeted updates that preserve useful context. Keep the README and stylesheet consistent for the next maintainer.

Television accepts the README beside the runtime files but does not expose it through the public theme route. Selector-level CSS also carries a concise purpose comment wherever the target alone does not explain why the rule exists.

### Bundled theme upgrades

Television may occasionally upgrade an installed bundled theme to deliver important Television fixes. Before replacing it, Television copies its current folder to a hidden timestamped backup beside the theme.

## Styling a partial overlay

`theme.css` is a partial overlay. State only the values and rules the design needs; untouched behavior continues to come from Television's foundation and application styles.

Override documented tokens at `:root` wherever they express the intended change, including changes to a single component. Use CSS selectors against existing markup only when tokens are insufficient. Keep those rules narrowly scoped, preserve interaction states, and add a purpose comment. Styling existing markup with CSS does not require changing the DOM.

The catalog contains foundation tokens, used by the app and live canonical artifacts, and application tokens, used only by the app. Use only documented theme tokens. Choose the token controlling the intended treatment; changing a value used by many other tokens affects all of them.

Theme CSS loads last and overrides foundation defaults with ordinary selectors; application rules can still win if their selectors are more specific.

Put cross-appearance statements at `:root`. A root statement applies in both appearances and beats foundation mode tables. Put appearance-specific differences under `[data-theme="light"]` and `[data-theme="dark"]`, after the theme's root statements, so equal-specificity mode statements win by source order. Only add mode-specific values when the design needs them.

### Start with the four semantic colors

Four tokens establish the basic readable scheme from which surfaces, controls and supporting copy take their colors:

- `--color-surface` — the main document and component ground;
- `--color-surface-muted` — the neighbouring ground used by the sidebar, code blocks and other quieter surfaces;
- `--color-text` — ordinary text and controls; and
- `--color-text-muted` — supporting text, labels and placeholders.

State all four together for each look the theme defines. A theme with Light and Dark variants puts one set under each mode selector. A fixed Dark-only or Light-only look puts one set at `:root`, uses no mode blocks, and declares the matching fixed value in the manifest's `colorScheme`. To customize only one appearance of an adaptive theme while leaving the other on foundation defaults, declare `light dark` and put one set under only that mode selector instead.

These four tokens are enough to replace the main surfaces and text, but they do not change every color in the interface. The following overrides are optional and keep their foundation defaults when omitted:

- Set `--accent` to change primary actions, selection and the focus ring. Otherwise they retain the default blue accent.
- Set `--app-wallpaper` at the app-scoped root to choose the application ground. Otherwise the wallpaper retains the foundation value rather than following `--color-surface` automatically.
- Set any of `--red`, `--orange`, `--yellow`, `--green`, `--cyan`, `--blue`, `--purple`, or `--pink` to change that color family. Each base color regenerates its complete scale, including status, data and tint uses that point to the scale.

### Token catalog

```css
/* Foundation tokens — fonts */
/* Hind — the committed variable cut, weights 300–700. */
@font-face {
  font-family: "Hind";
  src: url(../fonts/Hind-Variable.woff2) format("woff2-variations");
  font-weight: 300 700;
  font-display: block;
}

:where(:root) {  /* The faces above, named; weights as material plus the body role. */
  --font-sans: "Hind", -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
  --font-mono: ui-monospace, "SF Mono", SFMono-Regular, Menlo, monospace;
  --font-weight-normal: 400;
  --font-weight-medium: 500;
  --font-weight-semibold: 600;
}

/* Foundation tokens — colors */
/* Palette seeds and derived scales adapted from Tailwind CSS.
   https://github.com/tailwindlabs/tailwindcss
   Copyright (c) Tailwind Labs, Inc.
   MIT License; see THIRD-PARTY-NOTICES.txt. */

/* Both tables sit at zero specificity — ambient styling is the weakest
   thing in the cascade, tokens included. A theme states a token with a
   plain `:root` and out-cascades both tables at once — one statement, both
   modes — or writes its own `[data-theme="dark"]` block to retune dark
   alone. Between the tables themselves, source order decides: the dark
   table follows the light one, so it wins where the attribute matches. */
:where(:root) {
  /* Complete decorative paints. The consuming pseudo sets 1em to its radius. */
  --panel-edge-highlight: linear-gradient(to bottom,
    rgb(255 255 255 / .36) 0 .5px,
    rgb(255 255 255 / .28) .5px 1px,
    rgb(255 255 255 / .20) 1em);
  --panel-edge-shadow: 0 0 0 .5px rgb(0 0 0 / .205);

  /* Panels: popovers, menus, selects and dialogs. */
  --panel-background: var(--color-surface);
  --panel-text-color: var(--color-text);
  --panel-border-color: var(--color-border);
  --panel-border: none;

  /* Options: menu items and select options. */
  --option-background-highlighted: color-mix(in oklch, transparent, var(--option-background-active) var(--hover-mix));
  --option-background-active: var(--tint-active);

  /* Checked checklist marker fill and border. */
  --checkbox-color: var(--color-primary);

  /* Controls share these defaults in app and artifact documents. */
  --control-text-color: var(--color-text);
  --control-border-color: var(--color-border);
  --control-border-width: 1px;
  --control-border: var(--control-border-width) solid var(--control-border-color);
  --input-placeholder-text-color: var(--color-text-muted);

  /* Slots — the theme's raw material, each stated at its own 500 step: the
     hue's mid, the brand weight. A theme restates any of them and the scale
     below it regenerates. The neutral is genuinely neutral; give it a chroma
     and hue and the whole grey world takes the undertone. */
  --neutral: oklch(0.556 0 0);
  --red: oklch(0.637 0.237 25.331);
  --orange: oklch(0.705 0.213 47.604);
  --yellow: oklch(0.852 0.199 91.936);
  --green: oklch(0.723 0.219 149.579);
  --cyan: oklch(0.715 0.143 215.221);
  --blue: oklch(0.623 0.214 259.815);
  --purple: oklch(0.627 0.265 303.9);
  --pink: oklch(0.656 0.241 354.308);
  /* The one tunable identity — any color a theme or picker supplies; used
     raw as the accent fill, and everything accent-flavored derives from it. */
  --accent: var(--blue);

  /* Scales. Every step is generated around its slot: pale steps climb a
     fraction of the headroom from the slot's lightness to white, deep steps
     climb toward black, and every step keeps a fraction of the slot's chroma.
     The numbers sit inline, per scale, per step — tuning is editing the line.
     Cluster scales carry the curve measured off the hand-tuned red ramp;
     yellow carries its own numbers, including chroma fractions above 1 and
     per-step hue rotation (dark yellow at constant hue is olive).

     The neutral scale is a lightness ladder with chroma carried unchanged:
     dose arithmetic cannot hold an undertone at the pale end, and the
     interface's contrast skeleton wants stated lightnesses. */
  --neutral-50: oklch(from var(--neutral) 0.985 c h);
  --neutral-100: oklch(from var(--neutral) 0.97 c h);
  --neutral-200: oklch(from var(--neutral) 0.922 c h);
  --neutral-300: oklch(from var(--neutral) 0.87 c h);
  --neutral-400: oklch(from var(--neutral) 0.708 c h);
  --neutral-500: var(--neutral);
  --neutral-600: oklch(from var(--neutral) 0.439 c h);
  --neutral-700: oklch(from var(--neutral) 0.371 c h);
  --neutral-800: oklch(from var(--neutral) 0.269 c h);
  --neutral-900: oklch(from var(--neutral) 0.205 c h);
  --neutral-950: oklch(from var(--neutral) 0.145 c h);

  --red-50: oklch(from var(--red) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --red-100: oklch(from var(--red) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --red-200: oklch(from var(--red) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --red-300: oklch(from var(--red) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --red-400: oklch(from var(--red) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --red-500: var(--red);
  --red-600: oklch(from var(--red) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --red-700: oklch(from var(--red) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --red-800: oklch(from var(--red) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --red-900: oklch(from var(--red) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --red-950: oklch(from var(--red) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --orange-50: oklch(from var(--orange) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --orange-100: oklch(from var(--orange) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --orange-200: oklch(from var(--orange) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --orange-300: oklch(from var(--orange) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --orange-400: oklch(from var(--orange) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --orange-500: var(--orange);
  --orange-600: oklch(from var(--orange) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --orange-700: oklch(from var(--orange) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --orange-800: oklch(from var(--orange) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --orange-900: oklch(from var(--orange) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --orange-950: oklch(from var(--orange) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --yellow-50: oklch(from var(--yellow) calc(l + (1 - l) * 0.937) calc(c * 0.141) calc(h + 10));
  --yellow-100: oklch(from var(--yellow) calc(l + (1 - l) * 0.868) calc(c * 0.386) calc(h + 11));
  --yellow-200: oklch(from var(--yellow) calc(l + (1 - l) * 0.732) calc(c * 0.701) calc(h + 10));
  --yellow-300: oklch(from var(--yellow) calc(l + (1 - l) * 0.537) calc(c * 0.989) calc(h + 6));
  --yellow-400: oklch(from var(--yellow) calc(l + (1 - l) * 0.278) calc(c * 1.082) calc(h + 3));
  --yellow-500: var(--yellow);
  --yellow-600: oklch(from var(--yellow) calc(l * (1 - 0.08)) calc(c * 0.95) calc(h - 6));
  --yellow-700: oklch(from var(--yellow) calc(l * (1 - 0.19)) calc(c * 0.8) calc(h - 14));
  --yellow-800: oklch(from var(--yellow) calc(l * (1 - 0.33)) calc(c * 0.65) calc(h - 26));
  --yellow-900: oklch(from var(--yellow) calc(l * (1 - 0.46)) calc(c * 0.52) calc(h - 32));
  --yellow-950: oklch(from var(--yellow) calc(l * (1 - 0.64)) calc(c * 0.359) calc(h - 38));

  --green-50: oklch(from var(--green) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --green-100: oklch(from var(--green) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --green-200: oklch(from var(--green) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --green-300: oklch(from var(--green) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --green-400: oklch(from var(--green) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --green-500: var(--green);
  --green-600: oklch(from var(--green) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --green-700: oklch(from var(--green) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --green-800: oklch(from var(--green) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --green-900: oklch(from var(--green) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --green-950: oklch(from var(--green) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --cyan-50: oklch(from var(--cyan) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --cyan-100: oklch(from var(--cyan) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --cyan-200: oklch(from var(--cyan) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --cyan-300: oklch(from var(--cyan) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --cyan-400: oklch(from var(--cyan) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --cyan-500: var(--cyan);
  --cyan-600: oklch(from var(--cyan) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --cyan-700: oklch(from var(--cyan) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --cyan-800: oklch(from var(--cyan) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --cyan-900: oklch(from var(--cyan) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --cyan-950: oklch(from var(--cyan) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --blue-50: oklch(from var(--blue) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --blue-100: oklch(from var(--blue) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --blue-200: oklch(from var(--blue) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --blue-300: oklch(from var(--blue) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --blue-400: oklch(from var(--blue) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --blue-500: var(--blue);
  --blue-600: oklch(from var(--blue) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --blue-700: oklch(from var(--blue) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --blue-800: oklch(from var(--blue) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --blue-900: oklch(from var(--blue) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --blue-950: oklch(from var(--blue) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --purple-50: oklch(from var(--purple) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --purple-100: oklch(from var(--purple) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --purple-200: oklch(from var(--purple) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --purple-300: oklch(from var(--purple) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --purple-400: oklch(from var(--purple) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --purple-500: var(--purple);
  --purple-600: oklch(from var(--purple) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --purple-700: oklch(from var(--purple) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --purple-800: oklch(from var(--purple) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --purple-900: oklch(from var(--purple) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --purple-950: oklch(from var(--purple) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --pink-50: oklch(from var(--pink) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --pink-100: oklch(from var(--pink) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --pink-200: oklch(from var(--pink) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --pink-300: oklch(from var(--pink) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --pink-400: oklch(from var(--pink) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --pink-500: var(--pink);
  --pink-600: oklch(from var(--pink) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --pink-700: oklch(from var(--pink) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --pink-800: oklch(from var(--pink) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --pink-900: oklch(from var(--pink) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --pink-950: oklch(from var(--pink) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  --accent-50: oklch(from var(--accent) calc(l + (1 - l) * 0.92) calc(c * 0.055) h);
  --accent-100: oklch(from var(--accent) calc(l + (1 - l) * 0.824) calc(c * 0.135) h);
  --accent-200: oklch(from var(--accent) calc(l + (1 - l) * 0.683) calc(c * 0.262) h);
  --accent-300: oklch(from var(--accent) calc(l + (1 - l) * 0.471) calc(c * 0.481) h);
  --accent-400: oklch(from var(--accent) calc(l + (1 - l) * 0.185) calc(c * 0.806) h);
  --accent-500: var(--accent);
  --accent-600: oklch(from var(--accent) calc(l * (1 - 0.094)) calc(c * 1.034) h);
  --accent-700: oklch(from var(--accent) calc(l * (1 - 0.207)) calc(c * 0.899) h);
  --accent-800: oklch(from var(--accent) calc(l * (1 - 0.303)) calc(c * 0.747) h);
  --accent-900: oklch(from var(--accent) calc(l * (1 - 0.378)) calc(c * 0.595) h);
  --accent-950: oklch(from var(--accent) calc(l * (1 - 0.595)) calc(c * 0.435) h);

  /* Alpha scales — each color slot at a ladder of opacities, compositing on
     whatever is beneath: restate the slot and its alphas follow. Used for
     tinted backgrounds and translucent content. */
  --neutral-alpha-5: oklch(from var(--neutral) l c h / var(--alpha-5)); --neutral-alpha-10: oklch(from var(--neutral) l c h / var(--alpha-10)); --neutral-alpha-15: oklch(from var(--neutral) l c h / var(--alpha-15));
  --neutral-alpha-25: oklch(from var(--neutral) l c h / var(--alpha-25)); --neutral-alpha-50: oklch(from var(--neutral) l c h / var(--alpha-50)); --neutral-alpha-75: oklch(from var(--neutral) l c h / var(--alpha-75));
  --red-alpha-5: oklch(from var(--red) l c h / var(--alpha-5)); --red-alpha-10: oklch(from var(--red) l c h / var(--alpha-10)); --red-alpha-15: oklch(from var(--red) l c h / var(--alpha-15));
  --red-alpha-25: oklch(from var(--red) l c h / var(--alpha-25)); --red-alpha-50: oklch(from var(--red) l c h / var(--alpha-50)); --red-alpha-75: oklch(from var(--red) l c h / var(--alpha-75));
  --orange-alpha-5: oklch(from var(--orange) l c h / var(--alpha-5)); --orange-alpha-10: oklch(from var(--orange) l c h / var(--alpha-10)); --orange-alpha-15: oklch(from var(--orange) l c h / var(--alpha-15));
  --orange-alpha-25: oklch(from var(--orange) l c h / var(--alpha-25)); --orange-alpha-50: oklch(from var(--orange) l c h / var(--alpha-50)); --orange-alpha-75: oklch(from var(--orange) l c h / var(--alpha-75));
  --yellow-alpha-5: oklch(from var(--yellow) l c h / var(--alpha-5)); --yellow-alpha-10: oklch(from var(--yellow) l c h / var(--alpha-10)); --yellow-alpha-15: oklch(from var(--yellow) l c h / var(--alpha-15));
  --yellow-alpha-25: oklch(from var(--yellow) l c h / var(--alpha-25)); --yellow-alpha-50: oklch(from var(--yellow) l c h / var(--alpha-50)); --yellow-alpha-75: oklch(from var(--yellow) l c h / var(--alpha-75));
  --green-alpha-5: oklch(from var(--green) l c h / var(--alpha-5)); --green-alpha-10: oklch(from var(--green) l c h / var(--alpha-10)); --green-alpha-15: oklch(from var(--green) l c h / var(--alpha-15));
  --green-alpha-25: oklch(from var(--green) l c h / var(--alpha-25)); --green-alpha-50: oklch(from var(--green) l c h / var(--alpha-50)); --green-alpha-75: oklch(from var(--green) l c h / var(--alpha-75));
  --cyan-alpha-5: oklch(from var(--cyan) l c h / var(--alpha-5)); --cyan-alpha-10: oklch(from var(--cyan) l c h / var(--alpha-10)); --cyan-alpha-15: oklch(from var(--cyan) l c h / var(--alpha-15));
  --cyan-alpha-25: oklch(from var(--cyan) l c h / var(--alpha-25)); --cyan-alpha-50: oklch(from var(--cyan) l c h / var(--alpha-50)); --cyan-alpha-75: oklch(from var(--cyan) l c h / var(--alpha-75));
  --blue-alpha-5: oklch(from var(--blue) l c h / var(--alpha-5)); --blue-alpha-10: oklch(from var(--blue) l c h / var(--alpha-10)); --blue-alpha-15: oklch(from var(--blue) l c h / var(--alpha-15));
  --blue-alpha-25: oklch(from var(--blue) l c h / var(--alpha-25)); --blue-alpha-50: oklch(from var(--blue) l c h / var(--alpha-50)); --blue-alpha-75: oklch(from var(--blue) l c h / var(--alpha-75));
  --purple-alpha-5: oklch(from var(--purple) l c h / var(--alpha-5)); --purple-alpha-10: oklch(from var(--purple) l c h / var(--alpha-10)); --purple-alpha-15: oklch(from var(--purple) l c h / var(--alpha-15));
  --purple-alpha-25: oklch(from var(--purple) l c h / var(--alpha-25)); --purple-alpha-50: oklch(from var(--purple) l c h / var(--alpha-50)); --purple-alpha-75: oklch(from var(--purple) l c h / var(--alpha-75));
  --pink-alpha-5: oklch(from var(--pink) l c h / var(--alpha-5)); --pink-alpha-10: oklch(from var(--pink) l c h / var(--alpha-10)); --pink-alpha-15: oklch(from var(--pink) l c h / var(--alpha-15));
  --pink-alpha-25: oklch(from var(--pink) l c h / var(--alpha-25)); --pink-alpha-50: oklch(from var(--pink) l c h / var(--alpha-50)); --pink-alpha-75: oklch(from var(--pink) l c h / var(--alpha-75));
  --accent-alpha-5: oklch(from var(--accent) l c h / var(--alpha-5)); --accent-alpha-10: oklch(from var(--accent) l c h / var(--alpha-10)); --accent-alpha-15: oklch(from var(--accent) l c h / var(--alpha-15));
  --accent-alpha-25: oklch(from var(--accent) l c h / var(--alpha-25)); --accent-alpha-50: oklch(from var(--accent) l c h / var(--alpha-50)); --accent-alpha-75: oklch(from var(--accent) l c h / var(--alpha-75));

  /* The alpha ladder the alpha scales read — nominal percent, values tunable
     within a few points of their names. */
  --alpha-3: 3%; --alpha-5: 5%; --alpha-10: 10%; --alpha-15: 15%; --alpha-20: 20%;
  --alpha-25: 25%; --alpha-40: 40%; --alpha-50: 50%; --alpha-75: 75%;

  /* Active strength and hover position are independent. Hover moves this
     fraction of the way from resting to the resolved active color. The
     default is halfway in both appearances. */
  --hover-mix: 50%;
  --alpha-active: 8%;

  /* Ghost controls use their text pigment at active strength. Hover blends
     transparent with that resolved endpoint, including theme overrides. */
  --tint-hover: color-mix(in oklch, transparent, var(--tint-active) var(--hover-mix));
  --tint-active: oklch(from currentColor l c h / var(--alpha-active));

  /* Stencil icons: a drawing from the glyph vocabulary, packaged as a mask
     for the places a rule must draw it — a pseudo-element can hold no
     element, so the checklist's done mark and the select's selected mark
     paint through this stencil instead of composing tv-icon. */
  --icon-check: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 12 12"><path d="M10.2 3.1 5.1 8.2 2.4 5.5" fill="none" stroke="black" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>');

  /* Neutral roles — the interface's jobs, pointed at the material. Sheets
     read roles and scales, never a value of their own. */
  /* Pure white, stated: the surface is a frame for user content, and
     white-backgrounded artifacts and images must composite seamlessly. */
  --color-surface: white;
  --color-surface-muted: var(--neutral-100);
  --color-text: var(--neutral-900);
  --color-text-muted: var(--neutral-500);
  /* Reversed type — the text knocked out of a solid fill. One token: fills
     share the readable band by construction (meanings sit at fill weight),
     and a variant whose fill is too light for reversed type wears
     --color-text instead. */
  --color-text-reversed: white;
  /* Borders are tints of the text role, not stated steps: a translucent
     edge always separates from whatever ground it sits on — an opaque
     step vanishes the day a region paints that same step. Dark appearance
     uses stronger tints to keep the edges visible. */
  --color-border: oklch(from var(--color-text) l c h / 12%);
  /* Translucent surface fills; backdrop blur is applied by the component. */
  --tint-surface: oklch(from var(--color-surface) l c h / var(--alpha-50));
  --tint-surface-muted: oklch(from var(--color-surface-muted) l c h / var(--alpha-50));
  /* A control is a small surface, lifted by its border. */
  --control-background: var(--color-surface);
  /* Situations — danger, alert, success, primary. Each is one pointer at
     the hue's 500; its tints and state fills derive from it, so repointing
     the situation moves everything that wears it. */
  --color-danger: var(--red-500);
  --tint-danger: oklch(from var(--color-danger) l c h / var(--alpha-10));
  --tint-danger-hover: color-mix(in oklch, transparent, var(--tint-danger-active) var(--hover-mix));
  --tint-danger-active: oklch(from var(--color-danger) l c h / var(--alpha-active));

  --color-alert: var(--yellow-500);
  --tint-alert: oklch(from var(--color-alert) l c h / var(--alpha-10));
  --tint-alert-hover: color-mix(in oklch, transparent, var(--tint-alert-active) var(--hover-mix));
  --tint-alert-active: oklch(from var(--color-alert) l c h / var(--alpha-active));

  --color-success: var(--green-500);
  --tint-success: oklch(from var(--color-success) l c h / var(--alpha-10));

  /* The primary action — the accent worn as a situation: the default
     button, the current sidebar row. Selection wears it too; a selected
     role returns the day selection and primary want different colors. */
  --color-primary: var(--accent-500);
  --tint-primary: oklch(from var(--color-primary) l c h / var(--alpha-10));
  --tint-primary-hover: color-mix(in oklch, transparent, var(--tint-primary-active) var(--hover-mix));
  --tint-primary-active: oklch(from var(--color-primary) l c h / var(--alpha-active));
  /* The text on the primary fill, computed against it: dark ink above the
     contrast flip, near-white below — so any accent, light gold included,
     keeps a legible label wherever the fill appears. */
  --color-primary-text: oklch(from var(--color-primary) clamp(0.2, (var(--contrast-flip) - l) * 1e6, 0.98) 0 h);
  /* The keyboard's ring: one stroke wherever focus lands — width and
     pigment here, the offset each control's own. */
  --outline-focus: 2px solid var(--color-primary);

  /* Filled active colors move toward black or white according to state-flip.
     Hover follows the resolved active endpoint. Ordinary buttons consume
     only resting and active fills; their hover appearance stays unchanged.
     Derived states accept colors. Supply explicit states for gradients. */
  --control-background-hover: color-mix(in oklch, var(--control-background), var(--control-background-active) var(--hover-mix));
  --control-background-active: color-mix(in oklch, var(--control-background), oklch(from var(--control-background) clamp(0, (var(--state-flip) - l) * 1e6, 1) 0 h) var(--alpha-active));
  --color-primary-hover: color-mix(in oklch, var(--color-primary), var(--color-primary-active) var(--hover-mix));
  --color-primary-active: color-mix(in oklch, var(--color-primary), oklch(from var(--color-primary) clamp(0, (var(--state-flip) - l) * 1e6, 1) 0 h) var(--alpha-active));
  --color-danger-hover: color-mix(in oklch, var(--color-danger), var(--color-danger-active) var(--hover-mix));
  --color-danger-active: color-mix(in oklch, var(--color-danger), oklch(from var(--color-danger) clamp(0, (var(--state-flip) - l) * 1e6, 1) 0 h) var(--alpha-active));
  --color-alert-hover: color-mix(in oklch, var(--color-alert), var(--color-alert-active) var(--hover-mix));
  --color-alert-active: color-mix(in oklch, var(--color-alert), oklch(from var(--color-alert) clamp(0, (var(--state-flip) - l) * 1e6, 1) 0 h) var(--alpha-active));

  /* A link follows its surrounding text unless a theme gives it an accent.
     currentColor, not inherit: it means the ambient ink in any property it
     lands in, and CSS-wide keywords substitute brittly through var(). */
  --color-link: currentColor;
  /* The dialog backdrop: near-black at the overlay opacity, dimming the
     page; deriving from the neutral keeps the theme's undertone. */
  --color-overlay: oklch(from var(--neutral-950) l c h / var(--alpha-40));

  /* Two thresholds read a fill's own lightness and pick between two
     outcomes. --state-flip steers the fill's motion in its states: a fill
     lighter than it darkens on hover and press, a darker one lightens —
     a genuinely dark fill has nowhere darker to go. --contrast-flip
     steers the ink on the fill: a fill lighter than it wears dark text,
     a darker one wears reversed type. Different questions flip at
     different points — a red of 0.6 lightness wants reversed text yet
     still darkens on press. */
  --state-flip: 0.35;
  --contrast-flip: 0.66;

  color-scheme: light;
}

/* The dark table changes the role pointers. Scales are not restated as
   scales; the hue slots take slightly lighter values (saturated mids vibrate
   on near-black), and every derived step follows. The neutral slot does not
   move — the roles flip which steps they take. */
:where([data-theme="dark"]) {
  --panel-edge-shadow: 0 0 0 .5px rgb(0 0 0 / .912);
  --alpha-active: 12%;
  --red: oklch(0.704 0.191 22.216);
  --orange: oklch(0.75 0.183 55.934);
  --yellow: oklch(0.879 0.169 91.5);
  --green: oklch(0.792 0.209 151.711);
  --cyan: oklch(0.789 0.154 211.53);
  --blue: oklch(0.707 0.165 254.624);
  --purple: oklch(0.714 0.203 305.504);
  --pink: oklch(0.718 0.202 349.761);

  --color-surface: var(--neutral-900);
  --color-surface-muted: var(--neutral-800);
  --color-text: var(--neutral-100);
  --color-text-muted: var(--neutral-400);
  --color-border: oklch(from var(--color-text) l c h / 18%);

  color-scheme: dark;
}

/* Foundation tokens — text sizes and line heights */
:where(:root) {
  /* Controls follow the standard UI text size by default. */
  --control-font-size: var(--text-md);

  /* Type — one seed; each size a stated multiple, written as its tuned pixel
     over the default base: retune --text-base and every size follows (at the
     root — the var chain resolves there, not per subtree); read the fraction
     and you see the pixels it lands on at 14. */
  --text-base: 14px;
  --text-sm: round(calc(var(--text-base) * 12 / 14), 1px);
  --text-md: var(--text-base);
  --text-lg: round(calc(var(--text-base) * 16 / 14), 1px);
  --text-xl: round(calc(var(--text-base) * 18 / 14), 1px);
  --text-2xl: round(calc(var(--text-base) * 20 / 14), 1px);
  --text-3xl: round(calc(var(--text-base) * 24 / 14), 1px);
  --text-4xl: round(calc(var(--text-base) * 30 / 14), 1px);

  /* The control lines: the line-height a control's label uses, and the one
     input its height derives from — the line, plus the space-2 padding step
     top and bottom, plus a 1px border each side, gives every control the
     same height: 18 + 4 + 2 = 24px (14 + 4 + 2 = 20px at the small step).
     Stated in pixels, not a ratio: a control's cell is tighter than prose,
     immune to the leading around it, and lands on even pixels in any font. */
  --line-control: 18px;
  --line-control-sm: 14px;
}

/* Foundation tokens — spacing and corners */
:where(:root) {
  /* Ordinary buttons and text fields share control padding. */
  --control-padding: var(--space-2) var(--space-16);

  /* Spacing — the thirteen legal distances. The ladder is a constraint, not a
     theming surface: names state their fixed pixel values, and an off-ladder
     length in a sheet is a visible smell. A density mode, if ever real,
     swaps the ladder wholesale rather than lying under these names. */
  --space-2: 2px;
  --space-3: 3px;
  --space-4: 4px;
  --space-6: 6px;
  --space-8: 8px;
  --space-10: 10px;
  --space-12: 12px;
  --space-16: 16px;
  --space-20: 20px;
  --space-24: 24px;
  --space-32: 32px;
  --space-48: 48px;
  --space-64: 64px;

  /* Radius roles — jobs, themable. A numbered ladder would be value-names a
     theme cannot restate without lying. */
  --control-radius: 6px; /* buttons, inputs, menu items, small insets */
  --radius-pill: 9999px; /* fully rounded ends at any element size */

  /* Shared panel corners. */
  --panel-radius: 8px;

  /* The clearance a placed panel keeps on every side — off the trigger and
     off every window edge ([[ui/foundation/popover/index.md]], Placement).
     The element reads it from computed style, so a theme override reaches
     the placement algorithm. Foundation, not app chrome: the popover family
     ships to artifact documents, so its clearance must travel with it. */
  --popover-distance: var(--space-4);
}

/* Foundation tokens — shadows */
:where(:root) {
  /* Reusable elevations, from a close contact to a broad floating shadow. */
  --shadow-sm: 0 1px 4px oklch(from var(--neutral-950) l c h / var(--alpha-15));
  --shadow-md: 0 4px 12px oklch(from var(--neutral-950) l c h / var(--alpha-15));
  --shadow-lg:
    0 8px 24px oklch(from var(--neutral-950) l c h / var(--alpha-10)),
    0 2px 8px oklch(from var(--neutral-950) l c h / var(--alpha-5));

  /* Broad window elevation for artifact frames. */
  --shadow-xl:
    0 24px 48px oklch(from var(--neutral-950) l c h / var(--alpha-15)),
    0 4px 12px oklch(from var(--neutral-950) l c h / 7.5%);

  /* Popovers, including menus and selects, sit closer than dialogs. */
  --popover-shadow: var(--shadow-md);
  --dialog-shadow: var(--shadow-lg);
}

/* Dark surfaces need stronger shadows with the same neutral pigment. */
:where([data-theme="dark"]) {
  --shadow-sm: 0 1px 4px oklch(from var(--neutral-950) l c h / var(--alpha-40));
  --shadow-md: 0 4px 12px oklch(from var(--neutral-950) l c h / var(--alpha-40));
  --shadow-lg:
    0 8px 24px oklch(from var(--neutral-950) l c h / var(--alpha-40)),
    0 2px 8px oklch(from var(--neutral-950) l c h / var(--alpha-25));
  --shadow-xl:
    0 24px 48px oklch(from var(--neutral-950) l c h / 30%),
    0 4px 12px oklch(from var(--neutral-950) l c h / 18.75%);
}

/* Foundation tokens — layers */
/* The app-level paint order ([[ui/foundation/index.md]], Layers): every
   z-index either places an element on one of these or lifts above siblings
   with a calculation on one. Zero specificity like every token table — a
   plain `:root` rule from a theme beats it. */
:where(:root) {
  /* The base plane the document lays out. Nothing is placed on it; lifts on
     the base plane calculate from it. */
  --layer-ground: 0;
  /* Floating panels opened from a surface: menus, the top-bar popovers. */
  --layer-panel: 100;
  /* The interruption: the dialog overlay and its backdrop. */
  --layer-overlay: 200;
}

/* Application tokens — grouped by component */
/* Application vocabulary — the component tokens of the application chrome,
   as distinct from the materials the other token sheets state. The
   application document loads this sheet on top of the foundation; an
   artifact document never receives it ([[ui/foundation/index.md]],
   Delivery). */
/* Resolve relative image URLs where the theme declares them, before the
   application stylesheet consumes the value through --app-wallpaper. */
@property --app-wallpaper-image {
  syntax: "<url> | none";
  inherits: true;
  initial-value: none;
}

:where(:root) {
  /* Sidebar shell. */
  --sidebar-background: var(--color-surface-muted);
  --sidebar-text-color: var(--color-text);
  --sidebar-shadow: none;
  --sidebar-border-color: var(--color-border);
  --sidebar-border: var(--panel-border);
  --sidebar-heading-text-color: var(--color-text-muted);
  --sidebar-heading-font-size: var(--text-md);

  /* Channel rows and selection. */
  --channel-background: transparent;
  --channel-text-color: var(--sidebar-text-color);
  --channel-background-hover: color-mix(in oklch, var(--channel-background), var(--channel-background-active) var(--hover-mix));
  --channel-background-active: color-mix(in oklch, var(--channel-background), oklch(from currentColor l c h / 1) var(--alpha-active));
  --channel-background-selected: var(--color-primary);
  --channel-text-color-selected: var(--color-primary-text);
  --channel-radius: var(--control-radius);

  /* Tab labels. */
  --tab-text-color: oklch(from var(--navbar-text-color) l c h / var(--alpha-75));
  --tab-text-color-selected: var(--color-text);
  --tab-font-size: var(--text-md);

  /* Artifact frame and title bar. */
  --artifact-frame-shadow: var(--shadow-xl);
  --frame-background: var(--color-surface);
  --frame-border-color: var(--color-border);
  --frame-border: var(--panel-border);
  --frame-titlebar-background: var(--color-surface-muted);
  --frame-titlebar-text-color: var(--color-text);
  --frame-titlebar-border-color: transparent;
  --frame-titlebar-border-width: 1px;
  --frame-titlebar-border: var(--frame-titlebar-border-width) solid var(--frame-titlebar-border-color);
  --frame-titlebar-padding: var(--space-4) var(--space-10) var(--space-6);
  --frame-titlebar-divider-color: var(--color-border);

  /* Controls displayed over wallpaper. Blur is a backdrop radius: content
     stays sharp. Background and border default to transparent. */
  --wallpaper-overlay-text-color: var(--color-text);
  --wallpaper-overlay-background: transparent;
  /* Wallpaper controls retain the ghost text-color reaction by default.
     Themes can set the complete active background; hover follows it. */
  --wallpaper-overlay-background-hover: color-mix(in oklch, var(--wallpaper-overlay-background), var(--wallpaper-overlay-background-active) var(--hover-mix));
  --wallpaper-overlay-background-active: color-mix(in oklch, var(--wallpaper-overlay-background), oklch(from currentColor l c h / 1) var(--alpha-active));
  --wallpaper-overlay-border: 1px solid transparent;
  --wallpaper-overlay-outline: none;
  --wallpaper-overlay-blur: 0px;

  /* Navbar and empty stage. */
  /* The one height of the app's top bars — the navbar and the sidebar
     titlebar — so controls across the boundary share a band. The value
     centres the macOS traffic lights (15px top inset, 14px buttons) with even
     air above and below the cluster. */
  --app-bar-height: 44px;
  /* Match the top inset of the standard 24px navbar controls. */
  --app-bar-padding-inline: 10px;
  /* The horizontal room an app bar reserves for the traffic-light cluster
     when it holds the window's top-left corner. Only the desktop shell has
     the cluster, and it marks the document root; in a browser the
     reservation is zero. */
  --traffic-light-x-reserve: 0px;
  --navbar-background: transparent;
  --navbar-text-color: var(--wallpaper-overlay-text-color);
  --navbar-border-color: transparent;
  --navbar-border-width: 1px;
  --navbar-border: var(--navbar-border-width) solid var(--navbar-border-color);

  /* The ground behind the stage: one background value, composed from an
     optional image URL over the light or dark neutral ground. The image token
     accepts url(...) or none; use the full wallpaper token for gradients or
     other complete background treatments. */
  --app-wallpaper-image: none;
  --app-wallpaper: var(--app-wallpaper-image) center / cover no-repeat, var(--neutral-200);

  /* The stage's dials ([[ui/app/stage/stage.frame]] consumes them): the gap
     between pages, the inward inset from the stage edges, a background page's
     presence, and the armed snap's outline. Declared here, at the root, so
     a theme's root override wins — a declaration on the component would
     shadow it. */
  --page-gap: var(--space-24);
  --page-inset: var(--space-8);
  --page-inactive-blur: 0px;
  --page-inactive-backdrop-blur: var(--wallpaper-overlay-blur);
  --page-inactive-opacity: 0.55;
  --page-inactive-scale: 0.94;
  --page-snap-preview-border-color: oklch(from currentColor l c h / var(--alpha-25));
  --page-snap-preview-background: transparent;
  --page-snap-preview-border-width: 3px;
  --page-snap-preview-border: var(--page-snap-preview-border-width) solid var(--page-snap-preview-border-color);
  --page-snap-preview-radius: 12px;

  /* The tab pill's own dials ([[ui/app/tab-strip/tab.frame]] and the strip
     consume them). */
  --tab-radius: var(--control-radius);
  --tab-gap: var(--space-4); /* between tabs in the strip */
  --tab-border: var(--wallpaper-overlay-border);
  /* Transparent at rest; pointer states use the shared control tints. */
  --tab-background: var(--wallpaper-overlay-background);
  --tab-background-hover: color-mix(in oklch, var(--tab-background), var(--tab-background-active) var(--hover-mix));
  --tab-background-active: var(--wallpaper-overlay-background-active);
  --tab-background-selected: var(--color-surface-muted);
  /* Reserve the edge in every state so theme borders do not shift tabs. */
  --tab-border-selected: 1px solid transparent;
  --tab-outline: var(--wallpaper-overlay-outline);
  --tab-outline-selected: none;

  /* The drag placeholder's ghost fill — the "a carried thing could land
     here" mark the tab strip and the sidebar share
     ([[ui/app/tab-strip/tab-placeholder.frame]],
     [[ui/app/sidebar/channel-placeholder.frame]]). */
  --drag-placeholder-background: oklch(from var(--color-text) l c h / var(--alpha-10));

  /* Dragged items lift above the smaller action pill. */
  --drag-item-shadow: var(--shadow-md);
  --drag-action-shadow: var(--shadow-sm);


  /* The shared top band supplies equal air above and below the tabs. */
  --navbar-gap: 0px;

  /* The artifact frame's radius ([[ui/app/artifact-frame/artifact-frame.frame]]);
     artifact documents also apply it by reference
     ([[ui/app/artifact-frame/index.md#^af-document-corners]]). */
  --frame-radius: 12px;
}

:where([data-theme="dark"]) {
  --app-wallpaper: var(--app-wallpaper-image) center / cover no-repeat, var(--neutral-950);
}

/* The desktop shell marks the document root; the traffic-light reservation
   exists only there. */
:root[data-platform="electron"] {
  /* The cluster ends at 73px (15px inset, three 14px buttons, two 8px gaps);
     the bar adds its outer padding to this reservation, leaving air
     before its first control. */
  --traffic-light-x-reserve: 72px;
}
```

## Application structure for this release

This reference describes the application structure shipped with the matching Television release. Selectors may change between releases. The same theme stylesheet loads in the app and supported artifacts: prefix app-only rules with `:root[data-television-document="app"]` to avoid matching similarly named artifact elements. Leave rules intended for both documents unprefixed. The outlines omit text, repeated entries, and runtime pairing IDs where those do not affect styling. The application fills trigger references and manages interactive state; a theme styles those states without changing them.

An artifact frame belongs to the application document. The document inside its iframe is separate: an app selector cannot reach across that boundary. The error-page section below describes standalone artifact documents and therefore does not use the app root prefix.

### Shell and theme surfaces

```html
<div id="app" tabindex="-1">
  <aside class="app-sidebar"><nav class="sidebar">…</nav></aside>
  <main class="app-main">
    <header class="top-bar">…</header>
    <section class="stage">…</section>
  </main>
</div>
<div id="foreground-overlay" inert aria-hidden="true"></div>
```

`#app` owns the application layout and suppresses ordinary text selection in its contents; readable regions can explicitly restore selection. `.app-sidebar` holds the channel list. The sidebar defaults to 260 pixels wide, resizes between 160 and 350 pixels, and remembers the chosen width per browser; it can also collapse entirely, the navbar then opening with an expand control and — while a channel is open — a channel-switcher popover, with the collapsed state remembered per browser. `.app-main` holds the wallpaper, navbar and stage. `.app-main > .top-bar` supplies the horizontal control inset; stage padding is separate. Preserve the shell width constraints so overflowing tabs and pages scroll inside their regions without widening the document. `#app:focus` suppresses a ring on the container itself. The wallpaper region establishes a backdrop boundary so overlay blur samples the wallpaper rather than adjacent content, while fixed popovers retain viewport positioning.

During collapse or expansion, `#app[data-sidebar-motion]` contains both resting layouts and one travelling control:

```html
<button class="sidebar-motion-toggle" variant="ghost" icon aria-label="Collapse sidebar">
  <span class="expanded-paint" aria-hidden="true"><tv-icon name="sidebar" size="sm"></tv-icon></span>
  <span class="collapsed-paint" aria-hidden="true"><tv-icon name="sidebar" size="sm"></tv-icon></span>
</button>
```

The button is a direct child of `#app`, alongside `.app-sidebar` and `.app-main`. Its two spans carry the sidebar and navbar treatments, clipped at the moving boundary. The resting collapse and expand controls are hidden during motion. Theme rules that change those controls should give the corresponding spans the same treatment. Preserve the application-controlled position, clipping and opacity; shell transitions and animations are suppressed during this motion so they cannot trail the prescribed pose. Artifact documents remain independent.

For the optional iframe surfaces and protected overlay stacking, see [Theme effects and scripts](#theme-effects-and-scripts).

### Channel sidebar

```html
<nav class="sidebar">
  <header class="sidebar-titlebar">
    <button class="channel-create" variant="ghost" icon aria-label="New channel" title="New channel">…</button>
    <span class="toolbar-separator" role="separator" aria-orientation="vertical"></span>
    <button class="sidebar-collapse" variant="ghost" icon aria-label="Collapse sidebar" title="Collapse sidebar">…</button>
  </header>
  <div class="sidebar-body">
    <div class="channel-list" role="listbox" aria-label="Channels">
      <div class="channel-group" role="group" aria-labelledby="channel-group-pinned">
        <div class="channel-group-label" id="channel-group-pinned">Pinned</div>
        <div class="channel-row">
          <div class="channel" role="option" aria-selected="true" tabindex="-1">…</div>
          <button class="channel-menu-trigger" icon variant="ghost" size="sm" tabindex="-1" aria-haspopup="menu">…</button>
        </div>
      </div>
      <!-- The Recent group uses channel-group-unpinned. -->
    </div>
  </div>
</nav>
```

`.sidebar::after` paints the light inner seam over the sidebar background, alongside its optional soft shadow. `.app-main::before` paints the adjoining fractional dark seam over the wallpaper. Both are pointer-transparent. The sidebar accepts an independent native border; the main region reserves the exterior seam track. `.sidebar-titlebar` reserves the window controls and drag area; the reservation applies in the desktop shell, which marks the document root `data-platform="electron"`. `.sidebar-body` scrolls both groups together; `.sidebar:has(.sidebar-body[continues-start]) .sidebar-titlebar` draws the top boundary while content extends above the viewport. `.channel-list` holds the groups and their spacing, composed here and inside the collapsed navbar's switcher popover. `.channel-group + .channel-group` separates the groups, and `.channel-group-label` styles their labels. `.channel-create` and `.sidebar-collapse` sit at the right of the titlebar, split by `.toolbar-separator`, and are excluded from window dragging.

The channel name and its menu trigger are siblings within the row. `.channel` owns the name box; `.channel[aria-selected="true"]` paints the selection. The selected row changes `.channel-menu-trigger` text color through `.channel-row:has(> .channel[aria-selected="true"])`. Shared foundation tints follow that color for hover, press and expanded states. A hover or an expanded trigger preserves the unselected row tint through `:where(.channel-row:hover, .channel-row:has(.channel-menu-trigger[aria-expanded="true"])) .channel:not([aria-selected="true"])`. Trigger visibility follows row hover, `:focus-visible`, selected state, and expanded state. Use the expanded trigger state when styling an open menu subject. Panels remain in their authored DOM position when opened.

Renaming replaces the name and menu trigger with `input.channel-rename` and `button.channel-rename-commit`. The field uses shared native-input styling; these selectors retain its placement and the commit treatment in the same row seat. `.channel-row:has(.channel-rename)` keeps its focus ring visible. A dragged row carries `.channel-row.dragged`; an unpinning row also carries `.unpinning` and contains `.channel-drag-action` with a `tv-icon`. `.channel-row.dragged .channel` removes the spare trigger inset. `.channel-drag-action tv-icon` sizes the action glyph; `.channel-placeholder` marks the drop slot.

Channel context menus use `tv-menu`, `tv-menu-item`, `hr`, and `tv-menu-item[intent="danger"]`. An open menu is a sibling of the channel name and trigger inside `.channel-row`; its trigger carries `aria-expanded="true"`. The same composition appears in the sidebar and the channel popover. These shared elements supply menu styling and item states. Deletion uses the shared dialog outline below.

### Navbar and tabs

```html
<header class="top-bar">
  <!-- The lead group renders while the channel sidebar is collapsed. -->
  <div class="top-bar-lead">
    <button class="sidebar-expand" variant="ghost" icon aria-label="Show sidebar" title="Show sidebar">…</button>
    <button class="channel-switcher" variant="ghost" target aria-haspopup="listbox"><span class="channel-switcher-name">…</span>…</button>
    <!-- The open channel popover stays inside the lead group. -->
    <tv-popover open class="channel-switcher-pop">
      <div class="channel-switcher-pop-body">
        <div class="channel-list" role="listbox" aria-label="Channels">…</div>
      </div>
      <footer class="channel-switcher-pop-footer">
        <button class="channel-create" variant="ghost" icon aria-label="New channel">…</button>
      </footer>
    </tv-popover>
  </div>
  <div class="tab-strip" role="tablist">
    <div class="tab" role="tab" aria-selected="true" tabindex="0">
      <span class="tab-label">…</span>
    </div>
  </div>
  <div class="top-bar-controls">…</div>
</header>
```

`.top-bar` centers `.top-bar > .tab-strip` while tabs fit and keeps `.top-bar-controls` at the trailing edge. With `.tab-strip[data-overflow]`, the strip scrolls within the remaining space. The layout retains an 8-pixel gap before the strip and 48 pixels of draggable ground before the trailing controls; tabs and their edge fades stay clear of both control groups. Both child bands exclude native window dragging; empty bar ground remains available for it. The navbar and sidebar titlebar share the application bar height, with their controls vertically centered.

`.top-bar-lead` appears only while the channel sidebar is collapsed: `.sidebar-expand` reopens it, and — only while a channel is open — `.channel-switcher` (with `.channel-switcher-name` truncating the label) opens `.channel-switcher-pop`, a `tv-popover` composing the channel list over a pinned `.channel-switcher-pop-footer` create bar, its scrolling body `.channel-switcher-pop-body`. While the lead group is present the leading flank refuses to shrink below it, so a long name pushes the strip off-centre rather than clipping.

The popover retains channel row spacing, type and shape, but assigns its colors locally from ordinary panel and option roles. Sidebar-specific color overrides therefore do not set its colors. `.channel-switcher-pop .channel[aria-selected="true"]` keeps primary selection colors. Unselected rows use a neutral highlight for hover, keyboard focus or an open context menu; `.channel-switcher-pop .channel:focus-visible` has no outline. While keyboard focus is visible, a stationary pointer does not highlight a second row. Preserve these distinctions when styling the popover.

All `.top-bar tv-popover` panels have a 600-pixel maximum height, reduced further when the viewport requires it. The channel popover keeps its create footer fixed while `.channel-switcher-pop-body` scrolls. The shared dragged-row, unpin action, placeholder and rename selectors above also apply here. A channel context menu is a child panel of the channel popover; opening it keeps the parent visible, and closing the parent closes its children.

Ordinary controls in both groups — `.top-bar :is(.top-bar-controls, .top-bar-lead) > button:not([intent])` — follow the navbar text token. Ordinary ghost buttons share the wallpaper-overlay background, border and blur; hover and pressed/expanded states replace the complete background. Buttons with an intent retain their semantic colours.

`.tab-strip` scrolls horizontally. An edge fades while more tabs remain past it. `.tab[data-item-edge-fade]` carries a mask on the individual tab, preserving its backdrop blur; the strip has no mask. `.tab` owns the pill; `.tab-label` truncates its text. `.tab[data-compression="hugging"]` retains intrinsic width, while `.tab[data-compression="capped"]` permits compression to the floor. `.tab:hover:not([aria-selected="true"])` and `.tab:active:not([aria-selected="true"])` style unselected interaction; `.tab[aria-selected="true"]` paints the current page cue. `.tab.dragged` raises the carried tab, and `span.tab-placeholder[aria-hidden="true"]` occupies its drop slot. Nonselected tabs carry `aria-selected="false"` and `tabindex="-1"`.

### Stage and pages

```html
<section class="stage">
  <div class="filmstrip">
    <div class="filmstrip-inner">
      <div class="page" selected><div class="artifact-frame">…</div></div>
      <div class="page"><div class="artifact-frame">…</div></div>
    </div>
  </div>
</section>
```

`.stage` provides the clipping and size-container boundary. `body > .stage` fills a standalone stage; the app composition uses the flex region instead. `.filmstrip` is the viewport and `.filmstrip-inner` arranges pages. `.filmstrip-inner::before` and `::after` provide the end room needed for centering. `.page` receives its stored dimensions, and `.page[full-screen]` takes the available page box. `.page > .artifact-frame` fills that width.

`.page:not([selected])` scales the background page and blurs the wallpaper behind it; its direct `.artifact-frame` child controls content opacity and content blur separately; `.page:has(~ .page[selected])` and `.page[selected] ~ .page` set the corresponding transform origins. `.page[selected]` keeps the selected page above neighbors during reordering. `.page:not([selected]) .artifact-frame` takes no pointer input. Reduced-motion styling removes page and artifact-frame transitions. Preserve selection, clipping and size behavior when changing the appearance.

An empty channel adds `.stage-empty`, containing an artifact `tv-icon` and a paragraph in a compact, rounded box centered over the stage. It shares the wallpaper-overlay background, border, outline and blur, and has no shadow. Its text uses `--wallpaper-overlay-text-color`. `.stage-empty p` styles its supporting line. During an armed fullscreen snap, `.snap-outline[aria-hidden="true"]` overlays the page box without taking input. The outline and the tab/channel placeholders express transient drop or resize state, not persisted selection.

### Artifact frame and its menu

```html
<div class="artifact-frame">
  <div class="artifact-frame-clip">
    <iframe title="…"></iframe>
    <footer class="artifact-title-bar">
      <tv-icon name="artifact"></tv-icon>
      <span class="artifact-title">…</span>
      <!-- Navigation controls appear together when either direction exists. -->
      <button class="artifact-back" icon variant="ghost" size="sm">…</button>
      <button class="artifact-forward" icon variant="ghost" size="sm">…</button>
      <span class="artifact-bar-divider" aria-hidden="true"></span>
      <button class="artifact-menu-trigger" icon variant="ghost" size="sm">…</button>
      <tv-menu>…</tv-menu>
    </footer>
  </div>
</div>
```

`.artifact-frame` owns the border, curve, surface and broad shadow. Its `::before` paints the sharp rim and its `::after` paints a pointer-transparent masked inner highlight over document and titlebar. Background pages suppress the broad shadow while retaining the exterior rim. `.artifact-frame-clip > :is(iframe, webview)` fills the document area through the one-pixel padding track. `.artifact-frame-clip` clips the document and titlebar while the outer frame lets both shadows extend outward. `.artifact-title-bar` paints the lower band, `.artifact-title-bar .artifact-title` takes the remaining width and truncates, and `.artifact-bar-divider` separates navigation from the menu trigger. The two direction buttons use `disabled` to show unavailable history. The document repeats the top clipping radius where required for composited iframe/webview rendering.

The artifact menu uses the same shared menu elements and placement as channel menus. Deletion uses the shared dialog outline below.

### Settings

```html
<button class="settings-trigger" id="settings-trigger" icon variant="ghost">…</button>
<tv-popover class="settings-popover" trigger="settings-trigger">
  <div class="settings-heading">Settings</div>
  <div class="settings-field">
    <label id="settings-appearance-label">…</label>
    <button id="settings-appearance" aria-labelledby="settings-appearance-label">…</button>
    <tv-select trigger="settings-appearance"><tv-option value="system" selected>…</tv-option>…</tv-select>
  </div>
  <div class="settings-field">
    <label id="settings-theme-label">…</label>
    <div class="settings-theme-control">
      <button id="settings-theme" aria-labelledby="settings-theme-label">…</button>
      <tv-select trigger="settings-theme">…</tv-select>
      <button icon variant="ghost" aria-label="Refresh themes">…</button>
    </div>
  </div>
</tv-popover>
```

`.settings-popover` owns the panel interior, `.settings-heading` the heading, and `.settings-field` the field groups. `.settings-theme-control` arranges the theme choice and refresh button. `.settings-field button[aria-haspopup="listbox"]` makes each choice trigger fill its field. The shared select supplies combobox semantics, caret, `tv-option[selected]`, and its open highlight; opening it leaves Settings open.

An executable active theme adds `.settings-javascript-consent`, with `.settings-javascript-disclosure` and `label.settings-javascript-toggle`. The label contains a native checkbox carrying `role="switch"` and `name="theme-javascript-consent"`. `.settings-javascript-toggle input`, `input::before`, `input:checked`, `input:checked::before`, and `input:focus-visible` define its track, knob, enabled state and focus ring. `.settings-status[role="status"]`, `.settings-failure[role="alert"]`, and `.settings-errors` show loading, failure and registry diagnostics; `.settings-errors p` and `.settings-errors strong` separate the message and folder emphasis.

### Skills, update notice and copy confirmation

```html
<button class="skill-trigger" id="skills-trigger" icon variant="ghost">…</button>
<tv-popover class="skill-popover" trigger="skills-trigger">
  <div class="skill-heading">…</div><p class="skill-intro">…</p>
  <div class="skill-grid">
    <article class="skill-card">
      <div class="skill-thumb"><img alt=""></div>
      <div class="skill-name">…</div><p class="skill-desc">…</p>
      <button class="copy-button" size="sm">…</button>
      <span class="copy-button-status" role="status" aria-live="polite"></span>
    </article>
  </div>
</tv-popover>
```

`.skill-popover`, `.skill-heading`, and `.skill-intro` define the panel framing. `.skill-grid` arranges the cards. `.skill-card`, `.skill-thumb`, `.skill-thumb img`, `.skill-name`, and `.skill-desc` own each card; `.skill-card button.copy-button` places its copy action at the bottom.

The update control is `button.update-bell#update-bell[icon][intent="alert"]` paired with `tv-popover.update-popover[manual][trigger="update-bell"][role="status"]`. `.update-popover` contains readable notice text and `.update-actions` with `button.update-later` and an optional primary copy action. The manual panel stays open until the composing surface closes it.

The reusable copy control is `button.copy-button[size="sm"][prompt]`, optionally carrying `intent`, containing `.copy-button-idle` and `.copy-button-done`. Both contain an icon and label. `.copy-button[copied] .copy-button-idle` hides the idle content without changing its occupied space; `.copy-button[copied] .copy-button-done` overlays the confirmation. `.copy-button-status` is a separate visually hidden live announcement. Preserve that status region and stable button size when restyling confirmation.

### Dialogs, connection states and upgrade gate

```html
<div class="dialog-overlay">
  <dialog open>
    <div class="dialog-content">
    <div class="dialog-alert" role="alertdialog">
      <h2>…</h2><p>…</p>
      <div class="dialog-actions"><button>Cancel</button><button intent="danger">…</button></div>
    </div>
    </div>
  </dialog>
</div>
```

The scrolling `.dialog-content` wrapper may contain ordinary content instead of an alert. Ambient selectors `:where(.dialog-overlay)`, `:where(dialog)`, and `:where(dialog:focus-visible)` define dimming, centering, panel chrome and container focus treatment. `:where(.dialog-alert)`, `:where(.dialog-alert h2)`, `:where(.dialog-alert p)`, and `:where(.dialog-actions)` define the shared confirmation interior. Keep the modal input boundary and focus behavior intact.

Connection and authorization states reuse the dialog with `.system-modal`: an icon, heading and optional supporting paragraph. `.system-modal h2` and `.system-modal p:not(.tv-error)` set their hierarchy. Connecting and disconnected states use a spinning `tv-icon`; an error adds `.system-modal .server-url`. Authorization uses `form.system-modal.auth-form`. The field uses shared native-input styling; `.auth-form .auth-token` and `.auth-form .auth-submit` stretch the field and submit control. Rejection adds `aria-invalid="true"` and associates the shared error paragraph through `aria-describedby`; the alert communicates the failure. The rejected state is:

```html
<form class="system-modal auth-form">
  <tv-icon name="locked" size="xl"></tv-icon>
  <h2>…</h2><p>…</p>
  <input class="auth-token" type="password" name="token" placeholder="…" aria-label="…" autofocus required aria-invalid="true" aria-describedby="auth-token-error">
  <p id="auth-token-error" class="tv-error" role="alert">…</p>
  <button class="auth-submit" intent="primary">…</button>
</form>
```

The ordinary authorization state omits the invalid attribute, error association and error paragraph.

Upgrade instructions use `.desktop-upgrade-gate > .dialog-overlay > dialog > .dialog-content > .upgrade-gate-body`. The gate fills the halted page; the body scrolls within the panel. `.upgrade-gate-body h1`, `.upgrade-gate-body p`, `.upgrade-gate-body pre`, and `.upgrade-gate-body :last-child` restore local reading rhythm and command formatting.

### Standalone artifact error documents

```html
<main class="artifact-error">
  <div class="artifact-error-body">
    <h1>…</h1><p>…</p>
    <section class="artifact-error-block"><p>…</p><p><code>…</code></p></section>
  </div>
</main>
```

`.artifact-error` paints the document ground; `.artifact-error-body` limits the reading column. `.artifact-error h1`, `.artifact-error p`, and `.artifact-error-block` arrange its text. A missing artifact can include a path chip. An unsupported URL can include a link or `.plain-address`, followed by install and launch command chips. `.artifact-error a`, `.artifact-error .plain-address`, and `.artifact-error code` wrap long content; code is selected whole for copying. Optional host-supplied content may carry `[hidden]`.

## Panels and surface edges

Popovers, menus, select option lists, dialogs, artifact frames and the sidebar divider consume `--panel-edge-highlight` (a complete gradient) and `--panel-edge-shadow` (a complete sharp shadow). Set both to `none` to disable the decoration. Broad shadows are independent: `--artifact-frame-shadow` defaults to `var(--shadow-xl)` and either accepts `none` without erasing the rim. `--panel-border` defaults to `none`; frame and sidebar borders inherit it and accept explicit overrides. Ordinary buttons, text fields, separators and keyboard focus retain their own borders and outlines.

## Interaction feedback

Where a control provides hover or pressed feedback, the change must be visible and its text or icon remain readable. Prefer increasing label contrast on filled controls when that gives visible feedback; otherwise use a readable direction that does. Transparent controls can reveal their shape with a wash. Appearance mode and text color alone do not determine the right direction.

Most control families derive active background from resting background. Ordinary and semantic filled controls move the resting color toward a light or dark pole chosen from its lightness. Wallpaper-overlay controls add a wash of their text color. Hover then mixes the resting and active colors using `--hover-mix`. A root-level theme override of a documented resting token keeps this automatic derivation unless the theme also supplies an active token. An explicit active background is used as supplied, including opacity; it is not tinted or flipped again. Active also styles expanded triggers; selection is separate.

Unselected tabs use the wallpaper-overlay treatment by default. For one coordinated treatment across tabs, ordinary navbar controls and the empty-stage message, set `--wallpaper-overlay-background` at the app-scoped root. Its active and hover states derive automatically, and tabs inherit the complete family. Set `--wallpaper-overlay-background-active` only when the derived active treatment needs an explicit replacement.

Use the tab tokens only when tabs must differ from the shared overlay treatment. In that case set both `--tab-background` and `--tab-background-active`; tab active defaults to the shared overlay active background and does not derive from a tab-specific resting background. Leave `--tab-background-hover` alone so Television computes hover between the two tab values. The same local-pair rule applies when a selector gives individual controls different colors: set resting and active tokens on that element because an active value inherited from an ancestor was derived there, before the local resting override. Do not replace the state selectors themselves. Set `--tab-background-selected` separately because selection is not an interaction state.

Complete-background tokens also accept gradients and images, which cannot be color-interpolated. Supply explicit hover and active backgrounds for those treatments.

## Image backgrounds

Themes may optionally include an image background. If included, follow the setup and readability guidance below.

### Adding an image

Put wallpaper images inside the theme package and reference them from the app-scoped root in `theme.css`:

```css
:root[data-television-document="app"] {
  --app-wallpaper-image: url(assets/day.jpg);
}

/* Optional: use a different image in dark appearance. */
:root[data-television-document="app"][data-theme="dark"] {
  --app-wallpaper-image: url(assets/night.jpg);
}
```

`--app-wallpaper-image` is a registered URL value: relative paths resolve against the declaring stylesheet. Use `none` to remove the image. The default wallpaper treatment centers and covers the available ground. Use `--app-wallpaper` for a complete background declaration when changing positioning, adding gradients or composing other layers.

### Readability over the image

Choose `--wallpaper-overlay-background` and `--wallpaper-overlay-text-color` together. Unselected tabs, ordinary navbar overlay controls and the empty-stage message share this treatment. Over a busy image, start with `--tint-surface-muted` or `--tint-surface` and `--color-text`. These fills use `--alpha-50`; add `--wallpaper-overlay-blur` separately if a frosted treatment helps. A dark translucent fill with light text can suit either appearance. Judge the visible result over the image: blur softens detail but does not ensure contrast. Increase the fill opacity or use a solid surface when necessary.

Check resting, hover, pressed, open-menu and selected states in both appearances, over bright and dark image regions and at different window sizes. Apply the [interaction feedback](#interaction-feedback) guidance over the actual backdrop. If the automatic feedback does not suit the image, set the complete active background, including opacity:

```css
:root[data-television-document="app"] {
  --wallpaper-overlay-background-active: oklch(27.9% 0.041 260.031 / 75%);
}
```

Hover follows this override automatically. Tabs inherit the overlay active background but derive hover from their own resting background. If setting an explicit `--wallpaper-overlay-background-hover`, also set `--tab-background-hover: var(--wallpaper-overlay-background-hover)` when tabs should share it. This is needed for gradient or image fills, where interpolation is unavailable. Keep keyboard focus visible. The empty-stage message uses only the resting treatment.

## Runtime and loading

Television publishes the active package at one stable `/theme/` path. Eligible package files are served byte for byte. Keep `@import` and `url(...)` references relative to `theme.css`; the browser resolves them beneath the stable active-package path. Do not construct a URL from the theme ID, and do not expect Television to rewrite CSS.

The application loads `/theme/theme.css` after its complete foundation and application surface. Artifacts that link a theme-capable live canonical version load the same entry after their complete canonical foundation. No Television-owned stylesheet content follows the active theme. Frozen canonical v1 remains its built, unthemed, light-only surface.

Saving any file in the active package tree uses the live-update path. The application and affected artifacts refresh: local HTML artifacts reload, the markdown editor preserves its editor and contents while refreshing canonical styling, built-in artifact error documents reload, and third-party URL artifacts remain loaded. This update fanout covers nested stylesheets, images, fonts, the manifest, the authoring README, and other package files whether or not the active stylesheet currently requests them. Television combines the registered manifest's `colorScheme` with the stored appearance preference. `light dark` follows the preference; `light` or `dark` fixes presentation to that value while preserving preference changes for a later adaptive theme or `None`. An effective appearance change updates app and artifact document state without reloading those documents, the entry stylesheet, or the main script. It recreates each enabled theme frame and reruns its entry script. A stored preference change under a fixed theme changes no presentation.

Registry refresh publishes package discovery and manifest metadata. This includes a changed `colorScheme`. Use it after adding or repairing a package or changing its manifest; `tv set-theme <theme-id>` performs that refresh before selecting an installed ID. Saving a manifest in the active package also follows the live-update path, but its registry record keeps the last scanned metadata until refresh or the next serving boot.

If the active package folder temporarily disappears, Television keeps its exact theme ID selected and shows foundation and canonical styling while watching for the same path. Restoring the folder reapplies the package without a registry refresh. Refreshing the registry while the folder is absent selects `None`, and restoring the folder after that does not select it again.

## Theme effects and scripts

Theme effects are optional visual additions, such as textures, tint overlays, or animated backgrounds. Use them when the requested design calls for them, following the token-first approach above:

1. Prefer CSS. The permanent `#foreground-overlay` spans the application viewport above the interface, accepts no pointer input, and is the preferred surface for tint, `backdrop-filter`, translucent imagery, texture, and scanline effects. Ordinary menus and popovers sit beneath it; native modal dialogs remain above it.
2. Use JavaScript in a sandboxed frame when CSS cannot produce the effect and application DOM access is unnecessary, for example for an animation driven by host-supplied pointer information. Put effects behind the interface in `iframe-background.js` or above the interface and CSS foreground in `iframe-overlay.js`.
3. Use main-page JavaScript only when the effect requires access to the main application document.

The layers, from back to front, are `#theme-iframe-background` (z-index `0`), `#app` (`1`), `#foreground-overlay` (`2147483646`), and `#theme-iframe-overlay` (`2147483647`). Theme CSS may set opacity, filters and other presentation on the three effect surfaces. Their fixed viewport positioning, protected stacking and `pointer-events: none` keep effects separate from application interaction. Television keeps each frame element's `color-scheme` and its document's declared scheme matched to the effective `data-theme`; that match is what keeps the frame transparent. Read `data-theme` from the frame document's root once at startup. Appearance changes recreate the frames and rerun their scripts, so scripts need no appearance listener. Never change `color-scheme` on the frame document's root: a mismatch with the frame element forces the frame opaque. Theme CSS cannot cross into frame documents.

### Entry declarations

Each JavaScript surface has an independent manifest declaration and matching root entry:

| Manifest declaration | Root entry | Runtime |
| --- | --- | --- |
| `"enableMainJS": true` | `main.js` | Main application document after exact-ID user consent |
| `"enableIframeBackgroundJS": true` | `iframe-background.js` | Sandboxed frame behind the application |
| `"enableIframeOverlayJS": true` | `iframe-overlay.js` | Sandboxed frame above the application and CSS foreground |

For example, a package using all three entries declares:

```json
{
  "name": "Paperlike",
  "version": "1.0.0",
  "colorScheme": "light dark",
  "authoredForAppVersion": "<app-version>",
  "enableMainJS": true,
  "enableIframeBackgroundJS": true,
  "enableIframeOverlayJS": true
}
```

Each declaration is optional and must be a boolean when present. A present declaration, including `false`, requires its matching readable root file; only `true` enables execution. An entry without its declaration does not execute. The reserved `/theme/main.js`, `/theme/iframe-background.js`, and `/theme/iframe-overlay.js` URLs return a successful empty JavaScript response when their manifest or consent gates are closed, or the active package or entry is unavailable. Other script files are ordinary package assets. Television does not parse or validate JavaScript.

The declarations follow the server's registered manifest snapshot. Editing a declaration does not change its delivery gate until the theme registry refreshes. Saving any package file still triggers the active-package live-update path and uses the registered manifest snapshot.

All three entries run as classic scripts in browser and desktop application documents and never run in artifacts. During top-level execution, capture `document.currentScript.src` and resolve package assets relative to that URL. Ordinary document-relative URLs resolve against the entry's document.

### Sandboxed frames and pointer information

Iframe entries execute automatically without main-page consent. Each frame uses exactly `sandbox="allow-scripts"`, which gives its document an opaque origin. The script can draw and animate inside that document but cannot access the application DOM. The frames are inert, absent from sequential focus, and receive no direct pointer or keyboard input; pointer input continues to the application beneath them.

The host sends application pointer transitions and supported artifact pointer notifications to both current frames through `window` message events. Messages are not queued or replayed before a frame installs its listener. Accept messages only when `event.source === parent` and `data.type` is one of these closed names:

- `television-theme-pointer-move`
- `television-theme-pointer-down`
- `television-theme-pointer-up`
- `television-theme-pointer-cancel`
- `television-theme-pointer-click`

Every message contains only `type`, `clientX`, `clientY`, `button`, and `buttons`:

```js
{
  type,
  clientX,
  clientY,
  button,
  buttons
}
```

`clientX` and `clientY` are application-viewport CSS pixels. `buttons` is the standard post-transition bitmask. Move carries `button: -1` and the current buttons; down and up carry the changed button and the post-transition bitmask; click carries the clicked button and `buttons: 0`; cancel carries `button: -1, buttons: 0`. The host's opaque recipient requires `"*"` as the destination origin. This wildcard does not create a reverse command channel: Television ignores messages sent from a theme frame.

Keep each frame transparent wherever the application should remain visible. Destroying a frame ends its isolated runtime: the frame's removal ends its document, listeners, timers, and effects without reloading the application.

### Main-page trust boundary and lifecycle

A registered `enableMainJS: true` makes the root `main.js` eligible, and execution additionally requires the active theme's exact ID in the server's persisted consent set. Selecting or activating a theme does not grant consent. Before requesting consent, inspect and explain `main.js` and the effects it creates, then ask the user to grant consent in Settings. An authoring agent does not grant consent on the user's behalf. Settings shows the main-page JavaScript switch when the registered active theme declares `enableMainJS: true`. The user grants or withdraws consent there.

Consent is an explicit trust decision. Television loads eligible `main.js` as a classic script in each connected browser and desktop application document. It has ordinary access to that page's DOM, globals, browser storage, and network APIs. Electron grants no Node.js integration beyond capabilities the application renderer already exposes. Television's elements, globals, internal state, and other implementation details are not a stable JavaScript theme API and can change between releases. Consent persists for the exact theme ID until the user opts out, including while another theme is selected.

Keep main-page customization self-contained. Own top-level DOM rather than mutating Television-owned elements, and keep visual additions noninteractive. Excessive CPU or GPU use in any executable surface is a usability defect.

Every active-package refresh reruns an enabled and consented main script and destroys and recreates each enabled iframe, including refreshes caused by an unrelated package file. Main scripts therefore use repeat-safe ownership that recognizes and reuses or replaces their nodes, listeners, and timers. Frame replacement supplies cleanup for iframe effects. Consent-only changes preserve the frames unless the application reloads. Appearance changes recreate each enabled iframe and rerun its script; disconnect removes them. An active-package refresh, appearance change, or disconnect does not reload the application document, so successful main-page effects can remain until that document reloads.

When the main-script include is installed, opting out or selecting another theme or `None` automatically reloads the application document. The fresh document clears the script's DOM additions, listeners, timers, globals, and other document-lifetime effects before applying confirmed destination state. Persistent storage writes and completed network requests remain outside this reset boundary.

A load failure, syntax error, or uncaught exception in an iframe stays inside that frame. The same failure in `main.js` does not throw through Television's bundled application module, although successful main-page changes made before or around an error can still break the product.

## Appearance

Television combines the manifest's required `colorScheme` with the server-wide appearance preference. `light dark` follows that preference. `light` and `dark` fix the effective appearance to the declared value without rewriting the preference. Television dynamically maintains the resulting `data-theme="light"` or `data-theme="dark"` on the application root. Television-managed artifact documents install the same resolver with fixed `system`: a browser artifact resolves from the iframe's inherited scheme, while an Electron artifact resolves from Electron's native application preference. Each generated theme-frame document starts with the application's effective value and is recreated when that value changes.

Hinge all appearance-dependent theme styling on the root attribute. Do not use `light-dark()` or `prefers-color-scheme`; those mechanisms can follow browser or device state instead of Television's root marker. The foundation supplies Television's zero-specificity `color-scheme` value in every theme-capable app and artifact document, matching native controls and embedded contexts to `data-theme`. Theme CSS never declares `color-scheme`; the manifest is the theme's one appearance declaration.

A fixed theme puts its semantic colors and other token statements at `:root`, uses no mode blocks, and declares the matching `light` or `dark` value in its manifest. An adaptive theme declares `light dark`, states shared choices at `:root`, then puts only intended differences under `[data-theme="light"]` and `[data-theme="dark"]` after those shared statements. It can customize one mode and leave the other on foundation defaults.

Inspect light and dark effective appearance for an adaptive theme. For a fixed theme, inspect its one effective appearance under both a matching and an opposing stored preference, confirming that the presentation stays fixed. Mode-dependent foundation values that the theme does not override—including border opacity, active-state tint strength, and the four shadow tokens—follow the effective root marker.

## Theme selection

The user can choose a theme in the Settings UI. Agents activate one with `tv set-theme <theme-id>`, substituting the exact installed ID. Any capitalization of `none` selects no theme, which user-facing output labels `None`.

A successful command prints one of these transitions:

```text
Active theme changed from '<previous>' to '<new>'.
Active theme unchanged: '<selection>'.
Active theme: '<new>'.
```

The first two forms report the opening selection when it was available. The third confirms the new selection when the opening read was unavailable. Activation does not need a separate preliminary selection read; preserve prior-selection context when the command provides it.

## Authoring workflow

1. Gather the user's visual intent, references, palette and typography direction, and the application or artifact surfaces that matter.
2. Run `tv storage-path` to locate the themes directory and `tv status` to establish the target app version.
3. Choose an exact theme ID, inspect the target path, and apply the existing-folder safeguards above before writing.
4. Choose the least invasive visual surface, then write the manifest with its required `colorScheme`, entry stylesheet, README, any justified JavaScript entries, and relative assets as one package. Add purpose comments to narrow selector rules.
5. Activate the package with `tv set-theme <theme-id>`. Correct any manifest error the command reports.
6. When the package declares `main.js`, inspect and explain it, then ask the user to grant consent in Settings. For iframe entries, account for the sandbox, pointer-message contract, and frame replacement lifecycle.
7. Iterate on the files in its watched package tree. Refresh the theme registry to publish manifest changes.
8. Verify the application shell and one artifact using a theme-capable live canonical version. For an adaptive theme, inspect light and dark effective appearance. For a fixed theme, inspect matching and opposing stored preferences and confirm that presentation stays fixed. Check readability, asset loading, native controls, intended cross-document reach, and document continuity.
9. Leave the README, comments, stylesheet, script, manifest, and assets consistent for the next maintainer.

Visual verification can include screenshots when the environment can render both documents headlessly and interpret the results. Offer the user an optional review of the shell and one live-canonical artifact (four captures): light and dark effective appearance for an adaptive theme, or matching and opposing stored preferences for a fixed theme. Explain that it takes additional time. Wait for the user's consent before capturing them. Write captures only to a temporary location, review them, and delete every temporary capture after review. If the environment lacks either capability or the user declines, ask the user to inspect the same states.

## Clouds as a worked example

The installed Clouds package at `<storagePath>/themes/clouds/` is a locally available structural example of package shape, relative assets, application-only scoping, and purpose-specific shell rules. Its version may meet or exceed Television's minimum, and its files may contain user edits, so read it as a local example rather than a pristine template. Use the vocabulary and reference in this document as the authority when adapting the example.
