---
name: television
description: Mental model, when to create artifacts, how Television's bundled skills fit together, and where the admin guide lives. Read for channel, artifact, and CLI work, and before installing, upgrading, or reconfiguring Television.
---


# Television

Television is a persistent artifact channel for agents.

Load this skill when you need to create, update, inspect, focus, delete, or otherwise manage Television channels and artifacts.

Read the [theming guidance](./theming.md) when creating, revising, or bringing an installed theme up to date.

Re-read this skill only if it is not already in your context or you know it changed.

## When to use Television

Prefer Television when the result would be better as a persistent, scannable artifact than as an inline conversational reply. When a response would otherwise be long, structured, visual, research-heavy, or significantly clearer as HTML, markdown, or a table, prefer creating a Television artifact as the primary response.

Short, conversational replies belong in the agent's own output. Television is for results the user will want to scan, compare, revisit, or treat as a working surface.

Use judgment. Do not create an artifact for every response, but do prefer one when:

- the result is lengthy text or deep analysis
- the result is a table or other structured comparison
- the result is research output the user will likely revisit
- the result benefits from richer HTML layout, hierarchy, or widgets
- the user is likely to keep referring back to the result while continuing the conversation

Ground this decision in the user's likely experience. Ask yourself what will be easier for them to read, compare, revisit, or act on next.

When the artifact is the real answer, keep the final reply short. Tell the user what you created, what they will see, and where to find it rather than duplicating the full content in a conversational reply.

## Mental model

### Core entities

- A **channel** is a named viewer surface with a layout.
- An **artifact** is a registry record shown on exactly one channel.
- Channel layout is the source of truth for where an artifact appears.

That relationship matters:

- creating an artifact places it on a channel immediately
- reordering an artifact's tab page is a browser UI tab-drag gesture; the CLI does not expose layout mutation today
- deleting an artifact removes its registry record and its tab page from the channel
- moving the same underlying path or URL to another channel means deleting the old artifact and creating a new artifact with the same path or URL on the target channel

### Onboarding channels

Fresh installations receive a small set of bundled onboarding channels, each installed exactly once per storage directory. After installation they are ordinary channels: rename, edit, or delete them like any other, and Television never re-creates or overwrites what changed. You can tell one apart from a user-created channel by the `onboarding` marker containing its stable channel slug — visible in `tv list-channels` / `tv get-channel` output; user-created channels have no such field. Marked channels appear as ordinary unpinned channels and receive no special browser promotion. The marker changes nothing about how you manage the channel.

### Artifact shapes

Television artifacts point to one of three shapes:

- **Single file** — an absolute markdown or HTML file.
- **Directory** — a folder with `index.html` plus sibling assets.
- **URL** — an external `http(s)://` page or local web app URL.

Register files and directories with `tv create-path-artifact`; register URLs with `tv create-url-artifact`. See `artifact-workflow.md` for choosing a shape and authoring the underlying content.

### User-facing framing

Think in terms of what the user believes exists and where they expect to find it.

Questions to keep straight:

- should this result be markdown, HTML, or a URL pointer?
- where should the underlying file live?
- should it appear on the current channel, another existing channel, or a new channel?
- should the user see it immediately, or should it be prepared without moving their attention yet?

Those questions interact, but they are not the same question.

## Administration: install, upgrade, and server configuration

The deployed Television administrator guide lives at `https://television.run/install.md`. Fetch and follow it whenever the user asks for installation, an upgrade, an uninstall, or any administrative change to how Television is configured or deployed — network binding and listeners, auth and tokens, ports, the persistent daemon, Docker setups, the desktop client, or troubleshooting an unreachable or unhealthy server. This skill deliberately does not duplicate that material.

**Fetch the full raw content — never a summary.** Many agent web fetch tools auto-summarize pages. The guide's details are load-bearing, and a summarized version is insufficient to administer from. Retrieve the verbatim markdown, for example:

```bash
curl -fsS https://television.run/install.md
```

Read the entire document before acting. If your web fetch tool returned a summary, refetch with a method that returns the raw markdown.

## Telemetry awareness and control

Television collects anonymous, content-free telemetry by default to understand early usage and improve the product. Telemetry records predefined usage events and content-free classifications; it does not record artifact paths or URLs, artifact titles, channel names, theme names, file contents, or user-authored text.

If the user wants to opt out, you turn telemetry off for them by running `tv telemetry disable` against the running server (`tv telemetry enable` turns it back on); `tv status` reports the current telemetry state. The command needs a live server. Television also honors the standard `DO_NOT_TRACK` environment variable when it is set, but `tv telemetry disable` is the intended way to opt out. Install- and upgrade-time telemetry duties — the disclosure to give the user and the `--installed-by-agent` flag — are covered in the admin guide (see the Administration section above).

## Related skills

Television guidance is split across bundled skills.

- Use this skill for channels, focus, artifact decisions, and the `tv` CLI. Required means the knowledge is required, not that you must re-read it before every Television action.
- Install the bundled Television skills into the agent harness skills folder with `tv skills install <path>` (for example `~/.openclaw/skills`, `~/.hermes/skills`, or `~/.agents/skills`) or use `tv skills install -i` before artifact authoring work.
- For specialized HTML work, load the matching skill such as `tv-calendar` or `tv-table`.

`markdown editor UI recovery` remains out of scope for the current Television workflow.


# TV CLI capabilities

Use this document when you need to reason about what the `tv` CLI can do, which command family fits the user's request, and how focus and channel placement should be decided.

## Agent recovery surfaces

The `television` skill is the primary guidance surface for Television channels, focus, and artifact work. Keep that guidance available; re-read it only if it is not already in context or you know it changed.

Install bundled Television skills with either:

```bash
tv skills install ~/.openclaw/skills
tv skills install ~/.hermes/skills
tv skills install ~/.agents/skills
tv skills install -i
```

`tv help` is normal CLI help plus routing pointers.

## Command surface

Run:

```bash
tv help
tv help <command>
```

`tv help <command>` is the source of truth for per-command semantics that are not obvious from the name — focus decisions, cascading effects, JSON vs plain-text output, and distinctions between superficially similar commands (delete-artifact vs remove-channel, focus-channel vs focus-artifact). Read it before choosing a command when you are unsure.

Commands group into four intents:

- **Channel and display commands** — create, inspect, rename, remove, or switch channels, or change the active display theme (`create-channel`, `list-channels`, `get-channel`, `update-channel`, `remove-channel`, `focus-channel`, `focus-status`, `set-theme`).
- **Artifact creation commands** — register path or URL artifacts (`create-path-artifact`, `create-url-artifact`).
- **Artifact management commands** — inspect, retitle, repoint, focus, list, or delete existing artifacts (`delete-artifact`, `get-artifact`, `list-artifacts`, `update-artifact`, `focus-artifact`).
- **Server and environment commands** — operate on the Television server itself (`serve`, `status`, `stop`, `storage-path`, `telemetry`, `skills install`).

When the CLI rejects a command, follow the directive it prints rather than guessing flags.

### Server operation notes

`tv status` reports server health and, when supported, system service status. Workflow commands connect to `localhost:<port>` only; use `--port`, `TELEVISION_PORT`, or the default `32848` rather than `--server`. When targeting non-default storage, pass `--storage-path` or set `TELEVISION_STORAGE_PATH` so the CLI reads the matching token from `<storagePath>/state/token`.

Starting, installing, upgrading, or reconfiguring the server — `tv serve` flags, listeners, auth and tokens, ports, the persistent daemon (`--persist`), stopping and uninstalling (`tv stop`) — is administrator work covered by the deployed admin guide, not this skill. Before doing any of it, fetch the full raw guide as described in the Administration section of this skill; do not work from the command names alone or from a summary.

## Read vs mutate

Read commands print JSON. Workflow commands and most mutation commands print plain text. On success, `tv set-theme` prints one of these forms:

```text
Active theme changed from '<previous>' to '<new>'.
Active theme unchanged: '<selection>'.
Active theme: '<new>'.
```

The first two forms are available when the opening selection read succeeds. The third confirms activation when that read is unavailable. Theme labels preserve exact installed theme IDs and use `None` for no theme. `tv serve` startup output is human-readable connection text.

Use read commands when you need authoritative state for planning or verification. Use mutation commands when you are intentionally changing Television state.

## Focus model

Television separates state changes from focus. Choosing where an artifact lives is one decision; choosing whether the user's attention moves there is a separate one.

- **channel focus** is persistent: which channel the user is currently looking at
- **artifact focus** is transient: clients select the artifact's tab page, switching channels first when needed

There is a persisted focused channel.
There is not a persisted focused artifact.

Important consequence: creating something does not by itself answer whether the user should be taken to it now.

## Required explicit focus decisions

Create commands require an explicit focus decision.

- `tv create-channel` requires exactly one of `--focus-channel` or `--no-focus`
- `tv create-path-artifact` and `tv create-url-artifact` require exactly one of `--focus-artifact` or `--no-focus`

If you omit that decision, the CLI rejects the command. Dedicated `focus-channel` and `focus-artifact` commands can also move attention later as separate steps.

## Choosing the focus directive

Think about the user's current attention stream before deciding. Use theory of mind: what are they probably attending to right now? Do they expect an immediate reveal, or would moving their view feel jarring? What will they likely want to do next?

Rules of thumb:

- **focus now** when seeing the result immediately is part of successfully answering the request
- **`--no-focus`** when the user is likely to want the result available without breaking their current flow, or when the work should run in the background
- **direct `focus-channel` / `focus-artifact` commands** when placement and attention movement should happen as separate steps

When your action would not be visually obvious to the user — you used `--no-focus`, you placed something on a non-current channel, or you moved focus — tell them what you did, name the channel, and say what they should see. Otherwise the Television display may not change in a way they can interpret.

## Listening for focus intent in the user's language

The user's phrasing is a strong cue — not a deterministic rule, but a real signal worth listening for.

Phrases that usually indicate the user wants attention moved:

- "show me", "show me that", "let me see it", "let me review it"
- "switch to", "change to", "go to", "take me there", "open it"
- "put it on screen", "put it on my screen", "bring it up"
- references to the **active**, **current**, **showing**, or **visible** channel or artifact

Phrases that usually indicate the user wants the work to happen without disturbing their current view:

- "in the background", "while I'm doing X", "while I work on Y go and do Z"
- "set this up", "prepare it", "wire it in", "get it ready"
- "don't interrupt me", "leave my screen alone", "don't switch"

These are illustrative, not exhaustive. When the language is ambiguous or unusual, reason about the user's attention stream instead of pattern-matching keywords.

## Channel placement

Artifact creation commands require `--channel` because new artifacts need immediate channel membership. Reordering an artifact's tab page on its current channel is a browser UI tab-drag gesture; the CLI does not expose layout mutation today. To move the same underlying path or URL to a different channel, delete the old artifact and create a new artifact on the target channel with the same path or URL.

Think carefully about whether the user means:

- create something new on a channel
- reorder an artifact's tab page on its current channel in the browser UI
- delete an artifact from its channel
- recreate the same path or URL on a different channel

Those are different operations with different consequences. The per-command help text spells out which is which.

### Choosing the right channel

When deciding where an artifact should go:

- If the current channel is the right place and the user should see the result immediately, create the artifact there with `--focus-artifact`.
- If the current channel is the right place but the work should appear without interrupting their reading flow, create it with `--no-focus`.
- If the work belongs on a different existing channel, place it there. Then decide whether to focus or merely tell the user where it is.
- Sometimes a new channel is the right call.

### When to create a new channel

Reach for `tv create-channel` when:

- the request is meaningfully separate from the current channel's purpose
- the result should become its own durable workspace the user can return to
- mixing it into the current channel would make the user's mental model worse

Default to placing things on an existing channel unless one of those conditions is true. Spawning a new channel for every request fragments the user's workspace; spawning none ever forces unrelated content together.


# Artifact workflow

Read this document for Television artifact work: creating files, registering path or URL artifacts, browser-only tab-page reordering, deleting, and updating titles.

If you need bundled Television authoring skills installed first, copy them into the agent harness skills folder with `tv skills install <path>` (for example `~/.openclaw/skills`, `~/.hermes/skills`, or `~/.agents/skills`) or use `tv skills install -i`.

If you are authoring a specialized HTML artifact, read the matching skill after this one — for example `tv-calendar` or `tv-table`.

## Choosing the artifact kind

Television has three artifact shapes. Pick the one that fits the result:

- **Single file** — one self-contained markdown (`.md`) or HTML (`.html`) file on disk. Use when the artifact needs no dependent CSS, JS, images, or additional pages.
- **Directory** — a folder on disk containing `index.html` plus sibling assets (CSS, JS, images, additional pages). Use when the artifact needs richer structure: custom stylesheets, scripts, images, or multi-page navigation.
- **URL** — an external `http(s)://` web page or web app, or a locally running web server (e.g. `http://localhost:3000`). Use for remote pages, local web apps the agent does not manage, or any live web resource. Do not use URL artifacts for content Television should manage the lifecycle of — use a path artifact instead.

Single files and directories are both registered with `tv create-path-artifact`. URL artifacts use `tv create-url-artifact`.

Within path artifacts, choose the content shape:

- **HTML file or directory** — default for most artifacts. HTML gives Television its best rendering: styled layout, hierarchy, visual structure, and the canonical stylesheet. Use HTML whenever the result is a presentation, summary, dashboard, comparison, reference, or anything the user will primarily read rather than edit.
- **Markdown file** — use when the result is a document the user will want to edit directly: notes, drafts, working documents, or content that will be revised outside Television.

If you are unsure between HTML and markdown, default to HTML.

## Where to place new artifact files

Television registers pointers — the file lifecycle is yours, not Television's. Deleting an artifact removes the registry record, not the underlying file or folder, regardless of where it lives.

**Default for HTML (single file or directory): `~/.television/artifacts/`.** HTML artifacts are pure presentation — Television is where they live and get used, so the default folder is the right home. Put them there unless the user has told you otherwise. Create the directory if it does not exist.

**Markdown is different.** A markdown artifact is usually a user-owned document — a note, draft, write-up — that has a life outside Television. Putting one in `~/.television/artifacts/` is suspect: the user will likely want it alongside their other documents. For markdown, co-locate with the project, repo, notes folder, or workspace where a new document of that kind would naturally belong if the user had asked for one outside Television. If no such home is obvious from context, ask the user where it should go rather than dropping it in the default folder.

For HTML, put the file somewhere else only when the user has explicitly directed you to — either in this request, or via a durable instruction (project `AGENTS.md`/`CLAUDE.md`, a standing preference, an earlier "from now on…" in this session). Examples of explicit direction: "save it in the repo", "put it in my notes folder", "drop it in the agent workspace".

Do not co-locate HTML artifacts with project files, repo trees, notes folders, or agent workspaces on your own initiative — even when it seems natural. The default for HTML is `~/.television/artifacts/`.

### File and folder naming

Use durable, descriptive names that capture the artifact's subject specifically — favor `q3-revenue-by-region.html` over `report.html`. For HTML artifacts in the default folder, good names are how you'll find one again later among the others.

For directory artifacts the folder name is the primary identifier — files inside are typically generic (`index.html`, `styles.css`).

Always tell the user where you put the file.

## Accepted path targets

`tv create-path-artifact` accepts three path shapes:

- **Single markdown file** ending in `.md` or `.markdown`
- **Single HTML file** ending in `.htm` or `.html`
- **HTML directory** — a directory whose root contains `index.html` or `index.htm`, plus any sibling assets

The path must be absolute, existing, and readable by the server. A trailing separator is optional — the server checks the path on disk to decide whether it is a file or a directory.

## Authoring quality

Build artifacts that are durable, truthful, and maintainable by later agents.

Required standards:

- be faithful to source material
- do not invent missing facts to make the artifact look complete
- do not silently truncate a dataset and pretend it is comprehensive
- prefer truth over completeness when those goals conflict
- make limitations, sampling, gaps, and freshness visible when they matter
- avoid unnecessary layout or styling churn during simple refreshes

Anti-patterns to avoid:

- cursory or low-effort data collection
- fake completeness — padding to look thorough
- brittle one-off hacks that a later agent cannot reproduce
- hidden dependencies that are not documented next to the artifact

## Before creating artifacts

Before starting artifact creation, briefly tell the user what you are about to make. Artifact creation can be time-consuming; the user should be kept informed. Think about how to work expediently and avoid unnecessary extra steps without compromising the outcome.

## In-flight narration style

While a multi-step artifact workflow is running, narrate concisely so the user knows you are still working.

Required style:

- verbalize key actions and decisions as they happen
- keep updates short — a sentence per beat, not a paragraph
- prefer the user's framing over Television's internal machinery
- optimize for speed and token efficiency

Good examples:

- "Starting the artifact now."
- "Reviewing the draft and source material."
- "Writing the HTML and checking it in the browser."
- "Registering the artifact on your channel."
- "Done."

Avoid:

- multi-paragraph progress reports or long retrospective narration during execution
- verbose bullet lists for routine workflow steps (use bullets only when the user explicitly asks)
- workflow jargon ("calling create-path-artifact", "registering the path artifact") unless the user is debugging Television itself

## Markdown path artifacts

1. Write the markdown file.
2. Register it:

   ```bash
   tv create-path-artifact --channel "<channel-id>" --title "Artifact title" --path /absolute/path/to/file.md --focus-artifact
   ```

Rules:

- The file must already exist and be readable.
- Television's markdown editor reads and writes the pointed-to file.
- Deleting the artifact removes the registry record, not the markdown file.

## HTML path artifacts

HTML can be a single file or a directory bundle.

Single-file example:

```bash
tv create-path-artifact --channel "<channel-id>" --title "Artifact title" --path /absolute/path/to/report.html --focus-artifact
```

Directory example (trailing slash optional):

```bash
tv create-path-artifact --channel "<channel-id>" --title "Artifact title" --path /absolute/path/to/dashboard --focus-artifact
```

A directory artifact needs root `index.html` or `index.htm`. Keep sibling assets relative so they resolve through the artifact proxy:

```html
<link rel="stylesheet" href="./styles.css" />
<script type="module" src="./main.js"></script>
```

Use Television's canonical artifact stylesheet and record the app version whose canonical surface you authored against:

```html
<link rel="stylesheet" href="/canonical/v2/styles.css?authoredForAppVersion=<version>" />
```

Replace `<version>` with the target Television app version. For a running server, read the exact release `version` from `tv status`; when working in a Television checkout, read the exact version from the checkout root `package.json`. Copy that exact release version unchanged into `authoredForAppVersion`. A missing version or the `0.0.0` development sentinel does not identify a release, so omit the metadata.

`authoredForAppVersion` is advisory authoring context for a future agent. The server ignores it when serving the stylesheet, so it neither asserts compatibility nor controls whether the artifact loads. Set it when creating an artifact or deliberately re-authoring one against that app surface. Preserve an existing `authoredForAppVersion` value during unrelated maintenance. If you cannot establish the target app version, omit the query parameter; the canonical URL remains valid without it.

### Suggested HTML file set

For durable HTML artifacts, write nearby documentation so a future agent can maintain the work:

- `index.html` — rendered page
- `artifact.md` — purpose, user intent, data sources, rendering notes, update workflow, non-goals
- `memory.md` — maintenance log
- data source file when the artifact has non-trivial underlying data

For single-file artifacts, keep these files in the same directory. For directory artifacts, put `index.html` at the registered root and keep supporting files next to it unless the user's workspace convention says otherwise.

### Data before presentation

Before authoring the final HTML, think through the underlying data in a pure-data way.

Ask yourself:

- what facts exist?
- what structure do they have?
- what is missing?
- what separation between data and presentation would help the next agent?

Capture this reasoning in a supporting document or data file before the presentation work.

## Updating an artifact

To update markdown or HTML content, edit the pointed-to file or directory in place. Television watches supported path targets and refreshes connected clients.

To retitle an artifact:

```bash
tv update-artifact --id "<artifact-id>" --title "New title"
```

To repoint an artifact at different content (same kind only — a path artifact takes `--path`, a URL artifact takes `--url`):

```bash
tv update-artifact --id "<artifact-id>" --path /absolute/path/to/new-target
tv update-artifact --id "<artifact-id>" --url "https://example.com/next"
```

The new path follows the same rules as creation (file or indexed directory, trailing separator optional). Rendering follows the new pointer immediately, and for path artifacts the content watcher retargets with it. Prefer repointing over delete-and-recreate when the artifact should keep its identity and channel placement.

Reordering an artifact's tab page on its current channel is a browser UI tab-drag gesture; the CLI does not expose layout mutation today.

To move the same underlying path or URL to another channel, delete the existing artifact and create a new one on the target channel with the same `--path` or `--url`.

To delete the registry record and remove its tab page from the channel:

```bash
tv delete-artifact --id "<artifact-id>"
```

## URL artifacts

URL artifacts point at external `http(s)://` pages.

```bash
tv create-url-artifact --channel "<channel-id>" --title "Artifact title" --url https://example.com --no-focus
```

Rules:

- `--url` must be `http://` or `https://`.
- Electron displays the URL in a webview.
- Browser clients show a local unsupported placeholder for ordinary web URLs.
- Browser clients render Television artifact proxy URLs (`http://<host>:<port>/artifact/<id>/...`) inline and live-reload them when the producer's ETag changes.
- Television does not fetch or watch ordinary remote pages from the consumer server.

### Sharing Television artifacts by URL

To share a path artifact from one Television server to another, build a producer artifact proxy URL:

```text
http://<producer-host>:<port>/artifact/<artifact-id>/<basename>
```

**CRITICAL — the share URL carries NO token.** Do not append the server's main `tv` bearer token (or any `?token=`/`Authorization` value) to a share URL, and never include it when telling the recipient how to reach the artifact. The `/artifact/<id>/*` proxy is bearerless: the unguessable artifact id in the path *is* the capability. The recipient is authorized by holding the artifact-id URL, not by holding the producer's main token. Leaking the main token would hand over full read/write control of the entire producer server; the artifact id only grants read access to that one artifact's rendered content. A correct share URL is exactly the form above — host, port, `/artifact/<artifact-id>/`, basename — and nothing else.

First run `tv status` on the producer. Its JSON reports the server `bindAddresses` and `port`. Choose the host that the recipient can actually reach:

- Prefer a Tailscale CGNAT address (`100.64.0.0/10`) when one is present.
- Otherwise use a non-loopback bind address.
- Never emit a loopback host (`127.0.0.1` or `localhost`) for sharing; that points at the recipient's own machine.
- If the server is bound to `0.0.0.0`, determine the host's reachable IP by other means, then use that IP with the reported port.
- If you cannot determine an externally reachable IP, tell the user and explain that the server is only reporting loopback or wildcard bind information instead of inventing a localhost share URL.

For directory artifacts, use the directory proxy URL ending in `/artifact/<artifact-id>/`. For file artifacts, include the encoded basename. The recipient adds that URL with `tv create-url-artifact`; Television recognizes the `/artifact/<id>/...` shape, renders it inline, and reloads it from the producer when the producer content changes.

Markdown artifacts shared this way render as read-only HTML. Anyone with the artifact proxy URL can read the rendered artifact content, matching the HTML artifact capability model — and that read works with no token at all, because the artifact id alone authorizes the read.

There is no separate `share-artifact` command. To "share" a URL artifact, pass along the underlying URL and let the recipient create their own URL artifact.

If the browser placeholder for an ordinary non-Television URL is not sufficient, create a markdown or HTML path artifact that links to the page and summarizes what the user needs from it.

# HTML artifact style

Write a complete HTML document for the Television viewer. Load both canonical
v2 resources in the document head:

```html
<link rel="stylesheet" href="/canonical/v2/styles.css?authoredForAppVersion=<version>">
<script type="module" src="/canonical/v2/components.js"></script>
```

Replace `<version>` with the Television app version whose canonical surface you
are authoring against. The Artifact workflow section explains how to find that
version and when to set, preserve, or omit this advisory query parameter.

The stylesheet provides Television's reset, Hind font, public design tokens,
semantic document defaults, and public element styles. The script registers
the public custom elements. Add local CSS for the artifact's own layout,
density, hierarchy, and specialized presentation.

Lean on semantic HTML first. Put `text-display="prose"` on a body or section
to apply readable styling to its headings, paragraphs, lists, links, code
blocks, blockquotes, rules, images and tables. Rely on those defaults instead of recreating baseline
typography in each artifact.

## Page header

The artifact frame's title bar already names the artifact. Add a page header
when the document benefits from its own masthead, such as a more specific title
or a subtitle carrying a date, person, or status.

Use a `<header>` containing an `<h1>` and, when useful, one `<p>` subtitle:

```html
<header>
  <h1>Quarterly plan</h1>
  <p>Friday, June 26</p>
</header>
```

Use the prose region for document reading rhythm; author any special header treatment locally.

For prose-like documents, 32px padding on the top and sides with 64px at the
bottom is a useful starting point. Adapt it to the document's content and
available space:

```css
body {
  padding: 32px 32px 64px;
}
```

## Canonical tokens

Use these public custom properties in artifact-authored CSS. Include a literal
fallback when the artifact should remain readable without the canonical sheet:

```css
.panel {
  padding: var(--space-16, 16px);
  gap: var(--space-12, 12px);
  color: var(--color-text, #222);
}
```

Fallbacks are paired: a guarded ground goes with a guarded text color, using
values that read together.

This public inventory is closed. Do not invent token names; when no public
token expresses a value, use an intentional literal, and when one does, use the
documented token with a literal fallback where appropriate.

### Fonts

`--font-sans`, `--font-mono`, `--font-weight-normal`, `--font-weight-medium`, `--font-weight-semibold`.

### Colors

`--checkbox-color`, `--panel-background`, `--panel-text-color`, `--panel-border-color`, `--panel-border`, `--panel-edge-highlight`, `--panel-edge-shadow`, `--option-background-highlighted`, `--option-background-active`, `--control-background`, `--control-text-color`, `--control-border-color`, `--control-border-width`, `--control-border`, `--input-placeholder-text-color`, `--neutral`, `--red`, `--orange`, `--yellow`, `--green`, `--cyan`, `--blue`, `--purple`, `--pink`, `--accent`, `--neutral-50`, `--neutral-100`, `--neutral-200`, `--neutral-300`, `--neutral-400`, `--neutral-500`, `--neutral-600`, `--neutral-700`, `--neutral-800`, `--neutral-900`, `--neutral-950`, `--red-50`, `--red-100`, `--red-200`, `--red-300`, `--red-400`, `--red-500`, `--red-600`, `--red-700`, `--red-800`, `--red-900`, `--red-950`, `--orange-50`, `--orange-100`, `--orange-200`, `--orange-300`, `--orange-400`, `--orange-500`, `--orange-600`, `--orange-700`, `--orange-800`, `--orange-900`, `--orange-950`, `--yellow-50`, `--yellow-100`, `--yellow-200`, `--yellow-300`, `--yellow-400`, `--yellow-500`, `--yellow-600`, `--yellow-700`, `--yellow-800`, `--yellow-900`, `--yellow-950`, `--green-50`, `--green-100`, `--green-200`, `--green-300`, `--green-400`, `--green-500`, `--green-600`, `--green-700`, `--green-800`, `--green-900`, `--green-950`, `--cyan-50`, `--cyan-100`, `--cyan-200`, `--cyan-300`, `--cyan-400`, `--cyan-500`, `--cyan-600`, `--cyan-700`, `--cyan-800`, `--cyan-900`, `--cyan-950`, `--blue-50`, `--blue-100`, `--blue-200`, `--blue-300`, `--blue-400`, `--blue-500`, `--blue-600`, `--blue-700`, `--blue-800`, `--blue-900`, `--blue-950`, `--purple-50`, `--purple-100`, `--purple-200`, `--purple-300`, `--purple-400`, `--purple-500`, `--purple-600`, `--purple-700`, `--purple-800`, `--purple-900`, `--purple-950`, `--pink-50`, `--pink-100`, `--pink-200`, `--pink-300`, `--pink-400`, `--pink-500`, `--pink-600`, `--pink-700`, `--pink-800`, `--pink-900`, `--pink-950`, `--accent-50`, `--accent-100`, `--accent-200`, `--accent-300`, `--accent-400`, `--accent-500`, `--accent-600`, `--accent-700`, `--accent-800`, `--accent-900`, `--accent-950`, `--neutral-alpha-5`, `--neutral-alpha-10`, `--neutral-alpha-15`, `--neutral-alpha-25`, `--neutral-alpha-50`, `--neutral-alpha-75`, `--red-alpha-5`, `--red-alpha-10`, `--red-alpha-15`, `--red-alpha-25`, `--red-alpha-50`, `--red-alpha-75`, `--orange-alpha-5`, `--orange-alpha-10`, `--orange-alpha-15`, `--orange-alpha-25`, `--orange-alpha-50`, `--orange-alpha-75`, `--yellow-alpha-5`, `--yellow-alpha-10`, `--yellow-alpha-15`, `--yellow-alpha-25`, `--yellow-alpha-50`, `--yellow-alpha-75`, `--green-alpha-5`, `--green-alpha-10`, `--green-alpha-15`, `--green-alpha-25`, `--green-alpha-50`, `--green-alpha-75`, `--cyan-alpha-5`, `--cyan-alpha-10`, `--cyan-alpha-15`, `--cyan-alpha-25`, `--cyan-alpha-50`, `--cyan-alpha-75`, `--blue-alpha-5`, `--blue-alpha-10`, `--blue-alpha-15`, `--blue-alpha-25`, `--blue-alpha-50`, `--blue-alpha-75`, `--purple-alpha-5`, `--purple-alpha-10`, `--purple-alpha-15`, `--purple-alpha-25`, `--purple-alpha-50`, `--purple-alpha-75`, `--pink-alpha-5`, `--pink-alpha-10`, `--pink-alpha-15`, `--pink-alpha-25`, `--pink-alpha-50`, `--pink-alpha-75`, `--accent-alpha-5`, `--accent-alpha-10`, `--accent-alpha-15`, `--accent-alpha-25`, `--accent-alpha-50`, `--accent-alpha-75`, `--alpha-3`, `--alpha-5`, `--alpha-10`, `--alpha-15`, `--alpha-20`, `--alpha-25`, `--alpha-40`, `--alpha-50`, `--alpha-75`, `--hover-mix`, `--alpha-active`, `--tint-hover`, `--tint-active`, `--icon-check`, `--color-surface`, `--color-surface-muted`, `--color-text`, `--color-text-muted`, `--color-text-reversed`, `--color-border`, `--color-danger`, `--tint-danger`, `--tint-danger-hover`, `--tint-danger-active`, `--color-alert`, `--tint-alert`, `--tint-alert-hover`, `--tint-alert-active`, `--color-success`, `--tint-success`, `--tint-surface`, `--tint-surface-muted`, `--color-primary`, `--tint-primary`, `--tint-primary-hover`, `--tint-primary-active`, `--color-primary-text`, `--outline-focus`, `--control-background-hover`, `--control-background-active`, `--color-primary-hover`, `--color-primary-active`, `--color-danger-hover`, `--color-danger-active`, `--color-alert-hover`, `--color-alert-active`, `--color-link`, `--color-overlay`, `--state-flip`, `--contrast-flip`.

### Type

`--control-font-size`, `--text-base`, `--text-sm`, `--text-md`, `--text-lg`, `--text-xl`, `--text-2xl`, `--text-3xl`, `--text-4xl`, `--line-control`, `--line-control-sm`.

### Spacing and radii

`--control-radius`, `--control-padding`, `--space-2`, `--space-3`, `--space-4`, `--space-6`, `--space-8`, `--space-10`, `--space-12`, `--space-16`, `--space-20`, `--space-24`, `--space-32`, `--space-48`, `--space-64`, `--radius-pill`, `--panel-radius`, `--popover-distance`.

### Shadows

`--shadow-sm`, `--shadow-md`, `--shadow-lg`, `--shadow-xl`, `--popover-shadow`, `--dialog-shadow`.

### Layers

`--layer-ground`, `--layer-panel`, `--layer-overlay`.

## Canonical components

### Native inputs and errors

Native text-entry inputs and textareas receive shared styling automatically.
Supported input types are absent or empty type, `text`, `email`, `url`, `tel`,
`password` and `number`; search fields and other controls are outside this
treatment. Give each field an accessible label; a placeholder is only a hint.
Use `disabled` to disable a field and `readonly` to retain selectable contents
without editing. Preserve the keyboard focus ring and state styling.

When presenting a validation error, set `aria-invalid="true"` and associate
the message using `aria-describedby`. The foundation supplies the invalid
border. Use a paragraph with `class="tv-error"` for shared message styling:

```html
<label for="name">Name</label>
<input id="name" aria-invalid="true" aria-describedby="name-error">
<p id="name-error" class="tv-error">Enter a name.</p>
```

Explain what needs correcting in text, not color alone. Preserve existing hint
IDs when adding the error ID to `aria-describedby`. When the error clears,
remove `aria-invalid` (or set it to `false`), remove only the error ID from
`aria-describedby`, and remove the message or hide it with native `hidden`.
Do not mark untouched required fields invalid merely because they are empty.

`tv-error` adds no validation, visibility, focus or announcement behavior and
can also style messages outside inputs. Decide whether an asynchronous error
needs a live announcement; the class does not imply `role="alert"`.

### Popovers, menus and selects

Pair a panel with the ID of a trigger button in the same document:

```html
<button id="details">Details</button>
<tv-popover trigger="details" open>Panel contents</tv-popover>
<button id="manual-details">Manual details</button>
<tv-popover trigger="manual-details" manual>Explicitly dismissed contents</tv-popover>

<button id="actions">Actions</button>
<tv-menu trigger="actions" open>
  <tv-menu-item>Rename</tv-menu-item>
  <hr>
  <tv-menu-item intent="danger">Delete</tv-menu-item>
</tv-menu>
<button id="manual-actions">Manual actions</button>
<tv-menu trigger="manual-actions" manual>…</tv-menu>

<button id="choice" aria-label="Appearance"></button>
<tv-select trigger="choice" open>
  <tv-option value="light" selected>Light</tv-option>
  <tv-option value="dark">Dark</tv-option>
</tv-select>
```

The `open` attribute controls visibility; omit it for the usual closed initial state.
Popover and menu triggers toggle their panels. Manual popovers and menus require
explicit closing. Ordinary panels dismiss on outside press or Escape; menus
also close on item activation and support keyboard navigation and typeahead.
Author action handlers on the menu items.

Menus and popovers prefer below the trigger with left edges aligned, flip toward
more room when needed, and constrain scrolling within their own document. Selects
open over the selected row and keep an owning Settings-style popover open. A select
copies its selected option label to the trigger. Its `value` property reads or sets
a matching option value; a committed user choice emits `change`. No direction
attributes or internal classes are part of the authoring API.

### Static checkbox lists

Use `checkbox-list` and `checkbox-item` for a checklist whose state is authored
into the document:

```html
<checkbox-list>
  <checkbox-item checked>Completed item</checkbox-item>
  <checkbox-item>Open item</checkbox-item>
</checkbox-list>
```

`checked` marks a completed `checkbox-item`. Checked rows render muted and
struck through, and the marker shows a not-allowed cursor to communicate its
static nature. The list is presentational and does not toggle when pressed.
The shared `--checkbox-color` token controls checked marker fill and border:

```css
:root {
  --checkbox-color: var(--color-success);
}
```

### Icons

`<tv-icon name="…" size="sm|md|lg|xl" spinning>` is the complete authoring
shape. Supply `name`. Omit `size` beside text so the icon follows the current
font size, or choose `sm`, `md`, `lg`, or `xl` for a standalone icon. Add the
boolean `spinning` attribute for continuous activity.

`tv-icon` may be composed inside an author-created shadow root; it renders and
sizes there the same way it does in document light DOM.

```html
<tv-icon name="check" size="md"></tv-icon>
<tv-icon name="spinner" size="md" spinning></tv-icon>
```

The public icon names are:

- `check`, `close`, `copy`, `pin`, `search`, `add`, `more`, `settings`
- `expand`, `collapse`, `collapse-up`, `skills`, `notification`, `artifact`
- `spinner`, `locked`, `unpin`, `back`, `forward`, `television`, `calendar`
- `grid`, `hash`, `delete`, `edit`, `link`, `external`, `download`, `reload`
- `warning`, `info`, `file`, `folder`, `clock`, `user`, `upload`, `star`
- `home`, `send`, `filter`, `sort`, `help`, `chart`, `image`, `chat`
- `location`, `play`, `pause`, `stop`, `video`, `music`, `error`, `email`
- `phone`, `web`, `tag`, `bookmark`, `table`, `code`, `group`, `up`, `down`
- `money`, `bank`, `card`, `wallet`, `gauge`, `activity`, `trend-up`
- `trend-down`, `database`, `server`, `deploy`, `list`, `terminal`, `branch`
- `shield`, `select`, `sidebar`

Choose a name from this catalog. When the catalog has no suitable glyph, use a
text label or an emoji.

## Available document space

Television presents each artifact as its own document inside a resizable
artifact frame. Build responsive layouts from the document's available width
and height. Vertical document scrolling is appropriate for overflow; reserve
horizontal scrolling for content that needs width, such as a data table or
timeline.

For a native dialog, put its contents in one direct `.dialog-content` child: `<dialog><div class="dialog-content">…</div></dialog>`. Author content layout on that child. It scrolls within the viewport and any authored dialog height or maximum height while the dialog paints its rim and broad shadow. Unwrapped dialogs retain native overflow.
