---
name: native-web-search
description: "Trigger native web search. Use when you need quick internet research with concise summaries and full source URLs."
---

# Native Web Search

The `pi-web-access` package (installed via `npm:pi-web-access` in
`pi/.pi/agent/settings.json`) registers `web_search`, `source_check`,
`fetch_content`, and `get_search_content` directly as Pi tools. Use those
tools; there is no wrapper script to run here.

## Which tool to call

- `web_search` — one or more queries, optional provider filter, returns
  synthesized answers with source URLs. Prefer `queries` (array, 2-4
  varied angles) over a single `query` for research.
- `source_check` — verify a specific claim; returns machine-readable
  passage citations.
- `fetch_content` — fetch a URL as readable markdown, or use
  `mode: "answer"` with a `prompt` to answer strictly from a page.
- `get_search_content` — retrieve stored slices from a previous
  `web_search`, `source_check`, or `fetch_content` call via its
  `responseId`.

## Notes

- Provider selection, credentials, and OAuth token handling are owned by
  `pi-web-access`. There is no shell-side credential wrapper in this
  repo. Do not fabricate one; if the package is misconfigured, update
  its configuration through Pi settings.
- If the package is not present in a checkout, install it explicitly:
  `pi install npm:pi-web-access`. The custom skill script that used to
  live here was removed on redundancy grounds; do not resurrect it
  without a documented failure of the packaged tools.
