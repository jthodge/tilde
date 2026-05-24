import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const messages = [
  "Engineering...",
  "Computing...",
  "Processing...",
  "Executing...",
  "Analyzing...",
  "Synthesizing...",
  "Calculating...",
  "Generating...",
  "Formulating...",
  "Determining...",
  "Deriving...",
  "Implementing...",
];

function pickRandom(): string {
  return messages[Math.floor(Math.random() * messages.length)];
}

export default function (pi: ExtensionAPI) {
  pi.on("turn_start", async (_event, ctx) => {
    ctx.ui.setWorkingMessage(pickRandom());
  });

  pi.on("turn_end", async (_event, ctx) => {
    ctx.ui.setWorkingMessage(); // Reset for next time
  });
}
