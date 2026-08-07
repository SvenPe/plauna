import { copyFile, mkdir } from "node:fs/promises";
import { dirname } from "node:path";

const assets = [
  ["node_modules/vega/build/vega.min.js", "resources/public/js/vendor/vega.min.js"],
  ["node_modules/vega-lite/build/vega-lite.min.js", "resources/public/js/vendor/vega-lite.min.js"],
  ["node_modules/vega-embed/build/vega-embed.min.js", "resources/public/js/vendor/vega-embed.min.js"],
];

for (const [source, destination] of assets) {
  await mkdir(dirname(destination), { recursive: true });
  await copyFile(source, destination);
}
