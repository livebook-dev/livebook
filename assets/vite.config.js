import fs from "fs/promises";
import path from "path";
import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import { compression } from "vite-plugin-compression2";

export default defineConfig({
  plugins: [
    tailwindcss(),
    resumeStdinPlugin(),
    copyIframePlugin(),
    compression({
      // Given that we include assets in the desktop app and in the
      // escript executable, we only include the compressed assets
      // instead of both compressed and original in order to reduce
      // the size.
      deleteOriginalAssets: true,
      algorithms: ["gzip"],
    }),
  ],
  build: {
    outDir: "../priv/static",
    emptyOutDir: true,
    rollupOptions: {
      input: "js/app.js",
      output: {
        // Don't include hash in the name.
        entryFileNames: `assets/[name].js`,
        chunkFileNames: `assets/[name].js`,
        assetFileNames: `assets/[name].[ext]`,
      },
    },
  },
  server: {
    port: 4432,
    origin: "http://localhost:4432",
  },
});

function copyIframePlugin() {
  return {
    name: "copy-iframe",
    async generateBundle() {
      const src = path.resolve(__dirname, "../iframe/priv/static/iframe");
      await emitDir(this, src, "iframe");
    },
  };
}

async function emitDir(context, dir, prefix) {
  for (const entry of await fs.readdir(dir, { withFileTypes: true })) {
    const srcPath = path.join(dir, entry.name);
    const fileName = `${prefix}/${entry.name}`;
    if (entry.isDirectory()) {
      await emitDir(context, srcPath, fileName);
    } else {
      context.emitFile({
        type: "asset",
        fileName,
        source: await fs.readFile(srcPath),
      });
    }
  }
}

// Workaround to avoid a zombie process when the dev server is started
// as a watcher from Elixir. See https://github.com/vitejs/vite/issues/19091.
function resumeStdinPlugin() {
  return {
    name: "resume-stdin",
    configureServer() {
      // We guard this behaviour via explicit custom flag, because in
      // other cases it may not be desired. For example, it causes
      // tests run to never finish.
      if (process.env.WATCH_STDIN === "1") {
        process.stdin.resume();
      }
    },
  };
}
