const path = require("path");
const esbuild = require("esbuild");
const stylePlugin = require("esbuild-style-plugin");
const postcssImport = require("postcss-import");
const tailwind = require("tailwindcss");
const tailwindNesting = require("tailwindcss/nesting");
const postcssNesting = require("postcss-nesting");
const autoprefixer = require("autoprefixer");

const args = process.argv.slice(2);
const watch = args.includes("--watch");
const deploy = args.includes("--deploy");

const outDir = path.resolve(
  __dirname,
  deploy ? "../static/assets" : "../tmp/static_dev/assets",
);

async function main() {
  const ctx = await esbuild.context({
    entryPoints: ["js/app.js"],
    outdir: outDir,
    bundle: true,
    splitting: true,
    target: "es2017",
    format: "esm",
    minify: deploy,
    sourcemap: deploy ? undefined : "linked",
    plugins: [
      stylePlugin({
        postcss: {
          plugins: [
            postcssImport,
            // See https://tailwindcss.com/docs/using-with-preprocessors#nesting
            tailwindNesting(postcssNesting),
            tailwind,
            autoprefixer,
          ],
        },
      }),
    ],
    loader: {
      ".ttf": "file",
      ".woff": "file",
      ".woff2": "file",
      ".eot": "file",
      ".svg": "file",
    },
  });

  if (watch) {
    await ctx.watch();

    process.stdin.on("close", () => {
      process.exit(0);
    });

    process.stdin.resume();
  } else {
    await ctx.rebuild();
    process.exit(0);
  }
}

main();
