export default {
  plugins: {
    "postcss-import": {},
    // See https://tailwindcss.com/docs/using-with-preprocessors#nesting
    "tailwindcss/nesting": "postcss-nesting",
    tailwindcss: {},
    autoprefixer: {},
  },
};
