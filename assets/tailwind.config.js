const defaultTheme = require("tailwindcss/defaultTheme");
const plugin = require("tailwindcss/plugin");

module.exports = {
  content: [
    "../lib/**/*.ex",
    "../lib/**/*.leex",
    "../lib/**/*.heex",
    "../lib/**/*.eex",
    "./js/**/*.js",
  ],
  theme: {
    fontFamily: {
      sans: ["Inter", ...defaultTheme.fontFamily.sans],
      mono: ["JetBrains Mono", ...defaultTheme.fontFamily.mono],
      logo: ["Red Hat Text"],
    },
    extend: {
      colors: {
        blue: {
          50: "#F5F7FF",
          100: "#ECF0FF",
          200: "#D8E0FF",
          300: "#B2C1FF",
          400: "#8BA2FF",
          500: "#6583FF",
          600: "#3E64FF",
          700: "#2D4CDB",
          800: "#1F37B7",
          900: "#132593",
        },
        gray: {
          50: "#F8FAFC",
          100: "#F0F5F9",
          200: "#E1E8F0",
          300: "#CAD5E0",
          400: "#91A4B7",
          500: "#61758A",
          600: "#445668",
          700: "#304254",
          800: "#1C2A3A",
          900: "#0D1829",
        },
        red: {
          50: "#FDF3F4",
          100: "#FCE8E9",
          200: "#F8D1D2",
          300: "#F1A3A6",
          400: "#E97579",
          500: "#E2474D",
          600: "#DB1920",
          700: "#BC1227",
          800: "#9D0C2B",
          900: "#7F072B",
        },
        green: {
          50: "#F3F9F3",
          100: "#E9F4E9",
          200: "#D2E7D1",
          300: "#A5D0A3",
          400: "#77B876",
          500: "#4AA148",
          600: "#1D891A",
          700: "#137518",
          800: "#0D6219",
          900: "#084F18",
        },
        yellow: {
          50: "#FFFAF5",
          100: "#FFF7EC",
          200: "#FFEED9",
          300: "#FFDCB2",
          400: "#FFCB8C",
          500: "#FFB965",
          600: "#FFA83F",
          700: "#DB842E",
          800: "#B7641F",
          900: "#934814",
        },
        "green-bright": {
          50: "#F0FDF4",
          100: "#DCFCE7",
          200: "#BBF7D0",
          300: "#86EFAC",
          400: "#4ADE80",
          500: "#22C55E",
          600: "#16A34A",
          700: "#15803D",
          800: "#166534",
          900: "#14532D",
        },
        "yellow-bright": {
          50: "#FEFCE8",
          100: "#FEF9C3",
          200: "#FEF08A",
          300: "#FDE047",
          400: "#FACC15",
          500: "#EAB308",
          600: "#CA8A04",
          700: "#A16207",
          800: "#854D0E",
          900: "#713F12",
        },
        "brand-pink": "#e44c75",
      },
      keyframes: {
        shake: {
          "0%": { transform: "translateX(0)" },
          "20%": { transform: "translateX(-10px)" },
          "40%": { transform: "translateX(8px)" },
          "60%": { transform: "translateX(-6px)" },
          "80%": { transform: "translateX(4px)" },
          "100%": { transform: "translateX(0)" },
        },
      },
      animation: {
        shake: "shake 0.5s linear 0.2s",
      },
    },
  },
  plugins: [
    plugin(({ addVariant }) => {
      addVariant("phx-loading", [".phx-loading&", ".phx-loading &"]);
      addVariant("phx-connected", [".phx-connected&", ".phx-connected &"]);
      addVariant("phx-error", [".phx-error&", ".phx-error &"]);
      addVariant("phx-click-loading", [
        ".phx-click-loading&",
        ".phx-click-loading &",
      ]);
      addVariant("phx-submit-loading", [
        ".phx-submit-loading&",
        ".phx-submit-loading &",
      ]);
      addVariant("phx-change-loading", [
        ".phx-change-loading&",
        ".phx-change-loading &",
      ]);
    }),
  ],
};
