const defaultTheme = require("tailwindcss/defaultTheme");

module.exports = {
  content: ["./layouts/**/*.html"],
  theme: {
    extend: {},
    fontFamily: {
      mono: ["IBM Plex Mono", "JetBrains Mono", "monospace"],
      sans: ["IBM Plex Sans", ...defaultTheme.fontFamily.sans],
    },
  },
  plugins: [],
};
