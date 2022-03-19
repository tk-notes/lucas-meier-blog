const defaultTheme = require("tailwindcss/defaultTheme");

module.exports = {
  content: ["./layouts/**/*.html"],
  theme: {
    extend: {},
    fontFamily: {
      mono: ["Iosevka", "monospace"],
      sans: ["IBM Plex Sans", ...defaultTheme.fontFamily.sans],
    },
  },
  plugins: [],
};
