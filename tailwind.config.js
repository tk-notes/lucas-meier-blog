module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: {
    content: ["./layouts/**/*.html"],
  },
  theme: {
    fontFamily: {
      mono: ["IBM Plex Mono", "JetBrains Mono", "monospace"],
      serif: ["IBM Plex Serif", "Noto Serif", "Georgia", "Cambria", "Times New Roman", "serif"],
    },
    extend: {},
  },
  variants: {},
  plugins: [],
};
