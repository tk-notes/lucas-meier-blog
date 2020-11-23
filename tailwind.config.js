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
      mono: ["JetBrains Mono", "monospace"],
      serif: ["Noto Serif", "Georgia", "Cambria", "Times New Roman", "serif"],
    },
    extend: {},
  },
  variants: {},
  plugins: [],
};
