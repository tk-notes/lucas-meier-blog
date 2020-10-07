module.exports = {
  future: {
    removeDeprecatedGapUtilities: true,
    purgeLayersByDefault: true,
  },
  purge: [
    './layouts/**/*.html'
  ],
  theme: {
    fontFamily: {
      mono: ["Fira Code", "monospace"],
      serif: [
        "Noto Serif",
        "Georgia",
        "Cambria",
        "Times New Roman",
        "serif",
      ],
    },
    extend: {},
  },
  variants: {},
  plugins: [],
};
