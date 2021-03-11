module.exports = {
  purge: [
    '../lib/**/*.ex',
    '../lib/**/*.leex',
    '../lib/**/*.eex',
    './js/**/*.js'
  ],
  darkMode: false,
  theme: {
    fontFamily: {
      'sans': ['Inter'],
      'mono': ['JetBrains Mono'],
     },
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
