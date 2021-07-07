module.exports = {
  mode: 'jit',
  purge: [
    '../lib/**/*.ex',
    '../lib/**/*.leex',
    '../lib/**/*.heex',
    '../lib/**/*.eex',
    './js/**/*.js'
  ],
  darkMode: false,
  theme: {
    fontFamily: {
      'sans': ['Inter'],
      'mono': ['JetBrains Mono'],
    },
    extend: {
      colors: {
        blue: {
          50: '#F5F7FF',
          100: '#ECF0FF',
          200: '#D8E0FF',
          300: '#B2C1FF',
          400: '#8BA2FF',
          500: '#6583FF',
          600: '#3E64FF',
          700: '#2D4CDB',
          800: '#1F37B7',
          900: '#132593',
        },
        gray: {
          50: '#F8FAFC',
          100: '#F0F5F9',
          200: '#E1E8F0',
          300: '#CAD5E0',
          400: '#91A4B7',
          500: '#61758A',
          600: '#445668',
          700: '#304254',
          800: '#1C2A3A',
          900: '#0D1829',
        },
        red: {
          50: '#FFF5F5',
          100: '#FFECEC',
          200: '#FFD8D7',
          300: '#FFB2AF',
          400: '#FF8B88',
          500: '#FF6560',
          600: '#FF3E38',
          700: '#DB2832',
          800: '#DB2832',
          900: '#93112F',
        },
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
