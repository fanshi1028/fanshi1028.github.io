import type { Config } from 'tailwindcss'

// NOTE: https://colorpalettes.io/dark-academia-color-palette/
const darkAcademia = {
  // "charcoal-gray":  "#3F4443",
  "charcoal-gray": {
    DEFAULT: "#3F4443",
    7: "#3F4443",
    50: "#22453E",
    93: "#054538"
  },
  // "grayish-blue": "#6B7073",
  "grayish-blue": {
    DEFAULT: "#6B7073",
    7: "#6B7073",
    50: "#395D73",
    93: "#084A73"
  },
  // "dark-sienna": "#584446",
  "dark-sienna": {
    // DEFAULT: "#882D17",
    DEFAULT: "#584446",
    23: "#584446",
    50: "#592D31",
    77: "#59151B"
  },
  // "rosy-brown": "#6D4F47",
  "rosy-brown": {
    DEFAULT: "#6D4F47",
    35: "#6D4F47",
    50: "#6E4337",
    65: "#6E3626",
  },
  // "gray-olive": "#999179",
  "gray-olive": {
    DEFAULT: "#999179",
    21: "#999179",
    50: "#99864D",
    79: "#997B20"
  },
  // "olive-drab": "#B0AA7E",
  "olive-drab": {
    DEFAULT: "#B0AA7E",
    28: "#B0AA7E",
    50: "#B0A658",
    72: "#B0A131"
  }
} as const;

const solarized = {
  base3: "#fdf6e3",
  base2: "#eee8d5",
  base1: "#93a1a1",
  base0: "#839496",
  base00: "#657b83",
  base01: "#586e75",
  base02: "#073642",
  base03: "#002b36",
  yellow: "#b58900",
  orange: "#cb4b16",
  red: "#dc322f",
  magenta: "#d33682",
  violet: "#6c71c4",
  blue: "#268bd2",
  cyan: "#2aa198",
  green: "#859900",
} as const

const colors = {
  // NOTE: https://colorpalettes.io/dark-academia-color-palette/
  "dark-academia": darkAcademia,
  // NOTE: https://ethanschoonover.com/solarized/#the-values
  solarized: {
    yellow: solarized.yellow,
    orange: solarized.orange,
    red: solarized.red,
    magenta: solarized.magenta,
    violet: solarized.violet,
    blue: solarized.blue,
    cyan: solarized.cyan,
    green: solarized.green,
    // NOTE: ligth mode
    background: {
      highlight: solarized.base2,
      DEFAULT: solarized.base3,
    },
    content: {
      emphasized: solarized.base01,
      DEFAULT: solarized.base00,
      secondary: solarized.base1
    },
    // NOTE: dark mode
    dark: {
      background: {
        highlight: solarized.base02,
        DEFAULT: solarized.base03,
      },
      content: {
        emphasized: solarized.base1,
        DEFAULT: solarized.base0,
        secondary: solarized.base01
      }
    },
  }
} as const

export default {
  content: ["./src/**/*.{html,svelte,ts}"],
  theme: {
    extends: {

    },
    colors,
  },
  plugins: [],
} satisfies Config
