import type { Config } from 'tailwindcss'

export default {
  content: ['./app/*.hs'],
  theme: {
    extend: {},
  },
  plugins: [
   require('@tailwindcss/typography'),
   require('@tailwindcss/forms')
  ],
} satisfies Config

