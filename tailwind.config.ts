import type { Config } from 'tailwindcss'

export default {
  content: ['./app/*.hs'],
  theme: {
    extend: {
      keyframes: {
        wiggle: {
          '0%, 100%': { transform: 'rotate(-3deg)' },
          '50%': { transform: 'rotate(3deg)' },
        },
      },
      animation: {
          wiggle: 'wiggle 0.1s ease-in-out 2',
      }
    },
  },
  plugins: [
   require('@tailwindcss/typography'),
   require('@tailwindcss/forms')
  ],
} satisfies Config

