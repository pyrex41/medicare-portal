/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: ["class"],
  content: ["./src/**/*.{html,js,elm}", "*.{js,ts,jsx,tsx,mdx}"],
  theme: {
    extend: {
      fontFamily: {
        sans: [
          '-apple-system',
          'BlinkMacSystemFont',
          'Roboto',
          '"Segoe UI"',
          '"Helvetica Neue"',
          'Arial',
          '"Noto Sans"',
          'sans-serif',
          '"Apple Color Emoji"',
          '"Segoe UI Emoji"',
          '"Segoe UI Symbol"',
          '"Noto Color Emoji"',
        ],
        heading: ['Inter', 'system-ui', 'sans-serif'],
      },
      colors: {
        brand: {
          DEFAULT: '#03045e',
          light: '#f9fafb',
        },
        gray: {
          50: '#f9fafb',
          100: '#eaecf0',
          300: '#d0d5dd',
          400: '#98a2b3',
          500: '#667085',
          600: '#475467',
          700: '#344054',
          900: '#101828',
        },
        // Keep existing shadcn/ui colors
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        background: "hsl(var(--background))",
        foreground: "hsl(var(--foreground))",
        primary: {
          DEFAULT: "hsl(var(--primary))",
          foreground: "hsl(var(--primary-foreground))",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
        // Medicare-specific colors
        medicare: {
          success: "#047857",
          "success-light": "#eafcf1",
          danger: "#ef4444",
          "danger-light": "#fecaca",
        },
        // New theme colors for questions
        theme: {
          blue: {
            DEFAULT: "#0075F2",
            light: "rgba(0, 117, 242, 0.1)",
            medium: "rgba(0, 117, 242, 0.2)",
          },
          purple: {
            DEFAULT: "#7F56D9",
            light: "rgba(127, 86, 217, 0.1)",
            medium: "rgba(127, 86, 217, 0.2)",
          },
          amber: {
            DEFAULT: "#F59E0B",
            light: "rgb(254, 243, 199)",
            medium: "rgb(253, 230, 138)",
          },
          brand: {
            DEFAULT: "#03045E",
            light: "#ADE8F4",
            medium: "rgba(173, 232, 244, 0.5)",
          }
        }
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      boxShadow: {
        'card': '0px 4px 6px -2px rgba(0, 0, 0, 0.05), 0px 10px 15px -3px rgba(0, 0, 0, 0.1)',
      },
    },
  },
  plugins: [require("tailwindcss-animate")],
}