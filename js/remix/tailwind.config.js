module.exports = {
  content: [
    "./app/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      boxShadow: {
        "ant-light": "4px 4px var(--ant-primary-color)",
        "ant-dark": "4px 4px var(--ant-primary-9)",
        "black": "4px 4px @black",
      },
      colors: {
        ant: "var(--ant-primary-color)",
      },
      fontFamily: {
        'title': ['Courier New', 'Courier', 'monospace'],
      },
    },
  },
  plugins: [],
}
