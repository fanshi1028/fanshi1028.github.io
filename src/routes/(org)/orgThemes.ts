export const org_css = {
  // sandyuraz: "https://sandyuraz.com/styles/org.css",
  "solarized-light":
    "https://thomasf.github.io/solarized-css/solarized-light.min.css",
  "solarized-dark":
    "https://thomasf.github.io/solarized-css/solarized-dark.min.css",
  orgcss: "https://gongzhitaao.org/orgcss/org.css",
} as const;

export type OrgTheme = keyof typeof org_css | "default-minimal"
