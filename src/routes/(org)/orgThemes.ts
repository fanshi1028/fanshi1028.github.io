export const org_css = {
  solarizedDark:
    "https://thomasf.github.io/solarized-css/solarized-dark.min.css",
  solarizedLight:
    "https://thomasf.github.io/solarized-css/solarized-light.min.css",
  orgCss: "https://gongzhitaao.org/orgcss/org.css",
  sandyuraz: "https://sandyuraz.com/styles/org.css",
} as const;

export type OrgTheme = keyof typeof org_css | "defaultMinimal"
