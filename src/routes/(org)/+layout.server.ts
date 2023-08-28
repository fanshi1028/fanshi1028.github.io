import { get_org_html_style_default } from "$lib/server/org_post"
import type { LayoutServerLoad } from "./$types"
// import { org_css, type OrgTheme } from "./orgThemes"

// const fetchCss = (theme: keyof typeof org_css) => fetch(org_css[theme]).then(async res => {
//   console.log(`fetched ${theme}`)
//   return `<style>${await res.text()}</style>`
// }).catch(e => `fetch ${theme} failed: ${e}`)

// export const load: LayoutServerLoad<Record<OrgTheme, string>> = async () => {
//   const [defaultMinimal, sandyuraz, solarizedDark, solarizedLight, orgCss] = await Promise.all([
//     get_org_html_style_default(),
//      "sandyuraz" || fetchCss("sandyuraz"),
//      "solarizedDark"|| fetchCss("solarizedDark"),
//     "solarizedLight"|| fetchCss("solarizedLight"),
//      "orgCss" || fetchCss("orgCss")
//     // fetchCss("sandyuraz"),
//     // fetchCss("solarizedDark"),
//     // fetchCss("solarizedLight"),
//     // fetchCss("orgCss")
//   ])
//   return {
//     defaultMinimal,
//     sandyuraz,
//     solarizedDark,
//     solarizedLight,
//     orgCss
//   }
// }
