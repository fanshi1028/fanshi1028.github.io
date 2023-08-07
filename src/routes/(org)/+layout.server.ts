import { get_org_html_style_default } from "$lib/server/org_post"
import type { LayoutServerLoad } from "./$types"

export const load: LayoutServerLoad = async () => ({
  defaultCSS: await get_org_html_style_default()
})
