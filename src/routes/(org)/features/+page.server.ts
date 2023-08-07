import type { PageServerLoad } from "./$types"
import { export_org_as_html } from "$lib/server/org_post"

export const load: PageServerLoad = ({ depends }) => {
  depends(`features`)
  return {
    html: export_org_as_html("features.org")
  }
}
