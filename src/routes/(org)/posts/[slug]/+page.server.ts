import { error } from "@sveltejs/kit"
import type { PageServerLoad } from "./$types"
import { readdirSync } from "node:fs"
import { resolve } from "node:path"
import { export_org_as_html } from "$lib/server/org_post";

const ORG_POSTS_DIR = "posts"

export const load: PageServerLoad = ({ params, depends }) => {
  const slug = params.slug.toLowerCase()
  const path = readdirSync(ORG_POSTS_DIR).reduce<string | null>(
    (acc, path) =>
      acc || (path.toLowerCase() == `${slug}.org`.toLowerCase() && path) || null
    , null)
  depends(slug)
  if (path) {
    return {
      slug,
      html: export_org_as_html(resolve(ORG_POSTS_DIR, path))
    }
  }
  else throw error(404)
}
