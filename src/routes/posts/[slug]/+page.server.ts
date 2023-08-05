import { error } from "@sveltejs/kit"
import type { PageServerLoad } from "./$types"
import { readdirSync } from "node:fs"
import { resolve } from "node:path"
import { exec } from 'node:child_process';
import { promisify } from 'node:util';

const export_org_as_html = async (org_path: string, emacs_exe_path = "emacs") => promisify(exec)(`
  ${emacs_exe_path} \
  -batch ${org_path} \
  -l ox \
  --eval="(princ (org-export-as 'html nil nil t))"
`).then(({ stderr, stdout }) => {
  if (stderr != "") {
    console.log(stderr)
  }
  return stdout
})

export const load: PageServerLoad = ({ params, depends }) => {
  const ORG_POSTS_DIR = "src/routes/posts"
  const path = readdirSync(ORG_POSTS_DIR).reduce<string | null>(
    (acc, path) =>
      acc || (path.toLowerCase() == `${params.slug}.org`.toLowerCase() && path) || null
    , null)
  depends(`org_post_update:${params.slug}`)
  if (path) {
    return {
      slug: params.slug,
      html: export_org_as_html(resolve(ORG_POSTS_DIR, path))
    }
  }
  else throw error(404)
}
