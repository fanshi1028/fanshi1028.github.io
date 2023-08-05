import { error } from "@sveltejs/kit"
import type { PageServerLoad } from "./$types"
import { existsSync } from "node:fs"
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

export const load: PageServerLoad = async ({ params, depends }) => {
  const slug = params.slug.toLowerCase()
  depends(`org_post_update:${slug}`)
  const path = resolve(`src/routes/posts/${slug}.org`)
  console.log(path)
  if (existsSync(path)) {
    return {
      slug,
      html: export_org_as_html(path)
    }
  }
  else throw error(404)
}
