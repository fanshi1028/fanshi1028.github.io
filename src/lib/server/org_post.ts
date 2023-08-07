import { exec } from 'node:child_process';
import { promisify } from 'node:util';

const setq = (options: Record<string, any>) =>
  Object.entries(options).reduce<string>(
    (acc, i) => {
      const val =
        typeof i[1] == "boolean" ? (i[1] ? "t" : "nil")
          : typeof i[1] == "string" ? `"${i[1]}"` :
            i[1] ?? "nil"
      return `${acc} --eval="(setq ${i[0]} ${val})"`
    }, "")

export const export_org_as_html = async (
  org_path: string,
  opts: Record<string, any> = {
    "org-html-head-include-default-style": null
  },
  emacs_exe_path = "emacs"
) => promisify(exec)(`
  ${emacs_exe_path} \
-batch ${org_path} \
-l ox \
${setq(opts)} \
--eval="(princ (org-export-as 'html))"
  `).then(({ stderr, stdout }) => {
  if (stderr != "") {
    console.log(stderr)
  }
  return stdout
})
