import { exec } from 'node:child_process';
import { promisify } from 'node:util';

export const export_org_as_html = async (org_path: string, emacs_exe_path = "emacs") => promisify(exec)(`
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
