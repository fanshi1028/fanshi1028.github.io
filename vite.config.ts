import { sveltekit } from '@sveltejs/kit/vite';
import { basename, dirname, resolve } from 'path';
import { defineConfig, type PluginOption } from 'vite';

const orgHmr: PluginOption = {
	name: 'org-hmr',
	enforce: 'post',
	handleHotUpdate({ file, server }) {
		if (file == resolve("features.org")) {
			console.log(`reloading features.org: ${file}`);
			server.ws.send({
				type: 'custom',
				event: 'features_org_update'
			});
		} else {
			const slug =
				basename(file).match(/^(?<slug>.+)\.org$/i)?.
					groups?.slug.toLowerCase()
			if (slug && dirname(file) == resolve("posts")) {
				console.log(`reloading org file: ${file}`);
				server.ws.send({
					type: 'custom',
					event: 'org_post_update',
					data: slug
				});
			}
		}
	},
}

export default defineConfig({
	plugins: [sveltekit(), orgHmr]
});
