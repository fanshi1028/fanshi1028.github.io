import { sveltekit } from '@sveltejs/kit/vite';
import { basename, dirname } from 'path';
import { fileURLToPath } from 'url';
import { defineConfig, type PluginOption } from 'vite';

const orgHmr: PluginOption = {
	name: 'org-hmr',
	enforce: 'post',
	handleHotUpdate({ file, server }) {
		const org_posts_dir = `${dirname(fileURLToPath(import.meta.url))}/src/routes/posts`
		const slug =
			basename(file).match(/^(?<slug>.+)\.org$/i)?.
				groups?.slug.toLowerCase()
		if (slug && org_posts_dir == dirname(file)) {
			console.log(`reloading org file: ${file}`);
			server.ws.send({
				type: 'custom',
				event: 'org_posts_update',
				data: slug
			});
		}
	},
}

export default defineConfig({
	plugins: [sveltekit(), orgHmr]
});
