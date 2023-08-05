import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/kit/vite';
import { readdirSync } from 'fs';

const append_org_post_routes = posts => readdirSync("src/routes/posts").
	reduce((acc, path) => {
		const slug = path.match(/^(?<slug>.+).org$/i)?.groups?.slug
		return slug ? [...acc, `/posts/${slug}`] : acc
	}, posts)

/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://kit.svelte.dev/docs/integrations#preprocessors
	// for more information about preprocessors
	preprocess: vitePreprocess(),

	kit: {
		// adapter-auto only supports some environments, see https://kit.svelte.dev/docs/adapter-auto for a list.
		// If your environment is not supported or you settled on a specific environment, switch out the adapter.
		// See https://kit.svelte.dev/docs/adapters for more information about adapters.
		adapter: adapter({
			// fallback: '200.html' // may differ from host to host
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH,
		},
		prerender: {
			// use relative URLs similar to an anchor tag <a href="/test/1"></a>
			// do not include group layout folders in the path such as /(group)/test/1
			entries: [append_org_post_routes].reduce((posts, f) => f(posts), ["*"])
		}
	},
};

export default config;
