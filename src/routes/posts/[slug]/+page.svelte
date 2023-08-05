<script lang="ts">
    import { invalidate } from "$app/navigation";
    import type { PageServerData } from "./$types";
    export let data: PageServerData;

    import.meta.hot?.on("org_posts_update", (slug) => invalidate(`org_post_update:${slug}`));
</script>

{@html data.html}
