<script lang="ts">
    import { invalidate } from "$app/navigation";
    import type { PageServerData } from "./$types";
    export let data: PageServerData;

    import.meta.hot?.on("org_post_update", invalidate);
</script>

{@html data.html}
