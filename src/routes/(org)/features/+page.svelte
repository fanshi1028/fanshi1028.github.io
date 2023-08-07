<script lang="ts">
    import { invalidate } from "$app/navigation";
    import type { PageServerData } from "./$types";
    export let data: PageServerData;

    import.meta.hot?.on("features_org_update", () => invalidate("features"));
</script>

{@html data.html}
