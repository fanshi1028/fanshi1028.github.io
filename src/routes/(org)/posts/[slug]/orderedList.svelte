<script lang="ts">
    import type { findOne } from "domutils";
    import TextContent from "./textContent.svelte";

    export let listEle: NonNullable<ReturnType<typeof findOne>>;
</script>

<ol class={listEle.attribs.class}>
    {#each listEle.children as child}
        {#if child.type == "tag" && child.name == "li"}
            <svelte:element this={child.name} class={child.attribs.class}>
                <TextContent textEle={child} />
            </svelte:element>
        {/if}
    {/each}
</ol>
