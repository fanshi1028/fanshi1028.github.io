<script lang="ts">
    import { textContent, type findOne } from "domutils";

    export let listEle: NonNullable<ReturnType<typeof findOne>>;
</script>

<dl class={listEle.attribs.class}>
    {#each listEle.children as child}
        {#if child.type == "tag" && (child.name == "dd" || child.name == "dt")}
            <svelte:element this={child.name} class={child.attribs.class}>
                {textContent(child)}
            </svelte:element>
        {/if}
    {/each}
</dl>
