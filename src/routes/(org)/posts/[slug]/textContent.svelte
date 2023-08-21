<script lang="ts">
    import type { findOne } from "domutils";

    export let textEle: NonNullable<ReturnType<typeof findOne>>;
</script>

{#each textEle.children as child}
    {#if child.type == "tag"}
        {#if child.name == "a"}
            <a href={child.attribs.href} class={child.attribs.class}>
                <svelte:self textEle={child} />
            </a>
        {:else}
            <svelte:element this={child.name} class={child.attribs.class}>
                <svelte:self textEle={child} />
            </svelte:element>
        {/if}
    {:else if child.type == "text"}
        {child.data}
    {/if}
{/each}
