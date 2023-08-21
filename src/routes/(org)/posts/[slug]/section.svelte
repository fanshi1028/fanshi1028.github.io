<script lang="ts">
    import type { findOne } from "domutils";
    import TextContent from "./textContent.svelte";
    import DescriptionList from "./descriptionList.svelte";
    import UnorderedList from "./unorderedList.svelte";
    import OrderedList from "./orderedList.svelte";

    export let displaylevels = [1, 2, 3, 4, 5];
    export let level: number;
    export let ele: NonNullable<ReturnType<typeof findOne>>;
</script>

{#if displaylevels.indexOf(level) != -1}
    {#each ele.children as child}
        {#if child.type == "tag"}
            {#if child.name == `h${level}`}
                <svelte:element this={`h${level}`} class={child.attribs.class}>
                    <TextContent textEle={child} />
                </svelte:element>
            {:else if child.attribs.class == `outline-text-${level}`}
                {#each child.children as grandChild}
                    {#if grandChild.type == "tag"}
                        {#if grandChild.name == "p"}
                            <p class={grandChild.attribs.class}>
                                <TextContent textEle={grandChild} />
                            </p>
                        {:else if grandChild.name == "dl"}
                            <DescriptionList listEle={grandChild} />
                        {:else if grandChild.name == "ul"}
                            <UnorderedList listEle={grandChild} />
                        {:else if grandChild.attribs.class == "org-src-container"}
                            {#each grandChild.children as srcChild}
                                {#if srcChild?.type == "tag" && srcChild.name == "pre"}
                                    <!-- prettier-ignore -->
                                    <pre class={srcChild.attribs.class}><TextContent textEle={srcChild}/></pre>
                                {/if}
                            {/each}
                        {/if}
                    {/if}
                {/each}
            {:else if child.attribs.class == `outline-${level + 1}`}
                <svelte:self {displaylevels} level={level + 1} ele={child} />
            {:else if child.name == "ol"}
                <OrderedList listEle={child} />
            {/if}
        {/if}
    {/each}
{/if}
